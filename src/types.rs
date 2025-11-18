use crate::func::{FunctionMap, FunctionMapper};
use crate::lexer::{ComparisonOp, Type};
use crate::parser::{Elif, Expr, Spanned};
use std::collections::HashMap;
use std::ops::Deref;

pub fn infer_types(
    spanned: &mut Spanned<Expr>,
    vars: &mut HashMap<String, Type>,
    funcs: &mut HashMap<String, FunctionMap>,
) {
    match spanned {
        Spanned(Expr::Ineq { lhs, cmp, rhs }, _, _) => {
            if *cmp == ComparisonOp::IneqEq {
                if let Expr::Ident(x) = &lhs.0 {
                    let rh_type = calc_type(rhs, vars, funcs);
                    rhs.2 = rh_type.clone();
                    lhs.2 = rh_type.clone();
                    spanned.2 = rh_type.clone();
                    vars.insert(x.clone(), rh_type);
                }
            } else {
                spanned.2 = Type::Ineq
            }
        }
        Spanned(Expr::Def { args, body, name }, _, t) => {
            for arg in args.iter() {
                vars.insert(arg.0.clone(), arg.1.clone());
            }
            let bod_type = calc_type(body, vars, funcs);
            if *t != bod_type {
                mismatch_body_type(spanned)
            }
            body.2 = bod_type.clone();
            for arg in args.iter() {
                vars.remove(&arg.0);
            }
            funcs.insert(
                name.clone(),
                FunctionMap::newm(
                    name.to_string(),
                    FunctionMapper::from(Box::new(|_| bod_type.clone())),
                    spanned.1,
                ),
            );
        }
        Spanned(Expr::Fold { body, .. }, _, _) => {
            for item in body.iter_mut() {
                calc_type(item, vars, funcs);
            }
        }
        _ => {
            spanned.2 = calc_type(spanned, vars, funcs);
        }
    }
}

fn calc_type(
    Spanned(expr, _span, type_): &mut Spanned<Expr>,
    vars: &mut HashMap<String, Type>,
    funcs: &mut HashMap<String, FunctionMap>,
) -> Type {
    let ty = match expr {
        Expr::Ident(x) => vars.get(x).cloned().unwrap_or_else(|| var_not_found(x)),
        Expr::Num(_) => Type::Num,
        Expr::List(x) => {
            let mut old = None;
            let mut old_i = None;
            for i in x {
                let new = calc_type(i, vars, funcs);
                if old.clone().is_some_and(|x| x == new) || old.is_none() {
                    old = Some(new);
                    old_i = Some(i)
                } else {
                    #[allow(clippy::unnecessary_unwrap)]
                    mismatched_list_types(i, old_i.unwrap(), new, old.unwrap())
                }
            }
            Type::List(Box::new(old.unwrap_or(Type::Num)))
        }
        Expr::Pt2(x, y) => {
            let x_t = calc_type(x, vars, funcs);
            x.2 = x_t.clone();
            let y_t = calc_type(y, vars, funcs);
            y.2 = y_t.clone();
            if matches!(x_t, Type::List(_)) || matches!(y_t, Type::List(_)) {
                Type::List(Box::new(Type::Point))
            } else {
                Type::Point
            }
        }
        Expr::Pt3(x, y, z) => {
            let x_t = calc_type(x, vars, funcs);
            x.2 = x_t.clone();
            let y_t = calc_type(y, vars, funcs);
            y.2 = y_t.clone();
            let z_t = calc_type(y, vars, funcs);
            z.2 = z_t.clone();
            if matches!(x_t, Type::List(_))
                || matches!(y_t, Type::List(_))
                || matches!(z_t, Type::List(_))
            {
                Type::List(Box::new(Type::Point3))
            } else {
                Type::Point3
            }
        }
        Expr::Neg(x) => {
            let ty = calc_type(x, vars, funcs);
            x.2 = ty.clone();
            ty
        }
        Expr::Add(x, y) => add_sub(x, y, vars, funcs),
        Expr::Sub(x, y) => add_sub(x, y, vars, funcs),
        Expr::Div(x, y) => div_mul(x, y, vars, funcs),
        Expr::Mul(x, y) => div_mul(x, y, vars, funcs),
        Expr::Pow(x, y) => pow(x, y, vars, funcs),
        Expr::If {
            lh_cmp,
            rh_cmp,
            rrh_cmp,
            body,
            elifs,
            elsse,
            ..
        } => {
            let lh_ty = calc_type(lh_cmp, vars, funcs);
            let rh_ty = calc_type(rh_cmp, vars, funcs);
            if let Some(rrh_cmp) = rrh_cmp {
                let rrh_ty = calc_type(rrh_cmp, vars, funcs);
                if lh_ty != rh_ty {
                    mismatched_types(lh_cmp, rrh_cmp, lh_ty.clone(), rrh_ty, rh_ty)
                }
            };
            if lh_ty != rh_ty {
                mismatched_types(lh_cmp, rh_cmp, lh_ty.clone(), rh_ty, lh_ty)
            }
            let bod_ty = calc_type(body, vars, funcs);
            for Elif {
                lh_cmp,
                rh_cmp,
                rrh_cmp,
                body: ebody,
                ..
            } in elifs.iter_mut()
            {
                let elif_ty = calc_type(ebody, vars, funcs);
                if elif_ty != bod_ty {
                    mismatched_types(body, ebody, bod_ty.clone(), elif_ty, bod_ty)
                }

                let lh_ty = calc_type(lh_cmp, vars, funcs);
                let rh_ty = calc_type(rh_cmp, vars, funcs);
                if let Some(rrh_cmp) = rrh_cmp {
                    let rrh_ty = calc_type(rrh_cmp, vars, funcs);
                    if lh_ty != rh_ty {
                        mismatched_types(lh_cmp, rrh_cmp, lh_ty.clone(), rrh_ty, rh_ty)
                    }
                };
                if lh_ty != rh_ty {
                    mismatched_types(lh_cmp, rh_cmp, lh_ty.clone(), rh_ty, lh_ty)
                }
            }
            if let Some(elsse2) = elsse.as_mut() {
                let elsse_ty = calc_type(&mut elsse2.body, vars, funcs);
                if elsse_ty.clone() != bod_ty {
                    mismatched_types(body, &elsse2.body, bod_ty.clone(), elsse_ty, bod_ty)
                }
                elsse.as_mut().unwrap().body.2 = elsse_ty;
            }
            bod_ty
        }
        Expr::Call { name, params } => {
            let func = funcs
                .get(name)
                .unwrap_or_else(|| func_not_found(name.deref()));
            let mut ty_fs = vec![];
            let pars = func.params().clone();
            for (p_found, ty_e) in params.iter_mut().zip(pars.into_iter()) {
                let ty_f = calc_type(p_found, vars, funcs);
                if !ty_e.contains(&ty_f) {
                    wrong_func_type(func, p_found.deref(), &ty_f, ty_e)
                }
                ty_fs.push(ty_f);
            }
            func.mapper()(ty_fs)
        }
        Expr::Ineq { .. } => unreachable!(),
        Expr::Def { .. } => unreachable!(),
        Expr::Fold { .. } => unreachable!(),
        Expr::Note { .. } => Type::Infer,
    };
    *type_ = ty.clone();
    ty
}

fn wrong_func_type(
    func: &FunctionMap,
    found: &Spanned<Expr>,
    type_found: &Type,
    type_expected: Vec<Type>,
) -> ! {
    todo!()
}

fn func_not_found(name: &str) -> ! {
    todo!()
}

fn pow(
    x: &mut Spanned<Expr>,
    y: &mut Spanned<Expr>,
    vars: &mut HashMap<String, Type>,
    funcs: &mut HashMap<String, FunctionMap>,
) -> Type {
    let x_t = calc_type(x, vars, funcs);
    x.2 = x_t.clone();
    let y_t = calc_type(y, vars, funcs);
    y.2 = y_t.clone();
    match (x_t.clone(), y_t.clone()) {
        (Type::Action, _) => op_action(x, y),
        (_, Type::Action) => op_action(x, y),
        (Type::Num, Type::Num) => Type::Num,
        (Type::Point3, Type::Point3) => mismatched_types(x, y, x_t, y_t, Type::Num),
        (Type::Point, Type::Point) => mismatched_types(x, y, x_t, y_t, Type::Num),
        (Type::Infer, _) => unreachable!("[internal] found Infer type after calculating"),
        (_, Type::Infer) => unreachable!("[internal] found Infer type after calculating"),
        (Type::Ineq, _) => {
            unreachable!("[internal] Ineq type shouldn't be possible in pow")
        }
        (_, Type::Ineq) => {
            unreachable!("[internal] Ineq type shouldn't be possible in pow")
        }
        (Type::List(a), Type::Point3) if *a == Type::Point3 || *a == Type::Num => x_t,
        (Type::List(_), Type::Point) => mismatched_list_types(x, y, x_t, y_t),
        (Type::Num, Type::Point3) => Type::Point3,
        (Type::Num, Type::Point) => Type::Point,
        (Type::Num, Type::List(a))
            if *a == Type::Num || *a == Type::Point || *a == Type::Point3 =>
        {
            y_t
        }
        (Type::Num, Type::List(_)) => mismatched_list_types(x, y, x_t, y_t),
        (Type::Point3, Type::Num) => Type::Point3,
        (Type::Point3, Type::Point) => mismatched_types(x, y, x_t, y_t, Type::Point3),
        (Type::Point3, Type::List(a)) if *a == Type::Point3 || *a == Type::Num => y_t,
        (Type::Point3, Type::List(_)) => mismatched_list_types(x, y, x_t, y_t),
        (Type::Point, Type::Num) => Type::Point,
        (Type::Point, Type::Point3) => mismatched_types(x, y, x_t, y_t, Type::Point),
        (Type::Point, Type::List(a)) if *a == Type::Point || *a == Type::Num => y_t,
        (Type::Point, Type::List(_)) => mismatched_list_types(x, y, x_t, y_t),
        (Type::List(a), Type::Num)
            if *a == Type::Num || *a == Type::Point || *a == Type::Point3 =>
        {
            x_t
        }
        (Type::List(_), Type::Num) => mismatched_list_types(x, y, x_t, y_t),
        (Type::List(a), Type::Point3) if *a == Type::Point3 || *a == Type::Num => x_t,
        (Type::List(_), Type::Point3) => mismatched_list_types(x, y, x_t, y_t),
        (Type::List(a), Type::List(b)) => match (*a, *b) {
            (Type::List(_), _) => unreachable!(),
            (_, Type::List(_)) => unreachable!(),
            (Type::Ineq, _) => unreachable!(),
            (_, Type::Ineq) => unreachable!(),
            (Type::Infer, _) => unreachable!(),
            (_, Type::Infer) => unreachable!(),
            (Type::Action, _) => op_action(x, y),
            (_, Type::Action) => op_action(x, y),
            (Type::Point, Type::Num) => Type::Point,
            (Type::Num, Type::Point) => Type::Point,
            (Type::Point3, Type::Num) => Type::Point3,
            (Type::Num, Type::Point3) => Type::Point3,
            (Type::Point, _) => pow_point(x, y, x_t, y_t),
            (Type::Point3, _) => pow_point(x, y, x_t, y_t),
            (Type::Num, Type::Num) => Type::Num,
        },
    }
}

fn pow_point(a: &Spanned<Expr>, b: &Spanned<Expr>, a_t: Type, b_t: Type) -> ! {
    todo!()
}

fn div_mul(
    x: &mut Box<Spanned<Expr>>,
    y: &mut Box<Spanned<Expr>>,
    vars: &mut HashMap<String, Type>,
    funcs: &mut HashMap<String, FunctionMap>,
) -> Type {
    let x_t = calc_type(x, vars, funcs);
    x.2 = x_t.clone();
    let y_t = calc_type(y, vars, funcs);
    y.2 = y_t.clone();
    match (x_t.clone(), y_t.clone()) {
        (Type::Action, _) => op_action(x, y),
        (_, Type::Action) => op_action(x, y),
        (Type::Num, Type::Num) => Type::Num,
        (Type::Point3, Type::Point3) => mismatched_types(x, y, x_t, y_t, Type::Num),
        (Type::Point, Type::Point) => mismatched_types(x, y, x_t, y_t, Type::Num),
        (Type::Infer, _) => unreachable!("[internal] found Infer type after calculating"),
        (_, Type::Infer) => unreachable!("[internal] found Infer type after calculating"),
        (Type::Ineq, _) => {
            unreachable!("[internal] Ineq type shouldn't be possible in mul/div")
        }
        (_, Type::Ineq) => {
            unreachable!("[internal] Ineq type shouldn't be possible in mul/div")
        }
        (Type::List(a), Type::Point3) if *a == Type::Point3 || *a == Type::Num => x_t,
        (Type::List(_), Type::Point) => mismatched_list_types(x, y, x_t, y_t),
        (Type::Num, Type::Point3) => Type::Point3,
        (Type::Num, Type::Point) => Type::Point,
        (Type::Num, Type::List(a))
            if *a == Type::Num || *a == Type::Point || *a == Type::Point3 =>
        {
            y_t
        }
        (Type::Num, Type::List(_)) => mismatched_list_types(x, y, x_t, y_t),
        (Type::Point3, Type::Num) => Type::Point3,
        (Type::Point3, Type::Point) => mismatched_types(x, y, x_t, y_t, Type::Point3),
        (Type::Point3, Type::List(a)) if *a == Type::Point3 || *a == Type::Num => y_t,
        (Type::Point3, Type::List(_)) => mismatched_list_types(x, y, x_t, y_t),
        (Type::Point, Type::Num) => Type::Point,
        (Type::Point, Type::Point3) => mismatched_types(x, y, x_t, y_t, Type::Point),
        (Type::Point, Type::List(a)) if *a == Type::Point || *a == Type::Num => y_t,
        (Type::Point, Type::List(_)) => mismatched_list_types(x, y, x_t, y_t),
        (Type::List(a), Type::Num)
            if *a == Type::Num || *a == Type::Point || *a == Type::Point3 =>
        {
            x_t
        }
        (Type::List(_), Type::Num) => mismatched_list_types(x, y, x_t, y_t),
        (Type::List(a), Type::Point3) if *a == Type::Point3 || *a == Type::Num => x_t,
        (Type::List(_), Type::Point3) => mismatched_list_types(x, y, x_t, y_t),
        (Type::List(a), Type::List(b)) => match (*a, *b) {
            (Type::List(_), _) => unreachable!(),
            (_, Type::List(_)) => unreachable!(),
            (Type::Ineq, _) => unreachable!(),
            (_, Type::Ineq) => unreachable!(),
            (Type::Infer, _) => unreachable!(),
            (_, Type::Infer) => unreachable!(),
            (Type::Action, _) => op_action(x, y),
            (_, Type::Action) => op_action(x, y),
            (Type::Point, Type::Num) => Type::Point,
            (Type::Num, Type::Point) => Type::Point,
            (Type::Point3, Type::Num) => Type::Point3,
            (Type::Num, Type::Point3) => Type::Point3,
            (Type::Point, _) => mul_point(x, y, x_t, y_t),
            (Type::Point3, _) => mul_point(x, y, x_t, y_t),
            (Type::Num, Type::Num) => Type::Num,
        },
    }
}

fn mul_point(a: &Spanned<Expr>, b: &Spanned<Expr>, a_t: Type, b_t: Type) -> ! {
    todo!()
}

fn add_sub(
    x: &mut Box<Spanned<Expr>>,
    y: &mut Box<Spanned<Expr>>,
    vars: &mut HashMap<String, Type>,
    funcs: &mut HashMap<String, FunctionMap>,
) -> Type {
    let x_t = calc_type(x, vars, funcs);
    x.2 = x_t.clone();
    let y_t = calc_type(y, vars, funcs);
    y.2 = y_t.clone();
    match (x_t.clone(), y_t.clone()) {
        (Type::List(a), Type::List(b)) => {
            if a != b {
                mismatched_list_types(x, y, x_t, y_t);
            } else {
                x_t
            }
        }
        (Type::Action, _) => op_action(x, y),
        (_, Type::Action) => op_action(x, y),
        (Type::Num, Type::Num) => Type::Num,
        (Type::Point3, Type::Point3) => Type::Point3,
        (Type::Point, Type::Point) => Type::Point,
        (Type::Infer, _) => unreachable!("[internal] found Infer type after calculating"),
        (_, Type::Infer) => unreachable!("[internal] found Infer type after calculating"),
        (Type::Ineq, _) => {
            unreachable!("[internal] Ineq type shouldn't be possible in add")
        }
        (_, Type::Ineq) => {
            unreachable!("[internal] Ineq type shouldn't be possible in add")
        }
        (Type::List(a), Type::Point3) if *a == Type::Point3 => x_t,
        (Type::List(_), Type::Point) => mismatched_list_types(x, y, x_t, y_t),
        (Type::Num, Type::Point3) | (Type::Num, Type::Point) => {
            mismatched_types(x, y, x_t, y_t, Type::Num)
        }
        (Type::Num, Type::List(a)) if *a == Type::Num => y_t,
        (Type::Num, Type::List(_)) => mismatched_list_types(x, y, x_t, y_t),
        (Type::Point3, Type::Num) => mismatched_types(x, y, x_t, y_t, Type::Point3),
        (Type::Point3, Type::Point) => mismatched_types(x, y, x_t, y_t, Type::Point3),
        (Type::Point3, Type::List(a)) if *a == Type::Point3 => y_t,
        (Type::Point3, Type::List(_)) => mismatched_list_types(x, y, x_t, y_t),
        (Type::Point, Type::Num) => mismatched_types(x, y, x_t, y_t, Type::Point),
        (Type::Point, Type::Point3) => mismatched_types(x, y, x_t, y_t, Type::Point),
        (Type::Point, Type::List(a)) if *a == Type::Point => y_t,
        (Type::Point, Type::List(_)) => mismatched_list_types(x, y, x_t, y_t),
        (Type::List(a), Type::Num) if *a == Type::Num => x_t,
        (Type::List(_), Type::Num) => mismatched_list_types(x, y, x_t, y_t),
        (Type::List(a), Type::Point3) if *a == Type::Point3 => x_t,
        (Type::List(_), Type::Point3) => mismatched_list_types(x, y, x_t, y_t),
    }
}

fn mismatched_types(
    a: &Spanned<Expr>,
    b: &Spanned<Expr>,
    a_t: Type,
    b_t: Type,
    expected: Type,
) -> ! {
    todo!()
    // exit(1)
}

fn op_action(a: &Spanned<Expr>, b: &Spanned<Expr>) -> ! {
    todo!()
    // exit(1)
}

fn mismatched_list_types(a: &Spanned<Expr>, b: &Spanned<Expr>, a_t: Type, b_t: Type) -> ! {
    todo!()
    // exit(1)
}

fn var_not_found(x: &str) -> ! {
    todo!();
    // exit(1)
}

fn mismatch_body_type(fnn: &Spanned<Expr>) -> ! {
    todo!();
    // exit(1)
}
