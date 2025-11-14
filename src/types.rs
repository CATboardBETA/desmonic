use crate::lexer::{ComparisonOp, Type};
use crate::parser::{Expr, Spanned};
use std::collections::HashMap;

pub fn infer_types(spanned: &mut Spanned<Expr>, vars: &mut HashMap<String, Type>) {
    match spanned {
        Spanned(Expr::Ineq { lhs, cmp, rhs }, _, _) => {
            if *cmp == ComparisonOp::IneqEq {
                if let Expr::Ident(x) = &lhs.0 {
                    let rh_type = calc_type(rhs, vars);
                    rhs.2 = rh_type.clone();
                    lhs.2 = rh_type.clone();
                    spanned.2 = rh_type.clone();
                    vars.insert(x.clone(), rh_type);
                }
            } else {
                spanned.2 = Type::Ineq
            }
        }
        Spanned(Expr::Def { args, body, .. }, _, t) => {
            for arg in args.iter() {
                vars.insert(arg.0.clone(), arg.1.clone());
            }
            let bod_type = calc_type(body, vars);
            if *t != bod_type {
                mismatch_body_type(spanned)
            }
            body.2 = bod_type;
            for arg in args {
                vars.remove(&arg.0);
            }
        }
        _ => {
            spanned.2 = calc_type(spanned, vars);
        }
    }
}

fn calc_type(
    Spanned(expr, _span, _type_): &mut Spanned<Expr>,
    vars: &mut HashMap<String, Type>,
) -> Type {
    match expr {
        Expr::Ident(x) => vars.get(x).cloned().unwrap_or_else(|| var_not_found(x)),
        Expr::Num(_) => Type::Num,
        Expr::List(x) => {
            let mut old = None;
            let mut old_i = None;
            for i in x {
                let new = calc_type(i, vars);
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
            let x_t = calc_type(x, vars);
            x.2 = x_t.clone();
            let y_t = calc_type(y, vars);
            y.2 = y_t.clone();
            if matches!(x_t, Type::List(_)) || matches!(y_t, Type::List(_)) {
                Type::List(Box::new(Type::Point))
            } else {
                Type::Point
            }
        }
        Expr::Pt3(x, y, z) => {
            let x_t = calc_type(x, vars);
            x.2 = x_t.clone();
            let y_t = calc_type(y, vars);
            y.2 = y_t.clone();
            let z_t = calc_type(y, vars);
            z.2 = z_t.clone();
            if matches!(x_t, Type::List(_))
                || matches!(y_t, Type::List(_))
                || matches!(z_t, Type::List(_))
            {
                Type::List(Box::new(Type::Point))
            } else {
                Type::Point
            }
        }
        Expr::Neg(x) => {
            let ty = calc_type(x, vars);
            x.2 = ty.clone();
            ty
        }
        Expr::Add(x, y) => add_sub(x, y, vars),
        Expr::Sub(x, y) => add_sub(x, y, vars),
        Expr::Div(x, y) => div_mul(x, y, vars),
        Expr::Mul(x, y) => div_mul(x, y, vars),
        Expr::Pow(x, y) => pow(x, y, vars),
        Expr::If { .. } => todo!(),
        Expr::Call { .. } => todo!(),
        Expr::Ineq { .. } => todo!(),
        Expr::Def { .. } => todo!(),
    }
}

fn pow(
    x: &mut Box<Spanned<Expr>>,
    y: &mut Box<Spanned<Expr>>,
    vars: &mut HashMap<String, Type>,
) -> Type {
    let x_t = calc_type(x, vars);
    x.2 = x_t.clone();
    let y_t = calc_type(y, vars);
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
        (Type::List(a), Type::Point3)
            if *a == Type::Point3 || *a == Type::Num =>
        {
            x_t
        }
        (Type::List(_), Type::Point) => mismatched_list_types(x, y, x_t, y_t),
        (Type::Num, Type::Point3) => Type::Point3,
        (Type::Num, Type::Point) => Type::Point,
        (Type::Num, Type::List(a))
            if *a == Type::Num
                || *a == Type::Point
                || *a == Type::Point3 =>
        {
            y_t
        }
        (Type::Num, Type::List(_)) => mismatched_list_types(x, y, x_t, y_t),
        (Type::Point3, Type::Num) => Type::Point3,
        (Type::Point3, Type::Point) => mismatched_types(x, y, x_t, y_t, Type::Point3),
        (Type::Point3, Type::List(a))
            if *a == Type::Point3 || *a == Type::Num =>
        {
            y_t
        }
        (Type::Point3, Type::List(_)) => mismatched_list_types(x, y, x_t, y_t),
        (Type::Point, Type::Num) => Type::Point,
        (Type::Point, Type::Point3) => mismatched_types(x, y, x_t, y_t, Type::Point),
        (Type::Point, Type::List(a)) if *a == Type::Point || *a == Type::Num => {
            y_t
        }
        (Type::Point, Type::List(_)) => mismatched_list_types(x, y, x_t, y_t),
        (Type::List(a), Type::Num)
            if *a == Type::Num
                || *a == Type::Point
                || *a == Type::Point3 =>
        {
            x_t
        }
        (Type::List(_), Type::Num) => mismatched_list_types(x, y, x_t, y_t),
        (Type::List(a), Type::Point3)
            if *a == Type::Point3 || *a == Type::Num =>
        {
            x_t
        }
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
) -> Type {
    let x_t = calc_type(x, vars);
    x.2 = x_t.clone();
    let y_t = calc_type(y, vars);
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
        (Type::List(a), Type::Point3)
            if *a == Type::Point3 || *a == Type::Num =>
        {
            x_t
        }
        (Type::List(_), Type::Point) => mismatched_list_types(x, y, x_t, y_t),
        (Type::Num, Type::Point3) => Type::Point3,
        (Type::Num, Type::Point) => Type::Point,
        (Type::Num, Type::List(a))
            if *a == Type::Num
                || *a == Type::Point
                || *a == Type::Point3 =>
        {
            y_t
        }
        (Type::Num, Type::List(_)) => mismatched_list_types(x, y, x_t, y_t),
        (Type::Point3, Type::Num) => Type::Point3,
        (Type::Point3, Type::Point) => mismatched_types(x, y, x_t, y_t, Type::Point3),
        (Type::Point3, Type::List(a))
            if *a == Type::Point3 || *a == Type::Num =>
        {
            y_t
        }
        (Type::Point3, Type::List(_)) => mismatched_list_types(x, y, x_t, y_t),
        (Type::Point, Type::Num) => Type::Point,
        (Type::Point, Type::Point3) => mismatched_types(x, y, x_t, y_t, Type::Point),
        (Type::Point, Type::List(a)) if *a == Type::Point || *a == Type::Num => {
            y_t
        }
        (Type::Point, Type::List(_)) => mismatched_list_types(x, y, x_t, y_t),
        (Type::List(a), Type::Num)
            if *a == Type::Num
                || *a == Type::Point
                || *a == Type::Point3 =>
        {
            x_t
        }
        (Type::List(_), Type::Num) => mismatched_list_types(x, y, x_t, y_t),
        (Type::List(a), Type::Point3)
            if *a == Type::Point3 || *a == Type::Num =>
        {
            x_t
        }
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
) -> Type {
    let x_t = calc_type(x, vars);
    x.2 = x_t.clone();
    let y_t = calc_type(y, vars);
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