use crate::lexer::ComparisonOp;
use crate::parser::{Expr, Spanned};
use std::ops::Deref;

pub fn eval(Spanned(e, _): Spanned<Expr>) -> String {
    match e {
        Expr::Ident(x) => {
            let (start, rest) = x.split_at(1);
            if rest.is_empty() {
                start.to_string()
            } else {
                format!("{start}_{{{rest}}}")
            }
        }
        Expr::Num(n) => n,
        Expr::List(l) => {
            let mut full = String::new();
            for x in l {
                full += &(eval(x) + ",")
            }
            format!("\\left[{full}\\right]")
        }
        Expr::Pt2(x, y) => format!("\\left({},{}\\right)", eval(*x), eval(*y)),
        Expr::Pt3(x, y, z) => format!("\\left({},{},{}\\right)", eval(*x), eval(*y), eval(*z)),
        Expr::Neg(x) => format!("-{}", eval(*x)),
        Expr::Add(x, y) => format!("{}+{}", eval(*x), eval(*y)),
        Expr::Sub(x, y) => format!("{}-{}", eval(*x), eval(*y)),
        Expr::Div(x, y) => format!("\\frac{{{}}}{{{}}}", eval(*x), eval(*y)),
        Expr::Mul(x, y) => format!("\\left({}\\right)\\left({}\\right)", eval(*x), eval(*y)),
        Expr::Pow(x, y) => format!("{}^{{{}}}", eval(*x), eval(*y)),
        Expr::If {
            lh_cmp,
            cmp,
            rh_cmp,
            cmp2,
            rrh_cmp,
            body,
            elifs,
            elsse,
        } => {
            let mut elifs_s = String::new();
            for elif in elifs {
                elifs_s += &format!(
                    ",{}{}{}{}{}:{}",
                    eval(elif.lh_cmp),
                    ecmp(cmp),
                    eval(elif.rh_cmp),
                    if let Some(cmp2) = elif.cmp2 {
                        ecmp(cmp2)
                    } else {
                        ""
                    },
                    if let Some(rrh_cmp) = elif.rrh_cmp {
                        eval(rrh_cmp)
                    } else {
                        String::new()
                    },
                    eval(elif.body)
                )
            }

            let else_s = if let Some(elsse) = elsse {
                format!(",{}", eval(elsse.body))
            } else {
                String::new()
            };

            format!(
                "\\left\\{{{}{}{}{}{}:{}{}{}\\right\\}}",
                eval(*lh_cmp),
                ecmp(cmp),
                eval(*rh_cmp),
                if let Some(cmp2) = cmp2 {
                    ecmp(cmp2)
                } else {
                    ""
                },
                if let Some(rrh_cmp) = rrh_cmp {
                    eval(*rrh_cmp)
                } else {
                    String::new()
                },
                eval(*body),
                elifs_s,
                else_s
            )
        }
        Expr::Ineq { lhs, cmp, rhs } => format!("{}{}{}", eval(*lhs), ecmp(cmp), eval(*rhs)),
        Expr::Def { mut name, args, body } => {
            let (start, rest) = name.split_at(1);
            name = if rest.is_empty() {
                start.to_string()
            } else {
                format!("{start}_{{{rest}}}")
            };
            format!("{}\\left({}\\right)={}", name, args.join(" "), eval(*body))
        }
        Expr::Call { mut name, params } => {
            let (start, rest) = name.split_at(1);
            name = if rest.is_empty() {
                start.to_string()
            } else {
                format!("{start}_{{{rest}}}")
            };
            format!(
                "{}\\left({}\\right)",
                name,
                params
                    .into_iter()
                    .map(eval)
                    .collect::<Vec<String>>()
                    .join(" ")
            )
        },
    }
}

pub fn ecmp(cmp: ComparisonOp) -> &'static str {
    match cmp {
        ComparisonOp::Le => "\\le ",
        ComparisonOp::Ge => "\\le ",
        ComparisonOp::Lt => "\\lt ",
        ComparisonOp::Gt => "\\gt ",
        ComparisonOp::Eq | ComparisonOp::IneqEq => " = ",
    }
}
