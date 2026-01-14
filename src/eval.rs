use std::collections::HashMap;
use crate::lexer::ComparisonOp;
use crate::parser::{Expr, Spanned};
use std::sync::atomic::{AtomicU32, Ordering};

static ID_GEN: AtomicU32 = AtomicU32::new(0);

#[derive(Copy, Clone)]
pub struct Ids {
    pub id: u32,
    pub folder_id: Option<u32>,
}

fn eval(s: Spanned<Expr>, fid: Option<u32>) -> String {
    evalall(s, fid)[0].0.clone()
}

pub fn evalall(Spanned(e, _, _, sty): Spanned<Expr>, fid: Option<u32>) -> Vec<(String, Ids, HashMap<String, String>)> {
    let evalled = match e {
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
            let full = l
                .into_iter()
                .map(|x| eval(x, fid))
                .collect::<Vec<String>>()
                .join(",");
            format!("\\left[{full}\\right]")
        }
        Expr::Pt2(x, y) => format!("\\left({},{}\\right)", eval(*x, fid), eval(*y, fid)),
        Expr::Pt3(x, y, z) => format!(
            "\\left({},{},{}\\right)",
            eval(*x, fid),
            eval(*y, fid),
            eval(*z, fid)
        ),
        Expr::Neg(x) => format!("-{}", eval(*x, fid)),
        Expr::Add(x, y) => format!("{}+{}", eval(*x, fid), eval(*y, fid)),
        Expr::Sub(x, y) => format!("{}-{}", eval(*x, fid), eval(*y, fid)),
        Expr::Div(x, y) => format!("\\frac{{{}}}{{{}}}", eval(*x, fid), eval(*y, fid)),
        Expr::Mul(x, y) => format!(
            "\\left({}\\right)\\left({}\\right)",
            eval(*x, fid),
            eval(*y, fid)
        ),
        Expr::Pow(x, y) => format!("{}^{{{}}}", eval(*x, fid), eval(*y, fid)),
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
                    eval(elif.lh_cmp, fid),
                    ecmp(cmp),
                    eval(elif.rh_cmp, fid),
                    if let Some(cmp2) = elif.cmp2 {
                        ecmp(cmp2)
                    } else {
                        ""
                    },
                    if let Some(rrh_cmp) = elif.rrh_cmp {
                        eval(rrh_cmp, fid)
                    } else {
                        String::new()
                    },
                    eval(elif.body, fid)
                )
            }

            let else_s = if let Some(elsse) = elsse {
                format!(",{}", eval(elsse.body, fid))
            } else {
                String::new()
            };

            format!(
                "\\left\\{{{}{}{}{}{}:{}{}{}\\right\\}}",
                eval(*lh_cmp, fid),
                ecmp(cmp),
                eval(*rh_cmp, fid),
                if let Some(cmp2) = cmp2 {
                    ecmp(cmp2)
                } else {
                    ""
                },
                if let Some(rrh_cmp) = rrh_cmp {
                    eval(*rrh_cmp, fid)
                } else {
                    String::new()
                },
                eval(*body, fid),
                elifs_s,
                else_s
            )
        }
        Expr::Ineq { lhs, cmp, rhs } => {
            format!("{}{}{}", eval(*lhs, fid), ecmp(cmp), eval(*rhs, fid))
        }
        Expr::Def {
            mut name,
            args,
            body,
        } => {
            let (start, rest) = name.split_at(1);
            name = if rest.is_empty() {
                start.to_string()
            } else {
                format!("{start}_{{{rest}}}")
            };
            format!(
                "{}\\left({}\\right)={}",
                name,
                args.into_iter().map(|x| x.0).collect::<Vec<_>>().join(" "),
                eval(*body, fid)
            )
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
                    .map(|par| eval(par, fid))
                    .collect::<Vec<String>>()
                    .join(" ")
            )
        }
        Expr::Fold { name, body } => {
            let id = ID_GEN.fetch_add(1, Ordering::Relaxed);
            let mut contents = body
                .into_iter()
                .flat_map(|item| evalall(item, Some(id)))
                .collect::<Vec<_>>();
            let mut contents2 = vec![(
                format!("\\fold {name}"),
                Ids {
                    id,
                    folder_id: None,
                },
                sty
            )];
            contents2.append(&mut contents);
            return contents2;
        }
        Expr::Note { content } => {
            format!(
                "\\note {}",
                content
                    .strip_prefix(['\"', '\''])
                    .unwrap()
                    .strip_suffix(['\"', '\''])
                    .unwrap()
            )
        }
        Expr::Action(i, e) => format!("({}\\to {})",i, eval(*e,fid))
    };

    vec![(
        evalled,
        Ids {
            id: ID_GEN.fetch_add(1, Ordering::Relaxed),
            folder_id: fid,
        },
        sty
    )]
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
