use crate::lexer::ComparisonOp;
use crate::parser::{Elif, Expr, Spanned};
use ariadne::{ColorGenerator, Fmt, Label, Report, ReportKind, Source};
use log::error;
use std::collections::HashSet;
use std::fs;
use std::ops::Range;
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Flag {
    ListOfList(Range<usize>),
    PtOfPt(Range<usize>),
    FnInFn(Range<usize>),
}

impl Flag {
    fn list_of_list(&self) -> Range<usize> {
        if let Flag::ListOfList(it) = self {
            it.clone()
        } else {
            panic!()
        }
    }
    fn pt_of_pt(&self) -> Range<usize> {
        if let Flag::PtOfPt(it) = self {
            it.clone()
        } else {
            panic!()
        }
    }
    fn fn_in_fn(&self) -> Range<usize> {
        if let Flag::FnInFn(it) = self {
            it.clone()
        } else {
            panic!()
        }
    }
}

pub type VerifyFlags = HashSet<Flag>;

#[must_use]
pub fn verify(ast: &Spanned<Expr>, flags: &mut VerifyFlags, file: &String) -> bool {
    let x = &ast.0;
    let s = ast.1.into_range();
    let mut exiting = false;
    let mut cgen = ColorGenerator::new();
    let c1 = cgen.next();
    let c2 = cgen.next();
    let contents = fs::read_to_string(file).unwrap();
    match x {
        Expr::Ident(_) => {}
        Expr::Num(_) => {}
        Expr::List(l) => {
            let is_bad;
            if let Some(inner) = flags.iter().find(|x| matches!(x, Flag::ListOfList(_))) {
                is_bad = true;
                Report::build(ReportKind::Error, (file, s.clone()))
                    .with_message("Cannot have a list within a list")
                    .with_label(
                        Label::new((file, s.clone()))
                            .with_message(format!("Inner list {}", "here".fg(c2)))
                            .with_color(c2),
                    )
                    .with_label(
                        Label::new((file, inner.list_of_list()))
                            .with_message(format!("Outer list {}", "here".fg(c1)))
                            .with_color(c1)
                            .with_order(1),
                    )
                    .finish()
                    .print((file, Source::from(contents)))
                    .unwrap();
                exiting = true;
            } else {
                is_bad = false;
                flags.insert(Flag::ListOfList(s.clone()));
            }
            for x in l {
                exiting |= verify(x, flags, file)
            }
            if !is_bad {
                flags.remove(&Flag::ListOfList(s));
            }
        }
        Expr::Pt2(x, y) => {
            if flags.iter().any(|x| matches!(x, Flag::PtOfPt(_))) {
                // error!("[{s}] Error: Point of point");
                exiting = true
            } else {
                flags.insert(Flag::PtOfPt(s.clone()));
                exiting |= verify(x.as_ref(), flags, file);
                exiting |= verify(y.as_ref(), flags, file);
                flags.remove(&Flag::PtOfPt(s));
            }
        }
        Expr::Pt3(x, y, z) => {
            if flags.iter().any(|x| matches!(x, Flag::PtOfPt(_))) {
                // error!("[{s}] Error: Point of point");
                exiting = true
            } else {
                flags.insert(Flag::PtOfPt(s.clone()));
                exiting |= verify(x.as_ref(), flags, file);
                exiting |= verify(y.as_ref(), flags, file);
                exiting |= verify(z.as_ref(), flags, file);
                flags.remove(&Flag::PtOfPt(s));
            }
        }
        Expr::Neg(x) => {
            exiting |= verify(x, flags, file);
        }
        Expr::Add(x, y) => {
            exiting |= verify(x, flags, file);
            exiting |= verify(y, flags, file);}
        Expr::Sub(x, y) => {
            exiting |= verify(x, flags, file);
            exiting |= verify(y, flags, file);}
        Expr::Div(x, y) => {
            exiting |= verify(x, flags, file);
            exiting |= verify(y, flags, file);}
        Expr::Mul(x, y) => {
            exiting |= verify(x, flags, file);
            exiting |= verify(y, flags, file);
        }
        Expr::Pow(x, y) => {
            exiting |= verify(x, flags, file);
            exiting |= verify(y, flags, file);
        }
        Expr::If {
            lh_cmp, cmp, rh_cmp, cmp2, rrh_cmp, body, elifs, elsse
        } => {
            if matches!(cmp, ComparisonOp::IneqEq) {
                error!("You cannot use a single eq in an if statement");
                exiting = true
            }
            if matches!(cmp2.unwrap_or(ComparisonOp::Eq), ComparisonOp::IneqEq) {
                error!("You cannot use a single eq in an if statement");
                exiting = true
            }
            for elif in elifs {
                let Elif { lh_cmp, cmp, rh_cmp, cmp2, rrh_cmp, body } = elif;
                if matches!(cmp, ComparisonOp::IneqEq) {
                    error!("You cannot use a single eq in an if statement");
                    exiting = true
                }
                if matches!(cmp2.unwrap_or(ComparisonOp::Eq), ComparisonOp::IneqEq) {
                    error!("You cannot use a single eq in an if statement");
                    exiting = true
                }
                exiting |= verify(lh_cmp, flags, file);
                exiting |= verify(rh_cmp, flags, file);
                if let Some(rrh) = rrh_cmp {
                    exiting |= verify(rrh, flags, file);
                }
                exiting |= verify(body, flags, file);
            }
            exiting |= verify(lh_cmp, flags, file);
            exiting |= verify(rh_cmp, flags, file);
            if let Some(rrh) = rrh_cmp {
                exiting |= verify(rrh, flags, file);
            }
            exiting |= verify(body, flags, file);
            if let Some(elsse) = elsse {
                exiting |= verify(&elsse.body, flags, file);
            }
        }
        Expr::Ineq { cmp, .. } => {
            if matches!(cmp, ComparisonOp::Eq) {
                error!("You cannot use a double eq in an inequality");
                exiting = true
            }
        }
        Expr::Def { body, .. } => {
            if flags.iter().any(|x| matches!(x, Flag::FnInFn(_))) {
                error!("[internal] Functions within functions are not yet implemented.");
                exiting = true
            }

            flags.insert(Flag::FnInFn(s.clone()));
            exiting |= verify(body, flags, file);
            flags.remove(&Flag::FnInFn(s));
        }
        Expr::Call { .. } => {}
    }
    exiting
}
