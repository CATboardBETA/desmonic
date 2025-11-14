use crate::parser::{Expr, Spanned};
use ariadne::ColorGenerator;
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
        Expr::List(_) => {}
        Expr::Pt2(_, _) => {}
        Expr::Pt3(_, _, _) => {}
        Expr::Neg(x) => {
            exiting |= verify(x, flags, file);
        }
        Expr::Add(x, y) => {
            exiting |= verify(x, flags, file);
            exiting |= verify(y, flags, file);
        }
        Expr::Sub(x, y) => {
            exiting |= verify(x, flags, file);
            exiting |= verify(y, flags, file);
        }
        Expr::Div(x, y) => {
            exiting |= verify(x, flags, file);
            exiting |= verify(y, flags, file);
        }
        Expr::Mul(x, y) => {
            exiting |= verify(x, flags, file);
            exiting |= verify(y, flags, file);
        }
        Expr::Pow(x, y) => {
            exiting |= verify(x, flags, file);
            exiting |= verify(y, flags, file);
        }
        Expr::If { .. } => {}
        Expr::Ineq { .. } => {}
        Expr::Def { .. } => {}
        Expr::Call { .. } => {}
    }
    exiting
}
