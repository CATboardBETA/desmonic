#![allow(dead_code)]
use crate::lexer::Token;
use chumsky::prelude::*;
use log::{error, info};
use std::process;

#[derive(Debug)]
pub enum Expr {
    // Datatypes

    // Note that this time we never bother to convert it to and from a float;
    // This will be changed when code optimizations are implemented
    Num(String),
    List(Vec<Expr>),
    Pt2(Box<Expr>, Box<Expr>),
    Pt3(Box<Expr>, Box<Expr>, Box<Expr>),
    // Unary Expressions
    Neg(Box<Expr>),

    // Binary Expressions
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Pow(Box<Expr>, Box<Expr>),
}

pub fn parse(input: Vec<Token>, v: bool) -> Expr {
    if v {
        info!("Parsing input...");
    }

    // TODO: Make this good >:3
    let output = parser().parse(&input).into_result();
    let ret = if let Ok(output) = output {
        output
    } else {
        for err in output.err().unwrap() {
            error!("Error occured in parsing: {err}");
        }
        process::exit(1)
    };

    if v {
        info!("Done.");
    }
    ret
}

fn parser<'src>() -> impl Parser<'src, &'src [Token], Expr> {
    use crate::lexer::Token as Tk;
    recursive(|p| {
        let atom = {
            let parenthesized = p.clone().delimited_by(just(Tk::LParen), just(Tk::RParen));
            let list = p
                .clone()
                .separated_by(just(Tk::Comma))
                .collect::<Vec<_>>()
                .map(Expr::List)
                .delimited_by(just(Tk::LBracket), just(Tk::RBracket));
            let pt2 = p
                .clone()
                .then_ignore(just(Tk::Comma))
                .then(p.clone())
                .map(|(x, y)| Expr::Pt2(bx(x), bx(y)));
            let pt3 = p
                .clone()
                .then_ignore(just(Tk::Comma))
                .then(p.clone())
                .then_ignore(just(Tk::Comma))
                .then(p.clone())
                .map(|((x, y), z)| Expr::Pt3(bx(x), bx(y), bx(z)));
            let num = select! {Tk::Num(n) => Expr::Num(n)};
            pt2.or(pt3).or(parenthesized).or(list.boxed()).or(num)
        };

        let unary = just(Tk::Minus)
            .repeated()
            .foldr(atom, |_op, rhs| Expr::Neg(bx(rhs)));

        let pow = unary
            .clone()
            .foldl(just(Tk::Power).then(unary).repeated(), |lhs, (_op, rhs)| {
                Expr::Pow(bx(lhs), bx(rhs))
            });

        let product = pow.clone().foldl(
            just(Tk::Multiply).or(just(Tk::Divide)).then(pow).repeated(),
            |lhs, (op, rhs)| match op {
                Tk::Multiply => Expr::Mul(bx(lhs), bx(rhs)),
                Tk::Divide => Expr::Div(bx(lhs), bx(rhs)),
                _ => unreachable!(),
            },
        );

        product.clone().foldl(
            just(Tk::Plus).or(just(Tk::Minus)).then(product).repeated(),
            |lhs, (op, rhs)| match op {
                Tk::Plus => Expr::Add(bx(lhs), bx(rhs)),
                Tk::Minus => Expr::Sub(bx(lhs), bx(rhs)),
                _ => unreachable!(),
            },
        )
    })
}

fn bx<T>(x: T) -> Box<T> {
    Box::new(x)
}
