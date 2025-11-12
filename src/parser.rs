#![allow(dead_code)]

use crate::lexer::{ComparisonOp, Keyword, Token};
use chumsky::prelude::*;
use log::{error, info};
use std::ops::Deref;
use std::process::exit;

#[derive(Debug, PartialEq)]
pub struct Elif {
    pub lh_cmp: Spanned<Expr>,
    pub cmp: ComparisonOp,
    pub rh_cmp: Spanned<Expr>,
    pub cmp2: Option<ComparisonOp>,
    pub rrh_cmp: Option<Spanned<Expr>>,
    pub body: Spanned<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct Else {
    pub body: Spanned<Expr>,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    // Datatypes

    // Note that this time we never bother to convert it to and from a float;
    // This will be changed when code optimizations are implemented
    Ident(String),
    Num(String),
    List(Vec<Spanned<Expr>>),
    Pt2(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Pt3(Box<Spanned<Expr>>, Box<Spanned<Expr>>, Box<Spanned<Expr>>),

    // Unary Expressions
    Neg(Box<Spanned<Expr>>),

    // Binary Expressions
    Add(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Sub(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Div(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Mul(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Pow(Box<Spanned<Expr>>, Box<Spanned<Expr>>),

    // Fun expressions
    If {
        lh_cmp: Box<Spanned<Expr>>,
        cmp: ComparisonOp,
        rh_cmp: Box<Spanned<Expr>>,
        cmp2: Option<ComparisonOp>,
        rrh_cmp: Option<Box<Spanned<Expr>>>,
        body: Box<Spanned<Expr>>,
        elifs: Vec<Elif>,
        elsse: Option<Box<Else>>,
    },
    Call {
        name: String,
        params: Vec<Spanned<Expr>>,
    },

    // Statements
    Ineq {
        lhs: Box<Spanned<Expr>>,
        cmp: ComparisonOp,
        rhs: Box<Spanned<Expr>>,
    },
    Def {
        name: String,
        args: Vec<String>,
        body: Box<Spanned<Expr>>,
    },
}

#[derive(Debug, PartialEq)]
pub struct Spanned<T>(pub T, pub SimpleSpan<usize>);

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub fn parse(input: Vec<Token>, v: bool) -> Vec<Spanned<Expr>> {
    if v {
        info!("Parsing input...");
    }

    // TODO: Make this good >:3
    let output = parser().parse(&input).into_result();
    if let Ok(output) = output {
        output
    } else {
        for err in output.err().unwrap() {
            error!("Error occured in parsing: {err:?}");
        }
        exit(1)
    }
}

fn parser<'src>()
-> impl Parser<'src, &'src [Token], Vec<Spanned<Expr>>, extra::Full<Rich<'src, Token>, (), ()>> {
    use crate::lexer::Token as Tk;
    let expr = recursive(|p| {
        let atom = {
            let parenthesized = p
                .clone()
                .map(|x: Spanned<Expr>| x.0)
                .delimited_by(just(Tk::LParen), just(Tk::RParen));
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
                .delimited_by(just(Tk::LParen), just(Tk::RParen))
                .map(|(x, y)| Expr::Pt2(bx(x), bx(y)));
            let pt3 = p
                .clone()
                .then_ignore(just(Tk::Comma))
                .then(p.clone())
                .then_ignore(just(Tk::Comma))
                .then(p.clone())
                .delimited_by(just(Tk::LParen), just(Tk::RParen))
                .map(|((x, y), z)| Expr::Pt3(bx(x), bx(y), bx(z)));
            let num = select! {Tk::Num(n) => Expr::Num(n)};
            let ident = select! {Tk::Ident(s) => Expr::Ident(s)};
            let call = select! {Tk::Ident(s) => s}
                .then(
                    p.clone()
                        .separated_by(just(Tk::Comma))
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .delimited_by(just(Tk::LParen), just(Tk::RParen)),
                )
                .map(|(name, params)| Expr::Call { name, params });
            choice((pt3, pt2, list, parenthesized, num, call, ident))
                .map_with(|x, e| Spanned(x, e.span()))
        };

        let comp = select! {
            Tk::Comparison(c) => c
        };

        // TODO: Make if/elif handle more than one expr in body
        let elif = just(Tk::Keyword(Keyword::Elif))
            .ignore_then(p.clone())
            .then(comp)
            .then(p.clone())
            .then(comp.or_not())
            .then(p.clone().or_not())
            .then_ignore(just(Tk::LBrace))
            .then(
                p.clone(), /*.repeated().at_least(1).collect::<Vec<_>>()*/
            )
            .then_ignore(just(Tk::RBrace))
            .map(|(((((lh_cmp, cmp), rh_cmp), cmp2), rrh_cmp), body)| Elif {
                lh_cmp,
                cmp,
                rh_cmp,
                cmp2,
                rrh_cmp,
                body,
            });

        let elsse = just(Tk::Keyword(Keyword::Else))
            .ignore_then(just(Tk::LBrace))
            .ignore_then(
                p.clone(), /*.repeated().at_least(1).collect::<Vec<_>>()*/
            )
            .then_ignore(just(Tk::RBrace))
            .map(|body| Else { body });

        let iff = just(Tk::Keyword(Keyword::If))
            .ignore_then(p.clone())
            .then(comp)
            .then(p.clone())
            .then(comp.or_not())
            .then(p.clone().or_not())
            .then_ignore(just(Tk::LBrace))
            .then(
                p.clone(), /*.repeated().at_least(1).collect::<Vec<_>>()*/
            )
            .then_ignore(just(Tk::RBrace))
            .then(elif.repeated().collect::<Vec<_>>())
            .then(elsse.or_not())
            .map(
                |(((((((lh_cmp, cmp), rh_cmp), cmp2), rrh_cmp), body), elifs), elsse)| Expr::If {
                    lh_cmp: bx(lh_cmp),
                    cmp,
                    rh_cmp: bx(rh_cmp),
                    cmp2,
                    rrh_cmp: rrh_cmp.map(bx),
                    body: bx(body),
                    elifs,
                    elsse: elsse.map(bx),
                },
            )
            .map_with(|x, e| Spanned(x, e.span()))
            .boxed();

        let unary = just(Tk::Minus)
            .repeated()
            .foldr_with(atom, |_op, rhs, e| Spanned(Expr::Neg(bx(rhs)), e.span()));

        let pow = unary.clone().foldl_with(
            just(Tk::Power).then(unary).repeated(),
            |lhs, (_op, rhs), e| Spanned(Expr::Pow(bx(lhs), bx(rhs)), e.span()),
        );

        let product = pow.clone().foldl_with(
            just(Tk::Multiply).or(just(Tk::Divide)).then(pow).repeated(),
            |lhs, (op, rhs), e| {
                Spanned(
                    match op {
                        Tk::Multiply => Expr::Mul(bx(lhs), bx(rhs)),
                        Tk::Divide => Expr::Div(bx(lhs), bx(rhs)),
                        _ => unreachable!(),
                    },
                    e.span(),
                )
            },
        );

        let out = product
            .clone()
            .foldl_with(
                just(Tk::Plus).or(just(Tk::Minus)).then(product).repeated(),
                |lhs, (op, rhs), e| {
                    Spanned(
                        match op {
                            Tk::Plus => Expr::Add(bx(lhs), bx(rhs)),
                            Tk::Minus => Expr::Sub(bx(lhs), bx(rhs)),
                            _ => unreachable!(),
                        },
                        e.span(),
                    )
                },
            )
            .boxed();
        out.or(iff)
    })
    .boxed();
    let stmt = choice((
        expr.clone()
            .then(select! {
                Tk::Comparison(c) => c
            })
            .then(expr.clone())
            .map_with(|((lhs, cmp), rhs), e| {
                Spanned(
                    Expr::Ineq {
                        lhs: bx(lhs),
                        cmp,
                        rhs: bx(rhs),
                    },
                    e.span(),
                )
            }),
        just(Tk::Keyword(Keyword::Fn))
            .ignore_then(select! { Tk::Ident(s) => s })
            .then_ignore(just(Tk::LParen))
            .then(
                select! { Tk::Ident(s) => s }
                    .separated_by(just(Tk::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(Tk::RParen))
            .then(
                expr.clone()
                    .delimited_by(just(Tk::LBrace), just(Tk::RBrace)),
            )
            .map_with(|((name, args), body), e| {
                Spanned(
                    Expr::Def {
                        name,
                        args,
                        body: bx(body),
                    },
                    e.span(),
                )
            }),
    ));
    stmt.or(expr).repeated().collect::<Vec<_>>()
}

fn bx<T>(x: T) -> Box<T> {
    Box::new(x)
}
