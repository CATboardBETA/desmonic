use log::{error, info};
use logos::Logos;
use std::fs;
use std::path::Path;
use std::str::FromStr;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ComparisonOp {
    Le,
    Ge,
    Lt,
    Gt,
    Eq,
    IneqEq
}

impl FromStr for ComparisonOp {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use ComparisonOp::*;
        match s {
            "<=" => Ok(Le),
            ">=" => Ok(Ge),
            "<" => Ok(Lt),
            ">" => Ok(Gt),
            "=="=> Ok(Eq),
            "=" => Ok(IneqEq),
            _ => unreachable!("infallible, due to logos"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Keyword {
    If,
    Elif,
    Else,
}

impl FromStr for Keyword {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Keyword::*;
        match s {
            "if" => Ok(If),
            "elif" => Ok(Elif),
            "else" => Ok(Else),

            _ => unreachable!("infallible, due to logos"),
        }
    }
}

#[derive(Clone, Logos, Debug, PartialEq)]
#[logos(skip r"[[:space:]]+")]
pub enum Token {
    #[regex(r"if|elif|else", |lex| lex.slice().parse::<Keyword>().unwrap(), priority = 10000)]
    Keyword(Keyword),
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("/")]
    Divide,
    #[token("*")]
    Multiply,
    #[token("^")]
    Power,
    #[regex(r"[[:alpha:]][[:alnum:]]*", |lex| lex.slice().to_owned())]
    Ident(String),
    #[regex(r"<=|>=|<|>|==|=", |lex| lex.slice().parse::<ComparisonOp>().unwrap(), priority = 1)]
    Comparison(ComparisonOp),
    #[regex(r"-?(?:\d+(?:\.\d*)?|\.\d+)", |lex| lex.slice().to_owned())]
    Num(String),
}

pub fn lex<T: AsRef<Path>>(input: T, v: bool) -> Vec<Token> {
    if v {
        info!("Lexing input...")
    }
    let path = input.as_ref();
    if !path.exists() {
        error!("File '{}' is broken or does not exist!", path.display());
    }

    let mut tokens = vec![];
    for (token, span) in Token::lexer(&fs::read_to_string(path).unwrap()).spanned() {
        match token {
            Ok(token) => tokens.push(token),
            Err(e) => {
                error!("[byte {span:?}] Unexpected lexer error");
            }
        }
    }
    tokens
}
