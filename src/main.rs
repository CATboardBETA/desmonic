#![allow(dead_code)]

use crate::eval::evalall;
use crate::lexer::{Type, lex};
use crate::parser::parse;
use crate::state::ToGraphStateJson;
use crate::types::infer_types;
use clap::builder::styling;
use clap::{Parser, Subcommand};
use log::{LevelFilter, debug, info, trace};
use std::collections::HashMap;
use std::path::Path;

mod eval;
mod lexer;
mod parser;
mod state;
mod types;

const STYLES: styling::Styles = styling::Styles::styled()
    .header(styling::AnsiColor::Green.on_default().bold())
    .usage(styling::AnsiColor::Green.on_default().bold())
    .literal(styling::AnsiColor::Blue.on_default().bold())
    .placeholder(styling::AnsiColor::Cyan.on_default());

#[derive(Parser, Debug)]
#[command(version, about, long_about = None, styles = STYLES)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    #[command(arg_required_else_help = true, visible_alias = "b")]
    /// Compiles a desmonic file to graphstate JSON
    Build {
        /// The file to compile
        input: String,
        /// The file to output json to. Defaults to <input>.json
        output: Option<String>,
        /// Display debug info
        #[arg(short, long)]
        verbose: bool,
    },
    #[command(arg_required_else_help = true, visible_alias = "o")]
    /// Starts a webserver hosting the compiled JSON
    Open {
        /// The file to open
        input: String,
        /// The port to open the webserver to
        #[arg(short, long, default_value_t = 4444)]
        port: u16,
        /// Display debug info
        #[arg(short, long)]
        verbose: bool,
    },
    #[command(arg_required_else_help = true, visible_alias = "r")]
    /// Compiles a desmonic file and hosts the graphstate to a local webserver.
    Run {
        /// The file to open
        input: String,
        /// The file to output json to. Defaults to <input>.json
        output: Option<String>,
        /// The port to open the webserver to
        #[arg(short, long, default_value_t = 4444)]
        port: u16,
        /// Display debug info
        #[arg(short, long)]
        verbose: bool,
    },
}

fn main() {
    let args = Args::parse();

    colog::default_builder()
        .filter_level(LevelFilter::Debug)
        .init();

    match args.command {
        Commands::Build {
            input,
            output,
            verbose,
        } => compile(input, output, verbose),
        Commands::Open {
            input,
            port,
            verbose,
        } => open(input, port, verbose),
        Commands::Run {
            input,
            output,
            port,
            verbose,
        } => run(input, output, port, verbose),
    }
}

fn run(_input: String, _output: Option<String>, _port: u16, _v: bool) {
    todo!()
}

fn open(_input: String, _port: u16, _v: bool) {
    todo!()
}

fn compile(input: String, output: Option<String>, v: bool) {
    let lexed = lex(&input, v);
    if v {
        trace!("Lexed output: {lexed:?}");
    }
    let mut parsed = parse(lexed, v);
    if v {
        trace!("Parsed output:\n{parsed:#?}");
        info!("Verifying AST...")
    }
    let mut vars = HashMap::from([("x".to_string(), Type::Num), ("y".to_string(), Type::Num)]);
    let mut funcs = HashMap::from([]);
    for expr in &mut parsed {
        infer_types(expr, &mut vars, &mut funcs);
        debug!("Types: {expr}");
    }
    let mut evalled = vec![];
    for expr in parsed {
        let x = evalall(expr);
        evalled.push(x.clone());
    }
    let gstate = evalled.into_graph_state();
    let output_file =
        output.unwrap_or_else(|| format!("{}.json", input.trim_end_matches(".desm")));
    std::fs::write(output_file, gstate).unwrap();
}
