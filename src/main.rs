#![allow(dead_code)]

use crate::eval::evalall;
use crate::func::builtin_funcs;
use crate::lexer::{Type, lex};
use crate::parser::parse;
use crate::state::ToGraphStateJson;
use crate::types::infer_types;
use clap::builder::styling;
use clap::{Parser, Subcommand};
use log::{LevelFilter, info, trace};
use rocket::response::Responder;
use rocket::{Config, Request, Response, get, routes};
use std::collections::HashMap;
use std::fs::read_to_string;
use std::sync::Mutex;

mod eval;
mod func;
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
        #[arg(short, long, default_value_t = 8000)]
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
        #[arg(short, long, default_value_t = 8000)]
        port: u16,
        /// Display debug info
        #[arg(short, long)]
        verbose: bool,
    },
}

#[rocket::main]
async fn main() {
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
        } => open(input, port, verbose).await,
        Commands::Run {
            input,
            output,
            port,
            verbose,
        } => run(input, output, port, verbose).await,
    }
}

async fn run(input: String, output: Option<String>, port: u16, v: bool) {
    compile(input.clone(), output.clone(), v);
    open(
        output.unwrap_or_else(|| format!("{}.json", input.trim_end_matches(".desm"))),
        port,
        v,
    )
    .await
}

async fn open(input: String, port: u16, _v: bool) {
    *DATA_OUT.lock().unwrap() = read_to_string(&input).unwrap();
    let _rocket = rocket::build()
        .configure(
            Config::figment()
                .merge(("port", port))
                .merge(("log_level", "off")),
        )
        .mount("/", routes![data])
        .launch()
        .await;
}

static DATA_OUT: Mutex<String> = Mutex::new(String::new());

struct GraphStateResponse<R>(pub R);

impl<'r, 'o: 'r, R: Responder<'r, 'o>> Responder<'r, 'o> for GraphStateResponse<R> {
    fn respond_to(self, req: &'r Request<'_>) -> rocket::response::Result<'o> {
        Response::build_from(self.0.respond_to(req)?)
            .raw_header("Access-Control-Allow-Origin", "*")
            .ok()
    }
}

#[get("/")]
fn data() -> GraphStateResponse<String> {
    GraphStateResponse(DATA_OUT.lock().unwrap().clone())
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

    let builtins = builtin_funcs();
    let mut funcs = HashMap::from_iter(
        builtins
            .into_iter()
            .map(|func| (func.0.to_string(), func.1)),
    );
    for expr in &mut parsed {
        infer_types(expr, &mut vars, &mut funcs);
        println!("{expr}")
    }
    let mut evalled = vec![];
    for expr in parsed {
        let x = evalall(expr, None);
        evalled.push(x.clone());
    }
    let evalled = evalled.into_iter().flatten().collect::<Vec<_>>();
    let gstate = evalled.into_graph_state();
    let output_file = output.unwrap_or_else(|| format!("{}.json", input.trim_end_matches(".desm")));
    std::fs::write(output_file, gstate).unwrap();
}
