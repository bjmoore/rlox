mod environment;
mod error;
mod expression;
mod interpreter;
mod parser;
mod statement;
mod token;
mod value;

use std::env;
use std::error::Error;
use std::fs;
use std::io;
use std::io::Write;

use error::ParseError;
use interpreter::LoxInterpreter;
use statement::Stmt;
use token::Token;
use token::TokenStream;

use clap::Parser;
use parser::LoxParser;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    input_file: Option<String>,
    #[arg(short, long)]
    debug: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    // TODO: add a debug argument
    let cli = Cli::parse();
    match cli.input_file {
        Some(input_file) => run_file(&input_file, cli.debug),
        None => run_prompt(cli.debug),
    }
}

fn run_file(path: &str, debug: bool) -> Result<(), Box<dyn Error>> {
    let input = String::from_utf8(fs::read(path)?)?;
    Ok(run(&input, debug))
}

fn run_prompt(debug: bool) -> Result<(), Box<dyn Error>> {
    loop {
        let mut buf = String::new();
        io::stdout().write_all(b"> ")?;
        io::stdout().flush()?;
        io::stdin().read_line(&mut buf)?;
        run(&buf, debug);
    }
}

fn run(input: &str, debug: bool) {
    // lex and handle errs
    let (tokens, errs): (Vec<Result<Token, _>>, Vec<Result<_, ParseError>>) =
        TokenStream::new(input).partition(|t| t.is_ok());

    let tokens = tokens.into_iter().map(|t| t.unwrap()).collect::<Vec<_>>();
    let errs = errs.into_iter().map(|t| t.unwrap_err()).collect::<Vec<_>>();

    if debug {
        println!("Tokenized input:");
        for (i, token) in tokens.clone().iter().enumerate() {
            print!("{}:{} ", i, token);
        }
        println!();
    }

    // handle token errors

    let parser = LoxParser::new(tokens);
    let (program, parse_errors) = parser.parse();

    if debug {
        println!("Statements:");
        for statement in &program {
            println!("{}", statement.to_string())
        }

        println!("Errors:");
        for error in &parse_errors {
            println!("Error: {}", error.to_string())
        }
    }

    if parse_errors.is_empty() {
        let mut interpreter = LoxInterpreter::new();
        interpreter.run(&program);
    }
}
