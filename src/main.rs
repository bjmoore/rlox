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
use token::Token;
use token::TokenStream;

use crate::parser::Parser;

fn main() -> Result<(), Box<dyn Error>> {
    // TODO: add a debug argument
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        Err(Box::<dyn Error>::from("Usage: rlox [script]"))
    } else if args.len() == 2 {
        run_file(&args[1])
    } else {
        run_prompt()
    }
}

fn run_file(path: &str) -> Result<(), Box<dyn Error>> {
    let input = String::from_utf8(fs::read(path)?)?;
    Ok(run(&input))
}

fn run_prompt() -> Result<(), Box<dyn Error>> {
    loop {
        let mut buf = String::new();
        io::stdout().write_all(b"> ")?;
        io::stdout().flush()?;
        io::stdin().read_line(&mut buf)?;
        run(&buf);
    }
}

fn run(input: &str) {
    // lex and handle errs
    let (tokens, errs): (Vec<Result<Token, _>>, Vec<Result<_, ParseError>>) =
        TokenStream::new(input).partition(|t| t.is_ok());

    let tokens = tokens.into_iter().map(|t| t.unwrap()).collect::<Vec<_>>();
    let errs = errs.into_iter().map(|t| t.unwrap_err()).collect::<Vec<_>>();

    for token in &tokens {
        println!("{:?}", token);
    }

    /*
    for err in errs {
        println!("{:?}", err);
    } */

    // parse and handle errs
    let parser = Parser::new(tokens);
    let program = parser.parse().unwrap();
    let mut interpreter = LoxInterpreter::new();

    interpreter.run(&program);
}
