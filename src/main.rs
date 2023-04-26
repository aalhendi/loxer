use std::{
    env, fs,
    io::{self, BufRead, Write},
};

mod lox_error;
use lox_error::LoxError;
mod token;
use token::{Token, TokenType};
mod scanner;
use scanner::Scanner;
mod expr;
mod interpreter;
mod parser;

fn main() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => run_prompt(),
        2 => run_file(&args[1]).expect("Unable to run file."),
        _ => {
            println!("Usage: jlox [script]");
            // EX_USAGE (64) Command was used incorrectly, e.g., with the wrong number of arguments, a bad flag, bad syntax in a parameter, or whatever.
            std::process::exit(64)
        }
    }
}

fn run_file(file_path: &str) -> std::io::Result<()> {
    let contents = fs::read_to_string(file_path)?;
    if run(&contents).is_ok() {
        return Ok(());
    }

    Ok(()) // TODO: handle err case.
}

/// Goes into prompt-mode. Starts a REPL:
/// Read a line of input, Evaluate it, Print the result, then Loop
fn run_prompt() {
    print!("> ");
    io::stdout().flush().expect("Unable to flush stdout");
    for line in io::stdin().lock().lines() {
        match line {
            Ok(line) => {
                if line.is_empty() {
                    break;
                }
                // TODO: Error is not propagating correctly
                match run(&line) {
                    Ok(_) => {}
                    Err(e) => eprintln!("{e}"),
                }
                print!("> ");
                io::stdout().flush().expect("Unable to flush stdout");
            }
            Err(e) => panic!("{e}"),
        }
    }
}

fn run(source: &str) -> Result<(), LoxError> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens().to_vec();
    let mut parser = parser::Parser::new(&tokens);
    let expression = match parser.parse() {
        Ok(e) => e,
        // EX_DATAERR (65) User input data was incorrect in some way.
        Err(_) => std::process::exit(65),
    };
    let interpreter = interpreter::Interpreter::new();
    match interpreter.interpret(expression) {
        Ok(_) => {}
        // EX_SOFTWARE (70) Internal software error. Limited to non-OS errors.
        Err(_) => std::process::exit(70),
    }

    Ok(())
}
