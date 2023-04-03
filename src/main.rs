use std::{
    env, fs,
    io::{self, BufRead, Write},
    process::ExitCode,
};

mod lox_error;
use lox_error::LoxError;
mod token;
use token::{Token, TokenType};
mod scanner;
use scanner::Scanner;

fn main() -> Result<(), ExitCode> {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => run_prompt(),
        2 => run_file(&args[1]),
        _ => {
            println!("Usage: jlox [script]");
            Err(ExitCode::from(64))
        }
    }
}

fn run_file(file_path: &str) -> Result<(), ExitCode> {
    let contents = fs::read_to_string(file_path).expect("Should have been able to read the file");
    match run(&contents) {
        Ok(_) => Ok(()),
        Err(_) => Err(ExitCode::from(65)),
    }
}

/// Goes into prompt-mode. Starts a REPL:
/// Read a line of input, Evaluate it, Print the result, then Loop
fn run_prompt() -> Result<(), ExitCode> {
    // TODO: raw input from user. dont escape backslashes, \n fails
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
                    Err(_) => todo!(),
                }
                print!("> ");
                io::stdout().flush().expect("Unable to flush stdout");
            }
            Err(e) => panic!("{e}"),
        }
    }

    Ok(())
}

fn run(source: &str) -> Result<(), LoxError> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens();

    for token in tokens {
        println!("{token:?}")
    }

    Ok(())
}
