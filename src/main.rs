use std::{
    env, fs,
    io::{self, BufRead, Write},
};

mod lox_result;
use lox_result::LoxResult;
mod token;
use resolver::Resolver;
use token::{Token, TokenType};
mod scanner;
use scanner::Scanner;
mod interpreter;
use interpreter::Interpreter;
mod environment;
mod expr;
mod functions;
mod lox_class;
mod parser;
mod resolver;
mod stmt;

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
    let mut interpreter = Interpreter::new();
    // EX_DATAERR (65) User input data was incorrect in some way.
    // EX_SOFTWARE (70) Internal software error. Limited to non-OS errors.
    match run(&contents, &mut interpreter) {
        Ok(_) => std::process::exit(0),
        Err(LoxResult::RuntimeError { .. }) => std::process::exit(70),
        Err(LoxResult::ParseError { .. }) => std::process::exit(65),
        _ => std::process::exit(200), // TODO: TEMP CATCH ALL TO FIX ERR CODES
    }
}

/// Goes into prompt-mode. Starts a REPL:
/// Read a line of input, Evaluate it, Print the result, then Loop
fn run_prompt() {
    let mut interpreter = Interpreter::new();
    print!("> ");
    io::stdout().flush().expect("Unable to flush stdout");
    for line in io::stdin().lock().lines() {
        match line {
            Ok(line) => {
                if line.is_empty() {
                    break;
                }
                // TODO: Error is being discarded
                let _ = run(&line, &mut interpreter);
                print!("> ");
                io::stdout().flush().expect("Unable to flush stdout");
            }
            Err(e) => panic!("{e}"),
        }
    }
}

/// On error, returns an instance of LoxResult::Error and an ExitCode
fn run(source: &str, interpreter: &mut Interpreter) -> Result<(), LoxResult> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens()?.to_vec();

    let mut parser = parser::Parser::new(&tokens);
    let statements = match parser.parse() {
        Ok(s) => s,
        Err(_) => return Err(LoxResult::ParseError { token: Token::new(TokenType::Eof, "".to_string(), 0), message: "".to_string() }),
    };

    let mut resolver = Resolver::new(interpreter);
    resolver.resolve_stmts(&statements)?;

    if !resolver.had_error {
        interpreter.interpret(&statements)?;
    } else {
        std::process::exit(65);
    }

    Ok(())
}
