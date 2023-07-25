use std::{
    env, fs,
    io::{self, BufRead, Write},
};

mod lox_result;
mod token;
use parser::Parser;
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
    run(&contents, &mut interpreter);
    std::process::exit(0)
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
                run(&line, &mut interpreter);
                print!("> ");
                io::stdout().flush().expect("Unable to flush stdout");
            }
            Err(e) => eprintln!("{e}"),
        }
    }
}

/// On error, returns an instance of LoxResult::Error and an ExitCode
fn run(source: &str, interpreter: &mut Interpreter) {
    let mut scanner = Scanner::new(source);
    let tokens = {
        match scanner.scan_tokens() {
            Ok(t) => t,
            Err(e) => {
                eprintln!("{e}");
                std::process::exit(65);
            }
        }
    };

    let mut parser = Parser::new(tokens);
    let statements = match parser.parse() {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(65);
        }
    };

    let mut resolver = Resolver::new(interpreter);
    if let Err(e) = resolver.resolve_stmts(&statements) {
        eprintln!("{e}");
        std::process::exit(65);
    }

    if let Err(e) = interpreter.interpret(&statements) {
        eprintln!("{e}");
        std::process::exit(70)
    }
}
