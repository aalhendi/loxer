use logos::Logos;
use std::{env, fs, process::ExitCode};

fn main() -> Result<(), ExitCode> {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        1 => run_prompt(),
        2 => run_file(&args[0]),
        _ => {
            println!("Usage: jlox [script]");
            return Err(ExitCode::from(64));
        }
    }

    Ok(())
}

fn run_file(file_path: &str) {
    let contents = fs::read_to_string(file_path).expect("Should have been able to read the file");
    println!("{contents}");
    // TODO: run the file :)
}

fn run_prompt() {
    // REPL
    // Read a line of input, Evaluate it, Print the result, then Loop
    for line in std::io::stdin().lines() {
        print!("> ");
        run(&line.unwrap());
    }
}

fn run(source: &str) {
    let lex = TokenType::lexer(source);

    for token in lex {
        println!("{token:?}");
    }
}

#[allow(dead_code)]
fn error(line: u32, message: &str) {
    report(line, "", message);
}

#[allow(dead_code)]
fn report(line: u32, _where: &str, message: &str) {
    eprintln!("[{line}] Error {_where} : {message}");
}

#[allow(dead_code)]
#[derive(Logos, Debug, PartialEq)]
enum TokenType {
    // Single-character tokens.
    #[token("(")]
    LeftParen,

    #[token(")")]
    RightParen,

    #[token("[")]
    LeftBrace,

    #[token("]")]
    RightBrace,

    #[token(",")]
    Comma,

    #[token(".")]
    Dot,

    #[token("-")]
    Minus,

    #[token("+")]
    Plus,

    #[token(";")]
    Semicolon,

    #[token("/")]
    Slash,

    #[token("*")]
    Star,

    // One or two character tokens.
    #[token("!")]
    Bang,

    #[token("!=")]
    BangEqual,

    #[token("=")]
    Equal,

    #[token("==")]
    EqualEqual,

    #[token(">")]
    Greater,

    #[token(">=")]
    GreaterEqual,

    #[token("<")]
    Less,

    #[token("<=")]
    LessEqual,

    // Literals.
    //TODO: What would an identifier be
    #[token("identifier")]
    Identifier,

    #[regex("[a-zA-Z]+")]
    String,

    #[regex("[0-9]+", |lex| lex.slice().parse())]
    Number(u64),

    // Keywords.
    #[token("and")]
    And,

    #[token("class")]
    Class,

    #[token("else")]
    Else,

    #[token("false")]
    False,

    #[token("fun")]
    Fun,

    #[token("for")]
    For,

    #[token("if")]
    If,

    #[token("nil")]
    Nil,

    #[token("or")]
    Or,

    #[token("print")]
    Print,

    #[token("return")]
    Return,

    #[token("super")]
    Super,

    #[token("this")]
    This,

    #[token("true")]
    True,

    #[token("var")]
    Var,

    #[token("while")]
    While,

    // TODO: EOF
    Eof,

    // Logos requires one token variant to handle errors,
    // it can be named anything you wish.
    #[error]
    // We can also use this variant to define whitespace,
    // or any other matches we wish to skip.
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}
