use std::fmt::Display;

use crate::{
    expr::Literal,
    token::{Token, TokenType},
};

#[derive(Debug)]

pub enum LoxResult {
    ParseError { causes: Vec<ParseErrorCause> },
    RuntimeError { token: Token, message: String },
    Return(Literal),
    // Break // TODO:
}

impl LoxResult {
    pub fn runtime_error(token: &Token, message: &str) -> LoxResult {
        let err = LoxResult::RuntimeError {
            token: token.clone(),
            message: message.to_string(),
        };
        eprintln!("{err}");
        err
    }
}

impl Display for LoxResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxResult::ParseError { causes } => {
                for c in causes {
                    match &c.token {
                        // EOF
                        Some(lexeme) if lexeme.is_empty() => {
                            write!(f, "[line {}] Error at end: {}", c.line, c.message)?
                        }
                        Some(lexeme) => {
                            write!(f, "[line {}] Error at '{}': {}", c.line, lexeme, c.message)?
                        }
                        None => write!(f, "[line {}] Error: {}", c.line, c.message)?,
                    }
                }
                Ok(())
            }
            LoxResult::RuntimeError { token, message } => {
                if token.token_type == TokenType::Eof {
                    write!(f, "[{}] Error at end: {}", token.line, message)
                } else {
                    write!(f, "{}\n[line {}]", message, token.line)
                }
            }
            LoxResult::Return { .. } => write!(f, ""),
        }
    }
}

#[derive(Debug)]
pub struct ParseErrorCause {
    pub line: usize,
    pub token: Option<String>,
    pub message: String,
}

impl ParseErrorCause {
    pub fn new(line: usize, lexeme: Option<String>, message: &str) -> ParseErrorCause {
        ParseErrorCause {
            line,
            token: lexeme,
            message: message.to_string(),
        }
    }
}
