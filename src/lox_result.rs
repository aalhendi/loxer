use std::fmt::Display;

use crate::{
    expr::Literal,
    token::{Token, TokenType},
};

#[derive(Debug)]

pub enum LoxResult {
    ParseError { token: Token, message: String },
    RuntimeError { token: Token, message: String },
    Error { line: usize, message: String },
    Return(Literal),
    // Break // TODO:
}

impl LoxResult {
    pub fn error(line: usize, message: &str) -> LoxResult {
        let err = LoxResult::Error {
            line,
            message: message.to_string(),
        };
        eprintln!("{err}");
        err
    }

    pub fn parse_error(token: &Token, message: &str) -> LoxResult {
        let err = LoxResult::ParseError {
            token: token.clone(),
            message: message.to_string(),
        };
        eprintln!("{err}");
        err
    }

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
            LoxResult::ParseError { token, message } => {
                if token.token_type == TokenType::Eof {
                    write!(f, "[{}] Error at end: {}", token.line, message)
                } else {
                    write!(
                        f,
                        "[line {}] Error at '{}': {}",
                        token.line, token.lexeme, message
                    )
                }
            }
            LoxResult::RuntimeError { token, message } => {
                if token.token_type == TokenType::Eof {
                    write!(f, "[{}] Error at end: {}", token.line, message)
                } else {
                    write!(f, "{}\n[line {}]", message, token.line)
                }
            }
            LoxResult::Error { line, message } => {
                write!(f, "[{}] Error: {}", line, message)
            }
            LoxResult::Return { .. } => write!(f, ""),
        }
    }
}
