use crate::expr::Literal;
use std::fmt::Display;

#[derive(Debug)]

pub enum LoxResult {
    Error { line: u32, message: String },
    Return(Literal),
    // Break // TODO:
}

impl LoxResult {
    pub fn new_error(line: u32, message: &str) -> Self {
        Self::Error {
            line,
            message: message.to_owned(),
        }
    }
}

impl Display for LoxResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxResult::Error { line, message } => {
                write!(f, "Line {line}: {message}",)
            }
            LoxResult::Return(l) => write!(f, "{l}",),
        }
    }
}
