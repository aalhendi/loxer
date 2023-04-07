use std::fmt::Display;

#[derive(Debug)]
pub struct Token {
    token_type: TokenType,
    lexeme: String,
    literal: Option<Literal>,
    _line: u32, // TODO: Line numbers on tokens
}

impl Token {
    pub fn new(
        token_type: TokenType,
        lexeme: String,
        literal: Option<Literal>,
        line: u32,
    ) -> Token {
        Token {
            token_type,
            lexeme,
            _line: line,
            literal,
        }
    }

    pub fn _get_type(&self) -> &TokenType {
        &self.token_type
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{token_type:?} {lexeme} {literal:?}",
            token_type = self.token_type,
            lexeme = self.lexeme,
            literal = self.literal
        )
    }
}

#[derive(Debug)]
pub enum Literal {
    Identifier(String),
    String(String),
    Number(f64),
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    // --- Single-character tokens. ---
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // --- One or two character tokens. ---
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier,
    // --- Literals. ---
    String,
    Number(f64),
    // --- Keywords. ---
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Eof,
}
