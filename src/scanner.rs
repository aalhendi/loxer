use crate::lox_result::{LoxResult, ParseErrorCause};

use super::{Token, TokenType};
use std::{collections::HashMap, iter::Peekable, str::Chars};

pub struct Scanner<'a> {
    source: Peekable<Chars<'a>>,
    line: usize,
    tokens: Vec<Token>,
    errors: Vec<ParseErrorCause>,
}

impl Scanner<'_> {
    pub fn new(source: &str) -> Scanner {
        Scanner {
            source: source.chars().peekable(),
            line: 1,
            tokens: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn scan_tokens(&mut self) -> Result<&Vec<Token>, LoxResult> {
        while let Some(ch) = self.source.next() {
            self.scan_token(ch);
        }

        self.tokens
            .push(Token::new(TokenType::Eof, "".to_owned(), self.line));

        if self.errors.is_empty() {
            Ok(&self.tokens)
        } else {
            let errors = std::mem::take(&mut self.errors);
            Err(LoxResult::ParseError { causes: errors })
        }
    }

    fn scan_token(&mut self, ch: char) {
        // TODO: Once cell this
        let keywords: HashMap<&'static str, TokenType> = HashMap::from([
            ("and", TokenType::And),
            ("class", TokenType::Class),
            ("else", TokenType::Else),
            ("false", TokenType::False),
            ("for", TokenType::For),
            ("fun", TokenType::Fun),
            ("if", TokenType::If),
            ("nil", TokenType::Nil),
            ("or", TokenType::Or),
            ("print", TokenType::Print),
            ("return", TokenType::Return),
            ("super", TokenType::Super),
            ("this", TokenType::This),
            ("true", TokenType::True),
            ("var", TokenType::Var),
            ("while", TokenType::While),
        ]);

        match ch {
            '(' => self.push_token(TokenType::LeftParen, ch),
            ')' => self.push_token(TokenType::RightParen, ch),
            '{' => self.push_token(TokenType::LeftBrace, ch),
            '}' => self.push_token(TokenType::RightBrace, ch),
            ',' => self.push_token(TokenType::Comma, ch),
            '.' => self.push_token(TokenType::Dot, ch),
            '-' => self.push_token(TokenType::Minus, ch),
            '+' => self.push_token(TokenType::Plus, ch),
            // TODO: Colons are discarded, should they err if used without `?`
            ':' => self.push_token(TokenType::Colon, ch),
            ';' => self.push_token(TokenType::Semicolon, ch),
            '*' => self.push_token(TokenType::Star, ch),
            '?' => self.push_token(TokenType::QuestionMark, ch),
            '!' => self.push_token_ch_eq(TokenType::Bang, TokenType::BangEqual, ch),
            '=' => self.push_token_ch_eq(TokenType::Equal, TokenType::EqualEqual, ch),
            '<' => self.push_token_ch_eq(TokenType::Less, TokenType::LessEqual, ch),
            '>' => self.push_token_ch_eq(TokenType::Greater, TokenType::GreaterEqual, ch),
            '/' => {
                // Comment. ignore lexeme
                if self.source.next_if_eq(&'/').is_some() {
                    for next_ch in self.source.by_ref() {
                        // Consume till newline
                        if next_ch == '\n' {
                            self.line += 1;
                            break;
                        }
                    }
                } else if self.source.next_if_eq(&'*').is_some() {
                    // Block comment. ignore lexeme
                    if self.scan_block_comment().is_err() {
                        self.errors.push(ParseErrorCause::new(
                            self.line,
                            None,
                            "Unterminated block comment.",
                        ))
                    }
                } else {
                    self.push_token(TokenType::Slash, ch)
                }
            }
            ' ' | '\r' | '\t' => {
                // Skip whitespace
            }
            '\n' => {
                self.line += 1;
            }
            '"' => self.scan_string(),
            _ if ch.is_ascii_digit() => {
                let mut char_num = vec![ch];
                while let Some(next_ch) = self.source.next_if(|next_ch| next_ch.is_ascii_digit()) {
                    // Keep consuming while next is number
                    char_num.push(next_ch);
                }

                // No longer a number char
                // Check if next is dot (for decimals)
                if self.source.peek() == Some(&'.') {
                    // Append dot and consume
                    char_num.push('.');
                    self.source.next();

                    // Peek next
                    match self.source.peek() {
                        Some(c) if c.is_ascii_digit() => {
                            while let Some(next_ch) =
                                self.source.next_if(|next_ch| next_ch.is_ascii_digit())
                            {
                                // Keep consuming while next is number
                                char_num.push(next_ch);
                            }
                        }
                        _ => {
                            // Add the number, add the consumed dot as token
                            self.push_num(&char_num);
                            return self.push_token(TokenType::Dot, '.');
                        }
                    }
                }
                self.push_num(&char_num);
            }

            // TODO: allow unicode?
            _ if ch.is_ascii_alphabetic() || ch == '_' => {
                let mut lexeme = vec![ch]; // TODO: Capacity
                while let Some(next_ch) = self
                    .source
                    .next_if(|ch| ch.is_ascii_alphanumeric() || ch == &'_')
                {
                    lexeme.push(next_ch);
                }

                let lexeme = String::from_iter(lexeme);
                let t_type = keywords.get(&lexeme.as_str());
                match t_type {
                    Some(t) => self.tokens.push(Token::new(t.clone(), lexeme, self.line)),
                    None => self.tokens.push(Token::new(
                        TokenType::Identifier,
                        lexeme,
                        self.line,
                    )),
                }
            }
            _ => eprintln!("[line {}] Error: Unexpected character.", self.line),
        }
    }

    fn scan_string(&mut self) {
        // TODO: Handle escape sequences
        let mut lexeme = Vec::new();
        // reset text to trim first quote
        let mut is_term = false;
        for next_ch in self.source.by_ref() {
            if next_ch == '"' {
                is_term = true;
                break;
            } else if next_ch == '\n' {
                self.line += 1;
            }
            lexeme.push(next_ch);
        }
        if is_term {
            let lexeme = String::from_iter(lexeme);
            // TODO: Does this need to be in 2 places?
            self.tokens.push(Token::new(
                TokenType::String(lexeme.clone()),
                lexeme,
                self.line,
            ))
        } else {
            self.errors.push(ParseErrorCause::new(
                self.line,
                None,
                "Unterminated string.",
            ))
        }
    }

    fn push_num(&mut self, char_num: &[char]) {
        let str_num = String::from_iter(char_num);
        let value = str_num.parse::<f64>().unwrap();
        self.tokens
            .push(Token::new(TokenType::Number(value), str_num, self.line));
    }

    fn scan_block_comment(&mut self) -> Result<(), ()> {
        // Consume till loop broken or EOF
        while let Some(next_ch) = self.source.next() {
            if next_ch == '\n' {
                self.line += 1;
            } else if next_ch == '/' {
                if let Some(next_next_ch) = self.source.next() {
                    if next_next_ch == '*' {
                        self.scan_block_comment()?;
                    }
                }
            } else if next_ch == '*' {
                if let Some(next_next_ch) = self.source.next() {
                    if next_next_ch == '/' {
                        return Ok(());
                    }
                }
            }
        }
        Err(())
    }

    fn push_token(&mut self, ttype: TokenType, ch: char) {
        self.tokens
            .push(Token::new(ttype, ch.to_string(), self.line))
    }

    fn push_token_ch_eq(&mut self, ttype1: TokenType, ttype2: TokenType, ch: char) {
        if let Some(c) = self.source.next_if_eq(&'=') {
            self.tokens
                .push(Token::new(ttype2, String::from_iter([ch, c]), self.line))
        } else {
            self.push_token(ttype1, ch)
        }
    }
}

#[cfg(test)]
#[test]
fn test_bool() {
    let source = "true false True False // true // false";
    let mut scanner = Scanner::new(source);
    let ttypes: Vec<_> = scanner
        .scan_tokens()
        .unwrap()
        .iter()
        .map(|t| &t.token_type)
        .collect();

    assert_eq!(ttypes.len(), 5);
    assert_eq!(
        ttypes,
        vec![
            &TokenType::True,
            &TokenType::False,
            &TokenType::Identifier,
            &TokenType::Identifier,
            &TokenType::Eof,
        ]
    );
}

#[test]
fn test_number() {
    let source = "100 100.1 100.01 0 100d 100.d 100.".to_owned();
    let mut scanner = Scanner::new(&source);
    let ttypes: Vec<_> = scanner
        .scan_tokens()
        .unwrap()
        .iter()
        .map(|t| &t.token_type)
        .collect();

    assert_eq!(ttypes.len(), 12);
    assert_eq!(
        ttypes,
        vec![
            &TokenType::Number(100.00),
            &TokenType::Number(100.10),
            &TokenType::Number(100.01),
            &TokenType::Number(0.00),
            &TokenType::Number(100.00),
            &TokenType::Identifier,
            &TokenType::Number(100.00),
            &TokenType::Dot,
            &TokenType::Identifier,
            &TokenType::Number(100.00),
            &TokenType::Dot,
            &TokenType::Eof,
        ]
    );
}

#[test]
fn test_comment() {
    // TODO: Test always passes because `scanner.scan_tokens()` doesnt error out
    let source = "/*/* hi */ hello */ // world".to_owned();
    let mut scanner = Scanner::new(&source);
    let ttypes: Vec<_> = scanner
        .scan_tokens()
        .unwrap()
        .iter()
        .map(|t| &t.token_type)
        .collect();

    assert_eq!(ttypes.len(), 1);
    assert_eq!(ttypes[0], &TokenType::Eof);
}
