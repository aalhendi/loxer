use super::{LoxError, Token, TokenType};
use std::collections::HashMap;

pub struct Scanner {
    source: String,
    line: u32,
    tokens: Vec<Token>,
}

impl Scanner {
    pub fn new(source: &str) -> Scanner {
        Scanner {
            source: String::from(source),
            line: 1,
            tokens: Vec::new(),
        }
    }

    pub fn scan_tokens(&mut self) -> &Vec<Token> {
        let mut char_indices = self.source.char_indices().peekable();

        let keywords: HashMap<String, TokenType> = HashMap::from([
            ("and".to_owned(), TokenType::And),
            ("class".to_owned(), TokenType::Class),
            ("else".to_owned(), TokenType::Else),
            ("false".to_owned(), TokenType::False),
            ("for".to_owned(), TokenType::For),
            ("fun".to_owned(), TokenType::Fun),
            ("if".to_owned(), TokenType::If),
            ("nil".to_owned(), TokenType::Nil),
            ("or".to_owned(), TokenType::Or),
            ("print".to_owned(), TokenType::Print),
            ("return".to_owned(), TokenType::Return),
            ("super".to_owned(), TokenType::Super),
            ("this".to_owned(), TokenType::This),
            ("true".to_owned(), TokenType::True),
            ("var".to_owned(), TokenType::Var),
            ("while".to_owned(), TokenType::While),
        ]);

        while let Some((pos, ch)) = char_indices.next() {
            let mut lexeme = ch.to_string();

            let token_type = match ch {
                '(' => Ok(Some(TokenType::LeftParen)),
                ')' => Ok(Some(TokenType::RightParen)),
                '{' => Ok(Some(TokenType::LeftBrace)),
                '}' => Ok(Some(TokenType::RightBrace)),
                ',' => Ok(Some(TokenType::Comma)),
                '.' => Ok(Some(TokenType::Dot)),
                '-' => Ok(Some(TokenType::Minus)),
                '+' => Ok(Some(TokenType::Plus)),
                ';' => Ok(Some(TokenType::Semicolon)),
                '*' => Ok(Some(TokenType::Star)),
                '!' => {
                    if let Some((_, c)) = char_indices.next_if_eq(&(pos + 1, '=')) {
                        lexeme = lexeme + &c.to_string();
                        Ok(Some(TokenType::BangEqual))
                    } else {
                        Ok(Some(TokenType::Bang))
                    }
                }
                '=' => {
                    if let Some((_, c)) = char_indices.next_if_eq(&(pos + 1, '=')) {
                        lexeme = lexeme + &c.to_string();
                        Ok(Some(TokenType::EqualEqual))
                    } else {
                        Ok(Some(TokenType::Equal))
                    }
                }
                '<' => {
                    if let Some((_, c)) = char_indices.next_if_eq(&(pos + 1, '=')) {
                        lexeme = lexeme + &c.to_string();
                        Ok(Some(TokenType::LessEqual))
                    } else {
                        Ok(Some(TokenType::Less))
                    }
                }
                '>' => {
                    if let Some((_, c)) = char_indices.next_if_eq(&(pos + 1, '=')) {
                        lexeme = lexeme + &c.to_string();
                        Ok(Some(TokenType::GreaterEqual))
                    } else {
                        Ok(Some(TokenType::Greater))
                    }
                }
                '/' => {
                    // Comment. ignore lexeme
                    if char_indices.next_if_eq(&(pos + 1, '/')).is_some() {
                        for (_pos, next_ch) in char_indices.by_ref() {
                            // Consume till newline
                            if next_ch == '\n' {
                                break;
                            }
                        }
                        Ok(None)
                    } else if char_indices.next_if_eq(&(pos + 1, '*')).is_some() {
                        // Block comment. ignore lexeme
                        // BUG: termining */ should not have spaces between
                        let mut is_prev_star = false;
                        for (_pos, next_ch) in char_indices.by_ref() {
                            if next_ch == '*' {
                                is_prev_star = true;
                            } else if is_prev_star && next_ch == '/' {
                                break;
                            } else {
                                is_prev_star = false;
                            }
                        }
                        Ok(None)
                    } else {
                        Ok(Some(TokenType::Slash))
                    }
                }
                ' ' | '\r' | '\t' => {
                    Ok(None) // Skip whitespace
                }
                '\n' => {
                    self.line += 1;
                    Ok(None)
                }
                '"' => {
                    // TODO: Handle escape sequences
                    lexeme = String::new(); // reset text to trimp first quote
                    let mut return_val =
                        Err(LoxError::new(self.line, "Unterminated string.".to_owned()));
                    for (_pos, next_ch) in char_indices.by_ref() {
                        if next_ch == '"' {
                            return_val = Ok(Some(TokenType::String));
                            break;
                        } else {
                            if next_ch == '\n' {
                                self.line += 1;
                            }
                            lexeme += &next_ch.to_string();
                        }
                    }
                    return_val
                }
                _ if ch.is_ascii_digit() => {
                    while let Some((_, next_ch)) =
                        char_indices.next_if(|(_, next_ch)| next_ch.is_ascii_digit())
                    {
                        // Keep consuming while next is number
                        lexeme += &next_ch.to_string();
                    }

                    // No longer a number char
                    // Check if next is dot (for decimals)
                    if let Some((_, next_ch)) = char_indices.next_if(|(_, c)| *c == '.') {
                        // Append dot
                        lexeme += &next_ch.to_string();

                        while let Some((_, next_ch)) =
                            char_indices.next_if(|(_, next_ch)| next_ch.is_ascii_digit())
                        {
                            // Keep consuming while next is number
                            lexeme += &next_ch.to_string();
                        }
                    }

                    // TODO: unwrap
                    Ok(Some(TokenType::Number(lexeme.parse().unwrap())))
                }
                _ if ch.is_ascii_alphabetic() => {
                    while let Some((_, next_ch)) =
                        char_indices.next_if(|(_, ch)| ch.is_ascii_alphanumeric())
                    {
                        lexeme += &next_ch.to_string();
                    }

                    let t_type = keywords.get(&lexeme);
                    match t_type {
                        Some(t) => Ok(Some(t.clone())), // NOTE: is this clone needed?
                        None => Ok(Some(TokenType::Identifier)),
                    }
                }
                _ => Err(LoxError::new(
                    self.line,
                    format!("Unexpected Character \"{ch}\""),
                )),
            };

            match token_type {
                Ok(t_type) => {
                    if let Some(t) = t_type {
                        self.tokens
                            .push(Token::new(t, lexeme.to_owned(), None, self.line))
                    }
                }
                Err(e) => eprintln!("{e}"),
            }
        }

        self.tokens
            .push(Token::new(TokenType::Eof, "".to_owned(), None, self.line));

        &self.tokens
    }
}

#[cfg(test)]
#[test]
fn test_bool() {
    let source = "true false True False // true // false".to_owned();
    let mut scanner = Scanner::new(&source);
    let ttypes: Vec<_> = scanner
        .scan_tokens()
        .iter()
        .map(|t| t._get_type())
        .collect();

    assert_eq!(ttypes.len(), 5);
    assert_eq!(ttypes[0], &TokenType::True);
    assert_eq!(ttypes[1], &TokenType::False);
    assert_eq!(ttypes[2], &TokenType::Identifier);
    assert_eq!(ttypes[3], &TokenType::Identifier);
    assert_eq!(ttypes[4], &TokenType::Eof);
}

#[test]
fn test_number() {
    let source = "100 100.1 100.01 0 100d 100.d 100.".to_owned();
    let mut scanner = Scanner::new(&source);
    let ttypes: Vec<_> = scanner
        .scan_tokens()
        .iter()
        .map(|t| t._get_type())
        .collect();

    assert_eq!(ttypes.len(), 10);
    assert_eq!(ttypes[0], &TokenType::Number(100.00));
    assert_eq!(ttypes[1], &TokenType::Number(100.10));
    assert_eq!(ttypes[2], &TokenType::Number(100.01));
    assert_eq!(ttypes[3], &TokenType::Number(0.00));
    assert_eq!(ttypes[4], &TokenType::Number(100.00));
    assert_eq!(ttypes[5], &TokenType::Identifier);
    assert_eq!(ttypes[6], &TokenType::Number(100.00));
    assert_eq!(ttypes[7], &TokenType::Identifier);
    assert_eq!(ttypes[8], &TokenType::Number(100.00));
    assert_eq!(ttypes[9], &TokenType::Eof);
}
