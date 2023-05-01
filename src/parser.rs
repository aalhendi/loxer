use crate::{
    expr::{BinaryExpr, Expr, GroupingExpr, Literal, TernaryExpr, UnaryExpr},
    lox_error::LoxError,
    stmt::{ExpressionStmt, PrintStmt, Stmt},
    token::{Token, TokenType},
};

/// Recursive decent parser
pub struct Parser<'a> {
    tokens: std::iter::Peekable<std::slice::Iter<'a, Token>>,
}

/*
program        → statement* EOF ;
statement      → exprStmt
               | printStmt ;
printStmt      → "print" expression ";" ;
exprStmt       → expression ";" ;
expression     → ternary;
ternary        → equality ("?" expression ":" conditional)? ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
 */

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens: tokens.iter().peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, LoxError> {
        let mut statements = Vec::new();
        while let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::Eof {
                break;
            }
            statements.push(self.statement()?);
        }
        Ok(statements)
    }

    fn statement(&mut self) -> Result<Stmt, LoxError> {
        if let Some(_t) = self.tokens.next_if(|t| t.token_type == TokenType::Print) {
            return self.print_statement();
        }
        self.expression_statement()
    }

    fn print_statement(&mut self) -> Result<Stmt, LoxError> {
        let value = self.expression()?;
        if let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::Semicolon {
                self.tokens.next();
            } else {
                return Err(LoxError::new(
                    t.line,
                    &format!("at {}. Expect ';' after expression", t.lexeme),
                ));
            }
        }
        Ok(Stmt::Print(Box::new(PrintStmt::new(value))))
    }

    fn expression_statement(&mut self) -> Result<Stmt, LoxError> {
        let expr = self.expression()?;
        if let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::Semicolon {
                self.tokens.next();
            } else {
                return Err(LoxError::new(
                    t.line,
                    &format!("at {}. Expect ';' after expression", t.lexeme),
                ));
            }
        }
        println!("value {expr:?}");
        Ok(Stmt::Expression(Box::new(ExpressionStmt::new(expr))))
    }

    fn expression(&mut self) -> Result<Expr, LoxError> {
        self.ternary()
    }

    fn ternary(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.equality()?; // condition

        if let Some(_t) = self
            .tokens
            .next_if(|t| t.token_type == TokenType::QuestionMark)
        {
            let left = self.expression()?;
            if let Some(t) = self.tokens.peek() {
                if t.token_type == TokenType::Colon {
                    self.tokens.next();
                } else {
                    return Err(LoxError::new(
                        t.line,
                        &format!("at {}. Expect ':' after truthy expr in ternary", t.lexeme),
                    ));
                }
            }
            let right = self.ternary()?;
            expr = Expr::Ternary(Box::new(TernaryExpr::new(expr, left, right)))
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.comparison()?;

        while let Some(t) = self.tokens.next_if(|t| {
            t.token_type == TokenType::BangEqual || t.token_type == TokenType::EqualEqual
        }) {
            let operator = t;
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(BinaryExpr::new(expr, operator.to_owned(), right)));
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.term()?;

        while let Some(t) = self.tokens.next_if(|t| {
            t.token_type == TokenType::Greater
                || t.token_type == TokenType::GreaterEqual
                || t.token_type == TokenType::Less
                || t.token_type == TokenType::LessEqual
        }) {
            let operator = t;
            let right = self.term()?;
            expr = Expr::Binary(Box::new(BinaryExpr::new(expr, operator.to_owned(), right)));
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.factor()?;

        while let Some(t) = self
            .tokens
            .next_if(|t| t.token_type == TokenType::Minus || t.token_type == TokenType::Plus)
        {
            let operator = t;
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(BinaryExpr::new(expr, operator.to_owned(), right)));
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.unary()?;

        while let Some(t) = self
            .tokens
            .next_if(|t| t.token_type == TokenType::Slash || t.token_type == TokenType::Star)
        {
            let operator = t;
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(BinaryExpr::new(expr, operator.to_owned(), right)));
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, LoxError> {
        if let Some(t) = self
            .tokens
            .next_if(|t| t.token_type == TokenType::Bang || t.token_type == TokenType::Minus)
        {
            let operator = t;
            let right = self.unary()?;
            let e = Expr::Unary(Box::new(UnaryExpr::new(operator.clone(), right)));
            return Ok(e);
        }
        self.primary()
    }

    // TODO: Error propagation and handle panics.
    fn primary(&mut self) -> Result<Expr, LoxError> {
        if let Some(t) = self.tokens.next() {
            match &t.token_type {
                TokenType::False => Ok(Expr::Literal(Literal::Boolean(false))),
                TokenType::True => Ok(Expr::Literal(Literal::Boolean(true))),
                TokenType::Nil => Ok(Expr::Literal(Literal::Nil)),
                TokenType::String(s) => Ok(Expr::Literal(Literal::String(s.to_string()))),
                TokenType::Number(n) => Ok(Expr::Literal(Literal::Number(*n))),
                TokenType::LeftParen => {
                    let expr = self.expression()?;
                    if let Some(t) = self.tokens.peek() {
                        if t.token_type == TokenType::RightParen {
                            self.tokens.next();
                        } else {
                            return Err(LoxError::new(
                                t.line,
                                &format!("at {}. Expect ')' after expression", t.lexeme),
                            ));
                        }
                    }
                    Ok(Expr::Grouping(Box::new(GroupingExpr::new(expr))))
                }
                _ => match self.tokens.peek() {
                    Some(t) => Err(LoxError::new(t.line, "expected expression")),
                    None => Err(LoxError::new(t.line, "EOF, something unterminated")), // TODO: Better error msg
                },
            }
        } else {
            unreachable!("Parser peeks so it can't run out");
        }
    }

    #[allow(dead_code)]
    fn sync(&mut self) {
        while let Some(t) = self.tokens.next() {
            if t.token_type == TokenType::Semicolon {
                return;
            };

            match self.tokens.peek() {
                Some(t) => match t.token_type {
                    TokenType::Class
                    | TokenType::Fun
                    | TokenType::Var
                    | TokenType::For
                    | TokenType::If
                    | TokenType::While
                    | TokenType::Print
                    | TokenType::Return => return,
                    _ => {}
                },
                None => panic!("Was looking for semicolon... ran out of tokens"),
            }
        }
    }
}

// TODO: Fix tests
// #[cfg(test)]
// mod tests {
//     use crate::{expr::Expr, lox_error::LoxError, scanner::Scanner};

//     use super::Parser;

//     fn parse_expression(source: &str) -> Result<Expr, LoxError> {
//         let mut scanner = Scanner::new(source);
//         let tokens = scanner.scan_tokens().to_vec();
//         println!("{tokens:?}");
//         let mut parser = Parser::new(&tokens);
//         parser.parse()
//     }

//     #[test]
//     fn test_parser() {
//         let e = parse_expression(r#"(!"hello" -3 + true) != "hi""#);
//         assert_eq!(
//             e.unwrap().to_string(),
//             r#"(!= (group (+ (- (! "hello") 3) true)) "hi")"#
//         )
//     }

//     #[test]
//     fn test_precedence() {
//         let e = parse_expression(r#"1+2*4-5"#);
//         assert_eq!(e.unwrap().to_string(), r#"(- (+ 1 (* 2 4)) 5)"#)
//     }

//     #[test]
//     fn test_ternary() {
//         let e = parse_expression("5 < 6 ? 1 - 2 : 4 * 3");
//         assert_eq!(e.unwrap().to_string(), "(?: (< 5 6) (- 1 2) (* 4 3))")
//     }
// }
