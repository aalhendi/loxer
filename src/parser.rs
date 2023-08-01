use crate::{
    expr::{
        AssignExpr, BinaryExpr, CallExpr, ConditionalExpr, Expr, GetExpr, GroupingExpr, Literal,
        LogicalExpr, SetExpr, SuperExpr, ThisExpr, UnaryExpr, VariableExpr,
    },
    lox_result::{LoxResult, ParseErrorCause},
    stmt::{
        BlockStmt, ClassStmt, ExpressionStmt, FunctionStmt, IfStmt, PrintStmt, ReturnStmt, Stmt,
        VarStmt, WhileStmt,
    },
    token::{Token, TokenType},
};
use std::rc::Rc;

/// Recursive decent parser
pub struct Parser<'a> {
    tokens: std::iter::Peekable<std::slice::Iter<'a, Token>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens: tokens.iter().peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, LoxResult> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();
        while let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::Eof {
                break;
            }
            match self.declaration() {
                Ok(d) => {
                    statements.push(d);
                }
                Err(e) => {
                    errors.push(e);
                    self.sync();
                }
            }
        }

        if errors.is_empty() {
            Ok(statements)
        } else {
            Err(LoxResult::ParseError { causes: errors })
        }
    }

    fn declaration(&mut self) -> Result<Stmt, ParseErrorCause> {
        if let Some(_t) = self.tokens.next_if(|t| t.token_type == TokenType::Var) {
            self.var_declaration()
        } else if let Some(_t) = self.tokens.next_if(|t| t.token_type == TokenType::Fun) {
            return self.function("function");
        } else if let Some(_t) = self.tokens.next_if(|t| t.token_type == TokenType::Class) {
            self.class_declaration()
        } else {
            self.statement()
        }
    }

    fn class_declaration(&mut self) -> Result<Stmt, ParseErrorCause> {
        let name = self.consume_ident("Expect class name.")?.clone();
        let superclass = if self
            .tokens
            .next_if(|t| t.token_type == TokenType::Less)
            .is_some()
        {
            let name_t = self.consume_ident("Expect superclass name.")?;
            Some(VariableExpr::new(name_t.clone()))
        } else {
            None
        };

        self.consume(TokenType::LeftBrace, "Expect '{' before class body.")?;

        let mut methods = Vec::new();
        while let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::RightBrace || t.token_type == TokenType::Eof {
                break;
            }
            methods.push(self.function("method")?);
        }

        self.consume(TokenType::RightBrace, "Expect '}' after class body.")?;

        Ok(Stmt::Class(Box::new(ClassStmt::new(
            name, methods, superclass,
        ))))
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParseErrorCause> {
        let name = self.consume_ident("Expect variable name.")?.clone();
        let initializer = {
            if let Some(_t) = self.tokens.next_if(|t| t.token_type == TokenType::Equal) {
                Some(self.expression()?)
            } else {
                None
            }
        };

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        )?;

        Ok(Stmt::Var(Box::new(VarStmt::new(name, initializer))))
    }

    fn statement(&mut self) -> Result<Stmt, ParseErrorCause> {
        let t = self.tokens.peek().unwrap();
        match t.token_type {
            TokenType::If => {
                self.tokens.next();
                self.if_statement()
            }
            TokenType::Print => {
                self.tokens.next();
                self.print_statement()
            }
            TokenType::Return => self.return_statement(),
            TokenType::While => {
                self.tokens.next();
                self.while_statement()
            }
            TokenType::For => {
                self.tokens.next();
                self.for_statement()
            }
            TokenType::LeftBrace => {
                self.tokens.next();
                let s = self.block()?;
                Ok(Stmt::Block(Box::new(BlockStmt::new(s))))
            }
            _ => self.expression_statement(),
        }
    }

    fn while_statement(&mut self) -> Result<Stmt, ParseErrorCause> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after condition.")?;
        let body = self.statement()?;

        Ok(Stmt::While(Box::new(WhileStmt::new(condition, body))))
    }

    fn for_statement(&mut self) -> Result<Stmt, ParseErrorCause> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.")?;

        let initializer = {
            let t = self.tokens.peek().unwrap();
            if t.token_type == TokenType::Semicolon {
                self.tokens.next();
                None
            } else if t.token_type == TokenType::Var {
                self.tokens.next();
                Some(self.var_declaration()?)
            } else {
                Some(self.expression_statement()?)
            }
        };

        // Either there is a condition or it an infinite loop (true)
        let condition = {
            if self.tokens.peek().unwrap().token_type != TokenType::Semicolon {
                self.expression()?
            } else {
                Expr::Literal(Literal::Boolean(true))
            }
        };

        self.consume(TokenType::Semicolon, "Expect ';' after loop condition.")?;

        let increment = {
            if self.tokens.peek().unwrap().token_type != TokenType::RightParen {
                Some(self.expression()?)
            } else {
                None
            }
        };

        self.consume(TokenType::RightParen, "Expect ')' after for clauses.")?;

        let mut body = self.statement()?;

        // If an increment stmt exists, append it so it executes after the body
        // TODO: Verify generated ast nodes vs while
        if let Some(increment) = increment {
            let stmts = vec![
                body,
                Stmt::Expression(Box::new(ExpressionStmt::new(increment))),
            ];
            body = Stmt::Block(Box::new(BlockStmt::new(stmts)));
        }

        body = Stmt::While(Box::new(WhileStmt::new(condition, body)));

        // If an initializer exists, run it first, then execute the loop (fancy while loop)
        if let Some(initializer) = initializer {
            body = Stmt::Block(Box::new(BlockStmt::new(vec![initializer, body])))
        }

        Ok(body)
    }

    fn if_statement(&mut self) -> Result<Stmt, ParseErrorCause> {
        self.consume(TokenType::LeftParen, "Expect '(' after if.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after if condition.")?;
        let then_branch = self.statement()?;
        let else_branch = {
            match self.tokens.next_if(|t| t.token_type == TokenType::Else) {
                Some(_t) => Some(self.statement()?),
                None => None,
            }
        };

        Ok(Stmt::If(Box::new(IfStmt::new(
            condition,
            then_branch,
            else_branch,
        ))))
    }

    fn print_statement(&mut self) -> Result<Stmt, ParseErrorCause> {
        let value = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::Print(Box::new(PrintStmt::new(value))))
    }

    fn return_statement(&mut self) -> Result<Stmt, ParseErrorCause> {
        let keyword = self.tokens.next().unwrap();
        let value = {
            let t = self.tokens.peek().unwrap();
            if t.token_type != TokenType::Semicolon {
                Some(self.expression()?)
            } else {
                None
            }
        };

        self.consume(TokenType::Semicolon, "Expect ';' after return value.")?;

        Ok(Stmt::Return(Box::new(ReturnStmt::new(
            keyword.clone(),
            value,
        ))))
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParseErrorCause> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::Expression(Box::new(ExpressionStmt::new(expr))))
    }

    fn function(&mut self, kind: &str) -> Result<Stmt, ParseErrorCause> {
        let name = self.consume_ident(&format!("Expect {kind} name."))?.clone();
        self.consume(
            TokenType::LeftParen,
            &format!("Expect '(' after {kind} name."),
        )?;

        let mut params = Vec::new();
        let t = *self.tokens.peek().unwrap();
        if t.token_type != TokenType::RightParen {
            loop {
                let p = self.consume_ident("Expect parameter name.")?.clone();
                if params.len() >= 255 {
                    return Err(ParseErrorCause::new(
                        p.line,
                        Some(p.lexeme),
                        "Can't have more than 255 parameters.",
                    ));
                }
                params.push(p);
                let t = self.tokens.peek().unwrap();
                if t.token_type != TokenType::Comma {
                    break;
                } else {
                    self.tokens.next();
                }
            }
        }

        self.consume(TokenType::RightParen, "Expect ')' after parameters.")?;
        self.consume(
            TokenType::LeftBrace,
            &format!("Expect '{{' before {kind} body."),
        )?;
        let body = self.block()?;

        Ok(Stmt::Function(Rc::new(FunctionStmt::new(
            name, params, body,
        ))))
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParseErrorCause> {
        let mut statements = Vec::new();
        while let Some(t) = self.tokens.peek() {
            match t.token_type {
                TokenType::RightBrace | TokenType::Eof => break,
                _ => statements.push(self.declaration()?),
            }
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block")?;
        Ok(statements)
    }

    fn expression(&mut self) -> Result<Expr, ParseErrorCause> {
        self.conditional()
    }

    fn conditional(&mut self) -> Result<Expr, ParseErrorCause> {
        let mut expr = self.assignment()?; // condition

        if let Some(_t) = self
            .tokens
            .next_if(|t| t.token_type == TokenType::QuestionMark)
        {
            let left = self.expression()?;
            self.consume(TokenType::Colon, "Expect ':' after truthy expression")?;
            let right = self.conditional()?;
            expr = Expr::Conditional(Rc::new(ConditionalExpr::new(expr, left, right)))
        }
        Ok(expr)
    }

    fn assignment(&mut self) -> Result<Expr, ParseErrorCause> {
        let expr = self.logic_or()?;

        if let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::Equal {
                let equals = self.tokens.next().unwrap();
                // Recursively parse right-hand side since assignment is right-associative
                let value = self.assignment()?;

                match expr {
                    Expr::Variable(s) => {
                        return Ok(Expr::Assign(Rc::new(AssignExpr::new(
                            s.name.clone(),
                            value,
                        ))));
                    }
                    Expr::Get(e) => {
                        return Ok(Expr::Set(Rc::new(SetExpr::new(
                            e.object.clone(),
                            e.name.clone(),
                            value,
                        ))));
                    }
                    _ => {}
                }
                // NOTE: Err is reported but not thrown here, parser is not in confused state where it needs to panic and sync
                return Err(ParseErrorCause::new(
                    equals.line,
                    Some(equals.lexeme.clone()),
                    "Invalid assignment target.",
                ));
            }
        }

        Ok(expr)
    }

    fn logic_or(&mut self) -> Result<Expr, ParseErrorCause> {
        let mut expr = self.logic_and()?;

        while let Some(t) = self.tokens.next_if(|t| t.token_type == TokenType::Or) {
            let operator = t;
            let right = self.logic_and()?;
            expr = Expr::Logical(Rc::new(LogicalExpr::new(expr, operator.clone(), right)));
        }
        Ok(expr)
    }

    fn logic_and(&mut self) -> Result<Expr, ParseErrorCause> {
        let mut expr = self.equality()?;

        while let Some(t) = self.tokens.next_if(|t| t.token_type == TokenType::And) {
            let operator = t;
            let right = self.equality()?;
            expr = Expr::Logical(Rc::new(LogicalExpr::new(expr, operator.clone(), right)));
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParseErrorCause> {
        let mut expr = self.comparison()?;

        while let Some(t) = self.tokens.next_if(|t| {
            t.token_type == TokenType::BangEqual || t.token_type == TokenType::EqualEqual
        }) {
            let operator = t;
            let right = self.comparison()?;
            expr = Expr::Binary(Rc::new(BinaryExpr::new(expr, operator.to_owned(), right)));
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseErrorCause> {
        let mut expr = self.term()?;

        while let Some(t) = self.tokens.next_if(|t| {
            t.token_type == TokenType::Greater
                || t.token_type == TokenType::GreaterEqual
                || t.token_type == TokenType::Less
                || t.token_type == TokenType::LessEqual
        }) {
            let operator = t;
            let right = self.term()?;
            expr = Expr::Binary(Rc::new(BinaryExpr::new(expr, operator.to_owned(), right)));
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParseErrorCause> {
        let mut expr = self.factor()?;

        while let Some(t) = self
            .tokens
            .next_if(|t| t.token_type == TokenType::Minus || t.token_type == TokenType::Plus)
        {
            let operator = t;
            let right = self.factor()?;
            expr = Expr::Binary(Rc::new(BinaryExpr::new(expr, operator.to_owned(), right)));
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParseErrorCause> {
        let mut expr = self.unary()?;

        while let Some(t) = self
            .tokens
            .next_if(|t| t.token_type == TokenType::Slash || t.token_type == TokenType::Star)
        {
            let operator = t;
            let right = self.unary()?;
            expr = Expr::Binary(Rc::new(BinaryExpr::new(expr, operator.to_owned(), right)));
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseErrorCause> {
        if let Some(t) = self
            .tokens
            .next_if(|t| t.token_type == TokenType::Bang || t.token_type == TokenType::Minus)
        {
            let operator = t;
            let right = self.unary()?;
            let e = Expr::Unary(Rc::new(UnaryExpr::new(operator.clone(), right)));
            return Ok(e);
        }
        self.call()
    }

    fn call(&mut self) -> Result<Expr, ParseErrorCause> {
        let mut expr = self.primary()?;

        // Deliberate loop. Setting up for parsing object properties later on.
        loop {
            let t = self.tokens.peek().unwrap();
            if t.token_type == TokenType::LeftParen {
                self.tokens.next();
                expr = self.finish_call(expr)?;
            } else if t.token_type == TokenType::Dot {
                self.tokens.next();
                let name = self.consume_ident("Expect property name after '.'.")?;
                expr = Expr::Get(Rc::new(GetExpr::new(name.clone(), expr)));
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParseErrorCause> {
        let mut arguments = Vec::new();

        let t = *self.tokens.peek().unwrap();
        if t.token_type != TokenType::RightParen {
            // Do-while
            arguments.push(self.expression()?);
            while let Some(_nxt_t) = self.tokens.next_if(|t| t.token_type == TokenType::Comma) {
                if arguments.len() >= 255 {
                    let t = self.tokens.next().unwrap();
                    return Err(ParseErrorCause::new(
                        t.line,
                        Some(t.lexeme.clone()),
                        "Can't have more than 255 arguments.",
                    ));
                } else {
                    arguments.push(self.expression()?);
                }
            }
        }

        let paren = self.consume(TokenType::RightParen, "Expect ')' after arguments.")?;

        Ok(Expr::Call(Rc::new(CallExpr::new(
            callee,
            paren.clone(),
            arguments,
        ))))
    }

    // TODO: Error propagation and handle panics.
    fn primary(&mut self) -> Result<Expr, ParseErrorCause> {
        let t = self.tokens.next().unwrap();
        match &t.token_type {
            TokenType::False => Ok(Expr::Literal(Literal::Boolean(false))),
            TokenType::True => Ok(Expr::Literal(Literal::Boolean(true))),
            TokenType::Nil => Ok(Expr::Literal(Literal::Nil)),
            TokenType::String(s) => Ok(Expr::Literal(Literal::String(s.to_string()))),
            TokenType::Number(n) => Ok(Expr::Literal(Literal::Number(*n))),
            TokenType::This => Ok(Expr::This(Rc::new(ThisExpr::new(t.clone())))),
            TokenType::Super => {
                let keyword = t;
                self.consume(TokenType::Dot, "Expect '.' after 'super'.")?;
                let method = self.consume_ident("Expect superclass method name.")?;
                Ok(Expr::Super(Rc::new(SuperExpr::new(
                    keyword.clone(),
                    method.clone(),
                ))))
            }
            TokenType::LeftParen => {
                let expr = self.expression()?;
                self.consume(TokenType::RightParen, "Expect ')' after expression")?;
                Ok(Expr::Grouping(Rc::new(GroupingExpr::new(expr))))
            }
            TokenType::Identifier(_) => Ok(Expr::Variable(Rc::new(VariableExpr::new(t.clone())))),
            _ => Err(ParseErrorCause::new(
                t.line,
                Some(t.lexeme.clone()),
                "Expect expression.",
            )),
        }
    }

    fn sync(&mut self) {
        while let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::Eof {
                break;
            }
            match t.token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => (),
            }

            self.tokens.next();
        }
    }

    fn consume(&mut self, ttype: TokenType, message: &str) -> Result<&Token, ParseErrorCause> {
        let t = self.tokens.peek().unwrap();
        if t.token_type == ttype {
            Ok(self.tokens.next().unwrap())
        } else {
            Err(ParseErrorCause::new(
                t.line,
                Some(t.lexeme.clone()),
                message,
            ))
        }
    }

    fn consume_ident(&mut self, message: &str) -> Result<&Token, ParseErrorCause> {
        let t = self.tokens.peek().unwrap();
        if let TokenType::Identifier(_) = &t.token_type {
            Ok(self.tokens.next().unwrap())
        } else {
            Err(ParseErrorCause::new(
                t.line,
                Some(t.lexeme.clone()),
                message,
            ))
        }
    }
}

// TODO: Fix tests
// #[cfg(test)]
// mod tests {
//     use crate::{expr::Expr, lox_error::LoxResult, scanner::Scanner};

//     use super::Parser;

//     fn parse_expression(source: &str) -> Result<Expr, LoxResult> {
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
//     fn test_conditional() {
//         let e = parse_expression("5 < 6 ? 1 - 2 : 4 * 3");
//         assert_eq!(e.unwrap().to_string(), "(?: (< 5 6) (- 1 2) (* 4 3))")
//     }
// }
