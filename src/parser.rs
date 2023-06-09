use crate::{
    expr::{
        AssignExpr, BinaryExpr, CallExpr, ConditionalExpr, Expr, GetExpr, GroupingExpr, Literal,
        LogicalExpr, SetExpr, SuperExpr, ThisExpr, UnaryExpr, VariableExpr,
    },
    lox_result::LoxResult,
    stmt::{
        BlockStmt, ClassStmt, ExpressionStmt, FunctionStmt, IfStmt, PrintStmt, ReturnStmt, Stmt,
        VarStmt, WhileStmt,
    },
    token::{Token, TokenType},
};

/// Recursive decent parser
pub struct Parser<'a> {
    tokens: std::iter::Peekable<std::slice::Iter<'a, Token>>,
    pub had_error: bool,
}

/*
program        → statement* EOF ;
classDecl      → "class" IDENTIFIER ( "<" IDENTIFIER )?
                 "{" function* "}" ;
function       → IDENTIFIER "(" parameters? ")" block ;
declaration    → classDecl
               | funDecl
               | varDecl
               | statement ;
statement      → exprStmt
               | forStmt
               | ifStmt
               | printStmt
               | returnStmt
               | whileStmt
               | block ;
returnStmt     → "return" expression? ";" ;
forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
                 expression? ";"
                 expression? ")" statement ;
whileStmt      → "while" "(" expression ")" statement ;
ifStmt         → "if" "(" expression ")" statement
               ( "else" statement )? ;
block          → "{" declaration* "}" ;
printStmt      → "print" expression ";" ;
exprStmt       → expression ";" ;
varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
funDecl        → "fun" function ;
expression     → conditional;
parameters     → IDENTIFIER ( "," IDENTIFIER )* ;
conditional    → assignment ("?" expression ":" conditional)? ;
assignment     → ( call "." )? IDENTIFIER "=" assignment
               | logic_or ;
logic_or       → logic_and ( "or" logic_and )* ;
logic_and      → equality ( "and" equality )* ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary | call ;
call           → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
arguments      → expression ( "," expression )* ;
primary        → "true" | "false" | "nil" | "this"
               | NUMBER | STRING | IDENTIFIER | "(" expression ")"
               | "super" "." IDENTIFIER ;
 */

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens: tokens.iter().peekable(),
            had_error: false,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ()> {
        let mut statements = Vec::new();
        while let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::Eof {
                break;
            }
            if let Ok(d) = self.declaration() {
                statements.push(d);
            }
            // TODO: Log errors recursively?
        }
        if self.had_error {
            return Err(());
        }
        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Stmt, LoxResult> {
        if let Some(_t) = self.tokens.next_if(|t| t.token_type == TokenType::Var) {
            match self.var_declaration() {
                Ok(s) => return Ok(s),
                Err(e) => {
                    // TODO: Catch only runtime errors, add ``kind``
                    self.sync();
                    return Err(e);
                }
            }
        } else if let Some(_t) = self.tokens.next_if(|t| t.token_type == TokenType::Fun) {
            // TODO: Enum?
            return self.function("function");
        } else if let Some(_t) = self.tokens.next_if(|t| t.token_type == TokenType::Class) {
            return self.class_declaration();
        }
        match self.statement() {
            Ok(s) => Ok(s),
            Err(e) => {
                // TODO: Catch only runtime errors, add ``kind``
                self.sync();
                Err(e)
            }
        }
    }

    fn class_declaration(&mut self) -> Result<Stmt, LoxResult> {
        let name = match self.tokens.peek() {
            Some(t) => {
                if let TokenType::Identifier(_) = &t.token_type {
                    self.tokens.next().unwrap()
                } else {
                    let token = &(*t).clone();
                    return Err(self.error(token, "Expect class name."));
                }
            }
            None => unreachable!("Ran out of tokens"),
        };

        let superclass = if self
            .tokens
            .next_if(|t| t.token_type == TokenType::Less)
            .is_some()
        {
            if let Some(next_t) = self.tokens.peek() {
                if let TokenType::Identifier(_) = &next_t.token_type {
                    Some(VariableExpr::new(self.tokens.next().unwrap().clone()))
                } else {
                    let token = &(*next_t).clone();
                    return Err(self.error(token, "Expect superclass name."));
                }
            } else {
                None
            }
        } else {
            None
        };

        if let Some(t) = self.tokens.peek() {
            if let TokenType::LeftBrace = &t.token_type {
                self.tokens.next();
            } else {
                let token = &(*t).clone();
                return Err(self.error(token, "Expect '{' before class body."));
            }
        }

        let mut methods = Vec::new();
        while let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::RightBrace || t.token_type == TokenType::Eof {
                break;
            }
            methods.push(self.function("method")?);
        }

        if let Some(t) = self.tokens.peek() {
            if let TokenType::RightBrace = &t.token_type {
                self.tokens.next();
            } else {
                let token = &(*t).clone();
                return Err(self.error(token, "Expect '}' after class body."));
            }
        }

        Ok(Stmt::Class(Box::new(ClassStmt::new(
            name.clone(),
            methods,
            superclass,
        ))))
    }

    fn var_declaration(&mut self) -> Result<Stmt, LoxResult> {
        let name = match self.tokens.peek() {
            Some(t) => {
                if let TokenType::Identifier(_) = &t.token_type {
                    self.tokens.next().unwrap()
                } else if let TokenType::Eof = &t.token_type {
                    // TODO: unreachable?
                    let token = &(*t).clone();
                    return Err(self.error(token, "Expect variable name."));
                } else {
                    let token = &(*t).clone();
                    return Err(self.error(token, "Expect variable name."));
                }
            }
            _ => unreachable!("ran out of tokens"),
        };

        // TODO: Can be cleaned up into one loop?
        let mut initializer = None;
        if let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::Equal {
                self.tokens.next();
                initializer = Some(self.expression()?);
            }
        }

        if let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::Semicolon {
                self.tokens.next();
            } else {
                let token = &(*t).clone();
                return Err(self.error(token, "Expect ';' after variable declaration"));
            }
        }

        Ok(Stmt::Var(Box::new(VarStmt::new(name.clone(), initializer))))
    }

    fn statement(&mut self) -> Result<Stmt, LoxResult> {
        if let Some(_t) = self.tokens.next_if(|t| t.token_type == TokenType::If) {
            return self.if_statement();
        }
        if let Some(_t) = self.tokens.next_if(|t| t.token_type == TokenType::Print) {
            return self.print_statement();
        }
        if let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::Return {
                return self.return_statement();
            }
        }
        if let Some(_t) = self.tokens.next_if(|t| t.token_type == TokenType::While) {
            return self.while_statement();
        }
        if let Some(_t) = self.tokens.next_if(|t| t.token_type == TokenType::For) {
            return self.for_statement();
        }
        if let Some(_t) = self
            .tokens
            .next_if(|t| t.token_type == TokenType::LeftBrace)
        {
            return Ok(Stmt::Block(Box::new(BlockStmt::new(self.block()?))));
        }

        self.expression_statement()
    }

    fn while_statement(&mut self) -> Result<Stmt, LoxResult> {
        if let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::LeftParen {
                self.tokens.next();
            } else {
                let token = &(*t).clone();
                return Err(self.error(token, "Expect '(' after 'while'."));
            }
        }
        let condition = self.expression()?;
        if let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::RightParen {
                self.tokens.next();
            } else {
                let token = &(*t).clone();
                return Err(self.error(token, "Expect ')' after condition"));
            }
        }
        let body = self.statement()?;

        Ok(Stmt::While(Box::new(WhileStmt::new(condition, body))))
    }

    fn for_statement(&mut self) -> Result<Stmt, LoxResult> {
        if let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::LeftParen {
                self.tokens.next();
            } else {
                let token = &(*t).clone();
                return Err(self.error(token, "Expect '(' after 'for'."));
            }
        }

        let initializer = {
            if let Some(t) = self.tokens.peek() {
                if t.token_type == TokenType::Semicolon {
                    self.tokens.next();
                    None
                } else if t.token_type == TokenType::Var {
                    self.tokens.next();
                    Some(self.var_declaration()?)
                } else {
                    Some(self.expression_statement()?)
                }
            } else {
                unreachable!("Ran out of tokens")
            }
        };

        // Either there is a condition or it an infinite loop (true)
        let condition = {
            if let Some(t) = self.tokens.peek() {
                if t.token_type != TokenType::Semicolon {
                    self.expression()?
                } else {
                    Expr::Literal(Literal::Boolean(true))
                }
            } else {
                unreachable!("Ran out of tokens")
            }
        };

        if let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::Semicolon {
                self.tokens.next();
            } else {
                let token = &(*t).clone();
                return Err(self.error(token, "Expect ';' after loop condition."));
            }
        }

        let increment = {
            if let Some(t) = self.tokens.peek() {
                if t.token_type != TokenType::RightParen {
                    Some(self.expression()?)
                } else {
                    None
                }
            } else {
                unreachable!("Ran out of tokens")
            }
        };

        if let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::RightParen {
                self.tokens.next();
            } else {
                let token = &(*t).clone();
                return Err(self.error(token, "Expect ')' after for clauses."));
            }
        }

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

    fn if_statement(&mut self) -> Result<Stmt, LoxResult> {
        if let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::LeftParen {
                self.tokens.next();
            } else {
                let token = &(*t).clone();
                return Err(self.error(token, "Expect '(' after if."));
            }
        }
        let condition = self.expression()?;
        if let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::RightParen {
                self.tokens.next();
            } else {
                let token = &(*t).clone();
                return Err(self.error(token, "Expect ')' after if condition."));
            }
        }
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

    fn print_statement(&mut self) -> Result<Stmt, LoxResult> {
        let value = self.expression()?;
        if let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::Semicolon {
                self.tokens.next();
            } else {
                let token = &(*t).clone();
                return Err(self.error(token, "Expect ';' after expression"));
            }
        }
        Ok(Stmt::Print(Box::new(PrintStmt::new(value))))
    }

    fn return_statement(&mut self) -> Result<Stmt, LoxResult> {
        let keyword = self.tokens.next().unwrap();
        let value = match self.tokens.peek() {
            Some(t) => {
                if t.token_type != TokenType::Semicolon {
                    Some(self.expression()?)
                } else {
                    None
                }
            }
            None => None,
        };

        if let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::Semicolon {
                self.tokens.next();
            } else {
                let token = &(*t).clone();
                return Err(self.error(token, "Expect ';' after return value."));
            }
        }

        Ok(Stmt::Return(Box::new(ReturnStmt::new(
            keyword.to_owned(),
            value,
        ))))
    }

    fn expression_statement(&mut self) -> Result<Stmt, LoxResult> {
        let expr = self.expression()?;
        if let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::Semicolon {
                self.tokens.next();
            } else {
                let token = &(*t).clone();
                return Err(self.error(token, "Expect ';' after expression."));
            }
        }
        Ok(Stmt::Expression(Box::new(ExpressionStmt::new(expr))))
    }

    fn function(&mut self, kind: &str) -> Result<Stmt, LoxResult> {
        let name = match self.tokens.peek() {
            Some(t) => {
                if let TokenType::Identifier(_) = &t.token_type {
                    self.tokens.next().unwrap()
                } else {
                    let token = &(*t).clone();
                    return Err(self.error(token, &format!("Expect {kind} name.")));
                }
            }
            _ => unreachable!("Ran out of tokens"),
        };

        if let Some(t) = self.tokens.peek() {
            if let TokenType::LeftParen = &t.token_type {
                self.tokens.next();
            } else {
                let token = &(*t).clone();
                return Err(self.error(token, &format!("Expect '(' after {kind} name.")));
            }
        }

        let mut params = Vec::new();
        if let Some(t) = self.tokens.peek().cloned() {
            if t.token_type != TokenType::RightParen {
                // Do-while
                if params.len() >= 255 {
                    return Err(self.error(t, "Can't have more than 255 parameters."));
                }
                params.push(if let TokenType::Identifier(_) = &t.token_type {
                    self.tokens.next().unwrap().clone()
                } else {
                    return Err(self.error(t, "Expect parameter name."));
                });
                while let Some(_nxt_t) = self.tokens.next_if(|t| t.token_type == TokenType::Comma) {
                    if params.len() >= 255 && !self.had_error {
                        let next_t = (*self.tokens.peek().unwrap()).clone();
                        return Err(self.error(&next_t, "Can't have more than 255 parameters."));
                    }
                    params.push(if let TokenType::Identifier(_) = &t.token_type {
                        self.tokens.next().unwrap().clone()
                    } else {
                        return Err(self.error(t, "Expect paramter name."));
                    });
                }
            }
        }

        if let Some(t) = self.tokens.peek() {
            if let TokenType::RightParen = &t.token_type {
                self.tokens.next();
            } else {
                let token = &(*t).clone();
                return Err(self.error(token, "Expect ')' after parameters."));
            }
        }

        if let Some(t) = self.tokens.peek() {
            if let TokenType::LeftBrace = &t.token_type {
                self.tokens.next();
            } else {
                let token = &(*t).clone();
                return Err(self.error(token, &format!("Expect '{{' before {kind} body.")));
            }
        }

        let body = self.block()?;

        Ok(Stmt::Function(Box::new(FunctionStmt::new(
            name.clone(),
            &params,
            body,
        ))))
    }

    fn block(&mut self) -> Result<Vec<Stmt>, LoxResult> {
        let mut statements = Vec::new();
        while let Some(t) = self.tokens.peek() {
            match t.token_type {
                TokenType::RightBrace | TokenType::Eof => break,
                _ => statements.push(self.declaration()?),
            }
        }
        if let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::RightBrace {
                self.tokens.next();
            } else {
                let token = &(*t).clone();
                return Err(self.error(token, "Expect '}' after block."));
            }
        }
        Ok(statements)
    }

    fn expression(&mut self) -> Result<Expr, LoxResult> {
        self.conditional()
    }

    fn conditional(&mut self) -> Result<Expr, LoxResult> {
        let mut expr = self.assignment()?; // condition

        if let Some(_t) = self
            .tokens
            .next_if(|t| t.token_type == TokenType::QuestionMark)
        {
            let left = self.expression()?;
            if let Some(t) = self.tokens.peek() {
                if t.token_type == TokenType::Colon {
                    self.tokens.next();
                } else {
                    let token = &(*t).clone();
                    return Err(self.error(token, "Expect ':' after truthy expression"));
                }
            }
            let right = self.conditional()?;
            expr = Expr::Conditional(Box::new(ConditionalExpr::new(expr, left, right)))
        }
        Ok(expr)
    }

    fn assignment(&mut self) -> Result<Expr, LoxResult> {
        let expr = self.logic_or()?;

        if let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::Equal {
                let equals = self.tokens.next().unwrap();
                // Recursively parse right-hand side since assignment is right-associative
                let value = self.assignment()?;

                // TODO: Refactor
                match expr {
                    Expr::Variable(s) => {
                        return Ok(Expr::Assign(Box::new(AssignExpr::new(s.name, value))));
                    }
                    Expr::Get(e) => {
                        return Ok(Expr::Set(Box::new(SetExpr::new(e.object, e.name, value))));
                    }
                    _ => {}
                }
                // NOTE: Err is reported but not thrown here, parser is not in confused state where it needs to panic and sync
                return Err(self.error(equals, "Invalid assignment target."));
            }
        }

        Ok(expr)
    }

    fn logic_or(&mut self) -> Result<Expr, LoxResult> {
        let mut expr = self.logic_and()?;

        while let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::Or {
                let operator = self.tokens.next().unwrap();
                let right = self.logic_and()?;
                expr = Expr::Logical(Box::new(LogicalExpr::new(expr, operator.clone(), right)));
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn logic_and(&mut self) -> Result<Expr, LoxResult> {
        let mut expr = self.equality()?;

        while let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::And {
                let operator = self.tokens.next().unwrap();
                let right = self.equality()?;
                expr = Expr::Logical(Box::new(LogicalExpr::new(expr, operator.clone(), right)));
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, LoxResult> {
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

    fn comparison(&mut self) -> Result<Expr, LoxResult> {
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

    fn term(&mut self) -> Result<Expr, LoxResult> {
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

    fn factor(&mut self) -> Result<Expr, LoxResult> {
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

    fn unary(&mut self) -> Result<Expr, LoxResult> {
        if let Some(t) = self
            .tokens
            .next_if(|t| t.token_type == TokenType::Bang || t.token_type == TokenType::Minus)
        {
            let operator = t;
            let right = self.unary()?;
            let e = Expr::Unary(Box::new(UnaryExpr::new(operator.clone(), right)));
            return Ok(e);
        }
        self.call()
    }

    fn call(&mut self) -> Result<Expr, LoxResult> {
        let mut expr = self.primary()?;

        // Deliberate loop. Setting up for parsing object properties later on.
        loop {
            if let Some(_t) = self
                .tokens
                .next_if(|t| t.token_type == TokenType::LeftParen)
            {
                expr = self.finish_call(expr)?;
            } else if let Some(_t) = self.tokens.next_if(|t| t.token_type == TokenType::Dot) {
                let name = match self.tokens.peek() {
                    Some(t) => {
                        if let TokenType::Identifier(_) = &t.token_type {
                            self.tokens.next().unwrap()
                        } else {
                            let token = &(*t).clone();
                            return Err(self.error(token, "Expect property name after '.'."));
                        }
                    }
                    None => unreachable!("Ran out of tokens"),
                };

                expr = Expr::Get(Box::new(GetExpr::new(name.clone(), expr)));
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, LoxResult> {
        let mut arguments = Vec::new();

        if let Some(t) = self.tokens.peek() {
            if t.token_type != TokenType::RightParen {
                // Do-while
                arguments.push(self.expression()?);
                while let Some(_nxt_t) = self.tokens.next_if(|t| t.token_type == TokenType::Comma) {
                    if arguments.len() >= 255 && !self.had_error {
                        let token = &(*self.tokens.peek().unwrap()).clone();
                        return Err(self.error(token, "Can't have more than 255 arguments."));
                    } else {
                        arguments.push(self.expression()?);
                    }
                }
            }
        }

        let paren = if let Some(t) = self.tokens.peek() {
            if t.token_type == TokenType::RightParen {
                self.tokens.next().unwrap()
            } else {
                let token = &(*t).clone();
                return Err(self.error(token, "Expect ')' after arguments."));
            }
        } else {
            unreachable!("ran out of tokens lol")
        };

        Ok(Expr::Call(Box::new(CallExpr::new(
            callee,
            paren.clone(),
            arguments,
        ))))
    }

    // TODO: Error propagation and handle panics.
    fn primary(&mut self) -> Result<Expr, LoxResult> {
        if let Some(t) = self.tokens.next() {
            match &t.token_type {
                TokenType::False => Ok(Expr::Literal(Literal::Boolean(false))),
                TokenType::True => Ok(Expr::Literal(Literal::Boolean(true))),
                TokenType::Nil => Ok(Expr::Literal(Literal::Nil)),
                TokenType::String(s) => Ok(Expr::Literal(Literal::String(s.to_string()))),
                TokenType::Number(n) => Ok(Expr::Literal(Literal::Number(*n))),
                TokenType::This => Ok(Expr::This(Box::new(ThisExpr::new(t.clone())))),
                TokenType::Super => {
                    let keyword = t;
                    if let Some(t) = self.tokens.peek() {
                        if t.token_type == TokenType::Dot {
                            self.tokens.next();
                        } else {
                            let token = &(*t).clone();
                            return Err(self.error(token, "Expect '.' after 'super'."));
                        }
                    }

                    let method = if let Some(t) = self.tokens.peek() {
                        if let TokenType::Identifier(_) = &t.token_type {
                            self.tokens.next().unwrap()
                        } else {
                            let token = &(*t).clone();
                            return Err(self.error(token, "Expect superclass method name."));
                        }
                    } else {
                        unreachable!("Ran out of tokens")
                    };

                    Ok(Expr::Super(Box::new(SuperExpr::new(
                        keyword.clone(),
                        method.clone(),
                    ))))
                }
                TokenType::LeftParen => {
                    let expr = self.expression()?;
                    if let Some(t) = self.tokens.peek() {
                        if t.token_type == TokenType::RightParen {
                            self.tokens.next();
                        } else {
                            let token = &(*t).clone();
                            return Err(self.error(token, "Expect ')' after expression"));
                        }
                    }
                    Ok(Expr::Grouping(Box::new(GroupingExpr::new(expr))))
                }
                TokenType::Identifier(_) => {
                    Ok(Expr::Variable(Box::new(VariableExpr::new(t.clone()))))
                }
                _ => {
                    // TODO: Error?
                    Err(self.error(t, "Expect expression."))
                }
            }
        } else {
            unreachable!("Parser peeks so it can't run out");
        }
    }

    fn sync(&mut self) {
        while let Some(t) = self.tokens.next() {
            if t.token_type == TokenType::Semicolon {
                return;
            };

            if let Some(t) = self.tokens.peek() {
                match t.token_type {
                    TokenType::Class
                    | TokenType::Fun
                    | TokenType::Var
                    | TokenType::For
                    | TokenType::If
                    | TokenType::While
                    | TokenType::Print
                    | TokenType::Return => return,
                    _ => {}
                }
            }
        }
    }

    fn error(&mut self, token: &Token, message: &str) -> LoxResult {
        self.had_error = true;
        LoxResult::parse_error(token, message)
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
