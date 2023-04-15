use crate::{
    expr::{BinaryExpr, Expr, GroupingExpr, Literal, UnaryExpr},
    token::{Token, TokenType},
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

// TODO: Iterators
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    fn expression(&mut self) -> Expr {
        self.equality()
    }

    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();

        while self.matches(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous();
            let right = self.comparison();
            expr = Expr::Binary(Box::new(BinaryExpr::new(expr, operator, right)));
        }
        expr
    }

    fn matches(&mut self, types: &[TokenType]) -> bool {
        for ttype in types.iter() {
            if self.check(ttype) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&self, ttype: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        &self.peek().token_type == ttype
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        let t = self.peek();
        t.token_type == TokenType::Eof
    }

    // TODO: Unwraps
    fn peek(&self) -> Token {
        self.tokens.get(self.current).unwrap().clone()
    }

    fn previous(&self) -> Token {
        self.tokens.get(self.current - 1).unwrap().clone()
    }

    fn comparison(&mut self) -> Expr {
        let mut expr = self.term();

        while self.matches(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.previous();
            let right = self.term();
            expr = Expr::Binary(Box::new(BinaryExpr::new(expr, operator, right)));
        }
        expr
    }

    fn term(&mut self) -> Expr {
        let mut expr = self.factor();

        while self.matches(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous();
            let right = self.factor();
            expr = Expr::Binary(Box::new(BinaryExpr::new(expr, operator, right)));
        }
        expr
    }

    fn factor(&mut self) -> Expr {
        let mut expr = self.unary();

        while self.matches(&[TokenType::Slash, TokenType::Star]) {
            let operator = self.previous();
            let right = self.unary();
            expr = Expr::Binary(Box::new(BinaryExpr::new(expr, operator, right)));
        }
        expr
    }

    fn unary(&mut self) -> Expr {
        if self.matches(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous();
            let right = self.unary();
            return Expr::Unary(Box::new(UnaryExpr::new(operator, right)));
        }
        self.primary()
    }

    fn primary(&mut self) -> Expr {
        if self.matches(&[TokenType::False]) {
            return Expr::Literal(Literal::Boolean(false));
        } else if self.matches(&[TokenType::True]) {
            return Expr::Literal(Literal::Boolean(true));
        } else if self.matches(&[TokenType::Nil]) {
            return Expr::Literal(Literal::Nil);
        } else if self.matches(&[TokenType::Number]) {
            return Expr::Literal(Literal::Number(self.previous().lexeme.parse().unwrap()));
        } else if self.matches(&[TokenType::String]) {
            return Expr::Literal(Literal::String(self.previous().lexeme));
        }

        if self.matches(&[TokenType::LeftParen]) {
            let expr = self.expression();
            self.consume(&TokenType::RightParen, "Expect ')' after expression.");
            return Expr::Grouping(Box::new(GroupingExpr::new(expr)));
        }

        let t = self.peek();
        panic!("{t:?} expected expression");
    }

    // TODO: Error propagation
    fn consume(&mut self, ttype: &TokenType, message: &str) -> Token {
        if self.check(ttype) {
            return self.advance();
        }

        // self.error(self.peek(), message);
        let token = self.peek();
        if token.token_type == TokenType::Eof {
            // TODO: eprintln rather than panic?
            panic!("{} at end {}", token.line, message)
        } else {
            panic!("{} at {} {}", token.line, token.lexeme, message)
        }
    }

    // fn error(&self, token: Token, message: &str) {
    //     if token.token_type == TokenType::Eof {
    //         panic!("{} at end {}", token._line, message)
    //     } else {
    //         panic!("{} at {} {}", token._line, token.lexeme, message)
    //     }
    // }

    #[allow(dead_code)]
    fn sync(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }
            match self.peek().token_type {
                _ => {}
            }

            self.advance();
        }
    }

    pub fn parse(&mut self) -> Expr {
        self.expression()
    }
}
