use crate::{
    environment::Environment,
    expr::{Expr, Literal},
    lox_error::LoxError,
    stmt::Stmt,
    token::{Token, TokenType},
};

pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            environment: Environment::new(None),
        }
    }

    pub fn interpret(&mut self, statements: &[Stmt]) -> Result<(), LoxError> {
        for s in statements {
            self.execute(s)?;
        }
        Ok(())
    }

    fn execute(&mut self, s: &Stmt) -> Result<(), LoxError> {
        match s {
            Stmt::Block(s) => {
                self.execute_block(
                    &s.statements,
                    Environment::new(Some(Box::new(self.environment.clone()))),
                )?;
            }
            Stmt::Class(_) => todo!(),
            Stmt::Expression(s) => {
                self.evaluate(&s.expression)?;
            }
            Stmt::Function(_) => todo!(),
            Stmt::If(s) => {
                let condition_expr = self.evaluate(&s.condition)?;
                if self.is_truthy(condition_expr) {
                    self.execute(&s.then_branch)?;
                } else if let Some(else_branch) = &s.else_branch {
                    self.execute(else_branch)?;
                }
            }
            Stmt::Print(s) => {
                let value = self.evaluate(&s.expression)?;
                println!("{value}");
            }
            Stmt::Return(_) => todo!(),
            Stmt::Var(s) => {
                let value = if let Some(initializer) = &s.initializer {
                    self.evaluate(initializer)?
                } else {
                    Literal::Nil
                };
                self.environment.define(&s.name.lexeme, value);
            }
            Stmt::While(_) => todo!(),
        };
        Ok(())
    }

    fn execute_block(
        &mut self,
        statements: &[Stmt],
        environment: Environment,
    ) -> Result<(), LoxError> {
        let previous = self.environment.clone();
        self.environment = environment;
        for s in statements {
            self.execute(s)?;
        }
        self.environment = previous;

        Ok(())
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<Literal, LoxError> {
        match expr {
            Expr::Binary(e) => {
                let left = self.evaluate(&e.left)?;
                let right = self.evaluate(&e.right)?;
                match e.operator.token_type {
                    TokenType::Minus => {
                        let (n1, n2) = self.check_num(&left, &right, &e.operator)?;
                        Ok(Literal::Number(n1 - n2))
                    }
                    TokenType::Slash => {
                        let (n1, n2) = self.check_num(&left, &right, &e.operator)?;
                        if n2 == 0.0 {
                            return Err(LoxError::new(e.operator.line, "Division by zero"));
                        }
                        Ok(Literal::Number(n1 / n2))
                    }
                    TokenType::Star => {
                        let (n1, n2) = self.check_num(&left, &right, &e.operator)?;
                        Ok(Literal::Number(n1 * n2))
                    }
                    TokenType::Greater => {
                        let (n1, n2) = self.check_num(&left, &right, &e.operator)?;
                        Ok(Literal::Boolean(n1 > n2))
                    }
                    TokenType::GreaterEqual => {
                        let (n1, n2) = self.check_num(&left, &right, &e.operator)?;
                        Ok(Literal::Boolean(n1 >= n2))
                    }
                    TokenType::Less => {
                        let (n1, n2) = self.check_num(&left, &right, &e.operator)?;
                        Ok(Literal::Boolean(n1 < n2))
                    }
                    TokenType::LessEqual => {
                        let (n1, n2) = self.check_num(&left, &right, &e.operator)?;
                        Ok(Literal::Boolean(n1 <= n2))
                    }
                    TokenType::BangEqual => Ok(Literal::Boolean(!self.is_equal(left, right))),
                    TokenType::EqualEqual => Ok(Literal::Boolean(self.is_equal(left, right))),
                    TokenType::Plus => match (left, right) {
                        (Literal::String(mut s1), Literal::String(s2)) => {
                            s1.push_str(&s2);
                            Ok(Literal::String(s1))
                        }
                        (Literal::String(s), Literal::Number(n)) => {
                            Ok(Literal::String(format!("{s}{n}")))
                        }
                        (Literal::Number(n), Literal::String(s)) => {
                            Ok(Literal::String(format!("{n}{s}")))
                        }
                        (Literal::Number(n1), Literal::Number(n2)) => Ok(Literal::Number(n1 + n2)),
                        _ => Err(LoxError::new(
                            e.operator.line,
                            &format!(
                                "Operands must both be numbers. Operator: `{lexeme}`",
                                lexeme = e.operator.lexeme
                            ),
                        )),
                    },
                    _ => unreachable!("Invalid operator?"),
                }
            }
            Expr::Grouping(e) => self.evaluate(&e.expression),
            Expr::Literal(e) => Ok(e.clone()), // TODO: ?
            Expr::Unary(e) => {
                let right = self.evaluate(&e.right)?;
                match e.operator.token_type {
                    TokenType::Minus => match right {
                        Literal::Number(n) => Ok(Literal::Number(-n)),
                        _ => Err(LoxError::new(
                            e.operator.line,
                            &format!(
                                "Operand must be a number. Operator: `{lexeme}`",
                                lexeme = e.operator.lexeme
                            ),
                        )),
                    },
                    TokenType::Bang => Ok(Literal::Boolean(!self.is_truthy(right))),
                    _ => unreachable!("Invalid operator?"),
                }
            }
            Expr::Conditional(e) => {
                let condition = self.evaluate(&e.condition)?;
                if self.is_truthy(condition) {
                    Ok(self.evaluate(&e.left)?)
                } else {
                    Ok(self.evaluate(&e.right)?)
                }
            }
            // TODO: Clone?
            Expr::Variable(e) => self.environment.get(e.name.clone()),
            Expr::Assign(e) => {
                let value = self.evaluate(&e.value)?;
                self.environment.assign(e.name.clone(), value.clone())?;
                Ok(value)
            }
        }
    }

    fn check_num(
        &self,
        left: &Literal,
        right: &Literal,
        op: &Token,
    ) -> Result<(f64, f64), LoxError> {
        match (left, right) {
            (Literal::Number(n1), Literal::Number(n2)) => Ok((*n1, *n2)),
            _ => Err(LoxError::new(
                op.line,
                &format!(
                    "Operands must be numbers. Operator: `{lexeme}`",
                    lexeme = op.lexeme
                ),
            )),
        }
    }

    /// Lox's (and Ruby's) definition of truthy. Only ``false`` and ``nil`` are falsey.
    fn is_truthy(&self, e: Literal) -> bool {
        match e {
            Literal::Boolean(b) => b,
            Literal::Nil => false,
            Literal::Identifier(_) | Literal::String(_) | Literal::Number(_) => true,
        }
    }

    /// NOT following IEEE 754. Nil == Nil is possible. <https://en.wikipedia.org/wiki/IEEE_754>
    fn is_equal(&self, a: Literal, b: Literal) -> bool {
        match (a, b) {
            (Literal::Nil, Literal::Nil) => true,
            (Literal::Nil, _) => false,
            (left, right) => left == right,
        }
    }
}

#[cfg(test)]

mod tests {
    use crate::{
        expr::{BinaryExpr, Expr, Literal},
        token::{Token, TokenType},
    };

    use super::Interpreter;

    #[test]
    fn test_multiplication() {
        let left = Expr::Literal(Literal::Number(2.0));
        let right = Expr::Literal(Literal::Number(8.0));
        let e = Expr::Binary(Box::new(BinaryExpr::new(
            left,
            Token::new(TokenType::Star, "*".to_owned(), 1),
            right,
        )));

        let mut interpreter = Interpreter::new();
        let result = interpreter.evaluate(&e);
        match result {
            Ok(e) => assert_eq!(e, Literal::Number(16.0)),
            Err(_) => unreachable!(),
        }
    }

    #[test]
    fn test_precedence() {
        let left = Expr::Binary(Box::new(BinaryExpr::new(
            Expr::Literal(Literal::Number(1.0)),
            Token::new(TokenType::Plus, "+".to_owned(), 1),
            Expr::Literal(Literal::Number(2.0)),
        )));
        let right = Expr::Binary(Box::new(BinaryExpr::new(
            Expr::Literal(Literal::Number(4.0)),
            Token::new(TokenType::Minus, "-".to_owned(), 1),
            Expr::Literal(Literal::Number(5.0)),
        )));
        let e = Expr::Binary(Box::new(BinaryExpr::new(
            left,
            Token::new(TokenType::Star, "*".to_owned(), 1),
            right,
        )));

        let mut interpreter = Interpreter::new();
        let result = interpreter.evaluate(&e);
        match result {
            Ok(e) => assert_eq!(e, Literal::Number(-3.0)),
            Err(_) => unreachable!(),
        }
    }
}
