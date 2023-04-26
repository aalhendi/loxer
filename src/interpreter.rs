use crate::{
    expr::{Expr, Literal},
    lox_error::LoxError,
    token::{Token, TokenType},
};

pub struct Interpreter {}

impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn interpret(self, expr: Expr) -> Result<(), LoxError> {
        let value = self.evaluate(expr)?;
        println!("{}", value);

        Ok(())
    }

    fn evaluate(&self, expr: Expr) -> Result<Literal, LoxError> {
        match expr {
            Expr::Binary(e) => {
                let left = self.evaluate(e.left)?;
                let right = self.evaluate(e.right)?;
                match e.operator.token_type {
                    TokenType::Minus => {
                        let (n1, n2) = self.check_num(left, right, e.operator)?;
                        Ok(Literal::Number(n1 - n2))
                    }
                    TokenType::Slash => {
                        let (n1, n2) = self.check_num(left, right, e.operator)?;
                        Ok(Literal::Number(n1 / n2))
                    }
                    TokenType::Star => {
                        let (n1, n2) = self.check_num(left, right, e.operator)?;
                        Ok(Literal::Number(n1 * n2))
                    }
                    TokenType::Greater => {
                        let (n1, n2) = self.check_num(left, right, e.operator)?;
                        Ok(Literal::Boolean(n1 > n2))
                    }
                    TokenType::GreaterEqual => {
                        let (n1, n2) = self.check_num(left, right, e.operator)?;
                        Ok(Literal::Boolean(n1 >= n2))
                    }
                    TokenType::Less => {
                        let (n1, n2) = self.check_num(left, right, e.operator)?;
                        Ok(Literal::Boolean(n1 < n2))
                    }
                    TokenType::LessEqual => {
                        let (n1, n2) = self.check_num(left, right, e.operator)?;
                        Ok(Literal::Boolean(n1 <= n2))
                    }
                    TokenType::BangEqual => Ok(Literal::Boolean(!self.is_equal(left, right))),
                    TokenType::EqualEqual => Ok(Literal::Boolean(self.is_equal(left, right))),
                    TokenType::Plus => match (left, right) {
                        (Literal::String(mut s1), Literal::String(s2)) => {
                            s1.push_str(&s2);
                            Ok(Literal::String(s1))
                        }
                        (Literal::Number(n1), Literal::Number(n2)) => Ok(Literal::Number(n1 + n2)),
                        _ => Err(LoxError::new(
                            e.operator.line,
                            "Operands must be ",
                        )),
                    },
                    _ => unreachable!("Invalid operator?"),
                }
            }
            Expr::Grouping(e) => self.evaluate(e.expression),
            Expr::Literal(e) => Ok(e),
            Expr::Unary(e) => {
                let right = self.evaluate(e.right)?;
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
        }
    }

    fn check_num(&self, left: Literal, right: Literal, op: Token) -> Result<(f64, f64), LoxError> {
        match (left, right) {
            (Literal::Number(n1), Literal::Number(n2)) => Ok((n1, n2)),
            _ => Err(LoxError::new(
                op.line,
                &format!("Operands must be numbers. Operator: `{lexeme}`", lexeme = op.lexeme),
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
