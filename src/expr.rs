#![allow(dead_code)]
use crate::token::Token;
use std::fmt::Display;

#[derive(Debug)]
pub enum Literal {
    Identifier(String),
    Boolean(bool),
    Nil,
    String(String),
    Number(f64),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let v = match self {
            Literal::Identifier(i) => i.to_owned(),
            Literal::Boolean(b) => b.to_string(),
            Literal::Nil => String::from("Nil"),
            Literal::String(s) => format!("\"{s}\""),
            Literal::Number(n) => n.to_string(),
        };
        write!(f, "{v}")
    }
}

pub enum Expr {
    // Assign,
    Binary(Box<BinaryExpr>),
    // Call,
    // Get,
    Grouping(Box<GroupingExpr>),
    Literal(Literal),
    // Logical,
    // Set,
    // Super,
    // This,
    Unary(Box<UnaryExpr>),
    // VisitVariable,
}

pub struct BinaryExpr {
    left: Expr,
    operator: Token,
    right: Expr,
}

impl BinaryExpr {
    pub fn new(left: Expr, operator: Token, right: Expr) -> Self {
        Self {
            left,
            operator,
            right,
        }
    }
}

impl Display for BinaryExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            parenthesize(&self.operator.lexeme, &[&self.left, &self.right])
        )
    }
}

pub struct GroupingExpr {
    expression: Expr,
}

impl GroupingExpr {
    pub fn new(expression: Expr) -> Self {
        Self { expression }
    }
}

impl Display for GroupingExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", parenthesize("group", &[&self.expression]))
    }
}

pub struct LiteralExpr {
    value: Literal,
}

impl LiteralExpr {
    pub fn new(value: Literal) -> Self {
        Self { value }
    }
}

impl Display for LiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

pub struct UnaryExpr {
    operator: Token,
    right: Expr,
}

impl UnaryExpr {
    pub fn new(operator: Token, right: Expr) -> Self {
        Self { operator, right }
    }
}

impl Display for UnaryExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", parenthesize(&self.operator.lexeme, &[&self.right]))
    }
}

fn parenthesize(name: &str, exprs: &[&Expr]) -> String {
    let mut builder = String::new();
    builder.push('(');
    builder.push_str(name);
    for expr in exprs {
        builder.push(' ');
        let res = match expr {
            Expr::Binary(e) => parenthesize(&e.operator.lexeme, &[&e.left, &e.right]),
            Expr::Grouping(e) => parenthesize("group", &[&e.expression]),
            Expr::Literal(l) => format!("{l}"),
            Expr::Unary(e) => parenthesize(&e.operator.lexeme, &[&e.right]),
        };
        builder.push_str(&res);
    }
    builder.push(')');

    builder
}

#[cfg(test)]
#[test]
fn test_expr() {
    use crate::token::TokenType;

    let left = Expr::Unary(Box::new(UnaryExpr::new(
        Token::new(TokenType::Minus, "-".to_owned(), 1),
        Expr::Literal(Literal::Number(123.0)),
    )));
    let right = Expr::Grouping(Box::new(GroupingExpr::new(Expr::Literal(Literal::Number(
        45.76,
    )))));
    let expression = BinaryExpr::new(left, Token::new(TokenType::Star, "*".to_owned(), 1), right);

    assert_eq!(expression.to_string(), "(* (- 123) (group 45.76))");

    let left = Expr::Unary(Box::new(UnaryExpr::new(
        Token::new(TokenType::Bang, "!".to_owned(), 1),
        Expr::Literal(Literal::Boolean(true)),
    )));
    let right = Expr::Binary(Box::new(BinaryExpr::new(
        Expr::Literal(Literal::String("Hello".to_owned())),
        Token::new(TokenType::BangEqual, "!=".to_owned(), 1),
        Expr::Literal(Literal::String("World".to_owned())),
    )));
    let expression = BinaryExpr::new(
        left,
        Token::new(TokenType::EqualEqual, "==".to_owned(), 1),
        right,
    );

    assert_eq!(
        expression.to_string(),
        "(== (! true) (!= \"Hello\" \"World\"))"
    )
}
