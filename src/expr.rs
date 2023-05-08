#![allow(dead_code)]
use crate::token::Token;
use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug)]
pub enum Expr {
    Assign(Box<AssignExpr>),
    Binary(Box<BinaryExpr>),
    // Call,
    Conditional(Box<ConditionalExpr>), // Ternary
    // Get,
    Grouping(Box<GroupingExpr>),
    Literal(Literal),
    Logical(Box<LogicalExpr>),
    // Set,
    // Super,
    // This,
    Unary(Box<UnaryExpr>),
    Variable(Box<VariableExpr>),
}

// NOTE: This is used to display reverse Polish notation (RPN)
impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{expr}",
            expr = match self {
                Expr::Assign(e) => format!("{e}"),
                Expr::Binary(e) => format!("{e}"),
                Expr::Conditional(e) => format!("{e}"),
                Expr::Grouping(e) => format!("{e}"),
                Expr::Literal(e) => format!("{e}"),
                Expr::Logical(e) => format!("{e}"),
                Expr::Unary(e) => format!("{e}"),
                Expr::Variable(e) => format!("{e}"),
            }
        )
    }
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub left: Expr,
    pub operator: Token,
    pub right: Expr,
}

impl BinaryExpr {
    pub fn new(left: Expr, operator: Token, right: Expr) -> Self {
        Self {
            left,
            operator,
            right,
        }
    }

    fn to_rpn(&self) -> String {
        walk_rpn(&self.operator.lexeme, &[&self.left, &self.right])
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

#[derive(Debug)]
pub struct ConditionalExpr {
    pub condition: Expr,
    pub left: Expr,
    pub right: Expr,
}

impl ConditionalExpr {
    pub fn new(condition: Expr, left: Expr, right: Expr) -> Self {
        Self {
            condition,
            left,
            right,
        }
    }

    // fn to_rpn(&self) -> String {
    //     walk_rpn("?:", &[&self.condition, &self.left, &self.right])
    // }
}

impl Display for ConditionalExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            parenthesize("?:", &[&self.condition, &self.left, &self.right])
        )
    }
}

#[derive(Debug)]
pub struct GroupingExpr {
    pub expression: Expr,
}

impl GroupingExpr {
    pub fn new(expression: Expr) -> Self {
        Self { expression }
    }

    fn to_rpn(&self) -> String {
        walk_rpn("group", &[&self.expression])
    }
}

impl Display for GroupingExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", parenthesize("group", &[&self.expression]))
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct LogicalExpr {
    pub left: Expr,
    pub operator: Token,
    pub right: Expr,
}

impl LogicalExpr {
    pub fn new(left: Expr, operator: Token, right: Expr) -> Self {
        Self {
            left,
            operator,
            right,
        }
    }

    fn to_rpn(&self) -> String {
        walk_rpn(&self.operator.lexeme, &[&self.left, &self.right])
    }
}

impl Display for LogicalExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            parenthesize(&self.operator.lexeme, &[&self.left, &self.right])
        )
    }
}

#[derive(Debug)]
pub struct UnaryExpr {
    pub operator: Token,
    pub right: Expr,
}

impl UnaryExpr {
    pub fn new(operator: Token, right: Expr) -> Self {
        Self { operator, right }
    }

    fn to_rpn(&self) -> String {
        walk_rpn(&self.operator.lexeme, &[&self.right])
    }
}

impl Display for UnaryExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", parenthesize(&self.operator.lexeme, &[&self.right]))
    }
}

#[derive(Debug)]
pub struct VariableExpr {
    pub name: Token,
}

impl VariableExpr {
    pub fn new(name: Token) -> Self {
        Self { name }
    }
}

impl Display for VariableExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", parenthesize(&self.name.lexeme, &[]))
    }
}

#[derive(Debug)]
pub struct AssignExpr {
    pub name: Token,
    pub value: Expr,
}

impl AssignExpr {
    pub fn new(name: Token, value: Expr) -> Self {
        Self { name, value }
    }
}

impl Display for AssignExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", parenthesize(&self.name.lexeme, &[&self.value]))
    }
}

fn parenthesize(name: &str, exprs: &[&Expr]) -> String {
    let mut builder = String::new();
    builder.push('(');
    builder.push_str(name);
    for expr in exprs {
        builder.push(' ');
        let res = match expr {
            Expr::Assign(e) => parenthesize(&e.name.lexeme, &[&e.value]), // TODO: Check
            Expr::Binary(e) => parenthesize(&e.operator.lexeme, &[&e.left, &e.right]),
            Expr::Conditional(e) => parenthesize("?:", &[&e.condition, &e.left, &e.right]),
            Expr::Grouping(e) => parenthesize("group", &[&e.expression]),
            Expr::Literal(l) => format!("{l}"),
            Expr::Logical(e) => parenthesize(&e.operator.lexeme, &[&e.left, &e.right]),
            Expr::Unary(e) => parenthesize(&e.operator.lexeme, &[&e.right]),
            Expr::Variable(e) => format!("{e}"),
        };
        builder.push_str(&res);
    }
    builder.push(')');

    builder
}

fn walk_rpn(name: &str, exprs: &[&Expr]) -> String {
    let mut builder = String::new();
    for expr in exprs {
        let res = match expr {
            Expr::Assign(e) => {
                format!("{} {}", e.value, e.name.lexeme)
            }
            Expr::Binary(e) => {
                format!("{} {} {}", e.left, e.right, e.operator.lexeme)
            }
            Expr::Conditional(_e) => todo!("RPN for conditional expressions"), // TODO: RPN isn't expressive enough for ternary?
            Expr::Grouping(e) => format!("{} group", e.expression),
            Expr::Literal(l) => {
                format!("{l}")
            }
            Expr::Logical(e) => format!("{} {} {}", e.left, e.right, e.operator.lexeme),
            Expr::Unary(e) => format!("{} {}", e.right, e.operator),
            Expr::Variable(_e) => todo!(),
        };
        builder.push_str(&res);
        builder.push(' ');
    }
    builder.push_str(name);
    builder
}

#[cfg(test)]
mod tests {
    use crate::{
        expr::{BinaryExpr, ConditionalExpr, Expr, GroupingExpr, Literal, UnaryExpr},
        token::{Token, TokenType},
    };

    fn build_e1() -> BinaryExpr {
        let left = Expr::Binary(Box::new(BinaryExpr::new(
            Expr::Literal(Literal::Number(1.0)),
            Token::new(TokenType::Plus, "+".to_owned(), 1),
            Expr::Literal(Literal::Number(2.0)),
        )));
        let right = Expr::Binary(Box::new(BinaryExpr::new(
            Expr::Literal(Literal::Number(4.0)),
            Token::new(TokenType::Minus, "-".to_owned(), 1),
            Expr::Literal(Literal::Number(3.0)),
        )));

        BinaryExpr::new(left, Token::new(TokenType::Star, "*".to_owned(), 1), right)
    }

    fn build_e2() -> BinaryExpr {
        let left = Expr::Unary(Box::new(UnaryExpr::new(
            Token::new(TokenType::Minus, "-".to_owned(), 1),
            Expr::Literal(Literal::Number(123.0)),
        )));
        let right = Expr::Grouping(Box::new(GroupingExpr::new(Expr::Literal(Literal::Number(
            45.76,
        )))));

        BinaryExpr::new(left, Token::new(TokenType::Star, "*".to_owned(), 1), right)
    }

    fn build_e3() -> ConditionalExpr {
        let condition = Expr::Binary(Box::new(BinaryExpr::new(
            Expr::Literal(Literal::Number(5.0)),
            Token::new(TokenType::Greater, ">".to_owned(), 1),
            Expr::Literal(Literal::Number(6.0)),
        )));
        let left = Expr::Binary(Box::new(BinaryExpr::new(
            Expr::Literal(Literal::Number(1.0)),
            Token::new(TokenType::Plus, "+".to_owned(), 1),
            Expr::Literal(Literal::Number(2.0)),
        )));
        let right = Expr::Binary(Box::new(BinaryExpr::new(
            Expr::Literal(Literal::Number(4.0)),
            Token::new(TokenType::Minus, "-".to_owned(), 1),
            Expr::Literal(Literal::Number(3.0)),
        )));
        ConditionalExpr::new(condition, left, right)
    }

    #[test]
    fn test_expr() {
        let e = build_e2();
        assert_eq!(e.to_string(), "(* (- 123) (group 45.76))");

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
        );

        let e = build_e3();
        assert_eq!(e.to_string(), "(?: (> 5 6) (+ 1 2) (- 4 3))");
    }

    #[test]
    fn test_expr_rpn() {
        let e = build_e1();
        assert_eq!(e.to_rpn(), "1 2 + 4 3 - *");

        let e = UnaryExpr::new(
            Token::new(TokenType::Bang, "!".to_owned(), 1),
            Expr::Literal(Literal::Boolean(true)),
        );
        assert_eq!(e.to_rpn(), "true !");
    }
}
