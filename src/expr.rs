use crate::functions::LoxFunction;
use crate::lox_class::{LoxClass, LoxInstance};
use crate::{interpreter::Interpreter, lox_result::LoxResult, token::Token};
use std::cell::RefCell;
use std::fmt::{self, Display, Formatter};
use std::hash::Hash;
use std::rc::Rc;

#[derive(Clone)]
pub enum Literal {
    #[allow(unused)]
    Identifier(String),
    Boolean(bool),
    Nil,
    String(String),
    Number(f64),
    Function(Rc<LoxFunction>),
    NativeFunction(Rc<dyn LoxCallable>),
    Class(LoxClass),
    Instance(Rc<RefCell<LoxInstance>>),
}

// TODO: Verify
impl Eq for Literal {
    fn assert_receiver_is_total_eq(&self) {}
}

impl core::fmt::Debug for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Identifier(arg0) => f.debug_tuple("Identifier").field(arg0).finish(),
            Self::Boolean(arg0) => f.debug_tuple("Boolean").field(arg0).finish(),
            Self::Nil => write!(f, "Nil"),
            Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
            Self::Number(arg0) => f.debug_tuple("Number").field(arg0).finish(),
            Self::NativeFunction(arg0) => f
                .debug_tuple("NativeFunction")
                .field(&arg0.to_string())
                .finish(),
            Self::Function(arg0) => f.debug_tuple("Function").field(&arg0.to_string()).finish(),
            Self::Class(arg0) => f.debug_tuple("Class").field(arg0).finish(),
            Self::Instance(arg0) => f.debug_tuple("Instance").field(arg0).finish(),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let v = match self {
            Literal::Identifier(i) => i.to_owned(),
            Literal::Boolean(b) => b.to_string(),
            Literal::Nil => String::from("Nil"),
            Literal::NativeFunction(f) => f.to_string(),
            Literal::String(s) => format!("\"{s}\""),
            Literal::Number(n) => n.to_string(),
            Literal::Function(f) => f.to_string(),
            Literal::Class(c) => LoxCallable::to_string(c),
            Literal::Instance(i) => i.borrow().to_string(),
        };
        write!(f, "{v}")
    }
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Identifier(l0), Self::Identifier(r0)) => l0 == r0,
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::Function(_l0), Self::Function(_r0)) => todo!("idk, placeholder"),
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

// TODO: Static lifetime for function thing?
pub trait LoxCallable {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Literal>,
    ) -> Result<Literal, LoxResult>;
    fn get_arity(&self) -> usize;
    fn to_string(&self) -> String;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Assign(Box<AssignExpr>),
    Binary(Box<BinaryExpr>),
    Call(Box<CallExpr>),
    Conditional(Box<ConditionalExpr>), // Ternary
    Get(Box<GetExpr>),
    Grouping(Box<GroupingExpr>),
    Literal(Literal),
    Logical(Box<LogicalExpr>),
    Set(Box<SetExpr>),
    Super(Box<SuperExpr>),
    This(Box<ThisExpr>),
    Unary(Box<UnaryExpr>),
    Variable(Box<VariableExpr>),
}

impl Hash for Expr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self as *const _ as usize).hash(state)
    }
}

// NOTE: This is used to display reverse Polish notation (RPN)
impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{expr}",
            expr = match self {
                Expr::Assign(e) => format!("{e}"),
                Expr::Binary(e) => format!("{e}"),
                Expr::Call(_e) => todo!("Rpn?"),
                Expr::Conditional(e) => format!("{e}"),
                Expr::Grouping(e) => format!("{e}"),
                Expr::Literal(e) => format!("{e}"),
                Expr::Logical(e) => format!("{e}"),
                Expr::Super(e) => format!("{e}"), // TODO: Verify
                Expr::This(e) => format!("{e}"), // TODO: Verify
                Expr::Unary(e) => format!("{e}"),
                Expr::Variable(e) => format!("{e}"),
                Expr::Get(_e) => todo!("Getexpr"),
                Expr::Set(_e) => todo!("Setexpr"),
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

    #[allow(unused)]
    fn to_rpn(&self) -> String {
        walk_rpn(&self.operator.lexeme, &[&self.left, &self.right])
    }
}

impl Display for BinaryExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            parenthesize(&self.operator.lexeme, &[&self.left, &self.right])
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallExpr {
    pub callee: Expr,
    pub paren: Token,
    pub arguments: Vec<Expr>,
}

impl CallExpr {
    pub fn new(callee: Expr, paren: Token, arguments: Vec<Expr>) -> Self {
        Self {
            callee,
            paren,
            arguments,
        }
    }

    // fn to_rpn(&self) -> String {
    //     walk_rpn()
    // }
}

// impl Display for CallExpr {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(
//             f,
//             "{}",
//             parenthesize("call", &[&self.callee, &self.arguments])
//         )
//     }
// }

#[derive(Debug, Clone, PartialEq, Eq)]
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            parenthesize("?:", &[&self.condition, &self.left, &self.right])
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GroupingExpr {
    pub expression: Expr,
}

impl GroupingExpr {
    pub fn new(expression: Expr) -> Self {
        Self { expression }
    }

    #[allow(unused)]
    fn to_rpn(&self) -> String {
        walk_rpn("group", &[&self.expression])
    }
}

impl Display for GroupingExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", parenthesize("group", &[&self.expression]))
    }
}

// #[derive(Debug)]
// pub struct LiteralExpr {
//     value: Literal,
// }

// impl LiteralExpr {
//     pub fn new(value: Literal) -> Self {
//         Self { value }
//     }
// }

// impl Display for LiteralExpr {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         write!(f, "{}", self.value)
//     }
// }

#[derive(Debug, Clone, PartialEq, Eq)]
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

    #[allow(unused)]
    fn to_rpn(&self) -> String {
        walk_rpn(&self.operator.lexeme, &[&self.left, &self.right])
    }
}

impl Display for LogicalExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            parenthesize(&self.operator.lexeme, &[&self.left, &self.right])
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnaryExpr {
    pub operator: Token,
    pub right: Expr,
}

impl UnaryExpr {
    pub fn new(operator: Token, right: Expr) -> Self {
        Self { operator, right }
    }

    #[allow(unused)]
    fn to_rpn(&self) -> String {
        walk_rpn(&self.operator.lexeme, &[&self.right])
    }
}

impl Display for UnaryExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", parenthesize(&self.operator.lexeme, &[&self.right]))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariableExpr {
    pub name: Token,
}

impl VariableExpr {
    pub fn new(name: Token) -> Self {
        Self { name }
    }
}

impl Display for VariableExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", parenthesize(&self.name.lexeme, &[]))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", parenthesize(&self.name.lexeme, &[&self.value]))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GetExpr {
    pub object: Expr,
    pub name: Token,
}

impl GetExpr {
    pub fn new(name: Token, object: Expr) -> Self {
        Self { name, object }
    }
}

impl Display for GetExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", parenthesize(&self.name.lexeme, &[&self.object]))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SetExpr {
    pub object: Expr,
    pub name: Token,
    pub value: Expr,
}

impl SetExpr {
    pub fn new(object: Expr, name: Token, value: Expr) -> Self {
        Self {
            name,
            object,
            value,
        }
    }
}

impl Display for SetExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", parenthesize(&self.name.lexeme, &[&self.object]))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ThisExpr {
    pub keyword: Token,
}

impl ThisExpr {
    pub fn new(keyword: Token) -> Self {
        Self { keyword }
    }
}

impl Display for ThisExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", parenthesize(&self.keyword.lexeme, &[]))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SuperExpr {
    pub keyword: Token,
    pub method: Token,
}

impl SuperExpr {
    pub fn new(keyword: Token, method: Token) -> Self {
        Self { keyword, method }
    }
}

impl Display for SuperExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", parenthesize(&self.keyword.lexeme, &[]))
        // TODO: AST printer
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
            Expr::Call(_e) => todo!("Impl display"),
            Expr::Conditional(e) => parenthesize("?:", &[&e.condition, &e.left, &e.right]),
            Expr::Grouping(e) => parenthesize("group", &[&e.expression]),
            Expr::Get(e) => parenthesize("get", &[&e.object]), // TODO: Check?
            Expr::Literal(l) => format!("{l}"),
            Expr::Logical(e) => parenthesize(&e.operator.lexeme, &[&e.left, &e.right]),
            Expr::Set(e) => parenthesize("Set", &[&e.object, &e.value]), // TODO: Check?
            Expr::Super(_e) => parenthesize("Super", &[]), // TODO: Fix
            Expr::This(e) => format!("{e}"),
            Expr::Unary(e) => parenthesize(&e.operator.lexeme, &[&e.right]),
            Expr::Variable(e) => format!("{e}"),
        };
        builder.push_str(&res);
    }
    builder.push(')');

    builder
}

#[allow(unused)]
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
            Expr::Call(_e) => todo!("?"),
            Expr::Conditional(_e) => todo!("RPN for conditional expressions"), // TODO: RPN isn't expressive enough for ternary?
            Expr::Grouping(e) => format!("{} group", e.expression),
            Expr::Get(_e) => todo!("RPN for get exprs"),
            Expr::Literal(l) => {
                format!("{l}")
            }
            Expr::Logical(e) => format!("{} {} {}", e.left, e.right, e.operator.lexeme),
            Expr::Set(_e) => todo!("RPN for set exprs"),
            Expr::Super(_e) => todo!("RPN for super exprs"),
            Expr::This(e) => format!("{e}"),
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
