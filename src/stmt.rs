use crate::{
    expr::{Expr, VariableExpr},
    token::Token,
};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Stmt {
    Block(Box<BlockStmt>),
    Class(Box<ClassStmt>),
    Expression(Box<ExpressionStmt>),
    Function(Rc<FunctionStmt>),
    If(Box<IfStmt>),
    Print(Box<PrintStmt>),
    Return(Box<ReturnStmt>),
    Var(Box<VarStmt>),
    While(Box<WhileStmt>),
}

#[derive(Debug, Clone)]
pub struct BlockStmt {
    pub statements: Vec<Stmt>,
}

impl BlockStmt {
    pub fn new(statements: Vec<Stmt>) -> Self {
        Self { statements }
    }
}

#[derive(Debug, Clone)]
pub struct ClassStmt {
    pub name: Token,
    pub superclass: Option<VariableExpr>,
    pub methods: Vec<Stmt>,
}

impl ClassStmt {
    pub fn new(name: Token, methods: Vec<Stmt>, superclass: Option<VariableExpr>) -> Self {
        Self {
            name,
            methods,
            superclass,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExpressionStmt {
    pub expression: Expr,
}

impl ExpressionStmt {
    pub fn new(expression: Expr) -> Self {
        Self { expression }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionStmt {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}

impl FunctionStmt {
    pub fn new(name: Token, params: Vec<Token>, body: Vec<Stmt>) -> Self {
        Self { name, params, body }
    }
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Stmt,
    pub else_branch: Option<Stmt>,
}

impl IfStmt {
    pub fn new(condition: Expr, then_branch: Stmt, else_branch: Option<Stmt>) -> Self {
        Self {
            condition,
            then_branch,
            else_branch,
        }
    }
}

#[derive(Debug, Clone)]
pub struct PrintStmt {
    pub expression: Expr,
}

impl PrintStmt {
    pub fn new(expression: Expr) -> Self {
        Self { expression }
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub keyword: Token,
    pub value: Option<Expr>,
}

impl ReturnStmt {
    pub fn new(keyword: Token, value: Option<Expr>) -> Self {
        Self { keyword, value }
    }
}

#[derive(Debug, Clone)]
pub struct VarStmt {
    pub name: Token,
    pub initializer: Option<Expr>,
}

impl VarStmt {
    pub fn new(name: Token, initializer: Option<Expr>) -> Self {
        Self { name, initializer }
    }
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: Stmt,
}

impl WhileStmt {
    pub fn new(condition: Expr, body: Stmt) -> Self {
        Self { condition, body }
    }
}
