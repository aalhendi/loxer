#![allow(dead_code)]
use crate::{expr::Expr, token::Token};

#[derive(Debug)]
pub enum Stmt {
    Block(Box<BlockStmt>),
    Class(Box<ClassStmt>),
    Expression(Box<ExpressionStmt>),
    Function(Box<FunctionStmt>),
    If(Box<IfStmt>),
    Print(Box<PrintStmt>),
    Return(Box<ReturnStmt>),
    Var(Box<VarStmt>),
    While(Box<WhileStmt>),
}

#[derive(Debug)]
pub struct BlockStmt {
    pub statements: Vec<Stmt>,
}

impl BlockStmt {
    pub fn new(statements: Vec<Stmt>) -> Self {
        Self { statements }
    }
}

#[derive(Debug)]
pub struct ClassStmt {
    name: Token,
    // superclass: superclass, // TODO: ?
    methods: Vec<Stmt>,
}

#[derive(Debug)]
pub struct ExpressionStmt {
    pub expression: Expr,
}

impl ExpressionStmt {
    pub fn new(expression: Expr) -> Self {
        Self { expression }
    }
}

#[derive(Debug)]
pub struct FunctionStmt {
    name: Token,
    params: Vec<Token>,
    body: Vec<Stmt>,
}

#[derive(Debug)]
pub struct IfStmt {
    condition: Expr,
    then_branch: Stmt,
    else_branch: Stmt,
}

#[derive(Debug)]
pub struct PrintStmt {
    pub expression: Expr,
}

impl PrintStmt {
    pub fn new(expression: Expr) -> Self {
        Self { expression }
    }
}

#[derive(Debug)]
pub struct ReturnStmt {
    keyword: Token,
    value: Expr,
}

#[derive(Debug)]
pub struct VarStmt {
    pub name: Token,
    pub initializer: Option<Expr>,
}

impl VarStmt {
    pub fn new(name: Token, initializer: Option<Expr>) -> Self {
        Self { name, initializer }
    }
}

#[derive(Debug)]
pub struct WhileStmt {
    condition: Expr,
    body: Stmt,
}
