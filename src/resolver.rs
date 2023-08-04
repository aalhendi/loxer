use std::{collections::HashMap, rc::Rc};

use crate::{
    expr::{Expr, ExprId},
    interpreter::Interpreter,
    lox_result::{LoxResult, ParseErrorCause},
    stmt::{FunctionStmt, Stmt},
    token::Token,
};

#[derive(Clone, Copy)]
enum FunctionType {
    None,
    Function,
    Method,
    Initializer,
}

enum ClassType {
    None,
    Class,
    Subclass,
}

pub struct Resolver<'a> {
    pub interpreter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, bool>>,
    current_function: FunctionType,
    current_class: ClassType,
    errors: Vec<ParseErrorCause>,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Self {
            interpreter,
            scopes: vec![],
            current_function: FunctionType::None,
            current_class: ClassType::None,
            errors: vec![],
        }
    }

    // TODO: Refactor
    pub fn resolve_stmts(&mut self, stmts: &[Stmt]) -> Result<(), LoxResult> {
        for s in stmts {
            self.resolve_stmt(s)
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            let errors = std::mem::take(&mut self.errors);
            Err(LoxResult::ParseError { causes: errors })
        }
    }

    fn resolve_stmt(&mut self, s: &Stmt) {
        match s {
            Stmt::Block(s) => {
                self.begin_scope();
                for s in s.statements.iter() {
                    self.resolve_stmt(s)
                }
                self.end_scope();
            }
            Stmt::Class(s) => {
                let enclosing_class = std::mem::replace(&mut self.current_class, ClassType::Class);
                self.declare(&s.name);
                self.define(&s.name);
                if let Some(superclass) = &s.superclass {
                    self.current_class = ClassType::Subclass;

                    if superclass.name.lexeme == s.name.lexeme {
                        self.error(&s.name, "A class can't inherit from itself.");
                    }
                    self.resolve_expr(&Expr::Variable(Rc::new(superclass.clone())));
                    self.begin_scope();
                    self.scopes
                        .last_mut()
                        .unwrap()
                        .insert("super".to_string(), true);
                }
                self.begin_scope();
                self.scopes
                    .last_mut()
                    .unwrap()
                    .insert("this".to_string(), true);
                for method in &s.methods {
                    match method {
                        Stmt::Function(f) => {
                            let declaration = if f.name.lexeme.eq("init") {
                                FunctionType::Initializer
                            } else {
                                FunctionType::Method
                            };
                            self.resolve_function(f, declaration);
                        }
                        _ => unreachable!("Only function statements are stored in class statement methods field."),
                    }
                }
                self.end_scope();
                if s.superclass.is_some() {
                    self.end_scope();
                }
                self.current_class = enclosing_class;
            }
            Stmt::Expression(s) => self.resolve_expr(&s.expression),
            Stmt::Function(s) => {
                self.declare(&s.name);
                self.define(&s.name);
                self.resolve_function(s, FunctionType::Function);
            }
            Stmt::If(s) => {
                self.resolve_expr(&s.condition);
                self.resolve_stmt(&s.then_branch);
                if let Some(else_branch) = s.else_branch.clone() {
                    self.resolve_stmt(&else_branch);
                }
            }
            Stmt::Print(s) => self.resolve_expr(&s.expression),
            Stmt::Return(s) => {
                if matches!(self.current_function, FunctionType::None) {
                    self.error(&s.keyword, "Can't return from top-level code.");
                }
                if let Some(v) = &s.value {
                    if matches!(self.current_function, FunctionType::Initializer) {
                        self.error(&s.keyword, "Can't return a value from an initializer.");
                    }
                    self.resolve_expr(v);
                }
            }
            Stmt::Var(s) => {
                self.declare(&s.name);
                if let Some(i) = s.initializer.as_ref() {
                    self.resolve_expr(i);
                }
                self.define(&s.name);
            }
            Stmt::While(s) => {
                self.resolve_expr(&s.condition);
                self.resolve_stmt(&s.body);
            }
        }
    }

    fn resolve_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Assign(e) => {
                self.resolve_expr(&e.value);
                self.resolve_local(e.id, &e.name);
            }
            Expr::Binary(e) => {
                self.resolve_expr(&e.left);
                self.resolve_expr(&e.right);
            }
            Expr::Call(e) => {
                self.resolve_expr(&e.callee);
                for arg in e.arguments.iter() {
                    self.resolve_expr(arg);
                }
            }
            Expr::Conditional(e) => {
                // TODO: Verify
                self.resolve_expr(&e.condition);
                self.resolve_expr(&e.left);
                self.resolve_expr(&e.right);
            }
            Expr::Grouping(e) => self.resolve_expr(&e.expression),
            // Property dispatch is clearly dynamic since it is not processed during static resolution pass
            Expr::Get(e) => self.resolve_expr(&e.object),
            Expr::Literal(_e) => {}
            Expr::Logical(e) => {
                self.resolve_expr(&e.left);
                self.resolve_expr(&e.right);
            }
            Expr::Set(e) => {
                self.resolve_expr(&e.value);
                self.resolve_expr(&e.object);
            }
            Expr::Super(e) => match self.current_class {
                ClassType::None => {
                    self.error(&e.keyword, "Can't use 'super' outside of a class.");
                }
                ClassType::Class => {
                    self.error(
                        &e.keyword,
                        "Can't use 'super' in a class with no superclass.",
                    );
                }
                ClassType::Subclass => self.resolve_local(e.id, &e.keyword),
            },
            Expr::This(e) => {
                if matches!(self.current_class, ClassType::None) {
                    self.error(&e.keyword, "Can't use 'this' outside of a class.");
                    return;
                }
                self.resolve_local(e.id, &e.keyword);
            }
            Expr::Unary(e) => self.resolve_expr(&e.right),
            Expr::Variable(e) => {
                if let Some(l) = self.scopes.last() {
                    if l.get(&e.name.lexeme) == Some(&false) {
                        self.error(&e.name, "Can't read local variable in its own initializer.");
                    }
                }
                self.resolve_local(e.id, &e.name);
            }
        }
    }

    fn resolve_function(&mut self, f: &FunctionStmt, current_function: FunctionType) {
        let enclosing_is_in_function =
            std::mem::replace(&mut self.current_function, current_function);

        self.begin_scope();
        for p in f.params.iter() {
            self.declare(p);
            self.define(p);
        }
        for s in f.body.iter() {
            self.resolve_stmt(s)
        }
        self.end_scope();
        self.current_function = enclosing_is_in_function;
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last() {
            if scope.contains_key(&name.lexeme) {
                self.error(name, "Already a variable with this name in this scope.");
            }
        }
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.clone(), false);
        }
    }

    fn define(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.clone(), true);
        }
    }

    fn resolve_local(&mut self, expr: ExprId, name: &Token) {
        for (idx, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(&name.lexeme) {
                self.interpreter.resolve(&expr, idx);
                return;
            }
        }
    }

    fn error(&mut self, token: &Token, message: &str) {
        self.errors.push(ParseErrorCause::new(
            token.line,
            Some(token.lexeme.clone()),
            message,
        ));
    }
}
