use std::collections::HashMap;

use crate::{
    expr::Expr,
    interpreter::Interpreter,
    lox_result::LoxResult,
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
}

pub struct Resolver<'a> {
    pub interpreter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, bool>>,
    current_function: FunctionType,
    current_class: ClassType,
    // TODO: had_error? rather than error out during resolving
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Self {
            interpreter,
            scopes: vec![],
            current_function: FunctionType::None,
            current_class: ClassType::None,
        }
    }

    pub fn resolve_stmts(&mut self, stmts: &[Stmt]) -> Result<(), LoxResult> {
        for s in stmts {
            match s {
                Stmt::Block(s) => {
                    self.begin_scope();
                    self.resolve_stmts(&s.statements)?;
                    self.end_scope();
                }
                Stmt::Class(s) => {
                    let enclosing_class =
                        std::mem::replace(&mut self.current_class, ClassType::Class);
                    self.declare(&s.name)?;
                    self.define(&s.name);
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
                                self.resolve_function(f, declaration)?
                            }
                            _ => todo!(),
                        }
                    }
                    self.end_scope();
                    self.current_class = enclosing_class;
                }
                Stmt::Expression(s) => self.resolve_expr(&s.expression)?,
                Stmt::Function(s) => {
                    self.declare(&s.name)?;
                    self.define(&s.name);
                    self.resolve_function(s, FunctionType::Function)?;
                }
                Stmt::If(s) => {
                    self.resolve_expr(&s.condition)?;
                    self.resolve_stmts(&[s.then_branch.clone()])?;
                    if let Some(else_branch) = s.else_branch.clone() {
                        self.resolve_stmts(&[else_branch])?;
                    }
                }
                Stmt::Print(s) => self.resolve_expr(&s.expression)?,
                Stmt::Return(s) => {
                    if matches!(self.current_function, FunctionType::None) {
                        return Err(LoxResult::new_error(
                            s.keyword.line,
                            "Cannot return from top-level code",
                        ));
                    }
                    if let Some(v) = &s.value {
                        if matches!(self.current_function, FunctionType::Initializer) {
                            return Err(LoxResult::new_error(
                                s.keyword.line,
                                "Can't return a value from an initializer.",
                            ));
                        }
                        self.resolve_expr(v)?;
                    }
                }
                Stmt::Var(s) => {
                    self.declare(&s.name)?;
                    if let Some(i) = s.initializer.clone() {
                        self.resolve_expr(&i)?;
                    }
                    self.define(&s.name);
                }
                Stmt::While(s) => {
                    self.resolve_expr(&s.condition)?;
                    self.resolve_stmts(&[s.body.clone()])?;
                }
            }
        }
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &Expr) -> Result<(), LoxResult> {
        match expr {
            Expr::Assign(e) => {
                self.resolve_expr(&e.value)?;
                self.resolve_local(Expr::Assign(e.clone()), &e.name);
            }
            Expr::Binary(e) => {
                self.resolve_expr(&e.left)?;
                self.resolve_expr(&e.right)?;
            }
            Expr::Call(e) => {
                self.resolve_expr(&e.callee)?;
                for arg in e.arguments.iter() {
                    self.resolve_expr(arg)?;
                }
            }
            Expr::Conditional(e) => {
                // TODO: Verify
                self.resolve_expr(&e.condition)?;
                self.resolve_expr(&e.left)?;
                self.resolve_expr(&e.right)?;
            }
            Expr::Grouping(e) => self.resolve_expr(&e.expression)?,
            // Property dispatch is clearly dynamic since it is not processed during static resolution pass
            Expr::Get(e) => self.resolve_expr(&e.object)?,
            Expr::Literal(_e) => {}
            Expr::Logical(e) => {
                self.resolve_expr(&e.left)?;
                self.resolve_expr(&e.right)?;
            }
            Expr::Set(e) => {
                self.resolve_expr(&e.value)?;
                self.resolve_expr(&e.object)?;
            }
            Expr::This(e) => {
                if matches!(self.current_class, ClassType::None) {
                    return Err(LoxResult::new_error(
                        e.keyword.line,
                        "Can't use `this` outside of a class.",
                    ));
                }
                self.resolve_local(Expr::This(e.clone()), &e.keyword);
            }
            Expr::Unary(e) => self.resolve_expr(&e.right)?,
            Expr::Variable(e) => {
                if let Some(l) = self.scopes.last() {
                    if l.get(&e.name.lexeme) == Some(&false) {
                        return Err(LoxResult::new_error(
                            e.name.line,
                            &format!(
                                "{} Can't read local variable in its own initializer",
                                e.name
                            ),
                        ));
                    }
                }
                self.resolve_local(Expr::Variable(e.clone()), &e.name);
            }
        }
        Ok(())
    }

    fn resolve_function(
        &mut self,
        f: &FunctionStmt,
        current_function: FunctionType,
    ) -> Result<(), LoxResult> {
        let enclosing_is_in_function =
            std::mem::replace(&mut self.current_function, current_function);

        self.begin_scope();
        for p in f.params.iter() {
            self.declare(p)?;
            self.define(p);
        }
        self.resolve_stmts(&f.body)?;
        self.end_scope();
        self.current_function = enclosing_is_in_function;
        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Token) -> Result<(), LoxResult> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name.lexeme) {
                return Err(LoxResult::new_error(
                    name.line,
                    "Variable with this name already exists in this scope.",
                ));
            }
            scope.insert(name.lexeme.clone(), false);
        }
        Ok(())
    }

    fn define(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.clone(), true);
        }
    }

    // TODO: only varexpr?
    fn resolve_local(&mut self, expr: Expr, name: &Token) {
        for (idx, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(&name.lexeme) {
                self.interpreter.resolve(&expr, idx);
            }
        }
    }
}
