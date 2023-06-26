use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    environment::Environment,
    expr::{Expr, Literal, LoxCallable, VariableExpr},
    functions::{Clock, LoxFunction},
    lox_class::LoxClass,
    lox_result::LoxResult,
    stmt::Stmt,
    token::{Token, TokenType},
};

pub struct Interpreter {
    pub environment: Rc<RefCell<Environment>>,
    // ERROR(?): Expr needs to impl Hash
    locals: HashMap<Expr, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut environment = Environment::new(None);

        let clock = Literal::NativeFunction(Rc::new(Clock));
        environment.define("clock", clock);

        Self {
            environment: Rc::new(RefCell::new(environment)),
            locals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, statements: &[Stmt]) -> Result<(), LoxResult> {
        for s in statements {
            self.execute(s)?;
        }
        Ok(())
    }

    fn execute(&mut self, s: &Stmt) -> Result<(), LoxResult> {
        match s {
            Stmt::Block(s) => {
                self.execute_block(&s.statements, Environment::wrap(self.environment.clone()))?;
            }
            Stmt::Class(s) => {
                self.environment
                    .borrow_mut()
                    .define(&s.name.lexeme, Literal::Nil);
                let mut methods = HashMap::new();
                for method in &s.methods {
                    match method {
                       Stmt::Function(f)  => {
                        let function = LoxFunction::new(*f.clone(), self.environment.clone());
                        methods.insert(f.name.lexeme.clone(), function);
                       },
                       _ => todo!(),
                    }
                }
                let class = LoxClass::new(&s.name.lexeme, methods);
                self.environment
                    .borrow_mut()
                    .assign(s.name.clone(), Literal::Class(class))?;
            }
            Stmt::Expression(s) => {
                self.evaluate(&s.expression)?;
            }
            Stmt::Function(s) => {
                let function = LoxFunction::new(*s.clone(), self.environment.clone());
                self.environment
                    .borrow_mut()
                    .define(&s.name.lexeme, Literal::Function(function));
            }
            Stmt::If(s) => {
                let condition_expr = self.evaluate(&s.condition)?;
                if self.is_truthy(&condition_expr) {
                    self.execute(&s.then_branch)?;
                } else if let Some(else_branch) = &s.else_branch {
                    self.execute(else_branch)?;
                }
            }
            Stmt::Print(s) => {
                let value = self.evaluate(&s.expression)?;
                println!("{value}");
            }
            Stmt::Return(s) => {
                if let Some(v) = &s.value {
                    return Err(LoxResult::Return(self.evaluate(v)?));
                } else {
                    return Err(LoxResult::Return(Literal::Nil));
                }
            }
            Stmt::Var(s) => {
                let value = if let Some(initializer) = &s.initializer {
                    self.evaluate(initializer)?
                } else {
                    Literal::Nil
                };
                self.environment.borrow_mut().define(&s.name.lexeme, value);
            }
            Stmt::While(s) => {
                let mut condition = self.evaluate(&s.condition)?;
                while self.is_truthy(&condition) {
                    self.execute(&s.body)?;
                    condition = self.evaluate(&s.condition)?;
                }
            }
        };
        Ok(())
    }

    pub fn resolve(&mut self, expr: &Expr, depth: usize) {
        self.locals.insert(expr.clone(), depth);
    }

    pub fn execute_block(
        &mut self,
        statements: &[Stmt],
        environment: Rc<RefCell<Environment>>,
    ) -> Result<(), LoxResult> {
        let previous = self.environment.clone();
        self.environment = environment;
        let result = statements.iter().try_for_each(|s| self.execute(s));

        self.environment = previous;
        result
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<Literal, LoxResult> {
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
                            return Err(LoxResult::new_error(e.operator.line, "Division by zero"));
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
                        _ => Err(LoxResult::new_error(
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
            Expr::Call(e) => {
                let callee = self.evaluate(&e.callee)?;
                let mut arguments = Vec::new();
                for arg in &e.arguments {
                    arguments.push(self.evaluate(arg)?);
                }

                match callee {
                    Literal::Identifier(_)
                    | Literal::Boolean(_)
                    | Literal::Nil
                    | Literal::String(_)
                    | Literal::Instance(_)
                    | Literal::Number(_) => Err(LoxResult::new_error(
                        e.paren.line,
                        "Can only call classes and functions",
                    )),
                    Literal::Function(function) => {
                        if arguments.len() != function.get_arity() {
                            return Err(LoxResult::new_error(
                                e.paren.line,
                                &format!(
                                    "Expected {} arguments but got {}.",
                                    function.get_arity(),
                                    arguments.len()
                                ),
                            ));
                        }

                        function.call(self, arguments)
                    }
                    Literal::NativeFunction(function) => {
                        if arguments.len() != function.get_arity() {
                            return Err(LoxResult::new_error(
                                e.paren.line,
                                &format!(
                                    "Expected {} arguments but got {}.",
                                    function.get_arity(),
                                    arguments.len()
                                ),
                            ));
                        }

                        function.call(self, arguments)
                    }
                    Literal::Class(class) => {
                        // TODO: Is this the way to go or is there a cleaner implementation?
                        let class = &class as &dyn LoxCallable;
                        if arguments.len() != class.get_arity() {
                            return Err(LoxResult::new_error(
                                e.paren.line,
                                &format!(
                                    "Expected {} arguments but got {}.",
                                    class.get_arity(),
                                    arguments.len()
                                ),
                            ));
                        }

                        class.call(self, arguments)
                    }
                }
            }
            Expr::Grouping(e) => self.evaluate(&e.expression),
            Expr::Literal(e) => Ok(e.clone()), // TODO: ?
            Expr::Unary(e) => {
                let right = self.evaluate(&e.right)?;
                match e.operator.token_type {
                    TokenType::Minus => match right {
                        Literal::Number(n) => Ok(Literal::Number(-n)),
                        _ => Err(LoxResult::new_error(
                            e.operator.line,
                            &format!(
                                "Operand must be a number. Operator: `{lexeme}`",
                                lexeme = e.operator.lexeme
                            ),
                        )),
                    },
                    TokenType::Bang => Ok(Literal::Boolean(!self.is_truthy(&right))),
                    _ => unreachable!("Invalid operator?"),
                }
            }
            Expr::Conditional(e) => {
                let condition = self.evaluate(&e.condition)?;
                if self.is_truthy(&condition) {
                    Ok(self.evaluate(&e.left)?)
                } else {
                    Ok(self.evaluate(&e.right)?)
                }
            }
            // TODO: Clone?
            Expr::Variable(e) => self.look_up_variable(e.name.clone(), e.clone()),
            Expr::Assign(e) => {
                let value = self.evaluate(&e.value)?;
                if let Some(distance) = self.locals.get(&Expr::Assign(e.clone())) {
                    self.environment.borrow_mut().assign_at(
                        distance,
                        &e.name.lexeme,
                        value.clone(),
                    )?;
                } else {
                    self.environment
                        .borrow_mut()
                        .assign(e.name.clone(), value.clone())?;
                }
                Ok(value)
            }
            Expr::Logical(e) => {
                let left = self.evaluate(&e.left)?;
                if e.operator.token_type == TokenType::Or {
                    if self.is_truthy(&left) {
                        return Ok(left);
                    }
                } else if !self.is_truthy(&left) {
                    return Ok(left);
                }
                self.evaluate(&e.right)
            }
            Expr::Get(e) => {
                let object = self.evaluate(&e.object)?;
                match object {
                    Literal::Identifier(_)
                    | Literal::Boolean(_)
                    | Literal::Nil
                    | Literal::String(_)
                    | Literal::Number(_)
                    | Literal::NativeFunction(_)
                    | Literal::Function(_)
                    | Literal::Class(_) => Err(LoxResult::new_error(
                        e.name.line,
                        "Only instances have properties",
                    )),
                    Literal::Instance(instance) => instance.borrow().get(&e.name),
                }
            }
            Expr::Set(e) => {
                let object = self.evaluate(&e.object)?;
                match object {
                    Literal::Identifier(_)
                    | Literal::Boolean(_)
                    | Literal::Nil
                    | Literal::String(_)
                    | Literal::Number(_)
                    | Literal::NativeFunction(_)
                    | Literal::Function(_)
                    | Literal::Class(_) => Err(LoxResult::new_error(
                        e.name.line,
                        "Only instances have fields",
                    )),
                    Literal::Instance(instance) => {
                        let value = self.evaluate(&e.value)?;
                        instance.borrow_mut().set(e.name.clone(), value.clone());
                        Ok(value)
                    }
                }
            }
        }
    }

    fn look_up_variable(&self, name: Token, expr: Box<VariableExpr>) -> Result<Literal, LoxResult> {
        if let Some(distance) = self.locals.get(&Expr::Variable(expr)) {
            self.environment.borrow().get_at(distance, &name.lexeme)
        } else {
            self.environment.borrow().get(name)
        }
    }

    fn check_num(
        &self,
        left: &Literal,
        right: &Literal,
        op: &Token,
    ) -> Result<(f64, f64), LoxResult> {
        match (left, right) {
            (Literal::Number(n1), Literal::Number(n2)) => Ok((*n1, *n2)),
            _ => Err(LoxResult::new_error(
                op.line,
                &format!(
                    "Operands must be numbers. Operator: `{lexeme}`",
                    lexeme = op.lexeme
                ),
            )),
        }
    }

    /// Lox's (and Ruby's) definition of truthy. Only ``false`` and ``nil`` are falsey.
    fn is_truthy(&self, e: &Literal) -> bool {
        match e {
            Literal::Boolean(b) => *b,
            Literal::Nil => false,
            Literal::Identifier(_)
            | Literal::String(_)
            | Literal::Number(_)
            | Literal::Class(_)
            | Literal::Instance(_)
            | Literal::NativeFunction(_) => true,
            | Literal::Function(_) => true,
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
