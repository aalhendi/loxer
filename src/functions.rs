// TODO: Rename file to LoxFunction
use std::{cell::RefCell, rc::Rc, time::SystemTime};

use crate::{
    environment::Environment,
    expr::{Literal, LoxCallable},
    interpreter::Interpreter,
    lox_result::LoxResult,
    stmt::FunctionStmt,
};

/// Returns time in seconds from UNIX_EPOCH
pub struct Clock;

impl LoxCallable for Clock {
    fn call(
        &self,
        _interpreter: &mut Interpreter,
        _arguments: Vec<Literal>,
    ) -> Result<Literal, LoxResult> {
        match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
            Ok(n) => Ok(Literal::Number(n.as_secs_f64())),
            Err(_) => todo!(),
        }
    }

    fn get_arity(&self) -> usize {
        0
    }

    fn to_string(&self) -> String {
        "<native fn>".to_string()
    }
}

#[derive(Debug,Clone)]
pub struct LoxFunction {
    pub declaration: FunctionStmt,
    closure: Rc<RefCell<Environment>>,
}

impl LoxFunction {
    pub fn new(declaration: FunctionStmt, closure: Rc<RefCell<Environment>>) -> Self {
        Self {
            declaration,
            closure,
        }
    }
}

impl LoxCallable for LoxFunction {
    // TODO: Simplify environment refs?
    // author does Environment environment = new Environment(interpreter.globals); {java}
    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Literal>,
    ) -> Result<Literal, LoxResult> {
        // Create a new nested environment (scope) for the block. Set enclosing to be the parent scope.
        let environment = Environment::wrap(self.closure.clone());
        for (i, p) in self.declaration.params.iter().enumerate() {
            environment
                .borrow_mut()
                .define(&p.lexeme, arguments.get(i).unwrap().clone())
        }

        match interpreter.execute_block(&self.declaration.body, environment) {
            Err(LoxResult::Return(v)) => {
                // Return stuff
                Ok(v)
            }
            Err(e) => Err(e),
            Ok(_) => Ok(Literal::Nil),
        }
    }

    fn get_arity(&self) -> usize {
        self.declaration.params.len()
    }

    fn to_string(&self) -> String {
        format!("<fn {}>", self.declaration.name.lexeme)
    }
}
