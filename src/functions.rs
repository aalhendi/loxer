// TODO: Rename file to LoxFunction
use std::time::SystemTime;

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

pub struct LoxFunction {
    pub declaration: FunctionStmt,
}

impl LoxFunction {
    pub fn new(declaration: FunctionStmt) -> Self {
        Self { declaration }
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
        // Takes self.environment and sets #Default in its place temporarily
        let tmp = std::mem::take(&mut interpreter.environment);
        // Create a new nested environment (scope) for the block. Set enclosing to be the parent scope.
        interpreter.environment = Environment::new(Some(Box::new(tmp)));
        for (i, p) in self.declaration.params.iter().enumerate() {
            interpreter
                .environment
                .define(&p.lexeme, arguments.get(i).unwrap().clone())
        }

        match interpreter.execute_block(&self.declaration.body) {
            Err(LoxResult::Return(v)) => {
                // Set environment back to the parent scope the same way.
                // NOTE: compiler should realise the tmp store is being elided and remove it in release mode. This just gets around the borrow checker and avoids cloning
                let tmp = std::mem::take(&mut interpreter.environment);
                interpreter.environment = *tmp.enclosing.unwrap();
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
