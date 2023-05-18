use std::time::SystemTime;

use crate::{
    expr::{Literal, LoxCallable},
    interpreter::Interpreter,
    lox_error::LoxError,
};

/// Returns time in seconds from UNIX_EPOCH
pub struct Clock;

impl LoxCallable for Clock {
    fn call(
        &self,
        _interpreter: &mut Interpreter,
        _arguments: Vec<Literal>,
    ) -> Result<Literal, LoxError> {
        match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
            Ok(n) => Ok(Literal::Number(n.as_secs_f64())),
            Err(_) => todo!(),
        }
    }

    fn get_arity(&self) -> usize {
        0
    }
}
