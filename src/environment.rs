use crate::{expr::Literal, lox_error::LoxError, token::Token};
use std::collections::{HashMap, hash_map::Entry::Occupied};

pub struct Environment {
    values: HashMap<String, Literal>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    // NOTE: Shadowing is legal Lox
    /*
    var a = "before";
    print a; // "before".
    var a = "after";
    print a; // "after".
    */
    pub fn define(&mut self, name: &str, value: Literal) {
        self.values.insert(name.to_string(), value);
    }

    pub fn get(&self, name: Token) -> Result<Literal, LoxError> {
        match self.values.get(&name.lexeme) {
            Some(v) => Ok(v.clone()),
            None => Err(LoxError::new(
                name.line,
                &format!("Undefined variable `{}`.", name.lexeme),
            )),
        }
    }

    pub fn assign(&mut self, name: Token, value: Literal) -> Result<(), LoxError> {
        if let Occupied(mut e) = self.values.entry(name.lexeme.clone()) {
            e.insert(value);
            Ok(())
        } else {
            Err(LoxError::new(name.line, &format!("Undefined variable `{}`.", name.lexeme)))
        }
    }
}
