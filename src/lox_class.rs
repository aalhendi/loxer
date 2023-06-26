use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{
    expr::{Literal, LoxCallable},
    functions::LoxFunction,
    interpreter::Interpreter,
    lox_result::LoxResult,
    token::Token,
};

#[derive(Debug, Clone)]
pub struct LoxClass {
    pub name: String,
    pub methods: HashMap<String, LoxFunction>,
}

impl LoxClass {
    pub fn new(name: &str, methods: HashMap<String, LoxFunction>) -> Self {
        Self {
            name: name.to_string(),
            methods,
        }
    }

    pub fn find_method(&self, name: &Token) -> Result<Literal, LoxResult> {
        match self.methods.get(&name.lexeme) {
            Some(m) => {
                Ok(Literal::Function(m.clone()))
            },
            None => Err(LoxResult::new_error(
                name.line,
                &format!("Undefined property {}.", name.lexeme),
            )),
        }
    }
}

impl Display for LoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl LoxCallable for LoxClass {
    fn call(
        &self,
        _interpreter: &mut Interpreter,
        _arguments: Vec<Literal>,
    ) -> Result<Literal, LoxResult> {
        let instance = LoxInstance::new(self.clone());

        Ok(Literal::Instance(Rc::new(RefCell::new(instance))))
    }

    fn get_arity(&self) -> usize {
        0
    }

    fn to_string(&self) -> String {
        format!("{} instance", self.name)
    }
}

#[derive(Debug, Clone)]
pub struct LoxInstance {
    class: LoxClass,
    fields: HashMap<String, Literal>,
}

impl LoxInstance {
    pub fn new(class: LoxClass) -> Self {
        Self {
            class,
            fields: HashMap::new(),
        }
    }

    pub fn get(&self, name: &Token) -> Result<Literal, LoxResult> {
        match self.fields.get(&name.lexeme) {
            Some(v) => Ok(v.clone()),
            None => self.class.find_method(name),
        }
    }

    pub fn set(&mut self, name: Token, value: Literal) {
        self.fields.insert(name.lexeme, value);
    }
}

impl Display for LoxInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} instance", self.class.name)
    }
}
