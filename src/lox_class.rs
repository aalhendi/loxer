use std::hash::Hash;
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use indexmap::IndexMap;

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
    pub superclass: Option<Box<LoxClass>>,
}

impl PartialEq for LoxClass {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Hash for LoxClass {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self as *const _ as usize).hash(state)
    }
}

impl LoxClass {
    pub fn new(
        name: &str,
        superclass: Option<Box<LoxClass>>,
        methods: HashMap<String, LoxFunction>,
    ) -> Self {
        Self {
            name: name.to_string(),
            methods,
            superclass,
        }
    }

    pub fn find_method(&self, name: &str) -> Option<Literal> {
        match self.methods.get(name) {
            Some(m) => Some(Literal::Function(Rc::new(m.clone()))),
            None => {
                if let Some(superclass) = &self.superclass {
                    superclass.find_method(name)
                } else {
                    None
                }
            }
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
        interpreter: &mut Interpreter,
        arguments: Vec<Literal>,
    ) -> Result<Literal, LoxResult> {
        let instance = Rc::new(RefCell::new(LoxInstance::new(self.clone())));
        if let Some(Literal::Function(initializer)) = self.find_method("init") {
            initializer
                .bind_method(&instance)
                .call(interpreter, arguments)?;
        }

        Ok(Literal::Instance(instance))
    }

    fn get_arity(&self) -> usize {
        if let Some(Literal::Function(initializer)) = self.find_method("init") {
            initializer.get_arity()
        } else {
            0
        }
    }

    fn to_string(&self) -> String {
        format!("{} instance", self.name)
    }
}

#[derive(Debug, Clone)]
pub struct LoxInstance {
    class: LoxClass,
    fields: IndexMap<String, Literal>,
}

impl PartialEq for LoxInstance {
    fn eq(&self, other: &Self) -> bool {
        self.class == other.class && self.fields == other.fields
    }
}

impl Hash for LoxInstance {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.class.hash(state);
        for (k, v) in self.fields.iter() {
            k.hash(state);
            v.hash(state);
        }
    }
}

impl LoxInstance {
    pub fn new(class: LoxClass) -> Self {
        Self {
            class,
            fields: IndexMap::new(),
        }
    }

    pub fn get(&self, name: &Token, this: &Rc<RefCell<LoxInstance>>) -> Result<Literal, LoxResult> {
        if let Some(v) = self.fields.get(&name.lexeme) {
            Ok(v.clone())
        } else if let Some(m) = self.class.find_method(&name.lexeme) {
            if let Literal::Function(f) = m {
                Ok(Literal::Function(Rc::new(f.bind_method(this))))
            } else {
                unreachable!("Non-methods are handled beforehand");
            }
        } else {
            Err(LoxResult::new_error(
                name.line,
                &format!("Undefined property {}.", name.lexeme),
            ))
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
