use crate::{expr::Literal, lox_result::LoxResult, token::Token};
use std::{
    cell::RefCell,
    collections::{hash_map::Entry::Occupied, HashMap},
    rc::Rc,
};

#[derive(Clone, Default, Debug)]
pub struct Environment {
    values: HashMap<String, Literal>,
    pub enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Self {
        Self {
            values: HashMap::new(),
            enclosing,
        }
    }

    pub fn wrap(enclosing: Rc<RefCell<Environment>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            enclosing: Some(enclosing),
            values: HashMap::new(),
        }))
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

    pub fn get(&self, name: &Token) -> Result<Literal, LoxResult> {
        match self.values.get(&name.lexeme) {
            Some(v) => Ok(v.clone()),
            None => {
                if let Some(e) = &self.enclosing {
                    return e.borrow().get(name);
                }

                Err(LoxResult::runtime_error(
                    name,
                    &format!("Undefined variable '{}'.", name.lexeme),
                ))
            }
        }
    }

    pub fn assign(&mut self, name: &Token, value: Literal) -> Result<(), LoxResult> {
        if let Occupied(mut e) = self.values.entry(name.lexeme.clone()) {
            e.insert(value);
            Ok(())
        } else {
            if let Some(e) = &mut self.enclosing {
                return e.borrow_mut().assign(name, value);
            }
            Err(LoxResult::runtime_error(
                name,
                &format!("Undefined variable '{}'.", name.lexeme),
            ))
        }
    }

    pub fn get_at(&self, distance: &usize, name: &str) -> Result<Literal, LoxResult> {
        if distance == &0 {
            Ok(self.values.get(name).unwrap().clone()) // ERROR:  This should be able to unwrap
        } else {
            self.enclosing
                .as_ref()
                .unwrap()
                .borrow()
                .get_at(&(distance - 1), name)
        }
    }

    pub fn assign_at(
        &mut self,
        distance: &usize,
        name: &str,
        value: Literal,
    ) -> Result<(), LoxResult> {
        if distance == &0 {
            self.values.insert(name.to_string(), value);
            Ok(())
        } else {
            self.enclosing
                .as_ref()
                .unwrap()
                .borrow_mut()
                .assign_at(&(distance - 1), name, value)
        }
    }
}
