use std::collections::HashMap;

use crate::error::RuntimeError;
use crate::value::LoxValue;
pub struct Environment {
    values: Vec<HashMap<String, LoxValue>>,
    parent: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: vec![HashMap::new()],
            parent: None,
        }
    }

    pub fn push_env(&mut self) {
        self.values.push(HashMap::new())
    }

    pub fn pop_env(&mut self) {
        self.values.pop();
    }

    pub fn get(&self, name: &str) -> LoxValue {
        let last = self.values.len() - 1;
        match self.values[last].get(name) {
            Some(value) => value.clone(),
            None => match &self.parent {
                Some(environment) => environment.get(name),
                None => LoxValue::Nil,
            },
        }
    }

    pub fn put(&mut self, name: String, value: LoxValue) {
        let last = self.values.len() - 1;
        self.values[last].insert(name, value);
    }

    pub fn assign(&mut self, name: String, value: LoxValue) -> Result<LoxValue, RuntimeError> {
        let last = self.values.len() - 1;
        if self.values[last].contains_key(&name) {
            self.values[last].insert(name, value.clone());
            Ok(value)
        } else {
            // TODO: Fix this line, add the actual undefined variable name
            Err(RuntimeError::new("Assignment to undefined variable", 0))
        }
    }
}
