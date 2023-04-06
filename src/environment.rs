use std::collections::HashMap;

use crate::value::LoxValue;
pub struct Environment {
    values: HashMap<String, LoxValue>,
    parent: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            parent: None,
        }
    }

    pub fn push_env(self) -> Self {
        Self {
            values: HashMap::new(),
            parent: Some(Box::new(self)),
        }
    }

    pub fn pop_env(self) -> Option<Box<Self>> {
        self.parent
    }

    pub fn get(&self, name: &str) -> LoxValue {
        match self.values.get(name) {
            Some(value) => value.clone(),
            None => match &self.parent {
                Some(environment) => environment.get(name),
                None => LoxValue::Nil,
            },
        }
    }

    pub fn put(&mut self, name: String, value: LoxValue) {
        self.values.insert(name, value);
    }
}
