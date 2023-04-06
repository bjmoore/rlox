use std::collections::HashMap;

use crate::value::LoxValue;
pub struct Environment {
    pub values: HashMap<String, LoxValue>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> &LoxValue {
        self.values.get(name).unwrap_or(&LoxValue::Nil)
    }

    pub fn put(&mut self, name: String, value: LoxValue) {
        self.values.insert(name, value);
    }
}
