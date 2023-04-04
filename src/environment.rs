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
}
