use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum LoxValue {
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
}

impl fmt::Display for LoxValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoxValue::String(value) => {
                write!(f, "\"{}\"", value)
            }
            LoxValue::Bool(bool) => {
                write!(f, "{}", bool)
            }
            LoxValue::Nil => {
                write!(f, "nil")
            }
            LoxValue::Number(value) => write!(f, "{}", value),
        }
    }
}

impl LoxValue {
    pub fn truthy(&self) -> LoxValue {
        match self {
            LoxValue::Nil => LoxValue::Bool(false),
            LoxValue::Bool(bool) => LoxValue::Bool(*bool),
            _ => LoxValue::Bool(true),
        }
    }

    pub fn to_bool(&self) -> bool {
        match self {
            LoxValue::Bool(bool) => *bool,
            _ => self.truthy().to_bool(),
        }
    }
}
