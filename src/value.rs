use std::fmt;

#[derive(Debug, Clone)]
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
                write!(f, "{}", value)
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
    pub fn is_true(&self) -> bool {
        true
    }
}
