#[derive(Debug, Clone)]
pub enum LoxValue {
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
}

impl LoxValue {
    pub fn print(&self) -> String {
        match self {
            LoxValue::String(value) => {
                format!("{}", value)
            }
            LoxValue::Bool(bool) => {
                format!("{}", bool)
            }
            LoxValue::Nil => {
                format!("nil")
            }
            LoxValue::Number(value) => format!("{}", value),
        }
    }
}
