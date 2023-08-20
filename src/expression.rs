use std::fmt;

use token::Token;

use crate::token;
use crate::value::LoxValue;

// TODO: implement Display
#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Binary(Box<Expr<'a>>, &'a Token, Box<Expr<'a>>),
    Grouping(Box<Expr<'a>>),
    Literal(LoxValue),
    Unary(&'a Token, Box<Expr<'a>>),
    Variable(String), // Identifier names are host-language strings, not LoxValue strings
    Assign(String, Box<Expr<'a>>),
    Logical(Box<Expr<'a>>, &'a Token, Box<Expr<'a>>),
}

impl<'a> fmt::Display for Expr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Literal(value) => {
                write!(f, "({})", value.to_string())
            }
            Self::Assign(name, value) => {
                write!(f, "(= {} {})", name, value.to_string())
            }
            Self::Binary(left, op, right) => {
                write!(
                    f,
                    "({} {} {})",
                    op.to_string(),
                    left.to_string(),
                    right.to_string()
                )
            }
            Self::Grouping(expr) => {
                write!(f, "(group {})", expr.to_string())
            }
            Self::Unary(op, expr) => {
                write!(f, "({} {})", op.to_string(), expr.to_string())
            }
            Self::Variable(name) => {
                write!(f, "({})", name)
            }
            Self::Logical(left, op, right) => {
                write!(
                    f,
                    "({} {} {})",
                    op.to_string(),
                    left.to_string(),
                    right.to_string()
                )
            }
        }
    }
}
