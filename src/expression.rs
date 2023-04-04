use token::Token;

use crate::error::RuntimeError;
use crate::token;
use crate::value::LoxValue;

// TODO: implement Display
#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Binary(Box<Expr<'a>>, &'a Token, Box<Expr<'a>>),
    Grouping(Box<Expr<'a>>),
    Literal(LoxValue),
    Unary(&'a Token, Box<Expr<'a>>),
    Variable(String), // Identifier names are host-language strings, not LoxValue strings
}

impl Expr<'_> {
    pub fn evaluate(&self) -> Result<LoxValue, RuntimeError> {
        match self {
            Expr::Literal(lit) => Ok(lit.clone()),
            Expr::Variable(name) => todo!(),
            Expr::Unary(operator, expr) => {
                let right = expr.evaluate()?;
                match operator.token_type {
                    token::TokenType::Minus => match right {
                        LoxValue::Number(value) => Ok(LoxValue::Number(-value)),
                        _ => Err(RuntimeError::new(
                            "Invalid operand for unary minus",
                            operator.line,
                        )),
                    },
                    token::TokenType::Bang => match right {
                        LoxValue::Bool(value) => Ok(LoxValue::Bool(!value)),
                        _ => Err(RuntimeError::new(
                            "Invalid operand for unary bang",
                            operator.line,
                        )),
                    },
                    _ => Err(RuntimeError::new(
                        "Invalid operand for unary operator",
                        operator.line,
                    )),
                }
            }
            Expr::Binary(left, operator, right) => {
                let left = left.evaluate()?;
                let right = right.evaluate()?;
                match operator.token_type {
                    token::TokenType::Minus => match (left, right) {
                        (LoxValue::Number(left), LoxValue::Number(right)) => {
                            Ok(LoxValue::Number(left - right))
                        }
                        _ => Err(RuntimeError::new(
                            "Invalid operands for binary minus",
                            operator.line,
                        )),
                    },
                    token::TokenType::Plus => match (left, right) {
                        (LoxValue::Number(left), LoxValue::Number(right)) => {
                            Ok(LoxValue::Number(left + right))
                        }
                        (LoxValue::String(left), LoxValue::String(right)) => {
                            Ok(LoxValue::String(format!("{}{}", left, right)))
                        }
                        _ => Err(RuntimeError::new(
                            "Invalid operands for binary plus",
                            operator.line,
                        )),
                    },
                    token::TokenType::Slash => match (left, right) {
                        (LoxValue::Number(left), LoxValue::Number(right)) => {
                            Ok(LoxValue::Number(left / right))
                        }
                        _ => Err(RuntimeError::new(
                            "Invalid operands for binary slash",
                            operator.line,
                        )),
                    },
                    token::TokenType::Star => match (left, right) {
                        (LoxValue::Number(left), LoxValue::Number(right)) => {
                            Ok(LoxValue::Number(left * right))
                        }
                        _ => Err(RuntimeError::new(
                            "Invalid operands for binary star",
                            operator.line,
                        )),
                    },
                    token::TokenType::Greater => match (left, right) {
                        (LoxValue::Number(left), LoxValue::Number(right)) => {
                            Ok(LoxValue::Bool(left > right))
                        }
                        _ => Err(RuntimeError::new(
                            "Invalid operands for binary greater",
                            operator.line,
                        )),
                    },
                    token::TokenType::GreaterEqual => match (left, right) {
                        (LoxValue::Number(left), LoxValue::Number(right)) => {
                            Ok(LoxValue::Bool(left >= right))
                        }
                        _ => Err(RuntimeError::new(
                            "Invalid operands for binary greater equal",
                            operator.line,
                        )),
                    },
                    token::TokenType::Less => match (left, right) {
                        (LoxValue::Number(left), LoxValue::Number(right)) => {
                            Ok(LoxValue::Bool(left < right))
                        }
                        _ => Err(RuntimeError::new(
                            "Invalid operands for binary less",
                            operator.line,
                        )),
                    },
                    token::TokenType::LessEqual => match (left, right) {
                        (LoxValue::Number(left), LoxValue::Number(right)) => {
                            Ok(LoxValue::Bool(left <= right))
                        }
                        _ => Err(RuntimeError::new(
                            "Invalid operands for binary less equal",
                            operator.line,
                        )),
                    },
                    _ => Err(RuntimeError::new(
                        "Invalid operator for binary expression",
                        operator.line,
                    )),
                }
            }
            Expr::Grouping(expr) => expr.evaluate(),
        }
    }
}
