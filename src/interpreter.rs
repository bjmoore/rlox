use crate::environment::Environment;
use crate::error::RuntimeError;
use crate::expression::Expr;
use crate::statement::Stmt;
use crate::token;
use crate::value::LoxValue;

pub struct LoxInterpreter {}

impl LoxInterpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn run(&self, program: &Vec<Stmt>) {
        for statement in program {
            self.execute(statement);
        }
    }

    pub fn evaluate(&self, expr: &Expr) -> Result<LoxValue, RuntimeError> {
        match expr {
            Expr::Literal(lit) => Ok(lit.clone()),
            Expr::Variable(name) => todo!(), // this also needs access to the enclosing environment scope..
            Expr::Unary(operator, expr) => {
                let right = self.evaluate(expr)?;
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
                let left = self.evaluate(left)?;
                let right = self.evaluate(right)?;
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
            Expr::Grouping(expr) => self.evaluate(expr),
        }
    }

    pub fn execute(&self, stmt: &Stmt) -> Result<(), RuntimeError> {
        match &stmt {
            Stmt::ExprStmt(expr) => {
                self.evaluate(expr)?;
                Ok(())
            }
            Stmt::PrintStmt(value) => {
                println!("{}", self.evaluate(value)?);
                Ok(())
            }
            Stmt::VarStmt(name_token, initializer) => {
                // TODO: Obviously temporary, these vars arent getting piped out to anywhere
                // .execute() needs access to some shared mutable state (duh), which is ok (?) because
                // we don't need to recursively call execute (??)
                let mut environment = Environment::new();

                // TODO: should varstmts just have a string in them?
                let name = match &name_token.token_type {
                    token::TokenType::Identifier(name) => Ok(name.clone()),
                    _ => Err(RuntimeError::new(
                        "var stmt without identifier",
                        name_token.line,
                    )),
                }?;
                let initial_value = match initializer {
                    Some(expr) => self.evaluate(expr)?,
                    None => LoxValue::Nil,
                };
                environment.values.insert(name, initial_value);
                Ok(())
            }
        }
    }
}
