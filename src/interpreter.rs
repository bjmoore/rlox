use crate::environment::Environment;
use crate::error::RuntimeError;
use crate::expression::Expr;
use crate::statement::Stmt;
use crate::token;
use crate::value::LoxValue;

pub struct LoxInterpreter {
    environment: Environment,
}

impl LoxInterpreter {
    pub fn new() -> Self {
        Self {
            environment: Environment::new(),
        }
    }

    pub fn run(&mut self, program: &Vec<Stmt>) {
        for statement in program {
            match self.execute(statement) {
                Ok(_) => (),
                Err(e) => println!("{:?}", e),
            }
        }
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Result<LoxValue, RuntimeError> {
        match expr {
            Expr::Literal(lit) => Ok(lit.clone()),
            Expr::Variable(name) => Ok(self.environment.get(name).clone()),
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
            Expr::Assign(name, expr) => {
                let value = self.evaluate(expr)?;
                self.environment.assign(name.clone(), value)
            }
            Expr::Logical(left, op, right) => match op.token_type {
                token::TokenType::And => {
                    let leftval = self.evaluate(left)?.truthy();
                    if !leftval.to_bool() {
                        Ok(leftval)
                    } else {
                        Ok(self.evaluate(right)?.truthy())
                    }
                }
                token::TokenType::Or => {
                    let leftval = self.evaluate(left)?.truthy();
                    if leftval.to_bool() {
                        Ok(leftval)
                    } else {
                        Ok(self.evaluate(right)?.truthy())
                    }
                }
                _ => Err(RuntimeError::new(
                    "Somehow parsed a logical expression with non-logical operator",
                    op.line,
                )),
            },
        }
    }

    pub fn execute(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        match &stmt {
            Stmt::ExprStmt(expr) => {
                self.evaluate(expr)?;
                Ok(())
            }
            Stmt::PrintStmt(value) => {
                println!("{}", self.evaluate(value)?);
                Ok(())
            }
            Stmt::VarStmt(name, initializer) => {
                let initial_value = match initializer {
                    Some(expr) => self.evaluate(expr)?,
                    None => LoxValue::Nil,
                };
                self.environment.put(name.clone(), initial_value);
                Ok(())
            }
            Stmt::Block(block) => {
                self.environment.push_env();
                for statement in block {
                    self.execute(statement)?;
                }
                self.environment.pop_env();
                Ok(())
            }
            Stmt::IfStmt(condition, then_stmt, else_branch) => {
                if self.evaluate(condition)?.to_bool() {
                    self.execute(then_stmt)?;
                } else if let Some(else_stmt) = else_branch {
                    self.execute(else_stmt)?;
                }
                Ok(())
            }
        }
    }
}
