use crate::environment::Environment;
use crate::error::RuntimeError;
use crate::expression::Expr;
use crate::token::Token;
use crate::token::TokenType;
use crate::value::LoxValue;

#[derive(Debug)]
pub enum Stmt<'a> {
    ExprStmt(Expr<'a>),
    PrintStmt(Expr<'a>),
    VarStmt(&'a Token, Option<Expr<'a>>),
}

impl Stmt<'_> {
    pub fn execute(&self) -> Result<(), RuntimeError> {
        match self {
            Self::ExprStmt(expr) => {
                expr.evaluate()?;
                Ok(())
            }
            Self::PrintStmt(value) => {
                println!("{}", value.evaluate()?);
                Ok(())
            }
            Self::VarStmt(name_token, initializer) => {
                // TODO: Obviously temporary, these vars arent getting piped out to anywhere
                // .execute() needs access to some shared mutable state (duh), which is ok (?) because
                // we don't need to recursively call execute (??)
                let mut environment = Environment::new();

                // TODO: should varstmts just have a string in them?
                let name = match &name_token.token_type {
                    TokenType::Identifier(name) => Ok(name.clone()),
                    _ => Err(RuntimeError::new(
                        "var stmt without identifier",
                        name_token.line,
                    )),
                }?;
                let initial_value = match initializer {
                    Some(expr) => expr.evaluate()?,
                    None => LoxValue::Nil,
                };
                environment.values.insert(name, initial_value);
                Ok(())
            }
        }
    }
}
