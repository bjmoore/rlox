use crate::error::RuntimeError;
use crate::expression::Expr;
use crate::token::Token;

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
            Self::VarStmt(name, initializer) => {
                todo!()
            }
        }
    }
}
