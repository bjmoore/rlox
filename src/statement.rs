use crate::error::RuntimeError;
use crate::expression::Expr;

#[derive(Debug)]
pub enum Stmt<'a> {
    ExprStmt(Expr<'a>),
    PrintStmt(Expr<'a>),
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
        }
    }
}
