use crate::expression::Expr;
use crate::token::Token;

#[derive(Debug)]
pub enum Stmt<'a> {
    ExprStmt(Expr<'a>),
    PrintStmt(Expr<'a>),
    VarStmt(String, Option<Expr<'a>>),
}
