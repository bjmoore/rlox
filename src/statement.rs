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
