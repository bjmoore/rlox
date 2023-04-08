use crate::expression::Expr;

#[derive(Debug)]
pub enum Stmt<'a> {
    ExprStmt(Expr<'a>),
    PrintStmt(Expr<'a>),
    VarStmt(String, Option<Expr<'a>>),
    IfStmt(Expr<'a>, Box<Stmt<'a>>, Option<Box<Stmt<'a>>>),
    Block(Vec<Stmt<'a>>),
}
