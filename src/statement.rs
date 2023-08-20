use std::fmt;

use crate::expression::Expr;

#[derive(Debug, PartialEq)]
pub enum Stmt<'a> {
    ExprStmt(Expr<'a>),
    PrintStmt(Expr<'a>),
    VarStmt(String, Option<Expr<'a>>),
    IfStmt(Expr<'a>, Box<Stmt<'a>>, Option<Box<Stmt<'a>>>),
    While(Expr<'a>, Box<Stmt<'a>>),
    Block(Vec<Stmt<'a>>),
}

impl<'a> fmt::Display for Stmt<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ExprStmt(expr) => {
                writeln!(f, "{};", expr.to_string())
            }
            Self::PrintStmt(expr) => {
                writeln!(f, "(print {});", expr.to_string())
            }
            Self::VarStmt(name, initializer) => match initializer {
                Some(value) => {
                    writeln!(f, "(def {} {});", name.to_string(), value.to_string())
                }
                None => {
                    writeln!(f, "(def {});", name.to_string())
                }
            },
            Self::IfStmt(condition, then_stmt, else_stmt) => {
                writeln!(f, "if ({})", condition.to_string())?;
                writeln!(f, "then {};", then_stmt.to_string())?;
                if let Some(stmt) = else_stmt {
                    writeln!(f, "else ({})", stmt.to_string())?;
                }
                Ok(())
            }
            Self::While(condition, body) => {
                writeln!(
                    f,
                    "while ({}) do {};",
                    condition.to_string(),
                    body.to_string()
                )
            }
            Self::Block(body) => {
                writeln!(f, "{{")?;
                for statement in body {
                    write!(f, "{}", statement.to_string())?;
                }
                write!(f, "}}")?;
                Ok(())
            }
        }
    }
}
