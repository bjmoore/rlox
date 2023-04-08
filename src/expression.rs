use token::Token;

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
    Assign(String, Box<Expr<'a>>),
    Logical(Box<Expr<'a>>, &'a Token, Box<Expr<'a>>),
}
