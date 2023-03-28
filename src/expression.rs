use token::Token;
use token::TokenType;

use crate::token;

pub trait AstPrintable {
    fn print(&self) -> String;
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Binary(Box<Expr<'a>>, &'a Token, Box<Expr<'a>>),
    Grouping(Box<Expr<'a>>),
    Literal(Value),
    Unary(&'a Token, Box<Expr<'a>>),
}

#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
}

impl<'a> AstPrintable for Expr<'a> {
    fn print(&self) -> String {
        match self {
            Expr::Literal(lit) => match lit {
                Value::String(value) => {
                    format!("{}", value)
                }
                Value::Bool(bool) => {
                    format!("{}", bool)
                }
                Value::Nil => {
                    format!("nil")
                }
                Value::Number(value) => format!("{}", value),
            },
            Expr::Unary(operator, expr) => {
                format!("({} {})", operator, expr.print())
            }
            Expr::Binary(left, operator, right) => {
                format!("({} {} {})", operator, left.print(), right.print())
            }
            Expr::Grouping(expr) => {
                format!("(group {})", expr.print())
            }
        }
    }
}

pub fn test_ast_printer() {
    let bang_tok = Token::new(TokenType::Bang, 0);
    let star_tok = Token::new(TokenType::Star, 0);
    let expr = Expr::Binary(
        Box::new(Expr::Unary(
            &bang_tok,
            Box::new(Expr::Literal(Value::Number(123f64))),
        )),
        &star_tok,
        Box::new(Expr::Grouping(Box::new(Expr::Literal(Value::Number(
            45.67,
        ))))),
    );
    print!("{}", expr.print());
}
