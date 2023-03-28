use crate::expression::Expr;
use crate::expression::Value;
use crate::token::ParseError;
use crate::token::Token;
use crate::token::TokenType;

pub struct Parser {
    tokens: Vec<Token>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens }
    }

    pub fn parse(&self) -> Result<Expr, ParseError> {
        self.expression(0).0
    }

    fn expression(&self, index: usize) -> (Result<Expr, ParseError>, usize) {
        self.equality(index)
    }

    fn equality(&self, index: usize) -> (Result<Expr, ParseError>, usize) {
        let (mut expr, mut index) = self.comparison(index);
        if expr.is_err() {
            return (expr, index);
        }
        while let Some(tok) = self.tokens.get(index) {
            match tok.token_type {
                TokenType::BangEqual | TokenType::EqualEqual => {
                    let (right, new_index) = self.comparison(index + 1);
                    if right.is_err() {
                        return (right, new_index);
                    }
                    expr = Ok(Expr::Binary(
                        Box::new(expr.unwrap()),
                        tok,
                        Box::new(right.unwrap()),
                    ));
                    index = new_index;
                }
                _ => break,
            }
        }
        (expr, index)
    }

    fn comparison(&self, index: usize) -> (Result<Expr, ParseError>, usize) {
        // return an empty value
        (Ok(Expr::Literal(Value::Nil)), index)
    }
}
