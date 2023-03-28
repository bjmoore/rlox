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
        let (mut expr, mut index) = self.term(index);
        if expr.is_err() {
            return (expr, index);
        }
        while let Some(tok) = self.tokens.get(index) {
            match tok.token_type {
                TokenType::Greater
                | TokenType::GreaterEqual
                | TokenType::Less
                | TokenType::LessEqual => {
                    let (right, new_index) = self.term(index + 1);
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

    fn term(&self, index: usize) -> (Result<Expr, ParseError>, usize) {
        let (mut expr, mut index) = self.factor(index);
        if expr.is_err() {
            return (expr, index);
        }
        while let Some(tok) = self.tokens.get(index) {
            match tok.token_type {
                TokenType::Minus | TokenType::Plus => {
                    let (right, new_index) = self.factor(index + 1);
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

    fn factor(&self, index: usize) -> (Result<Expr, ParseError>, usize) {
        let (mut expr, mut index) = self.unary(index);
        if expr.is_err() {
            return (expr, index);
        }
        while let Some(tok) = self.tokens.get(index) {
            match tok.token_type {
                TokenType::Slash | TokenType::Star => {
                    let (right, new_index) = self.unary(index + 1);
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

    fn unary(&self, index: usize) -> (Result<Expr, ParseError>, usize) {
        if let Some(tok) = self.tokens.get(index) {
            match tok.token_type {
                TokenType::Bang | TokenType::Minus => {
                    let (expr, new_index) = self.unary(index + 1);
                    if expr.is_err() {
                        return (expr, new_index);
                    }
                    return (Ok(Expr::Unary(tok, Box::new(expr.unwrap()))), new_index);
                }
                _ => (),
            }
        }
        self.primary(index)
    }

    fn primary(&self, index: usize) -> (Result<Expr, ParseError>, usize) {
        if let Some(tok) = self.tokens.get(index) {
            match &tok.token_type {
                TokenType::False => return (Ok(Expr::Literal(Value::Bool(false))), index + 1),
                TokenType::True => return (Ok(Expr::Literal(Value::Bool(true))), index + 1),
                TokenType::Nil => return (Ok(Expr::Literal(Value::Nil)), index + 1),
                TokenType::Number(n) => return (Ok(Expr::Literal(Value::Number(*n))), index + 1),
                TokenType::String(s) => {
                    return (Ok(Expr::Literal(Value::String(s.clone()))), index + 1)
                }
                TokenType::LeftParen => {
                    let (expr, new_index) = self.expression(index + 1);
                    if expr.is_err() {
                        return (expr, new_index);
                    }
                    if let Some(tok) = self.tokens.get(new_index) {
                        if tok.token_type == TokenType::RightParen {
                            return (expr, new_index + 1);
                        }
                    }
                    return (Err(ParseError::UnbalancedParenthesis(0)), new_index);
                }
                _ => (),
            }
        }
        (Err(ParseError::ExpectedExpression(0)), index)
    }
}
