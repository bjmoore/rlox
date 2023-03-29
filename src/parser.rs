use crate::expression::Expr;
use crate::expression::Value;
use crate::token::ParseError;
use crate::token::Token;
use crate::token::TokenType;

pub struct Parser {
    tokens: Vec<Token>,
}

type ExprIndex<'a> = (Expr<'a>, usize);

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens }
    }

    pub fn parse(&self) -> Result<ExprIndex, ParseError> {
        self.expression(0)
    }

    fn expression(&self, index: usize) -> Result<ExprIndex, ParseError> {
        self.equality(index)
    }

    fn equality(&self, index: usize) -> Result<ExprIndex, ParseError> {
        let (mut expr, mut index) = self.comparison(index)?;
        while let Some(tok) = self.tokens.get(index) {
            match tok.token_type {
                TokenType::BangEqual | TokenType::EqualEqual => {
                    let (right, new_index) = self.comparison(index + 1)?;
                    expr = Expr::Binary(Box::new(expr), tok, Box::new(right));
                    index = new_index;
                }
                _ => break,
            }
        }
        Ok((expr, index))
    }

    fn comparison(&self, index: usize) -> Result<ExprIndex, ParseError> {
        let (mut expr, mut index) = self.term(index)?;
        while let Some(tok) = self.tokens.get(index) {
            match tok.token_type {
                TokenType::Greater
                | TokenType::GreaterEqual
                | TokenType::Less
                | TokenType::LessEqual => {
                    let (right, new_index) = self.term(index + 1)?;
                    expr = Expr::Binary(Box::new(expr), tok, Box::new(right));
                    index = new_index;
                }
                _ => break,
            }
        }
        Ok((expr, index))
    }

    fn term(&self, index: usize) -> Result<ExprIndex, ParseError> {
        let (mut expr, mut index) = self.factor(index)?;
        while let Some(tok) = self.tokens.get(index) {
            match tok.token_type {
                TokenType::Minus | TokenType::Plus => {
                    let (right, new_index) = self.factor(index + 1)?;
                    expr = Expr::Binary(Box::new(expr), tok, Box::new(right));
                    index = new_index;
                }
                _ => break,
            }
        }
        Ok((expr, index))
    }

    fn factor(&self, index: usize) -> Result<ExprIndex, ParseError> {
        let (mut expr, mut index) = self.unary(index)?;
        while let Some(tok) = self.tokens.get(index) {
            match tok.token_type {
                TokenType::Slash | TokenType::Star => {
                    let (right, new_index) = self.unary(index + 1)?;
                    expr = Expr::Binary(Box::new(expr), tok, Box::new(right));
                    index = new_index;
                }
                _ => break,
            }
        }
        Ok((expr, index))
    }

    fn unary(&self, index: usize) -> Result<ExprIndex, ParseError> {
        let tok = self.get_token(index)?;
        match tok.token_type {
            TokenType::Bang | TokenType::Minus => {
                let (expr, new_index) = self.unary(index + 1)?;
                Ok((Expr::Unary(tok, Box::new(expr)), new_index))
            }
            _ => self.primary(index),
        }
    }

    fn primary(&self, index: usize) -> Result<ExprIndex, ParseError> {
        let tok = self.get_token(index)?;
        match &tok.token_type {
            TokenType::False => Ok((Expr::Literal(Value::Bool(false)), index + 1)),
            TokenType::True => Ok((Expr::Literal(Value::Bool(true)), index + 1)),
            TokenType::Nil => Ok((Expr::Literal(Value::Nil), index + 1)),
            TokenType::Number(n) => Ok((Expr::Literal(Value::Number(*n)), index + 1)),
            TokenType::String(s) => Ok((Expr::Literal(Value::String(s.clone())), index + 1)),
            TokenType::LeftParen => {
                let (expr, new_index) = self.expression(index + 1)?;
                let tok = self.get_token(new_index)?;
                match tok.token_type {
                    TokenType::RightParen => Ok((Expr::Grouping(Box::new(expr)), new_index + 1)),
                    _ => Err(ParseError::UnbalancedParenthesis(tok.line)),
                }
            }
            _ => Err(ParseError::ExpectedExpression(tok.line)),
        }
    }

    fn get_token(&self, index: usize) -> Result<&Token, ParseError> {
        match self.tokens.get(index) {
            Some(tok) => Ok(tok),
            None => Err(ParseError::UnexpectedEof),
        }
    }
}
