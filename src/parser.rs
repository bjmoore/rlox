use crate::error::ParseError;
use crate::expression::Expr;
use crate::statement::Stmt;
use crate::token::Token;
use crate::token::TokenType;
use crate::value::LoxValue;

pub struct Parser {
    tokens: Vec<Token>,
}

type IndexedExpr<'a> = (Expr<'a>, usize);
type IndexedStmt<'a> = (Stmt<'a>, usize);

// TODO: Rationalize the index manipulation in this whole file
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens }
    }

    // TODO: This return value is probably wrong - depends on how we handle parsing errors?
    pub fn parse(&self) -> Result<Vec<Stmt>, ParseError> {
        let mut index: usize = 0;
        let mut program: Vec<Stmt> = Vec::new();
        while let Some(_) = self.tokens.get(index) {
            let (stmt, new_index) = self.declaration(index)?;
            program.push(stmt);
            index = new_index;
        }
        Ok(program)
    }

    fn declaration(&self, index: usize) -> Result<IndexedStmt, ParseError> {
        if let Some(tok) = self.tokens.get(index) {
            match tok.token_type {
                TokenType::Var => self.var_declaration(index + 1),
                _ => self.statement(index),
            }
        } else {
            Err(ParseError::UnexpectedEof)
        }
    }

    fn var_declaration(&self, index: usize) -> Result<IndexedStmt, ParseError> {
        let (tok, index) = self.next(index)?;
        // expect an identifier or fail
        if !matches!(&tok.token_type, TokenType::Identifier(_)) {
            return Err(ParseError::ExpectedIdentifier(tok.line));
        }

        // if the token after that is = then populate an initializer
        let (tok, mut index) = self.next(index)?;
        let initializer = match tok.token_type {
            TokenType::Equal => {
                let (expr, new_index) = self.expression(index)?;
                index = new_index;
                Some(expr)
            }
            _ => None,
        };

        // finally our statement should end with a semicolon
        println!("{:?}", index);
        let (tok, index) = self.next(index)?;
        if !matches!(&tok.token_type, TokenType::Semicolon) {
            return Err(ParseError::ExpectedSemicolon(tok.line));
        }

        // return (ident_tok, initializer)
        Ok((Stmt::VarStmt(tok, initializer), index))
    }

    fn statement(&self, index: usize) -> Result<IndexedStmt, ParseError> {
        if let Some(tok) = self.tokens.get(index) {
            match tok.token_type {
                TokenType::Print => self.print_statement(index + 1),
                _ => self.expression_statement(index),
            }
        } else {
            Err(ParseError::UnexpectedEof)
        }
    }

    fn print_statement(&self, index: usize) -> Result<IndexedStmt, ParseError> {
        let (value, new_index) = self.expression(index)?;
        if let Some(tok) = self.tokens.get(new_index) {
            match tok.token_type {
                TokenType::Semicolon => Ok((Stmt::PrintStmt(value), new_index + 1)),
                _ => Err(ParseError::ExpectedSemicolon(tok.line)),
            }
        } else {
            Err(ParseError::UnexpectedEof)
        }
    }

    fn expression_statement(&self, index: usize) -> Result<IndexedStmt, ParseError> {
        let (value, new_index) = self.expression(index)?;
        if let Some(tok) = self.tokens.get(new_index) {
            match tok.token_type {
                TokenType::Semicolon => Ok((Stmt::ExprStmt(value), new_index + 1)),
                _ => Err(ParseError::ExpectedSemicolon(tok.line)),
            }
        } else {
            Err(ParseError::UnexpectedEof)
        }
    }

    fn expression(&self, index: usize) -> Result<IndexedExpr, ParseError> {
        self.equality(index)
    }

    fn equality(&self, index: usize) -> Result<IndexedExpr, ParseError> {
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

    fn comparison(&self, index: usize) -> Result<IndexedExpr, ParseError> {
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

    fn term(&self, index: usize) -> Result<IndexedExpr, ParseError> {
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

    fn factor(&self, index: usize) -> Result<IndexedExpr, ParseError> {
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

    fn unary(&self, index: usize) -> Result<IndexedExpr, ParseError> {
        let (tok, index) = self.next(index)?;
        match tok.token_type {
            TokenType::Bang | TokenType::Minus => {
                let (expr, index) = self.unary(index)?;
                Ok((Expr::Unary(tok, Box::new(expr)), index))
            }
            _ => self.primary(tok, index),
        }
    }

    fn primary(&self, tok: &Token, index: usize) -> Result<IndexedExpr, ParseError> {
        match &tok.token_type {
            TokenType::False => Ok((Expr::Literal(LoxValue::Bool(false)), index)),
            TokenType::True => Ok((Expr::Literal(LoxValue::Bool(true)), index)),
            TokenType::Nil => Ok((Expr::Literal(LoxValue::Nil), index)),
            TokenType::Number(n) => Ok((Expr::Literal(LoxValue::Number(*n)), index)),
            TokenType::String(s) => Ok((Expr::Literal(LoxValue::String(s.clone())), index)),
            TokenType::LeftParen => {
                let (expr, index) = self.expression(index)?;
                let (tok, index) = self.next(index)?;
                match tok.token_type {
                    TokenType::RightParen => Ok((Expr::Grouping(Box::new(expr)), index)),
                    _ => Err(ParseError::UnbalancedParenthesis(tok.line)),
                }
            }
            _ => Err(ParseError::ExpectedExpression(tok.line)),
        }
    }

    fn next(&self, index: usize) -> Result<(&Token, usize), ParseError> {
        match self.tokens.get(index) {
            Some(tok) => Ok((tok, index + 1)),
            None => Err(ParseError::UnexpectedEof),
        }
    }
}
