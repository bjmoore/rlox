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

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens }
    }

    pub fn parse(&self) -> Vec<Result<Stmt, ParseError>> {
        let mut index: usize = 0;
        let mut program = Vec::new();
        while let Some(_) = self.tokens.get(index) {
            match self.declaration(index) {
                Ok((stmt, new_index)) => {
                    program.push(Ok(stmt));
                    index = new_index;
                }
                Err(e) => {
                    program.push(Err(e));
                    index = self.synchronize(index);
                }
            }
        }
        program
    }

    fn synchronize(&self, mut index: usize) -> usize {
        while let Some(tok) = self.tokens.get(index) {
            match tok.token_type {
                TokenType::Semicolon => {
                    index += 1;
                    break;
                }
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => break,
                _ => (),
            }

            index += 1;
        }

        index
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
        let name = match &tok.token_type {
            TokenType::Identifier(name) => Ok(name),
            _ => Err(ParseError::ExpectedIdentifier(tok.line)),
        }?;

        // if the token after that is = then populate an initializer
        // TODO bug: if there *isnt* an = this crashes on unexpected eof. peek()? next_if?
        let (initializer, index) =
            match self.next_if(index, |t| matches!(t.token_type, TokenType::Equal)) {
                (Some(_), index) => {
                    let (initializer, index) = self.expression(index)?;
                    (Some(initializer), index)
                }
                (None, new_index) => (None, new_index),
            };

        // finally our statement should end with a semicolon
        let (tok, index) = self.next(index)?;
        if !matches!(&tok.token_type, TokenType::Semicolon) {
            return Err(ParseError::ExpectedSemicolon(tok.line));
        }

        // return (ident_tok, initializer)
        Ok((Stmt::VarStmt(name.clone(), initializer), index))
    }

    fn statement(&self, index: usize) -> Result<IndexedStmt, ParseError> {
        println!("stmt input: {}", index);
        let (tok, index) = self.next(index)?;
        match tok.token_type {
            TokenType::If => self.if_statement(index),
            TokenType::Print => self.print_statement(index),
            TokenType::While => self.while_statement(index),
            TokenType::LeftBrace => {
                let (block, index) = self.block(index)?;
                Ok((Stmt::Block(block), index))
            }
            _ => self.expression_statement(index - 1),
        }
    }

    fn if_statement(&self, index: usize) -> Result<IndexedStmt, ParseError> {
        // expect a ( after if
        let (tok, index) = self.next(index)?;
        match tok.token_type {
            TokenType::LeftParen => Ok(()),
            _ => Err(ParseError::ExpectedExpression(
                tok.clone(),
                tok.line,
                "if stmt (",
            )),
        }?;
        // consume expr for condition
        let (condition, index) = self.expression(index)?;
        // expect a ) after condition
        let (tok, index) = self.next(index)?;
        match tok.token_type {
            TokenType::RightParen => Ok(()),
            _ => Err(ParseError::ExpectedExpression(
                tok.clone(),
                tok.line,
                "if stmt )",
            )),
        }?;

        // consume a then statement
        let (then_branch, index) = self.statement(index)?;
        // if the next thing is an else, consume an else statement
        let (else_branch, index) =
            match self.next_if(index, |t| matches!(t.token_type, TokenType::Else)) {
                (Some(_), index) => {
                    let (else_branch, index) = self.statement(index)?;
                    (Some(Box::new(else_branch)), index)
                }
                (None, index) => (None, index),
            };

        // emit an IfStmt(cond, thenstmt, elsestmt)
        Ok((
            Stmt::IfStmt(condition, Box::new(then_branch), else_branch),
            index,
        ))
    }

    fn while_statement(&self, index: usize) -> Result<IndexedStmt, ParseError> {
        let (tok, index) = self.next(index)?;
        match tok.token_type {
            TokenType::LeftParen => Ok(()),
            _ => Err(ParseError::TodoError(tok.line)),
        }?;

        let (condition, index) = self.expression(index)?;

        let (tok, index) = self.next(index)?;
        match tok.token_type {
            TokenType::RightParen => Ok(()),
            _ => Err(ParseError::TodoError(tok.line)),
        }?;

        let (body, index) = self.statement(index)?;

        Ok((Stmt::While(condition, Box::new(body)), index))
    }

    fn for_statement(&self, index: usize) -> Result<IndexedStmt, ParseError> {
        Err(ParseError::TodoError(0))
    }

    fn block(&self, mut index: usize) -> Result<(Vec<Stmt>, usize), ParseError> {
        let mut statements: Vec<Stmt> = Vec::new();

        // while the next token isnt a RightBrace
        // try to eat a statement and add to statements
        loop {
            let (tok, new_index) = self.next(index)?;
            match tok.token_type {
                TokenType::RightBrace => {
                    index = new_index;
                    break;
                }
                _ => {
                    let (statement, new_index) = self.declaration(index)?;
                    statements.push(statement);
                    index = new_index;
                }
            }
        }

        Ok((statements, index))
    }

    fn print_statement(&self, index: usize) -> Result<IndexedStmt, ParseError> {
        let (value, index) = self.expression(index)?;
        if let Some(tok) = self.tokens.get(index) {
            match tok.token_type {
                TokenType::Semicolon => Ok((Stmt::PrintStmt(value), index + 1)),
                _ => Err(ParseError::ExpectedSemicolon(tok.line)),
            }
        } else {
            Err(ParseError::UnexpectedEof)
        }
    }

    fn expression_statement(&self, index: usize) -> Result<IndexedStmt, ParseError> {
        println!("exprstmt input: {}", index);
        let (value, index) = self.expression(index)?;
        if let Some(tok) = self.tokens.get(index) {
            match tok.token_type {
                TokenType::Semicolon => Ok((Stmt::ExprStmt(value), index + 1)),
                _ => Err(ParseError::ExpectedSemicolon(tok.line)),
            }
        } else {
            Err(ParseError::UnexpectedEof)
        }
    }

    fn expression(&self, index: usize) -> Result<IndexedExpr, ParseError> {
        self.assignment(index)
    }

    fn assignment(&self, index: usize) -> Result<IndexedExpr, ParseError> {
        println!("input: {}", index);
        let (expr, index) = self.or(index)?;

        // if we have an equal
        if let (Some(tok), index) =
            self.next_if(index, |t| matches!(t.token_type, TokenType::Equal))
        {
            match expr {
                Expr::Variable(name) => {
                    println!("if of assignment");
                    println!("{:?}", self.tokens.get(index));
                    let (value, index) = self.assignment(index)?;
                    Ok((Expr::Assign(name.clone(), Box::new(value)), index))
                }
                _ => Err(ParseError::InvalidAssignment(tok.line)),
            }
        } else {
            //  return expr normally
            println!("return: {}", index);
            Ok((expr, index))
        }
    }

    fn or(&self, index: usize) -> Result<IndexedExpr, ParseError> {
        let (mut expr, mut index) = self.and(index)?;
        while let Some(tok) = self.tokens.get(index) {
            match tok.token_type {
                TokenType::Or => {
                    let (right, new_index) = self.and(index + 1)?;
                    expr = Expr::Logical(Box::new(expr), tok, Box::new(right));
                    index = new_index;
                }
                _ => break,
            }
        }
        Ok((expr, index))
    }

    fn and(&self, index: usize) -> Result<IndexedExpr, ParseError> {
        let (mut expr, mut index) = self.equality(index)?;
        while let Some(tok) = self.tokens.get(index) {
            match tok.token_type {
                TokenType::And => {
                    let (right, new_index) = self.equality(index + 1)?;
                    expr = Expr::Logical(Box::new(expr), tok, Box::new(right));
                    index = new_index;
                }
                _ => break,
            }
        }
        Ok((expr, index))
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
            TokenType::Identifier(name) => Ok((Expr::Variable(name.clone()), index)),
            _ => Err(ParseError::ExpectedExpression(
                tok.clone(),
                tok.line,
                "primary",
            )),
        }
    }

    fn next(&self, index: usize) -> Result<(&Token, usize), ParseError> {
        match self.tokens.get(index) {
            Some(tok) => Ok((tok, index + 1)),
            None => Err(ParseError::UnexpectedEof),
        }
    }

    fn next_if(&self, index: usize, predicate: fn(&Token) -> bool) -> (Option<&Token>, usize) {
        match self.tokens.get(index) {
            Some(tok) if predicate(tok) => (Some(tok), index + 1),
            _ => (None, index),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_literal_bool() {
        let number1 = Token {
            token_type: TokenType::Number(1.0),
            line: 0,
        };
        let plus = Token {
            token_type: TokenType::Plus,
            line: 0,
        };
        let number2 = Token {
            token_type: TokenType::Number(2.0),
            line: 0,
        };
        let semicolon = Token {
            token_type: TokenType::Semicolon,
            line: 0,
        };
        let test_tok_stream = vec![number1, plus.clone(), number2, semicolon];
        let test_parser = Parser::new(test_tok_stream);
        assert_eq!(
            *test_parser.parse()[0].as_ref().unwrap(),
            Stmt::ExprStmt(Expr::Binary(
                Box::new(Expr::Literal(LoxValue::Number(1.0))),
                &plus,
                Box::new(Expr::Literal(LoxValue::Number(2.0)))
            ))
        );
    }
}
