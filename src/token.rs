use std::error::Error;
use std::fmt;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier(String),
    String(String),
    Number(f64),

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line: u32,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.token_type {
            TokenType::And => write!(f, "and"),
            TokenType::Bang => write!(f, "!"),
            _ => write!(f, "{:?}", self.token_type),
        }
    }
}

pub struct TokenStream<'a> {
    raw_input: Peekable<Chars<'a>>,
    line: u32,
}

impl<'a> TokenStream<'a> {
    pub fn new(input: &'a str) -> Self {
        TokenStream {
            raw_input: input.chars().peekable(),
            line: 0,
        }
    }

    fn scan(&mut self) -> Option<Result<Token, ParseError>> {
        let mut tok = None;
        while tok == None {
            if let Some(c) = self.raw_input.next() {
                tok = match c {
                    '(' => self.add_token(TokenType::LeftParen),
                    ')' => self.add_token(TokenType::RightParen),
                    '{' => self.add_token(TokenType::LeftBrace),
                    '}' => self.add_token(TokenType::RightBrace),
                    ',' => self.add_token(TokenType::Comma),
                    '.' => self.add_token(TokenType::Dot),
                    '-' => self.add_token(TokenType::Minus),
                    '+' => self.add_token(TokenType::Plus),
                    ';' => self.add_token(TokenType::Semicolon),
                    '*' => self.add_token(TokenType::Star),
                    '!' => match self.raw_input.next_if_eq(&'=') {
                        Some(_) => self.add_token(TokenType::BangEqual),
                        None => self.add_token(TokenType::Bang),
                    },
                    '=' => match self.raw_input.next_if_eq(&'=') {
                        Some(_) => self.add_token(TokenType::EqualEqual),
                        None => self.add_token(TokenType::Equal),
                    },
                    '<' => match self.raw_input.next_if_eq(&'=') {
                        Some(_) => self.add_token(TokenType::LessEqual),
                        None => self.add_token(TokenType::Less),
                    },
                    '>' => match self.raw_input.next_if_eq(&'=') {
                        Some(_) => self.add_token(TokenType::GreaterEqual),
                        None => self.add_token(TokenType::Greater),
                    },
                    '/' => match self.raw_input.next_if_eq(&'/') {
                        Some(_) => {
                            self.consume_until('\n');
                            self.line += 1;
                            None
                        }
                        None => self.add_token(TokenType::Slash),
                    },
                    ' ' | '\r' | '\t' => None,
                    '\n' => {
                        self.line += 1;
                        None
                    }
                    '"' => self.string(),
                    _ if c.is_ascii_digit() => self.number(c),
                    _ if c.is_ascii_alphabetic() => self.identifier(c),
                    _ => Some(Err(ParseError::InvalidCharacter(c, self.line))),
                };
            } else {
                break;
            }
        }
        tok
    }

    fn add_token(&self, token_type: TokenType) -> Option<Result<Token, ParseError>> {
        Some(Ok(Token {
            token_type,
            line: self.line,
        }))
    }

    fn consume_until(&mut self, c: char) {
        self.raw_input.find(|&x| x == c);
    }

    fn string(&mut self) -> Option<Result<Token, ParseError>> {
        // consume characters from the input until we find a closing quote
        let mut string_value = String::new();
        loop {
            match self.raw_input.next() {
                Some('"') => break,
                Some('\n') => {
                    self.line += 1;
                    string_value.push('\n');
                }
                Some(c) => string_value.push(c),
                None => {
                    return Some(Err(ParseError::UnterminatedStringError(
                        string_value,
                        self.line,
                    )))
                }
            }
        }
        self.add_token(TokenType::String(string_value))
    }

    fn number(&mut self, c: char) -> Option<Result<Token, ParseError>> {
        let mut numeric_string = String::from(c);
        while let Some(c) = self.raw_input.next_if(|&c| c.is_ascii_digit()) {
            numeric_string.push(c);
        }

        // TODO: Add fractional parts

        match numeric_string.parse::<f64>() {
            Ok(n) => self.add_token(TokenType::Number(n)),
            Err(_) => Some(Err(ParseError::ParseFloatError(numeric_string, self.line))),
        }
    }

    fn identifier(&mut self, c: char) -> Option<Result<Token, ParseError>> {
        let mut identifier_string = String::from(c);
        while let Some(c) = self
            .raw_input
            .next_if(|&c| c.is_ascii_alphabetic() || c == '_')
        {
            identifier_string.push(c);
        }

        match identifier_string.as_str() {
            "and" => self.add_token(TokenType::And),
            "class" => self.add_token(TokenType::Class),
            "else" => self.add_token(TokenType::Else),
            "false" => self.add_token(TokenType::False),
            "for" => self.add_token(TokenType::For),
            "fun" => self.add_token(TokenType::Fun),
            "if" => self.add_token(TokenType::If),
            "nil" => self.add_token(TokenType::Nil),
            "or" => self.add_token(TokenType::Or),
            "print" => self.add_token(TokenType::Print),
            "return" => self.add_token(TokenType::Return),
            "super" => self.add_token(TokenType::Super),
            "this" => self.add_token(TokenType::This),
            "true" => self.add_token(TokenType::True),
            "var" => self.add_token(TokenType::Var),
            "while" => self.add_token(TokenType::While),
            _ => self.add_token(TokenType::Identifier(identifier_string)),
        }
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Result<Token, ParseError>;
    fn next(&mut self) -> Option<Self::Item> {
        self.scan()
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
    InvalidCharacter(char, u32),
    ParseFloatError(String, u32),
    UnterminatedStringError(String, u32),
    UnbalancedParenthesis(u32),
    ExpectedExpression(u32),
    UnexpectedEof,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::InvalidCharacter(char, line) => {
                write!(f, "Invalid character: {} on line {}", char, line)
            }
            Self::ParseFloatError(literal, line) => {
                write!(
                    f,
                    "Unable to parse number literal {} on line {}",
                    literal, line
                )
            }
            Self::UnterminatedStringError(literal, line) => {
                write!(f, "Unterminated string {} on line {}", literal, line)
            }
            Self::UnbalancedParenthesis(line) => {
                write!(f, "Mismatched parenthesis on line {}", line)
            }
            Self::ExpectedExpression(line) => {
                write!(f, "Expected expression on line {}", line)
            }
            Self::UnexpectedEof => write!(f, "Unexpected end of file"),
        }
    }
}

impl Error for ParseError {}
