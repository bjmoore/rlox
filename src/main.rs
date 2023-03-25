use std::env;
use std::error::Error;
use std::fmt;
use std::fs;
use std::io;
use std::io::Write;
use std::iter::Peekable;
use std::str::Chars;

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        Err(Box::<dyn Error>::from("Usage: rlox [script]"))
    } else if args.len() == 2 {
        run_file(&args[1])
    } else {
        run_prompt()
    }
}

fn run_file(path: &str) -> Result<(), Box<dyn Error>> {
    let input = String::from_utf8(fs::read(path)?)?;
    run(&input)
}

fn run_prompt() -> Result<(), Box<dyn Error>> {
    loop {
        let mut buf = String::new();
        io::stdout().write_all(b"> ")?;
        io::stdout().flush()?;
        io::stdin().read_line(&mut buf)?;
        run(&buf);
    }
}

fn run(input: &str) -> Result<(), Box<dyn Error>> {
    let (tokens, errs): (Vec<Result<Token, _>>, Vec<Result<_, ParseError>>) =
        TokenStream::new(input).partition(|t| t.is_ok());

    for token in tokens {
        println!("{:?}", token);
    }

    for err in errs {
        println!("{:?}", err);
    }

    Ok(())
}

#[derive(Debug, PartialEq)]
enum TokenType {
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

    Eof,
}

#[derive(Debug, PartialEq)]
struct Token {
    token_type: TokenType,
    line: u32,
}

struct TokenStream<'a> {
    raw_input: Peekable<Chars<'a>>,
    line: u32,
}

impl<'a> TokenStream<'a> {
    fn new(input: &'a str) -> Self {
        TokenStream {
            raw_input: input.chars().peekable(),
            line: 0,
        }
    }

    fn scan(&mut self) -> Option<Result<Token, ParseError>> {
        let mut tok = None;
        while tok == None && self.raw_input.peek().is_some() {
            let c = self.raw_input.next().unwrap();
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
                '!' => {
                    if self.match_next('=') {
                        self.add_token(TokenType::BangEqual)
                    } else {
                        self.add_token(TokenType::Bang)
                    }
                }
                '=' => {
                    if self.match_next('=') {
                        self.add_token(TokenType::EqualEqual)
                    } else {
                        self.add_token(TokenType::Equal)
                    }
                }
                '<' => {
                    if self.match_next('=') {
                        self.add_token(TokenType::LessEqual)
                    } else {
                        self.add_token(TokenType::Less)
                    }
                }
                '>' => {
                    if self.match_next('=') {
                        self.add_token(TokenType::GreaterEqual)
                    } else {
                        self.add_token(TokenType::Greater)
                    }
                }
                '/' => {
                    if self.match_next('/') {
                        self.consume_until('\n');
                        self.line += 1;
                        None
                    } else {
                        self.add_token(TokenType::Slash)
                    }
                }
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
        }
        tok
    }

    fn add_token(&self, token_type: TokenType) -> Option<Result<Token, ParseError>> {
        Some(Ok(Token {
            token_type,
            line: self.line,
        }))
    }

    fn match_next(&mut self, c: char) -> bool {
        match self.raw_input.peek() {
            Some(&next_char) => {
                if c == next_char {
                    self.raw_input.next();
                    true
                } else {
                    false
                }
            }
            None => false,
        }
    }

    fn consume_until(&mut self, c: char) {
        self.raw_input.find(|&x| x == c);
    }

    fn string(&mut self) -> Option<Result<Token, ParseError>> {
        let string_value: String = self.raw_input.by_ref().take_while(|&c| c != '"').collect();
        if string_value.ends_with('"') {
            Some(Ok(Token {
                token_type: TokenType::String(string_value),
                line: self.line,
            }))
        } else {
            Some(Err(ParseError::UnterminatedStringError(
                string_value,
                self.line,
            )))
        }
    }

    fn number(&mut self, c: char) -> Option<Result<Token, ParseError>> {
        let mut numeric_string = String::from(c);
        while let Some(c) = self.raw_input.next_if(|&c| c.is_ascii_digit()) {
            numeric_string.push(c);
        }

        // TODO: Add fractional parts

        match numeric_string.parse::<f64>() {
            Ok(n) => Some(Ok(Token {
                token_type: TokenType::Number(n),
                line: self.line,
            })),
            Err(_) => Some(Err(ParseError::ParseFloatError(numeric_string, self.line))),
        }
    }

    fn identifier(&mut self, c: char) -> Option<Result<Token, ParseError>> {
        None
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Result<Token, ParseError>;
    fn next(&mut self) -> Option<Self::Item> {
        self.scan()
    }
}

#[derive(Debug, PartialEq)]
enum ParseError {
    InvalidCharacter(char, u32),
    ParseFloatError(String, u32),
    UnterminatedStringError(String, u32),
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
        }
    }
}

impl Error for ParseError {}
