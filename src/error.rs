use std::error::Error;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum ParseError {
    InvalidCharacter(char, u32),
    ParseFloatError(String, u32),
    UnterminatedStringError(String, u32),
    UnbalancedParenthesis(u32),
    ExpectedExpression(u32),
    ExpectedSemicolon(u32),
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
            Self::ExpectedSemicolon(line) => {
                write!(f, "Expected semicolon at end of statement on line {}", line)
            }
        }
    }
}

impl Error for ParseError {}

#[derive(Debug)]
pub struct RuntimeError {
    message: String,
    line: u32,
}

impl RuntimeError {
    pub fn new(message: &str, line: u32) -> RuntimeError {
        RuntimeError {
            message: message.to_string(),
            line,
        }
    }
}
