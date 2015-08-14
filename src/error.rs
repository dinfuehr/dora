use std::fmt;

use lexer::position::Position;

#[derive(PartialEq, Eq, Debug)]
pub enum ErrorCode {
    UnclosedComment, UnknownChar, UnclosedString, NumberOverflow, UnknownFactor,
    UnexpectedToken, ExpectedTopLevelElement, ExpectedType, ExpectedIdentifier,
    MisplacedElse, IoError, CommaExpected, ExpectedValue,

    MainDefinition, Unimplemented
}

#[derive(Debug)]
pub struct ParseError {
    pub position: Position,
    pub message: String,
    pub code: ErrorCode
}

pub fn err<T>(pos: Position, msg: String, code: ErrorCode) -> Result<T, ParseError> {
    Err(ParseError {
        position: pos,
        message: msg,
        code: code,
    })
}

pub fn unimplemented(pos: Position) -> Result<(), ParseError> {
    err(pos, "not yet implemented".to_string(), ErrorCode::Unimplemented)
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "error at line {}: {}", self.position, self.message)
    }
}

