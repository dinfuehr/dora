use std::fmt;

use lexer::position::Position;

#[derive(PartialEq,Eq,Debug)]
pub enum ErrorCode {
    UnclosedComment, UnknownChar, UnclosedString, NumberOverflow, UnknownFactor,
    UnexpectedToken, ExpectedTopLevelElement, ExpectedType, ExpectedIdentifier,
    MisplacedElse, VarAlreadyExists, ExpectedLvalue, TypeMismatch, VarNotFound,
    IoError, NoReturnValue, UnreachableCode
}

pub struct ParseError {
    pub position: Position,
    pub message: String,
    pub code: ErrorCode
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "error at line {}: {}", self.position, self.message)
    }
}

impl fmt::Debug for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "error at line {}: {}", self.position, self.message)
    }
}
