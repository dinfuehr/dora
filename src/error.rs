use std::fmt;

use lexer::position::Position;

#[derive(PartialEq,Show)]
pub enum ErrorCode {
    UnclosedComment, UnknownChar, UnclosedString
}

pub struct ParseError {
    pub filename: String,
    pub position: Position,
    pub message: String,
    pub code: ErrorCode
}

impl fmt::Show for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "error in {} at line {:?}: {}", self.filename, self.position, self.message)
    }
}
