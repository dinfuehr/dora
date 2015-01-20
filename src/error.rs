use std::fmt;

use lexer::position::Position;

pub struct ParseError {
    pub filename: String,
    pub position: Position,
    pub message: String
}

impl fmt::Show for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "error in {} at line {:?}: {}", self.filename, self.position, self.message)
    }
}
