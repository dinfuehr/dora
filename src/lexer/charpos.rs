use std::fmt;
use lexer::position::Position;

pub struct CharPos {
    pub value: char,
    pub position: Position
}

impl fmt::Display for CharPos {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "char {} at line {:?}", self.value, self.position)
    }
}

impl Copy for CharPos {}
