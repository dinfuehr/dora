use std::fmt::{Formatter,Show,Error};
use std::result::Result;

use lexer::position::Position;

pub struct Character {
    value : Option<char>,
    position : Position
}

impl Character {
    pub fn is_eof(&self) -> bool {
        self.value.is_none()
    }
}

impl Show for Character {
    fn fmt(&self, f : &mut Formatter) -> Result<(), Error> {
        write!(f, "{} at {}", self.value, self.position)
    }
}
