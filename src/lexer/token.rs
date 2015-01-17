use std::fmt::{Formatter,Show,Error};
use std::result::Result;

use lexer::position::Position;

#[deriving(Show)]
pub enum TokenType {
    End
}

pub struct Token {
    ttype: TokenType,
    value: String,
    position: Position
}

impl Token {
    fn new( tok: TokenType, line: int, col: int ) -> Token {
        Token { ttype: tok, value: "".to_string(), position: Position::new(line, col) }
    }
}

impl Show for Token {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{} at {}", self.ttype, self.position)
    }
}

#[test]
fn test_new() {
    let tok = Token::new(TokenType::End, 1, 1);
    assert_eq!(format!("{}", tok).as_slice(), "End at 1:1");
}
