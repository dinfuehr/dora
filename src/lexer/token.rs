use std::fmt;
use std::result::Result;

use lexer::position::Position;

#[derive(PartialEq,Show)]
pub enum TokenType {
    Number, Identifier, End
}

pub struct Token {
    ttype: TokenType,
    value: String,
    position: Position
}

impl Token {
    pub fn new( tok: TokenType, pos: Position ) -> Token {
        Token { ttype: tok, value: "".to_string(), position: pos }
    }

    pub fn is_eof(&self) -> bool {
        self.ttype == TokenType::End
    }
}

impl fmt::Show for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "type {:?} (with value {:?}) at {:?}", self.ttype, self.value, self.position)
    }
}

#[test]
fn test_new() {
    let tok = Token::new(TokenType::End, Position::new(1, 1));
    assert_eq!(format!("{:?}", tok).as_slice(), "type End (with value \"\") at 1:1");
}
