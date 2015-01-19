use std::fmt;
use std::result::Result;

use lexer::position::Position;

#[derive(Show)]
pub enum TokenType {
    Number, Identifier, End
}

pub struct Token {
    ttype: TokenType,
    value: String,
    position: Position
}

impl Token {
    fn new( tok: TokenType, pos: Position ) -> Token {
        Token { ttype: tok, value: "".to_string(), position: pos }
    }
}

impl fmt::String for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{} (type {:?}) at {}", self.value, self.ttype, self.position)
    }
}

#[test]
fn test_new() {
    let tok = Token::new(TokenType::End, 1, 1);
    assert_eq!(format!("{}", tok).as_slice(), "End at 1:1");
}
