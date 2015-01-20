use lexer::reader::CodeReader;
use lexer::token::{Token,TokenType};
use lexer::position::Position;
use error::ParseError;

pub mod reader;
pub mod token;
pub mod position;

pub struct Lexer<T : CodeReader> {
    reader : T
}

impl<T : CodeReader> Lexer<T> {
    pub fn new(reader : T) -> Lexer<T> {
        Lexer::<T> { reader: reader }
    }

    pub fn read_token(&mut self) -> Result<Token,ParseError> {
        match self.reader.read_char() {
          Some(ch) => Err( ParseError {
            filename: "abc.dora".to_string(),
            position: Position::new(1, 1),
            message: "not implemented".to_string()
          } ),

          None => Ok(Token::new(TokenType::End, Position::new(1, 1)))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::reader::StrReader;

    #[test]
    fn test_read_empty_file() {
        let mut reader = Lexer::new(StrReader::new(""));
        assert!(reader.read_token().unwrap().is_eof());
        assert!(reader.read_token().unwrap().is_eof());
    }
}

