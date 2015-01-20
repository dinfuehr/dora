use lexer::reader::CodeReader;
use lexer::token::{Token,TokenType};
use lexer::position::Position;
use error::ParseError;

pub mod reader;
pub mod token;
pub mod position;

struct CharPos {
    value: char,
    position: Position
}

impl Copy for CharPos {}

pub struct Lexer<T : CodeReader> {
    reader: T,
    position: Position,

    buffer: Vec<CharPos>,
    eof_reached: bool
}

impl<T : CodeReader> Lexer<T> {
    pub fn new(reader : T) -> Lexer<T> {
        let mut lexer = Lexer::<T> {
            reader: reader,
            position: Position::new(1, 1),
            buffer: Vec::with_capacity(10),
            eof_reached: false
        };
        lexer.fill_buffer();

        lexer
    }

    pub fn read_token(&mut self) -> Result<Token,ParseError> {
        loop {
            self.skip_white();

            if self.top().is_none() {
                return Ok(Token::new( TokenType::End, self.position ));
            }

            if self.is_digit() {
                return self.read_number();
            } else {
                return Err( ParseError {
                  filename: self.reader.filename().to_string(),
                  position: self.position,
                  message: "not implemented".to_string()
                } )
            }
        }
    }

    fn skip_white(&mut self) {
        while self.is_whitespace() {
            self.read_char();
        }
    }

    fn read_number(&mut self) -> Result<Token,ParseError> {
        let mut tok = Token::new(TokenType::Number, self.position);

        while self.is_digit() {
            let ch = self.read_char().unwrap().value;
            tok.value.push(ch);
        }

        Ok(tok)
    }

    fn read_char(&mut self) -> Option<CharPos> {
        if self.buffer.len() > 0 {
            let ch = self.buffer.remove(0);
            self.fill_buffer();

            Some(ch)
        } else {
            None
        }
    }

    fn top(&self) -> Option<CharPos> {
        if self.buffer.len() > 0 {
            Some(self.buffer[0])
        } else {
            None
        }
    }

    fn fill_buffer(&mut self) {
        while !self.eof_reached && self.buffer.len() < 10 {
            let ch = self.reader.read_char();

            if ch.is_some() {
                let ch = ch.unwrap();
                self.buffer.push( CharPos { value: ch, position: self.position } );

                if ch == '\n' {
                    self.position.line += 1;
                    self.position.column = 1;
                } else {
                    self.position.column += 1;
                }
            } else {
                self.eof_reached = true;
            }
        }
    }

    fn is_digit(&self) -> bool {
        self.top().is_some() && self.top().unwrap().value.is_digit(10)
    }

    fn is_whitespace(&self) -> bool {
        self.top().is_some() && self.top().unwrap().value.is_whitespace()
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

    #[test]
    fn test_read_numbers() {
      let mut reader = Lexer::new(StrReader::new("1 2 0123 10"));
      assert_eq!("1", reader.read_token().unwrap().value);
      assert_eq!("2", reader.read_token().unwrap().value);
      assert_eq!("0123", reader.read_token().unwrap().value);
      assert_eq!("10", reader.read_token().unwrap().value);
      assert!(reader.read_token().unwrap().is_eof());
    }
}

