use std::fmt;
use lexer::reader::{CodeReader,StrReader,FileReader};
use lexer::token::{Token,TokenType};
use lexer::position::Position;
use error::{ParseError,ErrorCode};

pub mod reader;
pub mod token;
pub mod position;

struct CharPos {
    value: char,
    position: Position
}

impl fmt::Show for CharPos {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "char {} at line {:?}", self.value, self.position)
    }
}

impl Copy for CharPos {}

pub struct Lexer<T : CodeReader> {
    reader: T,
    position: Position,

    buffer: Vec<CharPos>,
    eof_reached: bool
}

impl Lexer<StrReader> {
    pub fn from_str(code: &'static str) -> Lexer<StrReader> {
        Lexer::new(StrReader::new(code))
    }
}

impl Lexer<StrReader> {
    pub fn from_file(filename: &'static str) -> Lexer<FileReader> {
        Lexer::new(FileReader::new(filename))
    }
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
                return Ok(Token::new(TokenType::End, self.position));
            }

            if self.is_digit() {
                return self.read_number();

            } else if self.is_comment_start() {
                match self.read_comment() {
                    Some(err) => return Err(err),
                    _ => {}
                }

            } else if self.is_multi_comment_start() {
                match self.read_multi_comment() {
                    Some(err) => return Err(err),
                    _ => {}
                }

            } else {
                return Err( ParseError {
                  filename: self.reader.filename().to_string(),
                  position: self.position,
                  code: ErrorCode::UnknownChar,
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

    fn read_comment(&mut self) -> Option<ParseError> {
        while self.top().is_some() && !self.is_newline() {
            self.read_char();
        }

        None
    }

    fn read_multi_comment(&mut self) -> Option<ParseError> {
        let pos = self.top().unwrap().position;

        self.read_char();
        self.read_char();

        while self.top().is_some() && !self.is_multi_comment_end() {
          self.read_char();
        }

        if self.top().is_none() {
          return Some(ParseError {
              filename: self.reader.filename().to_string(),
              position: pos,
              code: ErrorCode::UnclosedComment,
              message: "unclosed comment".to_string()
          } );
        }

        self.read_char();
        self.read_char();

        None
    }

    fn read_number(&mut self) -> Result<Token,ParseError> {
        let mut tok = self.build_token(TokenType::Number);

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
        self.at(0)
    }

    fn at(&self, index: usize) -> Option<CharPos> {
        if self.buffer.len() > index {
            Some(self.buffer[index])
        } else {
            None
        }
    }

    fn build_token(&self, ttype: TokenType) -> Token {
        Token::new(ttype, self.top().unwrap().position)
    }

    fn fill_buffer(&mut self) {
        while !self.eof_reached && self.buffer.len() < 10 {
            let ch = self.reader.read_char();

            if ch.is_some() {
                let ch = ch.unwrap();
                self.buffer.push(CharPos { value: ch, position: self.position });

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

    fn is_comment_start(&self) -> bool {
        let top = self.top();
        let ntop = self.at(1);

        top.is_some() && top.unwrap().value == '/' &&
            ntop.is_some() && ntop.unwrap().value == '/'
    }

    fn is_multi_comment_start(&self) -> bool {
        let top = self.top();
        let ntop = self.at(1);

        top.is_some() && top.unwrap().value == '/' &&
            ntop.is_some() && ntop.unwrap().value == '*'
    }

    fn is_multi_comment_end(&self) -> bool {
        let top = self.top();
        let ntop = self.at(1);

        top.is_some() && top.unwrap().value == '*' &&
            ntop.is_some() && ntop.unwrap().value == '/'
    }

    fn is_digit(&self) -> bool {
        let top = self.top();

        top.is_some() && top.unwrap().value.is_digit(10)
    }

    fn is_whitespace(&self) -> bool {
        let top = self.top();

        top.is_some() && top.unwrap().value.is_whitespace()
    }

    fn is_newline(&self) -> bool {
        let top = self.top();

        top.is_some() && top.unwrap().value == '\n'
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::reader::StrReader;
    use lexer::token::TokenType;
    use error::ErrorCode;

    fn assert_end(reader: &mut Lexer<StrReader>, l: u32, c: u32) {
        assert_tok(reader, TokenType::End, "", l, c);
    }

    fn assert_tok(reader: &mut Lexer<StrReader>, ttype: TokenType, val: &'static str, l: u32, c: u32) {
        let tok = reader.read_token().unwrap();
        assert_eq!(ttype, tok.ttype);
        assert_eq!(val, tok.value);
        assert_eq!(l, tok.position.line);
        assert_eq!(c, tok.position.column);
    }

    fn assert_err(reader: &mut Lexer<StrReader>, code: ErrorCode, l: u32, c: u32) {
        let err = reader.read_token().unwrap_err();
        assert_eq!(code, err.code);
        assert_eq!(l, err.position.line);
        assert_eq!(c, err.position.column);
    }

    #[test]
    fn test_read_empty_file() {
        let mut reader = Lexer::from_str("");
        assert_end(&mut reader, 1, 1);
        assert_end(&mut reader, 1, 1);
    }

    #[test]
    fn test_read_numbers() {
        let mut reader = Lexer::from_str("1 2\n0123 10");
        assert_tok(&mut reader, TokenType::Number, "1", 1, 1);
        assert_tok(&mut reader, TokenType::Number, "2", 1, 3);
        assert_tok(&mut reader, TokenType::Number, "0123", 2, 1);
        assert_tok(&mut reader, TokenType::Number, "10", 2, 6);
        assert_end(&mut reader, 2, 8);
    }

    #[test]
    fn test_skip_single_line_comment() {
      let mut reader = Lexer::from_str("//test\n1");
      assert_tok(&mut reader, TokenType::Number, "1", 2, 1);
      assert_end(&mut reader, 2, 2);
    }

    #[test]
    fn test_unfinished_line_comment() {
      let mut reader = Lexer::from_str("//abc");
      assert_end(&mut reader, 1, 6);
    }

    #[test]
    fn test_skip_multi_comment() {
      let mut reader = Lexer::from_str("/*test*/1");
      assert_tok(&mut reader, TokenType::Number, "1", 1, 9);
      assert_end(&mut reader, 1, 10);
    }

    #[test]
    fn test_unfinished_multi_comment() {
        let mut reader = Lexer::from_str("/*test");
        assert_err(&mut reader, ErrorCode::UnclosedComment, 1, 1);
    }
}

