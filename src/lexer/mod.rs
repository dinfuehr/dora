use std::collections::{HashMap, VecDeque};
use std::io::Error;

use lexer::reader::{CodeReader, FileReader, ReaderResult};
use lexer::token::{Token, TokenType};
use lexer::position::Position;
use lexer::charpos::CharPos;
use error::{ParseError, ErrorCode};

#[cfg(test)]
use lexer::reader::StrReader;

pub mod reader;
pub mod token;
pub mod position;
mod charpos;

pub struct Lexer<T : CodeReader> {
    reader: T,
    position: Position,
    eof_reached: bool,
    tabwidth: u32,
    state: State,
    keywords: HashMap<&'static str, TokenType>,

    buffer: VecDeque<Result<CharPos, ParseError>>
}

enum State {
    Initial, String
}

#[cfg(test)]
impl Lexer<StrReader> {
    pub fn from_str(code: &'static str) -> Lexer<StrReader> {
        Lexer::new(StrReader::new(code))
    }
}

impl Lexer<FileReader> {
    pub fn from_file(filename: &str) -> Result<Lexer<FileReader>, Error> {
        let reader = try!(FileReader::new(filename));

        Ok(Lexer::new(reader))
    }
}

// static KEYWORDS: phf::Map<&'static str,TokenType> = phf_map! {
//     "fn" => TokenType::Fn,
//     "var" => TokenType::Var,
//     "while" => TokenType::While,
//     "if" => TokenType::If,
//     "else" => TokenType::Else,
//     "loop" => TokenType::Loop,
//     "break" => TokenType::Break,
//     "continue" => TokenType::Continue,
//     "return" => TokenType::Return,
//     "true" => TokenType::True,
//     "false" => TokenType::False,
//     "enum" => TokenType::Enum,
//     "type" => TokenType::Type,
//     "alias" => TokenType::Alias,
//     "struct" => TokenType::Struct,
//     "trait" => TokenType::Trait,
// };

impl<T : CodeReader> Lexer<T> {
    pub fn new(reader: T) -> Lexer<T> {
        Lexer::new_with_tabwidth(reader, 4)
    }

    pub fn new_with_tabwidth(reader: T, tabwidth: u32) -> Lexer<T> {
        // TODO: replace HashMap with phf when it is stable
        let mut keywords = HashMap::new();
        keywords.insert("class", TokenType::Class);
        keywords.insert("self", TokenType::Selfie);
        keywords.insert("fun", TokenType::Fun);
        keywords.insert("let", TokenType::Let);
        keywords.insert("var", TokenType::Var);
        keywords.insert("while", TokenType::While);
        keywords.insert("if", TokenType::If);
        keywords.insert("else", TokenType::Else);
        keywords.insert("loop", TokenType::Loop);
        keywords.insert("break", TokenType::Break);
        keywords.insert("continue", TokenType::Continue);
        keywords.insert("return", TokenType::Return);
        keywords.insert("true", TokenType::True);
        keywords.insert("false", TokenType::False);
        keywords.insert("nil", TokenType::Nil);
        keywords.insert("enum", TokenType::Enum);
        keywords.insert("type", TokenType::Type);
        keywords.insert("alias", TokenType::Alias);
        keywords.insert("struct", TokenType::Struct);
        keywords.insert("trait", TokenType::Trait);
        keywords.insert("throws", TokenType::Throws);
        keywords.insert("throw", TokenType::Throw);
        keywords.insert("try", TokenType::Try);
        keywords.insert("catch", TokenType::Catch);
        keywords.insert("finally", TokenType::Finally);
        keywords.insert("open", TokenType::Open);
        keywords.insert("override", TokenType::Override);

        let mut lexer = Lexer::<T> {
            reader: reader,
            position: Position::new(1, 1),
            tabwidth: tabwidth,
            eof_reached: false,
            keywords: keywords,
            state: State::Initial,

            buffer: VecDeque::with_capacity(10)
        };
        lexer.fill_buffer();

        lexer
    }

    pub fn read_token(&mut self) -> Result<Token, ParseError> {
        loop {
            self.skip_white();

            if let None = self.top() {
                return Ok(Token::new(TokenType::End, self.position));
            }

            if self.is_digit() {
                return self.read_number();

            } else if self.is_comment_start() {
                if let Some(err) = self.read_comment() {
                    return Err(err)
                }

            } else if self.is_multi_comment_start() {
                if let Some(err) = self.read_multi_comment() {
                    return Err(err)
                }

            } else if self.is_identifier_start() {
                return self.read_identifier();

            } else if self.is_quote() {
                return self.read_string();

            } else if self.is_operator() {
                return self.read_operator();

            } else {
                let ch = self.top().unwrap().value;

                return Err(ParseError {
                    position: self.position,
                    code: ErrorCode::UnknownChar,
                    message: format!("unknown character {} (ascii code {})", ch, ch as usize)
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
        while !self.is_eof() && !self.is_newline() {
            self.read_char();
        }

        None
    }

    fn read_multi_comment(&mut self) -> Option<ParseError> {
        let pos = self.top().unwrap().position;

        self.read_char();
        self.read_char();

        while !self.is_eof() && !self.is_multi_comment_end() {
          self.read_char();
        }

        if self.is_eof() {
          return Some(ParseError {
              position: pos,
              code: ErrorCode::UnclosedComment,
              message: "unclosed comment".to_string()
          } );
        }

        self.read_char();
        self.read_char();

        None
    }

    fn read_identifier(&mut self) -> Result<Token, ParseError> {
        let mut tok = self.build_token(TokenType::Identifier);

        while self.is_identifier() {
            let ch = try!(self.read_char().unwrap()).value;
            tok.value.push(ch);
        }

        if let Some(toktype) = self.keywords.get(&tok.value[..]) {
            tok.token_type = *toktype
        }

        Ok(tok)
    }

    fn read_string(&mut self) -> Result<Token, ParseError> {
        let mut tok = self.build_token(TokenType::String);

        self.read_char();

        while !self.is_eof() && !self.is_newline() && !self.is_quote() {
            let ch = try!(self.read_char().unwrap()).value;
            tok.value.push(ch);
        }

        if self.is_quote() {
            self.read_char();

            Ok(tok)
        } else {
            Err(ParseError {
              position: tok.position,
              code: ErrorCode::UnclosedString,
              message: "unclosed string".to_string()
          })
        }
    }

    fn read_operator(&mut self) -> Result<Token, ParseError> {
        let mut tok = self.build_token(TokenType::End);
        let ch = try!(self.read_char().unwrap()).value;

        let nch = self.top();
        let nch = if nch.is_some() { nch.unwrap().value } else { 'x' };

        let nnch = self.at(1);
        let nnch = if nnch.is_some() { nnch.unwrap().value } else { 'x' };

        tok.token_type = match ch {
            '+' => TokenType::Add,
            '-' => {
                if nch == '>' {
                    self.read_char();
                    TokenType::Arrow
                } else {
                    TokenType::Sub
                }
            }

            '*' => TokenType::Mul,
            '/' => TokenType::Div,
            '%' => TokenType::Mod,

            '(' => TokenType::LParen,
            ')' => TokenType::RParen,
            '[' => TokenType::LBracket,
            ']' => TokenType::RBracket,
            '{' => TokenType::LBrace,
            '}' => TokenType::RBrace,

            '|' => {
                if nch == '|' {
                    self.read_char();
                    TokenType::Or
                } else {
                    TokenType::BitOr
                }
            }

            '&' => {
                if nch == '&' {
                    self.read_char();
                    TokenType::And
                } else {
                    TokenType::BitAnd
                }
            }

            '^' => TokenType::Caret,
            '~' => TokenType::Tilde,
            ',' => TokenType::Comma,
            ';' => TokenType::Semicolon,
            ':' => TokenType::Colon,
            '.' => TokenType::Dot,
            '=' => {
                if nch == '=' {
                    self.read_char();

                    if nnch == '=' {
                        self.read_char();
                        TokenType::Is
                    } else {
                        TokenType::EqEq
                    }
                } else {
                    TokenType::Eq
                }
            }

            '<' => {
                if nch == '=' {
                    self.read_char();
                    TokenType::Le
                } else {
                    TokenType::Lt
                }
            }

            '>' => {
                if nch == '=' {
                    self.read_char();
                    TokenType::Ge
                } else {
                    TokenType::Gt
                }
            }

            '!' => {
                if nch == '=' {
                    self.read_char();

                    if nnch == '=' {
                        self.read_char();
                        TokenType::IsNot
                    } else {
                        TokenType::Ne
                    }
                } else {
                    TokenType::Not
                }
            }

            _ => {
                return Err(ParseError {
                    position: tok.position,
                    code: ErrorCode::UnknownChar,
                    message: format!("unknown character {} (ascii code {})", ch, ch as usize)
                } )
            }
        };

        Ok(tok)
    }

    fn read_number(&mut self) -> Result<Token, ParseError> {
        let mut tok = self.build_token(TokenType::Number);

        while self.is_digit() {
            let ch = try!(self.read_char().unwrap()).value;
            tok.value.push(ch);
        }

        Ok(tok)
    }

    fn read_char(&mut self) -> Option<Result<CharPos, ParseError>> {
        let ch = self.buffer.pop_front();
        self.fill_buffer();

        ch
    }

    fn top(&self) -> Option<CharPos> {
        self.at(0)
    }

    fn at(&self, index: usize) -> Option<CharPos> {
        if self.buffer.len() > index {
            match self.buffer[index] {
                Ok(ch) => Some(ch),
                _ => None,
            }
        } else {
            None
        }
    }

    fn build_token(&self, token_type: TokenType) -> Token {
        Token::new(token_type, self.top().unwrap().position)
    }

    fn fill_buffer(&mut self) {
        while !self.eof_reached && self.buffer.len() < 10 {
            match self.reader.next() {
                ReaderResult::Char(ch) => {
                    self.buffer.push_back(Ok(CharPos { value: ch, position: self.position }));

                    match ch {
                        '\n' => {
                            self.position.line += 1;
                            self.position.column = 1;
                        },

                        '\t' => {
                            let tabdepth = (self.position.column-1)/self.tabwidth;

                            self.position.column = 1 + self.tabwidth * (tabdepth+1);
                        }

                        _ => self.position.column += 1
                    }
                },

                ReaderResult::Eof => self.eof_reached = true,

                ReaderResult::Err => {
                    self.buffer.push_back(Err(ParseError {
                        position: self.position,
                        message: "error reading from file".to_string(),
                        code: ErrorCode::IoError,
                    }))
                },
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

    fn is_operator(&self) -> bool {
        let top = self.top();

        if top.is_none() { return false; }

        "^+-*/%&|,=!~;:.()[]{}<>".contains(top.unwrap().value)
    }

    fn is_digit(&self) -> bool {
        let top = self.top();

        top.is_some() && top.unwrap().value.is_digit(10)
    }

    fn is_whitespace(&self) -> bool {
        let top = self.top();

        top.is_some() && top.unwrap().value.is_whitespace()
    }

    fn is_identifier_start(&self) -> bool {
        let top = self.top();
        if top.is_none() { return false; }

        let ch = top.unwrap().value;

        ( ch >= 'a' && ch <= 'z' ) || ( ch >= 'A' && ch <= 'Z' ) || ch == '_'
    }

    fn is_identifier(&self) -> bool {
        self.is_identifier_start() || self.is_digit()
    }

    fn is_newline(&self) -> bool {
        let top = self.top();

        top.is_some() && top.unwrap().value == '\n'
    }

    fn is_quote(&self) -> bool {
        let top = self.top();

        top.is_some() && top.unwrap().value == '\"'
    }

    fn is_eof(&self) -> bool {
        self.top().is_none()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::reader::{CodeReader, StrReader};
    use lexer::token::TokenType;
    use error::ErrorCode;

    fn assert_end<T: CodeReader>(reader: &mut Lexer<T>, l: u32, c: u32) {
        assert_tok(reader, TokenType::End, "", l, c);
    }

    fn assert_tok<T: CodeReader>(reader: &mut Lexer<T>, token_type: TokenType,
                                 val: &'static str, l: u32, c: u32) {
        let tok = reader.read_token().unwrap();
        assert_eq!(token_type, tok.token_type);
        assert_eq!(val, tok.value);
        assert_eq!(l, tok.position.line);
        assert_eq!(c, tok.position.column);
    }

    fn assert_err<T: CodeReader>(reader: &mut Lexer<T>, code: ErrorCode, l: u32, c: u32) {
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

        let mut reader = Lexer::from_str("1/*test");
        assert_tok(&mut reader, TokenType::Number, "1", 1, 1);
        assert_err(&mut reader, ErrorCode::UnclosedComment, 1, 2);
    }

    #[test]
    fn test_read_identifier() {
        let mut reader = Lexer::from_str("abc ident test");
        assert_tok(&mut reader, TokenType::Identifier, "abc", 1, 1);
        assert_tok(&mut reader, TokenType::Identifier, "ident", 1, 5);
        assert_tok(&mut reader, TokenType::Identifier, "test", 1, 11);
        assert_end(&mut reader, 1, 15);

    }

    #[test]
    fn test_code_with_spaces() {
        let mut reader = Lexer::from_str("1 2 3");
        assert_tok(&mut reader, TokenType::Number, "1", 1, 1);
        assert_tok(&mut reader, TokenType::Number, "2", 1, 3);
        assert_tok(&mut reader, TokenType::Number, "3", 1, 5);
        assert_end(&mut reader, 1, 6);
    }

    #[test]
    fn test_code_with_newlines() {
        let mut reader = Lexer::from_str("1\n2\n3");
        assert_tok(&mut reader, TokenType::Number, "1", 1, 1);
        assert_tok(&mut reader, TokenType::Number, "2", 2, 1);
        assert_tok(&mut reader, TokenType::Number, "3", 3, 1);
        assert_end(&mut reader, 3, 2);
    }

    #[test]
    fn test_code_with_tabs() {
        let mut reader = Lexer::from_str("1\t2\t3");
        assert_tok(&mut reader, TokenType::Number, "1", 1, 1);
        assert_tok(&mut reader, TokenType::Number, "2", 1, 5);
        assert_tok(&mut reader, TokenType::Number, "3", 1, 9);
        assert_end(&mut reader, 1, 10);
    }

    #[test]
    fn test_code_with_tabwidth8() {
        let str_reader = StrReader::new("1\t2\n1234567\t8\n12345678\t9");
        let mut reader = Lexer::new_with_tabwidth(str_reader, 8);

        assert_tok(&mut reader, TokenType::Number, "1", 1, 1);
        assert_tok(&mut reader, TokenType::Number, "2", 1, 9);
        assert_tok(&mut reader, TokenType::Number, "1234567", 2, 1);
        assert_tok(&mut reader, TokenType::Number, "8", 2, 9);
        assert_tok(&mut reader, TokenType::Number, "12345678", 3, 1);
        assert_tok(&mut reader, TokenType::Number, "9", 3, 17);
        assert_end(&mut reader, 3, 18);
    }

    #[test]
    fn test_string_with_newline() {
        let mut reader = Lexer::from_str("\"abc\ndef\"");
        assert_err(&mut reader, ErrorCode::UnclosedString, 1, 1);
    }

    #[test]
    fn test_unclosed_string() {
        let mut reader = Lexer::from_str("\"abc");
        assert_err(&mut reader, ErrorCode::UnclosedString, 1, 1);
    }

    #[test]
    fn test_string() {
        let mut reader = Lexer::from_str("\"abc\"");
        assert_tok(&mut reader, TokenType::String, "abc", 1, 1);
        assert_end(&mut reader, 1, 6);
    }

    #[test]
    fn test_keywords() {
        let mut reader = Lexer::from_str("fun let while if else self class");
        assert_tok(&mut reader, TokenType::Fun, "fun", 1, 1);
        assert_tok(&mut reader, TokenType::Let, "let", 1, 5);
        assert_tok(&mut reader, TokenType::While, "while", 1, 9);
        assert_tok(&mut reader, TokenType::If, "if", 1, 15);
        assert_tok(&mut reader, TokenType::Else, "else", 1, 18);
        assert_tok(&mut reader, TokenType::Selfie, "self", 1, 23);
        assert_tok(&mut reader, TokenType::Class, "class", 1, 28);

        let mut reader = Lexer::from_str("loop break continue return nil");
        assert_tok(&mut reader, TokenType::Loop, "loop", 1, 1);
        assert_tok(&mut reader, TokenType::Break, "break", 1, 6);
        assert_tok(&mut reader, TokenType::Continue, "continue", 1, 12);
        assert_tok(&mut reader, TokenType::Return, "return", 1, 21);
        assert_tok(&mut reader, TokenType::Nil, "nil", 1, 28);

        let mut reader = Lexer::from_str("type struct enum alias trait");
        assert_tok(&mut reader, TokenType::Type, "type", 1, 1);
        assert_tok(&mut reader, TokenType::Struct, "struct", 1, 6);
        assert_tok(&mut reader, TokenType::Enum, "enum", 1, 13);
        assert_tok(&mut reader, TokenType::Alias, "alias", 1, 18);
        assert_tok(&mut reader, TokenType::Trait, "trait", 1, 24);
    }

    #[test]
    fn test_operators() {
        let mut reader = Lexer::from_str("==+=-*/%~.");
        assert_tok(&mut reader, TokenType::EqEq, "", 1, 1);
        assert_tok(&mut reader, TokenType::Add, "", 1, 3);
        assert_tok(&mut reader, TokenType::Eq, "", 1, 4);
        assert_tok(&mut reader, TokenType::Sub, "", 1, 5);
        assert_tok(&mut reader, TokenType::Mul, "", 1, 6);
        assert_tok(&mut reader, TokenType::Div, "", 1, 7);
        assert_tok(&mut reader, TokenType::Mod, "", 1, 8);
        assert_tok(&mut reader, TokenType::Tilde, "", 1, 9);
        assert_tok(&mut reader, TokenType::Dot, "", 1, 10);

        let mut reader = Lexer::from_str("<=<>=><");
        assert_tok(&mut reader, TokenType::Le, "", 1, 1);
        assert_tok(&mut reader, TokenType::Lt, "", 1, 3);
        assert_tok(&mut reader, TokenType::Ge, "", 1, 4);
        assert_tok(&mut reader, TokenType::Gt, "", 1, 6);
        assert_tok(&mut reader, TokenType::Lt, "", 1, 7);

        let mut reader = Lexer::from_str("!=====!");
        assert_tok(&mut reader, TokenType::IsNot, "", 1, 1);
        assert_tok(&mut reader, TokenType::Is, "", 1, 4);
        assert_tok(&mut reader, TokenType::Not, "", 1, 7);

        let mut reader = Lexer::from_str("!=!");
        assert_tok(&mut reader, TokenType::Ne, "", 1, 1);
        assert_tok(&mut reader, TokenType::Not, "", 1, 3);

        let mut reader = Lexer::from_str("->");
        assert_tok(&mut reader, TokenType::Arrow, "", 1, 1);
    }
}
