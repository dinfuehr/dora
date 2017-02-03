use std::collections::{HashMap, VecDeque};
use std::io::Error;

use lexer::reader::{CodeReader, FileReader, ReaderResult};
use lexer::token::{Token, TokenKind};
use lexer::position::Position;
use lexer::charpos::CharPos;
use error::msg::{Msg, MsgWithPos};

#[cfg(test)]
use lexer::reader::StrReader;

pub mod reader;
pub mod token;
pub mod position;
mod charpos;

pub struct Lexer<T: CodeReader> {
    reader: T,
    position: Position,
    eof_reached: bool,
    tabwidth: u32,
    state: State,
    keywords: HashMap<&'static str, TokenKind>,

    buffer: VecDeque<Result<CharPos, MsgWithPos>>,
}

enum State {
    Initial,
    String,
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

impl<T: CodeReader> Lexer<T> {
    pub fn new(reader: T) -> Lexer<T> {
        Lexer::new_with_tabwidth(reader, 4)
    }

    pub fn new_with_tabwidth(reader: T, tabwidth: u32) -> Lexer<T> {
        // TODO: replace HashMap with phf when it is stable
        let mut keywords = HashMap::new();
        keywords.insert("class", TokenKind::Class);
        keywords.insert("self", TokenKind::This);
        keywords.insert("super", TokenKind::Super);
        keywords.insert("fun", TokenKind::Fun);
        keywords.insert("let", TokenKind::Let);
        keywords.insert("var", TokenKind::Var);
        keywords.insert("while", TokenKind::While);
        keywords.insert("if", TokenKind::If);
        keywords.insert("else", TokenKind::Else);
        keywords.insert("loop", TokenKind::Loop);
        keywords.insert("break", TokenKind::Break);
        keywords.insert("continue", TokenKind::Continue);
        keywords.insert("return", TokenKind::Return);
        keywords.insert("true", TokenKind::True);
        keywords.insert("false", TokenKind::False);
        keywords.insert("nil", TokenKind::Nil);
        keywords.insert("enum", TokenKind::Enum);
        keywords.insert("type", TokenKind::Type);
        keywords.insert("alias", TokenKind::Alias);
        keywords.insert("struct", TokenKind::Struct);
        keywords.insert("trait", TokenKind::Trait);
        keywords.insert("throws", TokenKind::Throws);
        keywords.insert("throw", TokenKind::Throw);
        keywords.insert("try", TokenKind::Try);
        keywords.insert("do", TokenKind::Do);
        keywords.insert("catch", TokenKind::Catch);
        keywords.insert("finally", TokenKind::Finally);
        keywords.insert("open", TokenKind::Open);
        keywords.insert("override", TokenKind::Override);
        keywords.insert("final", TokenKind::Final);
        keywords.insert("is", TokenKind::Is);
        keywords.insert("as", TokenKind::As);
        keywords.insert("internal", TokenKind::Internal);
        keywords.insert("init", TokenKind::Init);

        let mut lexer = Lexer::<T> {
            reader: reader,
            position: Position::new(1, 1),
            tabwidth: tabwidth,
            eof_reached: false,
            keywords: keywords,
            state: State::Initial,

            buffer: VecDeque::with_capacity(10),
        };
        lexer.fill_buffer();

        lexer
    }

    pub fn filename(&self) -> &str {
        self.reader.filename()
    }

    pub fn read_token(&mut self) -> Result<Token, MsgWithPos> {
        loop {
            self.skip_white();

            if let None = self.top() {
                return Ok(Token::new(TokenKind::End, self.position));
            }

            if self.is_digit() {
                return self.read_number();

            } else if self.is_comment_start() {
                try!(self.read_comment());

            } else if self.is_multi_comment_start() {
                try!(self.read_multi_comment());

            } else if self.is_identifier_start() {
                return self.read_identifier();

            } else if self.is_quote() {
                return self.read_string();

            } else if self.is_operator() {
                return self.read_operator();

            } else {
                let ch = self.top().unwrap().value;

                return Err(MsgWithPos::new(self.position, Msg::UnknownChar(ch)));
            }
        }
    }

    fn skip_white(&mut self) {
        while self.is_whitespace() {
            self.read_char();
        }
    }

    fn read_comment(&mut self) -> Result<(), MsgWithPos> {
        while !self.is_eof() && !self.is_newline() {
            self.read_char();
        }

        Ok(())
    }

    fn read_multi_comment(&mut self) -> Result<(), MsgWithPos> {
        let pos = self.top().unwrap().position;

        self.read_char();
        self.read_char();

        while !self.is_eof() && !self.is_multi_comment_end() {
            self.read_char();
        }

        if self.is_eof() {
            return Err(MsgWithPos::new(pos, Msg::UnclosedComment));
        }

        self.read_char();
        self.read_char();

        Ok(())
    }

    fn read_identifier(&mut self) -> Result<Token, MsgWithPos> {
        let pos = self.top().unwrap().position;
        let mut value = String::new();

        while self.is_identifier() {
            let ch = try!(self.read_char().unwrap()).value;
            value.push(ch);
        }

        let lookup = self.keywords.get(&value[..]).cloned();
        let mut ttype;

        if let Some(tok_type) = lookup {
            ttype = tok_type;

            if ttype == TokenKind::Try {
                if let Some(ch) = self.top() {
                    let ch = ch.value;

                    if ch == '!' || ch == '?' {
                        try!(self.read_char().unwrap());

                        ttype = if ch == '!' {
                            TokenKind::TryForce
                        } else {
                            TokenKind::TryOpt
                        };
                    }
                }
            }

        } else if value == "_" {
            ttype = TokenKind::Underscore;
        } else {
            ttype = TokenKind::Identifier(value);
        }

        Ok(Token::new(ttype, pos))
    }

    fn read_string(&mut self) -> Result<Token, MsgWithPos> {
        let pos = self.top().unwrap().position;
        let mut value = String::new();

        let mut escape = false;

        self.read_char();

        while !self.is_eof() && (escape || !self.is_quote()) {
            let mut ch = try!(self.read_char().unwrap()).value;

            if escape {
                ch = match ch {
                    '\\' => '\\',
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    '\"' => '\"',
                    '\'' => '\'',
                    _ => {
                        let msg = Msg::InvalidEscapeSequence(ch);
                        let msg = MsgWithPos::new(pos, msg);
                        return Err(msg);
                    }
                };

                escape = false;

            } else if ch == '\\' {
                escape = true;
                continue;
            }

            value.push(ch);
        }

        if !escape && self.is_quote() {
            self.read_char();

            let ttype = TokenKind::String(value);
            Ok(Token::new(ttype, pos))

        } else {
            Err(MsgWithPos::new(pos, Msg::UnclosedString))
        }
    }

    fn read_operator(&mut self) -> Result<Token, MsgWithPos> {
        let mut tok = self.build_token(TokenKind::End);
        let ch = try!(self.read_char().unwrap()).value;

        let nch = self.top();
        let nch = if nch.is_some() {
            nch.unwrap().value
        } else {
            'x'
        };

        let nnch = self.at(1);
        let nnch = if nnch.is_some() {
            nnch.unwrap().value
        } else {
            'x'
        };

        tok.kind = match ch {
            '+' => TokenKind::Add,
            '-' => {
                if nch == '>' {
                    self.read_char();
                    TokenKind::Arrow
                } else {
                    TokenKind::Sub
                }
            }

            '*' => TokenKind::Mul,
            '/' => TokenKind::Div,
            '%' => TokenKind::Mod,

            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,

            '|' => {
                if nch == '|' {
                    self.read_char();
                    TokenKind::Or
                } else {
                    TokenKind::BitOr
                }
            }

            '&' => {
                if nch == '&' {
                    self.read_char();
                    TokenKind::And
                } else {
                    TokenKind::BitAnd
                }
            }

            '^' => TokenKind::Caret,
            '~' => TokenKind::Tilde,
            ',' => TokenKind::Comma,
            ';' => TokenKind::Semicolon,
            ':' => TokenKind::Colon,
            '.' => TokenKind::Dot,
            '=' => {
                if nch == '=' {
                    self.read_char();

                    if nnch == '=' {
                        self.read_char();
                        TokenKind::EqEqEq
                    } else {
                        TokenKind::EqEq
                    }
                } else {
                    TokenKind::Eq
                }
            }

            '<' => {
                match nch {
                    '=' => {
                        self.read_char();
                        TokenKind::Le
                    }

                    '<' => {
                        self.read_char();
                        TokenKind::LtLt
                    }

                    _ => TokenKind::Lt,
                }
            }

            '>' => {
                match nch {
                    '=' => {
                        self.read_char();
                        TokenKind::Ge
                    }

                    '>' => {
                        self.read_char();

                        if nnch == '>' {
                            self.read_char();
                            TokenKind::GtGtGt
                        } else {
                            TokenKind::GtGt
                        }
                    }

                    _ => TokenKind::Gt,
                }
            }

            '!' => {
                if nch == '=' {
                    self.read_char();

                    if nnch == '=' {
                        self.read_char();
                        TokenKind::NeEqEq
                    } else {
                        TokenKind::Ne
                    }
                } else {
                    TokenKind::Not
                }
            }

            _ => {
                return Err(MsgWithPos::new(tok.position, Msg::UnknownChar(ch)));
            }
        };

        Ok(tok)
    }

    fn read_number(&mut self) -> Result<Token, MsgWithPos> {
        let pos = self.top().unwrap().position;
        let mut value = String::new();
        let mut long = false;

        while self.is_digit() {
            let ch = try!(self.read_char().unwrap()).value;
            value.push(ch);
        }

        if self.top().map(|ch| ch.value) == Some('L') {
            self.read_char().unwrap()?;
            long = true;
        }

        let ttype = TokenKind::Number(value, long);
        Ok(Token::new(ttype, pos))
    }

    fn read_char(&mut self) -> Option<Result<CharPos, MsgWithPos>> {
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

    fn build_token(&self, kind: TokenKind) -> Token {
        Token::new(kind, self.top().unwrap().position)
    }

    fn fill_buffer(&mut self) {
        while !self.eof_reached && self.buffer.len() < 10 {
            match self.reader.next() {
                ReaderResult::Char(ch) => {
                    self.buffer.push_back(Ok(CharPos {
                        value: ch,
                        position: self.position,
                    }));

                    match ch {
                        '\n' => {
                            self.position.line += 1;
                            self.position.column = 1;
                        }

                        '\t' => {
                            let tabdepth = (self.position.column - 1) / self.tabwidth;

                            self.position.column = 1 + self.tabwidth * (tabdepth + 1);
                        }

                        _ => self.position.column += 1,
                    }
                }

                ReaderResult::Eof => self.eof_reached = true,

                ReaderResult::Err => {
                    let msg = MsgWithPos::new(self.position, Msg::IoError);
                    self.buffer.push_back(Err(msg))
                }
            }
        }
    }

    fn is_comment_start(&self) -> bool {
        let top = self.top();
        let ntop = self.at(1);

        top.is_some() && top.unwrap().value == '/' && ntop.is_some() && ntop.unwrap().value == '/'
    }

    fn is_multi_comment_start(&self) -> bool {
        let top = self.top();
        let ntop = self.at(1);

        top.is_some() && top.unwrap().value == '/' && ntop.is_some() && ntop.unwrap().value == '*'
    }

    fn is_multi_comment_end(&self) -> bool {
        let top = self.top();
        let ntop = self.at(1);

        top.is_some() && top.unwrap().value == '*' && ntop.is_some() && ntop.unwrap().value == '/'
    }

    fn is_operator(&self) -> bool {
        let top = self.top();

        if top.is_none() {
            return false;
        }

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
        if top.is_none() {
            return false;
        }

        let ch = top.unwrap().value;

        (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_'
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
    use error::msg::Msg;
    use lexer::reader::{CodeReader, StrReader};
    use lexer::token::TokenKind;

    fn assert_end<T: CodeReader>(reader: &mut Lexer<T>, l: u32, c: u32) {
        assert_tok(reader, TokenKind::End, l, c);
    }

    fn assert_tok<T: CodeReader>(reader: &mut Lexer<T>, kind: TokenKind, l: u32, c: u32) {
        let tok = reader.read_token().unwrap();
        assert_eq!(kind, tok.kind);
        assert_eq!(l, tok.position.line);
        assert_eq!(c, tok.position.column);
    }

    fn assert_err<T: CodeReader>(reader: &mut Lexer<T>, msg: Msg, l: u32, c: u32) {
        let err = reader.read_token().unwrap_err();
        assert_eq!(msg, err.msg);
        assert_eq!(l, err.pos.line);
        assert_eq!(c, err.pos.column);
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
        assert_tok(&mut reader, TokenKind::Number("1".into(), false), 1, 1);
        assert_tok(&mut reader, TokenKind::Number("2".into(), false), 1, 3);
        assert_tok(&mut reader, TokenKind::Number("0123".into(), false), 2, 1);
        assert_tok(&mut reader, TokenKind::Number("10".into(), false), 2, 6);
        assert_end(&mut reader, 2, 8);
    }

    #[test]
    fn test_skip_single_line_comment() {
        let mut reader = Lexer::from_str("//test\n1");
        assert_tok(&mut reader, TokenKind::Number("1".into(), false), 2, 1);
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
        assert_tok(&mut reader, TokenKind::Number("1".into(), false), 1, 9);
        assert_end(&mut reader, 1, 10);
    }

    #[test]
    fn test_unfinished_multi_comment() {
        let mut reader = Lexer::from_str("/*test");
        assert_err(&mut reader, Msg::UnclosedComment, 1, 1);

        let mut reader = Lexer::from_str("1/*test");
        assert_tok(&mut reader, TokenKind::Number("1".into(), false), 1, 1);
        assert_err(&mut reader, Msg::UnclosedComment, 1, 2);
    }

    #[test]
    fn test_read_identifier() {
        let mut reader = Lexer::from_str("abc ident test");
        assert_tok(&mut reader, TokenKind::Identifier("abc".into()), 1, 1);
        assert_tok(&mut reader, TokenKind::Identifier("ident".into()), 1, 5);
        assert_tok(&mut reader, TokenKind::Identifier("test".into()), 1, 11);
        assert_end(&mut reader, 1, 15);

    }

    #[test]
    fn test_code_with_spaces() {
        let mut reader = Lexer::from_str("1 2 3");
        assert_tok(&mut reader, TokenKind::Number("1".into(), false), 1, 1);
        assert_tok(&mut reader, TokenKind::Number("2".into(), false), 1, 3);
        assert_tok(&mut reader, TokenKind::Number("3".into(), false), 1, 5);
        assert_end(&mut reader, 1, 6);
    }

    #[test]
    fn test_code_with_newlines() {
        let mut reader = Lexer::from_str("1\n2\n3");
        assert_tok(&mut reader, TokenKind::Number("1".into(), false), 1, 1);
        assert_tok(&mut reader, TokenKind::Number("2".into(), false), 2, 1);
        assert_tok(&mut reader, TokenKind::Number("3".into(), false), 3, 1);
        assert_end(&mut reader, 3, 2);
    }

    #[test]
    fn test_code_with_tabs() {
        let mut reader = Lexer::from_str("1\t2\t3");
        assert_tok(&mut reader, TokenKind::Number("1".into(), false), 1, 1);
        assert_tok(&mut reader, TokenKind::Number("2".into(), false), 1, 5);
        assert_tok(&mut reader, TokenKind::Number("3".into(), false), 1, 9);
        assert_end(&mut reader, 1, 10);
    }

    #[test]
    fn test_code_with_tabwidth8() {
        let str_reader = StrReader::new("1\t2\n1234567\t8\n12345678\t9");
        let mut reader = Lexer::new_with_tabwidth(str_reader, 8);

        assert_tok(&mut reader, TokenKind::Number("1".into(), false), 1, 1);
        assert_tok(&mut reader, TokenKind::Number("2".into(), false), 1, 9);
        assert_tok(&mut reader,
                   TokenKind::Number("1234567".into(), false),
                   2,
                   1);
        assert_tok(&mut reader, TokenKind::Number("8".into(), false), 2, 9);
        assert_tok(&mut reader,
                   TokenKind::Number("12345678".into(), false),
                   3,
                   1);
        assert_tok(&mut reader, TokenKind::Number("9".into(), false), 3, 17);
        assert_end(&mut reader, 3, 18);
    }

    #[test]
    fn test_string_with_newline() {
        let mut reader = Lexer::from_str("\"abc\ndef\"");
        assert_tok(&mut reader, TokenKind::String("abc\ndef".into()), 1, 1);
    }

    #[test]
    fn test_escape_sequences() {
        let mut reader = Lexer::from_str("\"\\\"\"");
        assert_tok(&mut reader, TokenKind::String("\"".into()), 1, 1);

        let mut reader = Lexer::from_str("\"\\\'\"");
        assert_tok(&mut reader, TokenKind::String("'".into()), 1, 1);

        let mut reader = Lexer::from_str("\"\\t\"");
        assert_tok(&mut reader, TokenKind::String("\t".into()), 1, 1);

        let mut reader = Lexer::from_str("\"\\n\"");
        assert_tok(&mut reader, TokenKind::String("\n".into()), 1, 1);

        let mut reader = Lexer::from_str("\"\\r\"");
        assert_tok(&mut reader, TokenKind::String("\r".into()), 1, 1);

        let mut reader = Lexer::from_str("\"\\\\\"");
        assert_tok(&mut reader, TokenKind::String("\\".into()), 1, 1);

        let mut reader = Lexer::from_str("\"\\");
        assert_err(&mut reader, Msg::UnclosedString, 1, 1);
    }

    #[test]
    fn test_unclosed_string() {
        let mut reader = Lexer::from_str("\"abc");
        assert_err(&mut reader, Msg::UnclosedString, 1, 1);
    }

    #[test]
    fn test_string() {
        let mut reader = Lexer::from_str("\"abc\"");
        assert_tok(&mut reader, TokenKind::String("abc".into()), 1, 1);
        assert_end(&mut reader, 1, 6);
    }

    #[test]
    fn test_keywords() {
        let mut reader = Lexer::from_str("fun let while if else self class");
        assert_tok(&mut reader, TokenKind::Fun, 1, 1);
        assert_tok(&mut reader, TokenKind::Let, 1, 5);
        assert_tok(&mut reader, TokenKind::While, 1, 9);
        assert_tok(&mut reader, TokenKind::If, 1, 15);
        assert_tok(&mut reader, TokenKind::Else, 1, 18);

        let mut reader = Lexer::from_str("self class super");
        assert_tok(&mut reader, TokenKind::This, 1, 1);
        assert_tok(&mut reader, TokenKind::Class, 1, 6);
        assert_tok(&mut reader, TokenKind::Super, 1, 12);

        let mut reader = Lexer::from_str("loop break continue return nil");
        assert_tok(&mut reader, TokenKind::Loop, 1, 1);
        assert_tok(&mut reader, TokenKind::Break, 1, 6);
        assert_tok(&mut reader, TokenKind::Continue, 1, 12);
        assert_tok(&mut reader, TokenKind::Return, 1, 21);
        assert_tok(&mut reader, TokenKind::Nil, 1, 28);

        let mut reader = Lexer::from_str("type struct enum alias trait");
        assert_tok(&mut reader, TokenKind::Type, 1, 1);
        assert_tok(&mut reader, TokenKind::Struct, 1, 6);
        assert_tok(&mut reader, TokenKind::Enum, 1, 13);
        assert_tok(&mut reader, TokenKind::Alias, 1, 18);
        assert_tok(&mut reader, TokenKind::Trait, 1, 24);
    }

    #[test]
    fn test_operators() {
        let mut reader = Lexer::from_str("==+=-*/%~.");
        assert_tok(&mut reader, TokenKind::EqEq, 1, 1);
        assert_tok(&mut reader, TokenKind::Add, 1, 3);
        assert_tok(&mut reader, TokenKind::Eq, 1, 4);
        assert_tok(&mut reader, TokenKind::Sub, 1, 5);
        assert_tok(&mut reader, TokenKind::Mul, 1, 6);
        assert_tok(&mut reader, TokenKind::Div, 1, 7);
        assert_tok(&mut reader, TokenKind::Mod, 1, 8);
        assert_tok(&mut reader, TokenKind::Tilde, 1, 9);
        assert_tok(&mut reader, TokenKind::Dot, 1, 10);

        let mut reader = Lexer::from_str("<=<>=><");
        assert_tok(&mut reader, TokenKind::Le, 1, 1);
        assert_tok(&mut reader, TokenKind::Lt, 1, 3);
        assert_tok(&mut reader, TokenKind::Ge, 1, 4);
        assert_tok(&mut reader, TokenKind::Gt, 1, 6);
        assert_tok(&mut reader, TokenKind::Lt, 1, 7);

        let mut reader = Lexer::from_str("!=====!");
        assert_tok(&mut reader, TokenKind::NeEqEq, 1, 1);
        assert_tok(&mut reader, TokenKind::EqEqEq, 1, 4);
        assert_tok(&mut reader, TokenKind::Not, 1, 7);

        let mut reader = Lexer::from_str("!=!");
        assert_tok(&mut reader, TokenKind::Ne, 1, 1);
        assert_tok(&mut reader, TokenKind::Not, 1, 3);

        let mut reader = Lexer::from_str("->");
        assert_tok(&mut reader, TokenKind::Arrow, 1, 1);

        let mut reader = Lexer::from_str("try!try?1");
        assert_tok(&mut reader, TokenKind::TryForce, 1, 1);
        assert_tok(&mut reader, TokenKind::TryOpt, 1, 5);
        assert_tok(&mut reader, TokenKind::Number("1".into(), false), 1, 9);

        let mut reader = Lexer::from_str(">><<>>>_");
        assert_tok(&mut reader, TokenKind::GtGt, 1, 1);
        assert_tok(&mut reader, TokenKind::LtLt, 1, 3);
        assert_tok(&mut reader, TokenKind::GtGtGt, 1, 5);
        assert_tok(&mut reader, TokenKind::Underscore, 1, 8);
    }
}
