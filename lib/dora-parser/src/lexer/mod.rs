use std::collections::HashMap;

use lexer::position::Position;
use lexer::reader::Reader;
use lexer::token::{FloatSuffix, IntSuffix, IntBase, Token, TokenKind};
use error::msg::{Msg, MsgWithPos};

pub mod map;
pub mod reader;
pub mod token;
pub mod position;

pub struct Lexer {
    reader: Reader,
    keywords: HashMap<&'static str, TokenKind>,
}

impl Lexer {
    #[cfg(test)]
    pub fn from_str(code: &str) -> Lexer {
        let reader = Reader::from_string(code);
        Lexer::new(reader)
    }

    pub fn new(reader: Reader) -> Lexer {
        let keywords = keywords_in_map();

        Lexer {
            reader: reader,
            keywords: keywords,
        }
    }

    pub fn path(&self) -> &str {
        self.reader.path()
    }

    pub fn read_token(&mut self) -> Result<Token, MsgWithPos> {
        loop {
            self.skip_white();

            let pos = self.reader.pos();
            let ch = self.cur();

            if let None = ch {
                return Ok(Token::new(TokenKind::End, pos));
            }

            if is_digit(ch) {
                return self.read_number();

            } else if self.is_comment_start() {
                try!(self.read_comment());

            } else if self.is_multi_comment_start() {
                try!(self.read_multi_comment());

            } else if is_identifier_start(ch) {
                return self.read_identifier();

            } else if is_quote(ch) {
                return self.read_string();

            } else if is_char_quote(ch) {
                return self.read_char_literal();

            } else if is_operator(ch) {
                return self.read_operator();

            } else {
                let ch = ch.unwrap();

                return Err(MsgWithPos::new(self.reader.path().to_string(), pos, Msg::UnknownChar(ch)));
            }
        }
    }

    fn skip_white(&mut self) {
        while is_whitespace(self.cur()) {
            self.read_char();
        }
    }

    fn read_comment(&mut self) -> Result<(), MsgWithPos> {
        while !self.cur().is_none() && !is_newline(self.cur()) {
            self.read_char();
        }

        Ok(())
    }

    fn read_multi_comment(&mut self) -> Result<(), MsgWithPos> {
        let pos = self.reader.pos();

        self.read_char();
        self.read_char();

        while !self.cur().is_none() && !self.is_multi_comment_end() {
            self.read_char();
        }

        if self.cur().is_none() {
            return Err(MsgWithPos::new(self.reader.path().to_string(), pos, Msg::UnclosedComment));
        }

        self.read_char();
        self.read_char();

        Ok(())
    }

    fn read_identifier(&mut self) -> Result<Token, MsgWithPos> {
        let pos = self.reader.pos();
        let mut value = String::new();

        while is_identifier(self.cur()) {
            let ch = self.cur().unwrap();
            self.read_char();
            value.push(ch);
        }

        let lookup = self.keywords.get(&value[..]).cloned();
        let mut ttype;

        if let Some(tok_type) = lookup {
            ttype = tok_type;

            if ttype == TokenKind::Try {
                if let Some(ch) = self.cur() {
                    if ch == '!' || ch == '?' {
                        self.read_char();

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

    fn read_char_literal(&mut self) -> Result<Token, MsgWithPos> {
        let pos = self.reader.pos();

        self.read_char();
        let ch = self.read_escaped_char(pos, Msg::UnclosedChar)?;

        if is_char_quote(self.cur()) {
            self.read_char();

            let ttype = TokenKind::LitChar(ch);
            Ok(Token::new(ttype, pos))
        } else {
            Err(MsgWithPos::new(self.reader.path().to_string(), pos, Msg::UnclosedChar))
        }
    }

    fn read_escaped_char(&mut self, pos: Position, unclosed: Msg) -> Result<char, MsgWithPos> {
        if let Some(ch) = self.cur() {
            self.read_char();

            if ch == '\\' {
                let ch = if let Some(ch) = self.cur() {
                    ch
                } else {
                    return Err(MsgWithPos::new(self.reader.path().to_string(), pos, unclosed));
                };

                self.read_char();

                match ch {
                    '\\' => Ok('\\'),
                    'n' => Ok('\n'),
                    't' => Ok('\t'),
                    'r' => Ok('\r'),
                    '\"' => Ok('\"'),
                    '\'' => Ok('\''),
                    '0' => Ok('\0'),
                    _ => {
                        let msg = Msg::InvalidEscapeSequence(ch);
                        Err(MsgWithPos::new(self.reader.path().to_string(), pos, msg))
                    }
                }

            } else {
                Ok(ch)
            }

        } else {
            Err(MsgWithPos::new(self.reader.path().to_string(), pos, unclosed))
        }
    }

    fn read_string(&mut self) -> Result<Token, MsgWithPos> {
        let pos = self.reader.pos();
        let mut value = String::new();

        self.read_char();

        while !self.cur().is_none() && !is_quote(self.cur()) {
            let ch = self.read_escaped_char(pos, Msg::UnclosedString)?;
            value.push(ch);
        }

        if is_quote(self.cur()) {
            self.read_char();

            let ttype = TokenKind::String(value);
            Ok(Token::new(ttype, pos))

        } else {
            Err(MsgWithPos::new(self.reader.path().to_string(), pos, Msg::UnclosedString))
        }
    }

    fn read_operator(&mut self) -> Result<Token, MsgWithPos> {
        let mut tok = self.build_token(TokenKind::End);
        let ch = self.cur().unwrap();
        self.read_char();

        let nch = self.cur().unwrap_or('x');
        let nnch = self.next().unwrap_or('x');

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
            ':' => {
                if nch == ':' {
                    self.read_char();
                    TokenKind::Sep
                } else {
                    TokenKind::Colon
                }
            }
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
                return Err(MsgWithPos::new(self.reader.path().to_string(), tok.position, Msg::UnknownChar(ch)));
            }
        };

        Ok(tok)
    }

    fn read_number(&mut self) -> Result<Token, MsgWithPos> {
        let pos = self.reader.pos();
        let mut value = String::new();

        let base = if self.cur() == Some('0') {
            let next = self.next();

            match next {
                Some('x') => {
                    self.read_char();
                    self.read_char();

                    IntBase::Hex
                }

                Some('b') => {
                    self.read_char();
                    self.read_char();

                    IntBase::Bin
                }

                _ => IntBase::Dec,
            }
        } else {
            IntBase::Dec
        };

        self.read_digits(&mut value, base);

        if base == IntBase::Dec && self.cur() == Some('.') && is_digit(self.next()) {
            self.read_char();
            value.push('.');

            self.read_digits(&mut value, IntBase::Dec);

            if self.cur() == Some('e') || self.cur() == Some('E') {
                value.push(self.cur().unwrap());
                self.read_char();

                if self.cur() == Some('+') || self.cur() == Some('-') {
                    value.push(self.cur().unwrap());
                    self.read_char();
                }

                self.read_digits(&mut value, IntBase::Dec);
            }

            let suffix = match self.cur() {
                Some('D') => {
                    self.read_char();
                    FloatSuffix::Double
                }

                Some('F') => {
                    self.read_char();
                    FloatSuffix::Float
                }

                _ => FloatSuffix::Double,
            };

            let ttype = TokenKind::LitFloat(value, suffix);
            return Ok(Token::new(ttype, pos));
        }

        let suffix = match self.cur() {
            Some('L') => {
                self.read_char();
                IntSuffix::Long
            }

            Some('Y') => {
                self.read_char();
                IntSuffix::Byte
            }

            Some('D') if base == IntBase::Dec => {
                self.read_char();

                let ttype = TokenKind::LitFloat(value, FloatSuffix::Double);
                return Ok(Token::new(ttype, pos));
            }

            Some('F') if base == IntBase::Dec => {
                self.read_char();

                let ttype = TokenKind::LitFloat(value, FloatSuffix::Float);
                return Ok(Token::new(ttype, pos));
            }

            _ => IntSuffix::Int,
        };

        let ttype = TokenKind::LitInt(value, base, suffix);
        Ok(Token::new(ttype, pos))
    }

    fn read_digits(&mut self, buffer: &mut String, base: IntBase) {
        while is_digit_or_underscore(self.cur(), base) {
            let ch = self.cur().unwrap();
            self.read_char();
            buffer.push(ch);
        }
    }

    fn read_char(&mut self) {
        self.reader.advance();
    }

    fn cur(&self) -> Option<char> {
        self.reader.cur()
    }

    fn next(&self) -> Option<char> {
        self.reader.next()
    }

    fn build_token(&self, kind: TokenKind) -> Token {
        Token::new(kind, self.reader.pos())
    }

    fn is_comment_start(&self) -> bool {
        self.cur() == Some('/') && self.next() == Some('/')
    }

    fn is_multi_comment_start(&self) -> bool {
        self.cur() == Some('/') && self.next() == Some('*')
    }

    fn is_multi_comment_end(&self) -> bool {
        self.cur() == Some('*') && self.next() == Some('/')
    }
}

fn is_digit(ch: Option<char>) -> bool {
    ch.map(|ch| ch.is_digit(10)).unwrap_or(false)
}

fn is_digit_or_underscore(ch: Option<char>, base: IntBase) -> bool {
    ch.map(|ch| ch.is_digit(base.num()) || ch == '_')
        .unwrap_or(false)
}

fn is_whitespace(ch: Option<char>) -> bool {
    ch.map(|ch| ch.is_whitespace()).unwrap_or(false)
}

fn is_newline(ch: Option<char>) -> bool {
    ch == Some('\n')
}

fn is_quote(ch: Option<char>) -> bool {
    ch == Some('\"')
}

fn is_char_quote(ch: Option<char>) -> bool {
    ch == Some('\'')
}

fn is_operator(ch: Option<char>) -> bool {
    ch.map(|ch| "^+-*/%&|,=!~;:.()[]{}<>".contains(ch))
        .unwrap_or(false)
}

fn is_identifier_start(ch: Option<char>) -> bool {
    match ch {
        Some(ch) => (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_',
        _ => false,
    }
}

fn is_identifier(ch: Option<char>) -> bool {
    is_identifier_start(ch) || is_digit(ch)
}

fn keywords_in_map() -> HashMap<&'static str, TokenKind> {
    let mut keywords = HashMap::new();

    keywords.insert("self", TokenKind::This);
    keywords.insert("Self", TokenKind::CapitalThis);
    keywords.insert("super", TokenKind::Super);
    keywords.insert("fun", TokenKind::Fun);
    keywords.insert("let", TokenKind::Let);
    keywords.insert("var", TokenKind::Var);
    keywords.insert("while", TokenKind::While);
    keywords.insert("if", TokenKind::If);
    keywords.insert("else", TokenKind::Else);
    keywords.insert("for", TokenKind::For);
    keywords.insert("in", TokenKind::In);
    keywords.insert("impl", TokenKind::Impl);
    keywords.insert("loop", TokenKind::Loop);
    keywords.insert("break", TokenKind::Break);
    keywords.insert("continue", TokenKind::Continue);
    keywords.insert("return", TokenKind::Return);
    keywords.insert("true", TokenKind::True);
    keywords.insert("false", TokenKind::False);
    keywords.insert("nil", TokenKind::Nil);
    keywords.insert("class", TokenKind::Class);
    keywords.insert("module", TokenKind::Module);
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
    keywords.insert("abstract", TokenKind::Abstract);
    keywords.insert("open", TokenKind::Open);
    keywords.insert("override", TokenKind::Override);
    keywords.insert("defer", TokenKind::Defer);
    keywords.insert("final", TokenKind::Final);
    keywords.insert("is", TokenKind::Is);
    keywords.insert("as", TokenKind::As);
    keywords.insert("internal", TokenKind::Internal);
    keywords.insert("optimize", TokenKind::Optimize);
    keywords.insert("pub", TokenKind::Pub);
    keywords.insert("static", TokenKind::Static);
    keywords.insert("spawn", TokenKind::Spawn);
    keywords.insert("const", TokenKind::Const);

    keywords
}

#[cfg(test)]
mod tests {
    use super::*;
    use error::msg::Msg;
    use lexer::reader::Reader;
    use lexer::token::TokenKind;

    fn assert_end(reader: &mut Lexer, l: u32, c: u32) {
        assert_tok(reader, TokenKind::End, l, c);
    }

    fn assert_tok(reader: &mut Lexer, kind: TokenKind, l: u32, c: u32) {
        let tok = reader.read_token().unwrap();
        assert_eq!(kind, tok.kind);
        assert_eq!(l, tok.position.line);
        assert_eq!(c, tok.position.column);
    }

    fn assert_err(reader: &mut Lexer, msg: Msg, l: u32, c: u32) {
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
        assert_tok(&mut reader,
                   TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::Int),
                   1,
                   1);
        assert_tok(&mut reader,
                   TokenKind::LitInt("2".into(), IntBase::Dec, IntSuffix::Int),
                   1,
                   3);
        assert_tok(&mut reader,
                   TokenKind::LitInt("0123".into(), IntBase::Dec, IntSuffix::Int),
                   2,
                   1);
        assert_tok(&mut reader,
                   TokenKind::LitInt("10".into(), IntBase::Dec, IntSuffix::Int),
                   2,
                   6);
        assert_end(&mut reader, 2, 8);

        let mut reader = Lexer::from_str("12Y 300Y 1_000 1__1");
        assert_tok(&mut reader,
                   TokenKind::LitInt("12".into(), IntBase::Dec, IntSuffix::Byte),
                   1,
                   1);
        assert_tok(&mut reader,
                   TokenKind::LitInt("300".into(), IntBase::Dec, IntSuffix::Byte),
                   1,
                   5);
        assert_tok(&mut reader,
                   TokenKind::LitInt("1_000".into(), IntBase::Dec, IntSuffix::Int),
                   1,
                   10);
        assert_tok(&mut reader,
                   TokenKind::LitInt("1__1".into(), IntBase::Dec, IntSuffix::Int),
                   1,
                   16);
    }

    #[test]
    fn test_skip_single_line_comment() {
        let mut reader = Lexer::from_str("//test\n1");
        assert_tok(&mut reader,
                   TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::Int),
                   2,
                   1);
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
        assert_tok(&mut reader,
                   TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::Int),
                   1,
                   9);
        assert_end(&mut reader, 1, 10);
    }

    #[test]
    fn test_unfinished_multi_comment() {
        let mut reader = Lexer::from_str("/*test");
        assert_err(&mut reader, Msg::UnclosedComment, 1, 1);

        let mut reader = Lexer::from_str("1/*test");
        assert_tok(&mut reader,
                   TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::Int),
                   1,
                   1);
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
        assert_tok(&mut reader,
                   TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::Int),
                   1,
                   1);
        assert_tok(&mut reader,
                   TokenKind::LitInt("2".into(), IntBase::Dec, IntSuffix::Int),
                   1,
                   3);
        assert_tok(&mut reader,
                   TokenKind::LitInt("3".into(), IntBase::Dec, IntSuffix::Int),
                   1,
                   5);
        assert_end(&mut reader, 1, 6);
    }

    #[test]
    fn test_float_numbers() {
        let mut reader = Lexer::from_str("1F 1.0 0.1F 1.3D 4D");
        assert_tok(&mut reader,
                   TokenKind::LitFloat("1".into(), FloatSuffix::Float),
                   1,
                   1);
        assert_tok(&mut reader,
                   TokenKind::LitFloat("1.0".into(), FloatSuffix::Double),
                   1,
                   4);
        assert_tok(&mut reader,
                   TokenKind::LitFloat("0.1".into(), FloatSuffix::Float),
                   1,
                   8);
        assert_tok(&mut reader,
                   TokenKind::LitFloat("1.3".into(), FloatSuffix::Double),
                   1,
                   13);
        assert_tok(&mut reader,
                   TokenKind::LitFloat("4".into(), FloatSuffix::Double),
                   1,
                   18);
    }

    #[test]
    fn test_float_scientific_notation() {
        let mut reader = Lexer::from_str("1.0e1 1.0E1 1.0e+1 1.0e-1");
        assert_tok(&mut reader,
                   TokenKind::LitFloat("1.0e1".into(), FloatSuffix::Double),
                   1,
                   1);
        assert_tok(&mut reader,
                   TokenKind::LitFloat("1.0E1".into(), FloatSuffix::Double),
                   1,
                   7);
        assert_tok(&mut reader,
                   TokenKind::LitFloat("1.0e+1".into(), FloatSuffix::Double),
                   1,
                   13);
        assert_tok(&mut reader,
                   TokenKind::LitFloat("1.0e-1".into(), FloatSuffix::Double),
                   1,
                   20);
    }

    #[test]
    fn test_hex_numbers() {
        let mut reader = Lexer::from_str("0x1 0x2L 0xABCDEF 0xB1L");

        assert_tok(&mut reader,
                   TokenKind::LitInt("1".into(), IntBase::Hex, IntSuffix::Int),
                   1,
                   1);
        assert_tok(&mut reader,
                   TokenKind::LitInt("2".into(), IntBase::Hex, IntSuffix::Long),
                   1,
                   5);
        assert_tok(&mut reader,
                   TokenKind::LitInt("ABCDEF".into(), IntBase::Hex, IntSuffix::Int),
                   1,
                   10);
        assert_tok(&mut reader,
                   TokenKind::LitInt("B1".into(), IntBase::Hex, IntSuffix::Long),
                   1,
                   19);
    }

    #[test]
    fn test_code_with_newlines() {
        let mut reader = Lexer::from_str("1\n2\n3");
        assert_tok(&mut reader,
                   TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::Int),
                   1,
                   1);
        assert_tok(&mut reader,
                   TokenKind::LitInt("2".into(), IntBase::Dec, IntSuffix::Int),
                   2,
                   1);
        assert_tok(&mut reader,
                   TokenKind::LitInt("3".into(), IntBase::Dec, IntSuffix::Int),
                   3,
                   1);
        assert_end(&mut reader, 3, 2);
    }

    #[test]
    fn test_code_with_tabs() {
        let mut reader = Lexer::from_str("1\t2\t3");
        assert_tok(&mut reader,
                   TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::Int),
                   1,
                   1);
        assert_tok(&mut reader,
                   TokenKind::LitInt("2".into(), IntBase::Dec, IntSuffix::Int),
                   1,
                   5);
        assert_tok(&mut reader,
                   TokenKind::LitInt("3".into(), IntBase::Dec, IntSuffix::Int),
                   1,
                   9);
        assert_end(&mut reader, 1, 10);
    }

    #[test]
    fn test_code_with_tabwidth8() {
        let mut reader = Reader::from_string("1\t2\n1234567\t8\n12345678\t9");
        reader.set_tabwidth(8);
        let mut reader = Lexer::new(reader);

        assert_tok(&mut reader,
                   TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::Int),
                   1,
                   1);
        assert_tok(&mut reader,
                   TokenKind::LitInt("2".into(), IntBase::Dec, IntSuffix::Int),
                   1,
                   9);
        assert_tok(&mut reader,
                   TokenKind::LitInt("1234567".into(), IntBase::Dec, IntSuffix::Int),
                   2,
                   1);
        assert_tok(&mut reader,
                   TokenKind::LitInt("8".into(), IntBase::Dec, IntSuffix::Int),
                   2,
                   9);
        assert_tok(&mut reader,
                   TokenKind::LitInt("12345678".into(), IntBase::Dec, IntSuffix::Int),
                   3,
                   1);
        assert_tok(&mut reader,
                   TokenKind::LitInt("9".into(), IntBase::Dec, IntSuffix::Int),
                   3,
                   17);
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
    fn test_unclosed_char() {
        let mut reader = Lexer::from_str("'a");
        assert_err(&mut reader, Msg::UnclosedChar, 1, 1);

        let mut reader = Lexer::from_str("'\\");
        assert_err(&mut reader, Msg::UnclosedChar, 1, 1);

        let mut reader = Lexer::from_str("'\\n");
        assert_err(&mut reader, Msg::UnclosedChar, 1, 1);

        let mut reader = Lexer::from_str("'ab'");
        assert_err(&mut reader, Msg::UnclosedChar, 1, 1);

        let mut reader = Lexer::from_str("'");
        assert_err(&mut reader, Msg::UnclosedChar, 1, 1);
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

        let mut reader = Lexer::from_str("type struct enum alias trait const");
        assert_tok(&mut reader, TokenKind::Type, 1, 1);
        assert_tok(&mut reader, TokenKind::Struct, 1, 6);
        assert_tok(&mut reader, TokenKind::Enum, 1, 13);
        assert_tok(&mut reader, TokenKind::Alias, 1, 18);
        assert_tok(&mut reader, TokenKind::Trait, 1, 24);
        assert_tok(&mut reader, TokenKind::Const, 1, 30);

        let mut reader = Lexer::from_str("pub static for in impl Self spawn");
        assert_tok(&mut reader, TokenKind::Pub, 1, 1);
        assert_tok(&mut reader, TokenKind::Static, 1, 5);
        assert_tok(&mut reader, TokenKind::For, 1, 12);
        assert_tok(&mut reader, TokenKind::In, 1, 16);
        assert_tok(&mut reader, TokenKind::Impl, 1, 19);
        assert_tok(&mut reader, TokenKind::CapitalThis, 1, 24);
        assert_tok(&mut reader, TokenKind::Spawn, 1, 29);

        let mut reader = Lexer::from_str("abstract open override defer");
        assert_tok(&mut reader, TokenKind::Abstract, 1, 1);
        assert_tok(&mut reader, TokenKind::Open, 1, 10);
        assert_tok(&mut reader, TokenKind::Override, 1, 15);
        assert_tok(&mut reader, TokenKind::Defer, 1, 24);
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
        assert_tok(&mut reader,
                   TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::Int),
                   1,
                   9);

        let mut reader = Lexer::from_str(">><<>>>_::");
        assert_tok(&mut reader, TokenKind::GtGt, 1, 1);
        assert_tok(&mut reader, TokenKind::LtLt, 1, 3);
        assert_tok(&mut reader, TokenKind::GtGtGt, 1, 5);
        assert_tok(&mut reader, TokenKind::Underscore, 1, 8);
        assert_tok(&mut reader, TokenKind::Sep, 1, 9);
    }
}
