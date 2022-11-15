use std::collections::HashMap;

use crate::error::{ParseError, ParseErrorAndPos};
use crate::lexer::position::{Position, Span};
use crate::lexer::reader::Reader;
use crate::lexer::token::{FloatSuffix, IntBase, IntSuffix, Token, TokenKind};

pub mod position;
pub mod reader;
pub mod token;

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

        Lexer { reader, keywords }
    }

    pub fn read_token(&mut self) -> Result<Token, ParseErrorAndPos> {
        loop {
            self.skip_white();

            let pos = self.reader.pos();
            let idx = self.reader.idx();
            let ch = self.curr();

            if let None = ch {
                return Ok(Token::new(TokenKind::End, pos, Span::at(idx)));
            }

            if is_digit(ch) {
                return self.read_number();
            } else if self.is_comment_start() {
                self.read_comment()?;
            } else if self.is_multi_comment_start() {
                self.read_multi_comment()?;
            } else if is_identifier_start(ch) {
                return self.read_identifier();
            } else if is_quote(ch) {
                return self.read_string(true);
            } else if is_char_quote(ch) {
                return self.read_char_literal();
            } else if is_operator(ch) {
                return self.read_operator();
            } else {
                let ch = ch.unwrap();

                return Err(ParseErrorAndPos::new(pos, ParseError::UnknownChar(ch)));
            }
        }
    }

    // The only intended use of this function is for semicolon inference.
    // The language should at no point require additional lookahead.
    pub fn peek_next_token(&mut self) -> Result<Token, ParseErrorAndPos> {
        let idx = self.reader.idx();
        let pos = self.reader.pos();
        let token_next = self.read_token();
        self.reader.set(idx, pos);
        token_next
    }

    fn skip_white(&mut self) {
        while is_whitespace(self.curr()) {
            self.read_char();
        }
    }

    fn read_comment(&mut self) -> Result<(), ParseErrorAndPos> {
        while !self.curr().is_none() && !is_newline(self.curr()) {
            self.read_char();
        }

        Ok(())
    }

    fn read_multi_comment(&mut self) -> Result<(), ParseErrorAndPos> {
        let pos = self.reader.pos();

        self.read_char();
        self.read_char();

        while !self.curr().is_none() && !self.is_multi_comment_end() {
            self.read_char();
        }

        if self.curr().is_none() {
            return Err(ParseErrorAndPos::new(pos, ParseError::UnclosedComment));
        }

        self.read_char();
        self.read_char();

        Ok(())
    }

    fn read_identifier(&mut self) -> Result<Token, ParseErrorAndPos> {
        let pos = self.reader.pos();
        let idx = self.reader.idx();
        let value = self.read_identifier_as_string();

        let lookup = self.keywords.get(&value[..]).cloned();
        let ttype = if let Some(tok_type) = lookup {
            tok_type
        } else if value == "_" {
            TokenKind::Underscore
        } else {
            TokenKind::Identifier(value)
        };

        let span = self.span_from(idx);
        Ok(Token::new(ttype, pos, span))
    }

    fn read_identifier_as_string(&mut self) -> String {
        let mut value = String::new();

        while is_identifier(self.curr()) {
            let ch = self.curr().unwrap();
            self.read_char();
            value.push(ch);
        }

        value
    }

    fn read_char_literal(&mut self) -> Result<Token, ParseErrorAndPos> {
        let pos = self.reader.pos();
        let idx = self.reader.idx();

        self.read_char();
        let ch = self.read_escaped_char(pos, ParseError::UnclosedChar)?;

        if is_char_quote(self.curr()) {
            self.read_char();

            let ttype = TokenKind::LitChar(ch);
            let span = self.span_from(idx);
            Ok(Token::new(ttype, pos, span))
        } else {
            Err(ParseErrorAndPos::new(pos, ParseError::UnclosedChar))
        }
    }

    fn read_escaped_char(
        &mut self,
        pos: Position,
        unclosed: ParseError,
    ) -> Result<char, ParseErrorAndPos> {
        if let Some(ch) = self.curr() {
            self.read_char();

            if ch == '\\' {
                let ch = if let Some(ch) = self.curr() {
                    ch
                } else {
                    return Err(ParseErrorAndPos::new(pos, unclosed));
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
                    '$' => Ok('$'),
                    _ => {
                        let msg = ParseError::InvalidEscapeSequence(ch);
                        Err(ParseErrorAndPos::new(pos, msg))
                    }
                }
            } else {
                Ok(ch)
            }
        } else {
            Err(ParseErrorAndPos::new(pos, unclosed))
        }
    }

    fn read_string(&mut self, skip_quote: bool) -> Result<Token, ParseErrorAndPos> {
        let pos = self.reader.pos();
        let idx = self.reader.idx();
        let mut value = String::new();

        if skip_quote {
            assert_eq!(self.curr(), Some('\"'));
            self.read_char();
        }

        while self.curr().is_some() && !is_quote(self.curr()) {
            if self.curr() == Some('$') && self.next() == Some('{') {
                self.read_char();
                self.read_char();

                let ttype = TokenKind::StringExpr(value);
                let span = self.span_from(idx);
                return Ok(Token::new(ttype, pos, span));
            }

            let ch = self.read_escaped_char(pos, ParseError::UnclosedString)?;
            value.push(ch);
        }

        if is_quote(self.curr()) {
            self.read_char();

            let ttype = TokenKind::StringTail(value);
            let span = self.span_from(idx);
            Ok(Token::new(ttype, pos, span))
        } else {
            Err(ParseErrorAndPos::new(pos, ParseError::UnclosedString))
        }
    }

    pub fn read_string_continuation(&mut self) -> Result<Token, ParseErrorAndPos> {
        self.read_string(false)
    }

    fn read_operator(&mut self) -> Result<Token, ParseErrorAndPos> {
        let pos = self.reader.pos();
        let idx = self.reader.idx();
        let ch = self.curr().unwrap();
        self.read_char();

        let nch = self.curr().unwrap_or('x');
        let nnch = self.next().unwrap_or('x');

        let kind = match ch {
            '+' => TokenKind::Add,
            '-' => TokenKind::Sub,
            '*' => TokenKind::Mul,
            '/' => TokenKind::Div,

            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,

            '|' => {
                if nch == '|' {
                    self.read_char();
                    TokenKind::OrOr
                } else {
                    TokenKind::Or
                }
            }

            '&' => {
                if nch == '&' {
                    self.read_char();
                    TokenKind::AndAnd
                } else {
                    TokenKind::And
                }
            }

            '^' => TokenKind::Caret,
            ',' => TokenKind::Comma,
            ';' => TokenKind::Semicolon,
            ':' => {
                if nch == ':' {
                    self.read_char();
                    TokenKind::ColonColon
                } else {
                    TokenKind::Colon
                }
            }
            '.' => {
                if nch == '.' && nnch == '.' {
                    self.read_char();
                    self.read_char();

                    TokenKind::DotDotDot
                } else {
                    TokenKind::Dot
                }
            }
            '=' => {
                if nch == '=' {
                    self.read_char();

                    if nnch == '=' {
                        self.read_char();
                        TokenKind::EqEqEq
                    } else {
                        TokenKind::EqEq
                    }
                } else if nch == '>' {
                    self.read_char();
                    TokenKind::DoubleArrow
                } else {
                    TokenKind::Eq
                }
            }

            '<' => match nch {
                '=' => {
                    self.read_char();
                    TokenKind::Le
                }

                _ => TokenKind::Lt,
            },

            '>' => match nch {
                '=' => {
                    self.read_char();
                    TokenKind::Ge
                }

                _ => TokenKind::Gt,
            },
            '!' => {
                if nch == '=' {
                    self.read_char();

                    if nnch == '=' {
                        self.read_char();
                        TokenKind::NeEqEq
                    } else {
                        TokenKind::NotEq
                    }
                } else {
                    return Err(ParseErrorAndPos::new(pos, ParseError::UnknownChar(ch)));
                }
            }
            '@' => TokenKind::At,

            _ => {
                return Err(ParseErrorAndPos::new(pos, ParseError::UnknownChar(ch)));
            }
        };

        let span = self.span_from(idx);
        Ok(Token::new(kind, pos, span))
    }

    fn read_number(&mut self) -> Result<Token, ParseErrorAndPos> {
        let pos = self.reader.pos();
        let idx = self.reader.idx();
        let mut value = String::new();

        let base = if self.curr() == Some('0') {
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

        if base == IntBase::Dec && self.curr() == Some('.') && is_digit(self.next()) {
            return self.read_number_as_float(pos, idx, value);
        }

        let kind = if is_identifier_start(self.curr()) {
            let suffix = self.read_identifier_as_string();

            match suffix.as_str() {
                "u8" => TokenKind::LitInt(value, base, IntSuffix::UInt8),
                "i32" => TokenKind::LitInt(value, base, IntSuffix::Int32),
                "i64" => TokenKind::LitInt(value, base, IntSuffix::Int64),
                "f32" if base == IntBase::Dec => TokenKind::LitFloat(value, FloatSuffix::Float32),
                "f64" if base == IntBase::Dec => TokenKind::LitFloat(value, FloatSuffix::Float64),
                _ => {
                    return Err(ParseErrorAndPos::new(
                        pos,
                        ParseError::InvalidSuffix(suffix),
                    ));
                }
            }
        } else {
            TokenKind::LitInt(value, base, IntSuffix::None)
        };

        let span = self.span_from(idx);
        Ok(Token::new(kind, pos, span))
    }

    fn read_number_as_float(
        &mut self,
        pos: Position,
        idx: u32,
        mut value: String,
    ) -> Result<Token, ParseErrorAndPos> {
        self.read_char();
        value.push('.');

        self.read_digits(&mut value, IntBase::Dec);

        if self.curr() == Some('e') || self.curr() == Some('E') {
            value.push(self.curr().unwrap());
            self.read_char();

            if self.curr() == Some('+') || self.curr() == Some('-') {
                value.push(self.curr().unwrap());
                self.read_char();
            }

            self.read_digits(&mut value, IntBase::Dec);
        }

        let suffix = if is_identifier_start(self.curr()) {
            let suffix = self.read_identifier_as_string();

            match suffix.as_str() {
                "f32" => FloatSuffix::Float32,
                "f64" => FloatSuffix::Float64,
                _ => {
                    return Err(ParseErrorAndPos::new(
                        pos,
                        ParseError::InvalidSuffix(suffix),
                    ));
                }
            }
        } else {
            FloatSuffix::Float64
        };

        let ttype = TokenKind::LitFloat(value, suffix);
        let span = self.span_from(idx);
        return Ok(Token::new(ttype, pos, span));
    }

    fn span_from(&self, start: u32) -> Span {
        Span::new(start, self.reader.idx() - start)
    }

    fn read_digits(&mut self, buffer: &mut String, base: IntBase) {
        while is_digit_or_separator(self.curr(), base) {
            let ch = self.curr().unwrap();
            self.read_char();
            buffer.push(ch);
        }
    }

    fn read_char(&mut self) {
        self.reader.advance();
    }

    fn curr(&self) -> Option<char> {
        self.reader.curr()
    }

    fn next(&self) -> Option<char> {
        self.reader.nth(1)
    }

    fn is_comment_start(&self) -> bool {
        self.curr() == Some('/') && self.next() == Some('/')
    }

    fn is_multi_comment_start(&self) -> bool {
        self.curr() == Some('/') && self.next() == Some('*')
    }

    fn is_multi_comment_end(&self) -> bool {
        self.curr() == Some('*') && self.next() == Some('/')
    }
}

fn is_digit(ch: Option<char>) -> bool {
    ch.map(|ch| ch.is_digit(10)).unwrap_or(false)
}

fn is_digit_or_separator(ch: Option<char>, base: IntBase) -> bool {
    ch.map(|ch| ch.is_digit(base.num()) || ch == '\'')
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
    ch.map(|ch| "^+-*/&|,=!~;:.()[]{}<>@".contains(ch))
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
    let mut keywords = HashMap::with_capacity(30);

    // literals
    keywords.insert("true", TokenKind::True);
    keywords.insert("false", TokenKind::False);

    // "big" shapes
    keywords.insert("class", TokenKind::Class);
    keywords.insert("enum", TokenKind::Enum);
    keywords.insert("struct", TokenKind::Struct);
    keywords.insert("trait", TokenKind::Trait);
    keywords.insert("impl", TokenKind::Impl);
    keywords.insert("annotation", TokenKind::Annotation);
    keywords.insert("mod", TokenKind::Mod);
    keywords.insert("use", TokenKind::Use);
    keywords.insert("package", TokenKind::Package);

    // "small" shapes
    keywords.insert("fun", TokenKind::Fun);
    keywords.insert("let", TokenKind::Let);
    keywords.insert("var", TokenKind::Var);
    keywords.insert("const", TokenKind::Const);

    // control flow
    keywords.insert("return", TokenKind::Return);
    keywords.insert("if", TokenKind::If);
    keywords.insert("else", TokenKind::Else);
    keywords.insert("while", TokenKind::While);
    keywords.insert("for", TokenKind::For);
    keywords.insert("in", TokenKind::In);
    keywords.insert("match", TokenKind::Match);

    // qualifiers
    keywords.insert("self", TokenKind::This);
    keywords.insert("super", TokenKind::Super);

    // casting
    keywords.insert("as", TokenKind::As);

    // unused
    keywords.insert("type", TokenKind::Type);
    keywords.insert("alias", TokenKind::Alias);
    keywords.insert("Self", TokenKind::CapitalThis);

    keywords
}

#[cfg(test)]
mod tests {
    use crate::lexer::reader::Reader;
    use crate::lexer::token::TokenKind;

    use super::*;

    fn assert_end(reader: &mut Lexer, l: u32, c: u32) {
        assert_tok(reader, TokenKind::End, l, c);
    }

    fn assert_tok(reader: &mut Lexer, kind: TokenKind, l: u32, c: u32) {
        let tok = reader.read_token().unwrap();
        assert_eq!(kind, tok.kind);
        assert_eq!(l, tok.position.line);
        assert_eq!(c, tok.position.column);
    }

    fn assert_peek(reader: &mut Lexer, kind: TokenKind, l: u32, c: u32) {
        let tok = reader.peek_next_token().unwrap();
        assert_eq!(kind, tok.kind);
        assert_eq!(l, tok.position.line);
        assert_eq!(c, tok.position.column);
    }

    fn assert_err(reader: &mut Lexer, msg: ParseError, l: u32, c: u32) {
        let err = reader.read_token().unwrap_err();
        assert_eq!(msg, err.error);
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
    fn test_peek() {
        let mut reader = Lexer::from_str("a b c");
        assert_peek(&mut reader, TokenKind::Identifier("a".into()), 1, 1);
        assert_peek(&mut reader, TokenKind::Identifier("a".into()), 1, 1);
        assert_tok(&mut reader, TokenKind::Identifier("a".into()), 1, 1);
        assert_peek(&mut reader, TokenKind::Identifier("b".into()), 1, 3);
        assert_tok(&mut reader, TokenKind::Identifier("b".into()), 1, 3);
    }

    #[test]
    fn test_read_numbers() {
        let mut reader = Lexer::from_str("1 2\n0123 10");
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::None),
            1,
            1,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("2".into(), IntBase::Dec, IntSuffix::None),
            1,
            3,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("0123".into(), IntBase::Dec, IntSuffix::None),
            2,
            1,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("10".into(), IntBase::Dec, IntSuffix::None),
            2,
            6,
        );
        assert_end(&mut reader, 2, 8);

        let mut reader = Lexer::from_str("12u8 300u8 1'000 1''1");
        assert_tok(
            &mut reader,
            TokenKind::LitInt("12".into(), IntBase::Dec, IntSuffix::UInt8),
            1,
            1,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("300".into(), IntBase::Dec, IntSuffix::UInt8),
            1,
            6,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1'000".into(), IntBase::Dec, IntSuffix::None),
            1,
            12,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1''1".into(), IntBase::Dec, IntSuffix::None),
            1,
            18,
        );
    }

    #[test]
    fn test_read_numbers_with_suffix() {
        let mut reader = Lexer::from_str("1i32 2u8 3i64");
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::Int32),
            1,
            1,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("2".into(), IntBase::Dec, IntSuffix::UInt8),
            1,
            6,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("3".into(), IntBase::Dec, IntSuffix::Int64),
            1,
            10,
        );
        assert_end(&mut reader, 1, 14);
    }

    #[test]
    fn test_skip_single_line_comment() {
        let mut reader = Lexer::from_str("//test\n1");
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::None),
            2,
            1,
        );
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
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::None),
            1,
            9,
        );
        assert_end(&mut reader, 1, 10);
    }

    #[test]
    fn test_unfinished_multi_comment() {
        let mut reader = Lexer::from_str("/*test");
        assert_err(&mut reader, ParseError::UnclosedComment, 1, 1);

        let mut reader = Lexer::from_str("1/*test");
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::None),
            1,
            1,
        );
        assert_err(&mut reader, ParseError::UnclosedComment, 1, 2);
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
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::None),
            1,
            1,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("2".into(), IntBase::Dec, IntSuffix::None),
            1,
            3,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("3".into(), IntBase::Dec, IntSuffix::None),
            1,
            5,
        );
        assert_end(&mut reader, 1, 6);
    }

    #[test]
    fn test_float_numbers() {
        let mut reader = Lexer::from_str("1f32 1.0 0.1f32 1.3f64 4f64");
        assert_tok(
            &mut reader,
            TokenKind::LitFloat("1".into(), FloatSuffix::Float32),
            1,
            1,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitFloat("1.0".into(), FloatSuffix::Float64),
            1,
            6,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitFloat("0.1".into(), FloatSuffix::Float32),
            1,
            10,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitFloat("1.3".into(), FloatSuffix::Float64),
            1,
            17,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitFloat("4".into(), FloatSuffix::Float64),
            1,
            24,
        );
    }

    #[test]
    fn test_float_scientific_notation() {
        let mut reader = Lexer::from_str("1.0e1 1.0E1 1.0e+1 1.0e-1");
        assert_tok(
            &mut reader,
            TokenKind::LitFloat("1.0e1".into(), FloatSuffix::Float64),
            1,
            1,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitFloat("1.0E1".into(), FloatSuffix::Float64),
            1,
            7,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitFloat("1.0e+1".into(), FloatSuffix::Float64),
            1,
            13,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitFloat("1.0e-1".into(), FloatSuffix::Float64),
            1,
            20,
        );
    }

    #[test]
    fn test_hex_numbers() {
        let mut reader = Lexer::from_str("0x1 0x2i64 0xABCDEF 0xB1i64");

        assert_tok(
            &mut reader,
            TokenKind::LitInt("1".into(), IntBase::Hex, IntSuffix::None),
            1,
            1,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("2".into(), IntBase::Hex, IntSuffix::Int64),
            1,
            5,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("ABCDEF".into(), IntBase::Hex, IntSuffix::None),
            1,
            12,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("B1".into(), IntBase::Hex, IntSuffix::Int64),
            1,
            21,
        );
    }

    #[test]
    fn test_code_with_newlines() {
        let mut reader = Lexer::from_str("1\n2\n3");
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::None),
            1,
            1,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("2".into(), IntBase::Dec, IntSuffix::None),
            2,
            1,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("3".into(), IntBase::Dec, IntSuffix::None),
            3,
            1,
        );
        assert_end(&mut reader, 3, 2);
    }

    #[test]
    fn test_code_with_tabs() {
        let mut reader = Lexer::from_str("1\t2\t3");
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::None),
            1,
            1,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("2".into(), IntBase::Dec, IntSuffix::None),
            1,
            5,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("3".into(), IntBase::Dec, IntSuffix::None),
            1,
            9,
        );
        assert_end(&mut reader, 1, 10);
    }

    #[test]
    fn test_code_with_tabwidth8() {
        let mut reader = Reader::from_string("1\t2\n1234567\t8\n12345678\t9");
        reader.set_tabwidth(8);
        let mut reader = Lexer::new(reader);

        assert_tok(
            &mut reader,
            TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::None),
            1,
            1,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("2".into(), IntBase::Dec, IntSuffix::None),
            1,
            9,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1234567".into(), IntBase::Dec, IntSuffix::None),
            2,
            1,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("8".into(), IntBase::Dec, IntSuffix::None),
            2,
            9,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("12345678".into(), IntBase::Dec, IntSuffix::None),
            3,
            1,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("9".into(), IntBase::Dec, IntSuffix::None),
            3,
            17,
        );
        assert_end(&mut reader, 3, 18);
    }

    #[test]
    fn test_string_with_newline() {
        let mut reader = Lexer::from_str("\"abc\ndef\"");
        assert_tok(&mut reader, TokenKind::StringTail("abc\ndef".into()), 1, 1);
    }

    #[test]
    fn test_escape_sequences() {
        let mut reader = Lexer::from_str("\"\\\"\"");
        assert_tok(&mut reader, TokenKind::StringTail("\"".into()), 1, 1);

        let mut reader = Lexer::from_str("\"\\$\"");
        assert_tok(&mut reader, TokenKind::StringTail("$".into()), 1, 1);

        let mut reader = Lexer::from_str("\"\\\'\"");
        assert_tok(&mut reader, TokenKind::StringTail("'".into()), 1, 1);

        let mut reader = Lexer::from_str("\"\\t\"");
        assert_tok(&mut reader, TokenKind::StringTail("\t".into()), 1, 1);

        let mut reader = Lexer::from_str("\"\\n\"");
        assert_tok(&mut reader, TokenKind::StringTail("\n".into()), 1, 1);

        let mut reader = Lexer::from_str("\"\\r\"");
        assert_tok(&mut reader, TokenKind::StringTail("\r".into()), 1, 1);

        let mut reader = Lexer::from_str("\"\\\\\"");
        assert_tok(&mut reader, TokenKind::StringTail("\\".into()), 1, 1);

        let mut reader = Lexer::from_str("\"\\");
        assert_err(&mut reader, ParseError::UnclosedString, 1, 1);
    }

    #[test]
    fn test_unclosed_string() {
        let mut reader = Lexer::from_str("\"abc");
        assert_err(&mut reader, ParseError::UnclosedString, 1, 1);
    }

    #[test]
    fn test_unclosed_char() {
        let mut reader = Lexer::from_str("'a");
        assert_err(&mut reader, ParseError::UnclosedChar, 1, 1);

        let mut reader = Lexer::from_str("'\\");
        assert_err(&mut reader, ParseError::UnclosedChar, 1, 1);

        let mut reader = Lexer::from_str("'\\n");
        assert_err(&mut reader, ParseError::UnclosedChar, 1, 1);

        let mut reader = Lexer::from_str("'ab'");
        assert_err(&mut reader, ParseError::UnclosedChar, 1, 1);

        let mut reader = Lexer::from_str("'");
        assert_err(&mut reader, ParseError::UnclosedChar, 1, 1);
    }

    #[test]
    fn test_string() {
        let mut reader = Lexer::from_str("\"abc\"");
        assert_tok(&mut reader, TokenKind::StringTail("abc".into()), 1, 1);
        assert_end(&mut reader, 1, 6);
    }

    #[test]
    fn test_keywords() {
        let mut reader = Lexer::from_str("fun let var");
        assert_tok(&mut reader, TokenKind::Fun, 1, 1);
        assert_tok(&mut reader, TokenKind::Let, 1, 5);
        assert_tok(&mut reader, TokenKind::Var, 1, 9);

        let mut reader = Lexer::from_str("while if else match return");
        assert_tok(&mut reader, TokenKind::While, 1, 1);
        assert_tok(&mut reader, TokenKind::If, 1, 7);
        assert_tok(&mut reader, TokenKind::Else, 1, 10);
        assert_tok(&mut reader, TokenKind::Match, 1, 15);
        assert_tok(&mut reader, TokenKind::Return, 1, 21);

        let mut reader = Lexer::from_str("self class super mod");
        assert_tok(&mut reader, TokenKind::This, 1, 1);
        assert_tok(&mut reader, TokenKind::Class, 1, 6);
        assert_tok(&mut reader, TokenKind::Super, 1, 12);
        assert_tok(&mut reader, TokenKind::Mod, 1, 18);

        let mut reader = Lexer::from_str("type struct enum alias trait const");
        assert_tok(&mut reader, TokenKind::Type, 1, 1);
        assert_tok(&mut reader, TokenKind::Struct, 1, 6);
        assert_tok(&mut reader, TokenKind::Enum, 1, 13);
        assert_tok(&mut reader, TokenKind::Alias, 1, 18);
        assert_tok(&mut reader, TokenKind::Trait, 1, 24);
        assert_tok(&mut reader, TokenKind::Const, 1, 30);

        let mut reader = Lexer::from_str("for in impl Self");
        assert_tok(&mut reader, TokenKind::For, 1, 1);
        assert_tok(&mut reader, TokenKind::In, 1, 5);
        assert_tok(&mut reader, TokenKind::Impl, 1, 8);
        assert_tok(&mut reader, TokenKind::CapitalThis, 1, 13);
    }

    #[test]
    fn test_operators() {
        let mut reader = Lexer::from_str("==-*/.@...,");
        assert_tok(&mut reader, TokenKind::EqEq, 1, 1);
        assert_tok(&mut reader, TokenKind::Sub, 1, 3);
        assert_tok(&mut reader, TokenKind::Mul, 1, 4);
        assert_tok(&mut reader, TokenKind::Div, 1, 5);
        assert_tok(&mut reader, TokenKind::Dot, 1, 6);
        assert_tok(&mut reader, TokenKind::At, 1, 7);
        assert_tok(&mut reader, TokenKind::DotDotDot, 1, 8);
        assert_tok(&mut reader, TokenKind::Comma, 1, 11);

        let mut reader = Lexer::from_str("<=<>=><");
        assert_tok(&mut reader, TokenKind::Le, 1, 1);
        assert_tok(&mut reader, TokenKind::Lt, 1, 3);
        assert_tok(&mut reader, TokenKind::Ge, 1, 4);
        assert_tok(&mut reader, TokenKind::Gt, 1, 6);
        assert_tok(&mut reader, TokenKind::Lt, 1, 7);

        let mut reader = Lexer::from_str("!=====");
        assert_tok(&mut reader, TokenKind::NeEqEq, 1, 1);
        assert_tok(&mut reader, TokenKind::EqEqEq, 1, 4);

        let mut reader = Lexer::from_str("!=");
        assert_tok(&mut reader, TokenKind::NotEq, 1, 1);

        let mut reader = Lexer::from_str("=>");
        assert_tok(&mut reader, TokenKind::DoubleArrow, 1, 1);

        let mut reader = Lexer::from_str("_::");
        assert_tok(&mut reader, TokenKind::Underscore, 1, 1);
        assert_tok(&mut reader, TokenKind::ColonColon, 1, 2);
    }
}
