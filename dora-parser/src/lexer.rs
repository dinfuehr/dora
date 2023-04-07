use std::collections::HashMap;
use std::sync::Arc;

use crate::error::{ParseError, ParseErrorWithLocation};
use crate::lexer::token::{FloatSuffix, IntBase, IntSuffix, Token, TokenKind};
use crate::Span;

pub mod span;
pub mod token;

pub struct Lexer {
    content: Arc<String>,
    offset: usize,
    keywords: HashMap<&'static str, TokenKind>,
}

impl Lexer {
    pub fn from_str(code: &str) -> Lexer {
        Lexer::new(Arc::new(String::from(code)))
    }

    pub fn source(&self) -> Arc<String> {
        self.content.clone()
    }

    pub fn new(content: Arc<String>) -> Lexer {
        let keywords = keywords_in_map();

        Lexer {
            offset: 0,
            content,
            keywords,
        }
    }

    pub fn read_token(&mut self) -> Result<Token, ParseErrorWithLocation> {
        loop {
            self.skip_white();

            let start = self.offset();
            let ch = self.curr();

            if let None = ch {
                return Ok(Token::new(TokenKind::End, Span::at(start)));
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
                self.eat_char();
                let span = self.span_from(start);

                return Err(ParseErrorWithLocation::new(
                    span,
                    ParseError::UnknownChar(ch),
                ));
            }
        }
    }

    fn skip_white(&mut self) {
        while is_whitespace(self.curr()) {
            self.eat_char();
        }
    }

    fn read_comment(&mut self) -> Result<(), ParseErrorWithLocation> {
        while !self.curr().is_none() && !is_newline(self.curr()) {
            self.eat_char();
        }

        Ok(())
    }

    fn read_multi_comment(&mut self) -> Result<(), ParseErrorWithLocation> {
        let start = self.offset();

        self.eat_char();
        self.eat_char();

        while !self.curr().is_none() && !self.is_multi_comment_end() {
            self.eat_char();
        }

        if self.curr().is_none() {
            let span = self.span_from(start);
            return Err(ParseErrorWithLocation::new(
                span,
                ParseError::UnclosedComment,
            ));
        }

        self.eat_char();
        self.eat_char();

        Ok(())
    }

    fn read_identifier(&mut self) -> Result<Token, ParseErrorWithLocation> {
        let idx = self.offset();
        let value = self.read_identifier_as_string();

        let lookup = self.keywords.get(&value[..]).cloned();
        let ttype = if let Some(tok_type) = lookup {
            tok_type
        } else if value == "_" {
            TokenKind::Underscore
        } else {
            TokenKind::Identifier
        };

        let span = self.span_from(idx);
        Ok(Token::new(ttype, span))
    }

    fn read_identifier_as_string(&mut self) -> String {
        let mut value = String::new();

        while is_identifier(self.curr()) {
            let ch = self.curr().unwrap();
            self.eat_char();
            value.push(ch);
        }

        value
    }

    fn read_char_literal(&mut self) -> Result<Token, ParseErrorWithLocation> {
        let start = self.offset();

        self.eat_char();
        let ch = self.read_escaped_char(start, ParseError::UnclosedChar)?;

        if is_char_quote(self.curr()) {
            self.eat_char();

            let ttype = TokenKind::LitChar(ch);
            let span = self.span_from(start);
            Ok(Token::new(ttype, span))
        } else {
            let span = self.span_from(start);
            Err(ParseErrorWithLocation::new(span, ParseError::UnclosedChar))
        }
    }

    fn read_escaped_char(
        &mut self,
        start: u32,
        unclosed: ParseError,
    ) -> Result<char, ParseErrorWithLocation> {
        if let Some(ch) = self.curr() {
            self.eat_char();

            if ch == '\\' {
                let ch = if let Some(ch) = self.curr() {
                    ch
                } else {
                    let span = self.span_from(start);
                    return Err(ParseErrorWithLocation::new(span, unclosed));
                };

                self.eat_char();

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
                        let span = self.span_from(start);
                        Err(ParseErrorWithLocation::new(span, msg))
                    }
                }
            } else {
                Ok(ch)
            }
        } else {
            let span = self.span_from(start);
            Err(ParseErrorWithLocation::new(span, unclosed))
        }
    }

    fn read_string(&mut self, skip_quote: bool) -> Result<Token, ParseErrorWithLocation> {
        let start = self.offset();
        let mut value = String::new();

        if skip_quote {
            assert_eq!(self.curr(), Some('\"'));
            self.eat_char();
        }

        while self.curr().is_some() && !is_quote(self.curr()) {
            if self.curr() == Some('$') && self.next() == Some('{') {
                self.eat_char();
                self.eat_char();

                let ttype = TokenKind::StringExpr(value);
                let span = self.span_from(start);
                return Ok(Token::new(ttype, span));
            }

            let ch = self.read_escaped_char(start, ParseError::UnclosedString)?;
            value.push(ch);
        }

        if is_quote(self.curr()) {
            self.eat_char();

            let ttype = TokenKind::StringTail(value);
            let span = self.span_from(start);
            Ok(Token::new(ttype, span))
        } else {
            let span = self.span_from(start);
            Err(ParseErrorWithLocation::new(
                span,
                ParseError::UnclosedString,
            ))
        }
    }

    pub fn read_string_continuation(&mut self) -> Result<Token, ParseErrorWithLocation> {
        self.read_string(false)
    }

    fn read_operator(&mut self) -> Result<Token, ParseErrorWithLocation> {
        let start = self.offset();
        let ch = self.curr().unwrap();
        self.eat_char();

        let nch = self.curr().unwrap_or('x');
        let nnch = self.next().unwrap_or('x');

        let kind = match ch {
            '+' => TokenKind::Add,
            '-' => {
                if nch == '>' {
                    self.eat_char();
                    TokenKind::Arrow
                } else {
                    TokenKind::Sub
                }
            }
            '*' => TokenKind::Mul,
            '/' => TokenKind::Div,
            '%' => TokenKind::Modulo,

            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,

            '|' => {
                if nch == '|' {
                    self.eat_char();
                    TokenKind::OrOr
                } else {
                    TokenKind::Or
                }
            }

            '&' => {
                if nch == '&' {
                    self.eat_char();
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
                    self.eat_char();
                    TokenKind::ColonColon
                } else {
                    TokenKind::Colon
                }
            }
            '.' => {
                if nch == '.' && nnch == '.' {
                    self.eat_char();
                    self.eat_char();

                    TokenKind::DotDotDot
                } else {
                    TokenKind::Dot
                }
            }
            '=' => {
                if nch == '=' {
                    self.eat_char();

                    if nnch == '=' {
                        self.eat_char();
                        TokenKind::EqEqEq
                    } else {
                        TokenKind::EqEq
                    }
                } else if nch == '>' {
                    self.eat_char();
                    TokenKind::DoubleArrow
                } else {
                    TokenKind::Eq
                }
            }

            '<' => match nch {
                '=' => {
                    self.eat_char();
                    TokenKind::Le
                }

                '<' => {
                    self.eat_char();
                    TokenKind::LtLt
                }

                _ => TokenKind::Lt,
            },

            '>' => match nch {
                '=' => {
                    self.eat_char();
                    TokenKind::Ge
                }

                '>' => {
                    self.eat_char();

                    if nnch == '>' {
                        self.eat_char();
                        TokenKind::GtGtGt
                    } else {
                        TokenKind::GtGt
                    }
                }

                _ => TokenKind::Gt,
            },
            '!' => {
                if nch == '=' {
                    self.eat_char();

                    if nnch == '=' {
                        self.eat_char();
                        TokenKind::NeEqEq
                    } else {
                        TokenKind::NotEq
                    }
                } else {
                    TokenKind::Not
                }
            }
            '@' => TokenKind::At,

            _ => {
                self.eat_char();
                let span = self.span_from(start);
                return Err(ParseErrorWithLocation::new(
                    span,
                    ParseError::UnknownChar(ch),
                ));
            }
        };

        let span = self.span_from(start);
        Ok(Token::new(kind, span))
    }

    fn read_number(&mut self) -> Result<Token, ParseErrorWithLocation> {
        let start = self.offset();
        let mut value = String::new();

        let base = if self.curr() == Some('0') {
            let next = self.next();

            match next {
                Some('x') => {
                    self.eat_char();
                    self.eat_char();

                    IntBase::Hex
                }

                Some('b') => {
                    self.eat_char();
                    self.eat_char();

                    IntBase::Bin
                }

                _ => IntBase::Dec,
            }
        } else {
            IntBase::Dec
        };

        self.read_digits(&mut value, base);

        if base == IntBase::Dec && self.curr() == Some('.') && is_digit(self.next()) {
            return self.read_number_as_float(start, value);
        }

        let kind = if is_identifier_start(self.curr()) {
            let suffix = self.read_identifier_as_string();
            let span = self.span_from(start);

            match suffix.as_str() {
                "u8" => TokenKind::LitInt(value, base, IntSuffix::UInt8),
                "i32" => TokenKind::LitInt(value, base, IntSuffix::Int32),
                "i64" => TokenKind::LitInt(value, base, IntSuffix::Int64),
                "f32" if base == IntBase::Dec => TokenKind::LitFloat(value, FloatSuffix::Float32),
                "f64" if base == IntBase::Dec => TokenKind::LitFloat(value, FloatSuffix::Float64),
                _ => {
                    return Err(ParseErrorWithLocation::new(
                        span,
                        ParseError::InvalidSuffix(suffix),
                    ));
                }
            }
        } else {
            TokenKind::LitInt(value, base, IntSuffix::None)
        };

        let span = self.span_from(start);
        Ok(Token::new(kind, span))
    }

    fn read_number_as_float(
        &mut self,
        start: u32,
        mut value: String,
    ) -> Result<Token, ParseErrorWithLocation> {
        self.eat_char();
        value.push('.');

        self.read_digits(&mut value, IntBase::Dec);

        if self.curr() == Some('e') || self.curr() == Some('E') {
            value.push(self.curr().unwrap());
            self.eat_char();

            if self.curr() == Some('+') || self.curr() == Some('-') {
                value.push(self.curr().unwrap());
                self.eat_char();
            }

            self.read_digits(&mut value, IntBase::Dec);
        }

        let suffix = if is_identifier_start(self.curr()) {
            let suffix = self.read_identifier_as_string();
            let span = self.span_from(start);

            match suffix.as_str() {
                "f32" => FloatSuffix::Float32,
                "f64" => FloatSuffix::Float64,
                _ => {
                    return Err(ParseErrorWithLocation::new(
                        span,
                        ParseError::InvalidSuffix(suffix),
                    ));
                }
            }
        } else {
            FloatSuffix::Float64
        };

        let ttype = TokenKind::LitFloat(value, suffix);
        let span = self.span_from(start);
        return Ok(Token::new(ttype, span));
    }

    fn span_from(&self, start: u32) -> Span {
        Span::new(start, self.offset() - start)
    }

    fn read_digits(&mut self, buffer: &mut String, base: IntBase) {
        while is_digit_or_underscore(self.curr(), base) {
            let ch = self.curr().unwrap();
            self.eat_char();
            buffer.push(ch);
        }
    }

    fn offset(&self) -> u32 {
        self.offset.try_into().expect("overflow")
    }

    fn eat_char(&mut self) -> Option<char> {
        let curr = self.curr();

        if let Some(ch) = curr {
            self.offset += ch.len_utf8();
        }

        self.curr()
    }

    fn curr(&self) -> Option<char> {
        if self.offset < self.content.len() {
            self.content[self.offset..].chars().next()
        } else {
            None
        }
    }

    fn next(&self) -> Option<char> {
        let pos = self.offset + 1;

        if pos < self.content.len() {
            self.content[pos..].chars().next()
        } else {
            None
        }
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
    ch.map(|ch| "^+-*/%&|,=!~;:.()[]{}<>@".contains(ch))
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
    keywords.insert("fn", TokenKind::Fn);
    keywords.insert("let", TokenKind::Let);
    keywords.insert("mut", TokenKind::Mut);
    keywords.insert("const", TokenKind::Const);

    // control flow
    keywords.insert("return", TokenKind::Return);
    keywords.insert("if", TokenKind::If);
    keywords.insert("else", TokenKind::Else);
    keywords.insert("while", TokenKind::While);
    keywords.insert("for", TokenKind::For);
    keywords.insert("in", TokenKind::In);
    keywords.insert("break", TokenKind::Break);
    keywords.insert("continue", TokenKind::Continue);
    keywords.insert("match", TokenKind::Match);

    // qualifiers
    keywords.insert("self", TokenKind::This);
    keywords.insert("super", TokenKind::Super);
    keywords.insert("pub", TokenKind::Pub);
    keywords.insert("static", TokenKind::Static);

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
    use super::*;
    use crate::lexer::token::TokenKind;

    fn assert_end(reader: &mut Lexer, start: u32) {
        assert_tok(reader, TokenKind::End, start, 0);
    }

    fn assert_tok(reader: &mut Lexer, kind: TokenKind, start: u32, count: u32) {
        let tok = reader.read_token().unwrap();
        assert_eq!(kind, tok.kind);
        assert_eq!(start, tok.span.start());
        assert_eq!(count, tok.span.count());
    }

    fn assert_err(reader: &mut Lexer, msg: ParseError, start: u32, count: u32) {
        let err = reader.read_token().unwrap_err();
        assert_eq!(msg, err.error);
        assert_eq!(start, err.span.start());
        assert_eq!(count, err.span.count());
    }

    #[test]
    fn test_read_empty_file() {
        let mut reader = Lexer::from_str("");
        assert_end(&mut reader, 0);
        assert_end(&mut reader, 0);
    }

    #[test]
    fn test_read_numbers() {
        let mut reader = Lexer::from_str("1 2\n0123 10");
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::None),
            0,
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
            TokenKind::LitInt("0123".into(), IntBase::Dec, IntSuffix::None),
            4,
            4,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("10".into(), IntBase::Dec, IntSuffix::None),
            9,
            2,
        );
        assert_end(&mut reader, 11);

        let mut reader = Lexer::from_str("12u8 300u8 1_000 1__1");
        assert_tok(
            &mut reader,
            TokenKind::LitInt("12".into(), IntBase::Dec, IntSuffix::UInt8),
            0,
            4,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("300".into(), IntBase::Dec, IntSuffix::UInt8),
            5,
            5,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1_000".into(), IntBase::Dec, IntSuffix::None),
            11,
            5,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1__1".into(), IntBase::Dec, IntSuffix::None),
            17,
            4,
        );
    }

    #[test]
    fn test_read_numbers_with_suffix() {
        let mut reader = Lexer::from_str("1i32 2u8 3i64");
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::Int32),
            0,
            4,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("2".into(), IntBase::Dec, IntSuffix::UInt8),
            5,
            3,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("3".into(), IntBase::Dec, IntSuffix::Int64),
            9,
            4,
        );
        assert_end(&mut reader, 13);
    }

    #[test]
    fn test_skip_single_line_comment() {
        let mut reader = Lexer::from_str("//test\n1");
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::None),
            7,
            1,
        );
        assert_end(&mut reader, 8);
    }

    #[test]
    fn test_unfinished_line_comment() {
        let mut reader = Lexer::from_str("//abc");
        assert_end(&mut reader, 5);
    }

    #[test]
    fn test_skip_multi_comment() {
        let mut reader = Lexer::from_str("/*test*/1");
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::None),
            8,
            1,
        );
        assert_end(&mut reader, 9);
    }

    #[test]
    fn test_unfinished_multi_comment() {
        let mut reader = Lexer::from_str("/*test");
        assert_err(&mut reader, ParseError::UnclosedComment, 0, 6);

        let mut reader = Lexer::from_str("1/*test");
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::None),
            0,
            1,
        );
        assert_err(&mut reader, ParseError::UnclosedComment, 1, 6);
    }

    #[test]
    fn test_read_identifier() {
        let mut reader = Lexer::from_str("abc ident test");
        assert_tok(&mut reader, TokenKind::Identifier, 0, 3);
        assert_tok(&mut reader, TokenKind::Identifier, 4, 5);
        assert_tok(&mut reader, TokenKind::Identifier, 10, 4);
        assert_end(&mut reader, 14);
    }

    #[test]
    fn test_code_with_spaces() {
        let mut reader = Lexer::from_str("1 2 3");
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::None),
            0,
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
            4,
            1,
        );
        assert_end(&mut reader, 5);
    }

    #[test]
    fn test_float_numbers() {
        let mut reader = Lexer::from_str("1f32 1.0 0.1f32 1.3f64 4f64");
        assert_tok(
            &mut reader,
            TokenKind::LitFloat("1".into(), FloatSuffix::Float32),
            0,
            4,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitFloat("1.0".into(), FloatSuffix::Float64),
            5,
            3,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitFloat("0.1".into(), FloatSuffix::Float32),
            9,
            6,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitFloat("1.3".into(), FloatSuffix::Float64),
            16,
            6,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitFloat("4".into(), FloatSuffix::Float64),
            23,
            4,
        );
    }

    #[test]
    fn test_float_scientific_notation() {
        let mut reader = Lexer::from_str("1.0e1 1.0E1 1.0e+1 1.0e-1");
        assert_tok(
            &mut reader,
            TokenKind::LitFloat("1.0e1".into(), FloatSuffix::Float64),
            0,
            5,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitFloat("1.0E1".into(), FloatSuffix::Float64),
            6,
            5,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitFloat("1.0e+1".into(), FloatSuffix::Float64),
            12,
            6,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitFloat("1.0e-1".into(), FloatSuffix::Float64),
            19,
            6,
        );
    }

    #[test]
    fn test_hex_numbers() {
        let mut reader = Lexer::from_str("0x1 0x2i64 0xABCDEF 0xB1i64");

        assert_tok(
            &mut reader,
            TokenKind::LitInt("1".into(), IntBase::Hex, IntSuffix::None),
            0,
            3,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("2".into(), IntBase::Hex, IntSuffix::Int64),
            4,
            6,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("ABCDEF".into(), IntBase::Hex, IntSuffix::None),
            11,
            8,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("B1".into(), IntBase::Hex, IntSuffix::Int64),
            20,
            7,
        );
    }

    #[test]
    fn test_code_with_newlines() {
        let mut reader = Lexer::from_str("1\n2\n3");
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::None),
            0,
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
            4,
            1,
        );
        assert_end(&mut reader, 5);
    }

    #[test]
    fn test_code_with_tabs() {
        let mut reader = Lexer::from_str("1\t2\t3");
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1".into(), IntBase::Dec, IntSuffix::None),
            0,
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
            4,
            1,
        );
        assert_end(&mut reader, 5);
    }

    #[test]
    fn test_string_with_newline() {
        let mut reader = Lexer::from_str("\"abc\ndef\"");
        assert_tok(&mut reader, TokenKind::StringTail("abc\ndef".into()), 0, 9);
    }

    #[test]
    fn test_escape_sequences() {
        let mut reader = Lexer::from_str("\"\\\"\"");
        assert_tok(&mut reader, TokenKind::StringTail("\"".into()), 0, 4);

        let mut reader = Lexer::from_str("\"\\$\"");
        assert_tok(&mut reader, TokenKind::StringTail("$".into()), 0, 4);

        let mut reader = Lexer::from_str("\"\\\'\"");
        assert_tok(&mut reader, TokenKind::StringTail("'".into()), 0, 4);

        let mut reader = Lexer::from_str("\"\\t\"");
        assert_tok(&mut reader, TokenKind::StringTail("\t".into()), 0, 4);

        let mut reader = Lexer::from_str("\"\\n\"");
        assert_tok(&mut reader, TokenKind::StringTail("\n".into()), 0, 4);

        let mut reader = Lexer::from_str("\"\\r\"");
        assert_tok(&mut reader, TokenKind::StringTail("\r".into()), 0, 4);

        let mut reader = Lexer::from_str("\"\\\\\"");
        assert_tok(&mut reader, TokenKind::StringTail("\\".into()), 0, 4);

        let mut reader = Lexer::from_str("\"\\");
        assert_err(&mut reader, ParseError::UnclosedString, 0, 2);
    }

    #[test]
    fn test_unclosed_string() {
        let mut reader = Lexer::from_str("\"abc");
        assert_err(&mut reader, ParseError::UnclosedString, 0, 4);
    }

    #[test]
    fn test_unclosed_char() {
        let mut reader = Lexer::from_str("'a");
        assert_err(&mut reader, ParseError::UnclosedChar, 0, 2);

        let mut reader = Lexer::from_str("'\\");
        assert_err(&mut reader, ParseError::UnclosedChar, 0, 2);

        let mut reader = Lexer::from_str("'\\n");
        assert_err(&mut reader, ParseError::UnclosedChar, 0, 3);

        let mut reader = Lexer::from_str("'ab'");
        assert_err(&mut reader, ParseError::UnclosedChar, 0, 2);

        let mut reader = Lexer::from_str("'");
        assert_err(&mut reader, ParseError::UnclosedChar, 0, 1);
    }

    #[test]
    fn test_string() {
        let mut reader = Lexer::from_str("\"abc\"");
        assert_tok(&mut reader, TokenKind::StringTail("abc".into()), 0, 5);
        assert_end(&mut reader, 5);
    }

    #[test]
    fn test_keywords() {
        let mut reader = Lexer::from_str("fn let while if else match");
        assert_tok(&mut reader, TokenKind::Fn, 0, 2);
        assert_tok(&mut reader, TokenKind::Let, 3, 3);
        assert_tok(&mut reader, TokenKind::While, 7, 5);
        assert_tok(&mut reader, TokenKind::If, 13, 2);
        assert_tok(&mut reader, TokenKind::Else, 16, 4);
        assert_tok(&mut reader, TokenKind::Match, 21, 5);

        let mut reader = Lexer::from_str("self class super mod");
        assert_tok(&mut reader, TokenKind::This, 0, 4);
        assert_tok(&mut reader, TokenKind::Class, 5, 5);
        assert_tok(&mut reader, TokenKind::Super, 11, 5);
        assert_tok(&mut reader, TokenKind::Mod, 17, 3);

        let mut reader = Lexer::from_str("break continue return");
        assert_tok(&mut reader, TokenKind::Break, 0, 5);
        assert_tok(&mut reader, TokenKind::Continue, 6, 8);
        assert_tok(&mut reader, TokenKind::Return, 15, 6);

        let mut reader = Lexer::from_str("type struct enum alias trait const");
        assert_tok(&mut reader, TokenKind::Type, 0, 4);
        assert_tok(&mut reader, TokenKind::Struct, 5, 6);
        assert_tok(&mut reader, TokenKind::Enum, 12, 4);
        assert_tok(&mut reader, TokenKind::Alias, 17, 5);
        assert_tok(&mut reader, TokenKind::Trait, 23, 5);
        assert_tok(&mut reader, TokenKind::Const, 29, 5);

        let mut reader = Lexer::from_str("for in impl Self mut");
        assert_tok(&mut reader, TokenKind::For, 0, 3);
        assert_tok(&mut reader, TokenKind::In, 4, 2);
        assert_tok(&mut reader, TokenKind::Impl, 7, 4);
        assert_tok(&mut reader, TokenKind::CapitalThis, 12, 4);
        assert_tok(&mut reader, TokenKind::Mut, 17, 3);
    }

    #[test]
    fn test_operators() {
        let mut reader = Lexer::from_str("==-*/%.@...,");
        assert_tok(&mut reader, TokenKind::EqEq, 0, 2);
        assert_tok(&mut reader, TokenKind::Sub, 2, 1);
        assert_tok(&mut reader, TokenKind::Mul, 3, 1);
        assert_tok(&mut reader, TokenKind::Div, 4, 1);
        assert_tok(&mut reader, TokenKind::Modulo, 5, 1);
        assert_tok(&mut reader, TokenKind::Dot, 6, 1);
        assert_tok(&mut reader, TokenKind::At, 7, 1);
        assert_tok(&mut reader, TokenKind::DotDotDot, 8, 3);
        assert_tok(&mut reader, TokenKind::Comma, 11, 1);

        let mut reader = Lexer::from_str("<=<>=><");
        assert_tok(&mut reader, TokenKind::Le, 0, 2);
        assert_tok(&mut reader, TokenKind::Lt, 2, 1);
        assert_tok(&mut reader, TokenKind::Ge, 3, 2);
        assert_tok(&mut reader, TokenKind::Gt, 5, 1);
        assert_tok(&mut reader, TokenKind::Lt, 6, 1);

        let mut reader = Lexer::from_str("!=====!");
        assert_tok(&mut reader, TokenKind::NeEqEq, 0, 3);
        assert_tok(&mut reader, TokenKind::EqEqEq, 3, 3);
        assert_tok(&mut reader, TokenKind::Not, 6, 1);

        let mut reader = Lexer::from_str("!=!");
        assert_tok(&mut reader, TokenKind::NotEq, 0, 2);
        assert_tok(&mut reader, TokenKind::Not, 2, 1);

        let mut reader = Lexer::from_str("=>->");
        assert_tok(&mut reader, TokenKind::DoubleArrow, 0, 2);
        assert_tok(&mut reader, TokenKind::Arrow, 2, 2);

        let mut reader = Lexer::from_str(">><<>>>_::");
        assert_tok(&mut reader, TokenKind::GtGt, 0, 2);
        assert_tok(&mut reader, TokenKind::LtLt, 2, 2);
        assert_tok(&mut reader, TokenKind::GtGtGt, 4, 3);
        assert_tok(&mut reader, TokenKind::Underscore, 7, 1);
        assert_tok(&mut reader, TokenKind::ColonColon, 8, 2);
    }
}
