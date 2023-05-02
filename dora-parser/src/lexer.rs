use std::collections::HashMap;
use std::sync::Arc;

use crate::error::{ParseError, ParseErrorWithLocation};
use crate::{FloatSuffix, IntBase, Span, Token, TokenKind};

pub struct Lexer {
    content: Arc<String>,
    offset: usize,
    keywords: HashMap<&'static str, TokenKind>,
    errors: Vec<ParseErrorWithLocation>,
    in_string_template: bool,
    open_braces: usize,
}

impl Lexer {
    pub fn from_str(code: &str) -> Lexer {
        Lexer::new(Arc::new(String::from(code)))
    }

    pub fn source(&self) -> Arc<String> {
        self.content.clone()
    }

    pub fn errors(self) -> Vec<ParseErrorWithLocation> {
        self.errors
    }

    pub fn new(content: Arc<String>) -> Lexer {
        let keywords = keywords_in_map();

        Lexer {
            offset: 0,
            content,
            keywords,
            errors: Vec::new(),
            in_string_template: false,
            open_braces: 0,
        }
    }

    pub fn read_token(&mut self) -> Token {
        loop {
            self.skip_white();

            let start = self.offset();
            let ch = self.curr();

            if let None = ch {
                return Token::new(TokenKind::End, Span::at(start));
            }

            if is_digit(ch) {
                return self.read_number();
            } else if self.is_comment_start() {
                self.read_comment();
            } else if self.is_multi_comment_start() {
                self.read_multi_comment();
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
                self.report_error_at(ParseError::UnknownChar(ch), span);
            }
        }
    }

    fn skip_white(&mut self) {
        while is_whitespace(self.curr()) {
            self.eat_char();
        }
    }

    fn read_comment(&mut self) {
        while !self.curr().is_none() && !is_newline(self.curr()) {
            self.eat_char();
        }
    }

    fn read_multi_comment(&mut self) {
        let start = self.offset();

        self.eat_char();
        self.eat_char();

        while !self.curr().is_none() && !self.is_multi_comment_end() {
            self.eat_char();
        }

        if self.curr().is_none() {
            let span = self.span_from(start);
            self.report_error_at(ParseError::UnclosedComment, span);
            return;
        }

        self.eat_char();
        self.eat_char();
    }

    fn read_identifier(&mut self) -> Token {
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
        Token::new(ttype, span)
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

    fn read_char_literal(&mut self) -> Token {
        let start = self.offset();
        let mut terminated = false;
        let mut iterations = 0;
        let mut ch = '\0';

        self.eat_char();

        while self.curr().is_some() && !is_char_quote(self.curr()) {
            let current_char = self.read_escaped_char();
            if iterations == 0 {
                ch = current_char;
            }
            iterations += 1;
        }

        if is_char_quote(self.curr()) {
            self.eat_char();
            if iterations == 1 {
                terminated = true;
            }
        }

        let ttype = TokenKind::LitChar(ch);
        let span = self.span_from(start);

        if !terminated {
            self.report_error_at(ParseError::UnclosedChar, span);
        }

        Token::new(ttype, span)
    }

    fn read_escaped_char(&mut self) -> char {
        let ch = self.curr().expect("missing char");
        let escaped_start = self.offset();
        self.eat_char();

        if ch == '\\' {
            let ch = if let Some(ch) = self.curr() {
                ch
            } else {
                let span = self.span_from(escaped_start);
                self.report_error_at(ParseError::InvalidEscapeSequence, span);
                return '\\';
            };

            self.eat_char();

            match ch {
                '\\' => '\\',
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                '\"' => '\"',
                '\'' => '\'',
                '0' => '\0',
                '$' => '$',
                _ => {
                    let span = self.span_from(escaped_start);
                    self.report_error_at(ParseError::InvalidEscapeSequence, span);
                    ch
                }
            }
        } else {
            ch
        }
    }

    fn read_string(&mut self, skip_quote: bool) -> Token {
        let start = self.offset();
        let mut value = String::new();

        if skip_quote {
            assert_eq!(self.curr(), Some('\"'));
            self.eat_char();
        }

        while self.curr().is_some() && !is_quote(self.curr()) {
            if self.curr() == Some('$') && self.lookahead() == Some('{') {
                self.eat_char();
                self.eat_char();

                let ttype = TokenKind::StringExpr(value);
                let span = self.span_from(start);
                return Token::new(ttype, span);
            }

            let ch = self.read_escaped_char();
            value.push(ch);
        }

        let terminated = if is_quote(self.curr()) {
            self.eat_char();
            true
        } else {
            false
        };

        let span = self.span_from(start);
        let ttype = TokenKind::StringTail(value);

        if !terminated {
            let span = self.span_from(start);
            self.report_error_at(ParseError::UnclosedString, span);
        }

        Token::new(ttype, span)
    }

    pub fn read_string_continuation(&mut self) -> Token {
        self.read_string(false)
    }

    fn read_operator(&mut self) -> Token {
        let start = self.offset();
        let ch = self.curr().unwrap();
        self.eat_char();

        let nch = self.curr().unwrap_or('x');
        let nnch = self.lookahead().unwrap_or('x');

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
            '{' => {
                if self.in_string_template {
                    self.open_braces += 1;
                }
                TokenKind::LBrace
            }
            '}' => {
                if self.in_string_template {
                    self.open_braces -= 1;
                }
                TokenKind::RBrace
            }

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
                unreachable!()
            }
        };

        let span = self.span_from(start);
        Token::new(kind, span)
    }

    fn read_number(&mut self) -> Token {
        let start = self.offset();
        let mut value = String::new();

        let base = if self.curr() == Some('0') {
            let next = self.lookahead();

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

        if base == IntBase::Dec && self.curr() == Some('.') && is_digit(self.lookahead()) {
            return self.read_number_as_float(start, value);
        }

        let kind = if is_identifier_start(self.curr()) {
            let suffix = self.read_identifier_as_string();
            let span = self.span_from(start);

            match suffix.as_str() {
                "u8" => TokenKind::LitInt(value, base, Some(suffix)),
                "i32" => TokenKind::LitInt(value, base, Some(suffix)),
                "i64" => TokenKind::LitInt(value, base, Some(suffix)),
                "f32" if base == IntBase::Dec => TokenKind::LitFloat(value, FloatSuffix::Float32),
                "f64" if base == IntBase::Dec => TokenKind::LitFloat(value, FloatSuffix::Float64),
                _ => {
                    self.report_error_at(ParseError::InvalidSuffix(suffix), span);
                    TokenKind::LitInt(value, base, None)
                }
            }
        } else {
            TokenKind::LitInt(value, base, None)
        };

        let span = self.span_from(start);
        Token::new(kind, span)
    }

    fn read_number_as_float(&mut self, start: u32, mut value: String) -> Token {
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
                    self.report_error_at(ParseError::InvalidSuffix(suffix), span);
                    FloatSuffix::Float64
                }
            }
        } else {
            FloatSuffix::Float64
        };

        let ttype = TokenKind::LitFloat(value, suffix);
        let span = self.span_from(start);
        Token::new(ttype, span)
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

    fn eat_char(&mut self) {
        let curr = self.curr();

        if let Some(ch) = curr {
            self.offset += ch.len_utf8();
        }
    }

    fn curr(&self) -> Option<char> {
        if self.offset < self.content.len() {
            self.content[self.offset..].chars().next()
        } else {
            None
        }
    }

    fn lookahead(&self) -> Option<char> {
        let pos = self.offset + 1;

        if pos < self.content.len() {
            self.content[pos..].chars().next()
        } else {
            None
        }
    }

    fn report_error_at(&mut self, msg: ParseError, span: Span) {
        self.errors.push(ParseErrorWithLocation::new(span, msg));
    }

    fn is_comment_start(&self) -> bool {
        self.curr() == Some('/') && self.lookahead() == Some('/')
    }

    fn is_multi_comment_start(&self) -> bool {
        self.curr() == Some('/') && self.lookahead() == Some('*')
    }

    fn is_multi_comment_end(&self) -> bool {
        self.curr() == Some('*') && self.lookahead() == Some('/')
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
    ch.map(|ch| "^+-*/%&|,=!;:.()[]{}<>@".contains(ch))
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
    use crate::{
        FloatSuffix, IntBase, Lexer, ParseError, ParseErrorWithLocation, Span, Token, TokenKind,
    };
    use std::sync::Arc;

    fn lex(content: &str) -> (Vec<Token>, Vec<ParseErrorWithLocation>) {
        let content = Arc::new(content.into());
        let mut lexer = Lexer::new(content);
        let mut tokens = Vec::new();

        loop {
            let token = lexer.read_token();
            if token.is_eof() {
                break;
            }
            tokens.push(token);
        }

        (tokens, lexer.errors)
    }

    fn lex_success(content: &str) -> Vec<Token> {
        let (tokens, errors) = lex(content);
        assert!(errors.is_empty());
        tokens
    }

    fn assert_end(reader: &mut Lexer, start: u32) {
        assert_tok(reader, TokenKind::End, start, 0);
    }

    fn assert_tok(reader: &mut Lexer, kind: TokenKind, start: u32, count: u32) {
        let tok = reader.read_token();
        assert_eq!(kind, tok.kind);
        assert_eq!(start, tok.span.start());
        assert_eq!(count, tok.span.count());
    }

    fn assert_tok2(tokens: Vec<Token>, kind: TokenKind, start: u32, count: u32) {
        assert_eq!(tokens.len(), 1);
        let token = &tokens[0];
        assert_eq!(kind, token.kind);
        assert_eq!(start, token.span.start());
        assert_eq!(count, token.span.count());
    }

    fn assert_err(errors: Vec<ParseErrorWithLocation>, msg: ParseError, start: u32, count: u32) {
        assert_eq!(errors.len(), 1);
        let error = &errors[0];
        assert_eq!(msg, error.error);
        assert_eq!(start, error.span.start());
        assert_eq!(count, error.span.count());
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
            TokenKind::LitInt("1".into(), IntBase::Dec, None),
            0,
            1,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("2".into(), IntBase::Dec, None),
            2,
            1,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("0123".into(), IntBase::Dec, None),
            4,
            4,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("10".into(), IntBase::Dec, None),
            9,
            2,
        );
        assert_end(&mut reader, 11);

        let mut reader = Lexer::from_str("12u8 300u8 1_000 1__1");
        assert_tok(
            &mut reader,
            TokenKind::LitInt("12".into(), IntBase::Dec, Some("u8".into())),
            0,
            4,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("300".into(), IntBase::Dec, Some("u8".into())),
            5,
            5,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1_000".into(), IntBase::Dec, None),
            11,
            5,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1__1".into(), IntBase::Dec, None),
            17,
            4,
        );
    }

    #[test]
    fn test_read_numbers_with_suffix() {
        let mut reader = Lexer::from_str("1i32 2u8 3i64");
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1".into(), IntBase::Dec, Some("i32".into())),
            0,
            4,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("2".into(), IntBase::Dec, Some("u8".into())),
            5,
            3,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("3".into(), IntBase::Dec, Some("i64".into())),
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
            TokenKind::LitInt("1".into(), IntBase::Dec, None),
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
            TokenKind::LitInt("1".into(), IntBase::Dec, None),
            8,
            1,
        );
        assert_end(&mut reader, 9);
    }

    #[test]
    fn test_unfinished_multi_comment() {
        let (tokens, errors) = lex("/*test");
        assert!(tokens.is_empty());
        assert_err(errors, ParseError::UnclosedComment, 0, 6);

        let (tokens, errors) = lex("1/*test");
        assert_eq!(
            tokens,
            vec![Token::new(
                TokenKind::LitInt("1".into(), IntBase::Dec, None),
                Span::new(0, 1)
            )]
        );
        assert_err(errors, ParseError::UnclosedComment, 1, 6);
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
            TokenKind::LitInt("1".into(), IntBase::Dec, None),
            0,
            1,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("2".into(), IntBase::Dec, None),
            2,
            1,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("3".into(), IntBase::Dec, None),
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
            TokenKind::LitInt("1".into(), IntBase::Hex, None),
            0,
            3,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("2".into(), IntBase::Hex, Some("i64".into())),
            4,
            6,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("ABCDEF".into(), IntBase::Hex, None),
            11,
            8,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("B1".into(), IntBase::Hex, Some("i64".into())),
            20,
            7,
        );
    }

    #[test]
    fn test_code_with_newlines() {
        let mut reader = Lexer::from_str("1\n2\n3");
        assert_tok(
            &mut reader,
            TokenKind::LitInt("1".into(), IntBase::Dec, None),
            0,
            1,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("2".into(), IntBase::Dec, None),
            2,
            1,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("3".into(), IntBase::Dec, None),
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
            TokenKind::LitInt("1".into(), IntBase::Dec, None),
            0,
            1,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("2".into(), IntBase::Dec, None),
            2,
            1,
        );
        assert_tok(
            &mut reader,
            TokenKind::LitInt("3".into(), IntBase::Dec, None),
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
        let tokens = lex_success("\"\\\"\"");
        assert_eq!(
            tokens,
            vec![Token::new(
                TokenKind::StringTail("\"".into()),
                Span::new(0, 4)
            )]
        );

        let tokens = lex_success("\"\\$\"");
        assert_tok2(tokens, TokenKind::StringTail("$".into()), 0, 4);

        let tokens = lex_success("\"\\\'\"");
        assert_tok2(tokens, TokenKind::StringTail("'".into()), 0, 4);

        let tokens = lex_success("\"\\t\"");
        assert_tok2(tokens, TokenKind::StringTail("\t".into()), 0, 4);

        let tokens = lex_success("\"\\n\"");
        assert_tok2(tokens, TokenKind::StringTail("\n".into()), 0, 4);

        let tokens = lex_success("\"\\r\"");
        assert_tok2(tokens, TokenKind::StringTail("\r".into()), 0, 4);

        let tokens = lex_success("\"\\\\\"");
        assert_tok2(tokens, TokenKind::StringTail("\\".into()), 0, 4);

        let (tokens, errors) = lex("\"\\");
        assert_eq!(
            tokens,
            vec![Token::new(
                TokenKind::StringTail("\\".into()),
                Span::new(0, 2)
            )]
        );
        assert_eq!(
            errors,
            vec![
                ParseErrorWithLocation::new(Span::new(1, 1), ParseError::InvalidEscapeSequence),
                ParseErrorWithLocation::new(Span::new(0, 2), ParseError::UnclosedString)
            ]
        );
    }

    #[test]
    fn test_unclosed_string() {
        let (tokens, errors) = lex("\"abc");
        assert_tok2(tokens, TokenKind::StringTail("abc".into()), 0, 4);
        assert_err(errors, ParseError::UnclosedString, 0, 4);
    }

    #[test]
    fn test_unclosed_char() {
        let (tokens, errors) = lex("'a");
        assert_tok2(tokens, TokenKind::LitChar('a'), 0, 2);
        assert_err(errors, ParseError::UnclosedChar, 0, 2);

        let (tokens, errors) = lex("'\\");
        assert_tok2(tokens, TokenKind::LitChar('\\'), 0, 2);
        assert_eq!(
            errors,
            vec![
                ParseErrorWithLocation::new(Span::new(1, 1), ParseError::InvalidEscapeSequence),
                ParseErrorWithLocation::new(Span::new(0, 2), ParseError::UnclosedChar)
            ]
        );

        let (tokens, errors) = lex("'\\n");
        assert_tok2(tokens, TokenKind::LitChar('\n'), 0, 3);
        assert_err(errors, ParseError::UnclosedChar, 0, 3);

        let (tokens, errors) = lex("'ab'");
        assert_tok2(tokens, TokenKind::LitChar('a'), 0, 4);
        assert_err(errors, ParseError::UnclosedChar, 0, 4);

        let (tokens, errors) = lex("'");
        assert_tok2(tokens, TokenKind::LitChar('\0'), 0, 1);
        assert_err(errors, ParseError::UnclosedChar, 0, 1);
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

    #[test]
    fn test_invalid_char() {
        let (tokens, errors) = lex("a☕b");
        assert_eq!(
            tokens,
            vec![
                Token::new(TokenKind::Identifier, Span::new(0, 1)),
                Token::new(TokenKind::Identifier, Span::new(4, 1))
            ]
        );
        assert_err(errors, ParseError::UnknownChar('☕'), 1, 3);
    }
}
