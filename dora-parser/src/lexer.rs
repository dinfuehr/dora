use std::collections::HashMap;
use std::sync::Arc;

use crate::error::{ParseError, ParseErrorWithLocation};
use crate::{Span, Token, TokenKind};

pub fn lex(content: &str) -> (Vec<Token>, Vec<ParseErrorWithLocation>) {
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

pub struct Lexer {
    content: Arc<String>,
    offset: usize,
    keywords: HashMap<&'static str, TokenKind>,
    errors: Vec<ParseErrorWithLocation>,
    open_braces: Vec<usize>,
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
            open_braces: Vec::new(),
        }
    }

    pub fn read_token(&mut self) -> Token {
        loop {
            self.skip_white();

            let start = self.offset();
            let ch = self.curr();

            if let None = ch {
                return Token::new(TokenKind::EOF, Span::at(start));
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
                return self.read_string(false);
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
            TokenKind::UNDERSCORE
        } else {
            TokenKind::IDENTIFIER
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
        self.eat_char();

        while self.curr().is_some() && !is_char_quote(self.curr()) {
            self.read_escaped_char();
        }

        if is_char_quote(self.curr()) {
            self.eat_char();
        } else {
            let span = self.span_from(start);
            self.report_error_at(ParseError::UnclosedChar, span);
        }

        let span = self.span_from(start);
        Token::new(TokenKind::CHAR_LITERAL, span)
    }

    fn read_escaped_char(&mut self) {
        if self.eat_char() == Some('\\') {
            self.eat_char();
        }
    }

    fn read_string(&mut self, continuation: bool) -> Token {
        let mut start = self.offset();

        if continuation {
            // } was already consumed by read_operator().
            start -= '}'.len_utf8() as u32;
        } else {
            assert_eq!(self.curr(), Some('\"'));
            self.eat_char();
        }

        while self.curr().is_some() && !is_quote(self.curr()) {
            if self.curr() == Some('$') && self.lookahead() == Some('{') {
                self.eat_char();
                self.eat_char();

                self.open_braces.push(1);

                let ttype = TokenKind::TEMPLATE_LITERAL;
                let span = self.span_from(start);
                return Token::new(ttype, span);
            }

            self.read_escaped_char();
        }

        if is_quote(self.curr()) {
            self.eat_char();
        } else {
            let span = self.span_from(start);
            self.report_error_at(ParseError::UnclosedString, span);
        }

        let span = self.span_from(start);
        Token::new(TokenKind::STRING_LITERAL, span)
    }

    fn read_operator(&mut self) -> Token {
        let start = self.offset();
        let ch = self.curr().unwrap();
        self.eat_char();

        let nch = self.curr().unwrap_or('x');
        let nnch = self.lookahead().unwrap_or('x');

        let kind = match ch {
            '+' => TokenKind::ADD,
            '-' => {
                if nch == '>' {
                    self.eat_char();
                    TokenKind::ARROW
                } else {
                    TokenKind::SUB
                }
            }
            '*' => TokenKind::MUL,
            '/' => TokenKind::DIV,
            '%' => TokenKind::MODULO,

            '(' => TokenKind::L_PAREN,
            ')' => TokenKind::R_PAREN,
            '[' => TokenKind::L_BRACKET,
            ']' => TokenKind::R_BRACKET,
            '{' => {
                if let Some(open_braces_top) = self.open_braces.last_mut() {
                    *open_braces_top += 1;
                }
                TokenKind::L_BRACE
            }
            '}' => {
                if let Some(open_braces_top) = self.open_braces.last_mut() {
                    *open_braces_top -= 1;

                    if *open_braces_top == 0 {
                        self.open_braces.pop();
                        return self.read_string(true);
                    }
                }
                TokenKind::R_BRACE
            }

            '|' => {
                if nch == '|' {
                    self.eat_char();
                    TokenKind::OR_OR
                } else {
                    TokenKind::OR
                }
            }

            '&' => {
                if nch == '&' {
                    self.eat_char();
                    TokenKind::AND_AND
                } else {
                    TokenKind::AND
                }
            }

            '^' => TokenKind::CARET,
            ',' => TokenKind::COMMA,
            ';' => TokenKind::SEMICOLON,
            ':' => {
                if nch == ':' {
                    self.eat_char();
                    TokenKind::COLON_COLON
                } else {
                    TokenKind::COLON
                }
            }
            '.' => {
                if nch == '.' && nnch == '.' {
                    self.eat_char();
                    self.eat_char();

                    TokenKind::DOT_DOT_DOT
                } else {
                    TokenKind::DOT
                }
            }
            '=' => {
                if nch == '=' {
                    self.eat_char();

                    if nnch == '=' {
                        self.eat_char();
                        TokenKind::EQ_EQ_EQ
                    } else {
                        TokenKind::EQ_EQ
                    }
                } else if nch == '>' {
                    self.eat_char();
                    TokenKind::DOUBLE_ARROW
                } else {
                    TokenKind::EQ
                }
            }

            '<' => match nch {
                '=' => {
                    self.eat_char();
                    TokenKind::LE
                }

                '<' => {
                    self.eat_char();
                    TokenKind::LT_LT
                }

                _ => TokenKind::LT,
            },

            '>' => match nch {
                '=' => {
                    self.eat_char();
                    TokenKind::GE
                }

                '>' => {
                    self.eat_char();

                    if nnch == '>' {
                        self.eat_char();
                        TokenKind::GT_GT_GT
                    } else {
                        TokenKind::GT_GT
                    }
                }

                _ => TokenKind::GT,
            },
            '!' => {
                if nch == '=' {
                    self.eat_char();

                    if nnch == '=' {
                        self.eat_char();
                        TokenKind::NOT_EQ_EQ
                    } else {
                        TokenKind::NOT_EQ
                    }
                } else {
                    TokenKind::NOT
                }
            }
            '@' => TokenKind::AT,

            _ => {
                unreachable!()
            }
        };

        let span = self.span_from(start);
        Token::new(kind, span)
    }

    fn read_number(&mut self) -> Token {
        let start = self.offset();

        let base = if self.curr() == Some('0') {
            let next = self.lookahead();

            match next {
                Some('x') => {
                    self.eat_char();
                    self.eat_char();

                    16
                }

                Some('b') => {
                    self.eat_char();
                    self.eat_char();

                    2
                }

                _ => 10,
            }
        } else {
            10
        };

        self.read_digits(base);

        if base == 10 && self.curr() == Some('.') && is_digit(self.lookahead()) {
            return self.read_number_as_float(start);
        }

        if is_identifier_start(self.curr()) {
            self.read_identifier_as_string();
        }

        let span = self.span_from(start);
        Token::new(TokenKind::INT_LITERAL, span)
    }

    fn read_number_as_float(&mut self, start: u32) -> Token {
        self.eat_char();

        self.read_digits(10);

        if self.curr() == Some('e') || self.curr() == Some('E') {
            self.eat_char();

            if self.curr() == Some('+') || self.curr() == Some('-') {
                self.eat_char();
            }

            self.read_digits(10);
        }

        if is_identifier_start(self.curr()) {
            self.read_identifier_as_string();
        }

        let span = self.span_from(start);
        Token::new(TokenKind::FLOAT_LITERAL, span)
    }

    fn span_from(&self, start: u32) -> Span {
        Span::new(start, self.offset() - start)
    }

    fn read_digits(&mut self, base: u32) {
        while is_digit_or_underscore(self.curr(), base) {
            self.eat_char();
        }
    }

    fn offset(&self) -> u32 {
        self.offset.try_into().expect("overflow")
    }

    fn eat_char(&mut self) -> Option<char> {
        let curr = self.curr();

        if let Some(ch) = curr {
            self.offset += ch.len_utf8();
            Some(ch)
        } else {
            None
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
        if self.offset < self.content.len() {
            let mut it = self.content[self.offset..].chars();
            it.next();
            it.next()
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

fn is_digit_or_underscore(ch: Option<char>, base: u32) -> bool {
    ch.map(|ch| ch.is_digit(base) || ch == '_').unwrap_or(false)
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
    keywords.insert("true", TokenKind::TRUE);
    keywords.insert("false", TokenKind::FALSE);

    // "big" shapes
    keywords.insert("class", TokenKind::CLASS);
    keywords.insert("enum", TokenKind::ENUM);
    keywords.insert("struct", TokenKind::STRUCT);
    keywords.insert("trait", TokenKind::TRAIT);
    keywords.insert("impl", TokenKind::IMPL);
    keywords.insert("mod", TokenKind::MOD);
    keywords.insert("use", TokenKind::USE);
    keywords.insert("package", TokenKind::PACKAGE);

    // "small" shapes
    keywords.insert("fn", TokenKind::FN);
    keywords.insert("let", TokenKind::LET);
    keywords.insert("mut", TokenKind::MUT);
    keywords.insert("const", TokenKind::CONST);

    // control flow
    keywords.insert("return", TokenKind::RETURN);
    keywords.insert("if", TokenKind::IF);
    keywords.insert("else", TokenKind::ELSE);
    keywords.insert("while", TokenKind::WHILE);
    keywords.insert("for", TokenKind::FOR);
    keywords.insert("in", TokenKind::IN);
    keywords.insert("break", TokenKind::BREAK);
    keywords.insert("continue", TokenKind::CONTINUE);
    keywords.insert("match", TokenKind::MATCH);

    // qualifiers
    keywords.insert("self", TokenKind::THIS);
    keywords.insert("super", TokenKind::SUPER);
    keywords.insert("pub", TokenKind::PUB);
    keywords.insert("static", TokenKind::STATIC);

    // casting
    keywords.insert("as", TokenKind::AS);

    // unused
    keywords.insert("type", TokenKind::TYPE);
    keywords.insert("alias", TokenKind::ALIAS);
    keywords.insert("Self", TokenKind::CAPITAL_THIS);

    keywords
}

#[cfg(test)]
mod tests {
    use crate::{lex, Lexer, ParseError, ParseErrorWithLocation, Span, Token, TokenKind};

    fn lex_success(content: &str) -> Vec<Token> {
        let (tokens, errors) = lex(content);
        assert!(errors.is_empty());
        tokens
    }

    fn dump_tokens(tokens: Vec<Token>) -> String {
        let mut content = String::new();

        for token in tokens {
            content.push_str(&format!(
                "{:?}@{}..{}\n",
                token.kind,
                token.span.start(),
                token.span.end()
            ));
        }

        content
    }

    fn assert_end(reader: &mut Lexer, start: u32) {
        assert_tok(reader, TokenKind::EOF, start, 0);
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
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 0, 1);
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 2, 1);
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 4, 4);
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 9, 2);
        assert_end(&mut reader, 11);

        let mut reader = Lexer::from_str("12u8 300u8 1_000 1__1");
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 0, 4);
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 5, 5);
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 11, 5);
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 17, 4);
    }

    #[test]
    fn test_read_numbers_with_suffix() {
        let mut reader = Lexer::from_str("1i32 2u8 3i64");
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 0, 4);
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 5, 3);
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 9, 4);
        assert_end(&mut reader, 13);
    }

    #[test]
    fn test_skip_single_line_comment() {
        let mut reader = Lexer::from_str("//test\n1");
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 7, 1);
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
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 8, 1);
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
            vec![Token::new(TokenKind::INT_LITERAL, Span::new(0, 1))]
        );
        assert_err(errors, ParseError::UnclosedComment, 1, 6);
    }

    #[test]
    fn test_read_identifier() {
        let mut reader = Lexer::from_str("abc ident test");
        assert_tok(&mut reader, TokenKind::IDENTIFIER, 0, 3);
        assert_tok(&mut reader, TokenKind::IDENTIFIER, 4, 5);
        assert_tok(&mut reader, TokenKind::IDENTIFIER, 10, 4);
        assert_end(&mut reader, 14);
    }

    #[test]
    fn test_code_with_spaces() {
        let mut reader = Lexer::from_str("1 2 3");
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 0, 1);
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 2, 1);
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 4, 1);
        assert_end(&mut reader, 5);
    }

    #[test]
    fn test_float_numbers() {
        let mut reader = Lexer::from_str("1f32 1.0 0.1f32 1.3f64 4f64");
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 0, 4);
        assert_tok(&mut reader, TokenKind::FLOAT_LITERAL, 5, 3);
        assert_tok(&mut reader, TokenKind::FLOAT_LITERAL, 9, 6);
        assert_tok(&mut reader, TokenKind::FLOAT_LITERAL, 16, 6);
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 23, 4);
    }

    #[test]
    fn test_float_scientific_notation() {
        let mut reader = Lexer::from_str("1.0e1 1.0E1 1.0e+1 1.0e-1");
        assert_tok(&mut reader, TokenKind::FLOAT_LITERAL, 0, 5);
        assert_tok(&mut reader, TokenKind::FLOAT_LITERAL, 6, 5);
        assert_tok(&mut reader, TokenKind::FLOAT_LITERAL, 12, 6);
        assert_tok(&mut reader, TokenKind::FLOAT_LITERAL, 19, 6);
    }

    #[test]
    fn test_hex_numbers() {
        let mut reader = Lexer::from_str("0x1 0x2i64 0xABCDEF 0xB1i64");

        assert_tok(&mut reader, TokenKind::INT_LITERAL, 0, 3);
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 4, 6);
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 11, 8);
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 20, 7);
    }

    #[test]
    fn test_code_with_newlines() {
        let mut reader = Lexer::from_str("1\n2\n3");
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 0, 1);
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 2, 1);
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 4, 1);
        assert_end(&mut reader, 5);
    }

    #[test]
    fn test_code_with_tabs() {
        let mut reader = Lexer::from_str("1\t2\t3");
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 0, 1);
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 2, 1);
        assert_tok(&mut reader, TokenKind::INT_LITERAL, 4, 1);
        assert_end(&mut reader, 5);
    }

    #[test]
    fn test_string_with_newline() {
        let mut reader = Lexer::from_str("\"abc\ndef\"");
        assert_tok(&mut reader, TokenKind::STRING_LITERAL, 0, 9);
    }

    #[test]
    fn test_escape_sequences() {
        let tokens = lex_success("\"\\\"\"");
        assert_eq!(
            tokens,
            vec![Token::new(TokenKind::STRING_LITERAL, Span::new(0, 4))]
        );

        let tokens = lex_success("\"\\$\"");
        assert_tok2(tokens, TokenKind::STRING_LITERAL, 0, 4);

        let tokens = lex_success("\"\\\'\"");
        assert_tok2(tokens, TokenKind::STRING_LITERAL, 0, 4);

        let tokens = lex_success("\"\\t\"");
        assert_tok2(tokens, TokenKind::STRING_LITERAL, 0, 4);

        let tokens = lex_success("\"\\n\"");
        assert_tok2(tokens, TokenKind::STRING_LITERAL, 0, 4);

        let tokens = lex_success("\"\\r\"");
        assert_tok2(tokens, TokenKind::STRING_LITERAL, 0, 4);

        let tokens = lex_success("\"\\\\\"");
        assert_tok2(tokens, TokenKind::STRING_LITERAL, 0, 4);

        let (tokens, errors) = lex("\"\\");
        assert_eq!(
            tokens,
            vec![Token::new(TokenKind::STRING_LITERAL, Span::new(0, 2))]
        );
        assert_eq!(
            errors,
            vec![ParseErrorWithLocation::new(
                Span::new(0, 2),
                ParseError::UnclosedString
            )]
        );
    }

    #[test]
    fn test_unclosed_string() {
        let (tokens, errors) = lex("\"abc");
        assert_tok2(tokens, TokenKind::STRING_LITERAL, 0, 4);
        assert_err(errors, ParseError::UnclosedString, 0, 4);
    }

    #[test]
    fn test_unclosed_char() {
        let (tokens, errors) = lex("'a");
        assert_tok2(tokens, TokenKind::CHAR_LITERAL, 0, 2);
        assert_err(errors, ParseError::UnclosedChar, 0, 2);

        let (tokens, errors) = lex("'\\");
        assert_tok2(tokens, TokenKind::CHAR_LITERAL, 0, 2);
        assert_eq!(
            errors,
            vec![ParseErrorWithLocation::new(
                Span::new(0, 2),
                ParseError::UnclosedChar
            )]
        );

        let (tokens, errors) = lex("'\\n");
        assert_tok2(tokens, TokenKind::CHAR_LITERAL, 0, 3);
        assert_err(errors, ParseError::UnclosedChar, 0, 3);

        let tokens = lex_success("'ab'");
        assert_tok2(tokens, TokenKind::CHAR_LITERAL, 0, 4);

        let (tokens, errors) = lex("'");
        assert_tok2(tokens, TokenKind::CHAR_LITERAL, 0, 1);
        assert_err(errors, ParseError::UnclosedChar, 0, 1);
    }

    #[test]
    fn test_string() {
        let mut reader = Lexer::from_str("\"abc\"");
        assert_tok(&mut reader, TokenKind::STRING_LITERAL, 0, 5);
        assert_end(&mut reader, 5);
    }

    #[test]
    fn test_keywords() {
        let mut reader = Lexer::from_str("fn let while if else match");
        assert_tok(&mut reader, TokenKind::FN, 0, 2);
        assert_tok(&mut reader, TokenKind::LET, 3, 3);
        assert_tok(&mut reader, TokenKind::WHILE, 7, 5);
        assert_tok(&mut reader, TokenKind::IF, 13, 2);
        assert_tok(&mut reader, TokenKind::ELSE, 16, 4);
        assert_tok(&mut reader, TokenKind::MATCH, 21, 5);

        let mut reader = Lexer::from_str("self class super mod");
        assert_tok(&mut reader, TokenKind::THIS, 0, 4);
        assert_tok(&mut reader, TokenKind::CLASS, 5, 5);
        assert_tok(&mut reader, TokenKind::SUPER, 11, 5);
        assert_tok(&mut reader, TokenKind::MOD, 17, 3);

        let mut reader = Lexer::from_str("break continue return");
        assert_tok(&mut reader, TokenKind::BREAK, 0, 5);
        assert_tok(&mut reader, TokenKind::CONTINUE, 6, 8);
        assert_tok(&mut reader, TokenKind::RETURN, 15, 6);

        let mut reader = Lexer::from_str("type struct enum alias trait const");
        assert_tok(&mut reader, TokenKind::TYPE, 0, 4);
        assert_tok(&mut reader, TokenKind::STRUCT, 5, 6);
        assert_tok(&mut reader, TokenKind::ENUM, 12, 4);
        assert_tok(&mut reader, TokenKind::ALIAS, 17, 5);
        assert_tok(&mut reader, TokenKind::TRAIT, 23, 5);
        assert_tok(&mut reader, TokenKind::CONST, 29, 5);

        let mut reader = Lexer::from_str("for in impl Self mut");
        assert_tok(&mut reader, TokenKind::FOR, 0, 3);
        assert_tok(&mut reader, TokenKind::IN, 4, 2);
        assert_tok(&mut reader, TokenKind::IMPL, 7, 4);
        assert_tok(&mut reader, TokenKind::CAPITAL_THIS, 12, 4);
        assert_tok(&mut reader, TokenKind::MUT, 17, 3);
    }

    #[test]
    fn test_operators() {
        let mut reader = Lexer::from_str("==-*/%.@...,");
        assert_tok(&mut reader, TokenKind::EQ_EQ, 0, 2);
        assert_tok(&mut reader, TokenKind::SUB, 2, 1);
        assert_tok(&mut reader, TokenKind::MUL, 3, 1);
        assert_tok(&mut reader, TokenKind::DIV, 4, 1);
        assert_tok(&mut reader, TokenKind::MODULO, 5, 1);
        assert_tok(&mut reader, TokenKind::DOT, 6, 1);
        assert_tok(&mut reader, TokenKind::AT, 7, 1);
        assert_tok(&mut reader, TokenKind::DOT_DOT_DOT, 8, 3);
        assert_tok(&mut reader, TokenKind::COMMA, 11, 1);

        let mut reader = Lexer::from_str("<=<>=><");
        assert_tok(&mut reader, TokenKind::LE, 0, 2);
        assert_tok(&mut reader, TokenKind::LT, 2, 1);
        assert_tok(&mut reader, TokenKind::GE, 3, 2);
        assert_tok(&mut reader, TokenKind::GT, 5, 1);
        assert_tok(&mut reader, TokenKind::LT, 6, 1);

        let mut reader = Lexer::from_str("!=====!");
        assert_tok(&mut reader, TokenKind::NOT_EQ_EQ, 0, 3);
        assert_tok(&mut reader, TokenKind::EQ_EQ_EQ, 3, 3);
        assert_tok(&mut reader, TokenKind::NOT, 6, 1);

        let mut reader = Lexer::from_str("!=!");
        assert_tok(&mut reader, TokenKind::NOT_EQ, 0, 2);
        assert_tok(&mut reader, TokenKind::NOT, 2, 1);

        let mut reader = Lexer::from_str("=>->");
        assert_tok(&mut reader, TokenKind::DOUBLE_ARROW, 0, 2);
        assert_tok(&mut reader, TokenKind::ARROW, 2, 2);

        let mut reader = Lexer::from_str(">><<>>>_::");
        assert_tok(&mut reader, TokenKind::GT_GT, 0, 2);
        assert_tok(&mut reader, TokenKind::LT_LT, 2, 2);
        assert_tok(&mut reader, TokenKind::GT_GT_GT, 4, 3);
        assert_tok(&mut reader, TokenKind::UNDERSCORE, 7, 1);
        assert_tok(&mut reader, TokenKind::COLON_COLON, 8, 2);
    }

    #[test]
    fn test_invalid_char() {
        let (tokens, errors) = lex("a☕b");
        assert_eq!(
            tokens,
            vec![
                Token::new(TokenKind::IDENTIFIER, Span::new(0, 1)),
                Token::new(TokenKind::IDENTIFIER, Span::new(4, 1))
            ]
        );
        assert_err(errors, ParseError::UnknownChar('☕'), 1, 3);
    }

    #[test]
    fn test_string_template() {
        let tokens = lex_success(r#""1${a}2${b}3"{}"#);
        assert_eq!(
            dump_tokens(tokens),
            r#"TEMPLATE_LITERAL@0..4
IDENTIFIER@4..5
TEMPLATE_LITERAL@5..9
IDENTIFIER@9..10
STRING_LITERAL@10..13
L_BRACE@13..14
R_BRACE@14..15
"#
        );
    }
}
