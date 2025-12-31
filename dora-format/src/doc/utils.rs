use std::iter::Peekable;

use dora_parser::TokenKind::{LINE_COMMENT, MULTILINE_COMMENT, WHITESPACE};
use dora_parser::ast::{GreenElement, SyntaxElement, SyntaxNode, SyntaxNodeBase};
use dora_parser::{TokenKind, TokenSet};

use super::{Formatter, format_node};

pub(crate) struct Options {
    emit_line_before: bool,
    emit_line_after: bool,
}

impl Options {
    pub(crate) fn new() -> Options {
        Options::default()
    }

    pub(crate) fn build() -> OptionsBuilder {
        OptionsBuilder {
            emit_line_before: false,
            emit_line_after: false,
        }
    }
}

impl Default for Options {
    fn default() -> Self {
        Options {
            emit_line_before: false,
            emit_line_after: false,
        }
    }
}

pub(crate) struct OptionsBuilder {
    emit_line_before: bool,
    emit_line_after: bool,
}

impl OptionsBuilder {
    pub(crate) fn emit_line_before(mut self) -> Self {
        self.emit_line_before = true;
        self
    }

    pub(crate) fn emit_line_after(mut self) -> Self {
        self.emit_line_after = true;
        self
    }

    pub(crate) fn new(self) -> Options {
        Options {
            emit_line_before: self.emit_line_before,
            emit_line_after: self.emit_line_after,
        }
    }
}

pub(crate) fn print_token<I>(f: &mut Formatter, iter: &mut I, until: TokenKind, opt: &Options)
where
    I: Iterator<Item = SyntaxElement>,
{
    while let Some(item) = iter.next() {
        match item {
            SyntaxElement::Token(token) => {
                if token.syntax_kind() == until {
                    f.token(token);
                    return;
                }

                match token.syntax_kind() {
                    WHITESPACE => {
                        // Ignore.
                    }
                    LINE_COMMENT => {
                        let token = iter.next().unwrap().to_token().unwrap();
                        f.token(token);
                        f.hard_line();
                    }
                    MULTILINE_COMMENT => {
                        let token = iter.next().unwrap().to_token().unwrap();
                        if opt.emit_line_after {
                            f.token(token);
                        }
                    }
                    _ => {
                        panic!(
                            "unexpected token {} (expected {} instead)",
                            token.syntax_kind(),
                            until
                        );
                    }
                }
            }

            SyntaxElement::Node(..) => {
                unreachable!()
            }
        }
    }

    unreachable!()
}

pub(crate) fn print_while<T, I>(f: &mut Formatter, iter: &mut Peekable<I>, opt: &Options)
where
    I: Iterator<Item = SyntaxElement>,
    T: SyntaxNodeBase,
{
    let mut first = true;
    while let Some(item) = iter.peek() {
        match item {
            SyntaxElement::Node(node) => {
                if T::cast(node.clone()).is_some() {
                    let item = iter.next().unwrap();
                    if !first && opt.emit_line_before {
                        f.hard_line();
                    }
                    first = false;
                    format_node(item.to_node().unwrap(), f);
                    if opt.emit_line_after {
                        f.hard_line();
                    }
                    continue;
                }

                break;
            }

            SyntaxElement::Token(token) => match token.syntax_kind() {
                WHITESPACE => {
                    // Move forward and ignore.
                    iter.next().unwrap();
                }
                LINE_COMMENT => {
                    let token = iter.next().unwrap().to_token().unwrap();
                    f.token(token);
                    f.hard_line();
                }
                MULTILINE_COMMENT => {
                    let token = iter.next().unwrap().to_token().unwrap();
                    f.token(token);
                    if opt.emit_line_after {
                        f.hard_line();
                    }
                }
                _ => {
                    return;
                }
            },
        }
    }
}

pub(crate) fn print_next_token<I>(f: &mut Formatter, iter: &mut Peekable<I>)
where
    I: Iterator<Item = SyntaxElement>,
{
    while let Some(item) = iter.next() {
        match item {
            SyntaxElement::Token(token) => match token.syntax_kind() {
                WHITESPACE => {
                    // Ignore.
                }
                LINE_COMMENT => {
                    let token = iter.next().unwrap().to_token().unwrap();
                    f.token(token);
                    f.hard_line();
                }
                MULTILINE_COMMENT => {
                    let token = iter.next().unwrap().to_token().unwrap();
                    f.token(token);
                }
                _ => {
                    f.token(token);
                    return;
                }
            },
            SyntaxElement::Node(_) => {
                // Skip nodes until we find a token.
            }
        }
    }
}

pub(crate) fn print_rest<I>(f: &mut Formatter, mut iter: I, opt: &Options)
where
    I: Iterator<Item = SyntaxElement>,
{
    while let Some(item) = iter.next() {
        match item {
            SyntaxElement::Token(token) => match token.syntax_kind() {
                WHITESPACE => {
                    // Ignore.
                }
                LINE_COMMENT => {
                    let token = iter.next().unwrap().to_token().unwrap();
                    f.token(token);
                    f.hard_line();
                }
                MULTILINE_COMMENT => {
                    let token = iter.next().unwrap().to_token().unwrap();
                    f.token(token);
                    if opt.emit_line_after {
                        f.hard_line();
                    }
                }
                _ => unreachable!(),
            },

            SyntaxElement::Node(..) => {
                unreachable!()
            }
        }
    }
}

pub(crate) fn has_between(node: &SyntaxNode, start: TokenKind, end: TokenKind) -> bool {
    let mut saw_start = false;

    for item in node.green().children() {
        match item {
            GreenElement::Token(token) => {
                let kind = token.kind;

                if kind == WHITESPACE {
                    continue;
                }

                if saw_start {
                    return kind != end;
                } else if kind == start {
                    saw_start = true;
                }
            }
            GreenElement::Node(_) => {
                if saw_start {
                    return true;
                }
            }
        }
    }

    false
}

const NONE: TokenKind = TokenKind::ERROR;

const NO_SPACE_BEFORE: TokenSet = TokenSet::new(&[
    TokenKind::R_PAREN,
    TokenKind::R_BRACE,
    TokenKind::R_BRACKET,
    TokenKind::COMMA,
    TokenKind::DOT,
    TokenKind::SEMICOLON,
    TokenKind::COLON,
    TokenKind::COLON_COLON,
]);
const NO_SPACE_AFTER: TokenSet = TokenSet::new(&[
    TokenKind::L_PAREN,
    TokenKind::L_BRACKET,
    TokenKind::L_BRACE,
    TokenKind::DOT,
]);

pub(crate) fn needs_space(last: TokenKind, next: TokenKind) -> bool {
    if last == NONE {
        return false;
    }

    if NO_SPACE_AFTER.contains(last) {
        return false;
    }

    if NO_SPACE_BEFORE.contains(next) {
        return false;
    }

    if last == TokenKind::IDENTIFIER && next == TokenKind::L_PAREN {
        return false;
    }

    true
}
