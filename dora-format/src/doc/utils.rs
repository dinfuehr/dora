use std::iter::Peekable;

use dora_parser::TokenKind::{LINE_COMMENT, MULTILINE_COMMENT, WHITESPACE};
use dora_parser::ast::{SyntaxElement, SyntaxNode, SyntaxNodeBase};
use dora_parser::{TokenKind, TokenSet};

use super::{Formatter, format_node};

pub(crate) fn print_until<I>(iter: &mut I, f: &mut Formatter, until: TokenKind)
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
                        f.token(token);
                    }
                    _ => unreachable!(),
                }
            }

            SyntaxElement::Node(..) => {
                unreachable!()
            }
        }
    }

    unreachable!()
}

pub(crate) fn print_while<T, I>(iter: &mut Peekable<I>, f: &mut Formatter)
where
    I: Iterator<Item = SyntaxElement>,
    T: SyntaxNodeBase,
{
    while let Some(item) = iter.peek() {
        match item {
            SyntaxElement::Node(node) => {
                if T::cast(node.clone()).is_some() {
                    let item = iter.next().unwrap();
                    format_node(item.to_node().unwrap(), f);
                    f.hard_line();
                    continue;
                }

                break;
            }

            SyntaxElement::Token(token) => match token.syntax_kind() {
                WHITESPACE => {
                    // Move forward and ignore.
                    iter.next().unwrap();
                }
                LINE_COMMENT | MULTILINE_COMMENT => {
                    let token = iter.next().unwrap().to_token().unwrap();
                    f.token(token);
                    f.hard_line();
                }
                _ => {
                    return;
                }
            },
        }
    }
}

pub(crate) fn print_rest<I>(mut iter: I, f: &mut Formatter)
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
                _ => unreachable!(),
            },

            SyntaxElement::Node(..) => {
                unreachable!()
            }
        }
    }
}

pub(crate) fn has_between(node: SyntaxNode, start: TokenKind, end: TokenKind) -> bool {
    let mut saw_start = false;

    for item in node.children_with_tokens() {
        match item {
            SyntaxElement::Token(token) => {
                let kind = token.syntax_kind();

                if kind == WHITESPACE {
                    continue;
                }

                if saw_start {
                    return kind != end;
                } else if kind == start {
                    saw_start = true;
                }
            }
            SyntaxElement::Node(_) => {
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
