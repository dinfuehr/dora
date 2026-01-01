use std::iter::Peekable;

use dora_parser::TokenKind;
use dora_parser::TokenKind::{LINE_COMMENT, MULTILINE_COMMENT, WHITESPACE};
use dora_parser::ast::{GreenElement, SyntaxElement, SyntaxNode, SyntaxNodeBase};

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

#[macro_export]
macro_rules! with_iter {
    ($node:expr, $f:expr, |$iter:ident, $opt:ident| $body:block) => {{
        let $opt = crate::doc::utils::Options::new();
        let mut $iter = $node.children_with_tokens().peekable();
        $body
        crate::doc::utils::print_rest($f, $iter, &$opt);
    }};
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

pub(crate) fn print_token<I>(
    f: &mut Formatter,
    iter: &mut Peekable<I>,
    until: TokenKind,
    _opt: &Options,
) where
    I: Iterator<Item = SyntaxElement>,
{
    print_trivia(f, iter);

    match iter.next() {
        Some(SyntaxElement::Token(token)) => {
            if token.syntax_kind() == until {
                f.token(token);
                return;
            }

            panic!(
                "unexpected token {} (expected {} instead)",
                token.syntax_kind(),
                until
            );
        }
        Some(SyntaxElement::Node(..)) => {
            unreachable!()
        }
        None => unreachable!(),
    }
}

pub(crate) fn print_token_opt<I>(
    f: &mut Formatter,
    iter: &mut Peekable<I>,
    kind: TokenKind,
    opt: &Options,
) -> bool
where
    I: Iterator<Item = SyntaxElement>,
{
    if if_token(f, iter, kind) {
        print_token(f, iter, kind, opt);
        true
    } else {
        false
    }
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

pub(crate) fn print_node<T, I>(f: &mut Formatter, iter: &mut Peekable<I>)
where
    I: Iterator<Item = SyntaxElement>,
    T: SyntaxNodeBase,
{
    print_trivia(f, iter);

    match iter.next() {
        Some(SyntaxElement::Node(node)) => {
            if T::cast(node.clone()).is_some() {
                format_node(node.unwrap(), f);
                return;
            }

            panic!("unexpected node {}", node.syntax_kind());
        }
        Some(SyntaxElement::Token(token)) => {
            panic!("unexpected token {}", token.syntax_kind());
        }
        None => panic!("expected node"),
    }
}

pub(crate) fn print_trivia<I>(f: &mut Formatter, iter: &mut Peekable<I>)
where
    I: Iterator<Item = SyntaxElement>,
{
    while let Some(item) = iter.peek() {
        match item {
            SyntaxElement::Token(token) => match token.syntax_kind() {
                WHITESPACE => {
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
                }
                _ => {
                    return;
                }
            },
            SyntaxElement::Node(_) => {
                return;
            }
        }
    }
}

pub(crate) fn if_token<I>(f: &mut Formatter, iter: &mut Peekable<I>, kind: TokenKind) -> bool
where
    I: Iterator<Item = SyntaxElement>,
{
    print_trivia(f, iter);
    matches!(
        iter.peek(),
        Some(SyntaxElement::Token(token)) if token.syntax_kind() == kind
    )
}

pub(crate) fn if_node<T, I>(f: &mut Formatter, iter: &mut Peekable<I>) -> bool
where
    I: Iterator<Item = SyntaxElement>,
    T: SyntaxNodeBase,
{
    print_trivia(f, iter);
    matches!(
        iter.peek(),
        Some(SyntaxElement::Node(node)) if T::cast(node.clone()).is_some()
    )
}

pub(crate) fn print_next_token<I>(f: &mut Formatter, iter: &mut Peekable<I>)
where
    I: Iterator<Item = SyntaxElement>,
{
    print_trivia(f, iter);
    let item = iter.next().expect("missing token");
    match item {
        SyntaxElement::Token(token) => {
            f.token(token);
        }
        SyntaxElement::Node(node) => {
            panic!("expected token but got node {}", node.syntax_kind());
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
                    f.token(token);
                    f.hard_line();
                }
                MULTILINE_COMMENT => {
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

pub(crate) fn print_comma_list<T, I>(
    f: &mut Formatter,
    iter: &mut Peekable<I>,
    open: TokenKind,
    closing: TokenKind,
    opt: &Options,
) where
    I: Iterator<Item = SyntaxElement>,
    T: SyntaxNodeBase,
{
    print_token(f, iter, open, opt);

    while !if_token(f, iter, closing) {
        print_node::<T, _>(f, iter);
        if if_token(f, iter, TokenKind::COMMA) {
            print_token(f, iter, TokenKind::COMMA, &opt);
            f.text(" ");
        }
    }

    print_token(f, iter, closing, opt);
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

                if kind == LINE_COMMENT || kind == MULTILINE_COMMENT {
                    return true;
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
