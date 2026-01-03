use std::iter::Peekable;

use dora_parser::TokenKind;
use dora_parser::TokenKind::{LINE_COMMENT, MULTILINE_COMMENT, WHITESPACE};
use dora_parser::ast::{
    GreenElement, SyntaxElement, SyntaxElementIter, SyntaxNode, SyntaxNodeBase,
};

use super::{DocId, Formatter, format_node};

pub(crate) type Iter<'a> = Peekable<SyntaxElementIter<'a>>;

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
        crate::doc::utils::print_rest($f, $iter);
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

pub(crate) fn print_token(
    f: &mut Formatter,
    iter: &mut Iter<'_>,
    until: TokenKind,
    _opt: &Options,
) {
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

pub(crate) fn print_token_opt(
    f: &mut Formatter,
    iter: &mut Iter<'_>,
    kind: TokenKind,
    opt: &Options,
) -> bool {
    if is_token(f, iter, kind) {
        print_token(f, iter, kind, opt);
        true
    } else {
        false
    }
}

pub(crate) fn eat_token_opt(f: &mut Formatter, iter: &mut Iter<'_>, kind: TokenKind) -> bool {
    if is_token(f, iter, kind) {
        iter.next().expect("expected token");
        true
    } else {
        false
    }
}

pub(crate) fn print_while<T: SyntaxNodeBase>(
    f: &mut Formatter,
    iter: &mut Iter<'_>,
    opt: &Options,
) {
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

pub(crate) fn print_node<T: SyntaxNodeBase>(f: &mut Formatter, iter: &mut Iter<'_>) {
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

#[allow(unused)]
pub(crate) fn collect_node<T: SyntaxNodeBase>(
    f: &mut Formatter,
    iter: &mut Iter<'_>,
) -> (T, DocId) {
    let saved = f.out.len();
    let mut found_node = None;

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
                    break;
                }
            },
            SyntaxElement::Node(node) => {
                if found_node.is_some() {
                    break;
                }

                if let Some(ast_node) = T::cast(node.clone()) {
                    let node = iter.next().unwrap().to_node().unwrap();
                    format_node(node, f);
                    found_node = Some(ast_node);
                }

                break;
            }
        }
    }

    let node = found_node.expect("node not found");

    let children = f.out.split_off(saved);
    let doc_id = f.concat(children);
    (node, doc_id)
}

pub(crate) fn print_trivia(f: &mut Formatter, iter: &mut Iter<'_>) {
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

pub(crate) fn is_token(f: &mut Formatter, iter: &mut Iter<'_>, kind: TokenKind) -> bool {
    print_trivia(f, iter);
    matches!(
        iter.peek(),
        Some(SyntaxElement::Token(token)) if token.syntax_kind() == kind
    )
}

pub(crate) fn is_node<T: SyntaxNodeBase>(f: &mut Formatter, iter: &mut Iter<'_>) -> bool {
    print_trivia(f, iter);
    matches!(
        iter.peek(),
        Some(SyntaxElement::Node(node)) if T::cast(node.clone()).is_some()
    )
}

pub(crate) fn print_next_token(f: &mut Formatter, iter: &mut Iter<'_>) {
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

pub(crate) fn print_rest(f: &mut Formatter, mut iter: Iter<'_>) {
    print_trivia(f, &mut iter);
    assert!(iter.next().is_none());
}

pub(crate) fn print_comma_list<T: SyntaxNodeBase>(
    f: &mut Formatter,
    iter: &mut Iter<'_>,
    open: TokenKind,
    closing: TokenKind,
    opt: &Options,
) {
    print_token(f, iter, open, opt);
    let mut first = true;

    while !is_token(f, iter, closing) {
        if !first {
            f.text(", ");
        }

        print_node::<T>(f, iter);
        eat_token_opt(f, iter, TokenKind::COMMA);
        first = false;
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
