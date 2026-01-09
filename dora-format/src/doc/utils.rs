use dora_parser::TokenKind;
use dora_parser::TokenKind::{COMMA, LINE_COMMENT, MULTILINE_COMMENT, NEWLINE, WHITESPACE};
use dora_parser::ast::{SyntaxElement, SyntaxElementIter, SyntaxNodeBase};

use crate::doc::BLOCK_INDENT;

use super::{DocId, Formatter, format_node};

pub(crate) type Iter<'a> = SyntaxElementIter<'a>;

pub(crate) struct Options {
    keep_empty_lines: bool,
}

impl Options {
    pub(crate) fn new() -> Options {
        Options {
            keep_empty_lines: false,
        }
    }

    pub(crate) fn keep_empty_lines() -> Options {
        Options {
            keep_empty_lines: true,
        }
    }
}

#[macro_export]
macro_rules! with_iter {
    ($node:expr, $f:expr, |$iter:ident, $opt:ident| $body:block) => {{
        let $opt = crate::doc::utils::Options::new();
        let mut $iter = $node.children_with_tokens();
        $body
        crate::doc::utils::print_rest($f, $iter, &$opt);
    }};
}

pub(crate) fn print_token(f: &mut Formatter, iter: &mut Iter<'_>, until: TokenKind, opt: &Options) {
    print_trivia(f, iter, opt);

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
    if is_token(iter, kind) {
        print_token(f, iter, kind, opt);
        true
    } else {
        false
    }
}

pub(crate) fn eat_token(f: &mut Formatter, iter: &mut Iter<'_>, kind: TokenKind, opt: &Options) {
    assert!(eat_token_opt(f, iter, kind, opt));
}

pub(crate) fn eat_token_opt(
    f: &mut Formatter,
    iter: &mut Iter<'_>,
    kind: TokenKind,
    opt: &Options,
) -> bool {
    if is_token(iter, kind) {
        print_trivia(f, iter, opt);
        iter.next().expect("expected token");
        true
    } else {
        false
    }
}

pub(crate) fn print_node<T: SyntaxNodeBase>(f: &mut Formatter, iter: &mut Iter<'_>, opt: &Options) {
    print_trivia(f, iter, opt);

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

pub(crate) enum CollectElement<T> {
    Comment(DocId),
    Element(T, DocId),
    Gap,
}

impl<T> CollectElement<T> {
    pub(crate) fn is_gap(&self) -> bool {
        match self {
            CollectElement::Gap => true,
            _ => false,
        }
    }

    pub(crate) fn is_element(&self) -> bool {
        match self {
            CollectElement::Element(..) => true,
            _ => false,
        }
    }

    pub(crate) fn is_comment(&self) -> bool {
        match self {
            CollectElement::Comment(..) => true,
            _ => false,
        }
    }
}

pub(crate) fn collect_nodes<T: SyntaxNodeBase>(
    f: &mut Formatter,
    iter: &mut Iter<'_>,
    _opt: &Options,
    allow_comma: bool,
) -> Vec<CollectElement<T>> {
    let mut elements = Vec::new();

    while let Some(kind) = iter.peek_kind() {
        match kind {
            NEWLINE => {
                let newline_count = consume_newlines(iter);
                if newline_count >= 2 {
                    elements.push(CollectElement::Gap);
                }
            }
            WHITESPACE => {
                iter.next().unwrap();
            }
            LINE_COMMENT => {
                let token = iter.next().unwrap().to_token().unwrap();
                let doc_id = f.concat(|f| {
                    f.token(token);
                });
                elements.push(CollectElement::Comment(doc_id));
            }
            MULTILINE_COMMENT => {
                let token = iter.next().unwrap().to_token().unwrap();
                let doc_id = f.concat(|f| {
                    f.token(token);
                });
                elements.push(CollectElement::Comment(doc_id));
            }
            COMMA if allow_comma => {
                iter.next().unwrap();
            }
            _ if T::can_cast(kind) => {
                let node = iter.next().unwrap().to_node().unwrap();
                let doc_id = f.concat(|f| {
                    format_node(node.clone(), f);
                });
                let node = T::cast(node).unwrap();
                elements.push(CollectElement::Element(node, doc_id));
            }
            _ => break,
        }
    }

    while matches!(elements.first(), Some(CollectElement::Gap)) {
        elements.remove(0);
    }

    while matches!(elements.last(), Some(CollectElement::Gap)) {
        elements.pop();
    }

    elements
}

pub(crate) fn collect_node<T: SyntaxNodeBase>(
    f: &mut Formatter,
    iter: &mut Iter<'_>,
    opt: &Options,
) -> (Option<T>, Option<DocId>) {
    let saved = f.out.len();
    let mut found_node = None;

    while let Some(kind) = iter.peek_kind() {
        match kind {
            NEWLINE => {
                let newline_count = consume_newlines(iter);
                handle_newlines(f, newline_count, opt);
            }
            WHITESPACE => {
                iter.next();
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

            _ if T::can_cast(kind) => {
                if found_node.is_some() {
                    break;
                }

                let node = iter.next().unwrap().to_node().unwrap();
                let ast_node = T::cast(node.clone()).unwrap();
                format_node(node, f);
                found_node = Some(ast_node);
            }

            _ => break,
        }
    }

    if saved == f.out.len() {
        (found_node, None)
    } else {
        let children = f.out.split_off(saved);
        let doc_id = f.concat_docs(children);
        (found_node, Some(doc_id))
    }
}

fn handle_newlines(f: &mut Formatter, count: usize, opt: &Options) {
    if opt.keep_empty_lines && count >= 2 {
        f.hard_line();
    }
}

fn consume_newlines(iter: &mut Iter<'_>) -> usize {
    let mut count = 1;
    assert_eq!(iter.peek_kind(), Some(NEWLINE));
    iter.next();

    loop {
        match iter.peek_kind() {
            Some(NEWLINE) => {
                iter.next();
                count += 1;
            }
            Some(WHITESPACE) => {
                iter.next();
            }
            _ => break,
        }
    }

    count
}

pub(crate) fn print_trivia(f: &mut Formatter, iter: &mut Iter<'_>, opt: &Options) {
    while let Some(item) = iter.peek_kind() {
        match item {
            NEWLINE => {
                let newline_count = consume_newlines(iter);
                handle_newlines(f, newline_count, opt);
            }
            WHITESPACE => {
                iter.next();
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
        }
    }
}

pub(crate) fn is_token(iter: &Iter<'_>, kind: TokenKind) -> bool {
    if let Some(next) = iter.peek_kind_ignore_trivia() {
        next == kind
    } else {
        false
    }
}

pub(crate) fn is_node<T: SyntaxNodeBase>(iter: &Iter<'_>) -> bool {
    if let Some(next) = iter.peek_kind_ignore_trivia() {
        T::can_cast(next)
    } else {
        false
    }
}

pub(crate) fn print_next_token(f: &mut Formatter, iter: &mut Iter<'_>, opt: &Options) {
    print_trivia(f, iter, opt);
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

pub(crate) fn print_rest(f: &mut Formatter, mut iter: Iter<'_>, opt: &Options) {
    print_trivia(f, &mut iter, opt);
    assert!(iter.next().is_none());
}

pub(crate) fn print_comma_list<T: SyntaxNodeBase>(
    f: &mut Formatter,
    iter: &mut Iter<'_>,
    open: TokenKind,
    closing: TokenKind,
    opt: &Options,
) {
    print_trivia(f, iter, opt);
    f.group(|f| {
        print_token(f, iter, open, opt);
        f.soft_break();

        f.nest(BLOCK_INDENT, |f| {
            while is_node::<T>(iter) {
                print_node::<T>(f, iter, opt);
                eat_token_opt(f, iter, TokenKind::COMMA, opt);

                if is_node::<T>(iter) {
                    f.text(",");
                    f.soft_line();
                } else {
                    f.if_break(|f| {
                        f.text(",");
                        f.soft_break();
                    });
                }
            }
        });

        print_token(f, iter, closing, opt);
    });
}
