use std::iter::Peekable;

use dora_parser::TokenKind;
use dora_parser::TokenKind::{LINE_COMMENT, MULTILINE_COMMENT, WHITESPACE};
use dora_parser::ast::{SyntaxElement, SyntaxElementIter, SyntaxNodeBase, SyntaxToken};

use super::{DocId, Formatter, format_node};

pub(crate) type Iter<'a> = Peekable<SyntaxElementIter<'a>>;

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
        let mut $iter = $node.children_with_tokens().peekable();
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
    if is_token(f, iter, kind, opt) {
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
    if is_token(f, iter, kind, opt) {
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
}

pub(crate) fn collect_nodes_with_gaps<T: SyntaxNodeBase>(
    f: &mut Formatter,
    iter: &mut Iter<'_>,
    _opt: &Options,
) -> Vec<CollectElement<T>> {
    let mut elements = Vec::new();
    let mut pending_comments = Vec::<DocId>::new();

    let flush_comments = |f: &mut Formatter,
                          elements: &mut Vec<CollectElement<T>>,
                          pending_comments: &mut Vec<DocId>| {
        if pending_comments.is_empty() {
            return;
        }

        let children = std::mem::replace(pending_comments, Vec::new());

        let doc_id = f.concat_docs(children);
        elements.push(CollectElement::Comment(doc_id));
    };

    while let Some(item) = iter.peek() {
        match item {
            SyntaxElement::Token(token) => match token.syntax_kind() {
                WHITESPACE => {
                    let token = iter.next().unwrap().to_token().unwrap();
                    if count_newlines(&token) >= 2 {
                        flush_comments(f, &mut elements, &mut pending_comments);
                    }
                }
                LINE_COMMENT => {
                    let token = iter.next().unwrap().to_token().unwrap();
                    let doc_id = f.concat(|f| {
                        f.token(token);
                        f.hard_line();
                    });

                    pending_comments.push(doc_id);
                }
                MULTILINE_COMMENT => {
                    let token = iter.next().unwrap().to_token().unwrap();
                    let doc_id = f.concat(|f| {
                        f.token(token);
                        f.hard_line();
                    });
                    pending_comments.push(doc_id);
                }
                _ => break,
            },
            SyntaxElement::Node(node) => {
                flush_comments(f, &mut elements, &mut pending_comments);
                if let Some(ast_node) = T::cast(node.clone()) {
                    let node = iter.next().unwrap().to_node().unwrap();
                    let doc_id = f.concat(|f| {
                        format_node(node, f);
                    });
                    elements.push(CollectElement::Element(ast_node, doc_id));
                } else {
                    break;
                }
            }
        }
    }

    flush_comments(f, &mut elements, &mut pending_comments);
    elements
}

pub(crate) fn collect_nodes<T: SyntaxNodeBase>(
    f: &mut Formatter,
    mut iter: &mut Iter<'_>,
    opt: &Options,
) -> (Vec<(T, DocId)>, Option<DocId>) {
    let mut elements = Vec::new();

    let remainder = loop {
        let (node, doc_id) = collect_node::<T>(f, &mut iter, &opt);
        if let Some(node) = node {
            elements.push((node, doc_id.unwrap()));
        } else {
            break doc_id;
        }
    };

    (elements, remainder)
}

pub(crate) fn collect_node<T: SyntaxNodeBase>(
    f: &mut Formatter,
    iter: &mut Iter<'_>,
    opt: &Options,
) -> (Option<T>, Option<DocId>) {
    let saved = f.out.len();
    let mut found_node = None;

    while let Some(item) = iter.peek() {
        match item {
            SyntaxElement::Token(token) => match token.syntax_kind() {
                WHITESPACE => {
                    let token = iter.next().unwrap().to_token().unwrap();
                    handle_whitespace(f, token, opt);
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
                } else {
                    break;
                }
            }
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

fn handle_whitespace(f: &mut Formatter, token: SyntaxToken, opt: &Options) {
    if opt.keep_empty_lines {
        if count_newlines(&token) >= 2 {
            f.hard_line();
        }
    }
}

fn count_newlines(token: &SyntaxToken) -> usize {
    let mut newlines = 0;
    let mut chars = token.text().chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '\r' {
            if matches!(chars.peek(), Some('\n')) {
                chars.next();
            }
            newlines += 1;
        } else if ch == '\n' {
            newlines += 1;
        }
    }

    newlines
}

pub(crate) fn print_trivia(f: &mut Formatter, iter: &mut Iter<'_>, opt: &Options) {
    while let Some(item) = iter.peek() {
        match item {
            SyntaxElement::Token(token) => match token.syntax_kind() {
                WHITESPACE => {
                    let token = iter.next().unwrap().to_token().unwrap();
                    handle_whitespace(f, token, opt);
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

pub(crate) fn is_token(
    f: &mut Formatter,
    iter: &mut Iter<'_>,
    kind: TokenKind,
    opt: &Options,
) -> bool {
    print_trivia(f, iter, opt);
    matches!(
        iter.peek(),
        Some(SyntaxElement::Token(token)) if token.syntax_kind() == kind
    )
}

pub(crate) fn is_node<T: SyntaxNodeBase>(
    f: &mut Formatter,
    iter: &mut Iter<'_>,
    opt: &Options,
) -> bool {
    print_trivia(f, iter, opt);
    matches!(
        iter.peek(),
        Some(SyntaxElement::Node(node)) if T::cast(node.clone()).is_some()
    )
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
    print_token(f, iter, open, opt);
    let mut first = true;

    while !is_token(f, iter, closing, opt) {
        if !first {
            f.text(", ");
        }

        print_node::<T>(f, iter, opt);
        eat_token_opt(f, iter, TokenKind::COMMA, opt);
        first = false;
    }

    print_token(f, iter, closing, opt);
}
