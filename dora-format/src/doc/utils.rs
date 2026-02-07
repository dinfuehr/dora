use dora_parser::TokenKind;
use dora_parser::TokenKind::{
    COMMA, IDENTIFIER, LINE_COMMENT, LIST_ITEM, MULTILINE_COMMENT, NEWLINE, PACKAGE_KW, SELF_KW,
    SUPER_KW, UPCASE_SELF_KW, WHITESPACE,
};
use dora_parser::ast::{
    AstCommaList, AstListItem, SyntaxElement, SyntaxElementIter, SyntaxNode, SyntaxNodeBase,
    SyntaxToken,
};

use crate::doc::BLOCK_INDENT;

use super::{Doc, Formatter, format_node};

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

fn is_path_segment_name(kind: TokenKind) -> bool {
    matches!(
        kind,
        IDENTIFIER | SELF_KW | UPCASE_SELF_KW | PACKAGE_KW | SUPER_KW
    )
}

pub(crate) fn print_path_segment_name(f: &mut Formatter, iter: &mut Iter<'_>, opt: &Options) {
    print_trivia(f, iter, opt);
    let token = iter
        .next()
        .expect("expected token")
        .to_token()
        .expect("expected token");
    assert!(
        is_path_segment_name(token.syntax_kind()),
        "unexpected token {} (expected path segment name)",
        token.syntax_kind()
    );
    f.token(token);
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
    Comment(Doc),
    Element(T, Doc),
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
) -> (Option<T>, Option<Doc>) {
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
        let doc = f.concat_docs(children);
        (found_node, Some(doc))
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
                if matches!(iter.peek_kind(), Some(NEWLINE)) {
                    f.hard_line();
                }
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
    print_trailing_comments(f, &mut iter);
    print_trivia(f, &mut iter, opt);
    assert!(iter.next().is_none());
}

pub(crate) fn print_trailing_comments(f: &mut Formatter, iter: &mut Iter<'_>) {
    // Skip whitespace only (not newlines) to find trailing comments on the same line
    while matches!(iter.peek_kind(), Some(WHITESPACE)) {
        iter.next();
    }

    // Check for line comment on the same line (before any newline)
    if let Some(LINE_COMMENT) = iter.peek_kind() {
        let token = iter.next().unwrap().to_token().unwrap();
        f.text(" ");
        f.token(token);
        f.hard_line();
    }
}

pub(crate) fn print_comma_list_grouped<T: AstCommaList>(
    f: &mut Formatter,
    node: &T,
    opt: &Options,
) {
    f.group(|f| {
        print_comma_list_ungrouped(f, node, opt);
    });
}

pub(crate) fn print_comma_list_ungrouped<T: AstCommaList>(
    f: &mut Formatter,
    node: &T,
    opt: &Options,
) {
    let iter = &mut node.children_with_tokens();
    print_next_token(f, iter, opt);
    f.soft_break();

    f.nest(BLOCK_INDENT, |f| {
        while is_node::<AstListItem>(iter) {
            print_trivia(f, iter, opt);
            let list_item_node = iter
                .next()
                .expect("missing list item")
                .to_node()
                .expect("expected list item node");
            assert_eq!(list_item_node.syntax_kind(), LIST_ITEM);

            let is_last = !is_node::<AstListItem>(iter);
            print_comma_list_item::<T::Item>(list_item_node, f, opt, is_last);
        }

        // Keep comments between the final list item and closing delimiter nested with the list.
        print_trivia(f, iter, opt);
    });

    print_next_token(f, iter, opt);
}

fn print_comma_list_item<T: SyntaxNodeBase>(
    node: SyntaxNode,
    f: &mut Formatter,
    opt: &Options,
    is_last: bool,
) {
    let iter = &mut node.children_with_tokens();
    print_node::<T>(f, iter, opt);
    eat_token_opt(f, iter, TokenKind::COMMA, opt);
    let trailing_comments = collect_comment_docs(iter, f);
    let has_line_comment = has_line_comment(&trailing_comments);
    assert!(iter.next().is_none());

    if !is_last {
        f.text(",");
        if trailing_comments.is_empty() {
            f.soft_line();
        } else {
            f.text(" ");
            for (doc, _) in trailing_comments {
                f.append(doc);
            }
            if !has_line_comment {
                f.soft_line();
            }
        }
    } else {
        if trailing_comments.is_empty() {
            f.if_break(|f| {
                f.text(",");
                f.soft_break();
            });
        } else {
            f.text(",");
            f.text(" ");
            for (doc, _) in trailing_comments {
                f.append(doc);
            }
            if !has_line_comment {
                f.if_break(|f| {
                    f.soft_break();
                });
            }
        }
    }
}

/// Skips whitespace and newlines only (not comments).
pub(crate) fn skip_whitespace(iter: &mut Iter<'_>) {
    while matches!(iter.peek_kind(), Some(NEWLINE | WHITESPACE)) {
        iter.next();
    }
}

/// Skips whitespace and newlines, collecting any comments as Docs.
/// Returns the collected comments and whether any line comment was found.
pub(crate) fn collect_comment_docs(
    iter: &mut Iter<'_>,
    f: &mut Formatter,
) -> Vec<(Doc, SyntaxToken)> {
    let mut comments = Vec::new();
    while let Some(kind) = iter.peek_kind() {
        match kind {
            NEWLINE | WHITESPACE => {
                iter.next();
            }
            LINE_COMMENT | MULTILINE_COMMENT => {
                let token = iter.next().unwrap().to_token().unwrap();
                let is_line_comment = kind == LINE_COMMENT;
                let doc = f.concat(|f| {
                    f.token(token.clone());
                    if is_line_comment {
                        f.hard_line();
                    }
                });
                comments.push((doc, token));
            }
            _ => break,
        }
    }
    comments
}

pub(crate) fn has_line_comment(comments: &[(Doc, SyntaxToken)]) -> bool {
    comments
        .iter()
        .any(|(_, token)| token.syntax_kind() == LINE_COMMENT)
}

/// Skips whitespace and returns the next token.
/// Panics if the next element is not a token or is trivia.
pub(crate) fn next_token(iter: &mut Iter<'_>) -> SyntaxToken {
    skip_whitespace(iter);
    let token = iter
        .next()
        .expect("expected token")
        .to_token()
        .expect("expected token");
    assert!(!token.syntax_kind().is_trivia(), "unexpected trivia token");
    token
}

/// Skips whitespace, takes the next node, and casts it to type T.
/// Panics if the next element is not a node or cannot be cast to T.
/// Call collect_comment_docs first to preserve comments.
pub(crate) fn next_node<T: SyntaxNodeBase>(iter: &mut Iter<'_>) -> T {
    skip_whitespace(iter);
    let node = iter
        .next()
        .expect("expected node")
        .to_node()
        .expect("expected node");
    T::cast(node).expect("failed to cast node")
}

pub(crate) fn format_node_as_doc<T: SyntaxNodeBase>(node: T, f: &mut Formatter) -> Doc {
    f.concat(|f| {
        format_node(node.unwrap(), f);
    })
}
