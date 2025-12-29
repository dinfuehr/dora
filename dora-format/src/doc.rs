use id_arena::{Arena, Id};

use dora_parser::TokenKind::*;
use dora_parser::ast::{self, SyntaxElement, SyntaxNode, SyntaxNodeBase, SyntaxToken};
use dora_parser::{TokenKind, TokenSet};

pub type DocId = Id<Doc>;

#[allow(unused)]
pub(crate) enum Doc {
    Concat { children: Vec<DocId> },
    Nest { indent: u32, doc: DocId },
    Group { doc: DocId },
    Text { text: String },
    SoftLine,
    HardLine,
}

pub(crate) fn format(root: SyntaxNode) -> (Arena<Doc>, DocId) {
    let mut f = Formatter::new();
    format_node(root, &mut f);
    f.finish()
}

pub(crate) struct Formatter {
    arena: Arena<Doc>,
    out: Vec<DocId>,
    last_kind: TokenKind,
    in_block: bool,
}

impl Formatter {
    pub(crate) fn new() -> Self {
        Self {
            arena: Arena::new(),
            out: Vec::new(),
            last_kind: TokenKind::ERROR,
            in_block: false,
        }
    }

    fn token(&mut self, token: SyntaxToken) -> DocId {
        let id = self.arena.alloc(Doc::Text {
            text: token.text().to_string(),
        });
        self.out.push(id);
        self.last_kind = token.syntax_kind();
        id
    }

    fn text(&mut self, text: &str) -> DocId {
        let id = self.arena.alloc(Doc::Text {
            text: text.to_string(),
        });
        self.out.push(id);
        self.last_kind = TokenKind::ERROR;
        id
    }

    fn hard_line(&mut self) -> DocId {
        let id = self.arena.alloc(Doc::HardLine);
        self.out.push(id);
        self.last_kind = TokenKind::ERROR;
        id
    }

    pub(crate) fn finish(mut self) -> (Arena<Doc>, DocId) {
        let children = std::mem::replace(&mut self.out, Vec::new());
        let root_id = self.arena.alloc(Doc::Concat { children });
        (self.arena, root_id)
    }
}

fn format_node(node: SyntaxNode, f: &mut Formatter) {
    match node.syntax_kind() {
        TokenKind::ELEMENT_LIST => format_element_list(node.as_element_list(), f),
        TokenKind::BLOCK => format_block(node.as_block(), f),
        _ => format_generic_node(node, f),
    }
}

fn format_element_list(node: ast::AstElementList, f: &mut Formatter) {
    for item in node.children_with_tokens() {
        match item {
            SyntaxElement::Token(token) => {
                format_generic_token(token, f);
            }

            SyntaxElement::Node(node) => {
                format_node(node, f);
            }
        }
    }
}

fn format_block(node: ast::AstBlock, f: &mut Formatter) {
    if !has_between(node.clone().unwrap(), L_BRACE, R_BRACE) {
        format_generic_node(node.unwrap(), f);
    } else {
        let mut iter = node.children_with_tokens().peekable();
        while let Some(item) = iter.next() {
            match item {
                SyntaxElement::Token(token) => {
                    if token.syntax_kind() == TokenKind::L_BRACE {
                        if needs_space(f.last_kind, token.syntax_kind()) {
                            f.text(" ");
                        }

                        f.token(token);

                        let outer_out = std::mem::take(&mut f.out);
                        f.out = Vec::new();
                        f.hard_line();

                        let previous_in_block = f.in_block;
                        f.in_block = true;

                        let mut r_brace = None;
                        while let Some(inner_item) = iter.next() {
                            match inner_item {
                                SyntaxElement::Token(token) => {
                                    if token.syntax_kind() == TokenKind::R_BRACE {
                                        r_brace = Some(token);
                                        break;
                                    } else if token.syntax_kind() == TokenKind::WHITESPACE {
                                        continue;
                                    } else {
                                        format_generic_token(token, f);
                                    }
                                }
                                SyntaxElement::Node(node) => {
                                    format_node(node, f);
                                }
                            }
                        }

                        let inner_out = std::mem::take(&mut f.out);
                        f.out = outer_out;

                        let inner_doc = f.arena.alloc(Doc::Concat {
                            children: inner_out,
                        });
                        let nest_doc = f.arena.alloc(Doc::Nest {
                            indent: BLOCK_INDENT,
                            doc: inner_doc,
                        });
                        f.out.push(nest_doc);
                        f.last_kind = TokenKind::ERROR;

                        if let Some(token) = r_brace {
                            f.hard_line();
                            f.token(token);
                        }

                        f.in_block = previous_in_block;
                    } else if token.syntax_kind() == TokenKind::R_BRACE {
                        f.hard_line();
                        f.token(token);
                    } else if token.syntax_kind() == WHITESPACE {
                        continue;
                    } else {
                        format_generic_token(token, f);
                    }
                }

                SyntaxElement::Node(node) => {
                    format_node(node, f);
                }
            }
        }
    }
}

const BLOCK_INDENT: u32 = 4;

fn format_generic_node(node: SyntaxNode, f: &mut Formatter) {
    for item in node.children_with_tokens() {
        match item {
            SyntaxElement::Token(token) => {
                if token.syntax_kind() == TokenKind::WHITESPACE {
                    format_whitespace(token.text(), f);
                } else {
                    if needs_space(f.last_kind, token.syntax_kind()) {
                        f.text(" ");
                    }

                    f.token(token);
                }
            }

            SyntaxElement::Node(node) => {
                format_node(node, f);
            }
        }
    }
}

fn format_generic_token(token: SyntaxToken, f: &mut Formatter) {
    if token.syntax_kind() == TokenKind::WHITESPACE {
        format_whitespace(token.text(), f);
    } else {
        if needs_space(f.last_kind, token.syntax_kind()) {
            f.text(" ");
        }

        f.token(token);
    }
}

fn format_whitespace(text: &str, f: &mut Formatter) {
    let bytes = text.as_bytes();
    let mut i = 0;
    let mut breaks = 0;

    while i < bytes.len() {
        match bytes[i] {
            b'\r' => {
                breaks += 1;
                if i + 1 < bytes.len() && bytes[i + 1] == b'\n' {
                    i += 2;
                } else {
                    i += 1;
                }
            }
            b'\n' => {
                breaks += 1;
                i += 1;
            }
            _ => {
                i += 1;
            }
        }
    }

    let emit = if f.in_block {
        if breaks >= 2 { 1 } else { 0 }
    } else {
        breaks.min(2)
    };

    for _ in 0..emit {
        f.hard_line();
    }
}

fn has_between(node: SyntaxNode, start: TokenKind, end: TokenKind) -> bool {
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
                } else {
                    if kind == start {
                        saw_start = true;
                    }
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

fn needs_space(last: TokenKind, next: TokenKind) -> bool {
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
