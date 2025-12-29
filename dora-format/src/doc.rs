use id_arena::{Arena, Id};

use dora_parser::TokenKind;
use dora_parser::TokenKind::*;
use dora_parser::ast::{
    AstBlock, AstElement, AstElementList, AstExpr, AstStmt, SyntaxElement, SyntaxNode,
    SyntaxNodeBase, SyntaxToken,
};

use crate::doc::utils::{has_between, needs_space, print_rest, print_until, print_while};

pub mod print;
pub(crate) mod utils;

pub(crate) type DocId = Id<Doc>;

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
        if needs_space(self.last_kind, token.syntax_kind()) {
            self.text(" ");
        }

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

    fn nest<F>(&mut self, increase: u32, fct: F) -> DocId
    where
        F: FnOnce(&mut Formatter),
    {
        let saved = self.out.len();

        fct(self);

        let children = self.out.split_off(saved);
        let children = self.concat(children);
        let nest_doc = self.arena.alloc(Doc::Nest {
            indent: increase,
            doc: children,
        });
        self.out.push(nest_doc);
        self.last_kind = TokenKind::ERROR;
        nest_doc
    }

    fn concat(&mut self, children: Vec<DocId>) -> DocId {
        if children.len() == 1 {
            children[0]
        } else {
            self.arena.alloc(Doc::Concat { children })
        }
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

fn format_element_list(node: AstElementList, f: &mut Formatter) {
    let mut iter = node.children_with_tokens().peekable();
    print_while::<AstElement, _>(&mut iter, f);
}

fn format_block(node: AstBlock, f: &mut Formatter) {
    if !has_between(node.clone().unwrap(), L_BRACE, R_BRACE) {
        format_generic_node(node.unwrap(), f);
    } else {
        let mut iter = node.children_with_tokens().peekable();
        print_until(&mut iter, f, L_BRACE);
        f.nest(BLOCK_INDENT, |f| {
            f.hard_line();
            print_while::<AstStmt, _>(&mut iter, f);
            print_while::<AstExpr, _>(&mut iter, f);
        });
        print_until(&mut iter, f, R_BRACE);
        print_rest(iter, f);
    }
}

const BLOCK_INDENT: u32 = 4;

fn format_generic_node(node: SyntaxNode, f: &mut Formatter) {
    for item in node.children_with_tokens() {
        match item {
            SyntaxElement::Token(token) => match token.syntax_kind() {
                WHITESPACE => {
                    format_whitespace(token.text(), f);
                }

                _ => {
                    f.token(token);
                }
            },

            SyntaxElement::Node(node) => {
                format_node(node, f);
            }
        }
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
