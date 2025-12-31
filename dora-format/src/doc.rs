use id_arena::{Arena, Id};

use dora_parser::TokenKind;
use dora_parser::TokenKind::*;
use dora_parser::ast::*;

use crate::doc::utils::{Options, has_between, needs_space, print_rest, print_token, print_while};

pub mod print;
pub(crate) mod stmt;
pub(crate) mod utils;

pub type DocId = Id<Doc>;

#[allow(unused)]
pub enum Doc {
    Concat { children: Vec<DocId> },
    Nest { indent: u32, doc: DocId },
    Group { doc: DocId },
    Text { text: String },
    SoftLine,
    HardLine,
}

pub struct DocBuilder {
    arena: Arena<Doc>,
    out: Vec<DocId>,
}

impl DocBuilder {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
            out: Vec::new(),
        }
    }

    pub fn text(&mut self, text: impl Into<String>) -> &mut Self {
        let id = self.arena.alloc(Doc::Text { text: text.into() });
        self.out.push(id);
        self
    }

    pub fn soft_line(&mut self) -> &mut Self {
        let id = self.arena.alloc(Doc::SoftLine);
        self.out.push(id);
        self
    }

    pub fn hard_line(&mut self) -> &mut Self {
        let id = self.arena.alloc(Doc::HardLine);
        self.out.push(id);
        self
    }

    pub fn group<F>(&mut self, f: F) -> &mut Self
    where
        F: FnOnce(&mut DocBuilder),
    {
        let saved = self.out.len();
        f(self);
        let children = self.out.split_off(saved);
        let children = self.concat_doc(children);
        let id = self.arena.alloc(Doc::Group { doc: children });
        self.out.push(id);
        self
    }

    pub fn nest<F>(&mut self, indent: u32, f: F) -> &mut Self
    where
        F: FnOnce(&mut DocBuilder),
    {
        let saved = self.out.len();
        f(self);
        let children = self.out.split_off(saved);
        let children = self.concat_doc(children);
        let id = self.arena.alloc(Doc::Nest {
            indent,
            doc: children,
        });
        self.out.push(id);
        self
    }

    pub fn concat(&mut self, children: Vec<DocId>) -> &mut Self {
        let id = self.concat_doc(children);
        self.out.push(id);
        self
    }

    fn concat_doc(&mut self, children: Vec<DocId>) -> DocId {
        if children.is_empty() {
            self.arena.alloc(Doc::Text { text: "".into() })
        } else if children.len() == 1 {
            children[0]
        } else {
            self.arena.alloc(Doc::Concat { children })
        }
    }

    pub fn finish(mut self) -> (Arena<Doc>, DocId) {
        let children = std::mem::take(&mut self.out);
        let root_id = self.concat_doc(children);
        (self.arena, root_id)
    }
}

pub fn format(root: SyntaxNode) -> (Arena<Doc>, DocId) {
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
        TokenKind::LET => stmt::format_let(node.as_let(), f),
        TokenKind::EXPR_STMT => stmt::format_expr_stmt(node.as_expr_stmt(), f),
        TokenKind::LIT_INT => format_lit_int(node.as_lit_int(), f),
        _ => format_generic_node(node, f),
    }
}

fn format_lit_int(node: AstLitInt, f: &mut Formatter) {
    let opt = Options::new();
    let mut iter = node.children_with_tokens().peekable();
    print_token(f, &mut iter, INT_LITERAL, &opt);
    print_rest(f, iter, &opt);
}

fn format_element_list(node: AstElementList, f: &mut Formatter) {
    let mut iter = node.children_with_tokens().peekable();
    let opt = Options::build().emit_line_before().emit_line_after().new();
    print_while::<AstElement, _>(f, &mut iter, &opt);
}

fn format_block(node: AstBlock, f: &mut Formatter) {
    let mut iter = node.children_with_tokens().peekable();
    if !has_between(node.syntax_node(), L_BRACE, R_BRACE) {
        let opt = Options::new();
        print_token(f, &mut iter, L_BRACE, &opt);
        print_token(f, &mut iter, R_BRACE, &opt);
        print_rest(f, iter, &opt);
    } else {
        let opt = Options::build().emit_line_after().new();
        print_token(f, &mut iter, L_BRACE, &opt);
        f.hard_line();
        f.nest(BLOCK_INDENT, |f| {
            print_while::<AstStmt, _>(f, &mut iter, &opt);
            print_while::<AstExpr, _>(f, &mut iter, &opt);
        });
        print_token(f, &mut iter, R_BRACE, &opt);
        print_rest(f, iter, &opt);
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
