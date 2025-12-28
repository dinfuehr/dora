use id_arena::{Arena, Id};

use dora_parser::TokenKind;
use dora_parser::ast::{SyntaxElement, SyntaxNode};

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

struct Formatter {
    arena: Arena<Doc>,
    out: Vec<DocId>,
}

impl Formatter {
    fn text(&mut self, text: &str) -> DocId {
        let id = self.arena.alloc(Doc::Text {
            text: text.to_string(),
        });
        self.out.push(id);
        id
    }

    fn hard_line(&mut self) -> DocId {
        let id = self.arena.alloc(Doc::HardLine);
        self.out.push(id);
        id
    }

    fn finish(&mut self) -> DocId {
        let children = std::mem::replace(&mut self.out, Vec::new());
        self.arena.alloc(Doc::Concat { children })
    }
}

#[allow(unused)]
fn format(root: SyntaxNode) -> (Arena<Doc>, DocId) {
    let mut f = Formatter {
        arena: Arena::new(),
        out: Vec::new(),
    };
    format_node(root, &mut f);
    let root_id = f.finish();
    (f.arena, root_id)
}

fn format_node(node: SyntaxNode, f: &mut Formatter) {
    for item in node.children_with_tokens() {
        match item {
            SyntaxElement::Token(token) => {
                if token.syntax_kind() == TokenKind::WHITESPACE {
                    format_whitespace(token.text(), f);
                } else {
                    f.text(token.text());
                }
            }

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

    let emit = breaks.min(2);
    for _ in 0..emit {
        f.hard_line();
    }
}
