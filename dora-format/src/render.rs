use id_arena::Arena;

use crate::doc::{Doc, DocId};

pub fn render_doc(arena: &Arena<Doc>, root: DocId) -> String {
    let mut render = Render::new(arena);
    render.render_node(root);
    render.finish()
}

struct Render<'a> {
    arena: &'a Arena<Doc>,
    out: String,
    indent: usize,
}

impl<'a> Render<'a> {
    fn new(arena: &'a Arena<Doc>) -> Self {
        Self {
            arena,
            out: String::new(),
            indent: 0,
        }
    }

    fn finish(self) -> String {
        self.out
    }

    fn render_node(&mut self, doc: DocId) {
        match self.arena.get(doc).expect("doc id") {
            Doc::Concat { children } => {
                for &child in children {
                    self.render_node(child);
                }
            }
            Doc::Nest {
                indent: nest_indent,
                doc,
            } => {
                let previous = self.indent;
                self.indent += *nest_indent as usize;
                self.render_node(*doc);
                self.indent = previous;
            }
            Doc::Group { doc } => {
                self.render_node(*doc);
            }
            Doc::Text { text } => {
                self.out.push_str(text);
            }
            Doc::SoftLine => {
                self.out.push(' ');
            }
            Doc::HardLine => {
                self.out.push('\n');
                for _ in 0..self.indent {
                    self.out.push(' ');
                }
            }
        }
    }
}
