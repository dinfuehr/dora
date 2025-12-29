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
    at_line_start: bool,
    col: usize,
}

impl<'a> Render<'a> {
    fn new(arena: &'a Arena<Doc>) -> Self {
        Self {
            arena,
            out: String::new(),
            indent: 0,
            at_line_start: true,
            col: 0,
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
                self.ensure_indent();
                self.render_node(*doc);
            }
            Doc::Text { text } => {
                self.ensure_indent();
                self.out.push_str(text);
            }
            Doc::SoftLine => {
                self.ensure_indent();
                self.out.push(' ');
            }
            Doc::HardLine => {
                self.out.push('\n');
                self.at_line_start = true;
                self.col = 0;
            }
        }
    }

    fn ensure_indent(&mut self) {
        if self.at_line_start {
            for _ in 0..self.indent {
                self.out.push(' ');
            }
            self.at_line_start = false;
            self.col = self.indent;
        }
    }
}
