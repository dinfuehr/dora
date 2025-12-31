use id_arena::Arena;

use crate::doc::{Doc, DocId};

pub fn render_doc(arena: &Arena<Doc>, root: DocId) -> String {
    render_doc_with_line_length(arena, root, 80)
}

pub fn render_doc_with_line_length(arena: &Arena<Doc>, root: DocId, line_length: u32) -> String {
    let mut render = Render::new(arena, line_length);
    render.render_node(root);
    render.finish()
}

#[derive(Clone, Copy)]
enum Mode {
    Flat,
    Break,
}

struct Render<'a> {
    arena: &'a Arena<Doc>,
    out: String,
    at_line_start: bool,
    col: usize,
    line_length: u32,
}

impl<'a> Render<'a> {
    fn new(arena: &'a Arena<Doc>, line_length: u32) -> Self {
        Self {
            arena,
            out: String::new(),
            at_line_start: true,
            col: 0,
            line_length,
        }
    }

    fn finish(self) -> String {
        self.out
    }

    fn render_node(&mut self, doc_id: DocId) {
        let mut stack = vec![(0, Mode::Break, doc_id)];

        while let Some((indent, mode, doc_id)) = stack.pop() {
            let doc = self.arena.get(doc_id).expect("doc id");

            match doc {
                Doc::Concat { children } => {
                    for &child in children.iter().rev() {
                        stack.push((indent, mode, child));
                    }
                }

                Doc::Nest {
                    indent: nest_indent,
                    doc: doc_id,
                } => stack.push((indent + nest_indent, mode, *doc_id)),

                Doc::Group { doc } => {
                    if self.fits(indent, *doc, &stack) {
                        stack.push((indent, Mode::Flat, *doc));
                    } else {
                        stack.push((indent, Mode::Break, *doc));
                    }
                }

                Doc::Text { text } => {
                    self.emit_text(indent, text);
                }

                Doc::SoftLine => match mode {
                    Mode::Flat => {
                        self.emit_text(indent, " ");
                    }

                    Mode::Break => {
                        self.emit_newline();
                    }
                },

                Doc::HardLine => {
                    self.emit_newline();
                }
            }
        }
    }

    fn fits(
        &mut self,
        indent: u32,
        doc_id: DocId,
        current_stack: &Vec<(u32, Mode, DocId)>,
    ) -> bool {
        let mut remaining = self.line_length as i32 - self.col as i32;
        let mut stack = current_stack.clone();
        stack.push((indent, Mode::Flat, doc_id));

        while !stack.is_empty() && remaining >= 0 {
            let (indent, mode, doc_id) = stack.pop().expect("doc id");
            let doc = self.arena.get(doc_id).expect("doc id");

            match doc {
                Doc::Concat { children } => {
                    for &child in children.iter().rev() {
                        stack.push((indent, mode, child));
                    }
                }

                Doc::Nest {
                    indent: nest_indent,
                    doc: doc_id,
                } => stack.push((indent + nest_indent, mode, *doc_id)),

                Doc::Group { doc } => {
                    stack.push((indent, Mode::Flat, *doc));
                }

                Doc::Text { text } => {
                    remaining -= text.chars().count() as i32;
                }

                Doc::SoftLine => match mode {
                    Mode::Flat => {
                        remaining -= 1;
                    }

                    Mode::Break => {
                        return true;
                    }
                },

                Doc::HardLine => return matches!(mode, Mode::Break),
            }
        }

        remaining >= 0
    }

    fn emit_newline(&mut self) {
        self.out.push('\n');
        self.at_line_start = true;
        self.col = 0;
    }

    fn emit_text(&mut self, indent: u32, str: &str) {
        self.ensure_indent(indent);
        self.out.push_str(str);
        self.col += str.chars().count();
    }

    fn ensure_indent(&mut self, indent: u32) {
        if self.at_line_start {
            for _ in 0..indent {
                self.out.push(' ');
            }
            self.at_line_start = false;
            self.col = indent as usize;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::doc::DocBuilder;

    #[test]
    fn render_concat() {
        let mut b = DocBuilder::new();
        b.text("aaaaa").text("+").text("bbbbb");
        let (arena, root) = b.finish();

        let rendered = render_doc_with_line_length(&arena, root, 10);
        assert_eq!(rendered, "aaaaa+bbbbb");

        let rendered = render_doc_with_line_length(&arena, root, 20);
        assert_eq!(rendered, "aaaaa+bbbbb");
    }

    #[test]
    fn render_group() {
        let mut b = DocBuilder::new();
        b.group(|b| {
            b.text("aaaaa")
                .soft_line()
                .text("+")
                .soft_line()
                .text("bbbbb");
        });
        let (arena, root) = b.finish();

        let rendered = render_doc_with_line_length(&arena, root, 20);
        assert_eq!(rendered, "aaaaa + bbbbb");

        let rendered = render_doc_with_line_length(&arena, root, 10);
        assert_eq!(rendered, "aaaaa\n+\nbbbbb");
    }

    #[test]
    fn render_group_affects_whole_line() {
        let mut b = DocBuilder::new();
        b.group(|b| {
            b.text("foo").soft_line().text("bar");
        })
        .text("baz");
        let (arena, root) = b.finish();

        let rendered = render_doc_with_line_length(&arena, root, 10);
        assert_eq!(rendered, "foo barbaz");

        let rendered = render_doc_with_line_length(&arena, root, 8);
        assert_eq!(rendered, "foo\nbarbaz");
    }

    #[test]
    fn render_group_affects_up_to_hard_line() {
        let mut b = DocBuilder::new();
        b.group(|b| {
            b.text("foo").soft_line().text("bar");
        })
        .text("baz")
        .hard_line()
        .text("next");
        let (arena, root) = b.finish();

        let rendered = render_doc_with_line_length(&arena, root, 10);
        assert_eq!(rendered, "foo barbaz\nnext");

        let rendered = render_doc_with_line_length(&arena, root, 8);
        assert_eq!(rendered, "foo\nbarbaz\nnext");
    }

    #[test]
    fn render_breaks_group_with_nested_softlines() {
        let long = "a".repeat(11);

        let mut b = DocBuilder::new();
        b.group(|b| {
            b.text("let").soft_line();
            b.nest(4, |b| {
                b.text("x")
                    .soft_line()
                    .text("=")
                    .soft_line()
                    .text(long.clone());
            });
        });
        let (arena, root) = b.finish();

        let rendered = render_doc_with_line_length(&arena, root, 10);
        let expected = format!("let\n    x\n    =\n    {}", long);

        assert_eq!(rendered, expected);
    }

    #[test]
    fn render_nest() {
        let mut b = DocBuilder::new();
        b.nest(4, |b| {
            b.text("foo");
        });
        let (arena, root) = b.finish();

        let rendered = render_doc_with_line_length(&arena, root, 80);
        assert_eq!(rendered, "    foo");
    }

    #[test]
    fn render_nest_after_text() {
        let mut b = DocBuilder::new();
        b.text("bar");
        b.nest(4, |b| {
            b.text("foo");
        });
        let (arena, root) = b.finish();

        let rendered = render_doc_with_line_length(&arena, root, 80);
        assert_eq!(rendered, "barfoo");
    }

    #[test]
    fn render_nest_after_hard_line() {
        let mut b = DocBuilder::new();
        b.hard_line();
        b.nest(4, |b| {
            b.text("foo");
        });
        let (arena, root) = b.finish();

        let rendered = render_doc_with_line_length(&arena, root, 80);
        assert_eq!(rendered, "\n    foo");
    }
}
