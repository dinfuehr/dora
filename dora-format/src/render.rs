use crate::doc::Doc;

pub fn render_doc(root: &Doc) -> String {
    render_doc_with_line_length(root, 90)
}

pub fn render_doc_with_line_length(root: &Doc, line_length: u32) -> String {
    let mut render = Render::new(line_length);
    render.render_node(root);
    render.finish()
}

#[derive(Clone, Copy)]
enum Mode {
    Flat,
    Break,
}

struct Render {
    out: String,
    at_line_start: bool,
    col: usize,
    line_length: u32,
}

impl Render {
    fn new(line_length: u32) -> Self {
        Self {
            out: String::new(),
            at_line_start: true,
            col: 0,
            line_length,
        }
    }

    fn finish(self) -> String {
        self.out
    }

    fn render_node(&mut self, root: &Doc) {
        let mut stack = vec![(0, Mode::Break, root)];

        while let Some((indent, mode, doc)) = stack.pop() {
            match doc {
                Doc::Concat { children } => {
                    for child in children.iter().rev() {
                        stack.push((indent, mode, child));
                    }
                }

                Doc::Nest {
                    indent: nest_indent,
                    doc,
                } => stack.push((indent + nest_indent, mode, doc.as_ref())),

                Doc::Group { doc } => {
                    if self.fits(indent, doc.as_ref(), &stack) {
                        stack.push((indent, Mode::Flat, doc.as_ref()));
                    } else {
                        stack.push((indent, Mode::Break, doc.as_ref()));
                    }
                }

                Doc::Text { text } => {
                    self.emit_text(indent, text.as_str());
                }

                Doc::SoftLine => match mode {
                    Mode::Flat => {
                        self.emit_text(indent, " ");
                    }

                    Mode::Break => {
                        self.emit_newline();
                    }
                },

                Doc::SoftBreak => match mode {
                    Mode::Flat => {}

                    Mode::Break => {
                        self.emit_newline();
                    }
                },

                Doc::IfBreak { doc } => match mode {
                    Mode::Flat => {}

                    Mode::Break => {
                        stack.push((indent, mode, doc.as_ref()));
                    }
                },

                Doc::HardLine => {
                    self.emit_newline();
                }
            }
        }
    }

    fn fits(&mut self, indent: u32, doc: &Doc, current_stack: &Vec<(u32, Mode, &Doc)>) -> bool {
        let col = if self.at_line_start {
            indent as usize
        } else {
            self.col
        };
        let mut remaining = self.line_length as i32 - col as i32;
        let mut stack = current_stack.clone();
        stack.push((indent, Mode::Flat, doc));

        while !stack.is_empty() && remaining >= 0 {
            let (indent, mode, doc) = stack.pop().expect("doc");

            match doc {
                Doc::Concat { children } => {
                    for child in children.iter().rev() {
                        stack.push((indent, mode, child));
                    }
                }

                Doc::Nest {
                    indent: nest_indent,
                    doc,
                } => stack.push((indent + nest_indent, mode, doc.as_ref())),

                Doc::Group { doc } => {
                    stack.push((indent, Mode::Flat, doc.as_ref()));
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

                Doc::SoftBreak => match mode {
                    Mode::Flat => {}

                    Mode::Break => {
                        return true;
                    }
                },

                Doc::IfBreak { doc } => match mode {
                    Mode::Flat => {}

                    Mode::Break => {
                        stack.push((indent, mode, doc.as_ref()));
                    }
                },

                Doc::HardLine => return matches!(mode, Mode::Break),
            }
        }

        remaining >= 0
    }

    fn emit_newline(&mut self) {
        while self.out.ends_with(' ') {
            self.out.pop();
        }
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
        let root = b.finish();

        let rendered = render_doc_with_line_length(&root, 10);
        assert_eq!(rendered, "aaaaa+bbbbb");

        let rendered = render_doc_with_line_length(&root, 20);
        assert_eq!(rendered, "aaaaa+bbbbb");
    }

    #[test]
    fn render_soft_line_breaks() {
        let mut b = DocBuilder::new();
        b.group(|b| {
            b.text("aaaaa");
            b.soft_line();
            b.text("bbbbb");
        });
        let root = b.finish();

        let rendered = render_doc_with_line_length(&root, 10);
        assert_eq!(rendered, "aaaaa\n    bbbbb");

        let rendered = render_doc_with_line_length(&root, 20);
        assert_eq!(rendered, "aaaaa bbbbb");
    }

    #[test]
    fn render_soft_breaks() {
        let mut b = DocBuilder::new();
        b.group(|b| {
            b.text("aaaaa");
            b.soft_break();
            b.text("bbbbb");
        });
        let root = b.finish();

        let rendered = render_doc_with_line_length(&root, 10);
        assert_eq!(rendered, "aaaaa\n    bbbbb");

        let rendered = render_doc_with_line_length(&root, 20);
        assert_eq!(rendered, "aaaaabbbbb");
    }

    #[test]
    fn render_nested() {
        let mut b = DocBuilder::new();
        b.group(|b| {
            b.text("aaaaa");
            b.nest(4, |b| {
                b.soft_break();
                b.text("bbbbb");
            });
        });
        let root = b.finish();

        let rendered = render_doc_with_line_length(&root, 10);
        assert_eq!(rendered, "aaaaa\n    bbbbb");

        let rendered = render_doc_with_line_length(&root, 20);
        assert_eq!(rendered, "aaaaa bbbbb");
    }

    #[test]
    fn render_if_break() {
        let mut b = DocBuilder::new();
        b.group(|b| {
            b.text("aaaaa");
            b.if_break(|b| {
                b.text(";");
            });
        });
        let root = b.finish();

        let rendered = render_doc_with_line_length(&root, 10);
        assert_eq!(rendered, "aaaaa\n    ;");

        let rendered = render_doc_with_line_length(&root, 20);
        assert_eq!(rendered, "aaaaa");
    }
}
