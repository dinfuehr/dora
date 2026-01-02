use id_arena::Arena;

use crate::doc::{Doc, DocId};

pub fn print_doc_to_string(arena: &Arena<Doc>, root: DocId) -> String {
    let mut out = String::new();
    let mut printer = Printer::new(arena, &mut out);
    let _ = printer.print_node(root);
    out
}

struct Printer<'a> {
    arena: &'a Arena<Doc>,
    writer: &'a mut String,
}

impl<'a> Printer<'a> {
    fn new(arena: &'a Arena<Doc>, writer: &'a mut String) -> Self {
        Self { arena, writer }
    }

    fn print_node(&mut self, doc: DocId) -> Result<(), ()> {
        match self.arena.get(doc).expect("doc id") {
            Doc::Concat { children } => {
                for (index, &child) in children.iter().enumerate() {
                    if index > 0 {
                        self.writer.push_str(" + ");
                    }
                    self.print_node(child)?;
                }
            }
            Doc::Nest {
                indent: nest_indent,
                doc,
            } => {
                self.writer.push_str("Nest(");
                self.writer.push_str(&format!("{}", nest_indent));
                self.writer.push_str(", ");
                self.print_node(*doc)?;
                self.writer.push_str(")");
            }
            Doc::Group { doc } => {
                self.writer.push_str("Group(");
                self.print_node(*doc)?;
                self.writer.push_str(")");
            }
            Doc::Text { text } => {
                self.write_escaped_text(text);
            }
            Doc::SoftLine => {
                self.writer.push_str("SL");
            }
            Doc::SoftBreak => {
                self.writer.push_str("SB");
            }
            Doc::IfBreak { doc } => {
                self.writer.push_str("IfBreak(");
                self.print_node(*doc)?;
                self.writer.push_str(")");
            }
            Doc::HardLine => {
                self.writer.push_str("HL");
            }
        }

        Ok(())
    }

    fn write_escaped_text(&mut self, text: &str) {
        self.writer.push_str("\"");

        for ch in text.chars() {
            let mut buf = [0u8; 4];
            let escaped = ch.escape_default();
            for esc in escaped {
                let len = esc.encode_utf8(&mut buf).len();
                self.writer
                    .push_str(std::str::from_utf8(&buf[..len]).unwrap());
            }
        }

        self.writer.push_str("\"");
    }
}
