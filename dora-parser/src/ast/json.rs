use std::fmt::Write;

use super::*;

pub fn dump_json(file: &File, include_trivia: bool) -> String {
    let mut dumper = AstJsonDumper {
        indent: 0,
        file,
        include_trivia,
        out: String::new(),
    };
    dumper.dump_file();
    dumper.out
}

struct AstJsonDumper<'a> {
    indent: u32,
    file: &'a File,
    include_trivia: bool,
    out: String,
}

impl<'a> AstJsonDumper<'a> {
    fn dump_file(&mut self) {
        self.dump_node(self.file.root());
        self.out.push('\n');
    }

    fn dump_node(&mut self, node: SyntaxNode) {
        self.write_object_start();
        let mut first = true;

        self.write_field_name("kind", &mut first);
        self.write_json_string(&format!("{}", node.syntax_kind()));

        self.write_field_name("length", &mut first);
        let _ = write!(self.out, "{}", node.text_length());

        self.write_field_name("children", &mut first);
        self.write_array_start();
        let mut first_child = true;
        for element in node.children_with_tokens() {
            if !self.include_trivia && element.is_trivia() {
                continue;
            }

            if !first_child {
                self.out.push_str(",\n");
            } else {
                first_child = false;
            }

            self.write_indent();
            match element {
                SyntaxElement::Token(token) => self.dump_token(token),
                SyntaxElement::Node(node) => self.dump_node(node),
            }
        }
        self.write_array_end();

        self.write_object_end();
    }

    fn dump_token(&mut self, token: SyntaxToken) {
        self.write_object_start();
        let mut first = true;

        self.write_field_name("kind", &mut first);
        self.write_json_string(&format!("{}", token.syntax_kind()));

        self.write_field_name("length", &mut first);
        let _ = write!(self.out, "{}", token.text_length());

        self.write_field_name("text", &mut first);
        self.write_json_string(token.text());

        self.write_object_end();
    }

    fn write_object_start(&mut self) {
        self.out.push_str("{\n");
        self.indent += 1;
    }

    fn write_object_end(&mut self) {
        self.out.push('\n');
        self.indent -= 1;
        self.write_indent();
        self.out.push('}');
    }

    fn write_array_start(&mut self) {
        self.out.push_str("[\n");
        self.indent += 1;
    }

    fn write_array_end(&mut self) {
        self.out.push('\n');
        self.indent -= 1;
        self.write_indent();
        self.out.push(']');
    }

    fn write_field_name(&mut self, name: &str, first: &mut bool) {
        if !*first {
            self.out.push_str(",\n");
        } else {
            *first = false;
        }

        self.write_indent();
        self.write_json_string(name);
        self.out.push_str(": ");
    }

    fn write_indent(&mut self) {
        for _ in 0..self.indent {
            self.out.push_str("  ");
        }
    }

    fn write_json_string(&mut self, value: &str) {
        self.out.push('"');
        for ch in value.chars() {
            match ch {
                '"' => self.out.push_str("\\\""),
                '\\' => self.out.push_str("\\\\"),
                '\n' => self.out.push_str("\\n"),
                '\r' => self.out.push_str("\\r"),
                '\t' => self.out.push_str("\\t"),
                _ if ch.is_control() => {
                    let _ = write!(self.out, "\\u{:04x}", ch as u32);
                }
                _ => self.out.push(ch),
            }
        }
        self.out.push('"');
    }
}
