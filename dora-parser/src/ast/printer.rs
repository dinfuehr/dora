use crate::{Span, TokenKind, compute_line_column, compute_line_starts};

use super::*;

macro_rules! dump {
    ($self_:ident, $($message:tt)*) => {{
        for _ in 0..($self_.indent * 2) {
            print!(" ");
        }

        println!($($message)*);
    }};
}

pub fn dump_file(file: &File) {
    let line_starts = compute_line_starts(file.content());
    let mut dumper = AstDumper {
        indent: 0,
        file,
        line_starts,
    };
    dumper.dump_file();
}

struct AstDumper<'a> {
    indent: u32,
    file: &'a File,
    line_starts: Vec<u32>,
}

impl<'a> AstDumper<'a> {
    fn dump_file(&mut self) {
        self.dump_node(self.file.root());
    }

    fn format_span(&self, span: Span) -> String {
        let (start_line, start_col) = compute_line_column(&self.line_starts, span.start());
        let (end_line, end_col) = compute_line_column(&self.line_starts, span.end());

        format!(
            "L{}:{}({})-L{}:{}({})",
            start_line,
            start_col,
            span.start(),
            end_line,
            end_col,
            span.end()
        )
    }

    fn dump_token(&mut self, token: SyntaxToken) {
        if token.syntax_kind() == TokenKind::STRING_LITERAL {
            dump!(self, "{} t {}", token.syntax_kind(), token.text());
        } else {
            dump!(self, "{} t {:?}", token.syntax_kind(), token.text());
        }
    }

    fn dump_node(&mut self, node: SyntaxNode) {
        let kind = node.syntax_kind();
        let span = self.format_span(node.span());
        let id_str = node.id();

        dump!(self, "{} #{} {}", kind, id_str, span);

        self.indent(|d| {
            for element in node.children_with_tokens() {
                match element {
                    SyntaxElement::Token(token) => {
                        d.dump_token(token);
                    }
                    SyntaxElement::Node(node) => {
                        d.dump_node(node);
                    }
                }
            }
        });
    }

    fn indent<F>(&mut self, fct: F)
    where
        F: Fn(&mut AstDumper) -> (),
    {
        let old = self.indent;
        self.indent = old + 1;

        fct(self);

        self.indent = old;
    }
}
