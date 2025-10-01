use std::sync::Arc;

use crate::ast::*;
use crate::{Span, compute_line_column, compute_line_starts};

macro_rules! dump {
    ($self_:ident, $($message:tt)*) => {{
        for _ in 0..($self_.indent*2) {
            print!(" ");
        }

        println!($($message)*);
    }};
}

pub fn dump_file(ast: &Arc<File>) {
    let line_starts = compute_line_starts(&ast.content);
    let mut dumper = AstDumper {
        indent: 0,
        f: ast.as_ref(),
        line_starts,
    };
    dumper.dump_file();
}

pub fn dump_node(f: &Arc<File>, id: AstId) {
    let line_starts = compute_line_starts(&f.content);
    let mut dumper = AstDumper {
        indent: 0,
        f,
        line_starts,
    };
    dumper.dump_node_id(id);
}

struct AstDumper<'a> {
    indent: u32,
    f: &'a File,
    line_starts: Vec<u32>,
}

impl<'a> AstDumper<'a> {
    fn dump_file(&mut self) {
        for &element_id in &self.f.elements {
            self.dump_node_id(element_id);
        }
    }

    fn dump_node_id(&mut self, id: AstId) {
        self.dump_node(id, self.f.node(id));
    }

    fn format_span(&self, span: Span) -> String {
        let (start_line, start_col) = compute_line_column(&self.line_starts, span.start());
        let (end_line, end_col) = compute_line_column(&self.line_starts, span.end());
        format!("L{}:{}-L{}:{}", start_line, start_col, end_line, end_col)
    }

    fn node_extra_info(&self, el: &Ast) -> Option<String> {
        match el {
            Ast::LitChar(lit) => Some(lit.value.clone()),
            Ast::LitInt(lit) => Some(lit.value.clone()),
            Ast::LitFloat(lit) => Some(lit.value.clone()),
            Ast::LitStr(lit) => Some(format!("{:?}", lit.value)),
            Ast::LitBool(lit) => Some(lit.value.to_string()),
            Ast::Ident(ident) => Some(ident.name.clone()),
            Ast::Bin(bin) => Some(bin.op.as_str().to_string()),
            Ast::Un(un) => Some(un.op.as_str().to_string()),
            _ => None,
        }
    }

    fn dump_node(&mut self, id: AstId, el: &Ast) {
        let name = el.name();
        let span = self.format_span(el.span());
        let id_str = id.index();

        if let Some(extra) = self.node_extra_info(el) {
            dump!(self, "{} {} #{} {}", name, extra, id_str, span);
        } else {
            dump!(self, "{} #{} {}", name, id_str, span);
        }

        let children = el.children();
        if !children.is_empty() {
            self.indent(|d| {
                for &child_id in &children {
                    d.dump_node_id(child_id);
                }
            });
        }
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
