use std::sync::Arc;

use crate::ast::*;

macro_rules! dump {
    ($self_:ident, $($message:tt)*) => {{
        for _ in 0..($self_.indent*2) {
            print!(" ");
        }

        println!($($message)*);
    }};
}

pub fn dump_file(ast: &Arc<File>) {
    let mut dumper = AstDumper {
        indent: 0,
        f: ast.as_ref(),
    };
    dumper.dump_file();
}

pub fn dump_node(f: &Arc<File>, id: AstId) {
    let mut dumper = AstDumper { indent: 0, f };
    dumper.dump_node_id(id);
}

struct AstDumper<'a> {
    indent: u32,
    f: &'a File,
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

    fn dump_node(&mut self, id: AstId, el: &Ast) {
        dump!(self, "{} {} #{}", el.name(), el.span(), id.index());
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
