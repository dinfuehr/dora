use crate::ctxt::{GlobalId, NodeMap, SemContext};
use crate::semck;
use crate::ty::BuiltinType;
use dora_parser::ast::visit::Visitor;
use dora_parser::ast::Elem::ElemGlobal;
use dora_parser::ast::{Ast, File, Global};
use dora_parser::error::msg::Msg;

pub fn check<'a, 'ast>(
    ctxt: &SemContext<'ast>,
    ast: &'ast Ast,
    map_global_defs: &NodeMap<GlobalId>,
) {
    let mut checker = GlobalDefCheck {
        ctxt: ctxt,
        current_type: BuiltinType::Unit,
        map_global_defs: map_global_defs,
    };

    checker.visit_ast(ast);
}

struct GlobalDefCheck<'a, 'ast: 'a> {
    ctxt: &'a SemContext<'ast>,
    current_type: BuiltinType,
    map_global_defs: &'a NodeMap<GlobalId>,
}

impl<'a, 'ast> Visitor<'ast> for GlobalDefCheck<'a, 'ast> {
    fn visit_file(&mut self, f: &'ast File) {
        for e in &f.elements {
            match *e {
                ElemGlobal(ref g) => self.visit_global(g),
                _ => {}
            }
        }
    }

    fn visit_global(&mut self, g: &'ast Global) {
        let global_id = *self.map_global_defs.get(g.id).unwrap();

        let ty = semck::read_type(self.ctxt, &g.data_type).unwrap_or(BuiltinType::Unit);
        let glob = self.ctxt.globals.idx(global_id);
        glob.lock().ty = ty;

        if g.expr.is_some() {
            self.ctxt
                .diag
                .lock()
                .report_without_path(g.pos, Msg::GlobalInitializerNotSupported);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::semck::tests::*;
    use dora_parser::error::msg::Msg;

    #[test]
    fn check_initializer() {
        err(
            "let a: Int = 0;",
            pos(1, 1),
            Msg::GlobalInitializerNotSupported,
        );
    }

    #[test]
    fn check_type() {
        err("var x: Foo;", pos(1, 8), Msg::UnknownType("Foo".into()));
    }
}
