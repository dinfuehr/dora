use dora_parser::ast::Elem::ElemGlobal;
use dora_parser::ast::{File, Global};
use dora_parser::ast::visit::Visitor;
use dora_parser::error::msg::Msg;
use ctxt::{Context, GlobalId, NodeMap};
use semck;
use ty::BuiltinType;

pub fn check<'a, 'ast>(ctxt: &Context<'ast>, map_global_defs: &NodeMap<GlobalId>) {
    let mut checker = GlobalDefCheck {
        ctxt: ctxt,
        current_type: BuiltinType::Unit,
        map_global_defs: map_global_defs,
    };

    checker.visit_ast(ctxt.ast);
}

struct GlobalDefCheck<'a, 'ast: 'a> {
    ctxt: &'a Context<'ast>,
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
        self.ctxt.globals[global_id].borrow_mut().ty = ty;

        if g.expr.is_some() {
            self.ctxt
                .diag
                .borrow_mut()
                .report(g.pos, Msg::GlobalInitializerNotSupported);
        }
    }
}

#[cfg(test)]
mod tests {
    use dora_parser::error::msg::Msg;
    use semck::tests::*;

    #[test]
    fn check_initializer() {
        err("let a: int = 0;",
            pos(1, 1),
            Msg::GlobalInitializerNotSupported);
    }

    #[test]
    fn check_type() {
        err("var x: Foo;", pos(1, 8), Msg::UnknownType("Foo".into()));
    }
}
