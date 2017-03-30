use ast::Elem::ElemGlobal;
use ast::{File, Global};
use ast::visit::Visitor;
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
    }
}

#[cfg(test)]
mod tests {
    use error::msg::Msg;
    use semck::tests::*;

    #[test]
    fn check_global_type() {
        ok("let a: int = 0; var b: char = 'x';");
        err("var x: Foo = nil;",
            pos(1, 8),
            Msg::UnknownType("Foo".into()));
    }
}
