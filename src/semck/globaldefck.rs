use crate::error::msg::SemError;
use crate::semck;
use crate::ty::BuiltinType;
use crate::vm::{GlobalId, NodeMap, VM};
use dora_parser::ast::visit::Visitor;
use dora_parser::ast::Elem::ElemGlobal;
use dora_parser::ast::{Ast, File, Global};

pub fn check<'a, 'ast>(vm: &VM<'ast>, ast: &'ast Ast, map_global_defs: &NodeMap<GlobalId>) {
    let mut checker = GlobalDefCheck {
        vm,
        current_type: BuiltinType::Unit,
        map_global_defs,
    };

    checker.visit_ast(ast);
}

struct GlobalDefCheck<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
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
        let glob = self.vm.globals.idx(global_id);
        let file = glob.lock().file;

        let ty = semck::read_type(self.vm, file, &g.data_type).unwrap_or(BuiltinType::Unit);
        glob.lock().ty = ty;

        if g.expr.is_some() {
            self.vm
                .diag
                .lock()
                .report(file, g.pos, SemError::GlobalInitializerNotSupported);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::SemError;
    use crate::semck::tests::*;

    #[test]
    fn check_initializer() {
        err(
            "let a: Int = 0;",
            pos(1, 1),
            SemError::GlobalInitializerNotSupported,
        );
    }

    #[test]
    fn check_type() {
        err(
            "var x: Foo;",
            pos(1, 8),
            SemError::UnknownType("Foo".into()),
        );
    }
}
