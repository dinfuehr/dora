use parking_lot::RwLock;

use crate::error::msg::SemError;
use crate::semck;
use crate::ty::BuiltinType;
use crate::vm::{Fct, FctKind, FctParent, FctSrc, GlobalId, NodeMap, VM};
use dora_parser::ast::visit::Visitor;
use dora_parser::ast::Elem::ElemGlobal;
use dora_parser::ast::{Ast, File, Global};

pub fn check<'a, 'ast>(vm: &mut VM<'ast>, ast: &'ast Ast, map_global_defs: &NodeMap<GlobalId>) {
    let mut checker = GlobalDefCheck {
        vm,
        current_type: BuiltinType::Unit,
        map_global_defs,
    };

    checker.visit_ast(ast);
}

struct GlobalDefCheck<'a, 'ast: 'a> {
    vm: &'a mut VM<'ast>,
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
        let mut glob = glob.write();
        let file = glob.file;

        let ty = semck::read_type(self.vm, file, &g.data_type).unwrap_or(BuiltinType::Error);
        glob.ty = ty;

        if let Some(ref initializer) = g.initializer {
            let fct = Fct::new(
                self.vm,
                initializer,
                file,
                FctKind::Source(RwLock::new(FctSrc::new())),
                FctParent::None,
                initializer.is_constructor,
            );

            let fct_id = self.vm.add_fct(fct);
            glob.initializer = Some(fct_id);
        } else {
            let msg = SemError::LetMissingInitialization;
            self.vm.diag.lock().report(file, g.pos, msg);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::SemError;
    use crate::semck::tests::*;

    #[test]
    fn check_initializer() {
        ok("let a: Int32 = 0;");
        ok("let a: Int32 = 0; var b: Int32 = a + 1;");
        err(
            "var a: Int32 = foo;",
            pos(1, 16),
            SemError::UnknownIdentifier("foo".into()),
        );
    }

    #[test]
    fn check_type() {
        err(
            "var x: Foo = 0;",
            pos(1, 8),
            SemError::UnknownType("Foo".into()),
        );
    }
}
