use parking_lot::RwLock;

use crate::semck;
use crate::ty::BuiltinType;
use crate::vm::{Fct, FctId, FctKind, FctParent, FctSrc, GlobalId, NodeMap, VM};
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
            let fct = Fct {
                id: FctId(0),
                pos: initializer.pos,
                ast: initializer,
                name: initializer.name,
                param_types: Vec::new(),
                return_type: BuiltinType::Unit,
                parent: FctParent::None,
                has_override: initializer.has_override,
                has_open: initializer.has_open,
                has_final: initializer.has_final,
                has_optimize_immediately: initializer.has_optimize_immediately,
                is_pub: initializer.is_pub,
                is_static: initializer.is_static,
                is_abstract: initializer.is_abstract,
                is_test: initializer.is_test,
                use_cannon: initializer.use_cannon,
                internal: initializer.internal,
                internal_resolved: false,
                overrides: None,
                is_constructor: initializer.is_constructor,
                vtable_index: None,
                initialized: false,
                impl_for: None,
                file: file,
                variadic_arguments: false,

                type_params: Vec::new(),
                kind: FctKind::Source(RwLock::new(FctSrc::new())),
                bytecode: None,
                intrinsic: None,
            };

            let fct_id = self.vm.add_fct(fct);
            glob.initializer = Some(fct_id);
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
            "var x: Foo;",
            pos(1, 8),
            SemError::UnknownType("Foo".into()),
        );
    }
}
