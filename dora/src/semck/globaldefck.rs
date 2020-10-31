use parking_lot::RwLock;

use crate::error::msg::SemError;
use crate::semck;
use crate::ty::SourceType;
use crate::vm::{Fct, FctId, FctKind, FctParent, FctSrc, GlobalId, NodeMap, VM};
use dora_parser::ast::visit::Visitor;
use dora_parser::ast::Elem::ElemGlobal;
use dora_parser::ast::{File, Global};

pub fn check<'a>(vm: &mut VM, map_global_defs: &NodeMap<GlobalId>) {
    let mut checker = GlobalDefCheck {
        vm,
        current_type: SourceType::Unit,
        map_global_defs,
    };

    checker.check();
}

struct GlobalDefCheck<'a> {
    vm: &'a mut VM,
    current_type: SourceType,
    map_global_defs: &'a NodeMap<GlobalId>,
}

impl<'a> GlobalDefCheck<'a> {
    fn check(&mut self) {
        let files = self.vm.files.clone();
        let files = files.read();

        for file in files.iter() {
            self.visit_file(file);
        }
    }
}

impl<'a> Visitor for GlobalDefCheck<'a> {
    fn visit_file(&mut self, f: &File) {
        for e in &f.elements {
            match *e {
                ElemGlobal(ref g) => self.visit_global(g),
                _ => {}
            }
        }
    }

    fn visit_global(&mut self, g: &Global) {
        let global_id = *self.map_global_defs.get(g.id).unwrap();
        let glob = self.vm.globals.idx(global_id);
        let mut glob = glob.write();
        let file = glob.file;

        let ty = semck::read_type_namespace(self.vm, file, glob.namespace_id, &g.data_type)
            .unwrap_or(SourceType::Error);
        glob.ty = ty;

        if let Some(ref initializer) = g.initializer {
            let fct = Fct {
                id: FctId(0),
                pos: initializer.pos,
                ast: initializer.clone(),
                name: initializer.name,
                namespace_id: None,
                param_types: Vec::new(),
                return_type: SourceType::Unit,
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
