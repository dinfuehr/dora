use parking_lot::RwLock;

use std::collections::HashMap;

use crate::error::msg::SemError;
use crate::semck;
use crate::ty::SourceType;
use crate::vm::{AnalysisData, Fct, FctId, FctKind, FctParent, FileId, GlobalId, NamespaceId, VM};
use dora_parser::ast;

pub fn check<'a>(vm: &VM) {
    for global in vm.globals.iter() {
        let (global_id, file_id, ast, namespace_id) = {
            let global = global.read();
            (
                global.id,
                global.file_id,
                global.ast.clone(),
                global.namespace_id,
            )
        };

        let mut checker = GlobalDefCheck {
            vm,
            file_id,
            ast: &ast,
            namespace_id,
            global_id,
        };

        checker.check();
    }
}

struct GlobalDefCheck<'a> {
    vm: &'a VM,
    file_id: FileId,
    namespace_id: Option<NamespaceId>,
    global_id: GlobalId,
    ast: &'a ast::Global,
}

impl<'a> GlobalDefCheck<'a> {
    fn check(&mut self) {
        let ty = semck::read_type_namespace(
            self.vm,
            self.file_id,
            self.namespace_id,
            &self.ast.data_type,
        )
        .unwrap_or(SourceType::Error);

        let glob = self.vm.globals.idx(self.global_id);
        let mut glob = glob.write();
        glob.ty = ty;

        if let Some(ref initializer) = self.ast.initializer {
            let fct = Fct {
                id: FctId(0),
                pos: initializer.pos,
                ast: initializer.clone(),
                name: initializer.name,
                namespace_id: self.namespace_id,
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
                file_id: self.file_id,
                variadic_arguments: false,
                specializations: RwLock::new(HashMap::new()),

                type_params: Vec::new(),
                kind: FctKind::Source(RwLock::new(AnalysisData::new())),
                bytecode: None,
                intrinsic: None,
            };

            let fct_id = self.vm.add_fct(fct);
            glob.initializer = Some(fct_id);
        } else {
            let msg = SemError::LetMissingInitialization;
            self.vm.diag.lock().report(self.file_id, self.ast.pos, msg);
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
