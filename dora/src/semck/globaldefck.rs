use crate::error::msg::SemError;
use crate::semck::{self, TypeParamContext};
use crate::sym::NestedSymTable;
use crate::ty::SourceType;
use crate::vm::{Fct, FctParent, FileId, GlobalId, NamespaceId, VM};
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

        let symtable = NestedSymTable::new(vm, namespace_id);

        let mut checker = GlobalDefCheck {
            vm,
            file_id,
            ast: &ast,
            namespace_id,
            global_id,
            symtable,
        };

        checker.check();
    }
}

struct GlobalDefCheck<'a> {
    vm: &'a VM,
    file_id: FileId,
    namespace_id: NamespaceId,
    global_id: GlobalId,
    ast: &'a ast::Global,
    symtable: NestedSymTable<'a>,
}

impl<'a> GlobalDefCheck<'a> {
    fn check(&mut self) {
        let ty = semck::read_type(
            self.vm,
            &self.symtable,
            self.file_id,
            &self.ast.data_type,
            TypeParamContext::None,
        )
        .unwrap_or(SourceType::Error);

        let glob = self.vm.globals.idx(self.global_id);
        let mut glob = glob.write();
        glob.ty = ty;

        if let Some(ref initializer) = self.ast.initializer {
            let fct = Fct::new(
                self.file_id,
                self.namespace_id,
                initializer,
                FctParent::None,
            );

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
            SemError::UnknownIdentifier("Foo".into()),
        );
    }
}
