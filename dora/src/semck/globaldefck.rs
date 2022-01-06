use crate::semck::error::msg::SemError;
use crate::semck::sym::NestedSymTable;
use crate::semck::{self, AllowSelf, TypeParamContext};
use crate::ty::SourceType;
use crate::vm::{FctDefinition, FctParent, FileId, GlobalDefinitionId, NamespaceId, SemAnalysis};
use dora_parser::ast;

pub fn check<'a>(sa: &SemAnalysis) {
    for global in sa.globals.iter() {
        let (global_id, file_id, ast, namespace_id) = {
            let global = global.read();
            (
                global.id,
                global.file_id,
                global.ast.clone(),
                global.namespace_id,
            )
        };

        let symtable = NestedSymTable::new(sa, namespace_id);

        let mut checker = GlobalDefCheck {
            sa,
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
    sa: &'a SemAnalysis,
    file_id: FileId,
    namespace_id: NamespaceId,
    global_id: GlobalDefinitionId,
    ast: &'a ast::Global,
    symtable: NestedSymTable<'a>,
}

impl<'a> GlobalDefCheck<'a> {
    fn check(&mut self) {
        let ty = semck::read_type(
            self.sa,
            &self.symtable,
            self.file_id,
            &self.ast.data_type,
            TypeParamContext::None,
            AllowSelf::No,
        )
        .unwrap_or(SourceType::Error);

        let glob = self.sa.globals.idx(self.global_id);
        let mut glob = glob.write();
        glob.ty = ty;

        if let Some(ref initializer) = self.ast.initializer {
            let fct = FctDefinition::new(
                self.sa,
                self.file_id,
                self.namespace_id,
                initializer,
                FctParent::None,
            );

            let fct_id = self.sa.add_fct(fct);
            glob.initializer = Some(fct_id);
        } else {
            let msg = SemError::LetMissingInitialization;
            self.sa.diag.lock().report(self.file_id, self.ast.pos, msg);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::semck::error::msg::SemError;
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
