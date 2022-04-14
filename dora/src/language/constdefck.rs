use crate::language::sem_analysis::{ConstDefinitionId, ModuleDefinitionId, SourceFileId};
use crate::language::sym::NestedSymTable;
use crate::language::ty::SourceType;
use crate::language::{self, AllowSelf, TypeParamContext};
use crate::vm::SemAnalysis;

use dora_parser::ast;

pub fn check(sa: &SemAnalysis) {
    for const_ in sa.consts.iter() {
        let (const_id, file_id, ast, module_id) = {
            let const_ = const_.read();
            (
                const_.id(),
                const_.file_id,
                const_.ast.clone(),
                const_.module_id,
            )
        };

        let mut clsck = ConstCheck {
            sa,
            const_id,
            file_id,
            ast: &ast,
            module_id,
            symtable: NestedSymTable::new(sa, module_id),
        };

        clsck.check();
    }
}

struct ConstCheck<'x> {
    sa: &'x SemAnalysis,
    const_id: ConstDefinitionId,
    file_id: SourceFileId,
    ast: &'x ast::Const,
    module_id: ModuleDefinitionId,
    symtable: NestedSymTable<'x>,
}

impl<'x> ConstCheck<'x> {
    fn check(&mut self) {
        let ty = language::read_type(
            self.sa,
            &self.symtable,
            self.file_id,
            &self.ast.data_type,
            TypeParamContext::None,
            AllowSelf::No,
        )
        .unwrap_or(SourceType::Error);

        let const_ = self.sa.consts.idx(self.const_id);
        let mut const_ = const_.write();
        const_.ty = ty;
    }
}

#[cfg(test)]
mod tests {
    use crate::language::error::msg::SemError;
    use crate::language::tests::*;

    #[test]
    fn const_unknown_type() {
        err(
            "const x: Foo = 0;",
            pos(1, 10),
            SemError::UnknownIdentifier("Foo".into()),
        );

        ok("const x: Int32 = 0I;");
    }
}
