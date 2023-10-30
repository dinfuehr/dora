use crate::sema::{ConstDefinitionId, Sema, SourceFileId};
use crate::sym::ModuleSymTable;
use crate::ty::SourceType;
use crate::{read_type_context, AllowSelf, TypeParamContext};

use dora_parser::ast;

pub fn check(sa: &Sema) {
    for (_const_id, const_) in sa.consts.iter() {
        let (const_id, file_id, ast, module_id) = {
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
            symtable: ModuleSymTable::new(sa, module_id),
        };

        clsck.check();
    }
}

struct ConstCheck<'x> {
    sa: &'x Sema,
    const_id: ConstDefinitionId,
    file_id: SourceFileId,
    ast: &'x ast::Const,
    symtable: ModuleSymTable,
}

impl<'x> ConstCheck<'x> {
    fn check(&mut self) {
        let ty = read_type_context(
            self.sa,
            &self.symtable,
            self.file_id,
            &self.ast.data_type,
            TypeParamContext::None,
            AllowSelf::No,
        )
        .unwrap_or(SourceType::Error);

        self.sa
            .const_(self.const_id)
            .ty
            .set(ty)
            .expect("already initialized");
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::ErrorMessage;
    use crate::tests::*;

    #[test]
    fn const_unknown_type() {
        err(
            "const x: Foo = 0;",
            (1, 10),
            ErrorMessage::UnknownIdentifier("Foo".into()),
        );

        ok("const x: Int32 = 0i32;");
    }
}
