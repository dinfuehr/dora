use crate::error::msg::ErrorMessage;
use crate::sema::{GlobalDefinitionId, Sema, SourceFileId};
use crate::sym::ModuleSymTable;
use crate::ty::SourceType;
use crate::{read_type_context, AllowSelf, TypeParamContext};
use dora_parser::ast;

pub fn check<'a>(sa: &Sema) {
    for global in sa.globals.iter() {
        let (global_id, file_id, ast, module_id) = {
            let global = global.read();
            (
                global.id(),
                global.file_id,
                global.ast.clone(),
                global.module_id,
            )
        };

        let symtable = ModuleSymTable::new(sa, module_id);

        let mut checker = GlobalDefCheck {
            sa,
            file_id,
            ast: &ast,
            global_id,
            symtable,
        };

        checker.check();
    }
}

struct GlobalDefCheck<'a> {
    sa: &'a Sema,
    file_id: SourceFileId,
    global_id: GlobalDefinitionId,
    ast: &'a ast::Global,
    symtable: ModuleSymTable,
}

impl<'a> GlobalDefCheck<'a> {
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

        let global_var = self.sa.globals.idx(self.global_id);
        let mut global_var = global_var.write();
        global_var.ty = ty;

        if global_var.ast.initial_value.is_none() {
            let msg = ErrorMessage::LetMissingInitialization;
            self.sa.report(self.file_id, self.ast.span, msg);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::ErrorMessage;
    use crate::tests::*;

    #[test]
    fn check_initializer() {
        ok("let a: Int32 = 0i32;");
        ok("let a: Int32 = 0i32; let mut b: Int32 = a + 1i32;");
        err(
            "let mut a: Int32 = foo;",
            (1, 20),
            ErrorMessage::UnknownIdentifier("foo".into()),
        );
    }

    #[test]
    fn check_type() {
        err(
            "let mut x: Foo = 0;",
            (1, 12),
            ErrorMessage::UnknownIdentifier("Foo".into()),
        );
    }
}
