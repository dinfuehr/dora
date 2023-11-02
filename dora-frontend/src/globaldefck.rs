use crate::error::msg::ErrorMessage;
use crate::sema::{GlobalDefinitionId, Sema, SourceFileId, TypeParamDefinition};
use crate::sym::ModuleSymTable;
use crate::{expand_type, AllowSelf};
use dora_parser::ast;

pub fn check<'a>(sa: &Sema) {
    for (id, global) in sa.globals.iter() {
        let symtable = ModuleSymTable::new(sa, global.module_id);

        let mut checker = GlobalDefCheck {
            sa,
            file_id: global.file_id,
            ast: &global.ast,
            global_id: id,
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
        let ty = expand_type(
            self.sa,
            &self.symtable,
            self.file_id,
            &self.ast.data_type,
            &TypeParamDefinition::new(),
            AllowSelf::No,
        );

        let global_var = self.sa.global(self.global_id);

        assert!(global_var.ty.set(ty).is_ok());

        if !global_var.has_initial_value() {
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

    #[test]
    fn alias_in_global() {
        ok("
            type MyInt = Int64;
            let mut x: MyInt = 10;
        ");
    }
}
