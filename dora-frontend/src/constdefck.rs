use crate::sema::{Sema, TypeParamDefinition};
use crate::sym::ModuleSymTable;
use crate::{expand_type, AllowSelf};

pub fn check(sa: &Sema) {
    for (id, const_) in sa.consts.iter() {
        let symtable = ModuleSymTable::new(sa, const_.module_id);

        let ty = expand_type(
            sa,
            &symtable,
            const_.file_id,
            &const_.ast.data_type,
            &TypeParamDefinition::new(),
            AllowSelf::No,
        );

        sa.const_(id).ty.set(ty).expect("already initialized");
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

    #[test]
    fn const_with_alias_type() {
        ok("
            type MyInt = Int64;
            const x: MyInt = 10;
        ")
    }
}
