use crate::sema::{Sema, SourceFileId, StructDefinitionId};
use crate::sym::{ModuleSymTable, SymbolKind};
use crate::{expand_type, AllowSelf};

use dora_parser::ast;

pub fn check(sa: &Sema) {
    for (id, struct_) in sa.structs.iter() {
        let mut clsck = StructCheck {
            sa,
            struct_id: id,
            file_id: struct_.file_id,
            ast: &struct_.ast,
            symtable: ModuleSymTable::new(sa, struct_.module_id),
        };

        clsck.check();
    }
}

struct StructCheck<'x> {
    sa: &'x Sema,
    struct_id: StructDefinitionId,
    file_id: SourceFileId,
    ast: &'x ast::Struct,
    symtable: ModuleSymTable,
}

impl<'x> StructCheck<'x> {
    fn check(&mut self) {
        self.symtable.push_level();

        {
            let struct_ = self.sa.struct_(self.struct_id);

            for (id, name) in struct_.type_params().names() {
                self.symtable.insert(name, SymbolKind::TypeParam(id));
            }
        }

        for (idx, field) in self.ast.fields.iter().enumerate() {
            self.visit_struct_field(idx, field);
        }

        self.symtable.pop_level();
    }

    fn visit_struct_field(&mut self, idx: usize, f: &ast::StructField) {
        let struct_ = self.sa.struct_(self.struct_id);

        let ty = expand_type(
            self.sa,
            &self.symtable,
            self.file_id,
            &f.data_type,
            struct_.type_params(),
            AllowSelf::No,
        );

        struct_.fields[idx].ty.set(ty).expect("already initialized");
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::ErrorMessage;
    use crate::tests::*;

    #[test]
    fn struct_field() {
        ok("struct Foo { a: Int32 }");
        ok("struct Foo { a: Int32, b: Int32 }");
        ok("struct Foo { a: Int32 } struct Bar { a: Int32 }");
        ok("struct Foo { a: Int32, bar: Bar } struct Bar { a: Int32 }");
        err(
            "struct Bar { a: Unknown }",
            (1, 17),
            ErrorMessage::UnknownIdentifier("Unknown".into()),
        );
        err(
            "struct Foo { a: Int32, a: Int32 }",
            (1, 24),
            ErrorMessage::ShadowField("a".into()),
        );
    }

    #[test]
    fn structs_generic() {
        ok("
            struct Foo[T] { f1: T, f2: Int32 }
        ");
    }

    #[test]
    fn alias_type_as_struct_field() {
        ok("
            type MyInt = Int64;
            struct Foo(x: MyInt)
            fn f(v: Int64): Foo {
                Foo(v)
            }
        ");
    }

    #[test]
    fn struct_with_type_param() {
        ok("trait SomeTrait {} struct Foo[T: SomeTrait] { f1: T, f2: Int32 }");
    }

    #[test]
    fn struct_internal() {
        err(
            "@internal struct Foo",
            (1, 11),
            ErrorMessage::UnresolvedInternal,
        );
    }

    #[test]
    fn struct_with_type_params_error() {
        err(
            "struct MyStruct[] { f1: Int32 }",
            (1, 1),
            ErrorMessage::TypeParamsExpected,
        );

        err(
            "struct MyStruct[X, X] { f1: X }",
            (1, 20),
            ErrorMessage::TypeParamNameNotUnique("X".into()),
        );

        err(
            "struct MyStruct[X: NonExistingTrait] { f1: X }",
            (1, 20),
            ErrorMessage::UnknownIdentifier("NonExistingTrait".into()),
        );
    }
}
