use crate::sema::{Sema, SourceFileId, StructDefinitionId};
use crate::sym::{ModuleSymTable, Sym};
use crate::ty::SourceType;
use crate::{read_type_context, AllowSelf, TypeParamContext};

use dora_parser::ast;

pub fn check(sa: &Sema) {
    for (_struct_id, struct_) in sa.structs.iter() {
        let (struct_id, file_id, ast, module_id) = {
            (
                struct_.id(),
                struct_.file_id,
                struct_.ast.clone(),
                struct_.module_id,
            )
        };

        let mut clsck = StructCheck {
            sa,
            struct_id,
            file_id,
            ast: &ast,
            symtable: ModuleSymTable::new(sa, module_id),
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
            let struct_ = &self.sa.structs[self.struct_id];

            for (id, name) in struct_.type_params().names() {
                self.symtable.insert(name, Sym::TypeParam(id));
            }
        }

        for (idx, field) in self.ast.fields.iter().enumerate() {
            self.visit_struct_field(idx, field);
        }

        self.symtable.pop_level();
    }

    fn visit_struct_field(&mut self, idx: usize, f: &ast::StructField) {
        let ty = read_type_context(
            self.sa,
            &self.symtable,
            self.file_id,
            &f.data_type,
            TypeParamContext::Struct(self.struct_id),
            AllowSelf::No,
        )
        .unwrap_or(SourceType::Error);

        let struct_ = &self.sa.structs[self.struct_id];
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
