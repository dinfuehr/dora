use crate::sema::{Sema, SourceFileId, StructDefinition, StructDefinitionField};
use crate::sym::{ModuleSymTable, SymbolKind};
use crate::{parsety, AliasReplacement};

pub fn check(sa: &Sema) {
    for (_id, struct_) in sa.structs.iter() {
        let mut clsck = StructCheck {
            sa,
            struct_,
            file_id: struct_.file_id,
            symtable: ModuleSymTable::new(sa, struct_.module_id),
        };

        clsck.check();
    }
}

struct StructCheck<'x> {
    sa: &'x Sema,
    struct_: &'x StructDefinition,
    file_id: SourceFileId,
    symtable: ModuleSymTable,
}

impl<'x> StructCheck<'x> {
    fn check(&mut self) {
        self.symtable.push_level();

        for (id, name) in self.struct_.type_param_definition().names() {
            self.symtable.insert(name, SymbolKind::TypeParam(id));
        }

        for field in self.struct_.fields.iter() {
            self.visit_struct_field(field);
        }

        self.symtable.pop_level();
    }

    fn visit_struct_field(&mut self, field: &StructDefinitionField) {
        let ctxt = parsety::TypeContext {
            allow_self: false,
            module_id: self.struct_.module_id,
            file_id: self.file_id,
            type_param_defs: self.struct_.type_param_definition(),
        };
        parsety::check_parsed_type2(self.sa, &ctxt, field.parsed_ty());

        parsety::expand_parsed_type2(
            self.sa,
            field.parsed_ty(),
            None,
            AliasReplacement::ReplaceWithActualType,
        );
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

    #[test]
    fn struct_with_where_bounds() {
        ok("
            trait MyTrait {}
            struct Foo[T] where T: MyTrait
        ");

        ok("
            trait MyTrait {}
            struct Foo[T] where Option[T]: MyTrait
        ");

        err(
            "
            trait MyTrait {}
            struct Foo[T] where F: MyTrait
        ",
            (3, 33),
            ErrorMessage::UnknownIdentifier("F".into()),
        );

        err(
            "
            struct Foo[T] where T: Int64
        ",
            (2, 36),
            ErrorMessage::BoundExpected,
        );
    }
}
