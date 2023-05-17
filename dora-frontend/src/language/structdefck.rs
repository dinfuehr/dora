use std::collections::HashSet;

use crate::language::error::msg::ErrorMessage;
use crate::language::sema::{
    Sema, SourceFileId, StructDefinitionField, StructDefinitionFieldId, StructDefinitionId,
    Visibility,
};
use crate::language::sym::{ModuleSymTable, Sym};
use crate::language::ty::SourceType;
use crate::language::{self, AllowSelf, TypeParamContext};

use dora_parser::ast;
use dora_parser::interner::Name;

pub fn check(sa: &Sema) {
    for struct_ in sa.structs.iter() {
        let (struct_id, file_id, ast, module_id) = {
            let struct_ = struct_.read();
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
            fields: HashSet::new(),
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
    fields: HashSet<Name>,
}

impl<'x> StructCheck<'x> {
    fn check(&mut self) {
        self.symtable.push_level();

        {
            let struct_ = self.sa.structs.idx(self.struct_id);
            let struct_ = struct_.read();

            for (id, name) in struct_.type_params().names() {
                self.symtable.insert(name, Sym::TypeParam(id));
            }
        }

        for (idx, field) in self.ast.fields.iter().enumerate() {
            self.visit_struct_field(field, idx.into());
        }

        self.symtable.pop_level();
    }

    fn visit_struct_field(&mut self, f: &ast::StructField, id: StructDefinitionFieldId) {
        let ty = language::read_type_context(
            self.sa,
            &self.symtable,
            self.file_id,
            &f.data_type,
            TypeParamContext::Struct(self.struct_id),
            AllowSelf::No,
        )
        .unwrap_or(SourceType::Error);

        let struct_ = self.sa.structs.idx(self.struct_id);
        let mut struct_ = struct_.write();
        let name = f.name.as_ref().expect("missing name").name;

        if !self.fields.insert(name) {
            let name = self.sa.interner.str(name).to_string();
            self.sa
                .diag
                .lock()
                .report(self.file_id, f.span, ErrorMessage::ShadowField(name));
            return;
        }

        let field = StructDefinitionField {
            id,
            span: f.span,
            name,
            ty,
            visibility: Visibility::from_ast(f.visibility),
        };

        struct_.fields.push(field);
        let old = struct_.field_names.insert(name, id);
        assert!(old.is_none());
    }
}

#[cfg(test)]
mod tests {
    use crate::language::error::msg::ErrorMessage;
    use crate::language::tests::*;

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
