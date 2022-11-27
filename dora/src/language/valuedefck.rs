use std::collections::HashSet;

use crate::language::error::msg::ErrorMessage;
use crate::language::sem_analysis::{
    ModuleDefinitionId, SemAnalysis, SourceFileId, ValueDefinitionField, ValueDefinitionFieldId,
    ValueDefinitionId, Visibility,
};
use crate::language::sym::{ModuleSymTable, Sym};
use crate::language::ty::SourceType;
use crate::language::{self, AllowSelf, TypeParamContext};

use dora_parser::ast;
use dora_parser::interner::Name;

pub fn check(sa: &SemAnalysis) {
    for value in sa.values.iter() {
        let (value_id, file_id, ast, module_id) = {
            let value = value.read();
            (
                value.id(),
                value.file_id,
                value.ast.clone(),
                value.module_id,
            )
        };

        let mut valueck = ValueCheck {
            sa,
            value_id,
            file_id,
            ast: &ast,
            module_id,
            symtable: ModuleSymTable::new(sa, module_id),
            fields: HashSet::new(),
        };

        valueck.check();
    }
}

struct ValueCheck<'x> {
    sa: &'x SemAnalysis,
    value_id: ValueDefinitionId,
    file_id: SourceFileId,
    ast: &'x ast::Value,
    module_id: ModuleDefinitionId,
    symtable: ModuleSymTable,
    fields: HashSet<Name>,
}

impl<'x> ValueCheck<'x> {
    fn check(&mut self) {
        self.symtable.push_level();

        {
            let struct_ = self.sa.values.idx(self.value_id);
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

    fn visit_struct_field(&mut self, f: &ast::ValueField, id: ValueDefinitionFieldId) {
        let ty = language::read_type(
            self.sa,
            &self.symtable,
            self.file_id,
            &f.data_type,
            TypeParamContext::Value(self.value_id),
            AllowSelf::No,
        )
        .unwrap_or(SourceType::Error);

        let struct_ = self.sa.values.idx(self.value_id);
        let mut struct_ = struct_.write();

        if !self.fields.insert(f.name) {
            let name = self.sa.interner.str(f.name).to_string();
            self.sa
                .diag
                .lock()
                .report(self.file_id, f.pos, ErrorMessage::ShadowField(name));
            return;
        }

        let field = ValueDefinitionField {
            id,
            pos: f.pos,
            name: f.name,
            ty,
            visibility: Visibility::from_ast(f.visibility),
        };

        struct_.fields.push(field);
        let old = struct_.field_names.insert(f.name, id);
        assert!(old.is_none());
    }
}

#[cfg(test)]
mod tests {
    use crate::language::error::msg::ErrorMessage;
    use crate::language::tests::*;

    #[test]
    fn value_field() {
        ok("value Foo { a: Int32 }");
        ok("value Foo { a: Int32, b: Int32 }");
        ok("value Foo { a: Int32 } value Bar { a: Int32 }");
        ok("value Foo { a: Int32, bar: Bar } value Bar { a: Int32 }");
        err(
            "value Bar { a: Unknown }",
            pos(1, 16),
            ErrorMessage::UnknownIdentifier("Unknown".into()),
        );
        err(
            "value Foo { a: Int32, a: Int32 }",
            pos(1, 23),
            ErrorMessage::ShadowField("a".into()),
        );
    }

    #[test]
    fn value_generic() {
        ok("
            value Foo[T] { f1: T, f2: Int32 }
        ");
    }

    #[test]
    fn value_with_type_param() {
        ok("trait SomeTrait {} value Foo[T: SomeTrait] { f1: T, f2: Int32 }");
    }

    #[test]
    fn value_internal() {
        err(
            "@internal value Foo",
            pos(1, 11),
            ErrorMessage::UnresolvedInternal,
        );
    }

    #[test]
    fn value_with_type_params_error() {
        err(
            "value MyStruct[] { f1: Int32 }",
            pos(1, 1),
            ErrorMessage::TypeParamsExpected,
        );

        err(
            "value MyStruct[X, X] { f1: X }",
            pos(1, 19),
            ErrorMessage::TypeParamNameNotUnique("X".into()),
        );

        err(
            "value MyStruct[X: NonExistingTrait] { f1: X }",
            pos(1, 19),
            ErrorMessage::UnknownIdentifier("NonExistingTrait".into()),
        );
    }
}
