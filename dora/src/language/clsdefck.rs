use std::collections::HashSet;

use crate::language::error::msg::SemError;
use crate::language::sem_analysis::{
    ClassDefinitionId, Field, FieldId, ModuleDefinitionId, SemAnalysis, SourceFileId,
};
use crate::language::sym::NestedSymTable;
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::language::{self, read_type, AllowSelf, TypeParamContext};

use dora_parser::ast;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

pub fn check(sa: &SemAnalysis) {
    for cls in sa.classes.iter() {
        let (cls_id, file_id, ast, module_id) = {
            let cls = cls.read();
            (
                cls.id(),
                cls.file_id(),
                cls.ast.as_ref().expect("missing ast").clone(),
                cls.module_id,
            )
        };

        let mut clsck = ClsDefCheck {
            sa,
            cls_id,
            file_id,
            ast: &ast,
            module_id,
            sym: NestedSymTable::new(sa, module_id),
            table: HashSet::new(),
        };

        clsck.check();
    }
}

struct ClsDefCheck<'x> {
    sa: &'x SemAnalysis,
    cls_id: ClassDefinitionId,
    file_id: SourceFileId,
    ast: &'x ast::Class,
    module_id: ModuleDefinitionId,
    sym: NestedSymTable,
    table: HashSet<Name>,
}

impl<'x> ClsDefCheck<'x> {
    fn check(&mut self) {
        self.sym.push_level();

        if let Some(ref type_params) = self.ast.type_params {
            self.check_type_params(type_params);
        } else {
            let cls = self.sa.classes.idx(self.cls_id);
            let mut cls = cls.write();
            cls.ty = Some(SourceType::Class(self.cls_id, SourceTypeArray::empty()));
        }

        for field in &self.ast.fields {
            self.visit_field(field);
        }

        self.sym.pop_level();
    }

    fn visit_field(&mut self, f: &ast::Field) {
        let ty = read_type(
            self.sa,
            &self.sym,
            self.file_id.into(),
            &f.data_type,
            TypeParamContext::Class(self.cls_id),
            AllowSelf::No,
        )
        .unwrap_or(SourceType::Error);
        self.add_field(f.pos, f.name, ty, f.mutable, f.is_pub);
    }

    fn add_field(
        &mut self,
        pos: Position,
        name: Name,
        ty: SourceType,
        mutable: bool,
        is_pub: bool,
    ) {
        let cls = self.sa.classes.idx(self.cls_id);
        let mut cls = cls.write();

        let id: FieldId = cls.fields.len().into();

        let field = Field {
            id,
            name,
            ty,
            mutable,
            is_pub,
        };

        self.check_if_symbol_exists(name, pos);

        cls.fields.push(field);
    }

    fn check_type_params(&mut self, ast_type_params: &[ast::TypeParam]) {
        let cls = self.sa.classes.idx(self.cls_id);
        let mut cls = cls.write();

        let type_params = language::check_type_params(
            self.sa,
            ast_type_params,
            &mut cls.type_params,
            &mut self.sym,
            self.file_id,
            self.ast.pos,
        );

        let params = SourceTypeArray::with(type_params);
        cls.ty = Some(SourceType::Class(self.cls_id, params));
    }

    fn check_if_symbol_exists(&mut self, name: Name, pos: Position) {
        if !self.table.insert(name) {
            let file: SourceFileId = self.file_id.into();

            let name = self.sa.interner.str(name).to_string();
            self.sa
                .diag
                .lock()
                .report(file, pos, SemError::ShadowField(name));
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::language::error::msg::SemError;
    use crate::language::tests::*;

    #[test]
    fn test_class_definition() {
        ok("class Foo");
        ok("class Foo()");
        ok("class Foo(a: Int32)");
        ok("class Foo(a: Int32, b:Int32)");
        ok("class Foo(a: Foo)");
        ok("class Foo(a: Bar) class Bar");
        err(
            "class Foo(a: Unknown)",
            pos(1, 14),
            SemError::UnknownIdentifier("Unknown".into()),
        );
        err(
            "class Foo(a: Int32, a: Int32)",
            pos(1, 21),
            SemError::ShadowField("a".to_string()),
        );
    }

    #[test]
    fn field_defined_twice() {
        err(
            "class Foo(a: Int32, a: Int32)",
            pos(1, 21),
            SemError::ShadowField("a".into()),
        );
    }

    #[test]
    fn test_generic_class() {
        ok("class A[T]");
        ok("class A[X, Y]");
        err(
            "class A[T, T]",
            pos(1, 12),
            SemError::TypeParamNameNotUnique("T".into()),
        );
        err("class A[]", pos(1, 1), SemError::TypeParamsExpected);
    }

    #[test]
    fn test_generic_argument() {
        ok("class A[T](val: T)");
        ok("class A[T](var val: T)");
        ok("class A[T](let val: T)");
    }

    #[test]
    fn test_generic_bound() {
        err(
            "class A[T: Foo]",
            pos(1, 12),
            SemError::UnknownIdentifier("Foo".into()),
        );
        err(
            "class Foo class A[T: Foo]",
            pos(1, 22),
            SemError::BoundExpected,
        );
        ok("trait Foo {} class A[T: Foo]");
    }

    #[test]
    fn test_duplicate_trait_bound() {
        err(
            "trait Foo {}
            class A[T: Foo + Foo]",
            pos(2, 21),
            SemError::DuplicateTraitBound,
        );
    }

    #[test]
    fn test_defining_static_method_twice() {
        err(
            "
            class X
            impl X { @static fn foo() {} @static fn foo(a: String) {} }",
            pos(3, 50),
            SemError::MethodExists("foo".into(), pos(3, 30)),
        );
    }
}
