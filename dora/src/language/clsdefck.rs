use std::collections::HashSet;

use crate::language::error::msg::ErrorMessage;
use crate::language::sem_analysis::{
    ClassDefinitionId, Field, FieldId, SemAnalysis, SourceFileId, Visibility,
};
use crate::language::sym::{ModuleSymTable, Sym};
use crate::language::ty::SourceType;
use crate::language::{read_type, AllowSelf, TypeParamContext};

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
            sym: ModuleSymTable::new(sa, module_id),
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
    sym: ModuleSymTable,
    table: HashSet<Name>,
}

impl<'x> ClsDefCheck<'x> {
    fn check(&mut self) {
        self.sym.push_level();

        {
            let cls = self.sa.classes.idx(self.cls_id);
            let cls = cls.read();

            for (id, name) in cls.type_params().names() {
                self.sym.insert(name, Sym::TypeParam(id));
            }
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
        self.add_field(f.pos, f.name, ty, f.mutable, f.visibility);
    }

    fn add_field(
        &mut self,
        pos: Position,
        name: Name,
        ty: SourceType,
        mutable: bool,
        visibility: ast::Visibility,
    ) {
        let cls = self.sa.classes.idx(self.cls_id);
        let mut cls = cls.write();

        let id: FieldId = cls.fields.len().into();

        let field = Field {
            id,
            name,
            ty,
            mutable,
            visibility: Visibility::from_ast(visibility),
        };

        self.check_if_symbol_exists(name, pos);

        cls.fields.push(field);
    }

    fn check_if_symbol_exists(&mut self, name: Name, pos: Position) {
        if !self.table.insert(name) {
            let file: SourceFileId = self.file_id.into();

            let name = self.sa.interner.str(name).to_string();
            self.sa
                .diag
                .lock()
                .report(file, pos, ErrorMessage::ShadowField(name));
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::language::error::msg::ErrorMessage;
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
            ErrorMessage::UnknownIdentifier("Unknown".into()),
        );
        err(
            "class Foo(a: Int32, a: Int32)",
            pos(1, 21),
            ErrorMessage::ShadowField("a".to_string()),
        );
    }

    #[test]
    fn field_defined_twice() {
        err(
            "class Foo(a: Int32, a: Int32)",
            pos(1, 21),
            ErrorMessage::ShadowField("a".into()),
        );
    }

    #[test]
    fn test_generic_class() {
        ok("class A[T]");
        ok("class A[X, Y]");
        err(
            "class A[T, T]",
            pos(1, 12),
            ErrorMessage::TypeParamNameNotUnique("T".into()),
        );
        err("class A[]", pos(1, 1), ErrorMessage::TypeParamsExpected);
    }

    #[test]
    fn test_generic_argument() {
        ok("class A[T](val: T)");
    }

    #[test]
    fn test_generic_bound() {
        err(
            "class A[T: Foo]",
            pos(1, 12),
            ErrorMessage::UnknownIdentifier("Foo".into()),
        );
        err(
            "class Foo class A[T: Foo]",
            pos(1, 22),
            ErrorMessage::BoundExpected,
        );
        ok("trait Foo {} class A[T: Foo]");
    }

    #[test]
    fn test_duplicate_trait_bound() {
        err(
            "trait Foo {}
            class A[T: Foo + Foo]",
            pos(2, 21),
            ErrorMessage::DuplicateTraitBound,
        );
    }

    #[test]
    fn test_defining_static_method_twice() {
        err(
            "
            class X
            impl X { @static fun foo(): Unit {} @static fun foo(a: String): Unit {} }",
            pos(3, 57),
            ErrorMessage::MethodExists("foo".into(), pos(3, 30)),
        );
    }
}
