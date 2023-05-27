use crate::sema::{ClassDefinitionId, Sema, SourceFileId};
use crate::sym::{ModuleSymTable, Sym};
use crate::ty::SourceType;
use crate::{read_type_context, AllowSelf, TypeParamContext};

use dora_parser::ast;

pub fn check(sa: &Sema) {
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
        };

        clsck.check();
    }
}

struct ClsDefCheck<'x> {
    sa: &'x Sema,
    cls_id: ClassDefinitionId,
    file_id: SourceFileId,
    ast: &'x ast::Class,
    sym: ModuleSymTable,
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

        for (idx, field) in self.ast.fields.iter().enumerate() {
            self.visit_field(idx, field);
        }

        self.sym.pop_level();
    }

    fn visit_field(&mut self, idx: usize, f: &ast::Field) {
        let ty = read_type_context(
            self.sa,
            &self.sym,
            self.file_id.into(),
            &f.data_type,
            TypeParamContext::Class(self.cls_id),
            AllowSelf::No,
        )
        .unwrap_or(SourceType::Error);

        let cls = self.sa.classes.idx(self.cls_id);
        let mut cls = cls.write();

        cls.fields[idx].ty = ty;
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::ErrorMessage;
    use crate::tests::*;
    use dora_parser::Span;

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
            (1, 14),
            ErrorMessage::UnknownIdentifier("Unknown".into()),
        );
        err(
            "class Foo(a: Int32, a: Int32)",
            (1, 21),
            ErrorMessage::ShadowField("a".to_string()),
        );
    }

    #[test]
    fn field_defined_twice() {
        err(
            "class Foo(a: Int32, a: Int32)",
            (1, 21),
            ErrorMessage::ShadowField("a".into()),
        );
    }

    #[test]
    fn test_generic_class() {
        ok("class A[T]");
        ok("class A[X, Y]");
        err(
            "class A[T, T]",
            (1, 12),
            ErrorMessage::TypeParamNameNotUnique("T".into()),
        );
        err("class A[]", (1, 1), ErrorMessage::TypeParamsExpected);
    }

    #[test]
    fn test_generic_argument() {
        ok("class A[T](val: T)");
    }

    #[test]
    fn test_generic_bound() {
        err(
            "class A[T: Foo]",
            (1, 12),
            ErrorMessage::UnknownIdentifier("Foo".into()),
        );
        err(
            "class Foo class A[T: Foo]",
            (1, 22),
            ErrorMessage::BoundExpected,
        );
        ok("trait Foo {} class A[T: Foo]");
    }

    #[test]
    fn test_duplicate_trait_bound() {
        err(
            "trait Foo {}
            class A[T: Foo + Foo]",
            (2, 21),
            ErrorMessage::DuplicateTraitBound,
        );
    }

    #[test]
    fn test_defining_static_method_twice() {
        err(
            "
            class X
            impl X { static fn foo() {} static fn foo(a: String) {} }",
            (3, 48),
            ErrorMessage::MethodExists("foo".into(), Span::new(49, 11)),
        );
    }
}
