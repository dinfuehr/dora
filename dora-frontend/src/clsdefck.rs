use crate::sema::{ClassDefinition, Sema, SourceFileId};
use crate::sym::{ModuleSymTable, SymbolKind};
use crate::{expand_type, AllowSelf};

use dora_parser::ast;

pub fn check(sa: &Sema) {
    for (_id, cls) in sa.classes.iter() {
        let mut clsck = ClsDefCheck {
            sa,
            cls,
            file_id: cls.file_id(),
            ast: cls.ast(),
            sym: ModuleSymTable::new(sa, cls.module_id),
        };

        clsck.check();
    }
}

struct ClsDefCheck<'x> {
    sa: &'x Sema,
    cls: &'x ClassDefinition,
    file_id: SourceFileId,
    ast: &'x ast::Class,
    sym: ModuleSymTable,
}

impl<'x> ClsDefCheck<'x> {
    fn check(&mut self) {
        self.sym.push_level();

        for (id, name) in self.cls.type_params().names() {
            self.sym.insert(name, SymbolKind::TypeParam(id));
        }

        for (idx, field) in self.ast.fields.iter().enumerate() {
            self.visit_field(idx, field);
        }

        self.sym.pop_level();
    }

    fn visit_field(&mut self, idx: usize, f: &ast::Field) {
        let ty = expand_type(
            self.sa,
            &self.sym,
            self.file_id.into(),
            &f.data_type,
            self.cls.type_params(),
            AllowSelf::No,
        );

        self.cls.fields[idx]
            .ty
            .set(ty)
            .expect("already initialized");
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
    fn test_defining_static_method_twice() {
        err(
            "
            class X
            impl X { static fn foo() {} static fn foo(a: String) {} }",
            (3, 48),
            ErrorMessage::AliasExists("foo".into(), Span::new(49, 11)),
        );
    }

    #[test]
    fn alias_type_as_class_field() {
        ok("
            type MyInt = Int64;
            class Foo(x: MyInt)
            fn f(v: Int64): Foo {
                Foo(v)
            }
        ");
    }

    #[test]
    fn class_with_where_bounds() {
        ok("
            trait MyTrait {}
            class Foo[T] where T: MyTrait
        ");

        ok("
            trait MyTrait {}
            class Foo[T] where Option[T]: MyTrait
        ");

        err(
            "
            trait MyTrait {}
            class Foo[T] where F: MyTrait
        ",
            (3, 32),
            ErrorMessage::UnknownIdentifier("F".into()),
        );

        err(
            "
            class Foo[T] where T: Int64
        ",
            (2, 35),
            ErrorMessage::BoundExpected,
        );
    }
}
