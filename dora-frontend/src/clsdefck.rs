#[cfg(test)]
mod tests {
    use crate::args;
    use crate::error::diagnostics::{
        ALIAS_EXISTS, BOUND_EXPECTED, SHADOW_FIELD, TYPE_PARAM_NAME_NOT_UNIQUE,
        TYPE_PARAMS_EXPECTED, UNKNOWN_IDENTIFIER,
    };
    use crate::tests::*;

    #[test]
    fn test_class_definition() {
        ok("class Foo");
        ok("class Foo()");
        ok("class Foo { a: Int32 }");
        ok("class Foo { a: Int32, b:Int32 }");
        ok("class Foo { a: Foo }");
        ok("class Foo { a: Bar } class Bar");
        err(
            "class Foo { a: Unknown }",
            (1, 16),
            7,
            crate::ErrorLevel::Error,
            &UNKNOWN_IDENTIFIER,
            args!("Unknown"),
        );
        err(
            "class Foo { a: Int32, a: Int32 }",
            (1, 23),
            8,
            crate::ErrorLevel::Error,
            &SHADOW_FIELD,
            args!("a"),
        );
    }

    #[test]
    fn field_defined_twice() {
        err(
            "class Foo { a: Int32, a: Int32 }",
            (1, 23),
            8,
            crate::ErrorLevel::Error,
            &SHADOW_FIELD,
            args!("a"),
        );
    }

    #[test]
    fn test_generic_class() {
        ok("class A[T]");
        ok("class A[X, Y]");
        err(
            "class A[T, T]",
            (1, 12),
            1,
            crate::ErrorLevel::Error,
            &TYPE_PARAM_NAME_NOT_UNIQUE,
            args!("T"),
        );
        err(
            "class A[]",
            (1, 8),
            2,
            crate::ErrorLevel::Error,
            &TYPE_PARAMS_EXPECTED,
            args!(),
        );
    }

    #[test]
    fn test_generic_argument() {
        ok("class A[T] { val: T }");
    }

    #[test]
    fn test_generic_bound() {
        err(
            "class A[T: Foo]",
            (1, 12),
            3,
            crate::ErrorLevel::Error,
            &UNKNOWN_IDENTIFIER,
            args!("Foo"),
        );
        err(
            "class Foo class A[T: Foo]",
            (1, 22),
            3,
            crate::ErrorLevel::Error,
            &BOUND_EXPECTED,
            args!(),
        );
        ok("trait Foo {} class A[T: Foo]");
    }

    #[test]
    fn test_defining_static_method_twice() {
        err(
            "
            class X
            impl X { static fn foo() {} static fn foo(a: String) {} }",
            (3, 41),
            27,
            crate::ErrorLevel::Error,
            &ALIAS_EXISTS,
            args!("foo", "main.dora:3:22"),
        );
    }

    #[test]
    fn alias_type_as_class_field() {
        ok("
            type MyInt = Int64;
            class Foo(MyInt)
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
            1,
            crate::ErrorLevel::Error,
            &UNKNOWN_IDENTIFIER,
            args!("F"),
        );

        err(
            "
            class Foo[T] where T: Int64
        ",
            (2, 35),
            5,
            crate::ErrorLevel::Error,
            &BOUND_EXPECTED,
            args!(),
        );
    }
}
