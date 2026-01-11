#[cfg(test)]
mod tests {
    use crate::args;
    use crate::error::diagnostics::{
        BOUND_EXPECTED, SHADOW_FIELD, TYPE_PARAM_NAME_NOT_UNIQUE, TYPE_PARAMS_EXPECTED,
        UNKNOWN_IDENTIFIER, UNRESOLVED_INTERNAL,
    };
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
            7,
            crate::ErrorLevel::Error,
            &UNKNOWN_IDENTIFIER,
            args!("Unknown"),
        );
        err(
            "struct Foo { a: Int32, a: Int32 }",
            (1, 24),
            8,
            crate::ErrorLevel::Error,
            &SHADOW_FIELD,
            args!("a"),
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
            struct Foo(MyInt)
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
            (1, 1),
            20,
            crate::ErrorLevel::Error,
            &UNRESOLVED_INTERNAL,
            args!(),
        );
    }

    #[test]
    fn struct_with_type_params_error() {
        err(
            "struct MyStruct[] { f1: Int32 }",
            (1, 16),
            2,
            crate::ErrorLevel::Error,
            &TYPE_PARAMS_EXPECTED,
            args!(),
        );

        err(
            "struct MyStruct[X, X] { f1: X }",
            (1, 20),
            1,
            crate::ErrorLevel::Error,
            &TYPE_PARAM_NAME_NOT_UNIQUE,
            args!("X"),
        );

        err(
            "struct MyStruct[X: NonExistingTrait] { f1: X }",
            (1, 20),
            16,
            crate::ErrorLevel::Error,
            &UNKNOWN_IDENTIFIER,
            args!("NonExistingTrait"),
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
            1,
            crate::ErrorLevel::Error,
            &UNKNOWN_IDENTIFIER,
            args!("F"),
        );

        err(
            "
            struct Foo[T] where T: Int64
        ",
            (2, 36),
            5,
            crate::ErrorLevel::Error,
            &BOUND_EXPECTED,
            args!(),
        );
    }
}
