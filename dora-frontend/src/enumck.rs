use crate::sema::Sema;

pub fn check(sa: &Sema) {
    for (_id, enum_) in sa.enums.iter() {
        let mut simple_enumeration = true;

        for &variant_id in enum_.variant_ids() {
            let variant = sa.variant(variant_id);
            if variant.field_ids().len() > 0 {
                simple_enumeration = false;
            }
        }

        assert!(enum_.simple_enumeration.set(simple_enumeration).is_ok());
    }
}

#[cfg(test)]
mod tests {
    use crate::args;
    use crate::error::diagnostics::{
        BOUND_EXPECTED, ENUM_VARIANT_MISSING_ARGUMENTS, NO_ENUM_VARIANT, RETURN_TYPE,
        SHADOW_ENUM_VARIANT, TYPE_NOT_IMPLEMENTING_TRAIT, TYPE_PARAM_NAME_NOT_UNIQUE,
        TYPE_PARAMS_EXPECTED, UNEXPECTED_ARGUMENTS_FOR_ENUM_VARIANT, UNKNOWN_IDENTIFIER,
        WRONG_NUMBER_TYPE_PARAMS, WRONG_TYPE_FOR_ARGUMENT,
    };
    use crate::tests::*;

    #[test]
    fn enum_definitions() {
        err(
            "enum Foo {}",
            (1, 1),
            11,
            crate::ErrorLevel::Error,
            &NO_ENUM_VARIANT,
            args!(),
        );
        ok("enum Foo { A, B, C }");
        err(
            "enum Foo { A, A }",
            (1, 15),
            1,
            crate::ErrorLevel::Error,
            &SHADOW_ENUM_VARIANT,
            args!("A"),
        );
    }

    #[test]
    fn enum_with_argument() {
        ok("
            enum Foo { A(Int32), B(Float32), C}
            fn give_me_a(): Foo { Foo::A(1i32) }
            fn give_me_b(): Foo { Foo::B(2.0f32) }
            fn give_me_c(): Foo { Foo::C }

        ");
    }

    #[test]
    fn enum_wrong_type() {
        err(
            "
            enum Foo { A(Int32), B(Float32), C}
            fn give_me_a(): Foo { Foo::A(2.0f32) }

        ",
            (3, 42),
            6,
            crate::ErrorLevel::Error,
            &WRONG_TYPE_FOR_ARGUMENT,
            args!("Int32", "Float32"),
        );
    }

    #[test]
    fn enum_missing_args() {
        err(
            "
            enum Foo { A(Int32), B(Float32), C}
            fn give_me_a(): Foo { Foo::A }

        ",
            (3, 38),
            2,
            crate::ErrorLevel::Error,
            &ENUM_VARIANT_MISSING_ARGUMENTS,
            args!(),
        );
    }

    #[test]
    fn enum_unexpected_args() {
        err(
            "
            enum Foo { A(Int32), B(Float32), C}
            fn give_me_c(): Foo { Foo::C(12.0f32) }

        ",
            (3, 35),
            15,
            crate::ErrorLevel::Error,
            &UNEXPECTED_ARGUMENTS_FOR_ENUM_VARIANT,
            args!(),
        );
    }

    #[test]
    fn enum_parens_but_no_args() {
        err(
            "
            enum Foo { A(Int32), B(Float32), C}
            fn give_me_c(): Foo { Foo::C() }
        ",
            (3, 35),
            8,
            crate::ErrorLevel::Error,
            &UNEXPECTED_ARGUMENTS_FOR_ENUM_VARIANT,
            args!(),
        );
    }

    #[test]
    fn enum_copy() {
        ok("
            enum Foo { A(Int32), B(Float32), C}
            fn foo_test(y: Foo): Foo { let x: Foo = y; x }
        ");
    }

    #[test]
    fn enum_generic() {
        ok("
            enum Foo[T] { One(T), Two }
        ");
    }

    #[test]
    fn enum_with_type_param() {
        ok("trait SomeTrait {} enum MyOption[T: SomeTrait] { None, Some(T) }");
    }

    #[test]
    fn enum_generic_with_failures() {
        err(
            "enum MyOption[] { A, B }",
            (1, 14),
            2,
            crate::ErrorLevel::Error,
            &TYPE_PARAMS_EXPECTED,
            args!(),
        );

        err(
            "enum MyOption[X, X] { A, B }",
            (1, 18),
            1,
            crate::ErrorLevel::Error,
            &TYPE_PARAM_NAME_NOT_UNIQUE,
            args!("X"),
        );

        err(
            "enum MyOption[X: NonExistingTrait] { A, B }",
            (1, 18),
            16,
            crate::ErrorLevel::Error,
            &UNKNOWN_IDENTIFIER,
            args!("NonExistingTrait"),
        );
    }

    #[test]
    fn check_enum_type() {
        err(
            "
                enum MyOption[X] { A, B }
                fn foo(v: MyOption) {}
            ",
            (3, 27),
            8,
            crate::ErrorLevel::Error,
            &WRONG_NUMBER_TYPE_PARAMS,
            args!(1, 0),
        );
    }

    #[test]
    fn check_enum_value() {
        ok("
            enum Foo { A(Int32), B }
            fn foo(): Foo { Foo::A(1i32) }
            fn bar(): Foo { Foo::B }
        ");

        err(
            "
            enum Foo { A(Int32), B }
            fn foo(): Foo { Foo::A(true) }
        ",
            (3, 36),
            4,
            crate::ErrorLevel::Error,
            &WRONG_TYPE_FOR_ARGUMENT,
            args!("Int32", "Bool"),
        );
    }

    #[test]
    fn check_enum_value_generic() {
        ok("
            enum Foo[T] { A, B }
            fn foo() { let tmp = Foo[String]::B; }
        ");

        err(
            "
            trait SomeTrait {}
            enum Foo[T: SomeTrait] { A, B }
            fn foo() { let tmp = Foo[String]::B; }
        ",
            (4, 45),
            2,
            crate::ErrorLevel::Error,
            &TYPE_NOT_IMPLEMENTING_TRAIT,
            args!("String", "SomeTrait"),
        );
    }

    #[test]
    fn enum_with_generic_argument() {
        ok("
            enum Foo[T] { A(T), B }
            fn foo() { let tmp = Foo[Int32]::A(0i32); }
        ");

        err(
            "
            enum Foo[T] { A(T), B }
            fn foo() { let tmp = Foo[Int32]::A(true); }
        ",
            (3, 48),
            4,
            crate::ErrorLevel::Error,
            &WRONG_TYPE_FOR_ARGUMENT,
            args!("Int32", "Bool"),
        );
    }

    #[test]
    fn enum_move_generic() {
        ok("
            enum Foo[T] { A(T), B }
            fn foo(x: Foo[Int32]): Foo[Int32] { x }
        ");

        err(
            "
            enum Foo[T] { A(T), B }
            fn foo(x: Foo[Int32]): Foo[Float32] { x }
        ",
            (3, 49),
            5,
            crate::ErrorLevel::Error,
            &RETURN_TYPE,
            args!("Foo[Float32]", "Foo[Int32]"),
        );
    }

    #[test]
    fn enum_nested() {
        ok("
            enum Foo { A(Foo), B }
        ");
    }

    #[test]
    fn alias_type_as_enum_field() {
        ok("
            type MyInt = Int64;
            enum Foo { A(MyInt), B }
            fn f(v: Int64): Foo {
                Foo::A(v)
            }
        ");
    }

    #[test]
    fn enum_with_where_bounds() {
        ok("
            trait MyTrait {}
            enum Foo[T] where T: MyTrait { A, B }
        ");

        ok("
            trait MyTrait {}
            enum Foo[T] where Option[T]: MyTrait { A, B }
        ");

        err(
            "
            trait MyTrait {}
            enum Foo[T] where F: MyTrait { A, B }
        ",
            (3, 31),
            1,
            crate::ErrorLevel::Error,
            &UNKNOWN_IDENTIFIER,
            args!("F"),
        );

        err(
            "
            enum Foo[T] where T: Int64 { A, B }
        ",
            (2, 34),
            5,
            crate::ErrorLevel::Error,
            &BOUND_EXPECTED,
            args!(),
        );
    }
}
