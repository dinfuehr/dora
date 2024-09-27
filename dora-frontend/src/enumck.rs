use crate::sema::Sema;
use crate::{parsety, AliasReplacement};

pub fn check(sa: &Sema) {
    for (_id, enum_) in sa.enums.iter() {
        let mut simple_enumeration = true;

        for variant in enum_.variants() {
            for parsed_ty in variant.types() {
                parsety::expand_parsed_type(
                    sa,
                    parsed_ty,
                    None,
                    AliasReplacement::ReplaceWithActualType,
                );
            }

            if variant.types().len() > 0 {
                simple_enumeration = false;
            }
        }

        assert!(enum_.simple_enumeration.set(simple_enumeration).is_ok());
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::ErrorMessage;
    use crate::tests::*;

    #[test]
    fn enum_definitions() {
        err("enum Foo {}", (1, 1), ErrorMessage::NoEnumVariant);
        ok("enum Foo { A, B, C }");
        err(
            "enum Foo { A, A }",
            (1, 15),
            ErrorMessage::ShadowEnumVariant("A".into()),
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
            (3, 35),
            ErrorMessage::EnumArgsIncompatible(
                "Foo".into(),
                "A".into(),
                vec!["Int32".into()],
                vec!["Float32".into()],
            ),
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
            ErrorMessage::EnumArgsIncompatible(
                "Foo".into(),
                "A".into(),
                vec!["Int32".into()],
                Vec::new(),
            ),
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
            ErrorMessage::EnumArgsIncompatible(
                "Foo".into(),
                "C".into(),
                Vec::new(),
                vec!["Float32".into()],
            ),
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
            ErrorMessage::EnumArgsNoParens("Foo".into(), "C".into()),
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
            ErrorMessage::TypeParamsExpected,
        );

        err(
            "enum MyOption[X, X] { A, B }",
            (1, 18),
            ErrorMessage::TypeParamNameNotUnique("X".into()),
        );

        err(
            "enum MyOption[X: NonExistingTrait] { A, B }",
            (1, 18),
            ErrorMessage::UnknownIdentifier("NonExistingTrait".into()),
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
            ErrorMessage::WrongNumberTypeParams(1, 0),
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
            (3, 29),
            ErrorMessage::EnumArgsIncompatible(
                "Foo".into(),
                "A".into(),
                vec!["Int32".into()],
                vec!["Bool".into()],
            ),
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
            ErrorMessage::TypeNotImplementingTrait("String".into(), "SomeTrait".into()),
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
            (3, 34),
            ErrorMessage::EnumArgsIncompatible(
                "Foo".into(),
                "A".into(),
                vec!["T".into()],
                vec!["Bool".into()],
            ),
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
            ErrorMessage::ReturnType("Foo[Float32]".into(), "Foo[Int32]".into()),
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
            ErrorMessage::UnknownIdentifier("F".into()),
        );

        err(
            "
            enum Foo[T] where T: Int64 { A, B }
        ",
            (2, 34),
            ErrorMessage::BoundExpected,
        );
    }
}
