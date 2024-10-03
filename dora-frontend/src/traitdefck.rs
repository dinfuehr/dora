#[cfg(test)]
mod tests {
    use crate::error::msg::ErrorMessage;
    use crate::tests::*;
    use dora_parser::Span;

    #[test]
    fn trait_method_with_body() {
        ok("trait Foo { fn foo(): Int32 { return 1; } }");

        err(
            "trait Foo { fn foo() { self.bar(); } }",
            (1, 24),
            ErrorMessage::UnknownMethod("Self".into(), "bar".into(), Vec::new()),
        );

        err(
            "trait Foo { fn foo(): Int32 { return false; } }",
            (1, 31),
            ErrorMessage::ReturnType("Int32".into(), "Bool".into()),
        );
    }

    #[test]
    fn trait_method_using_another_trait_method() {
        ok("
            trait Foo {
                fn foo(): Int64;
                fn bar(): Int64 { self.foo() }
            }");
    }

    #[test]
    #[ignore]
    fn trait_method_using_another_trait_method_generic() {
        ok("
        trait Foo[T] {
            fn foo(): Int64;
            fn bar(): Int64 { self.foo() }
        }");
    }

    #[test]
    fn trait_definitions() {
        ok("trait Foo {}");
        ok("trait Foo { fn toBool(): Bool; }");
        ok("trait Foo {
                fn toFloat32(): Float32;
                fn toFloat64(): Float64;
            }");

        err(
            "trait Bar { fn foo(): Unknown; }",
            (1, 23),
            ErrorMessage::UnknownIdentifier("Unknown".into()),
        );
        err(
            "trait Foo { fn foo(); fn foo(): Int32; }",
            (1, 23),
            ErrorMessage::AliasExists("foo".into(), Span::new(12, 9)),
        );

        err(
            "trait Foo { fn foo(); fn foo(); }",
            (1, 23),
            ErrorMessage::AliasExists("foo".into(), Span::new(12, 9)),
        );
    }

    #[test]
    fn trait_with_self() {
        err(
            "trait Foo {
            fn foo(): Int32;
            fn foo(): Self;
        }",
            (3, 13),
            ErrorMessage::AliasExists("foo".into(), Span::new(24, 16)),
        );
    }

    #[test]
    fn trait_with_multiple_types() {
        ok("
            trait Foo {
                type a;
                type b;
            }
        ");

        err(
            "
            trait Foo {
                type a;
                type a;
            }
        ",
            (4, 17),
            ErrorMessage::TypeExists("a".into(), Span::new(41, 7)),
        );
    }

    #[test]
    fn trait_with_where_bounds() {
        ok("
            trait MyTrait {}
            trait Foo[T] where T: MyTrait {}
        ");

        ok("
            trait MyTrait {}
            trait Foo[T] where Option[T]: MyTrait {}
        ");

        err(
            "
            trait MyTrait {}
            trait Foo[T] where F: MyTrait {}
        ",
            (3, 32),
            ErrorMessage::UnknownIdentifier("F".into()),
        );

        err(
            "
            trait Foo[T] where T: Int64 {}
        ",
            (2, 35),
            ErrorMessage::BoundExpected,
        );
    }

    #[test]
    fn trait_object_safe_alias() {
        ok("
            trait Foo {
                type X;
            }
            fn f(x: Foo[X=String]) {}
        ");
    }

    #[test]
    #[ignore]
    fn trait_object_safe_alias_as() {
        ok("
            trait Foo {
                type X;
            }
            impl Foo for Int64 {
                type X = String;
            }
            fn f(x: Int64) {
                x as Foo[X=String];
            }
        ");
    }

    #[test]
    fn trait_object_safe_static_method() {
        err(
            "
            trait Foo {
                static fn f();
            }
            impl Foo for Int64 {
                static fn f() {}
            }
            fn f(x: Int64) {
                x as Foo;
            }
        ",
            (9, 22),
            ErrorMessage::TraitNotObjectSafe,
        );

        err(
            "
            trait Foo {
                static fn f();
            }
            impl Foo for Int64 {
                static fn f() {}
            }
            fn f(x: Foo) {}
        ",
            (8, 21),
            ErrorMessage::TraitNotObjectSafe,
        );
    }

    #[test]
    fn trait_object_safe_trait_type_param() {
        ok("
            trait Foo[T] {
                fn f(): T;
            }
            impl Foo[Int64] for Int64 {
                fn f(): Self { self }
            }
            fn f(x: Int64) {
                x as Foo[Int64];
            }
        ");
    }

    #[test]
    fn trait_object_safe_fct_type_param() {
        err(
            "
            trait Foo {
                fn f[U](x: U): Self;
            }
            impl Foo for Int64 {
                fn f[U](_x: U): Self { self }
            }
            fn f(x: Int64) {
                x as Foo;
            }
        ",
            (9, 22),
            ErrorMessage::TraitNotObjectSafe,
        );

        err(
            "
            trait Foo {
                fn f[U](x: U): Self;
            }
            impl Foo for Int64 {
                fn f[U](_x: U): Self { self }
            }
            fn f(x: Foo) {}
        ",
            (8, 21),
            ErrorMessage::TraitNotObjectSafe,
        );
    }

    #[test]
    fn trait_object_safe_self_param() {
        err(
            "
            trait Foo {
                fn f(x: Self);
            }
            impl Foo for Int64 {
                fn f(x: Int64) {}
            }
            fn f(x: Int64) {
                x as Foo;
            }
        ",
            (9, 22),
            ErrorMessage::TraitNotObjectSafe,
        );

        err(
            "
            trait Foo {
                fn f(x: Self);
            }
            impl Foo for Int64 {
                fn f(x: Int64) {}
            }
            fn f(x: Foo) {}
        ",
            (8, 21),
            ErrorMessage::TraitNotObjectSafe,
        );
    }

    #[test]
    fn trait_object_safe_self_return_type() {
        err(
            "
            trait Foo {
                fn f(): Self;
            }
            impl Foo for Int64 {
                fn f(): Int64 { self }
            }
            fn f(x: Int64) {
                x as Foo;
            }
        ",
            (9, 22),
            ErrorMessage::TraitNotObjectSafe,
        );

        err(
            "
                trait Foo {
                    fn f(): Self;
                }
                impl Foo for Int64 {
                    fn f(): Int64 { self }
                }
                fn f(x: Foo) {}
            ",
            (8, 25),
            ErrorMessage::TraitNotObjectSafe,
        );
    }
}
