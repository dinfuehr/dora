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
            ErrorMessage::UnknownMethod("Self".into(), "bar".into()),
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
    fn trait_method_using_another_trait_method_generic() {
        ok("
            trait Foo[T] {
                fn foo(): Int64;
                fn bar(): Int64 { self.foo() }
            }
        ");
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
    fn trait_object_safe_generic_alias() {
        err(
            "
            trait Foo {
                type X[T];
            }
            fn f(x: Foo[X=String]) {}
        ",
            (5, 21),
            ErrorMessage::TraitNotObjectSafe,
        );
    }

    #[test]
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
    fn trait_object_safe_alias_as_wrong_binding() {
        err(
            "
        trait Foo {
            type X;
        }
        impl Foo for Int64 {
            type X = Float64;
        }
        fn f(x: Int64) {
            x as Foo[X=String];
        }
    ",
            (9, 13),
            ErrorMessage::TypeNotImplementingTrait("Int64".into(), "Foo[X = String]".into()),
        );
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

    #[test]
    fn super_trait() {
        ok("
            trait Bar {}
            trait Foo: Bar {}
        ");
    }

    #[test]
    fn super_trait_unknown() {
        err(
            "
            trait Foo: Unknown {}
        ",
            (2, 24),
            ErrorMessage::UnknownIdentifier("Unknown".into()),
        );
    }

    #[test]
    fn super_trait_call() {
        ok("
            trait Bar {
                fn g();
            }
            trait Foo: Bar {
                fn f() { self.g(); }
            }
        ");
    }

    #[test]
    fn super_trait_unsatisfied_in_impl() {
        err(
            "
            trait A {}
            trait B: A {}
            impl B for Int64 {}
        ",
            (4, 18),
            ErrorMessage::TypeNotImplementingTrait("Int64".into(), "A".into()),
        );

        errors(
            "
            trait A {}
            trait B: A {}
            trait C: B {}
            impl B for Int64 {}
            impl C for Int64 {}
        ",
            &[
                (
                    (5, 18),
                    ErrorMessage::TypeNotImplementingTrait("Int64".into(), "A".into()),
                ),
                (
                    (6, 18),
                    ErrorMessage::TypeNotImplementingTrait("Int64".into(), "B".into()),
                ),
            ],
        );
    }

    #[test]
    fn super_trait_in_impl() {
        ok("
            trait A {}
            trait B: A {}
            impl B for Int64 {}
            impl A for Int64 {}
        ");
    }

    #[test]
    fn super_trait_generic_call_in_super_trait() {
        ok("
            trait A { fn f(); }
            trait B: A { fn g(); }
            fn f[T: B](value: T) {
                value.f();
            }
        ");

        ok("
            trait A { fn f(); }
            trait B: A { fn g(); }
            trait C: B { fn h(); }
            fn f[T: C](value: T) {
                value.f();
            }
        ");
    }

    #[test]
    fn super_trait_call_in_default_implementation() {
        ok("
            trait A { fn f(); }
            trait B: A { fn g(); }
            trait C: B { fn h() { self.f(); } }
        ");
    }

    #[test]
    fn super_trait_call_on_trait_object() {
        ok("
            trait A { fn f(); }
            trait B: A { fn g(); }
            trait C: B { fn h() { self.f(); } }
            fn f(c: C) {
                c.f();
            }
        ");
    }

    #[test]
    fn check_super_trait_on_generic() {
        ok("
            trait A {}
            trait B: A {}
            class Data[T: B]
            impl A for Int64 {}
            impl B for Int64 {}
            fn f() {
                let _ = Data[Int64]();
            }
        ");
    }

    #[test]
    fn check_super_trait_operator_overloading() {
        ok("
            trait A: std::Equals {}
            impl A for Int64 {}
            fn f[T: A](lhs: T, rhs: T): Bool {
                lhs == rhs
            }
        ");
    }

    #[test]
    fn check_super_trait_on_generic_impl() {
        err(
            "
            trait A {}
            trait B: A {}
            class Foo[T](T)
            impl[T] B for Foo[T] {}
        ",
            (5, 21),
            ErrorMessage::TypeNotImplementingTrait("Foo[T]".into(), "A".into()),
        );

        ok("
            trait A {}
            trait B: A {}
            class Foo[T](T)
            impl[T] B for Foo[T] {}
            impl[T] A for Foo[T] {}
        ");
    }

    #[test]
    fn super_trait_object_safe() {
        ok("
            trait A {
                fn f();
            }
            trait B: A {
                fn g();
            }
            fn f(b: B) {}
        ");

        err(
            "
            trait A {
                static fn f();
            }
            trait B: A {
                fn g();
            }
            fn f(b: B) {}
        ",
            (8, 21),
            ErrorMessage::TraitNotObjectSafe,
        );
    }

    #[test]
    fn trait_default_method_containing_self() {
        ok("
            trait TraitA {
                fn f(): Foo[Self] {
                    Foo[Self]()
                }
            }

            class Foo[T: TraitA]
        ");
    }

    #[test]
    fn error_in_trait_default_method_reported_once() {
        err(
            "
            trait TraitA {
                fn f(): Int {
                    true
                }
            }

            impl TraitA for Int {}
            impl TraitA for Float64 {}
        ",
            (3, 29),
            ErrorMessage::ReturnType("Int64".into(), "Bool".into()),
        );
    }

    #[test]
    fn trait_default_method_containing_self_and_super() {
        ok("
            trait SuperTrait {}

            trait TraitA: SuperTrait {
                fn f(): Foo[Self] {
                    Foo[Self]()
                }
            }

            class Foo[T: TraitA + SuperTrait]
        ");
    }

    #[test]
    fn trait_default_method_containing_and_using_self() {
        ok("
            trait TraitA {
                fn f(): Foo[Self] {
                    Foo[Self](self)
                }
            }

            class Foo[T: TraitA](T)
        ");
    }
}
