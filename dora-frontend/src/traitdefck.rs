use fixedbitset::FixedBitSet;

use crate::args;
use crate::error::diagnostics::CYCLE_IN_HIERARCHY;
use crate::sema::{Element, Sema, TraitDefinitionId, TypeSymbol, type_ref_span};
use crate::{SourceType, SymbolKind};

pub fn check_super_trait_cycles(sa: &Sema) {
    let mut visited = FixedBitSet::with_capacity(sa.traits.len());

    for (_id, trait_) in sa.traits.iter() {
        check_super_trait_cycles_for_trait(sa, trait_.id(), &mut visited);
    }
}

fn check_super_trait_cycles_for_trait(
    sa: &Sema,
    start_trait_id: TraitDefinitionId,
    visited: &mut FixedBitSet,
) {
    let mut visiting = FixedBitSet::with_capacity(sa.traits.len());

    // Stack entries: (trait_id, bound_index)
    // bound_index tracks how far we've processed through own_bounds()
    let mut stack = vec![(start_trait_id, 0usize)];
    visiting.insert(start_trait_id.index());

    'outer: while let Some((trait_id, bound_index)) = stack.last().cloned() {
        if visited.contains(trait_id.index()) {
            stack.pop();
            continue;
        }

        let trait_ = sa.trait_(trait_id);
        let type_refs = trait_.type_ref_arena();

        for bound in trait_.type_param_definition.own_bounds().skip(bound_index) {
            stack.last_mut().unwrap().1 += 1;

            // Check if this bound is for Self
            if bound.parsed_ty().maybe_ty() != Some(SourceType::This) {
                continue;
            }

            // Get the TypeRefId for the trait type
            if let Some(type_ref_id) = bound.parsed_trait_ty().type_ref_id() {
                // Look up the symbol for this type ref
                if let Some(TypeSymbol::Symbol(SymbolKind::Trait(super_trait_id))) =
                    type_refs.symbol(type_ref_id)
                {
                    if visiting.contains(super_trait_id.index()) {
                        // Cycle detected - report error on the bound and clear the symbol
                        let span = type_ref_span(sa, type_refs, trait_.file_id, type_ref_id);
                        sa.report(trait_.file_id, span, &CYCLE_IN_HIERARCHY, args!());
                        type_refs.clear_symbol(type_ref_id);
                    } else {
                        // Push super trait to process first
                        visiting.insert(super_trait_id.index());
                        stack.push((super_trait_id, 0));
                        continue 'outer;
                    }
                }
            }
        }

        // No more super traits, mark as visited and pop
        visited.insert(trait_id.index());
        stack.pop();
    }
}

#[cfg(test)]
mod tests {
    use crate::args;
    use crate::error::diagnostics::{
        ALIAS_EXISTS, BOUND_EXPECTED, RETURN_TYPE, TRAIT_NOT_OBJECT_SAFE, TYPE_EXISTS,
        TYPE_NOT_IMPLEMENTING_TRAIT, UNKNOWN_IDENTIFIER, UNKNOWN_METHOD,
    };
    use crate::tests::*;

    #[test]
    fn trait_method_with_body() {
        ok("trait Foo { fn foo(): Int32 { return 1; } }");

        err(
            "trait Foo { fn foo() { self.bar(); } }",
            (1, 24),
            10,
            crate::ErrorLevel::Error,
            &UNKNOWN_METHOD,
            args!("Self", "bar"),
        );

        err(
            "trait Foo { fn foo(): Int32 { return false; } }",
            (1, 31),
            12,
            crate::ErrorLevel::Error,
            &RETURN_TYPE,
            args!("Int32", "Bool"),
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
            7,
            crate::ErrorLevel::Error,
            &UNKNOWN_IDENTIFIER,
            args!("Unknown"),
        );
        err(
            "trait Foo { fn foo(); fn foo(): Int32; }",
            (1, 23),
            16,
            crate::ErrorLevel::Error,
            &ALIAS_EXISTS,
            args!("foo", "main.dora:1:13"),
        );

        err(
            "trait Foo { fn foo(); fn foo(); }",
            (1, 23),
            9,
            crate::ErrorLevel::Error,
            &ALIAS_EXISTS,
            args!("foo", "main.dora:1:13"),
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
            15,
            crate::ErrorLevel::Error,
            &ALIAS_EXISTS,
            args!("foo", "main.dora:2:13"),
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
            7,
            crate::ErrorLevel::Error,
            &TYPE_EXISTS,
            args!("a", "main.dora:3:17"),
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
            1,
            crate::ErrorLevel::Error,
            &UNKNOWN_IDENTIFIER,
            args!("F"),
        );

        err(
            "
            trait Foo[T] where T: Int64 {}
        ",
            (2, 35),
            5,
            crate::ErrorLevel::Error,
            &BOUND_EXPECTED,
            args!(),
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
            13,
            crate::ErrorLevel::Error,
            &TRAIT_NOT_OBJECT_SAFE,
            args!(),
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
            18,
            crate::ErrorLevel::Error,
            &TYPE_NOT_IMPLEMENTING_TRAIT,
            args!("Int64", "Foo[X = String]"),
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
            3,
            crate::ErrorLevel::Error,
            &TRAIT_NOT_OBJECT_SAFE,
            args!(),
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
            3,
            crate::ErrorLevel::Error,
            &TRAIT_NOT_OBJECT_SAFE,
            args!(),
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
            3,
            crate::ErrorLevel::Error,
            &TRAIT_NOT_OBJECT_SAFE,
            args!(),
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
            3,
            crate::ErrorLevel::Error,
            &TRAIT_NOT_OBJECT_SAFE,
            args!(),
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
            3,
            crate::ErrorLevel::Error,
            &TRAIT_NOT_OBJECT_SAFE,
            args!(),
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
            3,
            crate::ErrorLevel::Error,
            &TRAIT_NOT_OBJECT_SAFE,
            args!(),
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
            3,
            crate::ErrorLevel::Error,
            &TRAIT_NOT_OBJECT_SAFE,
            args!(),
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
            3,
            crate::ErrorLevel::Error,
            &TRAIT_NOT_OBJECT_SAFE,
            args!(),
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
            7,
            crate::ErrorLevel::Error,
            &UNKNOWN_IDENTIFIER,
            args!("Unknown"),
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
            1,
            crate::ErrorLevel::Error,
            &TYPE_NOT_IMPLEMENTING_TRAIT,
            args!("Int64", "A"),
        );

        errors(
            "
            trait A {}
            trait B: A {}
            trait C: B {}
            impl B for Int64 {}
            impl C for Int64 {}
        ",
            vec![
                (
                    (5, 18),
                    1,
                    crate::ErrorLevel::Error,
                    &TYPE_NOT_IMPLEMENTING_TRAIT,
                    args!("Int64", "A"),
                ),
                (
                    (6, 18),
                    1,
                    crate::ErrorLevel::Error,
                    &TYPE_NOT_IMPLEMENTING_TRAIT,
                    args!("Int64", "B"),
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
            1,
            crate::ErrorLevel::Error,
            &TYPE_NOT_IMPLEMENTING_TRAIT,
            args!("Foo[T]", "A"),
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
            1,
            crate::ErrorLevel::Error,
            &TRAIT_NOT_OBJECT_SAFE,
            args!(),
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
            44,
            crate::ErrorLevel::Error,
            &RETURN_TYPE,
            args!("Int64", "Bool"),
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

    #[test]
    fn trait_default_method_returning_known_associated_type() {
        ok("
            trait Foo {
                fn bar[T: Bar[X=String]](x: T): T::X { x.toX() }
            }

            trait Bar {
                type X;
                fn toX(): Self::X;
            }
        ");
    }

    #[test]
    fn trait_default_method_returning_known_associated_type_fixed() {
        ok("
            trait Foo {
                fn bar[T: Bar[X=String]](x: T): String { x.toX() }
            }

            trait Bar {
                type X;
                fn toX(): Self::X;
            }
        ");
    }

    #[test]
    #[ignore]
    fn assoc_type_of_super_trait() {
        ok("
            trait Foo {
                type Item;
            }

            trait Bar: Foo {
                fn f(): Self::Item;
            }
        ");
    }
}
