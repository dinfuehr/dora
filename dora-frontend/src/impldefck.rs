use std::collections::{HashMap, HashSet};

use crate::extensiondefck::check_for_unconstrained_type_params;
use crate::sema::{
    implements_trait, AliasDefinitionId, FctDefinition, FctDefinitionId, ImplDefinition, Sema,
    TraitDefinition, TypeParamId,
};
use crate::specialize::replace_type;
use crate::{package_for_type, AliasReplacement, ErrorMessage, SourceType, SourceTypeArray};

pub fn check_definition(sa: &Sema) {
    for (_id, impl_) in sa.impls.iter() {
        check_impl_definition(sa, impl_);
    }
}

fn check_impl_definition(sa: &Sema, impl_: &ImplDefinition) {
    if !impl_.trait_ty().is_trait() && !impl_.trait_ty().is_error() {
        sa.report(impl_.file_id, impl_.ast.span, ErrorMessage::ExpectedTrait);
    }

    match impl_.extended_ty() {
        SourceType::TypeAlias(..) => unimplemented!(),
        SourceType::Any | SourceType::Ptr | SourceType::This => {
            unreachable!()
        }
        SourceType::Error
        | SourceType::Bool
        | SourceType::UInt8
        | SourceType::Char
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Class(..)
        | SourceType::Struct(..)
        | SourceType::Enum(..)
        | SourceType::Trait(..)
        | SourceType::Unit
        | SourceType::Lambda(..)
        | SourceType::Tuple(..)
        | SourceType::TypeParam(..) => {}
    }

    check_for_unconstrained_type_params(
        sa,
        impl_.extended_ty(),
        impl_.type_param_definition(),
        impl_.file_id,
        impl_.ast.span,
    );

    if impl_.trait_ty().is_trait() && !impl_.extended_ty().is_error() {
        let is_trait_foreign = package_for_type(sa, impl_.trait_ty()) != Some(impl_.package_id);
        let is_extended_ty_foreign =
            package_for_type(sa, impl_.extended_ty()) != Some(impl_.package_id);

        if is_trait_foreign && is_extended_ty_foreign {
            sa.report(
                impl_.file_id,
                impl_.ast.span,
                ErrorMessage::ImplTraitForeignType,
            );
        }
    }
}

pub fn check_definition_against_trait(sa: &Sema) {
    for (_id, impl_) in sa.impls.iter() {
        let trait_ty = impl_.trait_ty();

        if let Some(trait_id) = trait_ty.trait_id() {
            let trait_ = sa.trait_(trait_id);
            check_impl_methods(sa, impl_, trait_);
        }
    }
}

fn check_impl_methods(sa: &Sema, impl_: &ImplDefinition, trait_: &TraitDefinition) {
    let mut remaining_trait_methods: HashSet<FctDefinitionId> =
        trait_.methods().iter().cloned().collect();
    let mut trait_method_map = HashMap::new();

    let mut trait_alias_map: HashMap<AliasDefinitionId, SourceType> = HashMap::new();

    for (&trait_alias_id, &impl_alias_id) in impl_.trait_alias_map() {
        trait_alias_map.insert(trait_alias_id, sa.alias(impl_alias_id).ty());
    }

    for &impl_method_id in impl_.methods() {
        let impl_method = &sa.fct(impl_method_id);

        if let Some(trait_method_id) = trait_.get_method(impl_method.name, impl_method.is_static) {
            if let Some(existing_id) = trait_method_map.insert(trait_method_id, impl_method_id) {
                let existing_fct = sa.fct(existing_id);
                let method_name = sa.interner.str(existing_fct.name).to_string();

                sa.report(
                    impl_method.file_id,
                    impl_method.ast.span,
                    ErrorMessage::AliasExists(method_name, existing_fct.span),
                );
            }

            remaining_trait_methods.remove(&trait_method_id);

            let trait_method = sa.fct(trait_method_id);

            if !method_definitions_compatible(
                sa,
                trait_method,
                impl_.trait_ty().type_params(),
                &trait_alias_map,
                impl_method,
                impl_.extended_ty().clone(),
            ) {
                let msg = ErrorMessage::ImplMethodDefinitionMismatch;
                sa.report(impl_.file_id, impl_method.span, msg);
            }
        } else {
            sa.report(
                impl_.file_id,
                impl_method.span,
                ErrorMessage::ElementNotInTrait,
            )
        }
    }

    if !remaining_trait_methods.is_empty() {
        report_missing_methods(sa, impl_, trait_, remaining_trait_methods);
    }

    assert!(impl_.trait_method_map.set(trait_method_map).is_ok());
}

fn method_definitions_compatible(
    sa: &Sema,
    trait_method: &FctDefinition,
    trait_type_params: SourceTypeArray,
    trait_alias_map: &HashMap<AliasDefinitionId, SourceType>,
    impl_method: &FctDefinition,
    self_ty: SourceType,
) -> bool {
    let trait_params = trait_method.params_without_self();
    let impl_params = impl_method.params_without_self();

    if trait_params.len() != impl_params.len() {
        return false;
    }

    let fct_type_params = trait_method.type_param_definition().fct_type_params_len();

    if fct_type_params != impl_method.type_param_definition().fct_type_params_len() {
        return false;
    }

    let method_type_params = if fct_type_params > 0 {
        let fct_type_params = (0..fct_type_params)
            .into_iter()
            .map(|t| SourceType::TypeParam(TypeParamId(t)))
            .collect::<Vec<_>>();
        let fct_type_params = SourceTypeArray::with(fct_type_params);
        trait_type_params.connect(&fct_type_params)
    } else {
        trait_type_params
    };

    for (trait_arg_ty, impl_arg_ty) in trait_params.iter().zip(impl_params.iter()) {
        if !trait_and_impl_arg_ty_compatible(
            sa,
            trait_arg_ty.ty(),
            method_type_params.clone(),
            trait_alias_map,
            impl_arg_ty.ty(),
            self_ty.clone(),
        ) {
            return false;
        }
    }

    trait_and_impl_arg_ty_compatible(
        sa,
        trait_method.return_type(),
        method_type_params.clone(),
        trait_alias_map,
        impl_method.return_type(),
        self_ty.clone(),
    )
}

fn trait_and_impl_arg_ty_compatible(
    sa: &Sema,
    trait_arg_ty: SourceType,
    trait_type_params: SourceTypeArray,
    trait_alias_map: &HashMap<AliasDefinitionId, SourceType>,
    impl_arg_ty: SourceType,
    self_ty: SourceType,
) -> bool {
    replace_type(
        sa,
        trait_arg_ty.clone(),
        Some(&trait_type_params),
        Some(self_ty.clone()),
        AliasReplacement::Map(trait_alias_map),
    ) == impl_arg_ty.clone()
}

fn report_missing_methods(
    sa: &Sema,
    impl_: &ImplDefinition,
    _trait: &TraitDefinition,
    missing_methods: HashSet<FctDefinitionId>,
) {
    for method_id in missing_methods {
        let method = sa.fct(method_id);
        let mtd_name = sa.interner.str(method.name).to_string();

        sa.report(
            impl_.file_id,
            impl_.declaration_span,
            ErrorMessage::ElementNotInImpl(mtd_name),
        )
    }
}

pub fn check_type_aliases(sa: &Sema) {
    for (_id, impl_) in sa.impls.iter() {
        let trait_ty = impl_.trait_ty();

        if let Some(trait_id) = trait_ty.trait_id() {
            let trait_ = sa.trait_(trait_id);
            check_impl_types(sa, impl_, trait_);
        }
    }
}

fn check_impl_types(sa: &Sema, impl_: &ImplDefinition, trait_: &TraitDefinition) {
    let mut remaining_aliases: HashSet<AliasDefinitionId> =
        trait_.aliases().iter().cloned().collect();
    let mut trait_alias_map = HashMap::new();

    for &impl_alias_id in impl_.aliases() {
        let impl_alias = sa.alias(impl_alias_id);

        if let Some(&trait_alias_id) = trait_.alias_names().get(&impl_alias.name) {
            if let Some(existing_id) = trait_alias_map.insert(trait_alias_id, impl_alias_id) {
                let existing_alias = sa.alias(existing_id);
                let method_name = sa.interner.str(existing_alias.name).to_string();

                sa.report(
                    impl_alias.file_id,
                    impl_alias.node.span,
                    ErrorMessage::AliasExists(method_name, existing_alias.node.span),
                );
            }

            let trait_alias = sa.alias(trait_alias_id);

            for bound in trait_alias.bounds() {
                if !implements_trait(
                    sa,
                    impl_alias.ty(),
                    impl_.type_param_definition(),
                    bound.ty(),
                ) {
                    let name = impl_alias
                        .ty()
                        .name_with_type_params(sa, impl_.type_param_definition());
                    let trait_name = bound
                        .ty()
                        .name_with_type_params(sa, trait_.type_param_definition());
                    let msg = ErrorMessage::TypeNotImplementingTrait(name, trait_name);
                    sa.report(impl_.file_id, impl_alias.node.span, msg);
                }
            }

            remaining_aliases.remove(&trait_alias_id);
        } else {
            sa.report(
                impl_.file_id,
                impl_alias.node.span,
                ErrorMessage::ElementNotInTrait,
            )
        }
    }

    assert!(impl_.trait_alias_map.set(trait_alias_map).is_ok());
}

#[cfg(test)]
mod tests {
    use crate::error::msg::ErrorMessage;
    use crate::tests::*;
    use dora_parser::Span;

    #[test]
    fn impl_method_without_body() {
        err(
            "
            trait Foo {
                fn foo(): Int32;
            }
            class Bar
            impl Foo for Bar { fn foo(): Int32; }",
            (6, 32),
            ErrorMessage::MissingFctBody,
        );
    }

    #[test]
    fn impl_method_defined_twice() {
        err(
            "
            trait Foo {
                fn foo(): Int32;
            }
            class Bar
            impl Foo for Bar {
                fn foo(): Int32 { return 0; }
                fn foo(): Int32 { return 1; }
            }",
            (8, 17),
            ErrorMessage::AliasExists("foo".into(), Span::new(141, 29)),
        );
    }

    #[test]
    fn impl_for_unknown_trait() {
        err(
            "class A impl Foo for A {}",
            (1, 14),
            ErrorMessage::UnknownIdentifier("Foo".into()),
        );
    }

    #[test]
    fn impl_for_unknown_class() {
        err(
            "trait Foo {} impl Foo for A {}",
            (1, 27),
            ErrorMessage::UnknownIdentifier("A".into()),
        );

        ok("trait Foo {} trait A {} impl Foo for A {}");
    }

    #[test]
    fn impl_definitions() {
        ok("trait Foo {} class A {} impl Foo for A {}");
        ok("trait Foo { fn toBool(): Bool; }
            class A {}
            impl Foo for A { fn toBool(): Bool { return false; } }");
    }

    #[test]
    fn impl_struct() {
        ok("
            trait Foo {}
            struct A(x: Int32)
            impl Foo for A {}
        ");
        ok("
            trait Foo {}
            struct A[T](x: Int32)
            impl Foo for A[Int32] {}
            impl Foo for A[Float32] {}
        ");
        ok("
            trait Foo {}
            struct A[T](x: Int32)
            impl[T] Foo for A[T] {}
        ");
    }

    #[test]
    fn impl_enum() {
        ok("
            trait Foo {}
            enum A { B, C }
            impl Foo for A {}
        ");
        ok("
            trait Foo {}
            enum A[T] { A, B }
            impl Foo for A[Int32] {}
            impl Foo for A[Float32] {}
        ");
        ok("
            trait Foo {}
            enum A[T] { A, B }
            impl[T] Foo for A[T] {}
        ");
    }

    #[test]
    fn impl_class_type_params() {
        ok("trait MyTrait {} class Foo[T] impl MyTrait for Foo[String] {}");
    }

    #[test]
    fn impl_unconstrained_type_param() {
        err(
            "
            struct MyFoo[T]
            trait MyTrait {}
            impl[T] MyTrait for MyFoo[Int32] {}
        ",
            (4, 13),
            ErrorMessage::UnconstrainedTypeParam("T".into()),
        );
    }

    #[test]
    fn impl_mod() {
        err(
            "
            mod foo { trait MyTrait {} }
            class Foo
            impl foo::MyTrait for Foo {}",
            (4, 18),
            ErrorMessage::NotAccessible,
        );

        err(
            "
            mod foo { class Foo }
            trait MyTrait {}
            impl MyTrait for foo::Foo {}",
            (4, 30),
            ErrorMessage::NotAccessible,
        );
    }

    #[test]
    fn impl_generic_trait() {
        ok("
            trait MyEquals[T] { fn equals(val: T): Bool; }
            class Foo(x: Int64)
            impl MyEquals[Foo] for Foo {
                fn equals(val: Foo): Bool {
                    self.x == val.x
                }
            }
        ")
    }

    #[test]
    fn method_not_in_trait() {
        err(
            "
            trait Foo {}
            class A
            impl Foo for A {
                fn bar() {}
            }",
            (5, 17),
            ErrorMessage::ElementNotInTrait,
        );
    }

    #[test]
    fn alias_not_in_trait() {
        err(
            "
            trait Foo {}
            class A
            impl Foo for A {
                type X = Int64;
            }",
            (5, 17),
            ErrorMessage::ElementNotInTrait,
        );
    }

    #[test]
    fn alias_in_impl_multiple_times() {
        err(
            "
            trait Foo {
                type X;
            }
            class A
            impl Foo for A {
                type X = Int64;
                type X = Bool;
            }",
            (8, 17),
            ErrorMessage::AliasExists("X".into(), Span::new(128, 15)),
        );
    }

    #[test]
    fn method_missing_in_impl() {
        err(
            "
            trait Foo {
                fn bar();
            }
            class A
            impl Foo for A {}",
            (6, 13),
            ErrorMessage::ElementNotInImpl("bar".into()),
        );
    }

    #[test]
    fn method_returning_self() {
        ok("trait Foo {
                fn foo(): Self;
            }

            class A

            impl Foo for A {
                fn foo(): A { return A(); }
            }");
    }

    #[test]
    fn static_method_not_in_trait() {
        err(
            "
            trait Foo {}
            class A
            impl Foo for A {
                static fn bar() {}
            }",
            (5, 24),
            ErrorMessage::ElementNotInTrait,
        );
    }

    #[test]
    fn static_method_missing_in_impl() {
        err(
            "
            trait Foo {
                static fn bar();
            }
            class A
            impl Foo for A {}",
            (6, 13),
            ErrorMessage::ElementNotInImpl("bar".into()),
        );
    }

    #[test]
    fn method_return_type_check() {
        err(
            "trait X {
                fn m(): Bool;
                fn n(): Bool;
              }

              class CX

              impl X for CX {
                fn m(): Int32 { 0 }
                fn n(): Bool { true }
              }",
            (9, 17),
            ErrorMessage::ImplMethodDefinitionMismatch,
        );
    }

    #[test]
    fn method_params_type_check() {
        err(
            "trait X {
                fn f(a: Int64, b: Int64): Bool;
              }

              class CX

              impl X for CX {
                fn f(a: Int64, b: Int32): Bool { true }
              }",
            (8, 17),
            ErrorMessage::ImplMethodDefinitionMismatch,
        );
    }

    #[test]
    fn self_in_impl() {
        ok("
            trait X { fn f(a: Self); }
            class CX
            impl X for CX { fn f(a: CX) {} }
        ");
    }

    #[test]
    fn alias_use_in_impl() {
        ok("
            trait MyTrait {
                type X;
                fn next(): Option[X];
            }
            class CX
            impl MyTrait for CX {
                type X = Int64;
                fn next(): Option[X] {
                    None
                }
            }
        ");
    }

    #[test]
    fn alias_value_use_in_impl() {
        ok("
            trait MyTrait {
                type X;
                fn next(): Option[X];
            }
            class CX
            impl MyTrait for CX {
                type X = Int64;
                fn next(): Option[Int64] {
                    None
                }
            }
        ");

        err(
            "
            trait MyTrait {
                type X;
                fn next(): Option[X];
            }
            class CX
            impl MyTrait for CX {
                type X = Int64;
                fn next(): Option[String] {
                    None
                }
            }
        ",
            (9, 17),
            ErrorMessage::ImplMethodDefinitionMismatch,
        );
    }

    #[test]
    fn use_regular_alias_in_impl() {
        ok("
            type A = Int64;
            trait MyTrait {
                type X;
                fn f(a: A, x: X);
            }
            class CX
            impl MyTrait for CX {
                type X = Int64;
                fn f(a: Int64, x: X) {}
            }
        ");
    }

    #[test]
    fn use_second_alias_type_in_impl() {
        err(
            "
            trait MyTrait {
                type A;
                type B;
            }

            impl MyTrait for Int32 {
                type A = B;
                type B = Int64;
            }
        ",
            (8, 26),
            ErrorMessage::UnknownIdentifier("B".into()),
        );
    }

    #[test]
    fn use_self_in_impl() {
        ok("
            trait MyTrait {
                fn take(x: Self);
            }

            impl MyTrait for Int32 {
                fn take(x: Self) {}
            }

            fn f(x: Int32, y: Int32) {
                x.take(y);
            }
        ");
    }

    #[test]
    fn impl_with_where_bounds() {
        ok("
            trait MyTrait {}
            trait Foo {}
            class Bar[T]
            impl[T] Foo for Bar[T] where T: MyTrait {}
        ");

        ok("
            trait MyTrait {}
            trait Foo {}
            class Bar[T]
            impl[T] Foo for Bar[T] where Option[T]: MyTrait {}
        ");

        err(
            "
            trait MyTrait {}
            trait Foo {}
            class Bar[T]
            impl[T] Foo for Bar[T] where F: MyTrait {}
        ",
            (5, 42),
            ErrorMessage::UnknownIdentifier("F".into()),
        );

        err(
            "
            trait Foo {}
            class Bar[T]
            impl[T] Foo for Bar[T] where T: Int64 {}
        ",
            (4, 45),
            ErrorMessage::BoundExpected,
        );
    }

    #[test]
    fn impl_alias_with_bounds() {
        err(
            "
            trait Foo {
                type Ty: Bar;
            }
            trait Bar {}
            impl Foo for Int64 {
                type Ty = String;
            }
        ",
            (7, 17),
            ErrorMessage::TypeNotImplementingTrait("String".into(), "Bar".into()),
        );
    }

    #[test]
    fn impl_tuple() {
        ok("
            trait Foo {
                fn foo(): Int64;
            }
            impl Foo for (Int64, Int64) {
                fn foo(): Int64 { self.0 }
            }
            fn f(x: (Int64, Int64)): Int64 {
                x.foo()
            }
        ")
    }

    #[test]
    fn impl_lambda() {
        ok("
            trait Foo {
                fn foo(): Int64;
            }
            impl Foo for (Int64, Int64): Int64 {
                fn foo(): Int64 { self(1, 2) }
            }
            fn f(x: (Int64, Int64): Int64): Int64 {
                x.foo()
            }
        ");

        err(
            "
        trait Foo {
            fn foo(): Int64;
        }
        impl Foo for (Int64, Int64): Int64 {
            fn foo(): Int64 { self(1, 2) }
        }
        fn f(x: (Int64, String): Int64): Int64 {
            x.foo()
        }
    ",
            (9, 13),
            ErrorMessage::UnknownMethod("(Int64, String) -> Int64".into(), "foo".into(), vec![]),
        );

        err(
            "
        trait Foo {
            fn foo(): Int64;
        }
        impl Foo for (Int64, Int64): Int64 {
            fn foo(): Int64 { self(1, 2) }
        }
        fn f(x: (Int64, Int64): Bool): Int64 {
            x.foo()
        }
    ",
            (9, 13),
            ErrorMessage::UnknownMethod("(Int64, Int64) -> Bool".into(), "foo".into(), vec![]),
        );
    }

    #[test]
    fn impl_trait_object() {
        ok("
            trait Foo {
                fn foo(): Int64;
            }
            trait Bar {}
            impl Foo for Bar {
                fn foo(): Int64 { 0 }
            }
            fn f(x: Bar): Int64 {
                x.foo()
            }
        ");
    }

    #[test]
    fn impl_type_param() {
        ok("
            trait Foo {
                fn mytest(): Int64;
            }
            impl[T] Foo for T {
                fn mytest(): Int64 { 0 }
            }
            fn f(x: Bool): Int64 {
                x.mytest()
            }
        ");
    }
}
