use std::collections::HashMap;

use dora_parser::ast::SyntaxNodeBase;

use crate::element_collector::Annotations;
use crate::error::diagnostics::INVALID_EQUALS_ANNOTATION_USAGE;
use crate::sema::{
    Element, ElementId, FctDefinition, FctParent, ImplDefinition, Intrinsic, Param, Params, Sema,
    TypeParamDefinition, TypeRefArena,
};
use crate::{SourceType, TraitType, args};

pub fn check(sa: &mut Sema) {
    let mut derive_equals = Vec::new();

    for (enum_id, enum_) in sa.enums.iter() {
        let mut simple_enumeration = true;

        for &variant_id in enum_.variant_ids() {
            let variant = sa.variant(variant_id);
            if variant.field_ids().len() > 0 {
                simple_enumeration = false;
            }
        }

        assert!(enum_.simple_enumeration.set(simple_enumeration).is_ok());

        if enum_.derive_equals {
            if simple_enumeration {
                derive_equals.push(enum_id);
            } else {
                let span = enum_
                    .ast(sa)
                    .modifier_list()
                    .and_then(|list| {
                        list.items().find(|modifier| {
                            modifier
                                .ident()
                                .is_some_and(|ident| ident.text() == "Equals")
                        })
                    })
                    .map(|modifier| modifier.span())
                    .unwrap_or(enum_.span);
                sa.report(
                    enum_.file_id,
                    span,
                    &INVALID_EQUALS_ANNOTATION_USAGE,
                    args!(),
                );
            }
        }
    }

    for enum_id in derive_equals {
        create_equals_impl(sa, enum_id);
    }
}

fn create_equals_impl(sa: &mut Sema, enum_id: crate::sema::EnumDefinitionId) {
    let (package_id, module_id, file_id, declaration_span, span, enum_type_param_definition) = {
        let enum_ = sa.enum_(enum_id);
        let declaration_span = enum_
            .ast(sa)
            .modifier_list()
            .and_then(|list| list.find_modifier("Equals"))
            .expect("missing Equals annotation")
            .span();
        (
            enum_.package_id,
            enum_.module_id,
            enum_.file_id,
            declaration_span,
            enum_.span,
            enum_.type_param_definition(sa).clone(),
        )
    };
    let enum_type_params = enum_type_param_definition.identity_type_params(sa);
    let enum_ty = SourceType::Enum(enum_id, enum_type_params);
    let impl_type_param_definition_id = sa.type_param_definitions.alloc(enum_type_param_definition);
    let equals_trait_id = sa.known.traits.equals();
    let trait_ty = TraitType::from_trait_id(equals_trait_id);

    let impl_ = ImplDefinition::new_synthetic(
        package_id,
        module_id,
        file_id,
        declaration_span,
        span,
        impl_type_param_definition_id,
        trait_ty,
        enum_ty.clone(),
    );
    let impl_id = sa.impls.alloc(impl_);
    assert!(sa.impls[impl_id].id.set(impl_id).is_ok());
    sa.impls[impl_id].set_type_refs(TypeRefArena::new());

    let method_type_param_definition =
        TypeParamDefinition::new(sa, Some(impl_type_param_definition_id));
    let method_type_param_definition_id = sa
        .type_param_definitions
        .alloc(method_type_param_definition);
    let method_name = sa.interner.intern("equals");
    let mut modifiers = Annotations::default();
    modifiers.is_internal = true;
    modifiers.is_force_inline = true;
    let params = Params::new(
        vec![
            Param::new_ty(SourceType::This),
            Param::new_ty(enum_ty.clone()),
        ],
        true,
        false,
    );
    let method = FctDefinition::new_no_source(
        package_id,
        module_id,
        file_id,
        declaration_span,
        span,
        None,
        modifiers,
        method_name,
        method_type_param_definition_id,
        params,
        SourceType::Bool,
        FctParent::Impl(impl_id),
        false,
    );
    let method_id = sa.fcts.alloc(method);
    sa.fcts[method_id].id = Some(method_id);
    sa.fcts[method_id].set_type_refs(TypeRefArena::new());
    assert!(sa.fcts[method_id].intrinsic.set(Intrinsic::EnumEq).is_ok());

    let trait_method_id = sa
        .trait_(equals_trait_id)
        .get_method(method_name, false)
        .expect("Equals::equals missing");
    assert!(sa.impls[impl_id].methods.set(vec![method_id]).is_ok());
    assert!(sa.impls[impl_id].aliases.set(Vec::new()).is_ok());
    assert!(
        sa.impls[impl_id]
            .children
            .set(vec![ElementId::Fct(method_id)])
            .is_ok()
    );
    assert!(
        sa.impls[impl_id]
            .trait_method_map
            .set(HashMap::from([(trait_method_id, method_id)]))
            .is_ok()
    );
    assert!(
        sa.impls[impl_id]
            .trait_alias_map
            .set(HashMap::new())
            .is_ok()
    );
    sa.impls[impl_id].set_super_trait_witnesses(HashMap::new());
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
            (3, 35),
            6,
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
            (4, 34),
            14,
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
