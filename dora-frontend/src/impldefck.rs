use std::collections::{HashMap, HashSet};

use crate::extensiondefck::check_for_unconstrained_type_params;
use crate::program_parser::ParsedModifierList;
use crate::sema::{
    implements_trait, new_identity_type_params, AliasDefinitionId, Element, FctDefinition,
    FctDefinitionId, FctParent, ImplDefinition, ImplDefinitionId, Sema, TraitDefinition,
};
use crate::{package_for_type, replace_type, ErrorMessage, SourceType, SourceTypeArray, TraitType};

pub fn check_definition(sa: &Sema) {
    for (_id, impl_) in sa.impls.iter() {
        check_impl_definition(sa, impl_);
    }
}

fn check_impl_definition(sa: &Sema, impl_: &ImplDefinition) {
    match impl_.extended_ty() {
        SourceType::Alias(..) => unimplemented!(),
        SourceType::Any
        | SourceType::Ptr
        | SourceType::This
        | SourceType::Assoc(..)
        | SourceType::GenericAssoc(..) => {
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
        | SourceType::TraitObject(..)
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

    if impl_.trait_ty().is_some() && !impl_.extended_ty().is_error() {
        let trait_id = impl_.trait_id().expect("expected trait");
        let is_trait_foreign = sa.trait_(trait_id).package_id != impl_.package_id;
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

struct LazyTraitMethodMapSetter {
    impl_id: ImplDefinitionId,
    trait_methods: Vec<FctDefinitionId>,
    trait_method_map: HashMap<FctDefinitionId, FctDefinitionId>,
}

pub fn check_definition_against_trait(sa: &mut Sema) {
    let mut lazy_trait_method_map_setters = Vec::new();

    for (_id, impl_) in sa.impls.iter() {
        if let Some(trait_ty) = impl_.trait_ty() {
            let trait_ = sa.trait_(trait_ty.trait_id);
            check_impl_methods(
                sa,
                impl_,
                &trait_ty,
                trait_,
                &mut lazy_trait_method_map_setters,
            );
        }
    }

    for setter in lazy_trait_method_map_setters {
        let impl_id = setter.impl_id;
        let mut trait_method_map = setter.trait_method_map;
        let trait_methods = setter.trait_methods;
        let mut new_fcts = Vec::new();

        {
            let impl_ = sa.impl_(setter.impl_id);

            for trait_method_id in &trait_methods {
                let trait_method = sa.fct(*trait_method_id);

                let self_ty = Some(impl_.extended_ty());
                let params = trait_method.params.clone();

                for param in &params.params {
                    param.set_ty(replace_type(sa, param.ty(), None, self_ty.clone()));
                }

                let return_type = trait_method.return_type();
                let return_type = replace_type(sa, return_type, None, self_ty.clone());

                let fct = FctDefinition::new(
                    impl_.package_id,
                    impl_.module_id,
                    impl_.file_id,
                    &trait_method.ast,
                    ParsedModifierList::default(),
                    trait_method.name,
                    trait_method.type_param_definition().clone(),
                    params,
                    FctParent::Impl(impl_.id()),
                );
                fct.return_type.set_ty(return_type);
                new_fcts.push(fct);
            }
        }

        for (new_fct, trait_method_id) in new_fcts.into_iter().zip(trait_methods.into_iter()) {
            let impl_method_id = sa.fcts.alloc(new_fct);
            sa.fcts[impl_method_id].id = Some(impl_method_id);

            let previous = trait_method_map.insert(trait_method_id, impl_method_id);
            assert!(previous.is_none());
        }

        let impl_ = sa.impl_(impl_id);
        assert!(impl_.trait_method_map.set(trait_method_map).is_ok());
    }
}

fn check_impl_methods(
    sa: &Sema,
    impl_: &ImplDefinition,
    trait_ty: &TraitType,
    trait_: &TraitDefinition,
    lazy_trait_method_map_setters: &mut Vec<LazyTraitMethodMapSetter>,
) {
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
                trait_ty.type_params.clone(),
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

    let mut default_impl_methods = Vec::new();

    for trait_method_id in &remaining_trait_methods {
        let trait_method = sa.fct(*trait_method_id);

        if trait_method.has_body() {
            default_impl_methods.push(*trait_method_id);
        } else {
            let mtd_name = sa.interner.str(trait_method.name).to_string();

            sa.report(
                impl_.file_id,
                impl_.declaration_span,
                ErrorMessage::ElementNotInImpl(mtd_name),
            )
        }
    }

    if default_impl_methods.is_empty() {
        assert!(impl_.trait_method_map.set(trait_method_map).is_ok());
    } else {
        lazy_trait_method_map_setters.push(LazyTraitMethodMapSetter {
            impl_id: impl_.id(),
            trait_method_map,
            trait_methods: default_impl_methods,
        });
    }
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

    let fct_type_params = trait_method.type_param_definition().own_type_params_len();

    if fct_type_params != impl_method.type_param_definition().own_type_params_len() {
        return false;
    }

    let method_type_params = if fct_type_params > 0 {
        trait_type_params.connect(&new_identity_type_params(fct_type_params))
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
    match trait_arg_ty {
        SourceType::Class(trait_cls_id, trait_cls_type_params) => match impl_arg_ty {
            SourceType::Class(impl_cls_id, impl_cls_type_params) => {
                trait_cls_id == impl_cls_id
                    && trait_and_impl_arg_ty_compatible_array(
                        sa,
                        trait_cls_type_params,
                        trait_type_params,
                        trait_alias_map,
                        impl_cls_type_params,
                        self_ty,
                    )
            }

            _ => false,
        },

        SourceType::TraitObject(trait_trait_id, trait_trait_type_params, trait_trait_bindings) => {
            match impl_arg_ty {
                SourceType::TraitObject(
                    impl_trait_id,
                    impl_trait_type_params,
                    impl_trait_bindings,
                ) => {
                    trait_trait_id == impl_trait_id
                        && trait_and_impl_arg_ty_compatible_array(
                            sa,
                            trait_trait_type_params,
                            trait_type_params.clone(),
                            trait_alias_map,
                            impl_trait_type_params,
                            self_ty.clone(),
                        )
                        && trait_and_impl_arg_ty_compatible_array(
                            sa,
                            trait_trait_bindings,
                            trait_type_params,
                            trait_alias_map,
                            impl_trait_bindings,
                            self_ty,
                        )
                }

                _ => false,
            }
        }

        SourceType::Struct(trait_struct_id, trait_struct_type_params) => match impl_arg_ty {
            SourceType::Struct(impl_struct_id, impl_struct_type_params) => {
                trait_struct_id == impl_struct_id
                    && trait_and_impl_arg_ty_compatible_array(
                        sa,
                        trait_struct_type_params,
                        trait_type_params,
                        trait_alias_map,
                        impl_struct_type_params,
                        self_ty,
                    )
            }

            _ => false,
        },

        SourceType::Enum(trait_enum_id, trait_enum_type_params) => match impl_arg_ty {
            SourceType::Enum(impl_enum_id, impl_enum_type_params) => {
                trait_enum_id == impl_enum_id
                    && trait_and_impl_arg_ty_compatible_array(
                        sa,
                        trait_enum_type_params,
                        trait_type_params,
                        trait_alias_map,
                        impl_enum_type_params,
                        self_ty,
                    )
            }

            _ => false,
        },

        SourceType::Lambda(trait_arg_params, trait_arg_return_type) => match impl_arg_ty {
            SourceType::Lambda(impl_arg_params, impl_arg_return_type) => {
                trait_and_impl_arg_ty_compatible_array(
                    sa,
                    trait_arg_params,
                    trait_type_params.clone(),
                    trait_alias_map,
                    impl_arg_params,
                    self_ty.clone(),
                ) && trait_and_impl_arg_ty_compatible(
                    sa,
                    *trait_arg_return_type,
                    trait_type_params,
                    trait_alias_map,
                    *impl_arg_return_type,
                    self_ty,
                )
            }

            _ => false,
        },

        SourceType::Tuple(trait_tuple_subtypes) => {
            if let Some(impl_tuple_subtypes) = impl_arg_ty.tuple_subtypes() {
                trait_and_impl_arg_ty_compatible_array(
                    sa,
                    trait_tuple_subtypes,
                    trait_type_params,
                    trait_alias_map,
                    impl_tuple_subtypes,
                    self_ty,
                )
            } else {
                false
            }
        }

        SourceType::Assoc(id, type_params) => {
            assert!(type_params.is_empty());
            let ty = trait_alias_map.get(&id).unwrap_or(&SourceType::Error);
            ty.allows(sa, impl_arg_ty)
        }

        SourceType::TypeParam(id) => trait_type_params[id.index()].allows(sa, impl_arg_ty),

        SourceType::This => self_ty.allows(sa, impl_arg_ty),

        SourceType::Unit
        | SourceType::UInt8
        | SourceType::Bool
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64 => trait_arg_ty.allows(sa, impl_arg_ty),

        SourceType::Error => true,

        SourceType::Alias(..)
        | SourceType::Any
        | SourceType::Ptr
        | SourceType::GenericAssoc(..) => unreachable!(),
    }
}

fn trait_and_impl_arg_ty_compatible_array(
    sa: &Sema,
    trait_arg_types: SourceTypeArray,
    trait_type_params: SourceTypeArray,
    trait_alias_map: &HashMap<AliasDefinitionId, SourceType>,
    impl_arg_types: SourceTypeArray,
    self_ty: SourceType,
) -> bool {
    if trait_arg_types.len() != impl_arg_types.len() {
        return false;
    }

    for (trait_tuple_subtype, impl_tuple_subtype) in
        trait_arg_types.iter().zip(impl_arg_types.iter())
    {
        if !trait_and_impl_arg_ty_compatible(
            sa,
            trait_tuple_subtype,
            trait_type_params.clone(),
            trait_alias_map,
            impl_tuple_subtype,
            self_ty.clone(),
        ) {
            return false;
        }
    }

    true
}

pub fn connect_aliases_to_trait(sa: &Sema) {
    for (_id, impl_) in sa.impls.iter() {
        if let Some(trait_ty) = impl_.trait_ty() {
            let trait_ = sa.trait_(trait_ty.trait_id);
            connect_aliases_to_trait_inner(sa, impl_, trait_);
        }
    }
}

fn connect_aliases_to_trait_inner(sa: &Sema, impl_: &ImplDefinition, trait_: &TraitDefinition) {
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

            remaining_aliases.remove(&trait_alias_id);
        } else {
            sa.report(
                impl_.file_id,
                impl_alias.node.span,
                ErrorMessage::ElementNotInTrait,
            )
        }
    }

    for remaining_alias_id in remaining_aliases {
        let remaining_alias = sa.alias(remaining_alias_id);
        let name = sa.interner.str(remaining_alias.name).to_string();
        sa.report(
            impl_.file_id,
            impl_.span(),
            ErrorMessage::MissingAssocType(name),
        );
    }

    assert!(impl_.trait_alias_map.set(trait_alias_map).is_ok());
}

pub fn check_type_aliases_bounds(sa: &Sema) {
    for (_id, impl_) in sa.impls.iter() {
        if let Some(trait_ty) = impl_.trait_ty() {
            let trait_ = sa.trait_(trait_ty.trait_id);
            check_type_aliases_bounds_inner(sa, impl_, trait_);
        }
    }
}

fn check_type_aliases_bounds_inner(sa: &Sema, impl_: &ImplDefinition, trait_: &TraitDefinition) {
    for &trait_alias_id in trait_.aliases() {
        if let Some(&impl_alias_id) = impl_.trait_alias_map().get(&trait_alias_id) {
            let trait_alias = sa.alias(trait_alias_id);
            let impl_alias = sa.alias(impl_alias_id);

            for bound in trait_alias.bounds() {
                if let Some(trait_ty) = bound.ty() {
                    if !implements_trait(
                        sa,
                        impl_alias.ty(),
                        impl_,
                        impl_.type_param_definition(),
                        trait_ty.clone(),
                    ) {
                        let name = impl_alias
                            .ty()
                            .name_with_type_params(sa, impl_.type_param_definition());
                        let trait_name =
                            trait_ty.name_with_type_params(sa, trait_.type_param_definition());
                        let msg = ErrorMessage::TypeNotImplementingTrait(name, trait_name);
                        sa.report(impl_.file_id, impl_alias.node.span, msg);
                    }
                }
            }
        }
    }
}

pub fn check_super_traits(sa: &Sema) {
    for (_id, impl_) in sa.impls.iter() {
        if let Some(trait_ty) = impl_.trait_ty() {
            check_super_traits_for_bound(sa, impl_, trait_ty);
        }
    }
}

fn check_super_traits_for_bound(sa: &Sema, impl_: &ImplDefinition, trait_ty: TraitType) {
    let trait_ = sa.trait_(trait_ty.trait_id);
    let type_param_definition = trait_.type_param_definition();

    for bound in type_param_definition.bounds_for_self() {
        if implements_trait(
            sa,
            impl_.extended_ty(),
            impl_,
            impl_.type_param_definition(),
            bound.clone(),
        ) {
            check_super_traits_for_bound(sa, impl_, bound);
        } else {
            let name = impl_
                .extended_ty()
                .name_with_type_params(sa, impl_.type_param_definition());

            let bound_name = bound.name_with_type_params(sa, trait_.type_param_definition());
            let msg = ErrorMessage::TypeNotImplementingTrait(name, bound_name);
            sa.report(impl_.file_id, impl_.parsed_trait_ty().span(), msg);

            impl_.parsed_trait_ty().set_ty(None);
        }
    }
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
            struct A(Int32)
            impl Foo for A {}
        ");
        ok("
            trait Foo {}
            struct A[T](Int32)
            impl Foo for A[Int32] {}
            impl Foo for A[Float32] {}
        ");
        ok("
            trait Foo {}
            struct A[T](Int32)
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
            class Foo { x: Int64 }
            impl MyEquals[Foo] for Foo {
                fn equals(val: Foo): Bool {
                    self.x == val.x
                }
            }
        ");
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
                fn next(): Option[Self::X];
            }
            class CX
            impl MyTrait for CX {
                type X = Int64;
                fn next(): Option[Self::X] {
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
                fn next(): Option[Self::X];
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
                fn next(): Option[Self::X];
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
                fn f(a: A, x: Self::X);
            }
            class CX
            impl MyTrait for CX {
                type X = Int64;
                fn f(a: Int64, x: Self::X) {}
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
        ");
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
            ErrorMessage::UnknownMethod("(Int64, String) -> Int64".into(), "foo".into()),
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
            ErrorMessage::UnknownMethod("(Int64, Int64) -> Bool".into(), "foo".into()),
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

    #[test]
    fn impl_self_in_extended_ty() {
        err(
            "
            trait Foo {}
            struct Bar[T](T)
            impl Foo for Bar[Self] {}
        ",
            (4, 30),
            ErrorMessage::SelfTypeUnavailable,
        );
    }

    #[test]
    fn impl_self_in_trait_ty() {
        ok("
            trait Foo[T] {}
            struct Bar[T](T)
            impl Foo[Self] for Bar[Int64] {}
        ");
    }

    #[test]
    #[ignore]
    fn impl_of_generic_trait() {
        ok("
            trait Foo[T] {}
            impl[T] Foo[T] for Int64 {}
            fn f(x: Int64): Foo[String] { x as Foo[String] }
        ");
    }

    #[test]
    fn impl_generic_extended_ty() {
        ok("
            trait Foo[T] { fn get(): T; }
            impl[T] Foo[T] for T {
                fn get(): T { self }
            }
            fn f(x: Int64): Foo[Int64] { x as Foo[Int64] }
        ");
    }

    #[test]
    fn impl_generic_extended_ty_with_trait_bound() {
        err(
            "
            trait Foo[T] { fn get(): T; }
            trait Bar {}
            impl[T] Foo[T] for T where T: Bar {
                fn get(): T { self }
            }
            fn f(x: Int64): Foo[Int64] { x as Foo[Int64] }
        ",
            (7, 42),
            ErrorMessage::TypeNotImplementingTrait("Int64".into(), "Foo[Int64]".into()),
        );
    }

    #[test]
    fn impl_reuse_trait_implementation() {
        ok("
            trait Foo {
                fn f(): Int64;
                fn g(): Int64 { 0 }
            }

            impl Foo for String {
                fn f(): Int64 { 1 }
            }
        ");
    }

    #[test]
    fn impl_reuse_trait_implementation_with_call() {
        ok("
            trait Foo {
                fn f(): Int64;
                fn g(): Int64 { 0 }
            }

            impl Foo for String {
                fn f(): Int64 { 1 }
            }

            fn x(a: String) {
                a.g();
            }
        ");
    }

    #[test]
    fn impl_reuse_trait_implementation_with_self_call() {
        ok("
            trait Foo {
                fn f(): Int64;
                fn g(): Int64 { self.f() }
            }

            impl Foo for String {
                fn f(): Int64 { 1 }
            }

            fn x(a: String) {
                a.f();
            }
        ");
    }

    #[test]
    #[ignore]
    fn impl_same_trait_twice_assoc_types() {
        err(
            "
            trait Foo {
                type X;
            }

            impl Foo for Int { type X = Bool; }
            impl Foo for Int { type X = Float32; }
        ",
            (1, 1),
            ErrorMessage::Unimplemented,
        );
    }

    #[test]
    fn impl_same_generic_traits_twice_with_different_type_argument() {
        ok("
            trait Foo[T] {}

            impl Foo[Bool] for Int {}
            impl Foo[Float32] for Int {}
        ");
    }

    #[test]
    #[ignore]
    fn impl_same_generic_traits_twice_with_same_type_argument() {
        ok("
            trait Foo[T] {}

            impl Foo[Bool] for Int {}
            impl Foo[Bool] for Int {}
        ");
    }

    #[test]
    fn impl_missing_assoc_type() {
        err(
            "
            trait Foo {
                type X;
                fn bar(): Self::X;
            }
            impl Foo for Int {
                fn bar(): Int { 0 }
            }
        ",
            (6, 13),
            ErrorMessage::MissingAssocType("X".into()),
        );
    }

    #[test]
    fn impl_error_in_trait_type() {
        err(
            "
            trait Foo {
                fn bar(): Unknown;
            }
            impl Foo for Int {
                fn bar(): Int { 0 }
            }
        ",
            (3, 27),
            ErrorMessage::UnknownIdentifier("Unknown".into()),
        );
    }

    #[test]
    fn impl_error_in_impl_type() {
        err(
            "
            trait Foo {
                fn bar(): Int;
            }
            impl Foo for Int {
                fn bar(): Unknown { 0 }
            }
        ",
            (6, 27),
            ErrorMessage::UnknownIdentifier("Unknown".into()),
        );
    }
}
