use fixedbitset::FixedBitSet;

use crate::sema::{parent_element_or_self, AliasDefinition, AliasParent, Element};
use crate::{ErrorMessage, Sema, SourceType, SourceTypeArray};

pub fn detect_cycles(sa: &Sema) {
    let mut visited = FixedBitSet::with_capacity(sa.aliases.len());

    for (_id, alias) in sa.aliases.iter() {
        let mut visiting = FixedBitSet::with_capacity(sa.aliases.len());
        if let AliasParent::Impl(impl_id) = alias.parent {
            let impl_ = sa.impl_(impl_id);
            if let Some(trait_ty) = impl_.trait_ty() {
                let trait_ = sa.trait_(trait_ty.trait_id);
                if let Some(trait_alias_id) = trait_.alias_names().get(&alias.name) {
                    let trait_alias = sa.alias(*trait_alias_id);
                    detect_cycles_for_alias(sa, &mut visited, &mut visiting, impl_, trait_alias);
                }
            }
        } else {
            let context = parent_element_or_self(sa, alias);
            detect_cycles_for_alias(sa, &mut visited, &mut visiting, context, alias);
        }
        assert!(visiting.is_clear());
    }
}

fn detect_cycles_for_alias<'a>(
    sa: &'a Sema,
    visited: &mut FixedBitSet,
    visiting: &mut FixedBitSet,
    context: &'a dyn Element,
    mut alias: &'a AliasDefinition,
) -> bool {
    assert!(!alias.parent.is_impl());

    if alias.parent.is_trait() {
        if let Some(impl_) = context.to_impl() {
            if let Some(impl_alias_id) = impl_.trait_alias_map().get(&alias.id()) {
                alias = sa.alias(*impl_alias_id);
            } else {
                return false;
            }
        }
    }

    if visited.contains(alias.id().index()) {
        return false;
    }

    if visiting.contains(alias.id().index()) {
        sa.report(alias.file_id, alias.node.span, ErrorMessage::AliasCycle);
        return true;
    }

    visiting.insert(alias.id().index());

    if let Some(parsed_ty) = alias.parsed_ty() {
        let ty = expand_type(sa, parsed_ty.ty(), visited, visiting, context);
        parsed_ty.set_ty(ty);
    }
    visiting.remove(alias.id().index());
    visited.insert(alias.id().index());

    false
}

fn expand_type(
    sa: &Sema,
    ty: SourceType,
    visited: &mut FixedBitSet,
    visiting: &mut FixedBitSet,
    context: &dyn Element,
) -> SourceType {
    match &ty {
        SourceType::Class(cls_id, type_params) => SourceType::Class(
            *cls_id,
            expand_sta(sa, type_params, visited, visiting, context),
        ),

        SourceType::TraitObject(trait_id, type_params, bindings) => SourceType::TraitObject(
            *trait_id,
            expand_sta(sa, type_params, visited, visiting, context),
            expand_sta(sa, bindings, visited, visiting, context),
        ),

        SourceType::Struct(struct_id, type_params) => SourceType::Struct(
            *struct_id,
            expand_sta(sa, type_params, visited, visiting, context),
        ),

        SourceType::Enum(enum_id, type_params) => SourceType::Enum(
            *enum_id,
            expand_sta(sa, type_params, visited, visiting, context),
        ),

        SourceType::Lambda(params, return_type) => SourceType::Lambda(
            expand_sta(sa, params, visited, visiting, context),
            Box::new(expand_type(
                sa,
                return_type.as_ref().clone(),
                visited,
                visiting,
                context,
            )),
        ),

        SourceType::Tuple(subtypes) => {
            SourceType::Tuple(expand_sta(sa, subtypes, visited, visiting, context))
        }

        SourceType::Alias(id, _type_params) => {
            let alias = sa.alias(*id);
            let found_cycle = detect_cycles_for_alias(sa, visited, visiting, context, alias);

            if found_cycle {
                SourceType::Error
            } else {
                ty
            }
        }

        SourceType::Unit
        | SourceType::UInt8
        | SourceType::Bool
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Error
        | SourceType::This
        | SourceType::TypeParam(..) => ty,

        SourceType::Assoc(..) | SourceType::Any | SourceType::Ptr => {
            panic!("unexpected type = {:?}", ty);
        }
    }
}

fn expand_sta(
    sa: &Sema,
    array: &SourceTypeArray,
    visited: &mut FixedBitSet,
    visiting: &mut FixedBitSet,
    context: &dyn Element,
) -> SourceTypeArray {
    let new_array = array
        .iter()
        .map(|ty| expand_type(sa, ty, visited, visiting, context))
        .collect::<Vec<_>>();
    SourceTypeArray::with(new_array)
}

#[cfg(test)]
mod tests {
    use crate::error::msg::ErrorMessage;
    use crate::tests::*;

    #[test]
    fn type_alias() {
        ok("type Foo = Int64;");
        err("type Foo;", (1, 1), ErrorMessage::TypeAliasMissingType);
    }

    #[test]
    fn use_type_alias() {
        ok("
            type Foo = Int64;
            fn foo(x: Foo) {}
        ");
    }

    #[test]
    fn type_alias_in_impl() {
        ok("
            class Foo
            trait Bar { type X; }
            impl Bar for Foo {
                type X = Int64;
            }
        ");

        err(
            "
            class Foo
            trait Bar { type X; }
            impl Bar for Foo {
                type X;
            }
        ",
            (5, 17),
            ErrorMessage::TypeAliasMissingType,
        );
    }

    #[test]
    fn type_alias_in_trait() {
        ok("trait Foo { type Bar; }");
        err(
            "
                trait Foo { type Bar = Int64; }
            ",
            (2, 40),
            ErrorMessage::UnexpectedTypeAliasAssignment,
        );
    }

    #[test]
    fn use_alias_type_in_trait() {
        ok("
            trait Foo {
                type Bar;
                fn f(): Self::Bar;
            }
        ")
    }

    #[test]
    fn alias_using_other_alias() {
        ok("
            type A = B;
            type B = C;
            type C = Int64;

            fn f(a: A) {}
            fn g() { f(12); }
        ");
    }

    #[test]
    fn alias_cycle() {
        err(
            "
            type A = B;
            type B = C;
            type C = A;
        ",
            (2, 13),
            ErrorMessage::AliasCycle,
        );
    }

    #[test]
    fn regular_alias_with_type_bounds() {
        err(
            "
            trait Foo {}
            type A: Foo = Int64;
        ",
            (3, 13),
            ErrorMessage::UnexpectedTypeBounds,
        );
    }

    #[test]
    fn alias_in_trait_with_type_bounds() {
        ok("
            trait Foo {}
            trait Bar {
                type Ty: Foo;
            }
        ");
    }

    #[test]
    fn alias_in_impl_with_type_bounds() {
        err(
            "
            trait Foo {
                type Ty;
            }
            impl Foo for Int64 {
                type Ty: Foo = Int64;
            }
        ",
            (6, 17),
            ErrorMessage::UnexpectedTypeBounds,
        );
    }

    #[test]
    fn use_alias_type_bound_in_trait() {
        ok("
            trait Foo {
                type Ty: Bar;
            }
            trait Bar {}
        ");

        err(
            "
            trait Foo {
                type Ty: Bar;
            }
        ",
            (3, 26),
            ErrorMessage::UnknownIdentifier("Bar".into()),
        );
    }

    #[test]
    fn use_alias_trait_in_impl_trait_ty() {
        err(
            "
            trait Foo {}
            type FooB = Foo;
            impl FooB for Int64 {}
        ",
            (4, 18),
            ErrorMessage::BoundExpected,
        );
    }

    #[test]
    fn use_alias_trait_as_type_param_bound() {
        err(
            "
            trait Bar {}
            type BarA = Bar;
            fn f[T: BarA](x: T) {}
        ",
            (4, 21),
            ErrorMessage::BoundExpected,
        );
    }

    #[test]
    fn use_alias_trait_as_bound_for_associated_type() {
        err(
            "
            trait Foo {
                type X: BarA;
            }
            trait Bar {}
            type BarA = Bar;
        ",
            (3, 25),
            ErrorMessage::BoundExpected,
        );
    }

    #[test]
    fn use_alias_in_impl_extended_type() {
        ok("
            trait Foo {}
            type Foo1 = Foo;
            type Foo2 = Foo1;
            type FooA = Foo;
            type FooB = FooA;

            type Baz = Int64;
            type Baz1 = Baz;
            type Baz2 = Baz1;
            type BazA = Baz;
            type BazB = BazA;

            impl Foo for Baz2 {}

            struct Bar[T: Foo](value: T)

            fn f(x: Bar[BazB]) {}
        ")
    }

    #[test]
    fn impl_alias_self_referring() {
        err(
            "
            trait Foo { type X; }
            impl Foo for String {
                type X = Self::X;
            }
        ",
            (4, 17),
            ErrorMessage::AliasCycle,
        );
    }

    #[test]
    fn impl_alias_cycle() {
        err(
            "
            trait Foo { type X; type Y; }
            impl Foo for String {
                type X = Self::Y;
                type Y = Self::X;
            }
        ",
            (4, 17),
            ErrorMessage::AliasCycle,
        );
    }

    #[test]
    fn alias_with_type_params() {
        ok("
            struct Foo[T](value: T)
            type Bar[T] = Foo[T];
        ")
    }

    #[test]
    fn alias_with_type_params_missing_bound() {
        err(
            "
            trait TraitA {}
            struct Foo[T: TraitA](value: T)
            type Bar[T] = Foo[T];
        ",
            (4, 27),
            ErrorMessage::TypeNotImplementingTrait("T".into(), "TraitA".into()),
        );
    }

    #[test]
    fn alias_with_type_params_and_bound() {
        ok("
            trait TraitA {}
            struct Foo[T](value: T)
            type Bar[T: TraitA] = Foo[T];
        ");

        ok("
            trait TraitA {}
            struct Foo[T: TraitA](value: T)
            type Bar[T] where T: TraitA = Foo[T];
        ");

        ok("
            trait TraitA {}
            struct Foo[T](value: T)
            type Bar[T] where T: TraitA = Foo[T];
        ");
    }

    #[test]
    fn alias_with_type_params_and_where_in_wrong_position() {
        err(
            "
            trait TraitA {}
            struct Foo[T](value: T)
            type Bar[T] = Foo[T] where T: TraitA;
        ",
            (4, 34),
            ErrorMessage::UnexpectedWhere,
        );
    }

    #[test]
    fn trait_alias_with_where_in_wrong_position() {
        errors(
            "
            trait TraitA {}
            struct Foo[T](value: T)
            trait TraitB {
                type Bar[T] where T: TraitA = usize;
            }
        ",
            &[
                ((5, 47), ErrorMessage::UnexpectedTypeAliasAssignment),
                ((5, 29), ErrorMessage::UnexpectedWhere),
            ],
        );
    }

    #[test]
    fn impl_alias_with_where_in_wrong_position() {
        err(
            "
            trait TraitA {}
            struct Foo[T](value: T)
            trait TraitB {
                type Bar[T] where T: TraitA;
            }
            impl TraitB for Int64 {
                type Bar[T] where T: TraitA = String where T: TraitA;
            }
        ",
            (8, 29),
            ErrorMessage::UnexpectedWhere,
        );
    }

    #[test]
    fn alias_generic() {
        ok("
            struct Foo[T1, T2]
            type X[T1, T2, T3, T4] = Y[T2, T3, T4];
            type Y[T1, T2, T3] = Z[T2, T3];
            type Z[T1, T2] = Foo[T2, T1];
            fn f() {
                g(Foo[Int64, Float64]())
            }
            fn g(foo: X[String, Bool, Float64, Int64]) {}
        ");
    }

    #[test]
    #[ignore]
    fn impl_alias_as_return_type() {
        ok("
            struct Foo[T]
            trait TraitA {
                type X;
            }
            impl TraitA for String {
                type X = Int64;
            }
            fn x() {
                let x: Int64 = f[String](\"foo\");
            }
            fn f[T: TraitA](t: T): T::X {
                0
            }
        ");
    }
}
