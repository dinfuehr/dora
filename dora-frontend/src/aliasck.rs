use fixedbitset::FixedBitSet;

use crate::sema::{AliasDefinitionId, AliasParent};
use crate::{ErrorMessage, Sema, SourceType, SourceTypeArray};

pub fn detect_cycles(sa: &Sema) {
    let mut visited = FixedBitSet::with_capacity(sa.aliases.len());

    for (id, alias) in sa.aliases.iter() {
        match alias.parent {
            AliasParent::None | AliasParent::Impl(..) => {
                let mut visiting = FixedBitSet::with_capacity(sa.aliases.len());
                detect_cycles_for_alias(sa, &mut visited, &mut visiting, id);
                assert!(visiting.is_clear());
            }

            AliasParent::Trait(..) => {}
        }
    }
}

fn detect_cycles_for_alias(
    sa: &Sema,
    visited: &mut FixedBitSet,
    visiting: &mut FixedBitSet,
    id: AliasDefinitionId,
) -> bool {
    let alias = sa.alias(id);

    if visited.contains(id.index()) {
        return false;
    }

    if visiting.contains(id.index()) {
        sa.report(alias.file_id, alias.node.span, ErrorMessage::AliasCycle);
        return true;
    }

    visiting.insert(id.index());
    let ty = alias.parsed_ty().ty();
    let ty = expand_type(sa, visited, visiting, ty);
    visiting.remove(id.index());
    visited.insert(id.index());

    alias.parsed_ty().set_ty(ty.clone());
    false
}

fn expand_type(
    sa: &Sema,
    visited: &mut FixedBitSet,
    visiting: &mut FixedBitSet,
    ty: SourceType,
) -> SourceType {
    match ty {
        SourceType::Class(cls_id, params) => {
            let params = SourceTypeArray::with(
                params
                    .iter()
                    .map(|p| expand_type(sa, visited, visiting, p))
                    .collect::<Vec<_>>(),
            );

            SourceType::Class(cls_id, params)
        }

        SourceType::Trait(trait_id, old_type_params) => {
            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| expand_type(sa, visited, visiting, p))
                    .collect::<Vec<_>>(),
            );

            SourceType::Trait(trait_id, new_type_params)
        }

        SourceType::Struct(struct_id, old_type_params) => {
            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| expand_type(sa, visited, visiting, p))
                    .collect::<Vec<_>>(),
            );

            SourceType::Struct(struct_id, new_type_params)
        }

        SourceType::Enum(enum_id, old_type_params) => {
            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| expand_type(sa, visited, visiting, p))
                    .collect::<Vec<_>>(),
            );

            SourceType::Enum(enum_id, new_type_params)
        }

        SourceType::Lambda(params, return_type) => {
            let new_params = SourceTypeArray::with(
                params
                    .iter()
                    .map(|p| expand_type(sa, visited, visiting, p))
                    .collect::<Vec<_>>(),
            );

            let return_type = expand_type(sa, visited, visiting, return_type.as_ref().clone());

            SourceType::Lambda(new_params, Box::new(return_type))
        }

        SourceType::Tuple(subtypes) => {
            let new_subtypes = subtypes
                .iter()
                .map(|t| expand_type(sa, visited, visiting, t.clone()))
                .collect::<Vec<_>>();

            SourceType::Tuple(SourceTypeArray::with(new_subtypes))
        }

        SourceType::TypeAlias(id) => {
            let found_cycle = detect_cycles_for_alias(sa, visited, visiting, id);

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

        SourceType::Any | SourceType::Ptr => {
            panic!("unexpected type = {:?}", ty);
        }
    }
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
            "trait Foo { type Bar = Int64; }",
            (1, 13),
            ErrorMessage::UnexpectedTypeAliasAssignment,
        );
    }

    #[test]
    fn use_alias_type_in_trait() {
        ok("
            trait Foo {
                type Bar;
                fn f(): Bar;
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
}
