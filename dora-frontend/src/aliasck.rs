use std::collections::{HashMap, HashSet};

use crate::sema::{AliasDefinitionId, AliasParent, TypeParamDefinition};
use crate::{
    parse_type_bound, parsety, ErrorMessage, ModuleSymTable, ParsedType, Sema, SourceType,
    SourceTypeArray, SymbolKind,
};

pub fn check(sa: &Sema) {
    let mut alias_types: HashMap<AliasDefinitionId, SourceType> = HashMap::new();

    for (id, alias) in sa.aliases.iter() {
        match alias.parent {
            AliasParent::None => {
                if let Some(ref ty_node) = alias.node.ty {
                    let table = ModuleSymTable::new(sa, alias.module_id);

                    let parsed_ty = parsety::parse_type(sa, &table, alias.file_id, ty_node);
                    let parsed_ty = ParsedType::new_ast(parsed_ty);
                    parsety::convert_parsed_type2(sa, &parsed_ty);

                    let ctxt = parsety::TypeContext {
                        allow_self: false,
                        module_id: alias.module_id,
                        file_id: alias.file_id,
                        type_param_defs: &TypeParamDefinition::new(),
                    };
                    parsety::check_parsed_type2(sa, &ctxt, &parsed_ty);

                    assert!(alias_types.insert(id, parsed_ty.ty()).is_none());
                } else {
                    assert!(alias_types.insert(id, SourceType::Error).is_none());
                }
            }

            AliasParent::Impl(impl_id) => {
                if let Some(ref ty_node) = alias.node.ty {
                    let impl_ = sa.impl_(impl_id);
                    let mut table = ModuleSymTable::new(sa, alias.module_id);
                    table.push_level();

                    for (id, name) in impl_.type_params().names() {
                        table.insert(name, SymbolKind::TypeParam(id));
                    }

                    let parsed_ty = parsety::parse_type(sa, &table, alias.file_id, ty_node);
                    let parsed_ty = ParsedType::new_ast(parsed_ty);
                    parsety::convert_parsed_type2(sa, &parsed_ty);

                    let ctxt = parsety::TypeContext {
                        allow_self: false,
                        module_id: alias.module_id,
                        file_id: alias.file_id,
                        type_param_defs: impl_.type_params(),
                    };
                    parsety::check_parsed_type2(sa, &ctxt, &parsed_ty);

                    table.pop_level();

                    assert!(alias_types.insert(id, parsed_ty.ty()).is_none());
                } else {
                    assert!(alias_types.insert(id, SourceType::Error).is_none());
                }
            }

            AliasParent::Trait(id) => {
                let trait_ = sa.trait_(id);

                let mut bounds = Vec::with_capacity(alias.node.bounds.len());
                let mut table = ModuleSymTable::new(sa, alias.module_id);
                table.push_level();

                for (id, name) in trait_.type_param_definition().names() {
                    table.insert(name, SymbolKind::TypeParam(id));
                }

                for bound in &alias.node.bounds {
                    let ty = parse_type_bound(sa, &table, alias.file_id, bound);
                    bounds.push(ty);
                }

                table.pop_level();
                assert!(alias.bounds.set(bounds).is_ok());
            }
        }
    }

    expand_aliases(sa, alias_types);
}

pub fn expand_aliases(sa: &Sema, alias_types: HashMap<AliasDefinitionId, SourceType>) {
    for (id, alias) in sa.aliases.iter() {
        match alias.parent {
            AliasParent::None | AliasParent::Impl(..) => {
                let mut visiting = HashSet::new();
                expand_alias(sa, &alias_types, &mut visiting, id);
                assert!(visiting.is_empty());
                assert!(alias.ty.get().is_some());
            }
            AliasParent::Trait(..) => {}
        }
    }
}

fn expand_alias(
    sa: &Sema,
    alias_types: &HashMap<AliasDefinitionId, SourceType>,
    visiting: &mut HashSet<AliasDefinitionId>,
    id: AliasDefinitionId,
) -> SourceType {
    let alias = sa.alias(id);

    if let Some(ty) = alias.ty.get() {
        return ty.clone();
    }

    if visiting.contains(&id) {
        sa.report(alias.file_id, alias.node.span, ErrorMessage::AliasCycle);
        return SourceType::Error;
    }

    assert!(visiting.insert(id));
    let ty = alias_types.get(&id).cloned().expect("missing type");
    let ty = expand_type(sa, alias_types, visiting, ty);
    assert!(visiting.remove(&id));

    assert!(alias.ty.set(ty.clone()).is_ok());

    ty
}

fn expand_type(
    sa: &Sema,
    alias_types: &HashMap<AliasDefinitionId, SourceType>,
    visiting: &mut HashSet<AliasDefinitionId>,
    ty: SourceType,
) -> SourceType {
    match ty {
        SourceType::TypeParam(..) => ty,
        SourceType::Class(cls_id, params) => {
            let params = SourceTypeArray::with(
                params
                    .iter()
                    .map(|p| expand_type(sa, alias_types, visiting, p))
                    .collect::<Vec<_>>(),
            );

            SourceType::Class(cls_id, params)
        }

        SourceType::Trait(trait_id, old_type_params) => {
            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| expand_type(sa, alias_types, visiting, p))
                    .collect::<Vec<_>>(),
            );

            SourceType::Trait(trait_id, new_type_params)
        }

        SourceType::Struct(struct_id, old_type_params) => {
            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| expand_type(sa, alias_types, visiting, p))
                    .collect::<Vec<_>>(),
            );

            SourceType::Struct(struct_id, new_type_params)
        }

        SourceType::Enum(enum_id, old_type_params) => {
            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| expand_type(sa, alias_types, visiting, p))
                    .collect::<Vec<_>>(),
            );

            SourceType::Enum(enum_id, new_type_params)
        }

        SourceType::This => ty,

        SourceType::Lambda(params, return_type) => {
            let new_params = SourceTypeArray::with(
                params
                    .iter()
                    .map(|p| expand_type(sa, alias_types, visiting, p))
                    .collect::<Vec<_>>(),
            );

            let return_type = expand_type(sa, alias_types, visiting, return_type.as_ref().clone());

            SourceType::Lambda(new_params, Box::new(return_type))
        }

        SourceType::Tuple(subtypes) => {
            let new_subtypes = subtypes
                .iter()
                .map(|t| expand_type(sa, alias_types, visiting, t.clone()))
                .collect::<Vec<_>>();

            SourceType::Tuple(SourceTypeArray::with(new_subtypes))
        }

        SourceType::TypeAlias(id) => expand_alias(sa, alias_types, visiting, id),

        SourceType::Unit
        | SourceType::UInt8
        | SourceType::Bool
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Error => ty,

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
