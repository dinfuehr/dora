use crate::sema::{create_tuple, AliasParent, Sema};
use crate::{SourceType, SourceTypeArray};

pub fn specialize_type(sa: &Sema, ty: SourceType, type_params: &SourceTypeArray) -> SourceType {
    replace_type(sa, ty, Some(type_params), None, AliasReplacement::None)
}

#[derive(Clone)]
pub enum AliasReplacement {
    None,
    ReplaceWithActualType,
}

pub fn replace_type(
    sa: &Sema,
    ty: SourceType,
    type_params: Option<&SourceTypeArray>,
    self_ty: Option<SourceType>,
    alias_map: AliasReplacement,
) -> SourceType {
    match ty {
        SourceType::TypeParam(tpid) => {
            if let Some(type_params) = type_params {
                type_params[tpid.to_usize()].clone()
            } else {
                ty
            }
        }
        SourceType::Class(cls_id, params) => {
            let params = SourceTypeArray::with(
                params
                    .iter()
                    .map(|p| replace_type(sa, p, type_params, self_ty.clone(), alias_map.clone()))
                    .collect::<Vec<_>>(),
            );

            SourceType::Class(cls_id, params)
        }

        SourceType::Trait(trait_id, old_type_params) => {
            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| replace_type(sa, p, type_params, self_ty.clone(), alias_map.clone()))
                    .collect::<Vec<_>>(),
            );

            SourceType::Trait(trait_id, new_type_params)
        }

        SourceType::Struct(struct_id, old_type_params) => {
            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| replace_type(sa, p, type_params, self_ty.clone(), alias_map.clone()))
                    .collect::<Vec<_>>(),
            );

            SourceType::Struct(struct_id, new_type_params)
        }

        SourceType::Enum(enum_id, old_type_params) => {
            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| replace_type(sa, p, type_params, self_ty.clone(), alias_map.clone()))
                    .collect::<Vec<_>>(),
            );

            SourceType::Enum(enum_id, new_type_params)
        }

        SourceType::This => {
            if let Some(self_ty) = self_ty {
                self_ty
            } else {
                ty
            }
        }

        SourceType::Lambda(params, return_type) => {
            let new_params = SourceTypeArray::with(
                params
                    .iter()
                    .map(|p| replace_type(sa, p, type_params, self_ty.clone(), alias_map.clone()))
                    .collect::<Vec<_>>(),
            );

            let return_type = replace_type(
                sa,
                return_type.as_ref().clone(),
                type_params,
                self_ty.clone(),
                alias_map,
            );

            SourceType::Lambda(new_params, Box::new(return_type))
        }

        SourceType::Tuple(subtypes) => {
            let new_subtypes = subtypes
                .iter()
                .map(|t| {
                    replace_type(
                        sa,
                        t.clone(),
                        type_params,
                        self_ty.clone(),
                        alias_map.clone(),
                    )
                })
                .collect::<Vec<_>>();

            create_tuple(sa, new_subtypes)
        }

        SourceType::TypeAlias(id) => match alias_map {
            AliasReplacement::None => ty,
            AliasReplacement::ReplaceWithActualType => {
                let alias = sa.alias(id);
                if let AliasParent::Trait(..) = alias.parent {
                    ty
                } else {
                    alias.ty()
                }
            }
        },

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
            // unreachable!()
        }
    }
}
