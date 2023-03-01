use crate::language::sem_analysis::{create_tuple, SemAnalysis};
use crate::language::ty::{SourceType, SourceTypeArray};

pub fn specialize_type(
    sa: &SemAnalysis,
    ty: SourceType,
    type_params: &SourceTypeArray,
) -> SourceType {
    replace_type_param(sa, ty, type_params, None)
}

pub fn replace_type_param(
    sa: &SemAnalysis,
    ty: SourceType,
    type_params: &SourceTypeArray,
    self_ty: Option<SourceType>,
) -> SourceType {
    match ty {
        SourceType::TypeParam(tpid) => type_params[tpid.to_usize()].clone(),

        SourceType::Class(cls_id, params) => {
            let params = SourceTypeArray::with(
                params
                    .iter()
                    .map(|p| replace_type_param(sa, p, type_params, self_ty.clone()))
                    .collect::<Vec<_>>(),
            );

            SourceType::Class(cls_id, params)
        }

        SourceType::Trait(trait_id, old_type_params) => {
            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| replace_type_param(sa, p, type_params, self_ty.clone()))
                    .collect::<Vec<_>>(),
            );

            SourceType::Trait(trait_id, new_type_params)
        }

        SourceType::Struct(struct_id, old_type_params) => {
            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| replace_type_param(sa, p, type_params, self_ty.clone()))
                    .collect::<Vec<_>>(),
            );

            SourceType::Struct(struct_id, new_type_params)
        }

        SourceType::Enum(enum_id, old_type_params) => {
            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| replace_type_param(sa, p, type_params, self_ty.clone()))
                    .collect::<Vec<_>>(),
            );

            SourceType::Enum(enum_id, new_type_params)
        }

        SourceType::This => self_ty.expect("no type for Self given"),

        SourceType::Lambda(params, return_type) => {
            let new_params = SourceTypeArray::with(
                params
                    .iter()
                    .map(|p| replace_type_param(sa, p, type_params, self_ty.clone()))
                    .collect::<Vec<_>>(),
            );

            let return_type = replace_type_param(
                sa,
                return_type.as_ref().clone(),
                type_params,
                self_ty.clone(),
            );

            SourceType::Lambda(new_params, Box::new(return_type))
        }

        SourceType::Tuple(subtypes) => {
            let new_subtypes = subtypes
                .iter()
                .map(|t| replace_type_param(sa, t.clone(), type_params, self_ty.clone()))
                .collect::<Vec<_>>();

            create_tuple(sa, new_subtypes)
        }

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
