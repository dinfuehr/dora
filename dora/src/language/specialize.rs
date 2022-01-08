use crate::language::ty::{SourceType, SourceTypeArray};
use crate::vm::{ensure_tuple, SemAnalysis};

pub fn specialize_type(
    sa: &SemAnalysis,
    ty: SourceType,
    type_params: &SourceTypeArray,
) -> SourceType {
    replace_type_param(sa, ty, type_params, None)
}

pub fn specialize_type_list(
    sa: &SemAnalysis,
    list: &SourceTypeArray,
    type_params: &SourceTypeArray,
) -> SourceTypeArray {
    let types = list.types();

    if types.is_empty() {
        return SourceTypeArray::empty();
    }

    let mut specialized_types = Vec::with_capacity(types.len());

    for ty in types {
        let ty = replace_type_param(sa, ty.clone(), type_params, None);
        specialized_types.push(ty);
    }

    SourceTypeArray::with(specialized_types)
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

        SourceType::Lambda(_) => unimplemented!(),

        SourceType::Tuple(tuple_id) => {
            let subtypes = {
                let tuples = sa.tuples.lock();
                let tuple = tuples.get_tuple(tuple_id);

                if tuple.is_concrete_type() {
                    return ty;
                }

                tuple.args()
            };

            let new_subtypes = subtypes
                .iter()
                .map(|t| replace_type_param(sa, t.clone(), type_params, self_ty.clone()))
                .collect::<Vec<_>>();

            let tuple_id = ensure_tuple(sa, new_subtypes);
            SourceType::Tuple(tuple_id)
        }

        SourceType::Unit
        | SourceType::UInt8
        | SourceType::Bool
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Module(_)
        | SourceType::Error => ty,

        SourceType::Any | SourceType::Ptr => {
            panic!("unexpected type = {:?}", ty);
            // unreachable!()
        }
    }
}
