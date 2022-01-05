use crate::ty::{SourceType, SourceTypeArray};
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

        SourceType::Class(cls_id, list_id) => {
            let params = sa.source_type_arrays.lock().get(list_id);

            let params = SourceTypeArray::with(
                params
                    .iter()
                    .map(|p| replace_type_param(sa, p, type_params, self_ty.clone()))
                    .collect::<Vec<_>>(),
            );

            let list_id = sa.source_type_arrays.lock().insert(params);
            SourceType::Class(cls_id, list_id)
        }

        SourceType::Trait(trait_id, list_id) => {
            let old_type_params = sa.source_type_arrays.lock().get(list_id);

            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| replace_type_param(sa, p, type_params, self_ty.clone()))
                    .collect::<Vec<_>>(),
            );

            let new_type_params_id = sa.source_type_arrays.lock().insert(new_type_params);
            SourceType::Trait(trait_id, new_type_params_id)
        }

        SourceType::Struct(struct_id, list_id) => {
            let old_type_params = sa.source_type_arrays.lock().get(list_id);

            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| replace_type_param(sa, p, type_params, self_ty.clone()))
                    .collect::<Vec<_>>(),
            );

            let new_type_params_id = sa.source_type_arrays.lock().insert(new_type_params);
            SourceType::Struct(struct_id, new_type_params_id)
        }

        SourceType::Enum(enum_id, list_id) => {
            let old_type_params = sa.source_type_arrays.lock().get(list_id);

            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| replace_type_param(sa, p, type_params, self_ty.clone()))
                    .collect::<Vec<_>>(),
            );

            let new_type_params_id = sa.source_type_arrays.lock().insert(new_type_params);
            SourceType::Enum(enum_id, new_type_params_id)
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

pub fn replace_type_self(sa: &SemAnalysis, ty: SourceType, self_ty: SourceType) -> SourceType {
    match ty {
        SourceType::Class(cls_id, list_id) => {
            let params = sa.source_type_arrays.lock().get(list_id);

            let params = SourceTypeArray::with(
                params
                    .iter()
                    .map(|p| replace_type_self(sa, p, self_ty.clone()))
                    .collect::<Vec<_>>(),
            );

            let list_id = sa.source_type_arrays.lock().insert(params);
            SourceType::Class(cls_id, list_id)
        }

        SourceType::Trait(trait_id, list_id) => {
            let old_type_params = sa.source_type_arrays.lock().get(list_id);

            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| replace_type_self(sa, p, self_ty.clone()))
                    .collect::<Vec<_>>(),
            );

            let new_type_params_id = sa.source_type_arrays.lock().insert(new_type_params);
            SourceType::Trait(trait_id, new_type_params_id)
        }

        SourceType::Struct(struct_id, list_id) => {
            let old_type_params = sa.source_type_arrays.lock().get(list_id);

            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| replace_type_self(sa, p, self_ty.clone()))
                    .collect::<Vec<_>>(),
            );

            let new_type_params_id = sa.source_type_arrays.lock().insert(new_type_params);
            SourceType::Struct(struct_id, new_type_params_id)
        }

        SourceType::Enum(enum_id, list_id) => {
            let old_type_params = sa.source_type_arrays.lock().get(list_id);

            let new_type_params = SourceTypeArray::with(
                old_type_params
                    .iter()
                    .map(|p| replace_type_self(sa, p, self_ty.clone()))
                    .collect::<Vec<_>>(),
            );

            let new_type_params_id = sa.source_type_arrays.lock().insert(new_type_params);
            SourceType::Enum(enum_id, new_type_params_id)
        }

        SourceType::This => self_ty,

        SourceType::Lambda(_) => unimplemented!(),

        SourceType::Tuple(tuple_id) => {
            let subtypes = {
                let tuples = sa.tuples.lock();
                let tuple = tuples.get_tuple(tuple_id);

                tuple.args()
            };

            let new_subtypes = subtypes
                .iter()
                .map(|t| replace_type_self(sa, t.clone(), self_ty.clone()))
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
        | SourceType::TypeParam(_) => ty,

        SourceType::Any | SourceType::Error | SourceType::Ptr | SourceType::Module(_) => {
            unreachable!()
        }
    }
}
