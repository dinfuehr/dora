use dora_bytecode::{
    BytecodeTraitType, BytecodeType, BytecodeTypeArray, Program, TraitId, TypeParamData,
};

use crate::find_impl_in_program;

pub fn specialize_bty_array(
    types: &BytecodeTypeArray,
    type_params: &BytecodeTypeArray,
) -> BytecodeTypeArray {
    let types = types
        .iter()
        .map(|p| specialize_bty(p, type_params))
        .collect();
    BytecodeTypeArray::new(types)
}

pub fn specialize_bty(ty: BytecodeType, type_params: &BytecodeTypeArray) -> BytecodeType {
    match ty {
        BytecodeType::TypeParam(tpid) => type_params[tpid as usize].clone(),

        BytecodeType::Class(cls_id, params) => {
            let params = specialize_bty_array(&params, type_params);
            BytecodeType::Class(cls_id, params)
        }

        BytecodeType::TraitObject(trait_id, params, assoc_types) => {
            let params = specialize_bty_array(&params, type_params);
            let assoc_types = specialize_bty_array(&assoc_types, type_params);
            BytecodeType::TraitObject(trait_id, params, assoc_types)
        }

        BytecodeType::Struct(struct_id, params) => {
            let params = specialize_bty_array(&params, type_params);
            BytecodeType::Struct(struct_id, params)
        }

        BytecodeType::Enum(enum_id, params) => {
            let params = specialize_bty_array(&params, type_params);
            BytecodeType::Enum(enum_id, params)
        }

        BytecodeType::Lambda(params, return_type, is_variadic) => {
            let params = specialize_bty_array(&params, type_params);
            let return_type = specialize_bty(return_type.as_ref().clone(), type_params);
            BytecodeType::Lambda(params, Box::new(return_type), is_variadic)
        }

        BytecodeType::Tuple(subtypes) => {
            let subtypes = specialize_bty_array(&subtypes, type_params);
            BytecodeType::Tuple(subtypes)
        }

        BytecodeType::TypeAlias(..) | BytecodeType::Assoc { .. } | BytecodeType::This => {
            unreachable!()
        }

        BytecodeType::Unit
        | BytecodeType::UInt8
        | BytecodeType::Bool
        | BytecodeType::Char
        | BytecodeType::Int32
        | BytecodeType::Int64
        | BytecodeType::Float32
        | BytecodeType::Float64
        | BytecodeType::Ptr
        | BytecodeType::Address => ty,

        BytecodeType::Ref(inner) => BytecodeType::Ref(Box::new(specialize_bty(
            inner.as_ref().clone(),
            type_params,
        ))),
    }
}

pub fn specialize_ty_array_in_program(
    program: &Program,
    types: &BytecodeTypeArray,
    type_params: &BytecodeTypeArray,
) -> BytecodeTypeArray {
    BytecodeTypeArray::new(
        types
            .iter()
            .map(|ty| specialize_ty_in_program(program, ty, type_params))
            .collect(),
    )
}

pub fn specialize_trait_ty_in_program(
    program: &Program,
    trait_ty: &BytecodeTraitType,
    type_params: &BytecodeTypeArray,
) -> BytecodeTraitType {
    BytecodeTraitType {
        trait_id: trait_ty.trait_id,
        type_params: specialize_ty_array_in_program(program, &trait_ty.type_params, type_params),
        bindings: trait_ty
            .bindings
            .iter()
            .map(|(alias_id, ty)| {
                (
                    *alias_id,
                    specialize_ty_in_program(program, ty.clone(), type_params),
                )
            })
            .collect(),
    }
}

pub fn specialize_ty_in_program(
    program: &Program,
    ty: BytecodeType,
    type_params: &BytecodeTypeArray,
) -> BytecodeType {
    match ty {
        BytecodeType::TypeParam(tpid) => type_params[tpid as usize].clone(),

        BytecodeType::Class(cls_id, params) => BytecodeType::Class(
            cls_id,
            specialize_ty_array_in_program(program, &params, type_params),
        ),

        BytecodeType::TraitObject(trait_id, params, assoc_types) => BytecodeType::TraitObject(
            trait_id,
            specialize_ty_array_in_program(program, &params, type_params),
            specialize_ty_array_in_program(program, &assoc_types, type_params),
        ),

        BytecodeType::Struct(struct_id, params) => BytecodeType::Struct(
            struct_id,
            specialize_ty_array_in_program(program, &params, type_params),
        ),

        BytecodeType::Enum(enum_id, params) => BytecodeType::Enum(
            enum_id,
            specialize_ty_array_in_program(program, &params, type_params),
        ),

        BytecodeType::Lambda(params, return_type, is_variadic) => BytecodeType::Lambda(
            specialize_ty_array_in_program(program, &params, type_params),
            Box::new(specialize_ty_in_program(
                program,
                return_type.as_ref().clone(),
                type_params,
            )),
            is_variadic,
        ),

        BytecodeType::Tuple(subtypes) => BytecodeType::Tuple(specialize_ty_array_in_program(
            program,
            &subtypes,
            type_params,
        )),

        BytecodeType::Assoc {
            ty,
            trait_ty,
            assoc_id,
        } => {
            let specialized_ty =
                specialize_ty_in_program(program, ty.as_ref().clone(), type_params);
            assert!(specialized_ty.is_concrete_type());
            let trait_ty = specialize_trait_ty_in_program(program, &trait_ty, type_params);
            let type_param_data = TypeParamData {
                names: Vec::new(),
                container_count: 0,
                container_bound_count: 0,
                bounds: Vec::new(),
            };

            let (impl_id, bindings) =
                find_impl_in_program(program, specialized_ty, &type_param_data, trait_ty)
                    .expect("no impl found for associated type");
            let impl_ = program.impl_(impl_id);
            let impl_alias_id = impl_
                .trait_alias_map
                .iter()
                .find(|(trait_alias_id, _)| *trait_alias_id == assoc_id)
                .map(|(_, impl_alias_id)| *impl_alias_id)
                .expect("missing associated type in impl");
            let impl_alias_ty = program
                .alias(impl_alias_id)
                .ty
                .as_ref()
                .expect("value expected")
                .clone();

            specialize_ty_in_program(program, impl_alias_ty, &bindings)
        }

        BytecodeType::This => panic!("unexpected Self in bytecode specialization"),

        BytecodeType::TypeAlias(..) => unreachable!(),

        BytecodeType::Unit
        | BytecodeType::UInt8
        | BytecodeType::Bool
        | BytecodeType::Char
        | BytecodeType::Int32
        | BytecodeType::Int64
        | BytecodeType::Float32
        | BytecodeType::Float64
        | BytecodeType::Ptr
        | BytecodeType::Address => ty,

        BytecodeType::Ref(inner) => BytecodeType::Ref(Box::new(specialize_ty_in_program(
            program,
            inner.as_ref().clone(),
            type_params,
        ))),
    }
}

pub fn specialize_bty_for_trait_object(
    program: &Program,
    ty: BytecodeType,
    trait_id: TraitId,
    type_params: &BytecodeTypeArray,
    assoc_types: &BytecodeTypeArray,
) -> BytecodeType {
    match ty {
        BytecodeType::TypeParam(tpid) => type_params[tpid as usize].clone(),

        BytecodeType::Class(cls_id, params) => {
            let params = specialize_bty_for_trait_object_array(
                program,
                &params,
                trait_id,
                type_params,
                assoc_types,
            );
            BytecodeType::Class(cls_id, params)
        }

        BytecodeType::TraitObject(trait_id, trait_params, trait_assoc_types) => {
            let trait_params = specialize_bty_for_trait_object_array(
                program,
                &trait_params,
                trait_id,
                type_params,
                assoc_types,
            );
            let trait_assoc_types = specialize_bty_for_trait_object_array(
                program,
                &trait_assoc_types,
                trait_id,
                type_params,
                assoc_types,
            );
            BytecodeType::TraitObject(trait_id, trait_params, trait_assoc_types)
        }

        BytecodeType::Struct(struct_id, params) => {
            let params = specialize_bty_for_trait_object_array(
                program,
                &params,
                trait_id,
                type_params,
                assoc_types,
            );
            BytecodeType::Struct(struct_id, params)
        }

        BytecodeType::Enum(enum_id, params) => {
            let params = specialize_bty_for_trait_object_array(
                program,
                &params,
                trait_id,
                type_params,
                assoc_types,
            );
            BytecodeType::Enum(enum_id, params)
        }

        BytecodeType::Lambda(params, return_type, is_variadic) => {
            let params = specialize_bty_for_trait_object_array(
                program,
                &params,
                trait_id,
                type_params,
                assoc_types,
            );
            let return_type = specialize_bty_for_trait_object(
                program,
                *return_type,
                trait_id,
                type_params,
                assoc_types,
            );
            BytecodeType::Lambda(params, Box::new(return_type), is_variadic)
        }

        BytecodeType::Tuple(subtypes) => {
            let subtypes = specialize_bty_for_trait_object_array(
                program,
                &subtypes,
                trait_id,
                type_params,
                assoc_types,
            );
            BytecodeType::Tuple(subtypes)
        }

        BytecodeType::Assoc { assoc_id, .. } => {
            let alias = program.alias(assoc_id);
            assoc_types[alias.idx_in_trait()].clone()
        }

        BytecodeType::TypeAlias(..) | BytecodeType::This => {
            unreachable!()
        }

        BytecodeType::Unit
        | BytecodeType::UInt8
        | BytecodeType::Bool
        | BytecodeType::Char
        | BytecodeType::Int32
        | BytecodeType::Int64
        | BytecodeType::Float32
        | BytecodeType::Float64
        | BytecodeType::Ptr
        | BytecodeType::Address => ty,

        BytecodeType::Ref(inner) => BytecodeType::Ref(Box::new(specialize_bty_for_trait_object(
            program,
            inner.as_ref().clone(),
            trait_id,
            type_params,
            assoc_types,
        ))),
    }
}

pub fn specialize_bty_for_trait_object_array(
    program: &Program,
    types: &BytecodeTypeArray,
    trait_id: TraitId,
    type_params: &BytecodeTypeArray,
    assoc_types: &BytecodeTypeArray,
) -> BytecodeTypeArray {
    let types = types
        .iter()
        .map(|p| specialize_bty_for_trait_object(program, p, trait_id, type_params, assoc_types))
        .collect();
    BytecodeTypeArray::new(types)
}
