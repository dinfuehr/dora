use crate::compiler::SpecializeSelf;
use crate::vm::{VM, find_impl_in_program};
use dora_bytecode::{
    BytecodeTraitType, BytecodeType, BytecodeTypeArray, Program, TraitId, TypeParamData,
};

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

        BytecodeType::Lambda(params, return_type) => {
            let params = specialize_bty_array(&params, type_params);
            let return_type = specialize_bty(return_type.as_ref().clone(), type_params);
            BytecodeType::Lambda(params, Box::new(return_type))
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
    self_data: Option<&SpecializeSelf>,
    types: &BytecodeTypeArray,
    type_params: &BytecodeTypeArray,
) -> BytecodeTypeArray {
    let types = types
        .iter()
        .map(|p| specialize_ty_in_program(program, self_data, p, type_params))
        .collect();
    BytecodeTypeArray::new(types)
}

pub fn specialize_trait_ty_in_program(
    program: &Program,
    self_data: Option<&SpecializeSelf>,
    trait_ty: &BytecodeTraitType,
    type_params: &BytecodeTypeArray,
) -> BytecodeTraitType {
    BytecodeTraitType {
        trait_id: trait_ty.trait_id,
        type_params: specialize_ty_array_in_program(
            program,
            self_data,
            &trait_ty.type_params,
            type_params,
        ),
        bindings: trait_ty
            .bindings
            .iter()
            .map(|(alias_id, ty)| {
                (
                    *alias_id,
                    specialize_ty_in_program(program, self_data, ty.clone(), type_params),
                )
            })
            .collect(),
    }
}

pub fn specialize_ty(
    vm: &VM,
    self_data: Option<&SpecializeSelf>,
    ty: BytecodeType,
    type_params: &BytecodeTypeArray,
) -> BytecodeType {
    specialize_ty_in_program(&vm.program, self_data, ty, type_params)
}

pub fn specialize_ty_in_program(
    program: &Program,
    self_data: Option<&SpecializeSelf>,
    ty: BytecodeType,
    type_params: &BytecodeTypeArray,
) -> BytecodeType {
    match ty {
        BytecodeType::TypeParam(tpid) => {
            let tpid = tpid as usize;

            if let Some(self_data) = self_data {
                let trait_params_count = self_data.trait_ty.type_params.len();
                if tpid < trait_params_count {
                    let ty = self_data.trait_ty.type_params[tpid].clone();
                    specialize_ty_in_program(program, None, ty, type_params)
                } else {
                    let converted_id = self_data.container_type_params + tpid - trait_params_count;
                    type_params[converted_id].clone()
                }
            } else {
                type_params[tpid].clone()
            }
        }

        BytecodeType::Class(cls_id, params) => {
            let params = specialize_ty_array_in_program(program, self_data, &params, type_params);
            BytecodeType::Class(cls_id, params)
        }

        BytecodeType::TraitObject(trait_id, params, assoc_types) => {
            let params = specialize_ty_array_in_program(program, self_data, &params, type_params);
            let assoc_types =
                specialize_ty_array_in_program(program, self_data, &assoc_types, type_params);
            BytecodeType::TraitObject(trait_id, params, assoc_types)
        }

        BytecodeType::Struct(struct_id, params) => {
            let params = specialize_ty_array_in_program(program, self_data, &params, type_params);
            BytecodeType::Struct(struct_id, params)
        }

        BytecodeType::Enum(enum_id, params) => {
            let params = specialize_ty_array_in_program(program, self_data, &params, type_params);
            BytecodeType::Enum(enum_id, params)
        }

        BytecodeType::Lambda(params, return_type) => {
            let params = specialize_ty_array_in_program(program, self_data, &params, type_params);
            let return_type = specialize_ty_in_program(
                program,
                self_data,
                return_type.as_ref().clone(),
                type_params,
            );
            BytecodeType::Lambda(params, Box::new(return_type))
        }

        BytecodeType::Tuple(subtypes) => {
            let subtypes =
                specialize_ty_array_in_program(program, self_data, &subtypes, type_params);
            BytecodeType::Tuple(subtypes)
        }

        BytecodeType::Assoc {
            ty,
            trait_ty,
            assoc_id,
        } => {
            if ty.as_ref() == &BytecodeType::This {
                // This case handles Self::Item - used in default trait method implementations.
                let specialize_self = self_data.expect("unexpected associated item on Self.");

                // Here we only end up if default trait method implementation is used from
                // impl-method. While bytecode is used from the trait method, the type parameters
                // are still applied to the impl-method. So extended_ty can be specialized using
                // the ordinary type parameters.
                let extended_ty = specialize_ty_in_program(
                    program,
                    None,
                    specialize_self.extended_ty.clone(),
                    type_params,
                );

                assert!(extended_ty.is_concrete_type());

                let impl_ = program.impl_(specialize_self.impl_id);

                // First try to find the associated type in the current impl
                let impl_alias_id = impl_
                    .trait_alias_map
                    .iter()
                    .filter(|(trait_alias_id, _)| *trait_alias_id == assoc_id)
                    .map(|(_, impl_alias_id)| impl_alias_id)
                    .next()
                    .cloned();

                if let Some(impl_alias_id) = impl_alias_id {
                    let impl_alias = program.alias(impl_alias_id);
                    let impl_alias_ty = impl_alias.ty.as_ref().expect("value expected").clone();
                    specialize_ty_in_program(program, None, impl_alias_ty, type_params)
                } else {
                    // Associated type is from a super trait, find the impl for that trait
                    let type_param_data = TypeParamData {
                        names: Vec::new(),
                        container_count: 0,
                        bounds: Vec::new(),
                    };

                    let trait_ty =
                        specialize_trait_ty_in_program(program, self_data, &trait_ty, type_params);

                    let (impl_id, bindings) = find_impl_in_program(
                        program,
                        extended_ty,
                        &type_param_data,
                        trait_ty.clone(),
                    )
                    .expect("no impl found for super trait associated type");

                    let found_impl = program.impl_(impl_id);

                    let impl_alias_id = found_impl
                        .trait_alias_map
                        .iter()
                        .filter(|(trait_alias_id, _)| *trait_alias_id == assoc_id)
                        .map(|(_, impl_alias_id)| impl_alias_id)
                        .next()
                        .cloned()
                        .expect("missing alias in super trait impl");

                    let impl_alias = program.alias(impl_alias_id);
                    let impl_alias_ty = impl_alias.ty.as_ref().expect("value expected").clone();

                    specialize_ty_in_program(program, None, impl_alias_ty, &bindings)
                }
            } else {
                // Specialize the inner type first
                let specialized_ty =
                    specialize_ty_in_program(program, self_data, ty.as_ref().clone(), type_params);
                assert!(specialized_ty.is_concrete_type());

                let type_param_data = TypeParamData {
                    names: Vec::new(),
                    container_count: 0,
                    bounds: Vec::new(),
                };

                let (impl_id, bindings) = find_impl_in_program(
                    program,
                    specialized_ty,
                    &type_param_data,
                    trait_ty.clone(),
                )
                .expect("no impl found for generic trait method call");

                let impl_ = program.impl_(impl_id);

                let impl_alias_id = impl_
                    .trait_alias_map
                    .iter()
                    .filter(|(trait_alias_id, _)| *trait_alias_id == assoc_id)
                    .map(|(_, impl_alias_id)| impl_alias_id)
                    .next()
                    .cloned()
                    .expect("missing");

                let impl_alias = program.alias(impl_alias_id);
                let impl_alias_ty = impl_alias.ty.as_ref().expect("value expected").clone();

                specialize_ty_in_program(program, None, impl_alias_ty, &bindings)
            }
        }

        BytecodeType::This => {
            let ty = self_data.expect("unexpected Self").extended_ty.clone();
            specialize_ty_in_program(program, None, ty, type_params)
        }

        BytecodeType::TypeAlias(..) => {
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

        BytecodeType::Ref(inner) => BytecodeType::Ref(Box::new(specialize_ty_in_program(
            program,
            self_data,
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

        BytecodeType::Lambda(params, return_type) => {
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
            BytecodeType::Lambda(params, Box::new(return_type))
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

        BytecodeType::Assoc { ty, assoc_id, .. } => {
            assert!(ty.as_ref() == &BytecodeType::This);
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
