use crate::vm::{bounds_for_tp, tp_implements_trait, ty_implements_trait, BytecodeTypeExt, VM};
use dora_bytecode::{BytecodeType, BytecodeTypeArray, TypeParamData};

pub fn block_matches_ty(
    vm: &VM,
    check_ty: BytecodeType,
    check_type_param_defs: &TypeParamData,
    block_ty: BytecodeType,
    block_type_param_defs: &TypeParamData,
) -> Option<BytecodeTypeArray> {
    let mut bindings = vec![None; block_type_param_defs.names.len()];

    let result = matches(
        vm,
        check_ty,
        check_type_param_defs,
        block_ty.clone(),
        block_type_param_defs,
        &mut bindings,
    );

    if result {
        Some(BytecodeTypeArray::new(
            bindings.into_iter().map(|t| t.unwrap()).collect(),
        ))
    } else {
        None
    }
}

fn matches(
    vm: &VM,
    check_ty: BytecodeType,
    check_type_param_defs: &TypeParamData,
    block_ty: BytecodeType,
    block_type_param_defs: &TypeParamData,
    bindings: &mut [Option<BytecodeType>],
) -> bool {
    if let BytecodeType::TypeParam(tp_id) = block_ty {
        let binding = bindings[tp_id as usize].clone();

        if let Some(binding) = binding {
            compare_concrete_types(
                vm,
                check_ty,
                check_type_param_defs,
                binding,
                block_type_param_defs,
                bindings,
            )
        } else {
            let result = if check_ty.is_type_param() {
                compare_type_param_bounds(
                    vm,
                    check_ty.clone(),
                    check_type_param_defs,
                    block_ty,
                    block_type_param_defs,
                )
            } else {
                concrete_type_fulfills_bounds(
                    vm,
                    check_ty.clone(),
                    check_type_param_defs,
                    block_ty,
                    block_type_param_defs,
                )
            };

            bindings[tp_id as usize] = Some(check_ty);

            result
        }
    } else {
        if check_ty.is_type_param() {
            false
        } else {
            compare_concrete_types(
                vm,
                check_ty,
                check_type_param_defs,
                block_ty,
                block_type_param_defs,
                bindings,
            )
        }
    }
}

fn compare_type_param_bounds(
    _vm: &VM,
    check_ty: BytecodeType,
    check_type_param_defs: &TypeParamData,
    block_ty: BytecodeType,
    block_type_param_defs: &TypeParamData,
) -> bool {
    let ext_tp_id = block_ty.type_param_id().expect("expected type param");

    let check_tp_id = check_ty.type_param_id().expect("expected type param");

    for trait_ty in bounds_for_tp(block_type_param_defs, ext_tp_id) {
        if !tp_implements_trait(&check_type_param_defs, check_tp_id, trait_ty) {
            return false;
        }
    }

    true
}

fn concrete_type_fulfills_bounds(
    vm: &VM,
    check_ty: BytecodeType,
    check_type_param_defs: &TypeParamData,
    block_ty: BytecodeType,
    block_type_param_defs: &TypeParamData,
) -> bool {
    let ext_tp_id = block_ty.type_param_id().expect("expected type param");

    for trait_ty in bounds_for_tp(block_type_param_defs, ext_tp_id) {
        if !ty_implements_trait(vm, check_ty.clone(), check_type_param_defs, trait_ty) {
            return false;
        }
    }

    true
}

fn compare_concrete_types(
    vm: &VM,
    check_ty: BytecodeType,
    check_type_param_defs: &TypeParamData,
    block_ty: BytecodeType,
    block_type_param_defs: &TypeParamData,
    bindings: &mut [Option<BytecodeType>],
) -> bool {
    match check_ty.clone() {
        BytecodeType::Unit
        | BytecodeType::Bool
        | BytecodeType::Char
        | BytecodeType::UInt8
        | BytecodeType::Int32
        | BytecodeType::Int64
        | BytecodeType::Float32
        | BytecodeType::Float64
        | BytecodeType::TypeParam(_) => check_ty == block_ty,

        BytecodeType::Lambda(..) | BytecodeType::TraitObject(..) => {
            unimplemented!()
        }

        BytecodeType::Tuple(check_subtypes) => {
            if !block_ty.is_tuple() {
                return false;
            }

            let ext_subtypes = block_ty.tuple_subtypes();

            if check_subtypes.len() != ext_subtypes.len() {
                return false;
            }

            for (check_subty, ext_subty) in check_subtypes.iter().zip(ext_subtypes.iter()) {
                if !matches(
                    vm,
                    check_subty.clone(),
                    check_type_param_defs,
                    ext_subty.clone(),
                    block_type_param_defs,
                    bindings,
                ) {
                    return false;
                }
            }

            true
        }

        BytecodeType::Struct(check_struct_id, _) => {
            let ext_struct_id = if let BytecodeType::Struct(struct_id, _) = block_ty {
                struct_id
            } else {
                return false;
            };

            if check_struct_id.0 != ext_struct_id.0 {
                return false;
            }

            compare_type_params(
                vm,
                check_ty,
                check_type_param_defs,
                block_ty,
                block_type_param_defs,
                bindings,
            )
        }

        BytecodeType::Enum(check_enum_id, _) => {
            let ext_enum_id = if let BytecodeType::Enum(enum_id, _) = block_ty {
                enum_id
            } else {
                return false;
            };

            if check_enum_id.0 != ext_enum_id.0 {
                return false;
            }

            compare_type_params(
                vm,
                check_ty,
                check_type_param_defs,
                block_ty,
                block_type_param_defs,
                bindings,
            )
        }

        BytecodeType::Class(check_cls_id, _) => {
            let ext_cls_id = if let BytecodeType::Class(cls_id, _) = block_ty {
                cls_id
            } else {
                return false;
            };

            if check_cls_id != ext_cls_id {
                return false;
            }

            compare_type_params(
                vm,
                check_ty,
                check_type_param_defs,
                block_ty,
                block_type_param_defs,
                bindings,
            )
        }

        BytecodeType::TypeAlias(..)
        | BytecodeType::Assoc(..)
        | BytecodeType::GenericAssoc { .. }
        | BytecodeType::Ptr
        | BytecodeType::This => {
            unreachable!()
        }
    }
}

fn compare_type_params(
    vm: &VM,
    check_ty: BytecodeType,
    check_type_param_defs: &TypeParamData,
    block_ty: BytecodeType,
    block_type_param_defs: &TypeParamData,
    bindings: &mut [Option<BytecodeType>],
) -> bool {
    let check_tps = check_ty.type_params();
    let ext_tps = block_ty.type_params();

    assert_eq!(check_tps.len(), ext_tps.len());

    for (check_tp, ext_tp) in check_tps.iter().zip(ext_tps.iter()) {
        if !matches(
            vm,
            check_tp,
            check_type_param_defs,
            ext_tp,
            block_type_param_defs,
            bindings,
        ) {
            return false;
        }
    }

    true
}
