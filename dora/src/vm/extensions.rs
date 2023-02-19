use crate::language::sem_analysis::TypeParamDefinition;
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::vm::{implements_trait, VM};

pub fn extension_matches_ty(
    vm: &VM,
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    ext_ty: SourceType,
    ext_type_param_defs: &TypeParamDefinition,
) -> Option<SourceTypeArray> {
    let mut bindings = vec![None; ext_type_param_defs.len()];

    let result = matches(
        vm,
        check_ty,
        check_type_param_defs,
        ext_ty.clone(),
        ext_type_param_defs,
        &mut bindings,
    );

    if result {
        Some(SourceTypeArray::with(
            bindings.into_iter().map(|t| t.unwrap()).collect(),
        ))
    } else {
        None
    }
}

fn matches(
    vm: &VM,
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    ext_ty: SourceType,
    ext_type_param_defs: &TypeParamDefinition,
    bindings: &mut [Option<SourceType>],
) -> bool {
    if let SourceType::TypeParam(tp_id) = ext_ty {
        let binding = bindings[tp_id.to_usize()].clone();

        if let Some(binding) = binding {
            compare_concrete_types(
                vm,
                check_ty,
                check_type_param_defs,
                binding,
                ext_type_param_defs,
                bindings,
            )
        } else {
            let result = if check_ty.is_type_param() {
                compare_type_param_bounds(
                    vm,
                    check_ty.clone(),
                    check_type_param_defs,
                    ext_ty,
                    ext_type_param_defs,
                )
            } else {
                concrete_type_fulfills_bounds(
                    vm,
                    check_ty.clone(),
                    check_type_param_defs,
                    ext_ty,
                    ext_type_param_defs,
                )
            };

            bindings[tp_id.to_usize()] = Some(check_ty);

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
                ext_ty,
                ext_type_param_defs,
                bindings,
            )
        }
    }
}

fn compare_type_param_bounds(
    _vm: &VM,
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    ext_ty: SourceType,
    ext_type_param_defs: &TypeParamDefinition,
) -> bool {
    let ext_tp_id = ext_ty.type_param_id().expect("expected type param");

    let check_tp_id = check_ty.type_param_id().expect("expected type param");

    for trait_ty in ext_type_param_defs.bounds_for_type_param(ext_tp_id) {
        if !check_type_param_defs.implements_trait(check_tp_id, trait_ty) {
            return false;
        }
    }

    true
}

fn concrete_type_fulfills_bounds(
    vm: &VM,
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    ext_ty: SourceType,
    ext_type_param_defs: &TypeParamDefinition,
) -> bool {
    let ext_tp_id = ext_ty.type_param_id().expect("expected type param");

    for trait_ty in ext_type_param_defs.bounds_for_type_param(ext_tp_id) {
        if !implements_trait(vm, check_ty.clone(), check_type_param_defs, trait_ty) {
            return false;
        }
    }

    true
}

fn compare_concrete_types(
    vm: &VM,
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    ext_ty: SourceType,
    ext_type_param_defs: &TypeParamDefinition,
    bindings: &mut [Option<SourceType>],
) -> bool {
    match check_ty {
        SourceType::Unit
        | SourceType::Bool
        | SourceType::Char
        | SourceType::UInt8
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::TypeParam(_) => check_ty == ext_ty,

        SourceType::Lambda(_, _) | SourceType::Trait(_, _) => {
            unimplemented!()
        }

        SourceType::Tuple(check_subtypes) => {
            if !ext_ty.is_tuple() {
                return false;
            }

            let ext_subtypes = ext_ty.tuple_subtypes();

            if check_subtypes.len() != ext_subtypes.len() {
                return false;
            }

            for (check_subty, ext_subty) in check_subtypes.iter().zip(ext_subtypes.iter()) {
                if !matches(
                    vm,
                    check_subty.clone(),
                    check_type_param_defs,
                    ext_subty.clone(),
                    ext_type_param_defs,
                    bindings,
                ) {
                    return false;
                }
            }

            true
        }

        SourceType::Struct(check_struct_id, _) => {
            let ext_struct_id = if let Some(struct_id) = ext_ty.struct_id() {
                struct_id
            } else {
                return false;
            };

            if check_struct_id != ext_struct_id {
                return false;
            }

            compare_type_params(
                vm,
                check_ty,
                check_type_param_defs,
                ext_ty,
                ext_type_param_defs,
                bindings,
            )
        }

        SourceType::Enum(check_enum_id, _) => {
            let ext_enum_id = if let Some(enum_id) = ext_ty.enum_id() {
                enum_id
            } else {
                return false;
            };

            if check_enum_id != ext_enum_id {
                return false;
            }

            compare_type_params(
                vm,
                check_ty,
                check_type_param_defs,
                ext_ty,
                ext_type_param_defs,
                bindings,
            )
        }

        SourceType::Class(check_cls_id, _) => {
            let ext_cls_id = if let Some(cls_id) = ext_ty.cls_id() {
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
                ext_ty,
                ext_type_param_defs,
                bindings,
            )
        }

        SourceType::Ptr | SourceType::Error | SourceType::This | SourceType::Any => {
            unreachable!()
        }
    }
}

fn compare_type_params(
    vm: &VM,
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    ext_ty: SourceType,
    ext_type_param_defs: &TypeParamDefinition,
    bindings: &mut [Option<SourceType>],
) -> bool {
    let check_tps = check_ty.type_params();
    let ext_tps = ext_ty.type_params();

    assert_eq!(check_tps.len(), ext_tps.len());

    for (check_tp, ext_tp) in check_tps.iter().zip(ext_tps.iter()) {
        if !matches(
            vm,
            check_tp,
            check_type_param_defs,
            ext_tp,
            ext_type_param_defs,
            bindings,
        ) {
            return false;
        }
    }

    true
}
