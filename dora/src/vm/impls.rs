use crate::vm::{extension_matches_ty, VM};
use dora_bytecode::{BytecodeType, BytecodeTypeArray, FunctionId};
use dora_frontend::language::generator::{bty_from_ty, ty_from_bty};
use dora_frontend::language::sem_analysis::{
    FctDefinitionId, ImplDefinitionId, TypeParamDefinition, TypeParamId,
};

pub fn find_trait_impl(
    vm: &VM,
    fct_id: FunctionId,
    trait_ty: BytecodeType,
    object_type: BytecodeType,
) -> FunctionId {
    debug_assert!(object_type.is_concrete_type());
    let impl_id = find_impl(
        vm,
        object_type,
        &TypeParamDefinition::new(),
        trait_ty.clone(),
    )
    .expect("no impl found for generic trait method call");

    let impl_ = vm.impls[impl_id].read();

    let trait_id = match trait_ty {
        BytecodeType::Trait(trait_id, _) => trait_id,
        _ => unreachable!(),
    };

    assert_eq!(impl_.trait_id().to_usize(), trait_id.0 as usize);

    let fct_id = FctDefinitionId(fct_id.0 as usize);

    let impl_fct_id = impl_
        .impl_for
        .get(&fct_id)
        .cloned()
        .expect("no impl method found for generic trait call");

    FunctionId(impl_fct_id.0 as u32)
}

fn find_impl(
    vm: &VM,
    check_ty: BytecodeType,
    check_type_param_defs: &TypeParamDefinition,
    trait_ty: BytecodeType,
) -> Option<ImplDefinitionId> {
    for impl_ in vm.impls.iter() {
        let impl_ = impl_.read();

        assert!(impl_.trait_ty().is_concrete_type());

        if impl_.extended_ty != ty_from_bty(check_ty.clone()) {
            continue;
        }

        if impl_.trait_ty() != ty_from_bty(trait_ty.clone()) {
            continue;
        }

        if impl_matches(vm, check_ty.clone(), check_type_param_defs, impl_.id()).is_some() {
            return Some(impl_.id());
        }
    }

    None
}

fn impl_matches(
    vm: &VM,
    check_ty: BytecodeType,
    check_type_param_defs: &TypeParamDefinition,
    impl_id: ImplDefinitionId,
) -> Option<BytecodeTypeArray> {
    let impl_ = vm.impls[impl_id].read();
    extension_matches_ty(
        vm,
        check_ty,
        check_type_param_defs,
        bty_from_ty(impl_.extended_ty.clone()),
        impl_.type_params(),
    )
}

pub fn implements_trait(
    vm: &VM,
    check_ty: BytecodeType,
    check_type_param_defs: &TypeParamDefinition,
    trait_ty: BytecodeType,
) -> bool {
    let trait_id = match trait_ty {
        BytecodeType::Trait(trait_id, _) => trait_id,
        _ => unreachable!(),
    };

    if ty_from_bty(check_ty.clone()).is_primitive() && vm.known.zero_trait_id() == trait_id {
        assert!(ty_from_bty(trait_ty).type_params().is_empty());
        return true;
    }

    match check_ty.clone() {
        BytecodeType::Tuple(_)
        | BytecodeType::Unit
        | BytecodeType::Trait(_, _)
        | BytecodeType::Lambda(_, _) => false,

        BytecodeType::Bool
        | BytecodeType::UInt8
        | BytecodeType::Char
        | BytecodeType::Int32
        | BytecodeType::Int64
        | BytecodeType::Float32
        | BytecodeType::Float64
        | BytecodeType::Struct(_, _)
        | BytecodeType::Enum(_, _)
        | BytecodeType::Class(_, _) => {
            find_impl(vm, check_ty, check_type_param_defs, trait_ty).is_some()
        }

        BytecodeType::TypeParam(tp_id) => check_type_param_defs
            .implements_trait(TypeParamId(tp_id as usize), ty_from_bty(trait_ty)),

        BytecodeType::Ptr | BytecodeType::This => unreachable!(),
    }
}
