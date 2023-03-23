use crate::vm::{extension_matches_ty, VM};
use dora_bytecode::FunctionId;
use dora_frontend::language::sem_analysis::{
    FctDefinitionId, ImplDefinitionId, TypeParamDefinition,
};
use dora_frontend::language::ty::{SourceType, SourceTypeArray};

pub fn find_trait_impl(
    vm: &VM,
    fct_id: FunctionId,
    trait_ty: SourceType,
    object_type: SourceType,
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
    assert_eq!(
        impl_.trait_id(),
        trait_ty.trait_id().expect("trait expected")
    );

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
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    trait_ty: SourceType,
) -> Option<ImplDefinitionId> {
    for impl_ in vm.impls.iter() {
        let impl_ = impl_.read();

        assert!(impl_.trait_ty().is_concrete_type());

        if impl_.extended_ty != check_ty {
            continue;
        }

        if impl_.trait_ty() != trait_ty {
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
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    impl_id: ImplDefinitionId,
) -> Option<SourceTypeArray> {
    let impl_ = vm.impls[impl_id].read();
    extension_matches_ty(
        vm,
        check_ty,
        check_type_param_defs,
        impl_.extended_ty.clone(),
        impl_.type_params(),
    )
}

pub fn implements_trait(
    vm: &VM,
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    trait_ty: SourceType,
) -> bool {
    if check_ty.is_primitive()
        && vm.known_instances.zero_trait_id().0 == trait_ty.trait_id().expect("trait expected").0
    {
        assert!(trait_ty.type_params().is_empty());
        return true;
    }

    match check_ty {
        SourceType::Tuple(_)
        | SourceType::Unit
        | SourceType::Trait(_, _)
        | SourceType::Lambda(_, _) => false,

        SourceType::Bool
        | SourceType::UInt8
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Struct(_, _)
        | SourceType::Enum(_, _)
        | SourceType::Class(_, _) => {
            find_impl(vm, check_ty, check_type_param_defs, trait_ty).is_some()
        }

        SourceType::TypeParam(tp_id) => check_type_param_defs.implements_trait(tp_id, trait_ty),

        SourceType::Error | SourceType::Ptr | SourceType::This | SourceType::Any => unreachable!(),
    }
}
