use dora_bytecode::{
    BytecodeTraitType, BytecodeType, BytecodeTypeArray, FunctionId, ImplId, TypeParamData,
};

use crate::runtime::Runtime;

pub use dora_compiler::{
    TypeParamBoundsIter, bounds_for_tp, find_impl_in_program, find_trait_impl_in_program,
    find_trait_ty_impl_in_program, tp_implements_trait, ty_implements_trait_in_program,
};

pub fn find_trait_impl(
    rt: &Runtime,
    fct_id: FunctionId,
    trait_ty: BytecodeTraitType,
    object_type: BytecodeType,
) -> (FunctionId, BytecodeTypeArray) {
    find_trait_impl_in_program(&rt.program, fct_id, trait_ty, object_type)
}

pub fn find_trait_ty_impl(
    rt: &Runtime,
    trait_ty: BytecodeTraitType,
    object_type: BytecodeType,
) -> Option<(ImplId, BytecodeTypeArray)> {
    find_trait_ty_impl_in_program(&rt.program, trait_ty, object_type)
}

pub fn find_impl(
    rt: &Runtime,
    check_ty: BytecodeType,
    check_type_param_defs: &TypeParamData,
    trait_ty: BytecodeTraitType,
) -> Option<(ImplId, BytecodeTypeArray)> {
    find_impl_in_program(&rt.program, check_ty, check_type_param_defs, trait_ty)
}

pub fn ty_implements_trait(
    rt: &Runtime,
    check_ty: BytecodeType,
    check_type_param_defs: &TypeParamData,
    trait_ty: BytecodeTraitType,
) -> bool {
    ty_implements_trait_in_program(&rt.program, check_ty, check_type_param_defs, trait_ty)
}
