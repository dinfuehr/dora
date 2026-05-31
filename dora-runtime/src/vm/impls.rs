use dora_bytecode::{
    BytecodeTraitType, BytecodeType, BytecodeTypeArray, FunctionId, ImplId, TypeParamData,
};

use crate::vm::VM;

pub use dora_compiler::{
    TypeParamBoundsIter, bounds_for_tp, find_impl_in_program, find_trait_impl_in_program,
    find_trait_ty_impl_in_program, tp_implements_trait, ty_implements_trait_in_program,
};

pub fn find_trait_impl(
    vm: &VM,
    fct_id: FunctionId,
    trait_ty: BytecodeTraitType,
    object_type: BytecodeType,
) -> (FunctionId, BytecodeTypeArray) {
    find_trait_impl_in_program(&vm.program, fct_id, trait_ty, object_type)
}

pub fn find_trait_ty_impl(
    vm: &VM,
    trait_ty: BytecodeTraitType,
    object_type: BytecodeType,
) -> Option<(ImplId, BytecodeTypeArray)> {
    find_trait_ty_impl_in_program(&vm.program, trait_ty, object_type)
}

pub fn find_impl(
    vm: &VM,
    check_ty: BytecodeType,
    check_type_param_defs: &TypeParamData,
    trait_ty: BytecodeTraitType,
) -> Option<(ImplId, BytecodeTypeArray)> {
    find_impl_in_program(&vm.program, check_ty, check_type_param_defs, trait_ty)
}

pub fn ty_implements_trait(
    vm: &VM,
    check_ty: BytecodeType,
    check_type_param_defs: &TypeParamData,
    trait_ty: BytecodeTraitType,
) -> bool {
    ty_implements_trait_in_program(&vm.program, check_ty, check_type_param_defs, trait_ty)
}
