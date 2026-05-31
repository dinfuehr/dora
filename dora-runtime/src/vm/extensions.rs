use dora_bytecode::{BytecodeType, BytecodeTypeArray, TypeParamData};

use crate::vm::VM;

pub use dora_compiler::block_matches_ty_in_program;

pub fn block_matches_ty(
    vm: &VM,
    check_ty: BytecodeType,
    check_type_param_defs: &TypeParamData,
    block_ty: BytecodeType,
    block_type_param_defs: &TypeParamData,
) -> Option<BytecodeTypeArray> {
    block_matches_ty_in_program(
        &vm.program,
        check_ty,
        check_type_param_defs,
        block_ty,
        block_type_param_defs,
    )
}
