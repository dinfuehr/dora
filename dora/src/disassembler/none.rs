use crate::vm::{Code, VM};
use dora_bytecode::{BytecodeTypeArray, FunctionId};

pub fn supported() -> bool {
    false
}

pub fn disassemble(_vm: &VM, _fct_id: FunctionId, _type_params: &BytecodeTypeArray, _code: &Code) {
    unreachable!();
}
