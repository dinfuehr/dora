use crate::compiler::Code;
use crate::driver::cmd::AsmSyntax;
use crate::ty::TypeList;
use crate::vm::{Fct, VM};

pub fn supported() -> bool {
    false
}

pub fn disassemble(
    _vm: &VM,
    _fct: &Fct,
    _type_params: &TypeList,
    _code: &Code,
    _asm_syntax: AsmSyntax,
) {
    unreachable!();
}
