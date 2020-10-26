use crate::compiler::Code;
use crate::driver::cmd::AsmSyntax;
use crate::ty::TypeList;
use crate::vm::{Fct, FctSrc, VM};

pub fn supported() -> bool {
    false
}

pub fn disassemble<'ast>(
    _vm: &VM<'ast>,
    _fct: &Fct,
    _type_params: &TypeList,
    _code: &Code,
    _fct_src: Option<&FctSrc>,
    _asm_syntax: AsmSyntax,
) {
    unreachable!();
}
