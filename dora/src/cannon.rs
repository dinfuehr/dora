use self::codegen::CannonCodeGen;

use crate::bytecode;
use crate::compiler::asm::BaselineAssembler;
use crate::compiler::codegen::should_emit_bytecode;
use crate::compiler::Code;
use crate::ty::TypeList;
use crate::vm::{Fct, FctSrc, VM};

mod codegen;

pub(super) fn compile<'a, 'ast: 'a>(
    vm: &'a VM<'ast>,
    fct: &Fct<'ast>,
    src: &'a FctSrc,
    type_params: &TypeList,
) -> Code {
    let bytecode_fct = bytecode::generate(vm, fct, src, type_params);

    if should_emit_bytecode(vm, fct) {
        bytecode::dump(vm, &bytecode_fct);
    }

    CannonCodeGen::new(
        vm,
        &fct,
        fct.ast,
        BaselineAssembler::new(vm),
        src,
        &bytecode_fct,
        None,
        None,
        Vec::new(),
        None,
        None,
        None,
        type_params,
    )
    .generate()
}
