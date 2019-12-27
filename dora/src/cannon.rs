use self::codegen::CannonCodeGen;

use crate::bytecode;
use crate::compiler::asm::BaselineAssembler;
use crate::compiler::codegen::fct_pattern_match;
use crate::compiler::fct::JitFct;
use crate::ty::TypeList;
use crate::vm::{Fct, FctSrc, VM};

mod codegen;

pub(super) fn compile<'a, 'ast: 'a>(
    vm: &'a VM<'ast>,
    fct: &Fct<'ast>,
    src: &'a FctSrc,
    cls_type_params: &TypeList,
    fct_type_params: &TypeList,
) -> JitFct {
    let bytecode_fct = bytecode::generate(vm, fct, src, cls_type_params, fct_type_params);

    if should_emit_bytecode(vm, fct) {
        bytecode_fct.dump();
    }

    let jit_fct_base = CannonCodeGen::new(
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
        cls_type_params,
        fct_type_params,
    )
    .generate();

    JitFct::Base(jit_fct_base)
}

fn should_emit_bytecode(vm: &VM, fct: &Fct) -> bool {
    if let Some(ref dbg_names) = vm.args.flag_emit_bytecode {
        fct_pattern_match(vm, fct, dbg_names)
    } else {
        false
    }
}
