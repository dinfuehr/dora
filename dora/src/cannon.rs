use self::codegen::CannonCodeGen;

use crate::bytecode;
use crate::compiler::asm::BaselineAssembler;
use crate::compiler::codegen::fct_pattern_match;
use crate::compiler::fct::JitBaselineFct;
use crate::ty::TypeList;
use crate::vm::{Fct, FctSrc, VM};

mod codegen;

pub(super) fn compile<'a, 'ast: 'a>(
    vm: &'a VM<'ast>,
    fct: &Fct<'ast>,
    src: &'a FctSrc,
    cls_type_params: &TypeList,
    fct_type_params: &TypeList,
) -> JitBaselineFct {
    let bytecode_fct = bytecode::generate(vm, fct, src, cls_type_params, fct_type_params);

    if should_emit_bytecode(vm, fct) {
        bytecode_fct.dump();
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
        cls_type_params,
        fct_type_params,
    )
    .generate()
}

fn should_emit_bytecode(vm: &VM, fct: &Fct) -> bool {
    if let Some(ref dbg_names) = vm.args.flag_emit_bytecode {
        fct_pattern_match(vm, fct, dbg_names)
    } else {
        false
    }
}
