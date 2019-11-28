use self::codegen::CannonCodeGen;

use crate::baseline::asm::BaselineAssembler;
use crate::baseline::codegen::CodeGen;
use crate::baseline::fct::JitBaselineFct;
use crate::ty::TypeList;
use crate::vm::{Fct, FctSrc, VM};

mod codegen;

pub(super) fn compile<'a, 'ast: 'a>(
    vm: &'a VM<'ast>,
    fct: &Fct<'ast>,
    src: &'a mut FctSrc,
    cls_type_params: &TypeList,
    fct_type_params: &TypeList,
) -> JitBaselineFct {
    CannonCodeGen::new(
        vm,
        &fct,
        fct.ast,
        BaselineAssembler::new(vm),
        src,
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
