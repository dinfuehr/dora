use self::codegen::CannonCodeGen;

use crate::bytecode;
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
    let bytecode_fct = fct.bytecode.as_ref().expect("bytecode missing");

    if should_emit_bytecode(vm, fct) {
        bytecode::dump(vm, Some(fct), bytecode_fct);
    }

    CannonCodeGen::new(vm, fct, src, bytecode_fct, type_params).generate()
}
