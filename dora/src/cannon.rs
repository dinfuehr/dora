use self::codegen::CannonCodeGen;

use crate::bytecode;
use crate::cannon::liveness::BytecodeLiveness;
use crate::compiler::codegen::should_emit_bytecode;
use crate::compiler::Code;
use crate::ty::SourceTypeArray;
use crate::vm::{Fct, VM};

mod codegen;
mod liveness;

pub(super) fn compile<'a>(vm: &'a VM, fct: &Fct, type_params: &SourceTypeArray) -> Code {
    let bytecode_fct = fct.bytecode.as_ref().expect("bytecode missing");

    if should_emit_bytecode(vm, fct) {
        bytecode::dump(vm, Some(fct), bytecode_fct);
    }

    let liveness = BytecodeLiveness::analyze(bytecode_fct);

    CannonCodeGen::new(vm, fct, bytecode_fct, liveness, type_params).generate()
}
