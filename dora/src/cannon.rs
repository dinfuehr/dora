use self::codegen::CannonCodeGen;

use crate::boots;
use crate::bytecode;
use crate::cannon::liveness::BytecodeLiveness;
use crate::compiler::codegen::should_emit_bytecode;
use crate::handle::handle_scope;
use crate::language::ty::SourceTypeArray;
use crate::vm::{Code, FctDefinition, VM};

mod codegen;
mod liveness;

pub(super) fn compile<'a>(vm: &'a VM, fct: &FctDefinition, type_params: &SourceTypeArray) -> Code {
    let bytecode_fct = fct.bytecode.as_ref().expect("bytecode missing");

    if should_emit_bytecode(vm, fct) {
        bytecode::dump(vm, Some(fct), bytecode_fct);
    }

    if vm.args.flag_boots.is_some() {
        handle_scope(|| {
            boots::encode_test(vm, fct, type_params);
        })
    }

    let liveness = BytecodeLiveness::analyze(bytecode_fct);

    CannonCodeGen::new(vm, fct, bytecode_fct, liveness, type_params).generate()
}
