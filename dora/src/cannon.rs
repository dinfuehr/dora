use self::codegen::CannonCodeGen;

use crate::boots;
use crate::bytecode;
use crate::cannon::liveness::BytecodeLiveness;
use crate::compiler::codegen::{should_emit_asm, should_emit_bytecode, should_emit_debug};
use crate::handle::handle_scope;
use crate::language::sem_analysis::FctDefinition;
use crate::language::ty::SourceTypeArray;
use crate::masm::CodeDescriptor;
use crate::vm::VM;

mod codegen;
mod liveness;

pub struct CompilationFlags {
    mode: CompilationMode,
}

impl CompilationFlags {
    pub fn jit() -> CompilationFlags {
        CompilationFlags {
            mode: CompilationMode::JustInTime,
        }
    }

    pub fn aot() -> CompilationFlags {
        CompilationFlags {
            mode: CompilationMode::AheadOfTime,
        }
    }

    pub fn is_jit(&self) -> bool {
        match self.mode {
            CompilationMode::JustInTime => true,
            CompilationMode::AheadOfTime => false,
        }
    }

    pub fn is_aot(&self) -> bool {
        match self.mode {
            CompilationMode::AheadOfTime => true,
            CompilationMode::JustInTime => false,
        }
    }
}

pub enum CompilationMode {
    AheadOfTime,
    JustInTime,
}

pub(super) fn compile<'a>(
    vm: &'a VM,
    fct: &FctDefinition,
    type_params: &SourceTypeArray,
    flags: CompilationFlags,
) -> CodeDescriptor {
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

    let emit_debug = should_emit_debug(vm, fct);
    let emit_asm = should_emit_asm(vm, fct);

    let params = fct.params_with_self();
    let params = SourceTypeArray::with(params.to_vec());
    let return_type = fct.return_type.clone();

    CannonCodeGen::new(
        vm,
        params,
        fct.is_variadic,
        return_type,
        fct.pos,
        emit_debug,
        emit_asm,
        bytecode_fct,
        liveness,
        type_params,
        flags,
    )
    .generate()
}
