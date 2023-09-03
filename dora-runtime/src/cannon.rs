use self::codegen::CannonCodeGen;

use crate::compiler::codegen::CompilationData;
use crate::masm::CodeDescriptor;
use crate::vm::VM;

pub mod codegen;

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
    compilation_data: CompilationData<'a>,
    flags: CompilationFlags,
) -> CodeDescriptor {
    CannonCodeGen::new(vm, compilation_data, flags).generate()
}
