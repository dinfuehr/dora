pub use crate::compiler::codegen::CompilationData;
pub use crate::compiler::codegen::{compile_fct_aot, compile_fct_jit};
pub use crate::compiler::runtime_entry_trampoline::*;

pub mod aot;
pub mod codegen;
pub mod dora_entry_trampoline;
pub mod lazy_compilation_stub;
pub mod runtime_entry_trampoline;
pub mod trait_object_thunk;

#[derive(Copy, Clone, Debug)]
#[repr(u8)]
pub enum CompilationMode {
    Stage1,
    Stage2,
    Stage3,
    Jit,
}

impl CompilationMode {
    pub fn is_stage2_or_3(&self) -> bool {
        match self {
            CompilationMode::Stage2 | CompilationMode::Stage3 => true,
            _ => false,
        }
    }

    pub fn is_jit(&self) -> bool {
        match self {
            CompilationMode::Jit => true,
            _ => false,
        }
    }
}
