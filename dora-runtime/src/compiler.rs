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
pub enum CompilationMode {
    Stage1,
    Stage2,
    Stage3,
    Jit,
}

impl CompilationMode {
    pub fn is_stage2_or_3(&self) -> bool {
        matches!(self, CompilationMode::Stage2 | CompilationMode::Stage3)
    }

    pub fn is_jit(&self) -> bool {
        matches!(self, CompilationMode::Jit)
    }
}
