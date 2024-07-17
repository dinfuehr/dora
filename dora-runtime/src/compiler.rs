pub use crate::compiler::codegen::CompilationData;
pub use crate::compiler::codegen::{compile_fct_aot, compile_fct_jit};
pub use crate::compiler::runtime_entry_trampoline::*;

pub mod aot;
pub mod codegen;
pub mod dora_entry_trampoline;
pub mod lazy_compilation_stub;
pub mod runtime_entry_trampoline;
pub mod trait_object_thunk;

#[derive(Copy, Clone)]
pub enum CompilationMode {
    Aot,
    Jit,
}

impl CompilationMode {
    pub fn is_jit(&self) -> bool {
        match self {
            CompilationMode::Jit => true,
            CompilationMode::Aot => false,
        }
    }
}
