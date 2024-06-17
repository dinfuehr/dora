pub use crate::compiler::codegen::CompilationData;
pub use crate::compiler::codegen::{compile_fct_aot, compile_fct_jit};
pub use crate::compiler::runtime_entry_trampoline::*;

pub mod aot;
pub mod asm;
pub mod codegen;
pub mod dora_entry_trampoline;
pub mod lazy_compilation_stub;
pub mod runtime_entry_trampoline;
pub mod trait_object_thunk;

pub enum CompilationMode {
    Aot,
    Jit,
}
