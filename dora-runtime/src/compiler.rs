pub use crate::compiler::codegen::generate_fct;
pub use crate::compiler::runtime_entry_trampoline::*;

pub mod asm;
pub mod codegen;
pub mod dora_entry_trampoline;
pub mod lazy_compilation_stub;
pub mod runtime_entry_trampoline;
pub mod trait_object_thunk;
