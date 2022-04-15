pub use crate::compiler::codegen::{generate, generate_fct};
pub use crate::compiler::dora_exit_stubs::*;

pub mod asm;
pub mod codegen;
pub mod dora_entry_stub;
pub mod dora_exit_stubs;
pub mod lazy_compile_stub;
