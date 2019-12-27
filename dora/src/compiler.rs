pub use crate::compiler::codegen::{generate, generate_fct};

pub mod asm;
pub mod codegen;
pub mod compile_stub;
pub mod dora_stub;
pub mod fct;
pub mod map;
pub mod native_stub;
pub mod throw_stub;
