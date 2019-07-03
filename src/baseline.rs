pub use crate::baseline::codegen::{generate, generate_fct};

mod asm;
pub mod codegen;
pub mod dora_compile;
pub mod dora_entry;
pub mod dora_native;
pub mod dora_throw;
pub mod expr;
pub mod fct;
pub mod info;
pub mod map;
pub mod standard;
