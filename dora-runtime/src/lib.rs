#![allow(dead_code)]
#![allow(non_camel_case_types)]
#![recursion_limit = "256"]

extern crate alloc;

#[cfg(target_os = "windows")]
extern crate windows_sys;

#[macro_use]
extern crate memoffset;

#[cfg(feature = "aot")]
mod aot_entry;
mod boots;
mod cannon;
mod compiler;
mod cpu;
mod disassembler;
mod gc;
mod handle;
mod masm;
mod mem;
mod mirror;
mod mode;
mod os;
mod safepoint;
mod shape;
mod size;
mod snapshot;
mod stack;
mod stdlib;
mod threads;
mod timer;
mod utils;
pub mod vm;

pub use compiler::aot::{AotFunction, AotRelocation, compile_program_functions};
use compiler::codegen::{SpecializeSelf, get_bytecode};
pub use compiler::dora_entry_trampoline;
use gc::Address;
use shape::{Shape, ShapeVisitor};
pub use vm::VM;
pub use vm::{
    CollectorName, Compiler, MemSize, ShapeKind, VmFlags, VmMode, clear_vm, execute_on_main, set_vm,
};
