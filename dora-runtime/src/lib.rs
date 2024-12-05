#![allow(dead_code)]
#![allow(non_camel_case_types)]
#![recursion_limit = "256"]

extern crate alloc;

#[cfg(target_os = "windows")]
extern crate windows_sys;

#[macro_use]
extern crate memoffset;

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
mod size;
mod stack;
mod stdlib;
mod threads;
mod timer;
mod utils;
pub mod vm;
mod vtable;

pub use vm::VM;
pub use vm::{
    clear_vm, execute_on_main, set_vm, CollectorName, Compiler, MemSize, ShapeKind, VmFlags,
};
use vtable::{ShapeVisitor, VTable};
