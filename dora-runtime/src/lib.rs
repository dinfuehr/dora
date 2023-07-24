#![allow(dead_code)]
#![allow(non_camel_case_types)]
#![recursion_limit = "256"]

extern crate alloc;

#[cfg(target_os = "windows")]
extern crate winapi;

#[macro_use]
extern crate memoffset;

mod boots;
mod cannon;
mod compiler;
mod constpool;
mod cpu;
mod disassembler;
mod gc;
mod handle;
mod masm;
mod mem;
mod mode;
mod object;
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
    clear_vm, display_fct, execute_on_main, set_vm, CollectorName, CompilerName, Flags, MemSize,
};
