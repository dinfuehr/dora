#![feature(asm)]
#![feature(box_syntax)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]
#![feature(allocator_api)]
#![feature(llvm_asm)]
#![feature(new_uninit)]
#![recursion_limit = "256"]

extern crate alloc;

#[cfg(target_os = "windows")]
extern crate winapi;

#[macro_use]
extern crate memoffset;

mod boots;
mod bytecode;
mod cannon;
mod compiler;
mod cpu;
mod disassembler;
mod driver;
mod dseg;
mod gc;
mod handle;
mod language;
mod masm;
mod mem;
mod object;
mod os;
mod pkg;
mod safepoint;
mod size;
mod stack;
mod stdlib;
mod threads;
mod timer;
mod ty;
mod utils;
mod vm;
mod vtable;

#[cfg(not(test))]
pub fn run() -> i32 {
    driver::start()
}
