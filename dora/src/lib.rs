#![feature(asm)]
#![feature(box_syntax)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]
#![feature(allocator_api)]
#![feature(llvm_asm)]
#![feature(new_uninit)]
#![feature(option_result_contains)]

extern crate alloc;

#[cfg(target_os = "windows")]
extern crate winapi;

#[macro_use]
extern crate num_derive;

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
mod error;
mod gc;
mod handle;
mod masm;
mod mem;
mod object;
mod os;
mod safepoint;
mod semck;
mod size;
mod stack;
mod stdlib;
mod sym;
mod threads;
mod timer;
mod ty;
mod utils;
mod vm;
mod vtable;

#[cfg(test)]
mod test;

#[cfg(not(test))]
pub fn run() -> i32 {
    driver::start()
}
