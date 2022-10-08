#![allow(dead_code)]
#![allow(non_camel_case_types)]
#![allow(unstable_name_collisions)]
#![recursion_limit = "256"]

extern crate alloc;

#[cfg(target_os = "windows")]
extern crate winapi;

#[macro_use]
extern crate memoffset;

mod aot;
mod bytecode;
mod cannon;
mod compiler;
mod constpool;
mod cpu;
mod disassembler;
mod driver;
mod gc;
mod handle;
mod language;
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
mod vm;
mod vtable;

#[cfg(not(test))]
pub fn run() -> i32 {
    driver::start()
}

#[cfg(feature = "aot")]
#[cfg(not(test))]
#[no_mangle]
pub fn run_aot_program() -> i32 {
    driver::aot::start()
}
