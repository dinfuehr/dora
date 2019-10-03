#![feature(asm)]
#![feature(box_syntax)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]
#![feature(allocator_api)]

extern crate alloc;

#[cfg(target_os = "windows")]
extern crate winapi;

#[cfg(target_os = "windows")]
extern crate kernel32;

macro_rules! offset_of {
    ($ty: ty, $field: ident) => {
        unsafe { &(*(0 as *const $ty)).$field as *const _ as usize }
    };
}

mod baseline;
mod boots;
mod bytecode;
mod class;
mod cpu;
mod driver;
mod dseg;
mod error;
mod exception;
mod execstate;
mod field;
mod gc;
mod handle;
mod masm;
mod mem;
mod module;
mod object;
mod opt;
mod os;
mod os_cpu;
mod safepoint;
mod semck;
mod size;
mod stdlib;
mod sym;
mod threads;
mod timer;
mod ty;
mod typeck;
mod typeparams;
mod utils;
mod vm;
mod vtable;

#[cfg(test)]
mod test;

#[cfg(not(test))]
pub fn run_content(content: &str) -> i32 {
    driver::start(Some(content))
}

#[cfg(not(test))]
pub fn run() -> i32 {
    driver::start(None)
}
