#![feature(asm)]
#![feature(alloc)]
#![feature(box_syntax)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]
#![feature(allocator_api)]

extern crate alloc;
extern crate byteorder;
extern crate capstone;
extern crate docopt;
extern crate dora_parser;
extern crate libc;
extern crate llvm_sys as llvm;
extern crate rustc_serialize;
extern crate time;

#[cfg(target_os = "windows")]
extern crate winapi;

#[cfg(target_os = "windows")]
extern crate kernel32;

macro_rules! offset_of {
    ($ty:ty, $field:ident) => {
        unsafe { &(*(0 as *const $ty)).$field as *const _ as usize }
    }
}

mod baseline;
mod bytecode;
mod class;
mod cpu;
mod ctxt;
mod driver;
mod dseg;
mod exception;
mod execstate;
mod gc;
mod handle;
mod masm;
mod mem;
mod mir;
mod object;
mod opt;
mod os;
mod os_cpu;
mod semck;
mod stdlib;
mod safepoint;
mod sym;
mod threads;
mod timer;
mod ty;
mod utils;
mod vtable;

#[cfg(test)]
mod test;

#[cfg(not(test))]
use std::process::exit;

#[cfg(not(test))]
fn main() {
    os::mem::init_page_size();

    exit(driver::start());
}
