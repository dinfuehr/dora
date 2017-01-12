#![feature(asm)]
#![feature(alloc)]
#![feature(heap_api)]
#![feature(box_syntax)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]

extern crate alloc;
extern crate byteorder;
extern crate capstone;
extern crate docopt;
extern crate libc;
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

mod ast;
mod baseline;
mod class;
mod cpu;
mod ctxt;
mod driver;
mod dseg;
mod error;
mod execstate;
mod gc;
mod interner;
mod lexer;
mod masm;
mod mem;
mod mir;
mod object;
mod os;
mod os_cpu;
mod parser;
mod semck;
mod stacktrace;
mod stdlib;
mod sym;
mod timer;
mod ty;
mod vtable;

#[cfg(test)]
mod test;

#[cfg(not(test))]
use std::process::exit;

#[cfg(not(test))]
fn main() {
    exit(driver::start());
}
