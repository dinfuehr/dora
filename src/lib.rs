#![feature(asm)]
#![feature(alloc)]
#![feature(box_syntax)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]
#![feature(allocator_api)]

extern crate alloc;
extern crate backtrace;
extern crate byteorder;
extern crate capstone;
extern crate core;
extern crate crossbeam_deque;
extern crate docopt;
extern crate dora_parser;
extern crate fixedbitset;
extern crate libc;
extern crate num_cpus;
extern crate parking_lot;
extern crate perfcnt;
extern crate rand;
extern crate regex;
extern crate rustc_serialize;
extern crate scoped_threadpool;
extern crate time;

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
mod ctxt;
mod driver;
mod dseg;
mod exception;
mod execstate;
mod gc;
mod handle;
mod masm;
mod mem;
mod object;
mod opt;
mod os;
mod os_cpu;
mod safepoint;
mod semck;
mod stdlib;
mod sym;
mod threads;
mod timer;
mod ty;
mod utils;
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
