#![feature(asm)]

extern crate byteorder;
extern crate libc;
extern crate rustc_serialize;
extern crate docopt;
extern crate capstone;

mod ast;
mod class;
mod cpu;
mod ctxt;
mod driver;
mod dseg;
mod error;
mod execstate;
mod gc;
mod interner;
mod jit;
mod lexer;
mod mem;
mod object;
mod os;
mod os_cpu;
mod parser;
mod semck;
mod stack;
mod stdlib;
mod sym;
mod ty;

#[cfg(test)]
mod test;

#[cfg(not(test))]
use std::process::exit;

#[cfg(not(test))]
fn main() {
    exit(driver::start());
}
