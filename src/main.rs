extern crate byteorder;
extern crate libc;
extern crate rustc_serialize;
extern crate docopt;
extern crate capstone;

mod ast;
mod cpu;
mod ctxt;
mod driver;
mod dseg;
mod error;
mod interner;
mod jit;
mod lexer;
mod mir;
mod mem;
mod os;
mod parser;
mod semck;
mod stdlib;
mod sym;
mod trap;

#[cfg(test)]
mod test;

#[cfg(not(test))]
use std::process::exit;

#[cfg(not(test))]
fn main() {
    exit(driver::compile());
}
