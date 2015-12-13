#![feature(plugin)]
#![feature(box_syntax)]
#![feature(io)]

#![feature(plugin)]
#![plugin(phf_macros)]
extern crate phf;
extern crate byteorder;
extern crate libc;
extern crate rustc_serialize;
extern crate docopt;
extern crate capstone;

mod ast;
mod codegen;
mod ctxt;
mod driver;
mod dseg;
mod error;
mod interner;
mod mir;
mod lexer;
mod mem;
mod os;
mod parser;
mod semck;
mod stdlib;
mod sym;

#[cfg(test)]
mod test;

#[cfg(not(test))]
use std::process::exit;

#[cfg(not(test))]
fn main() {
    exit(driver::compile());
}
