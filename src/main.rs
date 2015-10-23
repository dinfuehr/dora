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

mod error;
mod parser;
mod semck;
mod sym;
mod driver;
mod codegen;
mod mem;
mod dseg;
mod stdlib;
mod os;

#[cfg(test)]
mod test;

#[cfg(not(test))]
use std::process::exit;

#[cfg(not(test))]
fn main() {
    exit(driver::compile());
}
