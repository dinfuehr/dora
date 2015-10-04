#![feature(plugin)]
#![feature(box_syntax)]
#![feature(io)]
#![feature(result_expect)]

#![feature(plugin)]
#![plugin(phf_macros)]
extern crate phf;
extern crate byteorder;
extern crate libc;
extern crate rustc_serialize;
extern crate docopt;
extern crate capstone;

use std::process::exit;

mod error;
mod parser;
mod semck;
mod sym;
mod driver;
mod codegen;
mod mem;
mod dseg;

#[cfg(test)]
mod test;

#[cfg(not(test))]
fn main() {
    exit(driver::compile());
}
