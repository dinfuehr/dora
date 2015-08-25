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

mod error;
mod parser;
mod semck;
mod sym;
mod driver;
mod hir;


#[cfg(not(test))]
fn main() {
    // start compiler
    driver::compile();
}
