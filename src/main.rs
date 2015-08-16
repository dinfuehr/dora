#![feature(plugin)]
#![feature(box_syntax)]
#![feature(io)]

#![feature(plugin)]
#![plugin(phf_macros)]
extern crate phf;
extern crate byteorder;
extern crate libc;

mod lexer;
mod error;
mod parser;
mod ast;
mod semck;
mod sym;
mod driver;


#[cfg(not(test))]
fn main() {
    // start compiler
    driver::compile();
}

