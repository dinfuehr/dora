#![feature(plugin)]
#![feature(box_syntax)]
#![feature(io)]

#![feature(plugin)]
#![plugin(phf_macros)]
extern crate phf;

mod lexer;
mod error;
mod parser;
mod ast;
mod data_type;

#[cfg(not(test))]
fn main() {
    let mut reader = parser::Parser::from_str("10");


    match reader.parse() {
        Ok(prog) => println!("prog = {:?}", prog),
        Err(err) => println!("err = {:?}", err),
    }

    println!("hello world!");
}
