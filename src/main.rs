#![feature(plugin)]
#![feature(box_syntax)]

#![feature(plugin)]
#![plugin(phf_macros)]
extern crate phf;

use parser::Parser;

mod lexer;
mod error;
mod parser;
mod ast;
mod data_type;

fn main() {
    let mut reader = Parser::from_str("10");

    match reader.parse() {
        Ok(prog) => println!("prog = {:?}", prog),
        Err(err) => println!("err = {:?}", err),
    }

    println!("hello world!");
}
