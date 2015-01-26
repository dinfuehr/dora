#![feature(plugin)]

#[plugin] #[no_link]
extern crate phf_mac;
extern crate phf;

use lexer::Lexer;
use parser::Parser;

mod lexer;
mod error;
mod parser;
mod ast;

fn main() {
    let mut reader = Parser::from_str("10");

    match reader.parse() {
        Ok(prog) => println!("prog = {:?}", prog),
        Err(err) => err.println()
    }

    println!("hello world!");
}
