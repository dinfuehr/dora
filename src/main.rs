#![feature(plugin)]
#![feature(box_syntax)]
#![feature(io)]

#![feature(plugin)]
#![plugin(phf_macros)]
extern crate phf;
extern crate byteorder;
extern crate libc;

#[cfg(not(test))]
use std::process::exit;

use ast::Ast;
use interner::Interner;
use parser::Parser;

mod cmd;
mod lexer;
mod error;
mod parser;
mod ast;
mod interner;
mod semck;
mod sym;


#[cfg(not(test))]
fn main() {
    let cmd = match cmd::parse() {
        Ok(cmd) => cmd,

        Err(_) => {
            cmd::usage();
            exit(1);
        }
    };

    let mut parser = match Parser::from_file(cmd.filename()) {
        Err(_) => {
            println!("unable to read file `{}`", cmd.filename());
            exit(1);
        }

        Ok(parser) => parser
    };

    let (ast, mut interner) = match parser.parse() {
        Ok(ret) => ret,

        Err(error) => {
            error.print();
            exit(1);
        }
    };

    ast::dump::dump(&ast, &interner);

    if let Err(errors) = semck::check(&ast, &mut interner) {
        for err in &errors {
            err.print();
        }

        println!("\n{} errors found", errors.len());
        exit(1);
    }
}

