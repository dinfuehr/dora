#![feature(asm)]
#![feature(alloc)]
#![feature(box_syntax)]
#![allow(dead_code)]
#![allow(non_camel_case_types)]
#![feature(allocator_api)]

extern crate alloc;
extern crate byteorder;
extern crate capstone;
extern crate docopt;
extern crate dora_parser;
extern crate libc;
extern crate rustc_serialize;
extern crate time;
extern crate core;

#[cfg(target_os = "windows")]
extern crate winapi;

#[cfg(target_os = "windows")]
extern crate kernel32;

macro_rules! offset_of {
    ($ty: ty, $field: ident) => {
        unsafe { &(*(0 as *const $ty)).$field as *const _ as usize }
    };
}

pub mod baseline;
pub mod bytecode;
pub mod class;
pub mod cpu;
pub mod ctxt;
pub mod driver;
pub mod dseg;
pub mod exception;
pub mod execstate;
pub mod gc;
pub mod handle;
pub mod masm;
pub mod mem;
pub mod mir;
pub mod object;
pub mod opt;
pub mod os;
pub mod os_cpu;
pub mod semck;
pub mod stdlib;
pub mod safepoint;
pub mod sym;
pub mod threads;
pub mod timer;
pub mod ty;
pub mod utils;
pub mod vtable;

#[cfg(test)]
pub mod test;
use std::path::Path;

use ctxt::{ SemContext};
use dora_parser::ast::Ast;

use dora_parser::interner::Interner;
use driver::cmd;

use dora_parser::parser::NodeIdGenerator;

use driver::*;
use dora_parser::ast;

pub fn start(file: &Path, custom_funcs: Option<*const u8>) -> i32 {
    let args = cmd::Args::default();
    os::init_page_size();


    let mut interner = Interner::new();
    let id_generator = NodeIdGenerator::new();
    let mut ast = Ast::new();

    if let Err(code) = parse_dir("stdlib", &id_generator, &mut ast, &mut interner).and_then(|_| {
        let path = file;

        if path.is_file() {
            parse_file(&file.to_owned().to_str().unwrap(), &id_generator, &mut ast,&mut interner)
        } else if path.is_dir() {
            parse_dir(&file.to_owned().to_str().unwrap(), &id_generator, &mut ast,&mut interner)
        } else {
            println!("file or directory `{}` does not exist.", &args.arg_file);
            Err(1)
        }
    }) {
        return code;
    }

    if args.flag_emit_ast {
        ast::dump::dump(&ast,&interner);
    }

    
    let mut ctxt = SemContext::new(args, &ast, interner);
    semck::check(&mut ctxt, custom_funcs);
    

    // register signal handler
    os::register_signals(&ctxt);

    let main = find_main(&ctxt);

    if ctxt.diag.borrow().has_errors() {
        ctxt.diag.borrow().dump();
        let no_errors = ctxt.diag.borrow().errors().len();

        if no_errors == 1 {
            println!("{} error found.", no_errors);
        } else {
            println!("{} errors found.", no_errors);
        }

        return 1;
    }


    run_main(&ctxt, main.unwrap())
}


pub use semck::prelude::internal_fct;
pub use semck::prelude::internal_method;
pub use semck::prelude::native_fct;
pub use semck::prelude::native_method;