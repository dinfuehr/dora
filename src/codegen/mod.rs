use codegen::buffer::*;
use codegen::codegen::*;
use parser::ast::ctxt::*;
use parser::ast::*;

use dseg::DSeg;

pub mod buffer;
pub mod codegen;
mod x64;
mod expr;
mod info;
mod emit;

pub fn generate<'a, 'ast>(ctxt: &'a Context<'a, 'ast>, fct: &'ast Function) -> (DSeg, Vec<u8>) {
    info::generate(ctxt, fct);

    CodeGen::new(ctxt, fct).generate()
}
