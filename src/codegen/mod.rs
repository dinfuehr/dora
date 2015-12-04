use codegen::buffer::*;
use codegen::codegen::*;
use ast::ctxt::*;
use ast::*;

use dseg::DSeg;

pub mod buffer;
pub mod codegen;
pub mod fct;
mod x64;
mod expr;
mod info;
mod emit;

pub fn generate<'a, 'ast>(ctxt: &'a Context<'a, 'ast>, fct: &'ast Function) -> (DSeg, Vec<u8>) {
    info::generate(ctxt, fct);

    CodeGen::new(ctxt, fct).generate()
}
