use ast::*;
use ctxt::*;

use dseg::DSeg;

use jit::buffer::*;
use jit::codegen::*;

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
