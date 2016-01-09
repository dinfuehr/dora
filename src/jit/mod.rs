use ast::*;
use ctxt::*;

use dseg::DSeg;

use jit::buffer::*;
use jit::codegen::*;

pub mod buffer;
pub mod codegen;
pub mod fct;

mod emit;
mod expr;
mod info;

pub fn generate<'a, 'ast>(ctxt: &'a Context<'a, 'ast>, fct: &'ast Function) -> (DSeg, Vec<u8>) {
    info::generate(ctxt, fct);

    CodeGen::new(ctxt, fct).generate()
}
