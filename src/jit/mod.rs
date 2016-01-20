use ast::Function;
use ctxt::*;

use dseg::DSeg;

use jit::buffer::*;
use jit::codegen::*;
use jit::fct::JitFct;

pub mod buffer;
pub mod codegen;
pub mod fct;
pub mod map;

mod expr;
mod info;
mod stub;

pub fn generate<'a, 'ast>(ctxt: &'a Context<'a, 'ast>, fct: &'ast Function) -> JitFct {
    info::generate(ctxt, fct);

    CodeGen::new(ctxt, fct).generate()
}
