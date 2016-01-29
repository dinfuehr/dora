use ast::Function;
use ctxt::*;

use dseg::DSeg;

use jit::buffer::*;
use mem::ptr::Ptr;

pub mod buffer;
pub mod codegen;
pub mod expr;
pub mod fct;
pub mod info;
pub mod map;
pub mod stub;


pub fn generate<'a, 'ast>(ctxt: &'a Context<'a, 'ast>, id: FctContextId) -> Ptr {
    codegen::generate(ctxt, id)
}
