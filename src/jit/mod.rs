use ast::Function;
use ctxt::*;

use jit::buffer::*;
use mem::ptr::Ptr;

pub mod buffer;
pub mod codegen;
pub mod expr;
pub mod fct;
pub mod info;
pub mod map;
pub mod stub;


pub fn generate<'ast>(ctxt: &Context<'ast>, id: FctId) -> Ptr {
    codegen::generate(ctxt, id)
}
