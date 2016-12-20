use ctxt::*;

use mem::ptr::Ptr;

pub mod codegen;
pub mod expr;
pub mod fct;
pub mod info;
pub mod map;
pub mod native;
pub mod stub;


pub fn generate<'ast>(ctxt: &Context<'ast>, id: FctId) -> Ptr {
    codegen::generate(ctxt, id)
}
