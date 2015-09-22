use codegen::buffer::*;
use codegen::codegen::*;
use parser::ast::ctxt::*;
use parser::ast::*;

pub mod buffer;
pub mod codegen;
pub mod x64;
mod expr;
mod info;

pub fn generate<'a, 'ast>(ctxt: &'a Context<'a, 'ast>, fct: &'ast Function) -> Vec<u8> {
    info::generate(ctxt, fct);

    CodeGen::new(ctxt, fct).generate().finish()
}
