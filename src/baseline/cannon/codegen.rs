use crate::baseline::asm::BaselineAssembler;
use dora_parser::ast::*;

use crate::baseline::codegen::{CodeGen, Scopes};
use crate::baseline::fct::{JitBaselineFct};
use crate::class::TypeParams;
use crate::ctxt::VM;
use crate::ctxt::{Fct, FctSrc};
use crate::masm::*;

pub struct CannonCodeGen<'a, 'ast: 'a> {
    pub vm: &'a VM<'ast>,
    pub fct: &'a Fct<'ast>,
    pub ast: &'ast Function,
    pub asm: BaselineAssembler<'a, 'ast>,
    pub scopes: Scopes,
    pub src: &'a mut FctSrc,

    pub lbl_break: Option<Label>,
    pub lbl_continue: Option<Label>,

    // stores all active finally blocks
    pub active_finallys: Vec<&'ast Stmt>,

    // label to jump instead of emitting epilog for return
    // needed for return's in finally blocks
    // return in finally needs to execute to next finally block and not
    // leave the current function
    pub lbl_return: Option<Label>,

    // length of active_finallys in last loop
    // default: 0
    // break/continue need to emit finally blocks up to the last loop
    // see tests/finally/break-while.dora
    pub active_loop: Option<usize>,

    // upper length of active_finallys in emitting finally-blocks for break/continue
    // default: active_finallys.len()
    // break/continue needs to execute finally-blocks in loop, return in these blocks
    // would dump all active_finally-entries from the loop but we need an upper bound.
    // see emit_finallys_within_loop and tests/finally/continue-return.dora
    pub active_upper: Option<usize>,

    pub cls_type_params: &'a TypeParams,
    pub fct_type_params: &'a TypeParams,
}

impl<'a, 'ast> CannonCodeGen<'a, 'ast>
where
    'ast: 'a,
{   
}

impl<'a, 'ast> CodeGen<'ast> for CannonCodeGen<'a, 'ast> {
    fn generate(mut self) -> JitBaselineFct {
        panic!("not implemented");
    }
}
