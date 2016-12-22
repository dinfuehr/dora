pub use self::param::*;
pub use self::reg::*;

use baseline::fct::ExHandler;
use ctxt::{Context, FctId};
use execstate::ExecState;
use object::{Handle, Obj};
use stacktrace::StackFrameInfo;

pub mod asm;
pub mod param;
pub mod reg;
pub mod trap;

pub fn sfi_from_execution_state(es: &ExecState) -> StackFrameInfo {
    unimplemented!();
}

pub fn resume_with_handler(es: &mut ExecState, handler: &ExHandler, fp: usize, exception: Handle<Obj>, stacksize: usize) {
    unimplemented!();
}

pub fn fp_from_execstate(es: &ExecState) -> usize {
    unimplemented!();
}

pub fn get_exception_object(es: &ExecState) -> Handle<Obj> {
    unimplemented!();
}

pub fn patch_fct_call(es: &mut ExecState, fct_ptr: *const u8) {
    unimplemented!();
}

pub fn patch_vtable_call(ctxt: &Context, es: &mut ExecState, fid: FctId, fct_ptr: *const u8) {
    unimplemented!();
}