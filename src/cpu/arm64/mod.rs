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
    let ra = es.regs[REG_LR.asm() as usize];

    StackFrameInfo {
        last: ptr::null(),
        fp: es.regs[REG_FP.asm() as usize],
        sp: es.sp,
        ra: ra,
        xpc: ra - 1,
    }
}

pub fn resume_with_handler(es: &mut ExecState,
                           handler: &ExHandler,
                           fp: usize,
                           exception: Handle<Obj>,
                           stacksize: usize) {
    unimplemented!();
}

pub fn fp_from_execstate(es: &ExecState) -> usize {
    es.regs[REG_FP.asm() as usize]
}

pub fn get_exception_object(es: &ExecState) -> Handle<Obj> {
    let obj: Handle<Obj> = es.regs[REG_RESULT.int() as usize].into();

    obj
}

pub fn patch_fct_call(es: &mut ExecState, fct_ptr: *const u8) {
    unimplemented!();
}

pub fn patch_vtable_call(ctxt: &Context, es: &mut ExecState, fid: FctId, fct_ptr: *const u8) {
    unimplemented!();
}
