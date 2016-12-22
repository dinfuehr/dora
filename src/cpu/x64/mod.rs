use std::ptr;

use baseline::fct::ExHandler;
use cpu::*;
use ctxt::{Context, FctId};
use execstate::ExecState;
use mem;
use object::{Handle, Obj};
use stacktrace::StackFrameInfo;

pub use self::param::*;
pub use self::reg::*;

pub mod asm;
pub mod param;
pub mod reg;

pub fn sfi_from_execution_state(es: &ExecState) -> StackFrameInfo {
    let ra = unsafe { *(es.sp as *const usize) };

    StackFrameInfo {
        last: ptr::null(),
        fp: es.regs[RBP.int() as usize],
        sp: es.sp + mem::ptr_width() as usize,
        ra: ra,
        xpc: ra - 1,
    }
}

pub fn resume_with_handler(es: &mut ExecState,
                           handler: &ExHandler,
                           fp: usize,
                           exception: Handle<Obj>,
                           stacksize: usize) {
    if let Some(offset) = handler.offset {
        let arg = (fp as isize + offset as isize) as usize;

        unsafe {
            *(arg as *mut usize) = exception.raw() as usize;
        }
    }

    es.regs[RSP.int() as usize] = fp - stacksize;
    es.regs[RBP.int() as usize] = fp;
    es.pc = handler.catch;
}

pub fn get_exception_object(es: &ExecState) -> Handle<Obj> {
    let obj: Handle<Obj> = es.regs[REG_RESULT.int() as usize].into();

    obj
}

pub fn fp_from_execstate(es: &ExecState) -> usize {
    es.regs[RBP.int() as usize]
}

pub fn patch_fct_call(es: &mut ExecState, fct_ptr: *const u8) {
    // get return address from top of stack
    let mut ra: isize = unsafe { *(es.sp as *const isize) };

    // return address is now after `call *%rax` (needs 2 bytes),
    // we want to be before it
    ra -= 2;

    // return address is now after `movq (%rip, disp), %rax`, we also
    // want to be before it to execute it again
    ra -= 7;

    // get address of function pointer
    let disp_addr: *const i32 = (ra + 3) as *const i32;
    let disp: isize = unsafe { *disp_addr } as isize;

    let fct_addr: *mut usize = (ra + 7 + disp) as *mut usize;

    // write function pointer
    unsafe {
        *fct_addr = fct_ptr as usize;
    }

    // execute fct call again
    es.pc = fct_ptr as usize;
}

pub fn patch_vtable_call(ctxt: &Context, es: &mut ExecState, fid: FctId, fct_ptr: *const u8) {
    let fct = ctxt.fct_by_id(fid);
    let vtable_index = fct.vtable_index.unwrap();
    let cls_id = fct.owner_class.unwrap();

    let cls = ctxt.cls_by_id(cls_id);
    let vtable = cls.vtable.as_ref().unwrap();

    let methodtable = vtable.table_mut();
    methodtable[vtable_index as usize] = fct_ptr as usize;

    // execute fct call again
    es.pc = fct_ptr as usize;
}
