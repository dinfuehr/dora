use std::ptr;

use baseline::fct::ExHandler;
use ctxt::{Context, FctId};
use execstate::ExecState;
use object::{Handle, Obj};
use stacktrace::StackFrameInfo;

pub use self::param::*;
pub use self::reg::*;

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

pub fn flush_icache(start: *const u8, len: usize) {
    let start = start as usize;
    let end = start + len;

    // FIXME: get real data/insn cache-line size
    let icacheline_size = 64;
    let dcacheline_size = 64;

    let istart = start & !(icacheline_size - 1);
    let dstart = start & !(dcacheline_size - 1);

    let mut ptr = dstart;

    while ptr < end {
        unsafe {
            asm!("dc civac, $0":: "r"(ptr) : "memory" : "volatile");
        }

        ptr += dcacheline_size;
    }

    unsafe {
        asm!("dsb ish" ::: "memory" : "volatile");
    }

    ptr = istart;

    while ptr < end {
        unsafe {
            asm!("ic ivau, $0":: "r"(ptr) : "memory" : "volatile");
        }

        ptr += icacheline_size;
    }

    unsafe {
        asm!("dsb ish
              isb" ::: "memory" : "volatile");
    }
}

pub fn fp_from_execstate(es: &ExecState) -> usize {
    es.regs[REG_FP.asm() as usize]
}

pub fn get_exception_object(es: &ExecState) -> Handle<Obj> {
    let obj: Handle<Obj> = es.regs[REG_RESULT.asm() as usize].into();

    obj
}

pub fn ra_from_execstate(es: &ExecState) -> usize {
    es.regs[REG_LR.asm() as usize]
}
