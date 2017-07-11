use std::ptr;

use baseline::fct::ExHandler;
use cpu::*;
use execstate::ExecState;
use mem;
use object::{Handle, Obj};
use os::signal::Trap;
use stacktrace::DoraToNativeInfo;

pub use self::param::*;
pub use self::reg::*;

pub mod asm;
pub mod param;
pub mod reg;

pub fn dtn_from_execution_state(es: &ExecState) -> DoraToNativeInfo {
    let ra = unsafe { *(es.sp as *const usize) };

    DoraToNativeInfo {
        last: ptr::null(),
        fp: es.regs[RBP.int() as usize],
        sp: es.sp + mem::ptr_width() as usize,
        ra: ra,
        xpc: ra - 1,
    }
}

pub fn receiver_from_execstate(es: &ExecState) -> usize {
    es.regs[REG_PARAMS[0].int() as usize]
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

pub fn flush_icache(_: *const u8, _: usize) {
    // no flushing needed on x86_64, but emit compiler barrier

    unsafe {
        asm!("" ::: "memory" : "volatile");
    }
}

pub fn get_exception_object(es: &ExecState) -> Handle<Obj> {
    let obj: Handle<Obj> = es.regs[REG_RESULT.int() as usize].into();

    obj
}

pub fn fp_from_execstate(es: &ExecState) -> usize {
    es.regs[RBP.int() as usize]
}

pub fn ra_from_execstate(es: &ExecState) -> usize {
    unsafe { *(es.sp as *const usize) }
}

pub fn read_trap(es: &ExecState) -> Option<Trap> {
    let v1;
    let v2;

    unsafe {
        let mut ptr: *const u32 = es.pc as *const u32;

        v1 = *ptr;
        ptr = ptr.offset(1);
        v2 = *ptr;
    }

    if v1 == 0x25148b4c {
        Trap::from(v2)
    } else {
        None
    }
}
