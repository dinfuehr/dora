use execstate::ExecState;
use object::{Handle, Obj};
use os::signal::Trap;

pub use self::param::*;
pub use self::reg::*;

pub mod asm;
pub mod param;
pub mod reg;

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
