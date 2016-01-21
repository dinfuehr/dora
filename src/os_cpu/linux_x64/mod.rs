use std;
use libc::c_void;

use execstate::ExecState;
use trap::Trap;

use self::ucontext::ucontext_t;
use self::ucontext_reg::*;

mod ucontext;
mod ucontext_reg;

pub fn read_execstate(uc: *const c_void) -> ExecState {
    let mut es : ExecState = unsafe { std::mem::uninitialized() };

    unsafe {
        let uc = uc as *mut ucontext_t;
        let mc = &(*uc).uc_mcontext;

        es.pc = mc.gregs[REG_RIP] as usize;
        es.sp = mc.gregs[REG_RSP] as usize;
        es.ra = 0;

        for i in 0..es.regs.len() {
            es.regs[i] = mc.gregs[i] as usize;
        }
    }

    es
}

pub fn write_execstate(es: ExecState, uc: *const c_void) {
    unsafe {
        let uc = uc as *mut ucontext_t;
        let mc = &mut (*uc).uc_mcontext;

        mc.gregs[REG_RIP] = es.pc as i64;
        mc.gregs[REG_RSP] = es.sp as i64;

        for i in 0..es.regs.len() {
            mc.gregs[i] = es.regs[i] as i64;
        }
    }
}

pub fn detect_trap(signo: i32, es: &ExecState) -> Option<Trap> {
    None
}
