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
            let src = reg2ucontext(i);
            let dest = i;

            es.regs[dest] = mc.gregs[src] as usize;
        }
    }

    es
}

pub fn write_execstate(es: &ExecState, uc: *mut c_void) {
    unsafe {
        let uc = uc as *mut ucontext_t;
        let mc = &mut (*uc).uc_mcontext;

        mc.gregs[REG_RIP] = es.pc as i64;
        mc.gregs[REG_RSP] = es.sp as i64;

        for i in 0..es.regs.len() {
            let src = i;
            let dest = reg2ucontext(i);

            mc.gregs[dest] = es.regs[src] as i64;
        }
    }
}

/// Dora uses same index for register as CPU,
// but the indices in mcontext are different and need to be mapped
fn reg2ucontext(reg: usize) -> usize {
    match reg {
        0 => REG_RAX,
        1 => REG_RCX,
        2 => REG_RDX,
        3 => REG_RBX,
        4 => REG_RSP,
        5 => REG_RBP,
        6 => REG_RSI,
        7 => REG_RDI,
        8 => REG_R8,
        9 => REG_R9,
        10 => REG_R10,
        11 => REG_R11,
        12 => REG_R12,
        13 => REG_R13,
        14 => REG_R14,
        15 => REG_R15,
        _ => unreachable!()
    }
}

pub fn detect_trap(signo: i32, es: &ExecState) -> Option<Trap> {
    use cpu::trap;

    trap::read(&es);

    None
}
