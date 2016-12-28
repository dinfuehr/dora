use std;
use libc::SIGSEGV;

use cpu;
use execstate::ExecState;
use os::signal::Trap;

pub fn read_execstate(uc: *const u8) -> ExecState {
    let mut es: ExecState = unsafe { std::mem::uninitialized() };

    unsafe {
        let uc = uc as *mut ucontext_t;
        let mc = &(*uc).uc_mcontext;

        es.pc = mc.regs[REG_RIP];
        es.sp = mc.regs[REG_RSP];
        es.ra = 0;

        for i in 0..es.regs.len() {
            let src = reg2ucontext(i);
            let dest = i;

            es.regs[dest] = mc.regs[src];
        }
    }

    es
}

pub fn write_execstate(es: &ExecState, uc: *mut u8) {
    unsafe {
        let uc = uc as *mut ucontext_t;
        let mc = &mut (*uc).uc_mcontext;

        mc.regs[REG_RIP] = es.pc;
        mc.regs[REG_RSP] = es.sp;

        for i in 0..es.regs.len() {
            let src = i;
            let dest = reg2ucontext(i);

            mc.regs[dest] = es.regs[src];
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
        _ => unreachable!(),
    }
}

pub fn detect_trap(signo: i32, es: &ExecState) -> Option<Trap> {
    if signo == SIGSEGV {
        cpu::read_trap(&es)
    } else {
        None
    }
}

#[repr(C)]
struct ucontext_t {
    _ignore: [u64; 5],
    pub uc_mcontext: mcontext_t,
}

#[repr(C)]
struct mcontext_t {
    pub regs: [usize; 23],
}

const REG_R8: usize = 0;
const REG_R9: usize = 1;
const REG_R10: usize = 2;
const REG_R11: usize = 3;
const REG_R12: usize = 4;
const REG_R13: usize = 5;
const REG_R14: usize = 6;
const REG_R15: usize = 7;
const REG_RDI: usize = 8;
const REG_RSI: usize = 9;
const REG_RBP: usize = 10;
const REG_RBX: usize = 11;
const REG_RDX: usize = 12;
const REG_RAX: usize = 13;
const REG_RCX: usize = 14;
const REG_RSP: usize = 15;
const REG_RIP: usize = 16;
