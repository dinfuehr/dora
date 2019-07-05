use libc::SIGSEGV;
use std;

use crate::cpu;
use crate::execstate::ExecState;
use crate::os::signal::Trap;

pub fn read_execstate(uc: *const u8) -> ExecState {
    let mut es: ExecState = unsafe { std::mem::uninitialized() };

    unsafe {
        let uc = uc as *mut ucontext_t;
        let mc = (*uc).uc_mcontext;
        let ss = &(*mc).ss[..];

        es.pc = ss[REG_RIP] as usize;
        es.sp = ss[REG_RSP] as usize;
        es.ra = 0;

        for i in 0..es.regs.len() {
            let src = reg2ucontext(i);
            let dest = i;

            es.regs[dest] = ss[src] as usize;
        }
    }

    es
}

pub fn write_execstate(es: &ExecState, uc: *mut u8) {
    unsafe {
        let uc = uc as *mut ucontext_t;
        let mc = (*uc).uc_mcontext;
        let ss = &mut (*mc).ss[..];

        ss[REG_RIP] = es.pc as u64;
        ss[REG_RSP] = es.sp as u64;

        for i in 0..es.regs.len() {
            let src = i;
            let dest = reg2ucontext(i);

            ss[dest] = es.regs[src] as u64;
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
pub struct ucontext_t {
    _ignore: [u8; 48],
    pub uc_mcontext: *mut mcontext_t,
}

#[repr(C)]
pub struct mcontext_t {
    _ignore: [u8; 16],
    pub ss: [u64; 17],
}

pub const REG_RAX: usize = 0;
pub const REG_RBX: usize = 1;
pub const REG_RCX: usize = 2;
pub const REG_RDX: usize = 3;
pub const REG_RDI: usize = 4;
pub const REG_RSI: usize = 5;
pub const REG_RBP: usize = 6;
pub const REG_RSP: usize = 7;
pub const REG_R8: usize = 8;
pub const REG_R9: usize = 9;
pub const REG_R10: usize = 10;
pub const REG_R11: usize = 11;
pub const REG_R12: usize = 12;
pub const REG_R13: usize = 13;
pub const REG_R14: usize = 14;
pub const REG_R15: usize = 15;
pub const REG_RIP: usize = 16;
