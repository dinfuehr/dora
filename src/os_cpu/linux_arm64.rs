use libc;
use std;

use execstate::ExecState;
use os::signal::Trap;

pub fn read_execstate(uc: *const u8) -> ExecState {
    let mut es: ExecState = unsafe { std::mem::uninitialized() };

    let uc = uc as *mut ucontext_t;
    let mc = unsafe { &(*uc).uc_mcontext };

    es.pc = mc.pc;
    es.sp = mc.sp;
    es.ra = 0;

    for i in 0..es.regs.len() {
        es.regs[i] = mc.regs[i];
    }

    es
}

pub fn write_execstate(es: &ExecState, uc: *mut u8) {
    let uc = uc as *mut ucontext_t;
    let mc = unsafe { &mut (*uc).uc_mcontext };

    mc.pc = es.pc;
    mc.sp = es.sp;

    for i in 0..es.regs.len() {
        mc.regs[i] = es.regs[i];
    }
}

pub fn detect_trap(signo: i32, es: &ExecState) -> Option<Trap> {
    if signo == libc::SIGILL {
        read_trap(&es)
    } else {
        None
    }
}

fn read_trap(es: &ExecState) -> Option<Trap> {
    let v: u32;

    unsafe {
        let ptr: *const u32 = es.pc as *const u32;

        v = *ptr;
    }

    if v & 0xFF000000 == 0xE7000000 {
        let v = v & 0xFF;

        Trap::from(v)
    } else {
        None
    }
}

#[repr(C)]
struct ucontext_t {
    _ignore: [u64; 22],
    pub uc_mcontext: mcontext_t,
}

#[repr(C)]
struct mcontext_t {
    _ignore: u64,
    pub regs: [usize; 31],
    pub sp: usize,
    pub pc: usize,
}
