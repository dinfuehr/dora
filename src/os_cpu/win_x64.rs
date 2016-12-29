use std;

use cpu;
use execstate::ExecState;
use os::signal::Trap;
use winapi::winnt::CONTEXT;

pub fn read_execstate(uc: *const u8) -> ExecState {
    unsafe {
        let mut es: ExecState = std::mem::uninitialized();

        let uc = uc as *const CONTEXT;
        let uc = &(*uc);

        es.pc = uc.Rip as usize;
        es.sp = uc.Rsp as usize;
        es.ra = 0;

        es.regs[cpu::RAX] = uc.Rax as usize;
        es.regs[cpu::RCX] = uc.Rcx as usize;
        es.regs[cpu::RDX] = uc.Rdx as usize;
        es.regs[cpu::RBX] = uc.Rbx as usize;
        es.regs[cpu::RSP] = uc.Rsp as usize;
        es.regs[cpu::RBP] = uc.Rbp as usize;
        es.regs[cpu::RSI] = uc.Rsi as usize;
        es.regs[cpu::RDI] = uc.Rdi as usize;
        es.regs[cpu::R8 ] = uc.R8 as usize;
        es.regs[cpu::R9 ] = uc.R9 as usize;
        es.regs[cpu::R10] = uc.R10 as usize;
        es.regs[cpu::R11] = uc.R11 as usize;
        es.regs[cpu::R12] = uc.R12 as usize;
        es.regs[cpu::R13] = uc.R13 as usize;
        es.regs[cpu::R14] = uc.R14 as usize;
        es.regs[cpu::R15] = uc.R15 as usize;

        es
    }
}

pub fn write_execstate(es: &ExecState, uc: *mut u8) {
    unsafe {
        let uc = uc as *mut CONTEXT;
        let uc = &mut (*uc);

        uc.Rip = es.pc as u64;
        uc.Rsp = es.sp as u64;

        uc.Rax = es.regs[cpu::RAX] as u64;
        uc.Rcx = es.regs[cpu::RCX] as u64;
        uc.Rdx = es.regs[cpu::RDX] as u64;
        uc.Rbx = es.regs[cpu::RBX] as u64;
        uc.Rsp = es.regs[cpu::RSP] as u64;
        uc.Rbp = es.regs[cpu::RBP] as u64;
        uc.Rsi = es.regs[cpu::RSI] as u64;
        uc.Rdi = es.regs[cpu::RDI] as u64;
        uc.R8  = es.regs[cpu::R8 ] as u64;
        uc.R9  = es.regs[cpu::R9 ] as u64;
        uc.R10 = es.regs[cpu::R10] as u64;
        uc.R11 = es.regs[cpu::R11] as u64;
        uc.R12 = es.regs[cpu::R12] as u64;
        uc.R13 = es.regs[cpu::R13] as u64;
        uc.R14 = es.regs[cpu::R14] as u64;
        uc.R15 = es.regs[cpu::R15] as u64;
    }
}

pub fn detect_trap(signo: i32, es: &ExecState) -> Option<Trap> {
    unimplemented!()
}