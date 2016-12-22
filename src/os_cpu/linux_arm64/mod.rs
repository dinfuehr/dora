use std;
use libc::{c_void, SIGSEGV};

use execstate::ExecState;
use os::signal::Trap;

pub fn read_execstate(uc: *const c_void) -> ExecState {
    unimplemented!();
}

pub fn write_execstate(es: &ExecState, uc: *mut c_void) {
    unimplemented!();
}

pub fn detect_trap(signo: i32, es: &ExecState) -> Option<Trap> {
    unimplemented!();
}
