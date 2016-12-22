use std;

use execstate::ExecState;
use os::signal::Trap;

pub fn read_execstate(uc: *const u8) -> ExecState {
    unimplemented!();
}

pub fn write_execstate(es: &ExecState, uc: *mut u8) {
    unimplemented!();
}

pub fn detect_trap(signo: i32, es: &ExecState) -> Option<Trap> {
    unimplemented!();
}
