use std;
use libc::c_void;

use execstate::ExecState;

pub fn read_execstate(ucontext: *const c_void) -> ExecState {
    let es : ExecState = unsafe { std::mem::zeroed() };

    es
}
