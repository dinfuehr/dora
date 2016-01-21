use std;
use libc::*;

use os_cpu::*;
use trap::*;

pub fn register_signals() {
    unsafe {
        let mut sa: sigaction = std::mem::uninitialized();

        sa.sa_sigaction = handler as usize;
        sigemptyset(&mut sa.sa_mask as *mut sigset_t);
        sa.sa_flags = SA_SIGINFO;

        if sigaction(SIGSEGV, &sa as *const sigaction, 0 as *mut sigaction) == -1 {
            perror("sigaction failed".as_ptr() as *const i8);
        }
    }
}

fn handler(signo: c_int, _: *const c_void, ucontext: *const c_void) {
    let es = read_execstate(ucontext);

    if let Some(trap) = detect_trap(signo as i32, &es) {
        match trap {
            Trap::Compiler => {
                println!("need to compile function");
            }

            Trap::Div0 => {
                println!("error: division by zero.");
            }
        }

    // could not recognize trap -> crash vm
    } else {
        println!("error: trap not detected (signal {}).", signo);
        unsafe { _exit(1); }
    }
}
