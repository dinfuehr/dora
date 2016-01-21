use std;
use libc::*;

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

fn handler(signo: c_int) {
    println!("SIGNAL {}!", signo);

    unsafe { _exit(1); }
}
