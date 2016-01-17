use libc::*;

extern crate libc;

fn handler(signo: c_int, info: *const siginfo_t, context: *const c_void) {
    println!("signal {}!", signo);
}

fn main() {

    unsafe {
        let mut sa: sigaction = std::mem::uninitialized();

        sa.sa_sigaction = handler as usize;
        sigemptyset(&mut sa.sa_mask as *mut sigset_t);
        sa.sa_flags = SA_SIGINFO;

        if sigaction(SIGINT, &sa as *const sigaction, 0 as *mut sigaction) == -1 {
            perror("sigaction failed".as_ptr() as *const i8);
        }

        raise(SIGINT);
    }

    println!("after raising and handling SIGINT\n");
}
