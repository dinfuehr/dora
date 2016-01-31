use std;
use std::mem;
use libc::*;

use cpu;
use ctxt::{Context, ctxt_ptr};
use jit;
use mem::ptr::Ptr;
use os_cpu::*;

pub fn register_signals(ctxt: &Context) {
    unsafe {
        let ptr = Ptr::new(ctxt as *const Context as *mut c_void);
        unsafe { ctxt_ptr = Some(ptr); }

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
    let mut es = read_execstate(ucontext);

    if let Some(trap) = detect_trap(signo as i32, &es) {
        use cpu::trap::COMPILER;

        match trap {
            COMPILER => {
                let ctxt: &Context = unsafe {
                    &*(ctxt_ptr.unwrap().raw() as *const Context)
                };

                let ptr = Ptr::new(es.pc as *mut c_void);
                let code_map = ctxt.code_map.lock().unwrap();

                if let Some(fct_id) = code_map.get(ptr) {
                    let jit_fct = jit::generate(ctxt, fct_id);
                    cpu::trap::patch_fct_call(&mut es, jit_fct);
                    write_execstate(&es, ucontext as *mut c_void);
                } else {
                    println!("error: code not found for address {:x}", ptr.raw() as u64);
                    unsafe { _exit(1); }
                }
            }

            _ => unsafe { _exit(1); }
        }

    // could not recognize trap -> crash vm
    } else {
        println!("error: trap not detected (signal {}).", signo);
        unsafe { _exit(1); }
    }
}
