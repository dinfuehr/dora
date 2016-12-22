use std;
use libc::*;

use baseline;
use cpu;
use ctxt::{Context, CTXT, get_ctxt};
use execstate::ExecState;
use os_cpu::*;
use stacktrace::{handle_exception, get_stacktrace};

pub fn register_signals(ctxt: &Context) {
    unsafe {
        let ptr = ctxt as *const Context as *const u8;
        CTXT = Some(ptr);

        let mut sa: sigaction = std::mem::uninitialized();

        sa.sa_sigaction = handler as usize;
        sigemptyset(&mut sa.sa_mask as *mut sigset_t);
        sa.sa_flags = SA_SIGINFO;

        if sigaction(SIGSEGV, &sa as *const sigaction, 0 as *mut sigaction) == -1 {
            perror("sigaction failed".as_ptr() as *const i8);
        }
    }
}

// signal handler function
fn handler(signo: c_int, _: *const u8, ucontext: *const c_void) {
    let mut es = read_execstate(ucontext);
    let ctxt = get_ctxt();

    if let Some(trap) = detect_trap(signo as i32, &es) {
        match trap {
            Trap::COMPILER => compile_request(ctxt, &mut es, ucontext),

            Trap::DIV0 => {
                println!("division by 0");
                let stacktrace = get_stacktrace(ctxt, &es);
                stacktrace.dump(ctxt);
                unsafe { _exit(101); }
            }

            Trap::ASSERT => {
                println!("assert failed");
                let stacktrace = get_stacktrace(ctxt, &es);
                stacktrace.dump(ctxt);
                unsafe { _exit(101); }
            }

            Trap::INDEX_OUT_OF_BOUNDS => {
                println!("array index out of bounds");
                let stacktrace = get_stacktrace(ctxt, &es);
                stacktrace.dump(ctxt);
                unsafe { _exit(102); }
            }

            Trap::NIL => {
                println!("nil check failed");
                let stacktrace = get_stacktrace(ctxt, &es);
                stacktrace.dump(ctxt);
                unsafe { _exit(103); }
            }

            Trap::THROW => {
                let handler_found = handle_exception(&mut es);

                if handler_found {
                    write_execstate(&es, ucontext as *mut c_void);
                } else {
                    println!("uncaught exception");
                    unsafe { _exit(104); }
                }
            }

            Trap::CAST => {
                println!("cast failed");
                let stacktrace = get_stacktrace(ctxt, &es);
                stacktrace.dump(ctxt);
                unsafe { _exit(105); }
            }

            Trap::UNEXPECTED => {
                println!("unexpected exception");
                let stacktrace = get_stacktrace(ctxt, &es);
                stacktrace.dump(ctxt);
                unsafe { _exit(106); }
            }
        }

    // could not recognize trap -> crash vm
    } else {
        println!("error: trap not detected (signal {}).", signo);
        unsafe { _exit(1); }
    }
}

fn compile_request(ctxt: &Context, es: &mut ExecState, ucontext: *const c_void) {
    let fct_id = {
        let code_map = ctxt.code_map.lock().unwrap();
        code_map.get(es.pc as *const u8)
    };

    if let Some(fct_id) = fct_id {
        let mut sfi = cpu::sfi_from_execution_state(es);

        ctxt.use_sfi(&mut sfi, || {
            let jit_fct = baseline::generate(ctxt, fct_id);
            let fct = ctxt.fct_by_id(fct_id);

            if fct.is_virtual() {
                cpu::patch_vtable_call(ctxt, es, fct_id, jit_fct);
            } else {
                cpu::patch_fct_call(es, jit_fct);
            }

            write_execstate(es, ucontext as *mut c_void);
        });
    } else {
        println!("error: code not found for address {:x}", es.pc);
        unsafe { _exit(200); }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Trap {
    COMPILER, DIV0, ASSERT,
    INDEX_OUT_OF_BOUNDS, NIL, THROW,
    CAST, UNEXPECTED
}

impl Trap {
    pub fn int(self) -> u32 {
        match self {
            Trap::COMPILER => 0,
            Trap::DIV0 => 1,
            Trap::ASSERT => 2,
            Trap::INDEX_OUT_OF_BOUNDS => 3,
            Trap::NIL => 4,
            Trap::THROW => 5,
            Trap::CAST => 6,
            Trap::UNEXPECTED => 7,
        }
    }

    pub fn from(value: u32) -> Option<Trap> {
        match value {
            0 => Some(Trap::COMPILER),
            1 => Some(Trap::DIV0),
            2 => Some(Trap::ASSERT),
            3 => Some(Trap::INDEX_OUT_OF_BOUNDS),
            4 => Some(Trap::NIL),
            5 => Some(Trap::THROW),
            6 => Some(Trap::CAST),
            7 => Some(Trap::UNEXPECTED),
            _ => None
        }
    }
}
