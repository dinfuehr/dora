use std;
use libc::*;

use cpu::{self, REG_RESULT};
use ctxt::{Context, CTXT, get_ctxt};
use execstate::ExecState;
use jit;
use mem::ptr::Ptr;
use object::{Handle, Obj};
use os_cpu::*;

pub fn register_signals(ctxt: &Context) {
    unsafe {
        let ptr = Ptr::new(ctxt as *const Context as *mut u8);
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
fn handler(signo: c_int, _: *const c_void, ucontext: *const c_void) {
    let mut es = read_execstate(ucontext);
    let ctxt = get_ctxt();

    if let Some(trap) = detect_trap(signo as i32, &es) {
        use cpu::trap::{ASSERT, CAST, COMPILER, INDEX_OUT_OF_BOUNDS, NIL, THROW};

        match trap {
            COMPILER => compile_request(ctxt, &mut es, ucontext),

            ASSERT => {
                println!("assert failed");
                let stacktrace = cpu::get_stacktrace(ctxt, &es);
                stacktrace.dump(ctxt);
                unsafe { _exit(101); }
            }

            INDEX_OUT_OF_BOUNDS => {
                println!("array index out of bounds");
                let stacktrace = cpu::get_stacktrace(ctxt, &es);
                stacktrace.dump(ctxt);
                unsafe { _exit(102); }
            }

            NIL => {
                println!("nil check failed");
                let stacktrace = cpu::get_stacktrace(ctxt, &es);
                stacktrace.dump(ctxt);
                unsafe { _exit(103); }
            }

            THROW => {
                let obj : Handle<Obj> = es.regs[REG_RESULT.int() as usize].into();
                let handler_found = cpu::handle_exception(obj, &mut es);

                if handler_found {
                    write_execstate(&es, ucontext as *mut c_void);
                } else {
                    println!("uncaught exception");
                    unsafe { _exit(104); }
                }
            }

            CAST => {
                println!("cast failed");
                let stacktrace = cpu::get_stacktrace(ctxt, &es);
                stacktrace.dump(ctxt);
                unsafe { _exit(105); }
            }

            _ => {
                println!("unknown trap");
                unsafe { _exit(1); }
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
        code_map.get(es.pc)
    };

    if let Some(fct_id) = fct_id {
        let jit_fct = jit::generate(ctxt, fct_id);
        let fct = ctxt.fct_by_id(fct_id);

        if fct.is_virtual() {
            cpu::trap::patch_vtable_call(ctxt, es, fct_id, jit_fct);
        } else {
            cpu::trap::patch_fct_call(es, jit_fct);
        }

        write_execstate(es, ucontext as *mut c_void);
    } else {
        println!("error: code not found for address {:x}", es.pc);
        unsafe { _exit(200); }
    }
}
