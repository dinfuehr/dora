use libc;

use ctxt::{Context, FctId, FctKind, get_ctxt};
use execstate::ExecState;
use mem::ptr::Ptr;
use stack::Stacktrace;

pub use self::param::*;
pub use self::reg::*;

pub mod emit;
pub mod instr;
pub mod param;
pub mod reg;
pub mod trap;

pub fn get_stacktrace(ctxt: &Context, es: &ExecState) -> Stacktrace {
    let mut stacktrace = Stacktrace::new();

    let mut pc = es.pc;
    determine_stack_entry(&mut stacktrace, ctxt, Ptr::new(es.pc as *mut libc::c_void));

    let mut rbp = es.regs[reg::RBP.int() as usize];

    while rbp != 0 {
        let ra = unsafe { *((rbp + 8) as *const usize) };
        determine_stack_entry(&mut stacktrace, ctxt, Ptr::new(ra as *mut libc::c_void));

        rbp = unsafe { *(rbp as *const usize) };
    }

    return stacktrace;
}

fn determine_stack_entry(stacktrace: &mut Stacktrace, ctxt: &Context, ra: Ptr) {
    let code_map = ctxt.code_map.lock().unwrap();
    let fct_id = code_map.get(ra);

    if let Some(fct_id) = fct_id {
        let mut lineno = 0;

        ctxt.fct_by_id(fct_id, |fct| {
            if let FctKind::Source(ref src) = fct.kind {
                if let Some(ref jit_fct) = src.jit_fct {
                    // TODO: access line info table as soon as it is available
                    lineno = 1;
                }
            }

        });

        stacktrace.push_entry(fct_id, lineno);
    }
}
