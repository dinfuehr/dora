use libc;

use ctxt::{Context, FctId, FctKind, get_ctxt};
use execstate::ExecState;
use mem::ptr::Ptr;

pub use self::param::*;
pub use self::reg::*;

pub mod emit;
pub mod instr;
pub mod param;
pub mod reg;
pub mod trap;

pub fn unwind(es: &ExecState) {
    let ctxt = get_ctxt();
    let mut stacktrace : Vec<Option<(FctId, u32)>> = Vec::new();

    let mut pc = es.pc;
    stacktrace.push(determine_stack_entry(ctxt, Ptr::new(es.pc as *mut libc::c_void)));

    let mut rbp = es.regs[reg::RBP.int() as usize];

    while rbp != 0 {
        let ra = unsafe { *((rbp + 8) as *const usize) };
        stacktrace.push(determine_stack_entry(ctxt, Ptr::new(ra as *mut libc::c_void)));

        rbp = unsafe { *(rbp as *const usize) };
    }

    println!("stacktrace = {:?}", stacktrace);
}

fn determine_stack_entry(ctxt: &Context, ra: Ptr) -> Option<(FctId, u32)> {
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

        Some((fct_id, lineno))
    } else {
        None
    }
}
