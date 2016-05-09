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

pub fn get_rootset(ctxt: &Context) -> Vec<Ptr> {
    // determine_root_set(&mut root_set, )
    Vec::new()
}

pub fn get_stacktrace(ctxt: &Context, es: &ExecState) -> Stacktrace {
    let mut stacktrace = Stacktrace::new();

    let mut pc = es.pc;
    determine_stack_entry(&mut stacktrace, ctxt, es.pc.into());

    let mut rbp = es.regs[reg::RBP.int() as usize];

    while rbp != 0 {
        let ra = unsafe { *((rbp + 8) as *const usize) };
        determine_stack_entry(&mut stacktrace, ctxt, ra);

        rbp = unsafe { *(rbp as *const usize) };
    }

    return stacktrace;
}

fn determine_stack_entry(stacktrace: &mut Stacktrace, ctxt: &Context, ra: usize) {
    let code_map = ctxt.code_map.lock().unwrap();
    let fct_id = code_map.get(ra);

    if let Some(fct_id) = fct_id {
        let mut lineno = 0;

        ctxt.fct_by_id(fct_id, |fct| {
            if let FctKind::Source(ref src) = fct.kind {
                if let Some(ref jit_fct) = src.jit_fct {
                    let offset = ra - (jit_fct.fct_ptr().raw() as usize);
                    lineno = jit_fct.lineno_for_offset(offset as i32);
                }
            }

        });

        stacktrace.push_entry(fct_id, lineno);
    }
}
