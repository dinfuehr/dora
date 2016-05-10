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

// pub fn get_rootset(ctxt: &Context, e) -> Vec<usize> {
//     let mut root_set = Vec::new();
//     let pc : usize = 0;
//     determine_root_set(&mut root_set, ctxt, pc);
//
//     let mut rbp = es.regs[reg::RBP.int() as usize];
//
//     while rbp != 0 {
//         let ra = unsafe { *((rbp + 8) as *const usize) };
//         determine_root_set(&mut stacktrace, ctxt, ra);
//
//         rbp = unsafe { *(rbp as *const usize) };
//     }
//
//     return stacktrace;
// }

// fn determine_root_set(root_set: Vec<usize>, ctxt: &Context, ra: usize) {
//     let code_map = ctxt.code_map.lock().unwrap();
//     let fct_id = code_map.get(ra);
//
//     if let Some(fct_id) = fct_id {
//         let mut lineno = 0;
//
//         ctxt.fct_by_id(fct_id, |fct| {
//             if let FctKind::Source(ref src) = fct.kind {
//                 if let Some(ref jit_fct) = src.jit_fct {
//                     let offset = ra - (jit_fct.fct_ptr().raw() as usize);
//                     lineno = jit_fct.safepoint_for_offset(offset as i32);
//                 }
//             }
//
//         });
//
//         root_set.push(fct_id, lineno);
//     }
// }

pub fn get_stacktrace(ctxt: &Context, es: &ExecState) -> Stacktrace {
    let mut stacktrace = Stacktrace::new();
    determine_stack_entry(&mut stacktrace, ctxt, es.pc);

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
                let jit_fct = src.jit_fct.as_ref().unwrap();
                let offset = ra - (jit_fct.fct_ptr().raw() as usize);
                lineno = jit_fct.lineno_for_offset(offset as i32);

                if lineno == 0 {
                    panic!("lineno not found for program point");
                }
            }

        });

        stacktrace.push_entry(fct_id, lineno);
    }
}
