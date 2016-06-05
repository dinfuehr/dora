use libc;

use ctxt::{Context, FctId, FctKind, get_ctxt};
use execstate::ExecState;
use jit::fct::CatchType;
use mem::ptr::Ptr;
use object::{Handle, Obj};
use stacktrace::Stacktrace;

pub use self::param::*;
pub use self::reg::*;

pub mod emit;
pub mod instr;
pub mod param;
pub mod reg;
pub mod trap;

pub fn handle_exception(exception: Handle<Obj>, es: &mut ExecState) -> bool {
    let mut pc : usize = es.pc;
    let mut fp : usize = es.regs[RBP.int() as usize];

    loop {
        let found = find_handler(exception, es, pc, fp);

        match found {
            HandlerFound::Yes => { return true; }
            HandlerFound::Stop => { return false; }
            HandlerFound::No => {
                if fp == 0 { return false; }
            }
        }

        pc = unsafe { *((fp + 8) as *const usize) };
        fp = unsafe { *(fp as *const usize) };
    }
}

#[derive(PartialEq, Eq, Debug)]
enum HandlerFound { Yes, No, Stop }

fn find_handler(exception: Handle<Obj>, es: &mut ExecState, pc: usize, fp: usize) -> HandlerFound {
    let ctxt = get_ctxt();
    let fct_id = {
        let code_map = ctxt.code_map.lock().unwrap();
        code_map.get(pc)
    };

    // println!("------------");
    // println!("find {:x}", pc);

    let mut result = HandlerFound::No;

    if let Some(fct_id) = fct_id {
        ctxt.fct_by_id(fct_id, |fct| {
            if let FctKind::Source(ref src) = fct.kind {
                let mut src = src.lock().unwrap();

                if let Some(ref jit_fct) = src.jit_fct {
                    let cls_id = exception.header().class().id;

                    for entry in &jit_fct.exception_handlers {
                        // println!("entry = {:x} to {:x} for {:?}",
                        //          entry.try_start, entry.try_end, entry.catch_type);

                        if entry.try_start < pc && pc <= entry.try_end
                            && (entry.catch_type == CatchType::Any
                                || entry.catch_type == CatchType::Class(cls_id)) {
                            let arg = (fp as isize + entry.offset as isize) as usize;

                            unsafe {
                                *(arg as *mut usize) = exception.raw() as usize;
                            }

                            es.regs[RSP.int() as usize] = fp - src.stacksize() as usize;
                            es.regs[RBP.int() as usize] = fp;
                            es.pc = entry.catch;

                            result = HandlerFound::Yes;
                            return;

                        } else if pc > entry.try_end {
                            // exception handlers are sorted, no more possible handlers
                            // in this function

                            return;
                        }
                    }
                }

                // exception can only bubble up in stacktrace if current function
                // is allowed to throw exceptions
                if !src.ast.throws {
                    result = HandlerFound::Stop;
                    return;
                }
            }
        });
    }

    result
}

pub fn get_rootset(ctxt: &Context) -> Vec<usize> {
    let mut rootset = Vec::new();
    let mut pc : usize = 0;
    unsafe { asm!("lea (%rip), $0": "=r"(pc)) }

    let mut fp : usize = 0;
    unsafe { asm!("mov %rbp, $0": "=r"(fp)) }

    determine_rootset(&mut rootset, ctxt, fp, pc);

    while fp != 0 {
        pc = unsafe { *((fp + 8) as *const usize) };
        fp = unsafe { *(fp as *const usize) };

        determine_rootset(&mut rootset, ctxt, fp, pc);
    }

    rootset
}

fn determine_rootset(rootset: &mut Vec<usize>, ctxt: &Context, fp: usize, pc: usize) {
    let code_map = ctxt.code_map.lock().unwrap();
    let fct_id = code_map.get(pc);

    if let Some(fct_id) = fct_id {
        let mut lineno = 0;

        ctxt.fct_by_id(fct_id, |fct| {
            if let FctKind::Source(ref src) = fct.kind {
                let mut src = src.lock().unwrap();

                if let Some(ref jit_fct) = src.jit_fct {
                    let offset = pc - (jit_fct.fct_ptr().raw() as usize);
                    let gcpoint = jit_fct.gcpoint_for_offset(offset as i32);

                    if let Some(gcpoint) = jit_fct.gcpoint_for_offset(offset as i32) {
                        for &offset in &gcpoint.offsets {
                            let addr = (fp as isize + offset as isize) as usize;
                            let obj = unsafe { *(addr as *const usize) };

                            rootset.push(obj);
                        }

                    } else {
                        panic!("no gc point found");
                    }
                }
            }
        });
    }
}

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

fn determine_stack_entry(stacktrace: &mut Stacktrace, ctxt: &Context, pc: usize) {
    let code_map = ctxt.code_map.lock().unwrap();
    let fct_id = code_map.get(pc);

    if let Some(fct_id) = fct_id {
        let mut lineno = 0;

        ctxt.fct_by_id(fct_id, |fct| {
            if let FctKind::Source(ref src) = fct.kind {
                let mut src = src.lock().unwrap();
                let jit_fct = src.jit_fct.as_ref().unwrap();
                let offset = pc - (jit_fct.fct_ptr().raw() as usize);
                lineno = jit_fct.lineno_for_offset(offset as i32);

                if lineno == 0 {
                    panic!("lineno not found for program point");
                }
            }

        });

        stacktrace.push_entry(fct_id, lineno);
    }
}
