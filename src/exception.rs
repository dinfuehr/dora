use std::ptr;

use baseline::fct::{CatchType, JitFctId};
use baseline::map::CodeData;
use cpu::{fp_from_execstate, get_exception_object, resume_with_handler};
use ctxt::{get_ctxt, SemContext};
use execstate::ExecState;
use object::{alloc, Array, Exception, Handle, IntArray, Obj, StackTraceElement, Str};

pub struct Stacktrace {
    elems: Vec<StackElem>,
}

impl Stacktrace {
    pub fn new() -> Stacktrace {
        Stacktrace { elems: Vec::new() }
    }

    pub fn len(&self) -> usize {
        self.elems.len()
    }

    pub fn push_entry(&mut self, fct_id: JitFctId, lineno: i32) {
        self.elems.push(StackElem {
            fct_id: fct_id,
            lineno: lineno,
        });
    }

    pub fn dump(&self, ctxt: &SemContext) {
        for (ind, elem) in self.elems.iter().enumerate() {
            let jit_fct = ctxt.jit_fcts[elem.fct_id].borrow();
            let fct_id = jit_fct.fct_id();
            let fct = ctxt.fcts[fct_id].borrow();
            let name = fct.full_name(ctxt);
            print!("{}: {}: ", ind, name);

            if elem.lineno == 0 {
                println!("?");
            } else {
                println!("{}", elem.lineno);
            }
        }
    }
}

struct StackElem {
    fct_id: JitFctId,
    lineno: i32,
}

pub struct DoraToNativeInfo {
    // pointer to previous DTN-Info
    pub last: *const DoraToNativeInfo,

    // frame pointer of native stub
    pub fp: usize,

    // some program counter into native stub
    pub pc: usize,
}

impl DoraToNativeInfo {
    pub fn new() -> DoraToNativeInfo {
        DoraToNativeInfo {
            last: ptr::null(),
            fp: 0,
            pc: 0,
        }
    }
}

pub fn stacktrace_from_es(ctxt: &SemContext, es: &ExecState) -> Stacktrace {
    let mut stacktrace = Stacktrace::new();
    let fp = fp_from_execstate(es);
    frames_from_pc(&mut stacktrace, ctxt, es.pc, fp);
    frames_from_dtns(&mut stacktrace, ctxt);
    return stacktrace;
}

pub fn stacktrace_from_last_dtn(ctxt: &SemContext) -> Stacktrace {
    let mut stacktrace = Stacktrace::new();
    frames_from_dtns(&mut stacktrace, ctxt);
    return stacktrace;
}

fn frames_from_dtns(stacktrace: &mut Stacktrace, ctxt: &SemContext) {
    let mut dtn_ptr = *ctxt.dtn.borrow();

    while !dtn_ptr.is_null() {
        let dtn = unsafe { &*dtn_ptr };

        let pc: usize = dtn.pc;
        let fp: usize = dtn.fp;

        frames_from_pc(stacktrace, ctxt, pc, fp);

        dtn_ptr = dtn.last
    }
}

fn frames_from_pc(stacktrace: &mut Stacktrace, ctxt: &SemContext, pc: usize, mut fp: usize) {
    if !determine_stack_entry(stacktrace, ctxt, pc) {
        return;
    }

    while fp != 0 {
        let ra = unsafe { *((fp + 8) as *const usize) };

        if !determine_stack_entry(stacktrace, ctxt, ra) {
            return;
        }

        fp = unsafe { *(fp as *const usize) };
    }
}

fn determine_stack_entry(stacktrace: &mut Stacktrace, ctxt: &SemContext, pc: usize) -> bool {
    let code_map = ctxt.code_map.lock().unwrap();
    let data = code_map.get(pc as *const u8);

    match data {
        Some(CodeData::Fct(fct_id)) => {
            let jit_fct = ctxt.jit_fcts[fct_id].borrow();

            let offset = pc - (jit_fct.fct_ptr() as usize);
            let jit_fct = jit_fct.to_base().expect("baseline expected");
            let lineno = jit_fct.lineno_for_offset(offset as i32);

            if lineno == 0 {
                panic!("lineno not found for program point");
            }

            stacktrace.push_entry(fct_id, lineno);

            true
        }

        Some(CodeData::NativeStub(fct_id)) => {
            let jit_fct = ctxt.jit_fcts[fct_id].borrow();
            let fct = ctxt.fcts[jit_fct.fct_id()].borrow();

            stacktrace.push_entry(fct_id, fct.ast.pos.line as i32);

            true
        }

        _ => false,
    }
}

pub fn handle_exception(es: &mut ExecState) -> bool {
    let mut pc: usize = es.pc;
    let mut fp: usize = fp_from_execstate(es);

    let exception = get_exception_object(es);

    loop {
        let found = find_handler(exception, es, pc, fp);

        match found {
            HandlerFound::Yes => {
                return true;
            }
            HandlerFound::Stop => {
                return false;
            }
            HandlerFound::No => {
                if fp == 0 {
                    return false;
                }
            }
        }

        pc = unsafe { *((fp + 8) as *const usize) };
        fp = unsafe { *(fp as *const usize) };
    }
}

#[derive(PartialEq, Eq, Debug)]
enum HandlerFound {
    Yes,
    No,
    Stop,
}

fn find_handler(exception: Handle<Obj>, es: &mut ExecState, pc: usize, fp: usize) -> HandlerFound {
    let ctxt = get_ctxt();
    let data = {
        let code_map = ctxt.code_map.lock().unwrap();
        code_map.get(pc as *const u8)
    };

    match data {
        Some(CodeData::Fct(fct_id)) | Some(CodeData::NativeStub(fct_id)) => {
            let jit_fct = ctxt.jit_fcts[fct_id].borrow();
            let jit_fct = jit_fct.to_base().expect("baseline expected");
            let clsptr = exception.header().vtbl().classptr();

            for entry in &jit_fct.exception_handlers {
                // println!("entry = {:x} to {:x} for {:?}",
                //          entry.try_start, entry.try_end, entry.catch_type);

                if entry.try_start < pc && pc <= entry.try_end
                    && (entry.catch_type == CatchType::Any
                        || entry.catch_type == CatchType::Class(clsptr))
                {
                    let stacksize = jit_fct.framesize as usize;
                    resume_with_handler(es, entry, fp, exception, stacksize);

                    return HandlerFound::Yes;
                } else if pc > entry.try_end {
                    // exception handlers are sorted, no more possible handlers
                    // in this function

                    return HandlerFound::No;
                }
            }

            // exception can only bubble up in stacktrace if current function
            // is allowed to throw exceptions
            if !jit_fct.throws {
                return HandlerFound::Stop;
            }
        }

        _ => {}
    }

    HandlerFound::No
}

pub extern "C" fn retrieve_stack_trace(obj: Handle<Exception>) {
    let ctxt = get_ctxt();
    set_exception_backtrace(ctxt, obj, true);
}

pub extern "C" fn stack_element(obj: Handle<Exception>, ind: i32) -> Handle<StackTraceElement> {
    let ctxt = get_ctxt();
    let obj = ctxt.handles.root(obj);
    let array = obj.backtrace;

    let ind = ind as usize * 2;

    let lineno = array.get_at(ind);
    let fct_id = array.get_at(ind + 1);
    let cls_def_id = ctxt.vips.stack_trace_element(ctxt);

    let ste: Handle<StackTraceElement> = alloc(ctxt, cls_def_id).cast();
    let mut ste = ctxt.handles.root(ste);
    ste.line = lineno;

    let jit_fct_id = JitFctId::from(fct_id as usize);
    let jit_fct = ctxt.jit_fcts[jit_fct_id].borrow();
    let fct = ctxt.fcts[jit_fct.fct_id()].borrow();
    let name = fct.full_name(ctxt);
    ste.name = Str::from_buffer(ctxt, name.as_bytes());

    ste.direct()
}

pub fn alloc_exception(ctxt: &SemContext, msg: Handle<Str>) -> Handle<Exception> {
    let cls_id = ctxt.vips.exception(ctxt);
    let obj: Handle<Exception> = alloc(ctxt, cls_id).cast();
    let mut obj = ctxt.handles.root(obj);

    obj.msg = msg;
    set_exception_backtrace(ctxt, obj.direct(), false);

    obj.direct()
}

fn set_exception_backtrace(ctxt: &SemContext, obj: Handle<Exception>, via_retrieve: bool) {
    let stacktrace = stacktrace_from_last_dtn(ctxt);
    let mut obj = ctxt.handles.root(obj);

    let skip = if via_retrieve { 2 } else { 0 };
    let len = stacktrace.len() - skip;

    let cls_id = ctxt.vips.int_array(ctxt);
    let array: Handle<IntArray> = Array::alloc(ctxt, len * 2, 0, cls_id);
    let mut array = ctxt.handles.root(array);
    let mut i = 0;

    // ignore first element of stack trace (ctor of Exception)
    for elem in stacktrace.elems.iter().skip(skip) {
        array.set_at(i, elem.lineno);
        array.set_at(i + 1, elem.fct_id.idx() as i32);

        i += 2;
    }

    obj.backtrace = array.direct();
}
