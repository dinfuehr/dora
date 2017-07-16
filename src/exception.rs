use std::ptr;

use baseline::fct::CatchType;
use baseline::map::CodeData;
use cpu::{get_exception_object, resume_with_handler, fp_from_execstate};
use ctxt::{SemContext, FctKind, FctId, get_ctxt};
use object::{alloc, Array, Exception, Handle, IntArray, Obj, StackTraceElement, Str};
use execstate::ExecState;

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

    pub fn push_entry(&mut self, fct_id: FctId, lineno: i32) {
        self.elems
            .push(StackElem {
                      fct_id: fct_id,
                      lineno: lineno,
                  });
    }

    pub fn dump(&self, ctxt: &SemContext) {
        for (ind, elem) in self.elems.iter().rev().enumerate() {
            let name = ctxt.fcts[elem.fct_id].borrow().full_name(ctxt);
            print!("  {}: {}:", ind, name);

            if elem.lineno == 0 {
                println!("?");
            } else {
                println!("{}", elem.lineno);
            }
        }
    }
}

struct StackElem {
    fct_id: FctId,
    lineno: i32,
}

pub struct DoraToNativeInfo {
    pub last: *const DoraToNativeInfo,
    pub sp: usize,
    pub fp: usize,
    pub ra: usize,
    pub xpc: usize,
}

impl DoraToNativeInfo {
    pub fn new() -> DoraToNativeInfo {
        DoraToNativeInfo {
            last: ptr::null(),
            sp: 0,
            fp: 0,
            ra: 0,
            xpc: 0,
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

        let pc: usize = dtn.ra;
        let fp: usize = dtn.fp;

        frames_from_pc(stacktrace, ctxt, pc, fp);

        dtn_ptr = dtn.last
    }
}

fn frames_from_pc(stacktrace: &mut Stacktrace, ctxt: &SemContext, pc: usize, mut fp: usize) {
    determine_stack_entry(stacktrace, ctxt, pc);

    while fp != 0 {
        let ra = unsafe { *((fp + 8) as *const usize) };
        let cont = determine_stack_entry(stacktrace, ctxt, ra);

        if !cont {
            break;
        }

        fp = unsafe { *(fp as *const usize) };
    }
}

fn determine_stack_entry(stacktrace: &mut Stacktrace, ctxt: &SemContext, pc: usize) -> bool {
    let code_map = ctxt.code_map.lock().unwrap();
    let data = code_map.get(pc as *const u8);

    if data.is_none() {
        return false;
    }

    if let CodeData::Fct(fct_id) = data.unwrap() {
        let mut lineno = 0;
        let fct = ctxt.fcts[fct_id].borrow();
        if let FctKind::Source(ref src) = fct.kind {
            let src = src.borrow();
            let jit_fct = src.jit_fct.read().unwrap();
            let jit_fct = jit_fct.as_ref().expect("fct not compiled yet");
            let offset = pc - (jit_fct.fct_ptr() as usize);
            lineno = jit_fct.lineno_for_offset(offset as i32);

            if lineno == 0 {
                panic!("lineno not found for program point");
            }
        }

        stacktrace.push_entry(fct_id, lineno);

        true
    } else {
        // only continue if we still haven't reached jitted functions
        stacktrace.len() == 0
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

    if data.is_none() {
        return HandlerFound::No;
    }

    if let CodeData::Fct(fct_id) = data.unwrap() {
        let fct = ctxt.fcts[fct_id].borrow();

        if let FctKind::Source(ref src) = fct.kind {
            let src = src.borrow();
            let jit_fct = src.jit_fct.read().unwrap();
            let jit_fct = jit_fct.as_ref().expect("fct not compiled yet");

            let cls_id = exception.header().vtbl().class().id;

            for entry in &jit_fct.exception_handlers {
                // println!("entry = {:x} to {:x} for {:?}",
                //          entry.try_start, entry.try_end, entry.catch_type);

                if entry.try_start < pc && pc <= entry.try_end &&
                   (entry.catch_type == CatchType::Any ||
                    entry.catch_type == CatchType::Class(cls_id)) {
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
            if !fct.ast.throws {
                return HandlerFound::Stop;
            }
        }
    }

    HandlerFound::No
}

pub extern "C" fn retrieve_stack_trace(obj: Handle<Exception>) {
    let ctxt = get_ctxt();
    set_exception_backtrace(ctxt, obj, true);
}

pub extern "C" fn stack_element(obj: Handle<Exception>, ind: i32) -> Handle<StackTraceElement> {
    let ctxt = get_ctxt();
    let array = obj.backtrace;

    let ind = ind as usize * 2;

    let lineno = array.get_at(ind);
    let fct_id = array.get_at(ind + 1);
    let cls_id = ctxt.primitive_classes.stack_trace_element_class;

    let mut obj: Handle<StackTraceElement> = alloc(ctxt, cls_id).cast();
    obj.line = lineno;

    let name = ctxt.fcts[FctId(fct_id as usize)]
        .borrow()
        .full_name(ctxt);
    obj.name = Str::from_buffer(ctxt, name.as_bytes());

    obj
}

pub fn alloc_exception(ctxt: &SemContext, msg: Handle<Str>) -> Handle<Exception> {
    let cls_id = ctxt.primitive_classes.exception_class;
    let mut obj: Handle<Exception> = alloc(ctxt, cls_id).cast();

    obj.msg = msg;
    set_exception_backtrace(ctxt, obj, false);

    obj
}

fn set_exception_backtrace(ctxt: &SemContext, mut obj: Handle<Exception>, via_retrieve: bool) {
    let stacktrace = stacktrace_from_last_dtn(ctxt);

    let skip = if via_retrieve { 1 } else { 0 };
    let len = stacktrace.len() - skip;

    let cls_id = ctxt.primitive_classes.int_array;
    let mut array: Handle<IntArray> = Array::alloc(ctxt, len * 2, 0, cls_id);
    let mut i = 0;

    // ignore first element of stack trace (ctor of Exception)
    for elem in stacktrace.elems.iter().skip(skip) {
        array.set_at(i, elem.lineno);
        array.set_at(i + 1, elem.fct_id.0 as i32);

        i += 2;
    }

    obj.backtrace = array;
}
