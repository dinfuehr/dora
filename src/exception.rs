use std::ptr;

use baseline::fct::{CatchType, JitFctId};
use baseline::map::CodeDescriptor;
use cpu::fp_from_execstate;
use ctxt::{get_vm, VM};
use execstate::ExecState;
use object::{alloc, Array, Exception, Handle, IntArray, Obj, StackTraceElement, Str};
use os::signal::Trap;
use stdlib;

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

    pub fn dump(&self, vm: &VM) {
        for (ind, elem) in self.elems.iter().enumerate() {
            let jit_fct = vm.jit_fcts.idx(elem.fct_id);
            let jit_fct = jit_fct.lock();
            let fct_id = jit_fct.fct_id();
            let fct = vm.fcts[fct_id].borrow();
            let name = fct.full_name(vm);
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

pub fn stacktrace_from_es(vm: &VM, es: &ExecState) -> Stacktrace {
    let mut stacktrace = Stacktrace::new();
    let fp = fp_from_execstate(es);
    frames_from_pc(&mut stacktrace, vm, es.pc, fp);
    frames_from_dtns(&mut stacktrace, vm);
    return stacktrace;
}

pub fn stacktrace_from_last_dtn(vm: &VM) -> Stacktrace {
    let mut stacktrace = Stacktrace::new();
    frames_from_dtns(&mut stacktrace, vm);
    return stacktrace;
}

fn frames_from_dtns(stacktrace: &mut Stacktrace, vm: &VM) {
    let mut dtn_ptr = *vm.dtn.borrow();

    while !dtn_ptr.is_null() {
        let dtn = unsafe { &*dtn_ptr };

        let pc: usize = dtn.pc;
        let fp: usize = dtn.fp;

        frames_from_pc(stacktrace, vm, pc, fp);

        dtn_ptr = dtn.last
    }
}

fn frames_from_pc(stacktrace: &mut Stacktrace, vm: &VM, pc: usize, mut fp: usize) {
    if !determine_stack_entry(stacktrace, vm, pc) {
        return;
    }

    while fp != 0 {
        let ra = unsafe { *((fp + 8) as *const usize) };

        if !determine_stack_entry(stacktrace, vm, ra) {
            return;
        }

        fp = unsafe { *(fp as *const usize) };
    }
}

fn determine_stack_entry(stacktrace: &mut Stacktrace, vm: &VM, pc: usize) -> bool {
    let code_map = vm.code_map.lock();
    let data = code_map.get(pc as *const u8);

    match data {
        Some(CodeDescriptor::DoraFct(fct_id)) => {
            let jit_fct = vm.jit_fcts.idx(fct_id);
            let jit_fct = jit_fct.lock();

            let offset = pc - (jit_fct.fct_ptr() as usize);
            let jit_fct = jit_fct.to_base().expect("baseline expected");
            let lineno = jit_fct.lineno_for_offset(offset as i32);

            if lineno == 0 {
                panic!("lineno not found for program point");
            }

            stacktrace.push_entry(fct_id, lineno);

            true
        }

        Some(CodeDescriptor::NativeThunk(fct_id)) => {
            let jit_fct = vm.jit_fcts.idx(fct_id);
            let jit_fct = jit_fct.lock();
            let fct = vm.fcts[jit_fct.fct_id()].borrow();

            stacktrace.push_entry(fct_id, fct.ast.pos.line as i32);

            true
        }

        Some(CodeDescriptor::TrapThunk) => true,
        Some(CodeDescriptor::ThrowThunk) => true,
        Some(CodeDescriptor::DoraEntry) => false,

        _ => {
            println!("data = {:?}", data);
            panic!("invalid stack frame");
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
enum HandlerFound {
    Yes,
    No,
    Stop,
}

pub struct ThrowResume {
    pc: usize,
    sp: usize,
    fp: usize,
}

pub extern "C" fn throw(exception: Handle<Obj>, resume: &mut ThrowResume) {
    let vm = get_vm();

    let dtn = unsafe { &**vm.dtn.borrow() };

    let mut pc: usize = dtn.pc;
    let mut fp: usize = dtn.fp;

    while fp != 0 {
        let res = find_handler(vm, exception, pc, fp, resume);

        match res {
            HandlerFound::Yes => {
                // handler found, resume from there
                return;
            }

            HandlerFound::Stop => {
                // no handler found
                stdlib::trap(Trap::THROW.int());
            }

            HandlerFound::No => {
                // try next stack frame
            }
        }

        pc = unsafe { *((fp + 8) as *const usize) };
        fp = unsafe { *(fp as *const usize) };
    }
}

fn find_handler(
    vm: &VM,
    exception: Handle<Obj>,
    pc: usize,
    fp: usize,
    resume: &mut ThrowResume,
) -> HandlerFound {
    let data = {
        let code_map = vm.code_map.lock();
        code_map.get(pc as *const u8)
    };

    match data {
        Some(CodeDescriptor::DoraFct(fct_id)) | Some(CodeDescriptor::NativeThunk(fct_id)) => {
            let jit_fct = vm.jit_fcts.idx(fct_id);
            let jit_fct = jit_fct.lock();
            let jit_fct = jit_fct.to_base().expect("baseline expected");
            let clsptr = exception.header().vtbl().classptr();

            for entry in &jit_fct.exception_handlers {
                // println!("entry = {:x} to {:x} for {:?}",
                //          entry.try_start, entry.try_end, entry.catch_type);

                if entry.try_start < pc
                    && pc <= entry.try_end
                    && (entry.catch_type == CatchType::Any
                        || entry.catch_type == CatchType::Class(clsptr))
                {
                    let stacksize = jit_fct.framesize as usize;

                    if let Some(offset) = entry.offset {
                        let arg = (fp as isize + offset as isize) as usize;

                        unsafe {
                            *(arg as *mut usize) = exception.raw() as usize;
                        }
                    }

                    resume.pc = entry.catch;
                    resume.sp = fp - stacksize;
                    resume.fp = fp;

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

            HandlerFound::No
        }

        Some(CodeDescriptor::DoraEntry) => HandlerFound::Stop,
        Some(CodeDescriptor::ThrowThunk) => HandlerFound::No,

        _ => {
            println!("data = {:?}", data);
            panic!("invalid stack frame");
        }
    }
}

pub extern "C" fn retrieve_stack_trace(obj: Handle<Exception>) {
    let vm = get_vm();
    set_exception_backtrace(vm, obj, true);
}

pub extern "C" fn stack_element(obj: Handle<Exception>, ind: i32) -> Handle<StackTraceElement> {
    let vm = get_vm();
    let obj = vm.handles.root(obj);
    let array = obj.backtrace;

    let ind = ind as usize * 2;

    let lineno = array.get_at(ind);
    let fct_id = array.get_at(ind + 1);
    let cls_def_id = vm.vips.stack_trace_element(vm);

    let ste: Handle<StackTraceElement> = alloc(vm, cls_def_id).cast();
    let mut ste = vm.handles.root(ste);
    ste.line = lineno;

    let jit_fct_id = JitFctId::from(fct_id as usize);
    let jit_fct = vm.jit_fcts.idx(jit_fct_id);
    let jit_fct = jit_fct.lock();
    let fct = vm.fcts[jit_fct.fct_id()].borrow();
    let name = fct.full_name(vm);
    ste.name = Str::from_buffer(vm, name.as_bytes());

    ste.direct()
}

pub fn alloc_exception(vm: &VM, msg: Handle<Str>) -> Handle<Exception> {
    let cls_id = vm.vips.exception(vm);
    let obj: Handle<Exception> = alloc(vm, cls_id).cast();
    let mut obj = vm.handles.root(obj);

    obj.msg = msg;
    set_exception_backtrace(vm, obj.direct(), false);

    obj.direct()
}

fn set_exception_backtrace(vm: &VM, obj: Handle<Exception>, via_retrieve: bool) {
    let stacktrace = stacktrace_from_last_dtn(vm);
    let mut obj = vm.handles.root(obj);

    let skip = if via_retrieve { 2 } else { 0 };
    let len = stacktrace.len() - skip;

    let cls_id = vm.vips.int_array(vm);
    let array: Handle<IntArray> = Array::alloc(vm, len * 2, 0, cls_id);
    let mut array = vm.handles.root(array);
    let mut i = 0;

    // ignore first element of stack trace (ctor of Exception)
    for elem in stacktrace.elems.iter().skip(skip) {
        array.set_at(i, elem.lineno);
        array.set_at(i + 1, elem.fct_id.idx() as i32);

        i += 2;
    }

    obj.backtrace = array.direct();
}
