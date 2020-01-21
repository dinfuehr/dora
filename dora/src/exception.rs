use std::ptr;

use crate::compiler::fct::{CatchType, JitFctId};
use crate::compiler::map::CodeDescriptor;
use crate::cpu::fp_from_execstate;
use crate::execstate::ExecState;
use crate::gc::Address;
use crate::handle::root;
use crate::object::{alloc, Array, IntArray, Obj, Ref, StackTraceElement, Str, Throwable};
use crate::stdlib;
use crate::threads::THREAD;
use crate::vm::{get_vm, FctParent, Trap, VM};

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
        self.elems.push(StackElem { fct_id, lineno });
    }

    pub fn dump(&self, vm: &VM) {
        let frames = self.elems.len();
        for (ind, elem) in self.elems.iter().enumerate() {
            let jit_fct = vm.jit_fcts.idx(elem.fct_id);
            let fct_id = jit_fct.fct_id();
            let fct = vm.fcts.idx(fct_id);
            let fct = fct.read();
            let name = fct.full_name(vm);
            print!("{}: {}: ", frames - ind, name);

            if elem.lineno == 0 {
                println!("?");
            } else {
                println!("{}", elem.lineno);
            }
        }
    }

    pub fn dump_err(&self, vm: &VM) {
        let frames = self.elems.len();
        for (ind, elem) in self.elems.iter().enumerate() {
            let jit_fct = vm.jit_fcts.idx(elem.fct_id);
            let fct_id = jit_fct.fct_id();
            let fct = vm.fcts.idx(fct_id);
            let fct = fct.read();
            let name = fct.full_name(vm);
            eprint!("{}: {}: ", frames - ind, name);

            if elem.lineno == 0 {
                eprintln!("?");
            } else {
                eprintln!("{}", elem.lineno);
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

    pub fn last_offset() -> i32 {
        offset_of!(DoraToNativeInfo, last) as i32
    }

    pub fn fp_offset() -> i32 {
        offset_of!(DoraToNativeInfo, fp) as i32
    }

    pub fn pc_offset() -> i32 {
        offset_of!(DoraToNativeInfo, pc) as i32
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
    let mut dtn_ptr = THREAD.with(|thread| {
        let thread = thread.borrow();
        let dtn = thread.dtn();

        dtn
    });

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
    let data = code_map.get(pc.into());

    match data {
        Some(CodeDescriptor::DoraFct(fct_id)) => {
            let jit_fct = vm.jit_fcts.idx(fct_id);

            let offset = pc - jit_fct.fct_ptr().to_usize();
            let jit_fct = jit_fct.to_base().expect("baseline expected");
            let position = jit_fct
                .position_for_offset(offset as u32)
                .expect("position not found for program point");

            stacktrace.push_entry(fct_id, position.line as i32);

            true
        }

        Some(CodeDescriptor::NativeStub(fct_id)) => {
            let jit_fct = vm.jit_fcts.idx(fct_id);
            let fct = vm.fcts.idx(jit_fct.fct_id());
            let fct = fct.read();

            stacktrace.push_entry(fct_id, fct.ast.pos.line as i32);

            true
        }

        Some(CodeDescriptor::TrapStub) => true,
        Some(CodeDescriptor::ThrowStub) => true,
        Some(CodeDescriptor::AllocStub) => true,
        Some(CodeDescriptor::DoraStub) => false,

        _ => {
            println!("data = {:?}, pc = {:x}", data, pc);
            code_map.dump(vm);
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

pub extern "C" fn throw(resume: &mut ThrowResume) {
    let vm = get_vm();

    let exception: Ref<Obj> = THREAD.with(|thread| {
        let thread = thread.borrow();
        thread.tld.exception_object().into()
    });

    let dtn = THREAD.with(|thread| {
        let thread = thread.borrow();
        let dtn = thread.dtn();

        dtn
    });
    let dtn = unsafe { &*dtn };

    let mut pc: usize = dtn.pc;
    let mut fp: usize = dtn.fp;

    while fp != 0 {
        let res = find_handler(vm, exception, pc, fp, resume);

        match res {
            HandlerFound::Yes => {
                // handler found, resume from there
                THREAD.with(|thread| {
                    let thread = thread.borrow();
                    thread.tld.set_exception_object(Address::null());
                });

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
    exception: Ref<Obj>,
    pc: usize,
    fp: usize,
    resume: &mut ThrowResume,
) -> HandlerFound {
    let data = {
        let code_map = vm.code_map.lock();
        code_map.get(pc.into())
    };

    match data {
        Some(CodeDescriptor::DoraFct(fct_id)) | Some(CodeDescriptor::NativeStub(fct_id)) => {
            let jit_fct = vm.jit_fcts.idx(fct_id);
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

        Some(CodeDescriptor::DoraStub) => HandlerFound::Stop,
        Some(CodeDescriptor::ThrowStub) => HandlerFound::No,

        _ => {
            println!("data = {:?}", data);
            panic!("invalid stack frame");
        }
    }
}

pub extern "C" fn retrieve_stack_trace(obj: Ref<Throwable>) {
    let vm = get_vm();
    set_exception_backtrace(vm, obj, true);
}

pub extern "C" fn stack_element(obj: Ref<Throwable>, ind: i32) -> Ref<StackTraceElement> {
    let vm = get_vm();
    let obj = root(obj);
    let array = obj.backtrace;

    let ind = ind as usize * 2;

    let lineno = array.get_at(ind);
    let fct_id = array.get_at(ind + 1);
    let cls_def_id = vm.vips.stack_trace_element(vm);

    let ste: Ref<StackTraceElement> = alloc(vm, cls_def_id).cast();
    let mut ste = root(ste);
    ste.line = lineno;

    let jit_fct_id = JitFctId::from(fct_id as usize);
    let jit_fct = vm.jit_fcts.idx(jit_fct_id);
    let fct = vm.fcts.idx(jit_fct.fct_id());
    let fct = fct.read();
    let name = fct.full_name(vm);
    ste.name = Str::from_buffer(vm, name.as_bytes());

    ste.direct()
}

pub fn alloc_exception(vm: &VM, msg: Ref<Str>) -> Ref<Throwable> {
    let cls_id = vm.vips.exception(vm);
    let obj: Ref<Throwable> = alloc(vm, cls_id).cast();
    let mut obj = root(obj);

    obj.msg = msg;
    set_exception_backtrace(vm, obj.direct(), false);

    obj.direct()
}

fn set_exception_backtrace(vm: &VM, obj: Ref<Throwable>, via_retrieve: bool) {
    let stacktrace = stacktrace_from_last_dtn(vm);
    let mut obj = root(obj);
    let mut skip = 0;

    let mut skip_retrieve_stack = false;
    let mut skip_constructor = false;

    // ignore every element until first not inside susubclass of Throwable (ctor of Exception)
    if via_retrieve {
        for elem in stacktrace.elems.iter() {
            let jit_fct_id = JitFctId::from(elem.fct_id.idx() as usize);
            let jit_fct = vm.jit_fcts.idx(jit_fct_id);
            let fct_id = jit_fct.fct_id();
            let fct = vm.fcts.idx(fct_id);
            let fct = fct.read();

            if !skip_retrieve_stack {
                let throwable_cls = vm.classes.idx(vm.vips.throwable_class);
                let throwable_cls = throwable_cls.read();
                let retrieve_stacktrace_fct_id = throwable_cls
                    .find_method(vm, vm.interner.intern("retrieveStackTrace"), false)
                    .expect("retrieveStackTrace not found in Throwable");

                if retrieve_stacktrace_fct_id == fct_id {
                    skip += 1;
                    continue;
                } else {
                    skip_retrieve_stack = true;
                }
            }

            if !skip_constructor {
                assert!(skip_retrieve_stack);
                if let FctParent::Class(owner_class) = fct.parent {
                    if fct.is_constructor {
                        let throw_object_cls_id = (&obj.header)
                            .vtbl()
                            .class()
                            .cls_id
                            .expect("no corresponding class");

                        if throw_object_cls_id == owner_class {
                            skip += 1;
                            skip_constructor = true;
                            break;
                        }

                        let throw_object_cls = vm.classes.idx(throw_object_cls_id);
                        let throw_object_cls = throw_object_cls.read();

                        if throw_object_cls.subclass_from(vm, owner_class) {
                            skip += 1;
                            continue;
                        }
                    } else {
                        skip_constructor = true;
                        break;
                    }
                } else {
                    skip_constructor = true;
                    break;
                }
            }
        }
        assert!(skip_constructor);
    }

    let len = stacktrace.len() - skip;

    let cls_id = vm.vips.int_array(vm);
    let array: Ref<IntArray> = Array::alloc(vm, len * 2, 0, cls_id);
    let mut array = root(array);
    let mut i = 0;

    for elem in stacktrace.elems.iter().skip(skip) {
        array.set_at(i, elem.lineno);
        array.set_at(i + 1, elem.fct_id.idx() as i32);
        i += 2;
    }
    obj.backtrace = array.direct();
}
