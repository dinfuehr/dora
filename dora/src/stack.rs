use std::ptr;

use crate::handle::{handle, Handle};
use crate::object::{alloc, Array, Int32Array, Ref, Stacktrace, StacktraceElement, Str};
use crate::threads::current_thread;
use crate::vm::{display_fct, get_vm, CodeId, CodeKind, VM};

pub struct NativeStacktrace {
    elems: Vec<StackElem>,
}

impl NativeStacktrace {
    pub fn new() -> NativeStacktrace {
        NativeStacktrace { elems: Vec::new() }
    }

    pub fn len(&self) -> usize {
        self.elems.len()
    }

    pub fn push_entry(&mut self, fct_id: CodeId, lineno: u32) {
        self.elems.push(StackElem { fct_id, lineno });
    }

    pub fn dump(&self, vm: &VM, w: &mut (impl std::io::Write + ?Sized)) -> std::io::Result<()> {
        for elem in &self.elems {
            let code = vm.code_objects.get(elem.fct_id);
            let fct_id = code.fct_id();
            let fct = vm.fcts.idx(fct_id);
            let fct = fct.read();
            let fct_name = display_fct(vm, fct_id);
            let file = &vm.source_file(fct.file_id).path;
            let lineno = if elem.lineno == 0 {
                fct.pos.line
            } else {
                elem.lineno
            };
            writeln!(w, "    {} ({}:{})", fct_name, file.display(), lineno)?;
        }

        Ok(())
    }
}

struct StackElem {
    fct_id: CodeId,
    lineno: u32,
}

#[repr(C)]
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

pub fn stacktrace_from_last_dtn(vm: &VM) -> NativeStacktrace {
    let mut stacktrace = NativeStacktrace::new();
    frames_from_dtns(&mut stacktrace, vm);
    return stacktrace;
}

fn frames_from_dtns(stacktrace: &mut NativeStacktrace, vm: &VM) {
    let mut dtn_ptr = current_thread().dtn();

    while !dtn_ptr.is_null() {
        let dtn = unsafe { &*dtn_ptr };

        let pc: usize = dtn.pc;
        let fp: usize = dtn.fp;

        frames_from_pc(stacktrace, vm, pc, fp);

        dtn_ptr = dtn.last
    }
}

fn frames_from_pc(stacktrace: &mut NativeStacktrace, vm: &VM, pc: usize, mut fp: usize) {
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

fn determine_stack_entry(stacktrace: &mut NativeStacktrace, vm: &VM, pc: usize) -> bool {
    let code_id = vm.code_map.get(pc.into());

    if let Some(code_id) = code_id {
        let code = vm.code_objects.get(code_id);
        match code.descriptor() {
            CodeKind::DoraFct(_) => {
                let offset = pc - code.instruction_start().to_usize();
                let position = code
                    .position_for_offset(offset as u32)
                    .expect("position not found for program point");

                stacktrace.push_entry(code_id, position.line);

                true
            }

            CodeKind::NativeStub(fct_id) => {
                let fct = vm.fcts.idx(fct_id);
                let fct = fct.read();

                stacktrace.push_entry(code_id, fct.ast.pos.line);

                true
            }

            CodeKind::TrapStub => true,
            CodeKind::GuardCheckStub => true,
            CodeKind::CompileStub => true,
            CodeKind::AllocStub => true,
            CodeKind::DoraStub => false,

            CodeKind::VerifyStub | CodeKind::SafepointStub => unreachable!(),
        }
    } else {
        println!("no code found at pc = {:x}", pc);
        vm.code_map.dump(vm);
        panic!("invalid stack frame");
    }
}

pub extern "C" fn retrieve_stack_trace(obj: Handle<Stacktrace>) {
    let vm = get_vm();
    set_backtrace(vm, obj, true);
}

pub extern "C" fn stack_element(obj: Handle<Stacktrace>, ind: i32) -> Ref<StacktraceElement> {
    let vm = get_vm();
    let array = obj.backtrace;

    let ind = ind as usize * 2;

    let lineno = array.get_at(ind);
    let fct_id = array.get_at(ind + 1);
    let cls_def_id = vm.stack_trace_element();

    let ste: Ref<StacktraceElement> = alloc(vm, cls_def_id).cast();
    let mut ste = handle(ste);
    ste.line = lineno;

    let code_id: CodeId = (fct_id as usize).into();
    let code = vm.code_objects.get(code_id);
    let name = display_fct(vm, code.fct_id());
    ste.name = Str::from_buffer(vm, name.as_bytes());

    ste.direct()
}

fn set_backtrace(vm: &VM, mut obj: Handle<Stacktrace>, via_retrieve: bool) {
    let stacktrace = stacktrace_from_last_dtn(vm);
    let mut skip = 0;

    let mut skip_retrieve_stack = false;

    // ignore every element until first not inside susubclass of Stacktrace (ctor of Exception)
    if via_retrieve {
        for elem in stacktrace.elems.iter() {
            let code_id = elem.fct_id.idx().into();
            let code = vm.code_objects.get(code_id);
            let fct_id = code.fct_id();

            if !skip_retrieve_stack {
                let retrieve_stacktrace_fct_id = vm.known.functions.stacktrace_retrieve();

                if retrieve_stacktrace_fct_id == fct_id {
                    skip += 2;
                    continue;
                } else {
                    skip_retrieve_stack = true;
                }
            }
        }
    }

    let len = stacktrace.len() - skip;

    let cls_id = vm.int_array();
    let array: Ref<Int32Array> = Array::alloc(vm, len * 2, 0, cls_id);
    let mut array = handle(array);
    let mut i = 0;

    for elem in stacktrace.elems.iter().skip(skip) {
        array.set_at(i, elem.lineno as i32);
        array.set_at(i + 1, elem.fct_id.idx() as i32);
        i += 2;
    }
    obj.backtrace = array.direct();
}
