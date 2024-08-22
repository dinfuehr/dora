use dora_bytecode::{FunctionId, Location};

use crate::handle::{create_handle, Handle};
use crate::object::{Array, Int32Array, Ref, Stacktrace, StacktraceIterator, Str};
use crate::threads::current_thread;
use crate::vm::{
    display_fct, get_vm, Code, CodeId, CodeKind, InlinedFunctionId, InlinedLocation, VM,
};

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

    pub fn push_entry(&mut self, code_id: CodeId, offset: u32) {
        self.elems.push(StackElem { code_id, offset });
    }

    pub fn dump(&self, vm: &VM, w: &mut (impl std::io::Write + ?Sized)) -> std::io::Result<()> {
        for elem in &self.elems {
            let code = vm.code_objects.get(elem.code_id);
            let fct_id = code.fct_id();
            let fct = vm.fct(fct_id);
            let location = code.location_for_offset(elem.offset);
            let location = match location {
                Some(mut inlined_location) => {
                    while inlined_location.is_inlined() {
                        let inlined_function =
                            code.inlined_function(inlined_location.inlined_function_id());
                        let fct = vm.fct(inlined_function.fct_id);
                        let fct_name = display_fct(vm, inlined_function.fct_id);
                        let file = &vm.file(fct.file_id).path;
                        writeln!(
                            w,
                            "    {} ({}:{})",
                            fct_name, file, inlined_location.location
                        )?;
                        inlined_location = inlined_function.inlined_location.clone();
                    }

                    inlined_location.location
                }

                None => fct.loc,
            };

            let file = &vm.file(fct.file_id).path;
            let fct_name = display_fct(vm, fct_id);
            writeln!(w, "    {} ({}:{})", fct_name, file, location)?;
        }

        Ok(())
    }
}

struct StackElem {
    code_id: CodeId,
    offset: u32,
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
            CodeKind::BaselineFct(_) | CodeKind::OptimizedFct(_) => {
                let offset = pc - code.instruction_start().to_usize();
                stacktrace.push_entry(code_id, offset as u32);

                true
            }

            CodeKind::RuntimeEntryTrampoline(_) => {
                stacktrace.push_entry(code_id, 0);

                true
            }

            CodeKind::TrapTrampoline => true,
            CodeKind::StackOverflowTrampoline => true,
            CodeKind::LazyCompilationStub => true,
            CodeKind::AllocationFailureTrampoline => true,
            CodeKind::DoraEntryTrampoline => false,

            CodeKind::SafepointTrampoline => unreachable!(),
        }
    } else {
        println!("no code found at pc = {:x}", pc);
        vm.code_map.dump(vm);
        panic!("invalid stack frame");
    }
}

pub extern "C" fn capture_stack_trace(mut obj: Handle<Stacktrace>) {
    let vm = get_vm();
    let stacktrace = stacktrace_from_last_dtn(vm);

    let cls_id = vm.int_array();
    let array: Ref<Int32Array> = Array::alloc(vm, stacktrace.len() * 2, 0, cls_id);
    let mut array = create_handle(array);
    let mut i = 0;

    for elem in &stacktrace.elems {
        array.set_at(i, elem.code_id.idx() as i32);
        array.set_at(i + 1, elem.offset as i32);
        i += 2;
    }
    obj.backtrace = array.direct();
}

pub extern "C" fn symbolize_stack_trace_element(mut obj: Handle<StacktraceIterator>) {
    let vm = get_vm();

    let code_id: CodeId = (obj.code_id as usize).into();
    let code = vm.code_objects.get(code_id);

    let (fct_id, location, next_inlined_function_id) = if obj.inlined_function_id == -1 {
        let offset = obj.offset as u32;

        match code.location_for_offset(offset) {
            Some(inlined_location) => destruct_inlined_location(&*code, inlined_location),

            None => {
                let fct_id = code.fct_id();
                let fct = vm.fct(fct_id);

                (code.fct_id(), fct.loc, FINAL_INLINED_FUNCTION_ID)
            }
        }
    } else {
        let id = InlinedFunctionId(obj.inlined_function_id as u32);
        let inlined_location = code.inlined_function(id).inlined_location.clone();

        destruct_inlined_location(&*code, inlined_location)
    };

    let fct = vm.fct(fct_id);
    let file = &vm.file(fct.file_id).path;
    let fct_name = display_fct(vm, fct_id);

    let text = format!("{} ({}:{})", fct_name, file, location);
    obj.text = Str::from_buffer(vm, text.as_bytes());
    obj.inlined_function_id = next_inlined_function_id.0 as i32;
}

fn destruct_inlined_location(
    code: &Code,
    inlined_location: InlinedLocation,
) -> (FunctionId, Location, InlinedFunctionId) {
    if inlined_location.is_inlined() {
        let id = inlined_location.inlined_function_id();
        let inlined_function = code.inlined_function(id);

        (
            inlined_function.fct_id,
            inlined_location.location,
            inlined_location
                .inlined_function_id
                .unwrap_or(FINAL_INLINED_FUNCTION_ID),
        )
    } else {
        (
            code.fct_id(),
            inlined_location.location,
            FINAL_INLINED_FUNCTION_ID,
        )
    }
}

const FINAL_INLINED_FUNCTION_ID: InlinedFunctionId = InlinedFunctionId(u32::MAX);
