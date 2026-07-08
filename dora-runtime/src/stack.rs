use std::io::{BufWriter, Write};

use dora_runtime_macros::dora_native;

use dora_bytecode::Location;

use crate::handle::{Handle, create_handle};
use crate::mirror::{Array, Int32Array, Ref, Stacktrace, StacktraceIterator, Str};
use crate::runtime::{
    Code, CodeId, CodeKind, FunctionInfoAot, InlinedFunctionId, InlinedLocation, Runtime,
    get_runtime,
};
use crate::threads::current_thread;

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

    pub fn dump_to_stderr(&self, rt: &Runtime) {
        let stderr = std::io::stderr();
        let stderr = stderr.lock();
        // stderr() is not buffered, create BufWriter for it.
        let mut writer = BufWriter::new(stderr);
        self.dump(rt, &mut writer).expect("output broken");
    }

    pub fn dump<W>(&self, rt: &Runtime, w: &mut W) -> std::io::Result<()>
    where
        W: Write,
    {
        for elem in &self.elems {
            let code = rt.code_map.get_code(elem.code_id);
            dump_stack_elem(w, code, elem.offset)?;
        }

        Ok(())
    }
}

fn dump_stack_elem<W>(w: &mut W, code: &Code, offset: u32) -> std::io::Result<()>
where
    W: Write,
{
    let function_info = code.function_info();
    let location = match code.location_for_offset(offset) {
        Some(mut inlined_location) => {
            while inlined_location.is_inlined() {
                let inlined_function =
                    code.inlined_function(inlined_location.inlined_function_id());
                writeln!(
                    w,
                    "    {} ({}:{})",
                    inlined_function.function_info.name,
                    inlined_function.function_info.file,
                    inlined_location.location
                )?;
                inlined_location = inlined_function.inlined_location.clone();
            }

            inlined_location.location
        }

        None => function_info.loc,
    };

    writeln!(
        w,
        "    {} ({}:{})",
        function_info.name, function_info.file, location
    )
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

pub fn stacktrace_from_last_dtn(rt: &Runtime) -> NativeStacktrace {
    let mut stacktrace = NativeStacktrace::new();
    frames_from_dtns(&mut stacktrace, rt);
    return stacktrace;
}

fn frames_from_dtns(stacktrace: &mut NativeStacktrace, rt: &Runtime) {
    let mut dtn_ptr = current_thread().dtn();

    while !dtn_ptr.is_null() {
        let dtn = unsafe { &*dtn_ptr };

        let pc: usize = dtn.pc;
        let fp: usize = dtn.fp;

        frames_from_pc(stacktrace, rt, pc, fp);

        dtn_ptr = dtn.last
    }
}

fn frames_from_pc(stacktrace: &mut NativeStacktrace, rt: &Runtime, pc: usize, mut fp: usize) {
    if !determine_stack_entry(stacktrace, rt, pc) {
        return;
    }

    while fp != 0 {
        let ra = unsafe { *((fp + 8) as *const usize) };

        if !determine_stack_entry(stacktrace, rt, ra) {
            return;
        }

        fp = unsafe { *(fp as *const usize) };
    }
}

fn determine_stack_entry(stacktrace: &mut NativeStacktrace, rt: &Runtime, pc: usize) -> bool {
    let code_id = rt.code_map.get(pc.into());

    if let Some(code_id) = code_id {
        let code = rt.code_map.get_code(code_id);
        match code.descriptor() {
            CodeKind::OptimizedFct(_) => {
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
            CodeKind::AllocationFailureTrampoline => true,
            CodeKind::UnreachableTrampoline => true,
            CodeKind::FatalErrorTrampoline => true,
            CodeKind::DoraEntryTrampoline => false,

            CodeKind::SafepointTrampoline => unreachable!(),
        }
    } else {
        println!("no code found at pc = {:x}", pc);
        rt.code_map.dump(rt);
        panic!("invalid stack frame");
    }
}

#[dora_native("std::Stacktrace#capture")]
pub extern "C" fn capture_stack_trace(mut obj: Handle<Stacktrace>) {
    let rt = get_runtime();
    let stacktrace = stacktrace_from_last_dtn(rt);

    let array: Ref<Int32Array> =
        Array::alloc(rt, stacktrace.len() * 2, 0, rt.known.int32_array_shape());
    let mut array = create_handle(array);
    let mut i = 0;

    for elem in &stacktrace.elems {
        array.set_at(i, elem.code_id.idx() as i32);
        array.set_at(i + 1, elem.offset as i32);
        i += 2;
    }
    obj.backtrace = array.direct();
}

#[dora_native("std::symbolize_stacktrace_element")]
pub extern "C" fn symbolize_stack_trace_element(mut obj: Handle<StacktraceIterator>) {
    let rt = get_runtime();

    let code_id: CodeId = (obj.code_id as usize).into();
    let code = rt.code_map.get_code(code_id);

    let (function_info, location, next_inlined_function_id) = if obj.inlined_function_id == -1 {
        let offset = obj.offset as u32;

        match code.location_for_offset(offset) {
            Some(inlined_location) => destruct_inlined_location(code, inlined_location),

            None => {
                let function_info = code.function_info();
                (function_info, function_info.loc, FINAL_INLINED_FUNCTION_ID)
            }
        }
    } else {
        let id = InlinedFunctionId(obj.inlined_function_id as u32);
        let inlined_location = code.inlined_function(id).inlined_location.clone();

        destruct_inlined_location(code, inlined_location)
    };

    let text = format!(
        "{} ({}:{})",
        function_info.name, function_info.file, location
    );
    obj.text = Str::from_buffer(rt, text.as_bytes());
    obj.inlined_function_id = next_inlined_function_id.0 as i32;
}

fn destruct_inlined_location(
    code: &Code,
    inlined_location: InlinedLocation,
) -> (&FunctionInfoAot, Location, InlinedFunctionId) {
    if inlined_location.is_inlined() {
        let id = inlined_location.inlined_function_id();
        let inlined_function = code.inlined_function(id);

        (
            &inlined_function.function_info,
            inlined_location.location,
            inlined_location
                .inlined_function_id
                .unwrap_or(FINAL_INLINED_FUNCTION_ID),
        )
    } else {
        (
            code.function_info(),
            inlined_location.location,
            FINAL_INLINED_FUNCTION_ID,
        )
    }
}

const FINAL_INLINED_FUNCTION_ID: InlinedFunctionId = InlinedFunctionId(u32::MAX);
