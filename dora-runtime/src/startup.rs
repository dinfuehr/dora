use crate::Address;
use crate::mirror::Str;
use crate::shape::Shape;
use crate::threads::current_thread;
use crate::vm::{
    CodeKind, FunctionInfoAot, GcPoint, GcPointTable, InlinedFunctionAot, InlinedFunctionId,
    InlinedLocation, LocationTable, VM, install_external_code_stub,
};
use dora_bytecode::{FunctionId, Location};
pub use dora_compiler::{
    AOT_CODE_KIND_ALLOCATION_FAILURE_TRAMPOLINE, AOT_CODE_KIND_DORA_ENTRY_TRAMPOLINE,
    AOT_CODE_KIND_FATAL_ERROR_TRAMPOLINE, AOT_CODE_KIND_OPTIMIZED,
    AOT_CODE_KIND_RUNTIME_ENTRY_TRAMPOLINE, AOT_CODE_KIND_SAFEPOINT_TRAMPOLINE,
    AOT_CODE_KIND_STACK_OVERFLOW_TRAMPOLINE, AOT_CODE_KIND_TRAP_TRAMPOLINE,
    AOT_CODE_KIND_UNREACHABLE_TRAMPOLINE, encode_shape_kind,
};
use std::{slice, str};

#[repr(C)]
/// Entry type for the `.dora.strings` metadata section.
pub struct AotStringEntry {
    /// Pointer to the UTF-8 payload bytes in `.rodata`.
    pub data_ptr: *const u8,
    /// Length of the UTF-8 payload in bytes.
    pub len: u64,
}

#[repr(C)]
/// Entry type for the `.dora.string_slots` metadata section.
pub struct AotStringSlotEntry {
    /// Address of the writable slot to patch with the heap string pointer.
    pub slot_ptr: *mut usize,
    /// Index into the `.dora.strings` table (`AotStringEntry` array).
    pub string_idx: u32,
    /// Reserved for alignment/forward compatibility.
    pub _reserved: u32,
}

#[repr(C)]
/// Entry type for the `.dora.known_shapes` metadata section.
pub struct AotKnownShapeEntry {
    /// Encoded known-shape slot kind (byte array, string, code, ...).
    pub kind: u32,
    /// Index into the `.dora.shape_offsets` table.
    pub shape_id: u32,
}

#[repr(C)]
/// Entry type for the `.dora.functions` metadata section.
pub struct AotFunctionEntry {
    /// Pointer to the first instruction byte of this code object.
    pub code_start: *const u8,
    /// Pointer one past the last instruction byte of this code object.
    pub code_end: *const u8,
    /// Function id (`FunctionId`) for function-like code kinds.
    pub fct_id: u32,
    /// Encoded AOT code kind (optimized/runtime-entry/dora-entry).
    pub kind: u32,
    /// Index into `.dora.function_info` (`AotFunctionInfoEntry` array).
    pub function_info_idx: u32,
    /// Start index into `.dora.gcpoints` (`AotGcPointEntry` array).
    pub gcpoints_start: u32,
    /// Number of gcpoint entries in `.dora.gcpoints` for this function.
    pub gcpoints_len: u32,
    /// Start index into `.dora.locations` (`AotLocationEntry` array).
    pub locations_start: u32,
    /// Number of location entries in `.dora.locations` for this function.
    pub locations_len: u32,
    /// Start index into `.dora.inlined_functions` (`AotInlinedFunctionEntry` array).
    pub inlined_functions_start: u32,
    /// Number of inlined-function entries for this function.
    pub inlined_functions_len: u32,
    /// Reserved padding to keep the table entry stride 8-byte aligned.
    pub _padding: u32,
}

#[repr(C)]
/// Entry type for the `.dora.tests` metadata section.
pub struct AotTestEntry {
    /// Pointer to the first instruction byte of the compiled test function.
    pub code_start: *const u8,
    /// Function id (`FunctionId`) for the test function.
    pub fct_id: u32,
    /// Reserved padding to keep the table entry stride 8-byte aligned.
    pub _reserved: u32,
}

#[repr(C)]
/// Entry type for the `.dora.gcpoints` metadata section.
pub struct AotGcPointEntry {
    /// Program counter offset inside the function.
    pub pc_offset: u32,
    /// Start index into `.dora.gcpoint_offsets` (flat i32 offsets table).
    pub offsets_start: u32,
    /// Number of stack-slot offsets for this gcpoint.
    pub offsets_len: u32,
}

#[repr(C)]
/// Entry type for the `.dora.function_info` metadata section.
pub struct AotFunctionInfoEntry {
    /// Index into `.dora.strings` for the function name.
    pub name_idx: u32,
    /// Index into `.dora.strings` for the source-file path.
    pub file_idx: u32,
    /// Default source line for code offsets without a location entry.
    pub line: u32,
    /// Default source column for code offsets without a location entry.
    pub column: u32,
}

#[repr(C)]
/// Entry type for the `.dora.locations` metadata section.
pub struct AotLocationEntry {
    /// Program counter offset inside the function.
    pub pc_offset: u32,
    /// Inlined function id, or `u32::MAX` when this location is not inlined.
    pub inlined_function_id: u32,
    /// Source line.
    pub line: u32,
    /// Source column.
    pub column: u32,
}

#[repr(C)]
/// Entry type for the `.dora.inlined_functions` metadata section.
pub struct AotInlinedFunctionEntry {
    /// Index into `.dora.function_info` (`AotFunctionInfoEntry` array).
    pub function_info_idx: u32,
    /// Parent inlined function id, or `u32::MAX` when the caller is not inlined.
    pub inlined_function_id: u32,
    /// Source line of the inlining call site.
    pub line: u32,
    /// Source column of the inlining call site.
    pub column: u32,
}

pub fn initialize_shapes(
    vm: &mut VM,
    shape_base: *const Shape,
    shape_size: usize,
    shape_offsets: &[u32],
    known_shape_entries: &[AotKnownShapeEntry],
) {
    let shape_base = Address::from_ptr(shape_base);
    assert!(
        shape_base.is_non_null(),
        "missing AOT shape descriptor base"
    );
    assert_eq!(
        shape_base.to_usize() % std::mem::align_of::<Shape>(),
        0,
        "AOT shape descriptor base must be Shape-aligned"
    );
    assert!(shape_size > 0, "empty AOT shape descriptor section");
    vm.set_shape_space(shape_base, shape_size);

    for (idx, &offset) in shape_offsets.iter().enumerate() {
        let offset = offset as usize;
        assert!(
            offset < shape_size,
            "invalid AOT shape offset {} for shape {} ({} bytes available)",
            offset,
            idx,
            shape_size
        );
        assert_eq!(
            offset % std::mem::align_of::<Shape>(),
            0,
            "AOT shape descriptors must be Shape-aligned"
        );
    }

    for known_shape in known_shape_entries {
        let shape_id = known_shape.shape_id as usize;
        let shape_ptr = shape_for_id(shape_base, shape_size, shape_offsets, shape_id);
        match known_shape.kind {
            0 => vm.known.byte_array_shape = shape_ptr,
            1 => vm.known.int32_array_shape = shape_ptr,
            2 => vm.known.string_shape = shape_ptr,
            3 => vm.known.thread_shape = shape_ptr,
            4 => vm.known.filler_word_shape = shape_ptr,
            5 => vm.known.filler_array_shape = shape_ptr,
            6 => vm.known.free_space_shape = shape_ptr,
            7 => vm.known.code_shape = shape_ptr,
            _ => panic!("invalid known shape kind {}", known_shape.kind),
        }
    }
}

fn shape_for_id(
    shape_base: Address,
    shape_size: usize,
    shape_offsets: &[u32],
    shape_id: usize,
) -> *const Shape {
    if shape_id >= shape_offsets.len() {
        panic!(
            "invalid AOT shape index {} ({} shapes available)",
            shape_id,
            shape_offsets.len()
        );
    }

    let offset = shape_offsets[shape_id] as usize;
    if offset >= shape_size {
        panic!(
            "invalid AOT shape offset {} for shape {} ({} bytes available)",
            offset, shape_id, shape_size
        );
    }

    shape_base.offset(offset).to_ptr::<Shape>()
}

pub fn initialize_code_map(
    vm: &mut VM,
    dora_entry_trampoline: *const u8,
    functions: &[AotFunctionEntry],
    gcpoints: &[AotGcPointEntry],
    gcpoint_offsets: &[i32],
    function_infos: &[AotFunctionInfoEntry],
    strings: &[AotStringEntry],
    locations: &[AotLocationEntry],
    inlined_functions: &[AotInlinedFunctionEntry],
) {
    vm.dora_entry_trampoline = Some(Address::from_ptr(dora_entry_trampoline));

    for function in functions {
        let code_start = function.code_start as usize;
        let code_end = function.code_end as usize;
        assert!(code_start <= code_end);

        let gcpoints_start = function.gcpoints_start as usize;
        let gcpoints_len = function.gcpoints_len as usize;

        let mut gcpoint_table = GcPointTable::new();
        for gcpoint in &gcpoints[gcpoints_start..gcpoints_start + gcpoints_len] {
            let offsets_start = gcpoint.offsets_start as usize;
            let offsets_len = gcpoint.offsets_len as usize;

            let offsets = gcpoint_offsets[offsets_start..offsets_start + offsets_len].to_vec();
            gcpoint_table.insert(gcpoint.pc_offset, GcPoint::from_offsets(offsets));
        }

        let locations_start = function.locations_start as usize;
        let locations_len = function.locations_len as usize;
        let function_info = decode_function_info(
            &function_infos[function.function_info_idx as usize],
            strings,
        );
        let location_entries = locations[locations_start..locations_start + locations_len]
            .iter()
            .map(|entry| {
                (
                    entry.pc_offset,
                    decode_inlined_location(entry.inlined_function_id, entry.line, entry.column),
                )
            })
            .collect();
        let location_table = LocationTable::from_entries(location_entries);

        let inlined_functions_start = function.inlined_functions_start as usize;
        let inlined_functions_len = function.inlined_functions_len as usize;
        let inlined_functions = inlined_functions
            [inlined_functions_start..inlined_functions_start + inlined_functions_len]
            .iter()
            .map(|entry| InlinedFunctionAot {
                function_info: decode_function_info(
                    &function_infos[entry.function_info_idx as usize],
                    strings,
                ),
                inlined_location: decode_inlined_location(
                    entry.inlined_function_id,
                    entry.line,
                    entry.column,
                ),
            })
            .collect();

        let code_kind = decode_code_kind(function.kind, function.fct_id);
        install_external_code_stub(
            vm,
            (function.code_start as usize).into(),
            (function.code_end as usize).into(),
            code_kind,
            gcpoint_table,
            location_table,
            function_info,
            inlined_functions,
        );
    }
}

const NO_INLINED_FUNCTION_ID: u32 = u32::MAX;

fn decode_inlined_location(inlined_function_id: u32, line: u32, column: u32) -> InlinedLocation {
    InlinedLocation {
        inlined_function_id: decode_inlined_function_id(inlined_function_id),
        location: Location::new(line, column),
    }
}

fn decode_inlined_function_id(value: u32) -> Option<InlinedFunctionId> {
    if value == NO_INLINED_FUNCTION_ID {
        None
    } else {
        Some(InlinedFunctionId(value))
    }
}

fn decode_function_info(
    entry: &AotFunctionInfoEntry,
    strings: &[AotStringEntry],
) -> FunctionInfoAot {
    FunctionInfoAot {
        name: decode_utf8(&strings[entry.name_idx as usize]),
        file: decode_utf8(&strings[entry.file_idx as usize]),
        loc: Location::new(entry.line, entry.column),
    }
}

fn decode_utf8(entry: &AotStringEntry) -> &'static str {
    let bytes = unsafe { slice::from_raw_parts(entry.data_ptr, entry.len as usize) };
    str::from_utf8(bytes).expect("AOT string payload is not valid UTF-8.")
}

pub fn patch_string_slots(
    vm: &VM,
    strings: &[AotStringEntry],
    string_slots: &[AotStringSlotEntry],
) {
    let mut string_addresses = vec![None; strings.len()];

    for slot in string_slots {
        let string_idx = slot.string_idx as usize;

        let address = match string_addresses[string_idx] {
            Some(address) => address,
            None => {
                let string_entry = &strings[string_idx];
                let bytes = unsafe {
                    slice::from_raw_parts(string_entry.data_ptr, string_entry.len as usize)
                };
                str::from_utf8(bytes).expect("AOT string payload is not valid UTF-8.");
                let address = Str::from_buffer_in_perm(vm, bytes).address().to_usize();
                string_addresses[string_idx] = Some(address);
                address
            }
        };

        unsafe {
            *slot.slot_ptr = address;
        }
    }
}

pub fn initialize_global_memory(vm: &mut VM, start: *const u8, end: *const u8, references: &[i32]) {
    use crate::gc::Address;
    use crate::vm::GlobalVariableMemory;

    let start_addr = Address::from(start as usize);
    let end_addr = Address::from(end as usize);
    let memory = GlobalVariableMemory::from_external(start_addr, end_addr, references.to_vec());
    vm.global_variable_memory = Some(memory);
}

pub fn current_thread_tld_address() -> usize {
    current_thread().tld_address().to_usize()
}

fn decode_code_kind(kind: u32, fct_id: u32) -> CodeKind {
    match kind {
        AOT_CODE_KIND_OPTIMIZED => CodeKind::OptimizedFct(FunctionId::from(fct_id as usize)),
        AOT_CODE_KIND_RUNTIME_ENTRY_TRAMPOLINE => {
            CodeKind::RuntimeEntryTrampoline(FunctionId::from(fct_id as usize))
        }
        AOT_CODE_KIND_DORA_ENTRY_TRAMPOLINE => CodeKind::DoraEntryTrampoline,
        AOT_CODE_KIND_ALLOCATION_FAILURE_TRAMPOLINE => CodeKind::AllocationFailureTrampoline,
        AOT_CODE_KIND_TRAP_TRAMPOLINE => CodeKind::TrapTrampoline,
        AOT_CODE_KIND_SAFEPOINT_TRAMPOLINE => CodeKind::SafepointTrampoline,
        AOT_CODE_KIND_UNREACHABLE_TRAMPOLINE => CodeKind::UnreachableTrampoline,
        AOT_CODE_KIND_FATAL_ERROR_TRAMPOLINE => CodeKind::FatalErrorTrampoline,
        AOT_CODE_KIND_STACK_OVERFLOW_TRAMPOLINE => CodeKind::StackOverflowTrampoline,
        _ => panic!("invalid AOT code kind {}", kind),
    }
}
