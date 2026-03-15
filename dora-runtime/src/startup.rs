use crate::mirror::Str;
use crate::shape::{Shape, ShapeVisitor};
use crate::threads::current_thread;
use crate::vm::{CodeKind, GcPoint, GcPointTable, ShapeKind, VM, install_external_code_stub};
use dora_bytecode::FunctionId;
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
/// Entry type for the `.dora.shapes` metadata section.
pub struct AotShapeEntry {
    /// Encoded shape kind (`ShapeKind` discriminant).
    pub kind: u64,
    /// Encoded shape visitor (`ShapeVisitor` discriminant).
    pub visitor: u64,
    /// Start index into the flat refs table in `.dora.shape_refs`.
    pub refs_start: u64,
    /// Number of entries in `.dora.shape_refs` for this shape.
    pub refs_len: u64,
    /// Instance size in bytes.
    pub instance_size: u64,
    /// Element size in bytes for array-like shapes.
    pub element_size: u64,
}

#[repr(C)]
/// Entry type for the `.dora.known_shapes` metadata section.
pub struct AotKnownShapeEntry {
    /// Encoded known-shape slot kind (byte array, string, code, ...).
    pub kind: u32,
    /// Index into the `.dora.shapes` table (`AotShapeEntry` array).
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
    /// Start index into `.dora.gcpoints` (`AotGcPointEntry` array).
    pub gcpoints_start: u32,
    /// Number of gcpoint entries in `.dora.gcpoints` for this function.
    pub gcpoints_len: u32,
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

pub fn initialize_shapes(
    vm: &mut VM,
    shape_refs: &[i32],
    shape_entries: &[AotShapeEntry],
    known_shape_entries: &[AotKnownShapeEntry],
) -> Vec<*const Shape> {
    let mut created_shapes = Vec::with_capacity(shape_entries.len());

    for entry in shape_entries {
        let refs_start = entry.refs_start as usize;
        let refs_len = entry.refs_len as usize;
        if refs_start + refs_len > shape_refs.len() {
            panic!(
                "invalid shape refs range {}..{} (len {})",
                refs_start,
                refs_start + refs_len,
                shape_refs.len()
            );
        }

        let refs = shape_refs[refs_start..refs_start + refs_len].to_vec();
        let shape = Shape::new(
            vm,
            decode_shape_kind(entry.kind),
            decode_shape_visitor(entry.visitor),
            refs,
            Vec::new(),
            entry.instance_size as usize,
            entry.element_size as usize,
            &[],
        );
        created_shapes.push(shape);
    }

    for known_shape in known_shape_entries {
        let shape_id = known_shape.shape_id as usize;
        if shape_id >= created_shapes.len() {
            panic!(
                "invalid known shape id {} ({} shapes available)",
                shape_id,
                created_shapes.len()
            );
        }

        let shape_ptr = created_shapes[shape_id];
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

    created_shapes
}

pub fn initialize_code_map(
    vm: &VM,
    functions: &[AotFunctionEntry],
    gcpoints: &[AotGcPointEntry],
    gcpoint_offsets: &[i32],
) {
    for function in functions {
        let code_start = function.code_start as usize;
        let code_end = function.code_end as usize;

        if code_start >= code_end {
            panic!(
                "invalid AOT function range {:x}..{:x}",
                code_start, code_end
            );
        }

        let gcpoints_start = function.gcpoints_start as usize;
        let gcpoints_len = function.gcpoints_len as usize;
        if gcpoints_start + gcpoints_len > gcpoints.len() {
            panic!(
                "invalid gcpoints range {}..{} (len {})",
                gcpoints_start,
                gcpoints_start + gcpoints_len,
                gcpoints.len()
            );
        }

        let mut gcpoint_table = GcPointTable::new();
        for gcpoint in &gcpoints[gcpoints_start..gcpoints_start + gcpoints_len] {
            let offsets_start = gcpoint.offsets_start as usize;
            let offsets_len = gcpoint.offsets_len as usize;
            if offsets_start + offsets_len > gcpoint_offsets.len() {
                panic!(
                    "invalid gcpoint offsets range {}..{} (len {})",
                    offsets_start,
                    offsets_start + offsets_len,
                    gcpoint_offsets.len()
                );
            }

            let offsets = gcpoint_offsets[offsets_start..offsets_start + offsets_len].to_vec();
            gcpoint_table.insert(gcpoint.pc_offset, GcPoint::from_offsets(offsets));
        }

        let code_kind = decode_code_kind(function.kind, function.fct_id);
        install_external_code_stub(
            vm,
            (function.code_start as usize).into(),
            (function.code_end as usize).into(),
            code_kind,
            gcpoint_table,
        );
    }
}

pub fn patch_string_slots(
    vm: &VM,
    strings: &[AotStringEntry],
    string_slots: &[AotStringSlotEntry],
) {
    if vm.known.string_shape.is_null() {
        panic!("AOT string slots present but VM string shape is not initialized.");
    }

    let mut string_addresses = Vec::with_capacity(strings.len());

    for string_entry in strings {
        let bytes =
            unsafe { slice::from_raw_parts(string_entry.data_ptr, string_entry.len as usize) };
        str::from_utf8(bytes).expect("AOT string payload is not valid UTF-8.");
        string_addresses.push(Str::from_buffer_in_perm(vm, bytes).address().to_usize());
    }

    for slot in string_slots {
        let string_idx = slot.string_idx as usize;
        if string_idx >= string_addresses.len() {
            panic!(
                "invalid AOT string slot index {} ({} strings available)",
                string_idx,
                string_addresses.len()
            );
        }

        unsafe {
            *slot.slot_ptr = string_addresses[string_idx];
        }
    }
}

#[repr(C)]
pub struct AotShapeSlotEntry {
    pub slot_ptr: *mut u32,
    pub shape_id: u32,
    pub _reserved: u32,
}

pub fn patch_shape_slots(
    vm: &VM,
    _shape_entries: &[AotShapeEntry],
    shape_slots: &[AotShapeSlotEntry],
    created_shapes: &[*const Shape],
) {
    for slot in shape_slots {
        let shape_id = slot.shape_id as usize;
        if shape_id >= created_shapes.len() {
            panic!(
                "invalid AOT shape slot index {} ({} shapes available)",
                shape_id,
                created_shapes.len()
            );
        }

        let shape_ptr = created_shapes[shape_id];
        let shape_address = crate::gc::Address::from_ptr(shape_ptr);
        let compressed = shape_address.offset_from(vm.meta_space_start()) as u32;

        unsafe {
            *slot.slot_ptr = compressed;
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

fn decode_shape_kind(kind: u64) -> ShapeKind {
    match kind {
        0 => ShapeKind::Builtin,
        1 => ShapeKind::String,
        _ => panic!("invalid shape kind {}", kind),
    }
}

fn decode_shape_visitor(visitor: u64) -> ShapeVisitor {
    match visitor {
        0 => ShapeVisitor::Regular,
        1 => ShapeVisitor::PointerArray,
        2 => ShapeVisitor::RecordArray,
        3 => ShapeVisitor::None,
        4 => ShapeVisitor::Invalid,
        _ => panic!("invalid shape visitor {}", visitor),
    }
}

fn decode_code_kind(kind: u32, fct_id: u32) -> CodeKind {
    match kind {
        0 => CodeKind::OptimizedFct(FunctionId::from(fct_id as usize)),
        1 => CodeKind::RuntimeEntryTrampoline(FunctionId::from(fct_id as usize)),
        2 => CodeKind::DoraEntryTrampoline,
        _ => panic!("invalid AOT code kind {}", kind),
    }
}
