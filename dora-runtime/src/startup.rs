use crate::mirror::Str;
use crate::shape::{Shape, ShapeVisitor};
use crate::threads::current_thread;
use crate::vm::{ShapeKind, VM};
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

pub fn initialize_shapes(
    vm: &mut VM,
    shape_refs: &[i32],
    shape_entries: &[AotShapeEntry],
    known_shape_entries: &[AotKnownShapeEntry],
) {
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
