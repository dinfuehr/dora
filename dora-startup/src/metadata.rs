use dora_runtime::startup::{
    AotFunctionEntry, AotFunctionInfoEntry, AotGcPointEntry, AotInlinedFunctionEntry,
    AotKnownShapeEntry, AotLocationEntry, AotShapeEntry, AotShapeSlotEntry, AotStringEntry,
    AotStringSlotEntry, AotTestEntry,
};
use std::{mem, ptr, slice};

pub(crate) struct ShapeMetadata {
    pub strings: &'static [AotStringEntry],
    pub shape_refs: &'static [i32],
    pub shape_kinds: &'static [u8],
    pub shape_fields: &'static [u8],
    pub shape_vtable_entries: &'static [usize],
    pub shape_entries: &'static [AotShapeEntry],
    pub known_shape_entries: &'static [AotKnownShapeEntry],
}

pub(crate) struct CodeMetadata {
    pub gcpoint_offsets: &'static [i32],
    pub gcpoint_entries: &'static [AotGcPointEntry],
    pub location_entries: &'static [AotLocationEntry],
    pub function_info_entries: &'static [AotFunctionInfoEntry],
    pub inlined_function_entries: &'static [AotInlinedFunctionEntry],
    pub function_entries: &'static [AotFunctionEntry],
    pub test_entries: &'static [AotTestEntry],
}

unsafe extern "C" {
    #[link_name = "_dora_gc_collector"]
    static dora_gc_collector: u8;

    #[link_name = "_dora_aot_program_start"]
    static dora_aot_program_start: u8;
    #[link_name = "_dora_aot_program_end"]
    static dora_aot_program_end: u8;

    #[link_name = "_dora_aot_strings_start"]
    static dora_aot_strings_start: u8;
    #[link_name = "_dora_aot_strings_end"]
    static dora_aot_strings_end: u8;

    #[link_name = "_dora_aot_string_slots_start"]
    static dora_aot_string_slots_start: u8;
    #[link_name = "_dora_aot_string_slots_end"]
    static dora_aot_string_slots_end: u8;

    #[link_name = "_dora_aot_shape_refs_start"]
    static dora_aot_shape_refs_start: u8;
    #[link_name = "_dora_aot_shape_refs_end"]
    static dora_aot_shape_refs_end: u8;

    #[link_name = "_dora_aot_shape_kinds_start"]
    static dora_aot_shape_kinds_start: u8;
    #[link_name = "_dora_aot_shape_kinds_end"]
    static dora_aot_shape_kinds_end: u8;

    #[link_name = "_dora_aot_shape_fields_start"]
    static dora_aot_shape_fields_start: u8;
    #[link_name = "_dora_aot_shape_fields_end"]
    static dora_aot_shape_fields_end: u8;

    #[link_name = "_dora_aot_shapes_start"]
    static dora_aot_shapes_start: u8;
    #[link_name = "_dora_aot_shapes_end"]
    static dora_aot_shapes_end: u8;

    #[link_name = "_dora_aot_known_shapes_start"]
    static dora_aot_known_shapes_start: u8;
    #[link_name = "_dora_aot_known_shapes_end"]
    static dora_aot_known_shapes_end: u8;

    #[link_name = "_dora_aot_shape_vtables_start"]
    static dora_aot_shape_vtables_start: u8;
    #[link_name = "_dora_aot_shape_vtables_end"]
    static dora_aot_shape_vtables_end: u8;

    #[link_name = "_dora_aot_shape_slots_start"]
    static dora_aot_shape_slots_start: u8;
    #[link_name = "_dora_aot_shape_slots_end"]
    static dora_aot_shape_slots_end: u8;

    #[link_name = "_dora_global_memory"]
    static dora_global_memory: u8;
    #[link_name = "_dora_global_memory_end"]
    static dora_global_memory_end: u8;

    #[link_name = "_dora_aot_global_refs_start"]
    static dora_aot_global_refs_start: u8;
    #[link_name = "_dora_aot_global_refs_end"]
    static dora_aot_global_refs_end: u8;

    #[link_name = "_dora_aot_gcpoint_offsets_start"]
    static dora_aot_gcpoint_offsets_start: u8;
    #[link_name = "_dora_aot_gcpoint_offsets_end"]
    static dora_aot_gcpoint_offsets_end: u8;

    #[link_name = "_dora_aot_gcpoints_start"]
    static dora_aot_gcpoints_start: u8;
    #[link_name = "_dora_aot_gcpoints_end"]
    static dora_aot_gcpoints_end: u8;

    #[link_name = "_dora_aot_function_info_start"]
    static dora_aot_function_info_start: u8;
    #[link_name = "_dora_aot_function_info_end"]
    static dora_aot_function_info_end: u8;

    #[link_name = "_dora_aot_locations_start"]
    static dora_aot_locations_start: u8;
    #[link_name = "_dora_aot_locations_end"]
    static dora_aot_locations_end: u8;

    #[link_name = "_dora_aot_inlined_functions_start"]
    static dora_aot_inlined_functions_start: u8;
    #[link_name = "_dora_aot_inlined_functions_end"]
    static dora_aot_inlined_functions_end: u8;

    #[link_name = "_dora_aot_functions_start"]
    static dora_aot_functions_start: u8;
    #[link_name = "_dora_aot_functions_end"]
    static dora_aot_functions_end: u8;

    #[link_name = "_dora_aot_tests_start"]
    static dora_aot_tests_start: u8;
    #[link_name = "_dora_aot_tests_end"]
    static dora_aot_tests_end: u8;
}

pub(crate) fn gc_collector() -> u8 {
    unsafe { dora_gc_collector }
}

pub(crate) fn program_bytes() -> &'static [u8] {
    read_bytes(
        ptr::addr_of!(dora_aot_program_start),
        ptr::addr_of!(dora_aot_program_end),
    )
}

pub(crate) fn shape_metadata() -> ShapeMetadata {
    ShapeMetadata {
        strings: unsafe {
            read_table::<AotStringEntry>(
                ptr::addr_of!(dora_aot_strings_start),
                ptr::addr_of!(dora_aot_strings_end),
            )
        },
        shape_refs: unsafe {
            read_table::<i32>(
                ptr::addr_of!(dora_aot_shape_refs_start),
                ptr::addr_of!(dora_aot_shape_refs_end),
            )
        },
        shape_kinds: read_bytes(
            ptr::addr_of!(dora_aot_shape_kinds_start),
            ptr::addr_of!(dora_aot_shape_kinds_end),
        ),
        shape_fields: read_bytes(
            ptr::addr_of!(dora_aot_shape_fields_start),
            ptr::addr_of!(dora_aot_shape_fields_end),
        ),
        shape_vtable_entries: unsafe {
            read_table::<usize>(
                ptr::addr_of!(dora_aot_shape_vtables_start),
                ptr::addr_of!(dora_aot_shape_vtables_end),
            )
        },
        shape_entries: unsafe {
            read_table::<AotShapeEntry>(
                ptr::addr_of!(dora_aot_shapes_start),
                ptr::addr_of!(dora_aot_shapes_end),
            )
        },
        known_shape_entries: unsafe {
            read_table::<AotKnownShapeEntry>(
                ptr::addr_of!(dora_aot_known_shapes_start),
                ptr::addr_of!(dora_aot_known_shapes_end),
            )
        },
    }
}

pub(crate) fn shape_slots() -> &'static [AotShapeSlotEntry] {
    unsafe {
        read_table::<AotShapeSlotEntry>(
            ptr::addr_of!(dora_aot_shape_slots_start),
            ptr::addr_of!(dora_aot_shape_slots_end),
        )
    }
}

pub(crate) fn string_slots() -> &'static [AotStringSlotEntry] {
    unsafe {
        read_table::<AotStringSlotEntry>(
            ptr::addr_of!(dora_aot_string_slots_start),
            ptr::addr_of!(dora_aot_string_slots_end),
        )
    }
}

pub(crate) fn global_memory() -> (*const u8, *const u8) {
    (
        ptr::addr_of!(dora_global_memory),
        ptr::addr_of!(dora_global_memory_end),
    )
}

pub(crate) fn global_refs() -> &'static [i32] {
    unsafe {
        read_table::<i32>(
            ptr::addr_of!(dora_aot_global_refs_start),
            ptr::addr_of!(dora_aot_global_refs_end),
        )
    }
}

pub(crate) fn code_metadata() -> CodeMetadata {
    CodeMetadata {
        gcpoint_offsets: unsafe {
            read_table::<i32>(
                ptr::addr_of!(dora_aot_gcpoint_offsets_start),
                ptr::addr_of!(dora_aot_gcpoint_offsets_end),
            )
        },
        gcpoint_entries: unsafe {
            read_table::<AotGcPointEntry>(
                ptr::addr_of!(dora_aot_gcpoints_start),
                ptr::addr_of!(dora_aot_gcpoints_end),
            )
        },
        location_entries: unsafe {
            read_table::<AotLocationEntry>(
                ptr::addr_of!(dora_aot_locations_start),
                ptr::addr_of!(dora_aot_locations_end),
            )
        },
        function_info_entries: unsafe {
            read_table::<AotFunctionInfoEntry>(
                ptr::addr_of!(dora_aot_function_info_start),
                ptr::addr_of!(dora_aot_function_info_end),
            )
        },
        inlined_function_entries: unsafe {
            read_table::<AotInlinedFunctionEntry>(
                ptr::addr_of!(dora_aot_inlined_functions_start),
                ptr::addr_of!(dora_aot_inlined_functions_end),
            )
        },
        function_entries: unsafe {
            read_table::<AotFunctionEntry>(
                ptr::addr_of!(dora_aot_functions_start),
                ptr::addr_of!(dora_aot_functions_end),
            )
        },
        test_entries: unsafe {
            read_table::<AotTestEntry>(
                ptr::addr_of!(dora_aot_tests_start),
                ptr::addr_of!(dora_aot_tests_end),
            )
        },
    }
}

unsafe fn read_table<T>(start: *const u8, end: *const u8) -> &'static [T] {
    let size = mem::size_of::<T>();
    assert!(size > 0);
    let bytes = end as usize - start as usize;
    assert_eq!(
        bytes % size,
        0,
        "corrupt AOT metadata table (entry size mismatch)"
    );

    let len = bytes / size;
    if len == 0 {
        return &[];
    }

    assert_eq!(
        start as usize % mem::align_of::<T>(),
        0,
        "corrupt AOT metadata table (unaligned start pointer)"
    );

    unsafe { slice::from_raw_parts(start as *const T, len) }
}

fn read_bytes(start: *const u8, end: *const u8) -> &'static [u8] {
    let start_addr = start as usize;
    let end_addr = end as usize;
    assert!(
        start_addr <= end_addr,
        "corrupt AOT metadata table (invalid byte range)"
    );
    let len = end_addr - start_addr;
    unsafe { slice::from_raw_parts(start, len) }
}
