use dora_bytecode::Program;
use dora_runtime::startup::{
    AotKnownShapeEntry, AotShapeEntry, AotStringEntry, AotStringSlotEntry,
    current_thread_tld_address, initialize_shapes, patch_string_slots,
};
use dora_runtime::{VM, VmFlags, VmMode, clear_vm, execute_on_main, set_vm};
use std::{mem, ptr, slice};

unsafe extern "C" {
    #[link_name = "_dora_main"]
    fn dora_main();

    #[link_name = "_dora_entry_trampoline"]
    fn dora_entry_trampoline(tld: usize, fct: *const u8) -> i32;

    // .dora.strings section:
    // bytes in [_start, _end) are an array of AotStringEntry values.
    // Each AotStringEntry stores (data_ptr, len).
    // data_ptr points to the string's UTF-8 payload in .rodata.
    // len stores the UTF-8 payload length in bytes.
    #[link_name = "_dora_aot_strings_start"]
    static dora_aot_strings_start: u8;
    #[link_name = "_dora_aot_strings_end"]
    static dora_aot_strings_end: u8;

    // .dora.string_slots section:
    // bytes in [_start, _end) are a read-only array of AotStringSlotEntry
    // relocation metadata, not the writable slots themselves.
    // Each AotStringSlotEntry is the pair (slot_ptr, string_idx).
    // Each entry points to a writable slot (currently emitted in .data) that
    // must be patched with the heap address of the string at string_idx.
    // string_idx is an index into the AotStringEntry table in .dora.strings.
    #[link_name = "_dora_aot_string_slots_start"]
    static dora_aot_string_slots_start: u8;
    #[link_name = "_dora_aot_string_slots_end"]
    static dora_aot_string_slots_end: u8;

    // .dora.shape_refs section:
    // bytes in [_start, _end) are a flat i32 array of shape reference offsets.
    // Shape entries reference subranges of this array.
    #[link_name = "_dora_aot_shape_refs_start"]
    static dora_aot_shape_refs_start: u8;
    #[link_name = "_dora_aot_shape_refs_end"]
    static dora_aot_shape_refs_end: u8;

    // .dora.shapes section:
    // bytes in [_start, _end) are an array of AotShapeEntry values.
    // These entries describe serialized shape objects that are recreated at
    // startup via Shape::new.
    #[link_name = "_dora_aot_shapes_start"]
    static dora_aot_shapes_start: u8;
    #[link_name = "_dora_aot_shapes_end"]
    static dora_aot_shapes_end: u8;

    // .dora.known_shapes section:
    // bytes in [_start, _end) are an array of AotKnownShapeEntry values.
    // Each entry maps a known shape slot (string/code/thread/...) to a shape
    // id from .dora.shapes so vm.known.* can be initialized.
    #[link_name = "_dora_aot_known_shapes_start"]
    static dora_aot_known_shapes_start: u8;
    #[link_name = "_dora_aot_known_shapes_end"]
    static dora_aot_known_shapes_end: u8;
}

fn empty_program() -> Program {
    Program {
        packages: Vec::new(),
        modules: Vec::new(),
        functions: Vec::new(),
        globals: Vec::new(),
        consts: Vec::new(),
        classes: Vec::new(),
        structs: Vec::new(),
        enums: Vec::new(),
        traits: Vec::new(),
        impls: Vec::new(),
        extensions: Vec::new(),
        aliases: Vec::new(),
        source_files: Vec::new(),
        extern_modules: Vec::new(),
        extern_functions: Vec::new(),
        extern_globals: Vec::new(),
        extern_consts: Vec::new(),
        extern_classes: Vec::new(),
        extern_structs: Vec::new(),
        extern_enums: Vec::new(),
        extern_traits: Vec::new(),
        extern_impls: Vec::new(),
        extern_extensions: Vec::new(),
        extern_aliases: Vec::new(),
        stdlib_package_id: 0.into(),
        program_package_id: 0.into(),
        boots_package_id: None,
        main_fct_id: None,
    }
}

#[unsafe(export_name = "dora_aot_main")]
pub extern "C" fn dora_aot_main() -> i32 {
    let vm_flags = VmFlags {
        emit_asm: None,
        emit_asm_file: None,
        emit_asm_boots: false,
        emit_bytecode_compiler: None,
        emit_bytecode_boots: false,
        emit_compiler: false,
        emit_graph: None,
        emit_graph_after_each_pass: false,
        emit_stubs: false,
        enable_perf: false,
        always_boots: false,
        use_boots: None,
        omit_bounds_check: false,
        emit_debug: None,
        emit_debug_boots: false,
        emit_debug_native: false,
        emit_debug_compile: false,
        emit_debug_entry: false,
        gc_events: false,
        gc_stress: false,
        gc_stress_minor: false,
        gc_stress_in_lazy_compile: false,
        gc_stats: false,
        gc_verbose: false,
        gc_verify: false,
        gc_worker: 0,
        gc_young_size: None,
        gc_semi_ratio: None,
        gc: None,
        compiler: None,
        min_heap_size: None,
        max_heap_size: None,
        code_size: None,
        readonly_size: None,
        disable_tlab: false,
        disable_barrier: false,
        bootstrap_compiler: false,
        snapshot_on_oom: None,
    };

    let mut vm = VM::new(VmMode::Aot, empty_program(), vm_flags, Vec::new());

    let shape_refs = unsafe {
        read_table::<i32>(
            ptr::addr_of!(dora_aot_shape_refs_start),
            ptr::addr_of!(dora_aot_shape_refs_end),
        )
    };
    let shape_entries = unsafe {
        read_table::<AotShapeEntry>(
            ptr::addr_of!(dora_aot_shapes_start),
            ptr::addr_of!(dora_aot_shapes_end),
        )
    };
    let known_shape_entries = unsafe {
        read_table::<AotKnownShapeEntry>(
            ptr::addr_of!(dora_aot_known_shapes_start),
            ptr::addr_of!(dora_aot_known_shapes_end),
        )
    };
    initialize_shapes(&mut vm, shape_refs, shape_entries, known_shape_entries);

    set_vm(&vm);

    let strings = unsafe {
        read_table::<AotStringEntry>(
            ptr::addr_of!(dora_aot_strings_start),
            ptr::addr_of!(dora_aot_strings_end),
        )
    };
    let string_slots = unsafe {
        read_table::<AotStringSlotEntry>(
            ptr::addr_of!(dora_aot_string_slots_start),
            ptr::addr_of!(dora_aot_string_slots_end),
        )
    };
    patch_string_slots(&vm, strings, string_slots);

    execute_on_main(|| unsafe {
        dora_entry_trampoline(current_thread_tld_address(), dora_main as *const u8)
    });

    vm.threads.join_all();
    vm.shutdown();
    clear_vm();

    0
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
