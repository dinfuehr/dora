// AOT metadata sections and their relationships
//
// Strings
// -------
// Machine code loads string pointers via RIP-relative moves from writable
// slots in .dora.string_data.  At startup, patch_string_slots allocates
// heap strings and writes their addresses into those slots.
//
//   .text (RX)           .dora.string_data (RW)   .dora.strings (R)      .rodata (R)
//   +----------------+   +---------------+         (AotStringEntry)       +-----------+
//   | mov reg,[rip]--+-->| slot (8 bytes) |        +----------------+     | "hello"   |
//   +----------------+   +---------------+         | data_ptr   ----+---->+-----------+
//                             ^                    | len            |
//                             |                    +----------------+
//   .dora.string_slots (R)    |
//   (AotStringSlotEntry)      |
//   +-------------------------+
//   | slot_ptr ---------------+
//   | string_idx = 0          |    index into .dora.strings
//   +-------------------------+
//
// Shapes
// ------
// Machine code loads compressed shape pointers via RIP-relative moves
// from writable slots in .dora.shape_data.  At startup, shapes are
// recreated from the .dora.shapes table, then patch_shape_slots writes
// the compressed pointer (shape address minus meta_space_start) into
// each slot.  The codegen combines the compressed pointer with the
// sentinel and remembered bit to form the full object header word.
//
//   .text (RX)           .dora.shape_data (RW)    .dora.shapes (R)       .dora.shape_refs (R)
//   +----------------+   +---------------+         (AotShapeEntry)        +--------+
//   | mov reg,[rip]--+-->| slot (4 bytes) |        +----------------+     | ref[0] |
//   +----------------+   +---------------+         | kind           |     | ref[1] |
//                             ^                    | visitor        |     | ...    |
//                             |                    | refs_start ----+---->+--------+
//   .dora.shape_slots (R)     |                    | refs_len       |
//   (AotShapeSlotEntry)       |                    | instance_size  |
//   +-------------------------+                    | element_size   |
//   | slot_ptr ---------------+                    +----------------+
//   | shape_id = 0            |    index into .dora.shapes
//   +-------------------------+
//
//   .dora.known_shapes (R)
//   (AotKnownShapeEntry)
//   +------------------+
//   | kind (e.g. Code) |    maps vm.known.* fields to shape ids
//   | shape_id = 0     |    index into .dora.shapes
//   +------------------+
//
// Functions / GC stack maps
// -------------------------
// Startup rebuilds function GC metadata and registers code ranges in
// vm.code_map via initialize_code_map().
//
//   .text (RX)                  .dora.functions (R)              .dora.gcpoints (R)
//   +-------------------+       (AotFunctionEntry)               (AotGcPointEntry)
//   | _dora_fct0 ...    |<--+   +--------------------------+    +----------------------+
//   |      ...          |   +---| code_start               |    | pc_offset            |
//   | _dora_fct0_end    |<------| code_end                 |    | offsets_start -------+---+
//   +-------------------+       | fct_id                   |    | offsets_len          |   |
//                               | kind                     |    +----------------------+   |
//                               | gcpoints_start ----------+--->   ...                     |
//                               | gcpoints_len             |                               |
//                               +--------------------------+                               |
//                                                                                          |
//                               .dora.gcpoint_offsets (R)                                  |
//                               (flat i32 stack-slot offsets)                              |
//                               +----------------------+                                   |
//                               | fp_off_0             |<----------------------------------+
//                               | fp_off_1             |
//                               | ...                  |
//                               +----------------------+

use clap::Parser;
use dora_bytecode::Program;
use dora_runtime::startup::{
    AotFunctionEntry, AotGcPointEntry, AotKnownShapeEntry, AotShapeEntry, AotShapeSlotEntry,
    AotStringEntry, AotStringSlotEntry, current_thread_tld_address, initialize_code_map,
    initialize_shapes, patch_shape_slots, patch_string_slots,
};
use dora_runtime::{VM, VmFlags, VmMode, clear_vm, execute_on_main, set_vm};
use std::{mem, ptr, slice};

unsafe extern "C" {
    #[link_name = "_dora_main"]
    fn dora_main();

    #[link_name = "_dora_entry_trampoline"]
    fn dora_entry_trampoline(tld: usize, fct: *const u8) -> i32;

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

    #[link_name = "_dora_aot_shapes_start"]
    static dora_aot_shapes_start: u8;
    #[link_name = "_dora_aot_shapes_end"]
    static dora_aot_shapes_end: u8;

    #[link_name = "_dora_aot_known_shapes_start"]
    static dora_aot_known_shapes_start: u8;
    #[link_name = "_dora_aot_known_shapes_end"]
    static dora_aot_known_shapes_end: u8;

    #[link_name = "_dora_aot_shape_slots_start"]
    static dora_aot_shape_slots_start: u8;
    #[link_name = "_dora_aot_shape_slots_end"]
    static dora_aot_shape_slots_end: u8;

    #[link_name = "_dora_aot_gcpoint_offsets_start"]
    static dora_aot_gcpoint_offsets_start: u8;
    #[link_name = "_dora_aot_gcpoint_offsets_end"]
    static dora_aot_gcpoint_offsets_end: u8;

    #[link_name = "_dora_aot_gcpoints_start"]
    static dora_aot_gcpoints_start: u8;
    #[link_name = "_dora_aot_gcpoints_end"]
    static dora_aot_gcpoints_end: u8;

    #[link_name = "_dora_aot_functions_start"]
    static dora_aot_functions_start: u8;
    #[link_name = "_dora_aot_functions_end"]
    static dora_aot_functions_end: u8;
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

#[derive(Parser)]
struct AotFlags {
    /// Verify heap before and after collections
    #[arg(long)]
    gc_verify: bool,

    /// Trigger GC at every allocation
    #[arg(long)]
    gc_stress: bool,

    /// Trigger minor GC at every allocation
    #[arg(long)]
    gc_stress_minor: bool,
}

#[unsafe(export_name = "dora_aot_main")]
pub extern "C" fn dora_aot_main() -> i32 {
    let dora_flags = std::env::var("DORA_FLAGS").unwrap_or_default();
    let args = match shlex::split(&dora_flags) {
        Some(args) => args,
        None => {
            eprintln!("DORA_FLAGS: invalid shell quoting");
            return 1;
        }
    };
    // try_parse_from expects argv[0] (program name) as the first element.
    let args = std::iter::once(String::new()).chain(args);
    let flags = match AotFlags::try_parse_from(args) {
        Ok(flags) => flags,
        Err(e) => {
            e.print().ok();
            return 1;
        }
    };

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
        gc_stress: flags.gc_stress,
        gc_stress_minor: flags.gc_stress_minor,
        gc_stress_in_lazy_compile: false,
        gc_stats: false,
        gc_verbose: false,
        gc_verify: flags.gc_verify,
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
    let created_shapes = initialize_shapes(&mut vm, shape_refs, shape_entries, known_shape_entries);

    let gcpoint_offsets = unsafe {
        read_table::<i32>(
            ptr::addr_of!(dora_aot_gcpoint_offsets_start),
            ptr::addr_of!(dora_aot_gcpoint_offsets_end),
        )
    };
    let gcpoint_entries = unsafe {
        read_table::<AotGcPointEntry>(
            ptr::addr_of!(dora_aot_gcpoints_start),
            ptr::addr_of!(dora_aot_gcpoints_end),
        )
    };
    let function_entries = unsafe {
        read_table::<AotFunctionEntry>(
            ptr::addr_of!(dora_aot_functions_start),
            ptr::addr_of!(dora_aot_functions_end),
        )
    };
    initialize_code_map(&vm, function_entries, gcpoint_entries, gcpoint_offsets);

    set_vm(&vm);
    vm.gc.setup(&vm);

    let shape_slots = unsafe {
        read_table::<AotShapeSlotEntry>(
            ptr::addr_of!(dora_aot_shape_slots_start),
            ptr::addr_of!(dora_aot_shape_slots_end),
        )
    };
    patch_shape_slots(&vm, shape_entries, shape_slots, &created_shapes);

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
