// AOT metadata sections and their relationships
//
// Strings
// -------
// .dora.strings is the shared AOT string table. Machine code string
// constants load heap string pointers via RIP-relative moves from writable
// slots in .dora.string_data.  At startup, patch_string_slots allocates heap
// strings only for entries referenced by .dora.string_slots and writes their
// addresses into those slots. Other metadata, such as function info, can
// reference .dora.strings directly as static UTF-8.
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
// Trait object shapes carry vtable entries — function pointers resolved
// by the linker — stored in .dora.shape_vtables.  At startup these are
// copied into the Shape's inline vtable so virtual dispatch works.
//
//   .text (RX)           .dora.shape_data (RW)    .dora.shapes (R)       .dora.shape_refs (R)
//   +----------------+   +----------------+        (AotShapeEntry)        +--------+
//   | mov reg,[rip]--+-->| slot (4 bytes) |        +----------------+     | ref[0] |
//   +----------------+   +----------------+        | kind           |     | ref[1] |
//                             ^                    | visitor        |     | ...    |
//                             |                    | refs_start ----+---->+--------+
//                             |                    | refs_len       |
//   .dora.shape_slots (R)     |                    | instance_size  |     .dora.shape_vtables (R)
//   (AotShapeSlotEntry)       |                    | element_size   |     (flat usize fn ptrs)
//   +--------------------+    |                    | vtable_start --+---->+--------+
//   | slot_ptr ----------+--->+                    | vtable_len     |     | fptr_0 |
//   | shape_id = 0       |                         +----------------+     | fptr_1 |
//   +--------------------+                                                | ...    |
//       index into .dora.shapes                                           +--------+
//
//   .dora.known_shapes (R)
//   (AotKnownShapeEntry)
//   +------------------+
//   | kind (e.g. Code) |    maps vm.known.* fields to shape ids
//   | shape_id = 0     |    index into .dora.shapes
//   +------------------+
//
// Global variables
// -----------------
// Global variable memory lives in a .bss section, zero-initialized at
// load time (matching UNINITIALIZED=0).  Machine code uses lea with a
// RIP-relative displacement and a linker relocation to compute the
// address of each global's state byte or value slot.  At startup,
// initialize_global_memory wraps the .bss region in a
// GlobalVariableMemory (non-owning) so the GC can scan it for
// references.
//
//   .text (RX)                  .bss (RW)                    .dora.global_refs (R)
//   +----------------------+    +-------------------------+   (flat i32 offsets)
//   | lea reg,[rip+disp] --+--->| state[0] (1 byte)       |   +----------------+
//   +----------------------+    | value[0] (N bytes)      |   | ref_offset[0]  |
//                               | state[1] (1 byte)       |   | ref_offset[1]  |
//                               | value[1] (N bytes)      |   | ...            |
//                               | ...                     |   +----------------+
//                               +-------------------------+
//                               _dora_global_memory
//                                    ...
//                               _dora_global_memory_end
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
    AotFunctionEntry, AotFunctionInfoEntry, AotGcPointEntry, AotInlinedFunctionEntry,
    AotKnownShapeEntry, AotLocationEntry, AotShapeEntry, AotShapeSlotEntry, AotStringEntry,
    AotStringSlotEntry, current_thread_tld_address, initialize_code_map, initialize_global_memory,
    initialize_shapes, patch_shape_slots, patch_string_slots,
};
use dora_runtime::{
    CollectorName, MemSize, TargetArch, VM, VmFlags, VmMode, clear_vm, execute_on_main, set_vm,
};
use std::io::Write;
use std::{mem, ptr, slice};

unsafe extern "C" {
    #[link_name = "_dora_main"]
    fn dora_main();

    #[link_name = "_dora_entry_trampoline"]
    fn dora_entry_trampoline(tld: usize, fct: *const u8) -> i32;

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
}

fn decode_program() -> Program {
    let bytes = read_bytes(
        ptr::addr_of!(dora_aot_program_start),
        ptr::addr_of!(dora_aot_program_end),
    );
    let config = bincode::config::standard();
    let (program, decoded_len): (Program, usize) =
        bincode::decode_from_slice(bytes, config).expect("failed to decode embedded AOT program");
    assert_eq!(
        decoded_len,
        bytes.len(),
        "embedded AOT program has trailing bytes"
    );
    program
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

    /// Print GC verbose output
    #[arg(long)]
    gc_verbose: bool,

    /// Number of GC worker threads
    #[arg(long, default_value_t = 0)]
    gc_worker: usize,

    /// Use fixed size for young generation
    #[arg(long, value_parser = parse_mem_size)]
    gc_young_size: Option<MemSize>,

    /// Disable tlab allocation
    #[arg(long)]
    disable_tlab: bool,

    /// Set minimum heap size
    #[arg(long, value_parser = parse_mem_size)]
    min_heap_size: Option<MemSize>,

    /// Set maximum heap size
    #[arg(long, value_parser = parse_mem_size)]
    max_heap_size: Option<MemSize>,
}

fn parse_mem_size(value: &str) -> Result<MemSize, String> {
    let suffix = if let Some(ch) = value.chars().last() {
        match ch {
            'k' | 'K' => 1024,
            'm' | 'M' => 1024 * 1024,
            'g' | 'G' => 1024 * 1024 * 1024,
            _ => 1,
        }
    } else {
        1
    };

    let prefix = if suffix != 1 {
        let (left, _) = value.split_at(value.len() - 1);
        left
    } else {
        value
    };

    match prefix.parse::<usize>() {
        Ok(size) => Ok(MemSize(size * suffix)),
        Err(_) => Err(format!("'{}' is not a valid mem size", value)),
    }
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
        gc_verbose: flags.gc_verbose,
        gc_verify: flags.gc_verify,
        gc_worker: flags.gc_worker,
        gc_young_size: flags.gc_young_size,
        gc_semi_ratio: None,
        gc: Some(decode_collector_name(unsafe { dora_gc_collector })),
        compiler: None,
        min_heap_size: flags.min_heap_size,
        max_heap_size: flags.max_heap_size,
        code_size: None,
        readonly_size: None,
        disable_tlab: flags.disable_tlab,
        disable_barrier: false,
        bootstrap_compiler: false,
        snapshot_on_oom: None,
        target_arch: TargetArch::host(),
    };

    let mut vm = VM::new(VmMode::Aot, decode_program(), vm_flags, Vec::new());

    let strings = unsafe {
        read_table::<AotStringEntry>(
            ptr::addr_of!(dora_aot_strings_start),
            ptr::addr_of!(dora_aot_strings_end),
        )
    };
    let shape_refs = unsafe {
        read_table::<i32>(
            ptr::addr_of!(dora_aot_shape_refs_start),
            ptr::addr_of!(dora_aot_shape_refs_end),
        )
    };
    let shape_vtable_entries = unsafe {
        read_table::<usize>(
            ptr::addr_of!(dora_aot_shape_vtables_start),
            ptr::addr_of!(dora_aot_shape_vtables_end),
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
    let created_shapes = initialize_shapes(
        &mut vm,
        strings,
        shape_refs,
        shape_vtable_entries,
        shape_entries,
        known_shape_entries,
    );

    let global_refs = unsafe {
        read_table::<i32>(
            ptr::addr_of!(dora_aot_global_refs_start),
            ptr::addr_of!(dora_aot_global_refs_end),
        )
    };
    initialize_global_memory(
        &mut vm,
        ptr::addr_of!(dora_global_memory),
        ptr::addr_of!(dora_global_memory_end),
        global_refs,
    );

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
    let location_entries = unsafe {
        read_table::<AotLocationEntry>(
            ptr::addr_of!(dora_aot_locations_start),
            ptr::addr_of!(dora_aot_locations_end),
        )
    };
    let function_info_entries = unsafe {
        read_table::<AotFunctionInfoEntry>(
            ptr::addr_of!(dora_aot_function_info_start),
            ptr::addr_of!(dora_aot_function_info_end),
        )
    };
    let inlined_function_entries = unsafe {
        read_table::<AotInlinedFunctionEntry>(
            ptr::addr_of!(dora_aot_inlined_functions_start),
            ptr::addr_of!(dora_aot_inlined_functions_end),
        )
    };
    let function_entries = unsafe {
        read_table::<AotFunctionEntry>(
            ptr::addr_of!(dora_aot_functions_start),
            ptr::addr_of!(dora_aot_functions_end),
        )
    };
    initialize_code_map(
        &mut vm,
        dora_entry_trampoline as *const u8,
        function_entries,
        gcpoint_entries,
        gcpoint_offsets,
        function_info_entries,
        strings,
        location_entries,
        inlined_function_entries,
    );

    set_vm(&vm);
    vm.gc.setup(&vm);

    let shape_slots = unsafe {
        read_table::<AotShapeSlotEntry>(
            ptr::addr_of!(dora_aot_shape_slots_start),
            ptr::addr_of!(dora_aot_shape_slots_end),
        )
    };
    patch_shape_slots(&vm, shape_entries, shape_slots, &created_shapes);

    let string_slots = unsafe {
        read_table::<AotStringSlotEntry>(
            ptr::addr_of!(dora_aot_string_slots_start),
            ptr::addr_of!(dora_aot_string_slots_end),
        )
    };
    patch_string_slots(&vm, strings, string_slots);

    let exit_code = execute_on_main(|| unsafe {
        dora_entry_trampoline(current_thread_tld_address(), dora_main as *const u8)
    });

    let main_fct_id = vm.program.main_fct_id.expect("missing AOT main function");
    let main_returns_unit = vm.fct(main_fct_id).return_type.is_unit();

    std::io::stdout().flush().ok();

    vm.threads.join_all();
    vm.shutdown();
    clear_vm();

    if main_returns_unit { 0 } else { exit_code }
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

fn decode_collector_name(value: u8) -> CollectorName {
    match value {
        0 => CollectorName::Zero,
        1 => CollectorName::Copy,
        2 => CollectorName::Sweep,
        3 => CollectorName::Swiper,
        _ => panic!("invalid GC collector value {}", value),
    }
}
