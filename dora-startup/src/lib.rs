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
//   .text (RX)           .dora.shape_data (RW)    .dora.shapes (R)       .dora.shape_kinds (R)
//   +----------------+   +----------------+        (AotShapeEntry)        +----------+
//   | mov reg,[rip]--+-->| slot (4 bytes) |        +----------------+     | kind[0]  |
//   +----------------+   +----------------+        | kind_start ----+---->+----------+
//                             ^                    | kind_len       |
//                             |                    | visitor        |     .dora.shape_refs (R)
//                             |                    | refs_start ----+---->+--------+
//   .dora.shape_slots (R)     |                    | refs_len       |     | ref[0] |
//   (AotShapeSlotEntry)       |                    | fields_start --+-+   | ref[1] |
//   +--------------------+    |                    | fields_len     | |   | ...    |
//   | slot_ptr ----------+--->+                    | instance_size  | |   +--------+
//   | shape_id = 0       |                         | element_size   | |
//   +--------------------+                         | vtable_start --+-+->.dora.shape_vtables (R)
//       index into .dora.shapes                    | vtable_len     | |   (flat usize fn ptrs)
//                                                  +----------------+ |   +--------+
//                                                                      |   | fptr_0 |
//                                                                      |   | fptr_1 |
//                                                                      |   | ...    |
//                                                                      |   +--------+
//                                                                      |
//                                                                      +-> .dora.shape_fields (R)
//                                                                          (encoded fields)
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
    current_thread_tld_address, initialize_code_map, initialize_global_memory, initialize_shapes,
    patch_shape_slots, patch_string_slots,
};
use dora_runtime::{
    CollectorName, MemSize, TargetArch, VM, VmFlags, VmMode, clear_vm, execute_on_main, set_vm,
};
use std::ffi::CStr;
use std::io::Write;
use std::os::raw::{c_char, c_int};

mod boots;
mod metadata;

unsafe extern "C" {
    #[link_name = "_dora_main"]
    fn dora_main();

    #[link_name = "_dora_entry_trampoline"]
    fn dora_entry_trampoline(tld: usize, fct: *const u8) -> i32;
}

fn decode_program() -> Program {
    let bytes = metadata::program_bytes();
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
struct RuntimeFlags {
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

fn program_args_from_argv(
    argc: c_int,
    argv: *const *const c_char,
    skip_first: bool,
) -> Vec<String> {
    let argc = match usize::try_from(argc) {
        Ok(argc) => argc,
        Err(_) => return Vec::new(),
    };

    if argc == 0 || argv.is_null() {
        return Vec::new();
    }

    let start_idx = if skip_first { 1 } else { 0 };
    if start_idx >= argc {
        return Vec::new();
    }

    let mut program_args = Vec::with_capacity(argc - start_idx);

    for idx in start_idx..argc {
        let arg = unsafe { *argv.add(idx) };
        assert!(!arg.is_null(), "program argument pointer is null");

        let arg = unsafe { CStr::from_ptr(arg) };
        let arg = arg.to_str().expect("program argument is not valid UTF-8");
        program_args.push(arg.to_string());
    }

    program_args
}

fn vm_flags_from_runtime_flags(runtime_flags: &RuntimeFlags) -> VmFlags {
    VmFlags {
        emit_asm: None,
        emit_asm_file: None,
        emit_bytecode_compiler: None,
        emit_compiler: false,
        emit_graph: None,
        emit_graph_after_each_pass: false,
        emit_stubs: false,
        enable_perf: false,
        emit_debug: None,
        emit_debug_native: false,
        emit_debug_compile: false,
        emit_debug_entry: false,
        gc_events: false,
        gc_stress: runtime_flags.gc_stress,
        gc_stress_minor: runtime_flags.gc_stress_minor,
        gc_stress_in_lazy_compile: false,
        gc_stats: false,
        gc_verbose: runtime_flags.gc_verbose,
        gc_verify: runtime_flags.gc_verify,
        gc_worker: runtime_flags.gc_worker,
        gc_young_size: runtime_flags.gc_young_size,
        gc_semi_ratio: None,
        gc: Some(decode_collector_name(metadata::gc_collector())),
        min_heap_size: runtime_flags.min_heap_size,
        max_heap_size: runtime_flags.max_heap_size,
        code_size: None,
        readonly_size: None,
        disable_tlab: runtime_flags.disable_tlab,
        disable_barrier: false,
        snapshot_on_oom: None,
        target_arch: TargetArch::host(),
    }
}

fn parse_runtime_flags_from_env() -> Result<RuntimeFlags, i32> {
    let dora_flags = std::env::var("DORA_FLAGS").unwrap_or_default();
    let args = match shlex::split(&dora_flags) {
        Some(args) => args,
        None => {
            eprintln!("DORA_FLAGS: invalid shell quoting");
            return Err(1);
        }
    };
    // try_parse_from expects argv[0] (program name) as the first element.
    let args = std::iter::once(String::new()).chain(args);
    match RuntimeFlags::try_parse_from(args) {
        Ok(runtime_flags) => Ok(runtime_flags),
        Err(e) => {
            e.print().ok();
            Err(1)
        }
    }
}

#[unsafe(export_name = "dora_aot_main")]
pub extern "C" fn dora_aot_main(argc: c_int, argv: *const *const c_char) -> i32 {
    let program_args = program_args_from_argv(argc, argv, true);
    let runtime_flags = match parse_runtime_flags_from_env() {
        Ok(runtime_flags) => runtime_flags,
        Err(exit_code) => return exit_code,
    };

    let vm_flags = vm_flags_from_runtime_flags(&runtime_flags);

    let mut vm = VM::new(VmMode::Aot, decode_program(), vm_flags, program_args);

    let shape_metadata = metadata::shape_metadata();
    let strings = shape_metadata.strings;
    let shape_entries = shape_metadata.shape_entries;
    let created_shapes = initialize_shapes(
        &mut vm,
        strings,
        shape_metadata.shape_refs,
        shape_metadata.shape_kinds,
        shape_metadata.shape_fields,
        shape_metadata.shape_vtable_entries,
        shape_entries,
        shape_metadata.known_shape_entries,
    );

    let (global_memory_start, global_memory_end) = metadata::global_memory();
    initialize_global_memory(
        &mut vm,
        global_memory_start,
        global_memory_end,
        metadata::global_refs(),
    );

    let code_metadata = metadata::code_metadata();
    initialize_code_map(
        &mut vm,
        dora_entry_trampoline as *const u8,
        code_metadata.function_entries,
        code_metadata.gcpoint_entries,
        code_metadata.gcpoint_offsets,
        code_metadata.function_info_entries,
        strings,
        code_metadata.location_entries,
        code_metadata.inlined_function_entries,
    );

    set_vm(&vm);
    vm.gc.setup(&vm);

    patch_shape_slots(&vm, shape_entries, metadata::shape_slots(), &created_shapes);
    patch_string_slots(&vm, strings, metadata::string_slots());

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

#[unsafe(export_name = "dora_boots_compiler_main")]
pub extern "C" fn dora_boots_compiler_main(
    argc: c_int,
    argv: *const *const c_char,
    compile_address: *const u8,
) -> i32 {
    boots::dora_boots_compiler_main(argc, argv, compile_address)
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
