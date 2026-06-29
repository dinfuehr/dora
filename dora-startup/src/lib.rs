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
// Machine code computes compressed shape pointers at allocation sites by
// materializing the Shape descriptor address and dora_aot_shape_base address,
// then subtracting base from descriptor. The codegen combines the compressed
// pointer with the sentinel and remembered bit to form the full object header
// word.
//
// Trait object shapes carry vtable entries — function pointers resolved
// by the linker — stored inline after the static Shape descriptor so
// virtual dispatch can load through the Shape address directly.
//
//   .text (RX)
//   +----------------+
//   | shape address -+----> .dora.shapes descriptor
//   | base address --+----> dora_aot_shape_base
//   +----------------+
//
//   .dora.shapes (R)
//   (Shape descriptors)
//   +----------------+
//   | visitor        |
//   | refs_data -----+----> .dora.shape_refs (R), or tagged bitmap when bit 0 is set
//   | refs_len       |      (flat i32 offsets)
//   | instance_size  |
//   | element_size   |
//   | vtable_len     |
//   | kind_data -----+----> .dora.shape_kinds (R)
//   | kind_len       |      (encoded ShapeKind values)
//   | inline vtable  |
//   +----------------+
//
//   .dora.known_shapes (R)
//   (AotKnownShapeEntry)
//   +-------------------+
//   | kind (e.g. Code)  |    maps vm.known.* fields to shape descriptors
//   | shape_ptr --------+---> .dora.shapes descriptor
//   +-------------------+
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
//                               dora_global_memory
//                                    ...
//                               dora_global_memory_end
//
// Functions / GC stack maps
// -------------------------
// Startup rebuilds function GC metadata and registers code ranges in
// vm.code_map via initialize_code_map().
//
//   .text (RX)                  .dora.functions (R)              .dora.gcpoints (R)
//   +-------------------+       (AotFunctionEntry)               (AotGcPointEntry)
//   | dora_fct0 ...     |<--+   +--------------------------+    +----------------------+
//   |      ...          |   +---| code_start               |    | pc_offset            |
//   | dora_fct0_end     |<------| code_end                 |    | offsets_start -------+---+
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
use dora_bytecode::{FunctionId, Program, display_fct};
use dora_runtime::startup::{
    AotTestEntry, current_thread_tld_address, initialize_code_map, initialize_global_memory,
    initialize_shapes, patch_string_slots,
};
use dora_runtime::{CollectorName, MemSize, VM, VmFlags, clear_vm, execute_on_main, set_vm};
use std::ffi::CStr;
use std::io::Write;
use std::os::raw::{c_char, c_int};

mod boots;
mod metadata;

unsafe extern "C" {
    #[link_name = "dora_entry_trampoline"]
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
        gc_stress: runtime_flags.gc_stress,
        gc_stress_minor: runtime_flags.gc_stress_minor,
        gc_stats: false,
        gc_verbose: runtime_flags.gc_verbose,
        gc_verify: runtime_flags.gc_verify,
        gc_worker: runtime_flags.gc_worker,
        gc_young_size: runtime_flags.gc_young_size,
        gc: Some(decode_collector_name(metadata::gc_collector())),
        min_heap_size: runtime_flags.min_heap_size,
        max_heap_size: runtime_flags.max_heap_size,
        readonly_size: None,
        disable_tlab: runtime_flags.disable_tlab,
        snapshot_on_oom: None,
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
pub extern "C" fn dora_aot_main(
    argc: c_int,
    argv: *const *const c_char,
    main_address: *const u8,
) -> i32 {
    run_aot(argc, argv, AotStartupEntry::Main(main_address))
}

#[unsafe(export_name = "dora_aot_test_main")]
pub extern "C" fn dora_aot_test_main(argc: c_int, argv: *const *const c_char) -> i32 {
    run_aot(argc, argv, AotStartupEntry::Tests)
}

#[derive(Clone, Copy)]
enum AotStartupEntry {
    Main(*const u8),
    Tests,
}

fn run_aot(argc: c_int, argv: *const *const c_char, entry: AotStartupEntry) -> i32 {
    let program_args = program_args_from_argv(argc, argv, true);
    let runtime_flags = match parse_runtime_flags_from_env() {
        Ok(runtime_flags) => runtime_flags,
        Err(exit_code) => return exit_code,
    };

    let vm_flags = vm_flags_from_runtime_flags(&runtime_flags);

    let mut vm = VM::new(decode_program(), vm_flags, program_args);

    let shape_metadata = metadata::shape_metadata();
    let strings = shape_metadata.strings;
    initialize_shapes(
        &mut vm,
        shape_metadata.shape_base,
        shape_metadata.shape_size,
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

    patch_string_slots(&vm, strings, metadata::string_slots());

    let exit_code = execute_on_main(|| match entry {
        AotStartupEntry::Main(main_address) => unsafe {
            dora_entry_trampoline(current_thread_tld_address(), main_address)
        },
        AotStartupEntry::Tests => {
            if run_tests(&vm, code_metadata.test_entries) {
                0
            } else {
                1
            }
        }
    });

    let main_returns_unit = match entry {
        AotStartupEntry::Main(_) => {
            let main_fct_id = vm.program.main_fct_id.expect("missing AOT main function");
            vm.fct(main_fct_id).return_type.is_unit()
        }
        AotStartupEntry::Tests => false,
    };

    std::io::stdout().flush().ok();

    vm.threads.join_all();
    vm.shutdown();
    clear_vm();

    if main_returns_unit { 0 } else { exit_code }
}

fn run_tests(vm: &VM, test_entries: &[AotTestEntry]) -> bool {
    let mut tests = 0;
    let mut passed = 0;

    for test_entry in test_entries {
        let fct_id = FunctionId::from(test_entry.fct_id as usize);
        debug_assert!(vm.program.fct(fct_id).is_test);

        tests += 1;

        print!("test {} ... ", display_fct(&vm.program, fct_id));

        unsafe {
            dora_entry_trampoline(current_thread_tld_address(), test_entry.code_start);
        }

        passed += 1;
        println!("ok");
    }

    println!(
        "{} tests executed; {} passed; {} failed.",
        tests,
        passed,
        tests - passed
    );

    tests == passed
}

#[unsafe(export_name = "dora_boots_compiler_main")]
pub extern "C" fn dora_boots_compiler_main(
    argc: c_int,
    argv: *const *const c_char,
    compile_address: *const u8,
) -> i32 {
    eprintln!("Boots startup trace: wrapper entry argc={argc} compile_address={compile_address:p}");
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
