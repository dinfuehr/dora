use dora_bytecode::Program;

use crate::threads::current_thread;
use crate::vm::{VM, VmFlags, VmMode, clear_vm, execute_on_main, set_vm};

unsafe extern "C" {
    #[link_name = "_dora_main"]
    fn dora_main();

    #[link_name = "_dora_entry_trampoline"]
    fn dora_entry_trampoline(tld: usize, fct: *const u8) -> i32;
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

    let vm = VM::new(VmMode::Aot, empty_program(), vm_flags, Vec::new());
    set_vm(&vm);

    execute_on_main(|| {
        let tld = current_thread().tld_address();
        unsafe { dora_entry_trampoline(tld.to_usize(), dora_main as *const u8) };
    });

    vm.threads.join_all();
    vm.shutdown();
    clear_vm();

    0
}
