use std::fs::File;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::driver::flags::CompileArgs;
use crate::driver::start::{Result, compile_boots, compile_program, finish_vm};
use dora_bytecode::lookup::lookup_fct;
use dora_runtime::{
    AotAssemblyKind, AotCompileInputs, TargetArch, VM, VmFlags, VmMode,
    compile_boots_compiler as compile_boots_compiler_aot, compile_program as compile_program_aot,
    dora_entry_trampoline, execute_on_main, set_vm, write_assembly,
};

pub fn command_compile(args: CompileArgs) -> Result<()> {
    let file = args.file.as_str();
    let (prog, compile_boots_entry) = if args.internal_compile_boots {
        let prog = compile_boots(file, &args.common)?;
        let compile_fct_id = lookup_fct(&prog, "boots::interface::compile")
            .expect("boots::interface::compile not found");
        (prog, Some(compile_fct_id))
    } else {
        (compile_program(file, &args.common, true)?, None)
    };

    let vm_flags = VmFlags {
        emit_asm: None,
        emit_asm_file: None,
        emit_asm_boots: false,
        emit_bytecode_compiler: None,
        emit_bytecode_boots: false,
        emit_compiler: false,
        emit_graph: args.emit_graph.clone(),
        emit_graph_after_each_pass: args.emit_graph_after_each_pass,
        emit_stubs: false,
        enable_perf: false,
        always_boots: !args.internal_compile_boots,
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
        gc: args.gc,
        compiler: None,
        min_heap_size: None,
        max_heap_size: None,
        code_size: None,
        readonly_size: None,
        disable_tlab: false,
        disable_barrier: false,
        bootstrap_compiler: false,
        snapshot_on_oom: None,
        target_arch: args.target.unwrap_or(TargetArch::host()),
    };

    let vm = VM::new(VmMode::Jit, prog, vm_flags, Vec::new());
    set_vm(&vm);
    vm.compile_boots_aot();

    let target_arch = vm.flags.target_arch;
    let trampoline = dora_entry_trampoline::generate(&vm);
    let assembly_kind = if compile_boots_entry.is_some() {
        AotAssemblyKind::CompilerImage
    } else {
        AotAssemblyKind::Regular
    };
    let boots_compile_fct_address = vm.boots_compile_fct_address();
    let aot = match compile_boots_entry {
        Some(compile_fct_id) => execute_on_main(|| {
            let aot_inputs = AotCompileInputs::from_vm(&vm);
            compile_boots_compiler_aot(&vm, compile_fct_id, boots_compile_fct_address, aot_inputs)
        }),
        None => execute_on_main(|| {
            let aot_inputs = AotCompileInputs::from_vm(&vm);
            compile_program_aot(&vm, &vm.program, boots_compile_fct_address, aot_inputs)
        }),
    };
    let encoded_program = bincode::encode_to_vec(&vm.program, bincode::config::standard())
        .expect("program serialization failed");

    let asm_file = if args.emit_asm {
        None
    } else {
        Some(tempfile::Builder::new().suffix(".s").tempfile()?)
    };

    let asm_path = match &asm_file {
        Some(tmp) => tmp.path().to_path_buf(),
        None => PathBuf::from(&args.output).with_extension("s"),
    };

    {
        let mut f = File::create(&asm_path)?;
        write_assembly(
            &mut f,
            &aot,
            &encoded_program,
            &trampoline.code,
            target_arch,
            assembly_kind,
        )?;
    }

    finish_vm(&vm);

    if args.emit_asm {
        return Ok(());
    }

    // Find the runtime static library next to the current executable.
    let exe_dir = std::env::current_exe()?
        .parent()
        .expect("no parent directory")
        .to_path_buf();
    let runtime_lib = find_staticlib(&exe_dir, "dora_runtime")
        .ok_or_else(|| "runtime library not found in target directory".to_string())?;
    let startup_lib = find_staticlib(&exe_dir, "dora_startup")
        .ok_or_else(|| "startup library not found in target directory".to_string())?;

    let cc = std::env::var("CC").unwrap_or_else(|_| "gcc".to_string());
    let status = Command::new(&cc)
        .arg(&asm_path)
        .arg(&startup_lib)
        .arg(&runtime_lib)
        .arg("-lpthread")
        .arg("-ldl")
        .arg("-lm")
        .arg("-o")
        .arg(&args.output)
        .status()?;

    if !status.success() {
        return Err("gcc failed".into());
    }

    Ok(())
}

fn find_staticlib(exe_dir: &Path, crate_name: &str) -> Option<PathBuf> {
    let path = exe_dir.join(format!("lib{}.a", crate_name));
    if path.exists() { Some(path) } else { None }
}
