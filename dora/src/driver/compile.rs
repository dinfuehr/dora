use std::fs::{self, File};
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::driver::flags::CompileArgs;
use crate::driver::start::{Result, compile_boots, compile_program, finish_vm};
use dora_bytecode::lookup::lookup_fct;
use dora_runtime::{
    AotAssemblyKind, AotCompileArgs, AotCompileInputs, CollectorName, TargetArch, VM, VmFlags,
    VmMode, compile_boots_compiler_aot, compile_program_aot, dora_entry_trampoline,
    execute_on_main, install_boots_compiler_for_aot, set_vm, write_assembly,
};
use tempfile::NamedTempFile;

impl AotCompileArgs for CompileArgs {
    fn target_arch(&self) -> TargetArch {
        self.target.unwrap_or(TargetArch::host())
    }

    fn collector_name(&self) -> CollectorName {
        self.gc.unwrap_or(CollectorName::Swiper)
    }

    fn emit_graph(&self) -> Option<&str> {
        self.emit_graph.as_deref()
    }

    fn emit_graph_after_each_pass(&self) -> bool {
        self.emit_graph_after_each_pass
    }
}

pub fn command_compile(args: CompileArgs) -> Result<()> {
    let asm_file = if args.emit_asm {
        None
    } else {
        Some(tempfile::Builder::new().suffix(".s").tempfile()?)
    };

    let asm_path = match &asm_file {
        Some(tmp) => tmp.path().to_path_buf(),
        None => PathBuf::from(&args.output).with_extension("s"),
    };

    let (package_path, _opt_package_tempfile) = compile_to_package(&args)?;

    if args.internal_compile_boots {
        compile_package_in_process(&args, &package_path, &asm_path)?;
    } else {
        compile_package_using_compiler_binary(&args, &package_path, &asm_path)?;
    }

    if args.emit_asm {
        return Ok(());
    }

    link_assembly(&asm_path, &args.output)?;

    Ok(())
}

fn compile_to_package(args: &CompileArgs) -> Result<(PathBuf, Option<NamedTempFile>)> {
    if is_package_file(&args.file) {
        return Ok((PathBuf::from(&args.file), None));
    }

    let tempfile = tempfile::Builder::new().prefix("dora-program").tempfile()?;
    let package_file = tempfile.path().to_path_buf();

    let (prog, _compile_boots_entry) = if args.internal_compile_boots {
        let prog = compile_boots(&args.file, &args.common)?;
        let compile_fct_id = lookup_fct(&prog, "boots::interface::compile")
            .expect("boots::interface::compile not found");
        (prog, Some(compile_fct_id))
    } else {
        (compile_program(&args.file, &args.common, true)?, None)
    };

    let encoded_program = bincode::encode_to_vec(prog, bincode::config::standard())
        .expect("program serialization failed");
    fs::write(&package_file, encoded_program)?;

    Ok((package_file, Some(tempfile)))
}

fn compile_package_in_process(
    args: &CompileArgs,
    _package_path: &Path,
    asm_path: &Path,
) -> Result<()> {
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
        emit_bytecode_compiler: None,
        emit_compiler: false,
        emit_graph: args.emit_graph.clone(),
        emit_graph_after_each_pass: args.emit_graph_after_each_pass,
        emit_stubs: false,
        enable_perf: false,
        omit_bounds_check: false,
        emit_debug: None,
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
        min_heap_size: None,
        max_heap_size: None,
        code_size: None,
        readonly_size: None,
        disable_tlab: false,
        disable_barrier: false,
        snapshot_on_oom: None,
        target_arch: args.target.unwrap_or(TargetArch::host()),
    };

    let vm = VM::new(VmMode::Jit, prog, vm_flags, Vec::new());
    set_vm(&vm);
    install_boots_compiler_for_aot(&vm);

    let trampoline = dora_entry_trampoline::generate(&vm);
    let aot_inputs = AotCompileInputs::new(&vm, args, vm.boots_compile_fct_address());
    let target_arch = aot_inputs.target_arch();
    let aot = match compile_boots_entry {
        Some(compile_fct_id) => {
            execute_on_main(|| compile_boots_compiler_aot(&vm.program, compile_fct_id, aot_inputs))
        }
        None => execute_on_main(|| compile_program_aot(&vm.program, aot_inputs)),
    };
    let encoded_program = bincode::encode_to_vec(&vm.program, bincode::config::standard())
        .expect("program serialization failed");

    {
        let assembly_kind = if compile_boots_entry.is_some() {
            AotAssemblyKind::CompilerImage
        } else {
            AotAssemblyKind::Regular
        };
        let mut f = File::create(asm_path)?;
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

    Ok(())
}

fn compile_package_using_compiler_binary(
    args: &CompileArgs,
    package_path: &Path,
    asm_path: &Path,
) -> Result<()> {
    let exe_dir = current_exe_dir()?;
    let boots_compiler = exe_dir.join(format!(
        "dora-boots-compiler{}",
        std::env::consts::EXE_SUFFIX
    ));

    if !boots_compiler.exists() {
        return Err(format!("boots compiler not found at '{}'", boots_compiler.display()).into());
    }

    let mut command = Command::new(&boots_compiler);
    command.arg(package_path).arg("-o").arg(asm_path);

    if let Some(target) = args.target {
        command.arg("--target").arg(target_arch_name(target));
    }

    if let Some(gc) = args.gc {
        command.arg("--gc").arg(collector_name(gc));
    }

    if let Some(emit_graph) = &args.emit_graph {
        command.arg("--emit-graph").arg(emit_graph);
    }

    if args.emit_graph_after_each_pass {
        command.arg("--emit-graph-after-each-pass");
    }

    let status = command.status()?;
    if !status.success() {
        return Err("dora-boots-compiler failed".into());
    }

    Ok(())
}

fn link_assembly(asm_path: &Path, output: &str) -> Result<()> {
    // Find the runtime static library next to the current executable.
    let exe_dir = current_exe_dir()?;
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
        .arg(output)
        .status()?;

    if !status.success() {
        return Err("gcc failed".into());
    }

    Ok(())
}

fn is_package_file(path: &str) -> bool {
    path.ends_with(".dora-package")
}

fn current_exe_dir() -> Result<PathBuf> {
    Ok(std::env::current_exe()?
        .parent()
        .expect("no parent directory")
        .to_path_buf())
}

fn find_staticlib(exe_dir: &Path, crate_name: &str) -> Option<PathBuf> {
    let path = exe_dir.join(format!("lib{}.a", crate_name));
    if path.exists() { Some(path) } else { None }
}

fn target_arch_name(target: TargetArch) -> &'static str {
    match target {
        TargetArch::X64 => "x64",
        TargetArch::Arm64 => "arm64",
    }
}

fn collector_name(gc: CollectorName) -> &'static str {
    match gc {
        CollectorName::Zero => "zero",
        CollectorName::Copy => "copy",
        CollectorName::Sweep => "sweep",
        CollectorName::Swiper => "swiper",
    }
}
