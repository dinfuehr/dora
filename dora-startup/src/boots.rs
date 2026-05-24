use clap::Parser;
use dora_bytecode::Program;
use dora_runtime::startup::{
    initialize_code_map, initialize_global_memory, initialize_shapes, patch_shape_slots,
    patch_string_slots,
};
use dora_runtime::{
    AotAssemblyKind, AotCompileArgs, AotCompileInputs, CollectorName, TargetArch, VM, VmMode,
    clear_vm, compile_program_aot, dora_entry_trampoline as dora_entry_trampoline_codegen,
    execute_on_main, set_vm, write_assembly,
};
use std::fs::File;
use std::io::{BufWriter, Write};
use std::os::raw::{c_char, c_int};
use std::path::{Path, PathBuf};

use super::metadata;

#[derive(Parser)]
struct BootsCompilerArgs {
    input: PathBuf,

    #[arg(short = 'o')]
    output: PathBuf,

    /// Target architecture (x64, arm64; default: host)
    #[arg(long, value_parser = parse_target_arch)]
    target: Option<TargetArch>,

    /// Switch GC (zero, copy, sweep, swiper)
    #[arg(long, value_parser = parse_collector)]
    gc: Option<CollectorName>,

    /// Emits graph for function
    #[arg(long, value_name = "FCT")]
    emit_graph: Option<String>,

    /// Emits graph after each pass
    #[arg(long)]
    emit_graph_after_each_pass: bool,
}

impl AotCompileArgs for BootsCompilerArgs {
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

pub fn dora_boots_compiler_main(
    argc: c_int,
    argv: *const *const c_char,
    compile_address: *const u8,
) -> i32 {
    let args = match parse_args(argc, argv) {
        Ok(args) => args,
        Err(exit_code) => return exit_code,
    };
    let runtime_flags = match super::parse_runtime_flags_from_env() {
        Ok(runtime_flags) => runtime_flags,
        Err(exit_code) => return exit_code,
    };
    let program = match read_program_from_file(&args.input) {
        Ok(program) => program,
        Err(err) => {
            eprintln!("{err}");
            return 1;
        }
    };

    let vm_flags = super::vm_flags_from_runtime_flags(&runtime_flags);
    let mut vm = VM::new(VmMode::Jit, program, vm_flags, Vec::new());

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
        super::dora_entry_trampoline as *const u8,
        code_metadata.function_entries,
        code_metadata.gcpoint_entries,
        code_metadata.gcpoint_offsets,
        code_metadata.function_info_entries,
        strings,
        code_metadata.location_entries,
        code_metadata.inlined_function_entries,
    );

    set_vm(&vm);

    patch_shape_slots(&vm, shape_entries, metadata::shape_slots(), &created_shapes);
    patch_string_slots(&vm, strings, metadata::string_slots());

    let aot_inputs = AotCompileInputs::new(&vm, &args, compile_address);
    let target_arch = aot_inputs.target_arch();
    let aot = execute_on_main(|| compile_program_aot(&vm.program, aot_inputs));
    let encoded_program = bincode::encode_to_vec(&vm.program, bincode::config::standard())
        .expect("program serialization failed");
    let trampoline = dora_entry_trampoline_codegen::generate(&vm);

    let write_result = (|| {
        let output = File::create(&args.output)?;
        let mut output = BufWriter::new(output);
        write_assembly(
            &mut output,
            &aot,
            &encoded_program,
            &trampoline.code,
            target_arch,
            AotAssemblyKind::Regular,
        )?;
        output.flush()
    })();

    vm.threads.join_all();
    vm.shutdown();
    clear_vm();

    if let Err(err) = write_result {
        eprintln!(
            "failed to write assembly to '{}': {err}",
            args.output.display()
        );
        return 1;
    }

    0
}

fn parse_args(argc: c_int, argv: *const *const c_char) -> Result<BootsCompilerArgs, i32> {
    let args = super::program_args_from_argv(argc, argv, false);
    match BootsCompilerArgs::try_parse_from(args) {
        Ok(args) => Ok(args),
        Err(e) => {
            e.print().ok();
            Err(1)
        }
    }
}

fn decode_program_from_bytes(bytes: &[u8]) -> Program {
    let config = bincode::config::standard();
    let (program, decoded_len): (Program, usize) =
        bincode::decode_from_slice(bytes, config).expect("failed to decode AOT program");
    assert_eq!(
        decoded_len,
        bytes.len(),
        "encoded AOT program has trailing bytes"
    );
    program
}

fn read_program_from_file(path: &Path) -> Result<Program, String> {
    let encoded_program = std::fs::read(path).map_err(|err| {
        format!(
            "failed to read encoded program input '{}': {err}",
            path.display()
        )
    })?;

    if encoded_program.is_empty() {
        return Err(format!(
            "missing encoded program input '{}'",
            path.display()
        ));
    }

    Ok(decode_program_from_bytes(&encoded_program))
}

fn parse_collector(s: &str) -> Result<CollectorName, String> {
    match s {
        "zero" => Ok(CollectorName::Zero),
        "copy" => Ok(CollectorName::Copy),
        "sweep" => Ok(CollectorName::Sweep),
        "swiper" => Ok(CollectorName::Swiper),
        _ => Err(format!(
            "unknown collector '{}', expected: zero, copy, sweep, swiper",
            s
        )),
    }
}

fn parse_target_arch(s: &str) -> Result<TargetArch, String> {
    match s {
        "x64" | "x86_64" | "x86-64" => Ok(TargetArch::X64),
        "arm64" | "aarch64" => Ok(TargetArch::Arm64),
        _ => Err(format!("unknown target '{}', expected: x64, arm64", s)),
    }
}
