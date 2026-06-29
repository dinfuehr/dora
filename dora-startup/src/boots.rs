use clap::Parser;
use dora_bytecode::{lookup::lookup_fct, read_program_from_file};
use dora_runtime::startup::{
    initialize_code_map, initialize_global_memory, initialize_shapes, patch_string_slots,
};
use dora_runtime::{
    AotAssemblyKind, AotCompileArgs, AotCompileInputs, CollectorName, CompilerInvocation,
    TargetArch, VM, clear_vm, compile_boots_compiler_aot, compile_program_aot, compile_test_runner,
    dora_entry_trampoline as dora_entry_trampoline_codegen, execute_on_main, parse_collector,
    parse_target_arch, set_vm, write_assembly,
};
use std::os::raw::{c_char, c_int};
use std::path::PathBuf;

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

    /// Compile tests and use the unit test runner as entry point
    #[arg(long)]
    test: bool,

    /// Internal: compile the Boots compiler image or Boots package tests
    #[arg(long, hide = true)]
    internal_compile_boots: bool,
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

    let input_program = match read_program_from_file(&args.input) {
        Ok(program) => program,
        Err(err) => {
            eprintln!("{err}");
            return 1;
        }
    };

    let vm_flags = super::vm_flags_from_runtime_flags(&runtime_flags);
    let mut vm = VM::new(super::decode_program(), vm_flags, Vec::new());

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
    let dora_entry_trampoline = super::dora_entry_trampoline as *const u8;
    initialize_code_map(
        &mut vm,
        dora_entry_trampoline,
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

    let compiler_invocation = CompilerInvocation::new(dora_boots_compiler::BootsAotBackend::new(
        compile_address,
        dora_entry_trampoline,
    ));

    let aot_inputs = AotCompileInputs::from_program(&input_program, &args, compiler_invocation);
    let target_arch = aot_inputs.target_arch();

    let aot = if args.internal_compile_boots && args.test {
        execute_on_main(|| {
            compile_test_runner(&input_program, input_program.program_package_id, aot_inputs)
        })
    } else if args.internal_compile_boots {
        let compile_fct_id = lookup_fct(&input_program, "program::interface::compile")
            .expect("program::interface::compile not found");
        execute_on_main(|| compile_boots_compiler_aot(&input_program, compile_fct_id, aot_inputs))
    } else if args.test {
        execute_on_main(|| {
            compile_test_runner(&input_program, input_program.program_package_id, aot_inputs)
        })
    } else {
        execute_on_main(|| compile_program_aot(&input_program, aot_inputs))
    };
    let encoded_program = bincode::encode_to_vec(&input_program, bincode::config::standard())
        .expect("program serialization failed");

    let trampoline = dora_entry_trampoline_codegen::generate_aot(target_arch);
    let assembly_kind = if args.internal_compile_boots && !args.test {
        AotAssemblyKind::CompilerImage
    } else if args.test {
        AotAssemblyKind::Test
    } else {
        AotAssemblyKind::Regular
    };

    write_assembly(
        &args.output,
        &aot,
        &encoded_program,
        &trampoline.code,
        target_arch,
        assembly_kind,
    );

    vm.threads.join_all();
    vm.shutdown();
    clear_vm();

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
