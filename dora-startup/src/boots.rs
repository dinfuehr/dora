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
    eprintln!(
        "Boots startup trace: unconditional entry argc={argc} compile_address={compile_address:p}"
    );
    let trace = boots_trace_enabled();
    boots_trace(
        trace,
        format_args!("entry argc={argc} compile_address={compile_address:p}"),
    );

    let args = match parse_args(argc, argv) {
        Ok(args) => args,
        Err(exit_code) => return exit_code,
    };
    boots_trace(
        trace,
        format_args!(
            "args parsed input={} output={} internal_compile_boots={} test={} target={:?}",
            args.input.display(),
            args.output.display(),
            args.internal_compile_boots,
            args.test,
            args.target
        ),
    );

    let runtime_flags = match super::parse_runtime_flags_from_env() {
        Ok(runtime_flags) => runtime_flags,
        Err(exit_code) => return exit_code,
    };
    boots_trace(trace, format_args!("runtime flags parsed"));

    boots_trace(trace, format_args!("read input program start"));
    let input_program = match read_program_from_file(&args.input) {
        Ok(program) => program,
        Err(err) => {
            eprintln!("{err}");
            return 1;
        }
    };
    boots_trace(
        trace,
        format_args!(
            "read input program done functions={}",
            input_program.functions.len()
        ),
    );

    let vm_flags = super::vm_flags_from_runtime_flags(&runtime_flags);
    boots_trace(trace, format_args!("vm new start"));
    let mut vm = VM::new(super::decode_program(), vm_flags, Vec::new());
    boots_trace(trace, format_args!("vm new done"));

    boots_trace(trace, format_args!("initialize shapes start"));
    let shape_metadata = metadata::shape_metadata();
    let strings = shape_metadata.strings;
    initialize_shapes(
        &mut vm,
        shape_metadata.shape_base,
        shape_metadata.shape_size,
        shape_metadata.known_shape_entries,
    );
    boots_trace(trace, format_args!("initialize shapes done"));

    boots_trace(trace, format_args!("initialize globals start"));
    let (global_memory_start, global_memory_end) = metadata::global_memory();
    initialize_global_memory(
        &mut vm,
        global_memory_start,
        global_memory_end,
        metadata::global_refs(),
    );
    boots_trace(trace, format_args!("initialize globals done"));

    boots_trace(trace, format_args!("initialize code map start"));
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
    boots_trace(trace, format_args!("initialize code map done"));

    boots_trace(trace, format_args!("set vm start"));
    set_vm(&vm);
    boots_trace(trace, format_args!("set vm done"));

    boots_trace(trace, format_args!("gc setup start"));
    vm.gc.setup(&vm);
    boots_trace(trace, format_args!("gc setup done"));

    boots_trace(trace, format_args!("patch string slots start"));
    patch_string_slots(&vm, strings, metadata::string_slots());
    boots_trace(trace, format_args!("patch string slots done"));

    boots_trace(trace, format_args!("compiler invocation start"));
    let compiler_invocation = CompilerInvocation::new(dora_boots_compiler::BootsAotBackend::new(
        compile_address,
        dora_entry_trampoline,
    ));
    boots_trace(trace, format_args!("compiler invocation done"));

    boots_trace(trace, format_args!("aot inputs start"));
    let aot_inputs = AotCompileInputs::from_program(&input_program, &args, compiler_invocation);
    let target_arch = aot_inputs.target_arch();
    boots_trace(
        trace,
        format_args!("aot inputs done target_arch={target_arch:?}"),
    );

    let aot = if args.internal_compile_boots && args.test {
        boots_trace(
            trace,
            format_args!("execute_on_main compile boots tests start"),
        );
        let aot = execute_on_main(|| {
            boots_trace(trace, format_args!("compile boots tests start"));
            let aot =
                compile_test_runner(&input_program, input_program.program_package_id, aot_inputs);
            boots_trace(trace, format_args!("compile boots tests done"));
            aot
        });
        boots_trace(
            trace,
            format_args!("execute_on_main compile boots tests done"),
        );
        aot
    } else if args.internal_compile_boots {
        boots_trace(trace, format_args!("lookup compile function start"));
        let compile_fct_id = lookup_fct(&input_program, "program::interface::compile")
            .expect("program::interface::compile not found");
        boots_trace(
            trace,
            format_args!("lookup compile function done id={compile_fct_id:?}"),
        );
        boots_trace(
            trace,
            format_args!("execute_on_main compile boots compiler start"),
        );
        let aot = execute_on_main(|| {
            boots_trace(trace, format_args!("compile boots compiler start"));
            let aot = compile_boots_compiler_aot(&input_program, compile_fct_id, aot_inputs);
            boots_trace(trace, format_args!("compile boots compiler done"));
            aot
        });
        boots_trace(
            trace,
            format_args!("execute_on_main compile boots compiler done"),
        );
        aot
    } else if args.test {
        boots_trace(trace, format_args!("execute_on_main compile tests start"));
        let aot = execute_on_main(|| {
            boots_trace(trace, format_args!("compile tests start"));
            let aot =
                compile_test_runner(&input_program, input_program.program_package_id, aot_inputs);
            boots_trace(trace, format_args!("compile tests done"));
            aot
        });
        boots_trace(trace, format_args!("execute_on_main compile tests done"));
        aot
    } else {
        boots_trace(trace, format_args!("execute_on_main compile program start"));
        let aot = execute_on_main(|| {
            boots_trace(trace, format_args!("compile program start"));
            let aot = compile_program_aot(&input_program, aot_inputs);
            boots_trace(trace, format_args!("compile program done"));
            aot
        });
        boots_trace(trace, format_args!("execute_on_main compile program done"));
        aot
    };
    boots_trace(trace, format_args!("encode input program start"));
    let encoded_program = bincode::encode_to_vec(&input_program, bincode::config::standard())
        .expect("program serialization failed");
    boots_trace(trace, format_args!("encode input program done"));

    boots_trace(trace, format_args!("generate trampoline start"));
    let trampoline = dora_entry_trampoline_codegen::generate_aot(target_arch);
    boots_trace(trace, format_args!("generate trampoline done"));
    let assembly_kind = if args.internal_compile_boots && !args.test {
        AotAssemblyKind::CompilerImage
    } else if args.test {
        AotAssemblyKind::Test
    } else {
        AotAssemblyKind::Regular
    };
    let assembly_kind_name = match assembly_kind {
        AotAssemblyKind::Regular => "regular",
        AotAssemblyKind::Test => "test",
        AotAssemblyKind::CompilerImage => "compiler-image",
    };

    boots_trace(
        trace,
        format_args!("write assembly start kind={assembly_kind_name}"),
    );
    write_assembly(
        &args.output,
        &aot,
        &encoded_program,
        &trampoline.code,
        target_arch,
        assembly_kind,
    );
    boots_trace(trace, format_args!("write assembly done"));

    boots_trace(trace, format_args!("vm teardown start"));
    vm.threads.join_all();
    vm.shutdown();
    clear_vm();
    boots_trace(trace, format_args!("vm teardown done"));

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

fn boots_trace_enabled() -> bool {
    std::env::var("DORA_AOT_TRACE_COMPILE").as_deref() == Ok("1")
        || std::env::var("DORA_BOOTS_TRACE").as_deref() == Ok("1")
}

fn boots_trace(enabled: bool, args: std::fmt::Arguments<'_>) {
    if enabled {
        eprintln!("Boots startup trace: {args}");
    }
}
