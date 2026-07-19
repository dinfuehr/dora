use std::path::PathBuf;

use clap::Parser;
use dora_bytecode::{Program, lookup::lookup_fct, read_program_from_file};
use dora_runtime::{
    AotAssemblyKind, AotCompileArgs, AotCompileInputs, CollectorName, CompilerInvocation,
    TargetArch, compile_boots_compiler_aot, compile_program_aot, compile_test_runner,
    dora_entry_trampoline, parse_collector, parse_target_arch, write_assembly,
};

#[derive(Parser)]
struct Args {
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

impl AotCompileArgs for Args {
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

fn main() {
    let args = Args::parse();

    if let Err(err) = run(args) {
        eprintln!("{err}");
        std::process::exit(1);
    }
}

fn run(args: Args) -> Result<(), String> {
    let program = read_program_from_file(&args.input)?;
    compile_package_with_cannon(program, &args)
}

fn compile_package_with_cannon(program: Program, args: &Args) -> Result<(), String> {
    let aot_inputs = AotCompileInputs::from_program(
        &program,
        args,
        CompilerInvocation::external(dora_cannon_compiler::compile),
    );
    let target_arch = aot_inputs.target_arch();
    let aot = if args.internal_compile_boots && args.test {
        compile_test_runner(&program, program.program_package_id, aot_inputs)
    } else if args.internal_compile_boots {
        let compile_fct_id = lookup_fct(&program, "program::interface::compile")
            .expect("program::interface::compile not found");
        let compile_trait_object_thunk_fct_id =
            lookup_fct(&program, "program::interface::compile_trait_object_thunk")
                .expect("program::interface::compile_trait_object_thunk not found");
        compile_boots_compiler_aot(
            &program,
            &[compile_fct_id, compile_trait_object_thunk_fct_id],
            aot_inputs,
        )
    } else if args.test {
        compile_test_runner(&program, program.program_package_id, aot_inputs)
    } else {
        compile_program_aot(&program, aot_inputs)
    };
    let encoded_program = bincode::encode_to_vec(&program, bincode::config::standard())
        .expect("program serialization failed");
    let trampoline = dora_entry_trampoline::generate_aot(target_arch);
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

    Ok(())
}
