use std::fs;
use std::path::PathBuf;

use clap::Parser;

use crate::driver::build::command_build;
use crate::driver::flags::{Cli, Command, CommonFlags, create_sema_params};
use crate::driver::run::command_run;
use crate::driver::test::command_test;
use dora_bytecode::Program;
use dora_frontend as language;
use dora_frontend::sema::Sema;
use dora_runtime::{VM, clear_vm};

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

pub fn start() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Some(Command::Run(args)) => command_run(args),
        Some(Command::Test(args)) => command_test(args),
        Some(Command::Build(args)) => command_build(args),
        None => command_run(cli.run_args),
    }
}

pub fn compile_or_load(file: &str, common: &CommonFlags, include_boots: bool) -> Result<Program> {
    if file.ends_with(".dora-package") {
        decode_input_program(file)
    } else {
        compile_program(file, common, include_boots)
    }
}

pub fn compile_program(file: &str, common: &CommonFlags, include_boots: bool) -> Result<Program> {
    let sema_params = create_sema_params(PathBuf::from(file), common.packages(), include_boots);

    let mut sa = Sema::new(sema_params);

    let success = language::check_program(&mut sa);
    assert_eq!(success, !sa.diag.borrow().has_errors());

    if report_errors(&sa, common.report_all_warnings) {
        return Err("compilation failed".into());
    }

    if let Some(ref filter) = common.emit_ast {
        language::emit_ast(&sa, filter);
    }

    let prog = language::emit_program(sa);

    if let Some(ref filter) = common.emit_bytecode {
        language::emit_bytecode(&prog, filter);
    }

    Ok(prog)
}

fn decode_input_program(file: &str) -> Result<Program> {
    let encoded_program = fs::read(file)?;

    let config = bincode::config::standard();
    let (decoded_prog, decoded_len): (Program, usize) =
        bincode::decode_from_slice(&encoded_program, config)?;
    assert_eq!(decoded_len, encoded_program.len());

    Ok(decoded_prog)
}

pub fn encode_and_decode_for_testing(prog: Program) -> Program {
    let config = bincode::config::standard();
    let encoded_program = bincode::encode_to_vec(prog, config).expect("serialization failed");
    let (decoded_prog, decoded_len): (Program, usize) =
        bincode::decode_from_slice(&encoded_program, config).expect("serialization failure");
    assert_eq!(decoded_len, encoded_program.len());

    decoded_prog
}

pub fn report_errors(sa: &Sema, report_all_warnings: bool) -> bool {
    if sa.diag.borrow().has_errors_or_warnings() {
        sa.diag.borrow_mut().dump(&sa, report_all_warnings);
        sa.diag.borrow().has_errors()
    } else {
        false
    }
}

pub fn finish_vm(vm: &VM) {
    vm.threads.join_all();
    vm.shutdown();

    if vm.flags.gc_stats {
        let duration = vm.startup_time().elapsed();
        vm.dump_gc_summary(duration.as_secs_f32() * 1000f32);
    }

    clear_vm();
}
