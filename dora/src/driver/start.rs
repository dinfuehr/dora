use clap::Parser;

use crate::driver::build::command_build;
use crate::driver::compile::command_compile;
use crate::driver::flags::{Cli, Command, CommonFlags};
use crate::driver::init::command_init;
use dora_bytecode::Program;
use dora_frontend as language;
use dora_frontend::sema::{Sema, SemaCreationParams};

pub type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

pub fn start() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Command::Build(args) => command_build(args),
        Command::Compile(args) => command_compile(args),
        Command::Init(args) => command_init(args),
    }
}

pub fn compile_program(sema_params: SemaCreationParams, common: &CommonFlags) -> Result<Program> {
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

pub fn report_errors(sa: &Sema, report_all_warnings: bool) -> bool {
    if sa.diag.borrow().has_errors_or_warnings() {
        sa.diag.borrow_mut().dump(&sa, report_all_warnings);
        sa.diag.borrow().has_errors()
    } else {
        false
    }
}
