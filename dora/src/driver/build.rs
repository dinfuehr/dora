use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

use crate::driver::flags::{BuildArgs, CommonFlags};
use crate::driver::start::{compile_program, report_errors, Result};
use dora_frontend as language;
use dora_frontend::sema::{Sema, SemaCreationParams};

pub fn command_build(args: BuildArgs) -> Result<()> {
    let prog = if args.stdlib {
        compile_stdlib(&args.file, &args.common)?
    } else {
        compile_program(&args.file, &args.common, false)?
    };

    let config = bincode::config::standard();
    let encoded_program = bincode::encode_to_vec(prog, config).expect("serialization failed");

    let output = args.output.unwrap_or_else(|| default_output(&args.file));
    write_program_into_file(&encoded_program, &output)?;

    Ok(())
}

fn default_output(input: &str) -> String {
    let path = std::path::Path::new(input);
    let stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("out");
    format!("{}.dora-package", stem)
}

fn compile_stdlib(
    file: &str,
    common: &CommonFlags,
) -> Result<dora_bytecode::Program> {
    let sema_params = SemaCreationParams::new()
        .set_program_path(PathBuf::from(file))
        .set_standard_library(true);
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

fn write_program_into_file(prog: &[u8], file: &str) -> Result<()> {
    let path = PathBuf::from(file);

    let mut f = File::create(&path)?;
    f.write_all(prog)?;
    Ok(())
}
