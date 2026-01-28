use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

use crate::driver::flags::{BuildArgs, CommonFlags};
use crate::driver::start::{Result, compile_program, report_errors};
use dora_frontend as language;
use dora_frontend::sema::{Sema, SemaCreationParams};

pub fn command_build(args: BuildArgs) -> Result<()> {
    if args.separate_stdlib_check {
        compile_std_library(&args.common)?;
    }

    let prog = compile_program(&args.file, &args.common, false)?;

    let config = bincode::config::standard();
    let encoded_program = bincode::encode_to_vec(prog, config).expect("serialization failed");

    write_program_into_file(&encoded_program, &args.output)?;

    Ok(())
}

fn compile_std_library(common: &CommonFlags) -> Result<()> {
    let path = get_stdlib_path(common).ok_or("standard library not found")?;

    let sema_params = SemaCreationParams::new()
        .set_program_path(path)
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

    if let Some(ref filter) = common.emit_bytecode {
        let prog = language::emit_program(sa);
        language::emit_bytecode(&prog, filter);
    }

    Ok(())
}

fn get_stdlib_path(common: &CommonFlags) -> Option<PathBuf> {
    for (name, path) in common.packages() {
        if name == "std" {
            return Some(path);
        }
    }

    find_pkg_file("std/std.dora")
}

fn find_pkg_file(relative_pkg_file: &str) -> Option<PathBuf> {
    const RELATIVE_PKG_DIRS: &[&str] = &["share/dora/pkgs", "pkgs"];

    let exe_path = std::env::current_exe().ok()?;

    for ancestor in exe_path.ancestors() {
        for pkg_dir in RELATIVE_PKG_DIRS {
            let candidate = ancestor.join(pkg_dir).join(relative_pkg_file);

            if candidate.exists() {
                return Some(candidate);
            }
        }
    }

    None
}

fn write_program_into_file(prog: &[u8], file: &str) -> Result<()> {
    let path = PathBuf::from(file);

    let mut f = File::create(&path)?;
    f.write_all(prog)?;
    Ok(())
}
