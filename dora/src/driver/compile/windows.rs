use std::path::{Path, PathBuf};
use std::process::Command;

use crate::driver::start::Result;
use tempfile::TempPath;

pub(super) fn create_object_file(asm_path: &Path) -> Result<TempPath> {
    let ml64 = windows_masm();
    let obj_file = tempfile::Builder::new().suffix(".obj").tempfile()?;
    let obj_path = obj_file.into_temp_path();
    let obj_path_ref: &Path = obj_path.as_ref();

    let mut masm_command = Command::new(&ml64);
    masm_command
        .arg("/nologo")
        .arg("/c")
        .arg(format!("/Fo{}", obj_path_ref.display()))
        .arg(asm_path);

    let status = masm_command.status()?;

    if !status.success() {
        return Err(format!("{} failed while assembling AOT assembly", ml64.display()).into());
    }

    Ok(obj_path)
}

pub(super) fn link_object(
    obj_path: &Path,
    output: &str,
    startup_lib: &Path,
    runtime_lib: &Path,
) -> Result<()> {
    let linker = windows_linker();

    let mut linker_command = Command::new(&linker);
    linker_command
        .arg("/NOLOGO")
        .arg("/Brepro")
        .arg("/MACHINE:X64")
        .arg(format!("/OUT:{output}"))
        .arg(obj_path)
        .arg(startup_lib)
        .arg(runtime_lib)
        .arg("legacy_stdio_definitions.lib")
        .arg("kernel32.lib")
        .arg("ntdll.lib")
        .arg("userenv.lib")
        .arg("ws2_32.lib")
        .arg("dbghelp.lib")
        .arg("/defaultlib:msvcrt");

    let status = linker_command.status()?;

    if !status.success() {
        return Err(format!("{} failed while linking AOT binary", linker.display()).into());
    }

    Ok(())
}

fn windows_masm() -> PathBuf {
    PathBuf::from(std::env::var("DORA_ML64").unwrap_or_else(|_| "ml64".to_string()))
}

fn windows_linker() -> PathBuf {
    PathBuf::from(std::env::var("DORA_LINK").unwrap_or_else(|_| "link".to_string()))
}
