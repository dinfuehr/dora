use std::path::{Path, PathBuf};
use std::process::Command;

use super::maybe_print_subcommand;
use crate::driver::start::Result;
use dora_runtime::TargetArch;
use tempfile::TempPath;

pub(super) fn create_object_file(
    target_arch: TargetArch,
    asm_path: &Path,
    verbose: bool,
) -> Result<TempPath> {
    let assembler = windows_assembler(target_arch);
    let obj_file = tempfile::Builder::new().suffix(".obj").tempfile()?;
    let obj_path = obj_file.into_temp_path();
    let obj_path_ref: &Path = obj_path.as_ref();

    let mut assembler_command = Command::new(&assembler);
    match target_arch {
        TargetArch::X64 => {
            assembler_command
                .arg("/nologo")
                .arg("/c")
                .arg(format!("/Fo{}", obj_path_ref.display()))
                .arg(asm_path);
        }
        TargetArch::Arm64 => {
            assembler_command
                .arg("-nologo")
                .arg("-o")
                .arg(obj_path_ref)
                .arg(asm_path);
        }
    }

    maybe_print_subcommand(&assembler_command, verbose);
    let status = assembler_command.status()?;

    if !status.success() {
        return Err(format!(
            "{} failed while assembling AOT assembly",
            assembler.display()
        )
        .into());
    }

    Ok(obj_path)
}

pub(super) fn link_object(
    target_arch: TargetArch,
    obj_path: &Path,
    output: &str,
    startup_lib: &Path,
    runtime_lib: &Path,
    verbose: bool,
) -> Result<()> {
    let linker = windows_linker();
    let machine = match target_arch {
        TargetArch::X64 => "X64",
        TargetArch::Arm64 => "ARM64",
    };

    let mut linker_command = Command::new(&linker);
    linker_command
        .arg("/NOLOGO")
        .arg("/Brepro")
        .arg(format!("/MACHINE:{machine}"))
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

    if target_arch.is_arm64() {
        linker_command.arg("/INCLUDE:main");
    }

    maybe_print_subcommand(&linker_command, verbose);
    let status = linker_command.status()?;

    if !status.success() {
        return Err(format!("{} failed while linking AOT binary", linker.display()).into());
    }

    Ok(())
}

fn windows_assembler(target_arch: TargetArch) -> PathBuf {
    match target_arch {
        TargetArch::X64 => PathBuf::from("ml64"),
        TargetArch::Arm64 => PathBuf::from("armasm64"),
    }
}

fn windows_linker() -> PathBuf {
    PathBuf::from(std::env::var("DORA_LINK").unwrap_or_else(|_| "link".to_string()))
}
