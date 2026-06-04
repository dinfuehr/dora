use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::driver::flags::CompileArgs;
use crate::driver::start::{Result, compile_boots, compile_program};
use dora_runtime::{AotCompileArgs, CollectorName, TargetArch};
use tempfile::NamedTempFile;

#[cfg(target_os = "windows")]
mod windows;

impl AotCompileArgs for CompileArgs {
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

pub fn command_compile(args: CompileArgs) -> Result<()> {
    let assembly_extension = assembly_file_extension();
    let assembly_suffix = format!(".{assembly_extension}");
    let asm_file = if args.emit_asm {
        None
    } else {
        Some(
            tempfile::Builder::new()
                .suffix(&assembly_suffix)
                .tempfile()?,
        )
    };

    let asm_path = match &asm_file {
        Some(tmp) => tmp.path().to_path_buf(),
        None => PathBuf::from(&args.output).with_extension(assembly_extension),
    };

    let (package_path, _opt_package_tempfile) = compile_to_package(&args)?;

    compile_package_using_compiler_binary(&args, &package_path, &asm_path)?;

    if args.emit_asm {
        return Ok(());
    }

    #[cfg(target_os = "windows")]
    assert!(matches!(args.target_arch(), TargetArch::X64));

    link_assembly(&asm_path, &args.output)?;

    Ok(())
}

fn compile_to_package(args: &CompileArgs) -> Result<(PathBuf, Option<NamedTempFile>)> {
    if is_package_file(&args.file) {
        return Ok((PathBuf::from(&args.file), None));
    }

    let tempfile = tempfile::Builder::new().prefix("dora-program").tempfile()?;
    let package_file = tempfile.path().to_path_buf();

    let prog = if args.internal_compile_boots {
        compile_boots(&args.file, &args.common)?
    } else {
        compile_program(&args.file, &args.common, false)?
    };

    let encoded_program = bincode::encode_to_vec(prog, bincode::config::standard())
        .expect("program serialization failed");
    fs::write(&package_file, encoded_program)?;

    Ok((package_file, Some(tempfile)))
}

fn compile_package_using_compiler_binary(
    args: &CompileArgs,
    package_path: &Path,
    asm_path: &Path,
) -> Result<()> {
    let compiler = compiler_binary_path(args)?;

    if !compiler.exists() {
        return Err(format!("compiler not found at '{}'", compiler.display()).into());
    }

    let mut command = Command::new(&compiler);
    command.arg(package_path).arg("-o").arg(asm_path);

    if let Some(target) = args.target {
        command.arg("--target").arg(target_arch_name(target));
    }

    if let Some(gc) = args.gc {
        command.arg("--gc").arg(collector_name(gc));
    }

    if let Some(emit_graph) = &args.emit_graph {
        command.arg("--emit-graph").arg(emit_graph);
    }

    if args.emit_graph_after_each_pass {
        command.arg("--emit-graph-after-each-pass");
    }

    if args.test {
        command.arg("--test");
    }

    if args.internal_compile_boots {
        command.arg("--internal-compile-boots");
    }

    let status = command.status()?;
    if !status.success() {
        return Err(format!("{} failed", compiler.display()).into());
    }

    Ok(())
}

fn compiler_binary_path(args: &CompileArgs) -> Result<PathBuf> {
    if let Some(compiler) = &args.compiler {
        return Ok(compiler.clone());
    }

    let compiler = append_exe_suffix(PathBuf::from(default_compiler_binary_name(args)));
    Ok(current_exe_dir()?.join(compiler))
}

fn default_compiler_binary_name(args: &CompileArgs) -> &'static str {
    if args.cannon {
        "dora-cannon-compiler"
    } else {
        "dora-boots-compiler"
    }
}

fn append_exe_suffix(mut path: PathBuf) -> PathBuf {
    let exe_suffix = std::env::consts::EXE_SUFFIX;
    if !exe_suffix.is_empty() && path.extension().is_none() {
        let extension = exe_suffix.strip_prefix('.').unwrap_or(exe_suffix);
        path.set_extension(extension);
    }
    path
}

fn link_assembly(asm_path: &Path, output: &str) -> Result<()> {
    // Find the runtime static library next to the current executable.
    let exe_dir = current_exe_dir()?;
    let runtime_lib = find_staticlib(&exe_dir, "dora_runtime")
        .ok_or_else(|| "runtime library not found in target directory".to_string())?;
    let startup_lib = find_staticlib(&exe_dir, "dora_startup")
        .ok_or_else(|| "startup library not found in target directory".to_string())?;

    #[cfg(target_os = "windows")]
    {
        return windows::link_assembly(asm_path, output, &startup_lib, &runtime_lib);
    }

    #[cfg(not(target_os = "windows"))]
    {
        link_assembly_unix(asm_path, output, &startup_lib, &runtime_lib)
    }
}

#[cfg(not(target_os = "windows"))]
fn link_assembly_unix(
    asm_path: &Path,
    output: &str,
    startup_lib: &Path,
    runtime_lib: &Path,
) -> Result<()> {
    let cc = std::env::var("CC").unwrap_or_else(|_| "gcc".to_string());
    let mut command = Command::new(&cc);
    command
        .arg(&asm_path)
        .arg(&startup_lib)
        .arg(&runtime_lib)
        // Drop local symbols so GCC's random temporary object names do not
        // make otherwise identical AOT binaries differ.
        .arg("-Wl,-x");

    if cfg!(target_os = "macos") {
        command.arg("-Wl,-no_uuid");
    }

    command.arg("-lpthread");
    if cfg!(target_os = "linux") {
        command.arg("-ldl");
    }
    let status = command.arg("-lm").arg("-o").arg(output).status()?;

    if !status.success() {
        return Err("gcc failed".into());
    }

    if cfg!(target_os = "macos") {
        let status = Command::new("codesign")
            .arg("-s")
            .arg("-")
            .arg("-i")
            .arg("dora-aot")
            .arg(output)
            .status()?;

        if !status.success() {
            return Err("codesign failed".into());
        }
    }

    Ok(())
}

fn assembly_file_extension() -> &'static str {
    if cfg!(target_os = "windows") {
        "asm"
    } else {
        "s"
    }
}

fn is_package_file(path: &str) -> bool {
    path.ends_with(".dora-package")
}

fn current_exe_dir() -> Result<PathBuf> {
    Ok(std::env::current_exe()?
        .parent()
        .expect("no parent directory")
        .to_path_buf())
}

fn find_staticlib(exe_dir: &Path, crate_name: &str) -> Option<PathBuf> {
    let name = if cfg!(target_os = "windows") {
        format!("{crate_name}.lib")
    } else {
        format!("lib{crate_name}.a")
    };
    let path = exe_dir.join(name);
    path.exists().then_some(path)
}

fn target_arch_name(target: TargetArch) -> &'static str {
    match target {
        TargetArch::X64 => "x64",
        TargetArch::Arm64 => "arm64",
    }
}

fn collector_name(gc: CollectorName) -> &'static str {
    match gc {
        CollectorName::Zero => "zero",
        CollectorName::Copy => "copy",
        CollectorName::Sweep => "sweep",
        CollectorName::Swiper => "swiper",
    }
}
