use std::ffi::OsStr;
use std::fs;
use std::io::IsTerminal;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, Instant};

use crate::driver::append_exe_suffix;
use crate::driver::flags::CompileArgs;
use crate::driver::start::{Result, compile_program};
use dora_frontend::sema::SemaCreationParams;
use dora_runtime::{AotCompileArgs, CollectorName, TargetArch};
use tempfile::TempPath;

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
    let total_start = Instant::now();
    let mut timings = CompileTimings::default();

    let output_package_file;
    let temporary_package_file;
    let input_is_package = is_package_file(&args.file);

    let package_file: &Path = if input_is_package {
        Path::new(&args.file)
    } else if args.compile_to_package_only {
        output_package_file = args.output.as_ref().map(PathBuf::from).unwrap_or_else(|| {
            let path = Path::new(&args.file);
            let stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("out");
            PathBuf::from(format!("{stem}.dora-package"))
        });
        &output_package_file
    } else {
        temporary_package_file = tempfile::Builder::new().prefix("dora-program").tempfile()?;
        temporary_package_file.path()
    };

    if !input_is_package {
        measure(&mut timings.package, || {
            compile_to_package(&args, package_file)
        })?;
    }

    if args.compile_to_package_only {
        maybe_print_timings(&args, &timings, total_start.elapsed());
        return Ok(());
    }

    let assembly_extension = assembly_file_extension();
    let assembly_suffix = format!(".{assembly_extension}");
    let asm_file = if args.asm_only {
        None
    } else {
        Some(
            tempfile::Builder::new()
                .suffix(&assembly_suffix)
                .tempfile()?,
        )
    };

    let output = binary_output_path(&args);
    let asm_path = match &asm_file {
        Some(tmp) => tmp.path().to_path_buf(),
        None => PathBuf::from(&output).with_extension(assembly_extension),
    };

    measure(&mut timings.machine, || {
        compile_package_using_compiler_binary(&args, package_file, &asm_path)
    })?;

    if args.asm_only {
        maybe_print_timings(&args, &timings, total_start.elapsed());
        return Ok(());
    }

    #[cfg(target_os = "windows")]
    assert!(matches!(args.target_arch(), TargetArch::X64));

    let obj_path = measure(&mut timings.object, || {
        create_object_file(&asm_path, args.verbose)
    })?;
    let obj_path_ref: &Path = obj_path.as_ref();
    measure(&mut timings.link, || {
        link_object(obj_path_ref, &output, args.verbose)
    })?;

    maybe_print_timings(&args, &timings, total_start.elapsed());

    Ok(())
}

#[derive(Default)]
struct CompileTimings {
    package: Duration,
    machine: Duration,
    object: Duration,
    link: Duration,
}

fn measure<T>(duration: &mut Duration, action: impl FnOnce() -> T) -> T {
    let start = Instant::now();
    let result = action();
    *duration += start.elapsed();
    result
}

fn maybe_print_timings(args: &CompileArgs, timings: &CompileTimings, total: Duration) {
    if args.emit_timings {
        println!("{}", format_timing_line(timings, total));
    }
}

fn format_timing_line(timings: &CompileTimings, total: Duration) -> String {
    let colors = use_colors();
    let mut fields = Vec::new();

    let mut push_timing_field =
        |label: &str, label_color: &str, value_color: &str, duration: Duration| {
            if duration != Duration::ZERO {
                fields.push(format!(
                    "{}={}",
                    paint(colors, label_color, label),
                    paint(colors, value_color, &format_duration(duration)),
                ));
            }
        };

    push_timing_field("package", "\x1b[36m", "\x1b[1;33m", timings.package);
    push_timing_field("machine", "\x1b[36m", "\x1b[1;33m", timings.machine);
    push_timing_field("object", "\x1b[36m", "\x1b[1;33m", timings.object);
    push_timing_field("link", "\x1b[36m", "\x1b[1;33m", timings.link);
    push_timing_field("total", "\x1b[1;36m", "\x1b[1;32m", total);

    let prefix = paint(colors, "\x1b[1;36m", "TIME");
    if fields.is_empty() {
        prefix
    } else {
        format!("{}: {}", prefix, fields.join(" "))
    }
}

fn format_duration(duration: Duration) -> String {
    format!("{:.1}ms", duration.as_secs_f64() * 1000.0)
}

fn use_colors() -> bool {
    std::io::stdout().is_terminal() && std::env::var_os("NO_COLOR").is_none()
}

fn paint(colors: bool, code: &str, text: &str) -> String {
    if colors {
        format!("{code}{text}\x1b[0m")
    } else {
        text.to_string()
    }
}

fn compile_to_package(args: &CompileArgs, package_file: &Path) -> Result<()> {
    let program_path = PathBuf::from(&args.file);
    let sema_params = SemaCreationParams::new()
        .set_program_path(program_path)
        .set_package_paths(args.common.packages());

    let sema_params = if args.internal_compile_stdlib {
        sema_params.set_standard_library(true)
    } else if args.internal_compile_boots {
        sema_params.set_boots(true)
    } else {
        sema_params
    };

    let prog = compile_program(sema_params, &args.common)?;

    let encoded_program = bincode::encode_to_vec(prog, bincode::config::standard())
        .expect("program serialization failed");
    fs::write(package_file, encoded_program)?;

    Ok(())
}

fn binary_output_path(args: &CompileArgs) -> String {
    args.output.clone().unwrap_or_else(|| "out".into())
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

    maybe_print_subcommand(&command, args.verbose);
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

fn create_object_file(asm_path: &Path, verbose: bool) -> Result<TempPath> {
    #[cfg(target_os = "windows")]
    {
        return windows::create_object_file(asm_path, verbose);
    }

    #[cfg(not(target_os = "windows"))]
    {
        create_object_file_unix(asm_path, verbose)
    }
}

fn link_object(obj_path: &Path, output: &str, verbose: bool) -> Result<()> {
    // Find the runtime static library next to the current executable.
    let exe_dir = current_exe_dir()?;
    let runtime_lib = find_staticlib(&exe_dir, "dora_runtime")
        .ok_or_else(|| "runtime library not found in target directory".to_string())?;
    let startup_lib = find_staticlib(&exe_dir, "dora_startup")
        .ok_or_else(|| "startup library not found in target directory".to_string())?;

    #[cfg(target_os = "windows")]
    {
        return windows::link_object(obj_path, output, &startup_lib, &runtime_lib, verbose);
    }

    #[cfg(not(target_os = "windows"))]
    {
        link_object_unix(obj_path, output, &startup_lib, &runtime_lib, verbose)
    }
}

#[cfg(not(target_os = "windows"))]
fn create_object_file_unix(asm_path: &Path, verbose: bool) -> Result<TempPath> {
    let obj_file = tempfile::Builder::new().suffix(".o").tempfile()?;
    let obj_path = obj_file.into_temp_path();
    let obj_path_ref: &Path = obj_path.as_ref();

    let cc = std::env::var("CC").unwrap_or_else(|_| "gcc".to_string());
    let mut command = Command::new(&cc);
    command.arg("-c").arg(asm_path).arg("-o").arg(obj_path_ref);

    maybe_print_subcommand(&command, verbose);
    let status = command.status()?;

    if !status.success() {
        return Err(format!("{cc} failed while assembling AOT assembly").into());
    }

    Ok(obj_path)
}

#[cfg(not(target_os = "windows"))]
fn link_object_unix(
    obj_path: &Path,
    output: &str,
    startup_lib: &Path,
    runtime_lib: &Path,
    verbose: bool,
) -> Result<()> {
    let cc = std::env::var("CC").unwrap_or_else(|_| "gcc".to_string());
    let mut command = Command::new(&cc);
    command
        .arg(obj_path)
        .arg(&startup_lib)
        .arg(&runtime_lib)
        // Drop local symbols so GCC's random temporary object names do not
        // make otherwise identical AOT binaries differ.
        .arg("-Wl,-x");

    command.arg("-lpthread");
    if cfg!(target_os = "linux") {
        command.arg("-ldl");
    }
    command.arg("-lm").arg("-o").arg(output);

    maybe_print_subcommand(&command, verbose);
    let status = command.status()?;

    if !status.success() {
        return Err("gcc failed".into());
    }

    if cfg!(target_os = "macos") {
        let mut command = Command::new("codesign");
        command
            .arg("-s")
            .arg("-")
            .arg("-i")
            .arg("dora-aot")
            .arg(output);

        maybe_print_subcommand(&command, verbose);
        let status = command.status()?;

        if !status.success() {
            return Err("codesign failed".into());
        }
    }

    Ok(())
}

fn maybe_print_subcommand(command: &Command, verbose: bool) {
    if verbose {
        eprintln!("+ {}", format_subcommand(command));
    }
}

fn format_subcommand(command: &Command) -> String {
    std::iter::once(command.get_program())
        .chain(command.get_args())
        .map(shell_quote)
        .collect::<Vec<_>>()
        .join(" ")
}

fn shell_quote(value: &OsStr) -> String {
    let value = value.to_str().expect("subcommand contains invalid UTF-8");

    if value.is_empty() {
        return "''".to_string();
    }

    if value.chars().all(|ch| {
        ch.is_ascii_alphanumeric() || matches!(ch, '_' | '-' | '.' | '/' | ':' | '=' | ',' | '+')
    }) {
        value.to_string()
    } else {
        format!("'{}'", value.replace('\'', "'\\''"))
    }
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
