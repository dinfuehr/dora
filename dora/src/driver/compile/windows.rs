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

    trace_arm64_object(target_arch, obj_path_ref);

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

    maybe_print_subcommand(&linker_command, verbose);
    let status = linker_command.status()?;

    if !status.success() {
        return Err(format!("{} failed while linking AOT binary", linker.display()).into());
    }

    trace_arm64_binary(target_arch, output);

    Ok(())
}

fn trace_arm64_object(target_arch: TargetArch, obj_path: &Path) {
    if !trace_arm64_enabled(target_arch) {
        return;
    }

    trace_arm64_object_entry(obj_path);
}

fn trace_arm64_binary(target_arch: TargetArch, output: &str) {
    if !trace_arm64_enabled(target_arch) {
        return;
    }

    let output = Path::new(output);
    dumpbin_filtered(
        "binary headers",
        &["/nologo", "/headers"],
        output,
        &["entry point", "machine", ".text"],
        80,
    );
    dumpbin_disasm_windows(
        "binary disasm",
        output,
        &["main", "dora_boots_compiler_main"],
    );
}

fn trace_arm64_enabled(target_arch: TargetArch) -> bool {
    target_arch.is_arm64() && std::env::var("DORA_AOT_TRACE_COMPILE").as_deref() == Ok("1")
}

fn trace_arm64_object_entry(obj_path: &Path) {
    let symbols = match dumpbin_output("object symbols", &["/nologo", "/symbols"], obj_path) {
        Some(output) => output,
        None => return,
    };

    let entries = [
        "main",
        "dora_boots_compiler_main",
        "dora_interface_3A_3Acompile",
    ];
    for entry in entries {
        match find_dumpbin_symbol(&symbols, entry) {
            Some(symbol) => eprintln!("dumpbin object symbol: {}", symbol.line),
            None => eprintln!("dumpbin object symbol: {entry}: <not found>"),
        }
    }

    let Some(main_symbol) = find_dumpbin_symbol(&symbols, "main") else {
        return;
    };

    let Some(disasm) = dumpbin_output("object disasm", &["/nologo", "/disasm"], obj_path) else {
        return;
    };
    dump_object_address_window("object disasm", &disasm, main_symbol.offset, 4, 24);

    let Some(relocs) = dumpbin_output("object relocations", &["/nologo", "/relocations"], obj_path)
    else {
        return;
    };
    dump_relocations_for_range(
        "object relocations",
        &relocs,
        main_symbol.offset,
        main_symbol.offset + 0x40,
    );
}

struct DumpbinSymbol<'a> {
    line: &'a str,
    offset: u64,
}

fn find_dumpbin_symbol<'a>(symbols: &'a str, name: &str) -> Option<DumpbinSymbol<'a>> {
    let suffix = format!("| {name}");
    for line in symbols.lines() {
        if !line.trim_end().ends_with(&suffix) {
            continue;
        }

        let mut parts = line.split_whitespace();
        parts.next()?;
        let offset = u64::from_str_radix(parts.next()?, 16).ok()?;
        return Some(DumpbinSymbol { line, offset });
    }

    None
}

fn dump_object_address_window(
    label: &str,
    output: &str,
    address: u64,
    before: usize,
    after: usize,
) {
    let needle = format!("{address:016X}:");
    let lines = output.lines().collect::<Vec<_>>();
    for (idx, line) in lines.iter().enumerate() {
        if !line.contains(&needle) {
            continue;
        }

        let start = idx.saturating_sub(before);
        let end = usize::min(idx + after, lines.len());
        eprintln!("dumpbin {label} window for {needle}");
        for line in &lines[start..end] {
            eprintln!("{line}");
        }
        return;
    }

    eprintln!("dumpbin {label}: <no window for {needle}>");
}

fn dump_relocations_for_range(label: &str, output: &str, start: u64, end: u64) {
    eprintln!("dumpbin {label} entries for {start:08X}..{end:08X}:");
    let mut printed = 0;
    for line in output.lines() {
        let Some(first) = line.split_whitespace().next() else {
            continue;
        };
        let Ok(offset) = u64::from_str_radix(first, 16) else {
            continue;
        };
        if start <= offset && offset < end {
            eprintln!("{line}");
            printed += 1;
        }
    }

    if printed == 0 {
        eprintln!("dumpbin {label}: <no entries in range>");
    }
}

fn dumpbin_output(label: &str, args: &[&str], path: &Path) -> Option<String> {
    eprintln!(
        "Diagnostic command: dumpbin {} {}",
        args.join(" "),
        path.display()
    );
    let output = match Command::new("dumpbin").args(args).arg(path).output() {
        Ok(output) => output,
        Err(err) => {
            eprintln!("Diagnostic command failed: {err}");
            return None;
        }
    };
    eprintln!("Diagnostic command status: {}", output.status);

    let stderr = String::from_utf8_lossy(&output.stderr);
    if !stderr.trim().is_empty() {
        eprintln!("dumpbin {label} stderr:\n{stderr}");
    }

    Some(String::from_utf8_lossy(&output.stdout).into_owned())
}

fn dumpbin_filtered(label: &str, args: &[&str], path: &Path, needles: &[&str], limit: usize) {
    eprintln!(
        "Diagnostic command: dumpbin {} {}",
        args.join(" "),
        path.display()
    );
    let output = match Command::new("dumpbin").args(args).arg(path).output() {
        Ok(output) => output,
        Err(err) => {
            eprintln!("Diagnostic command failed: {err}");
            return;
        }
    };
    eprintln!("Diagnostic command status: {}", output.status);

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    if !stderr.trim().is_empty() {
        eprintln!("dumpbin {label} stderr:\n{stderr}");
    }

    let needles = needles
        .iter()
        .map(|needle| needle.to_ascii_lowercase())
        .collect::<Vec<_>>();
    let mut printed = 0;
    eprintln!("dumpbin {label} filtered output:");
    for line in stdout.lines() {
        let lower = line.to_ascii_lowercase();
        if needles.iter().any(|needle| lower.contains(needle)) {
            eprintln!("{line}");
            printed += 1;
            if printed >= limit {
                eprintln!("dumpbin {label} filtered output truncated after {limit} lines");
                return;
            }
        }
    }
    if printed == 0 {
        eprintln!("dumpbin {label} filtered output: <no matching lines>");
    }
}

fn dumpbin_disasm_windows(label: &str, path: &Path, symbols: &[&str]) {
    eprintln!(
        "Diagnostic command: dumpbin /nologo /disasm {}",
        path.display()
    );
    let output = match Command::new("dumpbin")
        .arg("/nologo")
        .arg("/disasm")
        .arg(path)
        .output()
    {
        Ok(output) => output,
        Err(err) => {
            eprintln!("Diagnostic command failed: {err}");
            return;
        }
    };
    eprintln!("Diagnostic command status: {}", output.status);

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    if !stderr.trim().is_empty() {
        eprintln!("dumpbin {label} stderr:\n{stderr}");
    }

    let lines = stdout.lines().collect::<Vec<_>>();
    let mut printed_any = false;
    for symbol in symbols {
        let symbol = symbol.to_ascii_lowercase();
        for (idx, line) in lines.iter().enumerate() {
            if !line.to_ascii_lowercase().contains(&symbol) {
                continue;
            }

            printed_any = true;
            let start = idx.saturating_sub(4);
            let end = usize::min(idx + 24, lines.len());
            eprintln!("dumpbin {label} window for {symbol}:");
            for line in &lines[start..end] {
                eprintln!("{line}");
            }
            break;
        }
    }

    if !printed_any {
        eprintln!("dumpbin {label}: <no matching symbol windows>");
    }
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
