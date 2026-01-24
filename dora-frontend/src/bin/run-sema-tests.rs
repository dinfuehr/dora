use std::fs;
use std::panic::{self, AssertUnwindSafe};
use std::path::{Path, PathBuf};
use std::time::Instant;

use dora_frontend::check_program;
use dora_frontend::sema::{Sema, SemaCreationParams};

const SEPARATOR: &str = "\n====================\n";

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let force = args.iter().any(|a| a == "--force");
    let paths: Vec<&str> = args
        .iter()
        .skip(1)
        .filter(|a| !a.starts_with('-'))
        .map(|s| s.as_str())
        .collect();

    let tests_dir = find_tests_dir();

    let test_files = if paths.is_empty() {
        collect_test_files(&tests_dir)
    } else {
        paths.iter().map(PathBuf::from).collect()
    };

    if test_files.is_empty() {
        eprintln!("No test files found");
        std::process::exit(1);
    }

    let mut failed = 0;
    let mut passed = 0;
    let mut updated = 0;

    let start = Instant::now();

    for path in &test_files {
        match run_test(path, force) {
            TestResult::Passed => passed += 1,
            TestResult::Failed => failed += 1,
            TestResult::Updated => updated += 1,
        }
    }

    let elapsed = start.elapsed();

    println!();
    println!(
        "{} passed, {} failed, {} updated in {:.2}s",
        passed,
        failed,
        updated,
        elapsed.as_secs_f64()
    );

    if failed > 0 {
        std::process::exit(1);
    }
}

fn find_tests_dir() -> PathBuf {
    // Try to find tests directory relative to current dir or manifest dir
    let candidates = [
        PathBuf::from("test/sema"),
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .join("test/sema"),
    ];

    for candidate in candidates {
        if candidate.is_dir() {
            return candidate;
        }
    }

    eprintln!("Could not find tests directory");
    std::process::exit(1);
}

fn collect_test_files(dir: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    collect_test_files_recursive(dir, &mut files);
    files.sort();
    files
}

fn collect_test_files_recursive(dir: &Path, files: &mut Vec<PathBuf>) {
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                collect_test_files_recursive(&path, files);
            } else if path.extension().is_some_and(|e| e == "dora") {
                files.push(path);
            }
        }
    }
}

enum TestResult {
    Passed,
    Failed,
    Updated,
}

fn run_test(path: &Path, force: bool) -> TestResult {
    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("FAIL {}: could not read file: {}", path.display(), e);
            return TestResult::Failed;
        }
    };

    let (code, expected) = if let Some(pos) = content.find(SEPARATOR) {
        let code = &content[..pos];
        let expected = &content[pos + SEPARATOR.len()..];
        (code.to_string(), Some(expected.to_string()))
    } else {
        (content.clone(), None)
    };

    let actual = match run_sema(&code, path) {
        Ok(output) => output,
        Err(panic_msg) => {
            eprintln!("FAIL {}: panic: {}", path.display(), panic_msg);
            return TestResult::Failed;
        }
    };

    match expected {
        None => {
            // No separator - auto-generate output
            let new_content = format!("{}{}{}", code, SEPARATOR, actual);
            if let Err(e) = fs::write(path, &new_content) {
                eprintln!("FAIL {}: could not write file: {}", path.display(), e);
                return TestResult::Failed;
            }
            println!("UPDATE {}", path.display());
            TestResult::Updated
        }
        Some(_) if force => {
            // Force mode - regenerate output
            let new_content = format!("{}{}{}", code, SEPARATOR, actual);
            if new_content != content {
                if let Err(e) = fs::write(path, &new_content) {
                    eprintln!("FAIL {}: could not write file: {}", path.display(), e);
                    return TestResult::Failed;
                }
                println!("UPDATE {}", path.display());
                TestResult::Updated
            } else {
                println!("PASS {}", path.display());
                TestResult::Passed
            }
        }
        Some(expected) => {
            // Normal mode - compare output
            if actual == expected {
                println!("PASS {}", path.display());
                TestResult::Passed
            } else {
                eprintln!("FAIL {}", path.display());
                eprintln!("=== Expected:");
                for line in expected.lines() {
                    eprintln!("  {}", line);
                }
                eprintln!("=== Actual:");
                for line in actual.lines() {
                    eprintln!("  {}", line);
                }
                eprintln!();
                TestResult::Failed
            }
        }
    }
}

fn run_sema(code: &str, path: &Path) -> Result<String, String> {
    let code = code.to_string();
    let filename = PathBuf::from(path.file_name().unwrap_or_default());

    let result = panic::catch_unwind(AssertUnwindSafe(|| {
        let args = SemaCreationParams::new()
            .set_program_path(filename.clone())
            .set_file_content(filename, code.clone());
        let mut sa = Sema::new(args);

        check_program(&mut sa);

        sa.diag.borrow_mut().dump_to_string(&sa, true)
    }));

    result.map_err(|e| {
        if let Some(s) = e.downcast_ref::<&str>() {
            s.to_string()
        } else if let Some(s) = e.downcast_ref::<String>() {
            s.clone()
        } else {
            "unknown panic".to_string()
        }
    })
}
