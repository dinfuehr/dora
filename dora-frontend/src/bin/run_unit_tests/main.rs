mod bc_test;
mod format_test;
mod parse_test;
mod sema_test;

use std::fs;
use std::panic::{self, AssertUnwindSafe};
use std::path::{Path, PathBuf};
use std::time::Instant;

use clap::Parser;
use rayon::prelude::*;

use bc_test::run_bc_test;
use format_test::run_format_test;
use parse_test::run_parse_test;
use sema_test::run_sema_test;

#[derive(Parser)]
struct Args {
    /// Regenerate expected output for all tests
    #[arg(long)]
    force: bool,

    /// Number of threads to use (default: number of CPUs)
    #[arg(short = 'j', long)]
    threads: Option<usize>,

    /// Test files to run (default: all tests in test/sema and test/bc)
    files: Vec<PathBuf>,
}

fn main() {
    let args = Args::parse();

    if let Some(threads) = args.threads {
        rayon::ThreadPoolBuilder::new()
            .num_threads(threads)
            .build_global()
            .unwrap();
    }

    let test_files: Vec<(PathBuf, TestKind)> = if args.files.is_empty() {
        let mut files = Vec::new();
        for (dir, kind) in [
            ("test/parse", TestKind::Parse),
            ("test/fmt", TestKind::Format),
            ("test/sema", TestKind::Sema),
            ("test/bc", TestKind::Bytecode),
        ] {
            if let Some(tests_dir) = find_tests_dir(dir) {
                collect_test_files(&tests_dir, kind, &mut files);
            }
        }
        files.sort_by(|a, b| a.0.cmp(&b.0));
        files
    } else {
        let mut files = Vec::new();
        for path in &args.files {
            if path.is_dir() {
                let kind = detect_test_kind(path);
                collect_test_files_recursive(path, kind, &mut files);
            } else {
                let kind = detect_test_kind(path);
                files.push((path.clone(), kind));
            }
        }
        files.sort_by(|a, b| a.0.cmp(&b.0));
        files
    };

    if test_files.is_empty() {
        eprintln!("No test files found");
        std::process::exit(1);
    }

    let start = Instant::now();

    let force = args.force;
    let results: Vec<(&PathBuf, TestKind, TestResult)> = test_files
        .par_iter()
        .map(|(path, kind)| (path, *kind, run_test(path, *kind, force)))
        .collect();

    let elapsed = start.elapsed();

    let mut passed = 0;
    let mut failed = 0;
    let mut updated = 0;
    let mut ignored = 0;
    let mut parse_count = 0;
    let mut format_count = 0;
    let mut sema_count = 0;
    let mut bc_count = 0;
    let mut unknown_count = 0;
    let mut failed_tests: Vec<(&PathBuf, String)> = Vec::new();

    for (path, kind, result) in results {
        match kind {
            TestKind::Parse => parse_count += 1,
            TestKind::Format => format_count += 1,
            TestKind::Sema => sema_count += 1,
            TestKind::Bytecode => bc_count += 1,
            TestKind::Unknown => unknown_count += 1,
        }
        match result {
            TestResult::Passed => passed += 1,
            TestResult::Failed(error) => {
                failed += 1;
                failed_tests.push((path, error));
            }
            TestResult::Updated => updated += 1,
            TestResult::Ignored => ignored += 1,
        }
    }

    if failed > 0 {
        println!();
        for (path, error) in failed_tests {
            println!("Failed test: {}", path.display());
            print!("{}", error);
        }
    }

    println!();
    println!(
        "{} parse, {} format, {} sema, {} bytecode, {} unknown",
        parse_count, format_count, sema_count, bc_count, unknown_count
    );
    println!(
        "{} passed, {} failed, {} updated, {} ignored in {:.2}s",
        passed,
        failed,
        updated,
        ignored,
        elapsed.as_secs_f64()
    );

    if failed > 0 {
        std::process::exit(1);
    }
}

fn find_tests_dir(subdir: &str) -> Option<PathBuf> {
    let candidates = [
        PathBuf::from(subdir),
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .join(subdir),
    ];

    for candidate in candidates {
        if candidate.is_dir() {
            return Some(candidate);
        }
    }

    None
}

fn collect_test_files(dir: &Path, kind: TestKind, files: &mut Vec<(PathBuf, TestKind)>) {
    collect_test_files_recursive(dir, kind, files);
}

fn collect_test_files_recursive(dir: &Path, kind: TestKind, files: &mut Vec<(PathBuf, TestKind)>) {
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                collect_test_files_recursive(&path, kind, files);
            } else if is_test_input_file(&path, kind) {
                files.push((path, kind));
            }
        }
    }
}

fn is_test_input_file(path: &Path, kind: TestKind) -> bool {
    let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
    match kind {
        TestKind::Format => name.ends_with(".in.dora"),
        _ => path.extension().is_some_and(|e| e == "dora"),
    }
}

#[derive(Clone)]
pub enum TestResult {
    Passed,
    Failed(String),
    Updated,
    Ignored,
}

#[derive(Clone, Copy, PartialEq)]
enum TestKind {
    Parse,
    Format,
    Sema,
    Bytecode,
    Unknown,
}

fn run_test(path: &Path, kind: TestKind, force: bool) -> TestResult {
    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(e) => {
            let error = format!("could not read file: {}", e);
            eprintln!("FAIL {}: {}", path.display(), error);
            return TestResult::Failed(error);
        }
    };

    if content.starts_with("//= ignore\n") {
        println!("IGNORE {}", path.display());
        return TestResult::Ignored;
    }

    let result = panic::catch_unwind(AssertUnwindSafe(|| match kind {
        TestKind::Parse => run_parse_test(path, &content, force),
        TestKind::Format => run_format_test(path, &content, force),
        TestKind::Sema => run_sema_test(path, &content, force),
        TestKind::Bytecode => run_bc_test(path, &content, force),
        TestKind::Unknown => {
            let error =
                "unknown test kind: path must contain 'parse', 'fmt', 'sema', or 'bc' directory"
                    .to_string();
            eprintln!("FAIL {}: {}", path.display(), error);
            TestResult::Failed(error)
        }
    }));

    match result {
        Ok(result) => result,
        Err(e) => {
            let panic_msg = if let Some(s) = e.downcast_ref::<&str>() {
                s.to_string()
            } else if let Some(s) = e.downcast_ref::<String>() {
                s.clone()
            } else {
                "unknown panic".to_string()
            };
            let error = format!("panic: {}", panic_msg);
            eprintln!("FAIL {}: {}", path.display(), error);
            TestResult::Failed(error)
        }
    }
}

fn detect_test_kind(path: &Path) -> TestKind {
    for ancestor in path.ancestors() {
        if let Some(name) = ancestor.file_name() {
            if name == "parse" {
                return TestKind::Parse;
            } else if name == "fmt" {
                return TestKind::Format;
            } else if name == "sema" {
                return TestKind::Sema;
            } else if name == "bc" {
                return TestKind::Bytecode;
            }
        }
    }
    TestKind::Unknown
}
