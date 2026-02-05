mod bc_test;
mod format_test;
mod parse_test;
mod sema_test;

use std::cell::RefCell;
use std::fs;
use std::panic::{self, AssertUnwindSafe};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};

thread_local! {
    static PANIC_INFO: RefCell<Option<String>> = const { RefCell::new(None) };
}

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

    /// Only run ignored tests
    #[arg(long)]
    ignored: bool,

    /// Number of threads to use (default: number of CPUs)
    #[arg(short = 'j', long)]
    threads: Option<usize>,

    /// Timeout per test in seconds
    #[arg(long, default_value = "60")]
    timeout: u64,

    /// Test files to run (default: all tests in test/sema and test/bc)
    files: Vec<PathBuf>,
}

fn main() {
    panic::set_hook(Box::new(|info| {
        let msg = info.to_string();
        PANIC_INFO.with(|cell| {
            *cell.borrow_mut() = Some(msg);
        });
    }));

    let args = Args::parse();

    if let Some(threads) = args.threads {
        rayon::ThreadPoolBuilder::new()
            .num_threads(threads)
            .build_global()
            .unwrap();
    }

    let test_files: Vec<TestFile> = if args.files.is_empty() {
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
        files.sort_by(|a, b| a.path.cmp(&b.path));
        files
    } else {
        let mut files = Vec::new();
        for path in &args.files {
            if path.is_dir() {
                let kind = detect_test_kind(path);
                collect_test_files_recursive(path, kind, &mut files);
            } else {
                let kind = detect_test_kind(path);
                if let Ok(content) = fs::read_to_string(path) {
                    files.push(TestFile {
                        path: path.clone(),
                        kind,
                        is_ignore: is_ignored(&content),
                        content,
                    });
                }
            }
        }
        files.sort_by(|a, b| a.path.cmp(&b.path));
        files
    };

    if test_files.is_empty() {
        eprintln!("No test files found");
        std::process::exit(1);
    }

    let start = Instant::now();

    let force = args.force;
    let only_ignored = args.ignored;
    let timeout = Duration::from_secs(args.timeout);
    let results: Vec<(&TestFile, TestResult)> = test_files
        .par_iter()
        .map(|test| (test, run_test(test, force, only_ignored, timeout)))
        .collect();

    let elapsed = start.elapsed();

    let mut passed = 0;
    let mut failed = 0;
    let mut panicked = 0;
    let mut timed_out = 0;
    let mut updated = 0;
    let mut ignored = 0;
    let mut parse_count = 0;
    let mut format_count = 0;
    let mut sema_count = 0;
    let mut bc_count = 0;
    let mut unknown_count = 0;
    let mut failed_tests: Vec<(&PathBuf, String)> = Vec::new();
    let mut panicked_tests: Vec<(&PathBuf, String)> = Vec::new();
    let mut timeout_tests: Vec<&PathBuf> = Vec::new();

    for (test, result) in results {
        match test.kind {
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
                failed_tests.push((&test.path, error));
            }
            TestResult::Panicked(error) => {
                panicked += 1;
                panicked_tests.push((&test.path, error));
            }
            TestResult::Timeout => {
                timed_out += 1;
                timeout_tests.push(&test.path);
            }
            TestResult::Updated => updated += 1,
            TestResult::Ignored => ignored += 1,
        }
    }

    if failed > 0 || panicked > 0 || timed_out > 0 {
        println!();
        for (path, error) in failed_tests {
            println!("Failed test: {}", path.display());
            println!(
                "  Run: cargo run --bin run-unit-tests -- {}",
                path.display()
            );
            println!("   or: target/debug/run-unit-tests {}", path.display());
            print!("{}", error);
        }
        for (path, error) in panicked_tests {
            println!("Panicked test: {}", path.display());
            println!(
                "  Run: cargo run --bin run-unit-tests -- {}",
                path.display()
            );
            println!("   or: target/debug/run-unit-tests {}", path.display());
            print!("{}", error);
        }
        for path in timeout_tests {
            println!("Timed out test: {}", path.display());
            println!(
                "  Run: cargo run --bin run-unit-tests -- {}",
                path.display()
            );
            println!("   or: target/debug/run-unit-tests {}", path.display());
        }
    }

    println!();
    println!(
        "{} parse, {} format, {} sema, {} bytecode, {} unknown",
        parse_count, format_count, sema_count, bc_count, unknown_count
    );
    println!(
        "{} passed, {} failed, {} panicked, {} timed out, {} updated, {} ignored in {:.2}s",
        passed,
        failed,
        panicked,
        timed_out,
        updated,
        ignored,
        elapsed.as_secs_f64()
    );

    if failed > 0 || panicked > 0 || timed_out > 0 {
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

fn collect_test_files(dir: &Path, kind: TestKind, files: &mut Vec<TestFile>) {
    collect_test_files_recursive(dir, kind, files);
}

fn collect_test_files_recursive(dir: &Path, kind: TestKind, files: &mut Vec<TestFile>) {
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                collect_test_files_recursive(&path, kind, files);
            } else if is_test_input_file(&path, kind) {
                if let Ok(content) = fs::read_to_string(&path) {
                    files.push(TestFile {
                        path,
                        kind,
                        is_ignore: is_ignored(&content),
                        content,
                    });
                }
            }
        }
    }
}

fn is_test_input_file(path: &Path, _kind: TestKind) -> bool {
    path.extension().is_some_and(|e| e == "dora")
}

#[derive(Clone)]
pub enum TestResult {
    Passed,
    Failed(String),
    Panicked(String),
    Timeout,
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

struct TestFile {
    path: PathBuf,
    kind: TestKind,
    content: String,
    is_ignore: bool,
}

fn is_ignored(content: &str) -> bool {
    content.starts_with("//= ignore\n")
}

static TEMP_COUNTER: AtomicU64 = AtomicU64::new(0);

pub fn report_mismatch(out_path: &Path, expected: &str, actual: &str) -> String {
    let mut error = String::new();

    if let Some(temp_path) = write_actual_to_temp(actual) {
        error.push_str(&format!(
            "  vimdiff {} {}\n",
            out_path.display(),
            temp_path.display()
        ));
        error.push_str(&format!(
            "  diff {} {}\n",
            out_path.display(),
            temp_path.display()
        ));
    }

    let exp_lines: Vec<&str> = expected.lines().collect();
    let act_lines: Vec<&str> = actual.lines().collect();
    let max_len = exp_lines.len().max(act_lines.len());

    // Find first differing line.
    let first_diff = (0..max_len)
        .find(|&i| exp_lines.get(i) != act_lines.get(i))
        .unwrap_or(0);

    let context = 2;
    let start = first_diff.saturating_sub(context);
    let end = (first_diff + context + 1).min(max_len);

    let max_actual_width = (start..end)
        .filter_map(|i| act_lines.get(i).map(|l| l.len()))
        .max()
        .unwrap_or(0);

    let lineno_width = (end).to_string().len();

    for i in start..end {
        let exp = exp_lines.get(i).copied();
        let act = act_lines.get(i).copied();
        let lineno = i + 1;

        let act_str = act.unwrap_or("<missing>");
        let exp_str = exp.unwrap_or("<missing>");

        let marker = if i == first_diff {
            ">"
        } else if exp != act {
            "!"
        } else {
            " "
        };

        error.push_str(&format!(
            "{} {:>lw$}: {:aw$} | {}\n",
            marker,
            lineno,
            act_str,
            exp_str,
            lw = lineno_width,
            aw = max_actual_width + 2
        ));
    }

    error
}

fn write_actual_to_temp(actual: &str) -> Option<PathBuf> {
    let id = TEMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    let pid = std::process::id();
    let temp_name = format!("dora-test-{}-{}.actual", pid, id);
    let temp_path = std::env::temp_dir().join(temp_name);
    fs::write(&temp_path, actual).ok()?;
    Some(temp_path)
}

fn run_test(test: &TestFile, force: bool, only_ignored: bool, timeout: Duration) -> TestResult {
    if test.is_ignore != only_ignored {
        if !only_ignored {
            println!("IGNORE {}", test.path.display());
        }
        return TestResult::Ignored;
    }

    let (tx, rx) = mpsc::channel();
    let path = test.path.clone();
    let content = test.content.clone();
    let kind = test.kind;

    thread::spawn(move || {
        let result = panic::catch_unwind(AssertUnwindSafe(|| match kind {
            TestKind::Parse => run_parse_test(&path, &content, force),
            TestKind::Format => run_format_test(&path, &content, force),
            TestKind::Sema => run_sema_test(&path, &content, force),
            TestKind::Bytecode => run_bc_test(&path, &content, force),
            TestKind::Unknown => {
                let error =
                    "unknown test kind: path must contain 'parse', 'fmt', 'sema', or 'bc' directory"
                        .to_string();
                eprintln!("FAIL {}: {}", path.display(), error);
                TestResult::Failed(error)
            }
        }));

        let result = match result {
            Ok(result) => result,
            Err(_) => {
                let panic_msg = PANIC_INFO
                    .with(|cell| cell.borrow_mut().take())
                    .unwrap_or_else(|| "unknown panic".to_string());
                eprintln!("PANIC {}", path.display());
                TestResult::Panicked(format!("{}\n", panic_msg))
            }
        };

        let _ = tx.send(result);
    });

    match rx.recv_timeout(timeout) {
        Ok(result) => result,
        Err(mpsc::RecvTimeoutError::Timeout) => {
            eprintln!("TIMEOUT {}", test.path.display());
            TestResult::Timeout
        }
        Err(mpsc::RecvTimeoutError::Disconnected) => {
            eprintln!("PANIC {}", test.path.display());
            TestResult::Panicked("thread disconnected without result\n".to_string())
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
