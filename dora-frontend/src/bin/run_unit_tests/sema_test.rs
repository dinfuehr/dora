use std::fs;
use std::path::{Path, PathBuf};

use dora_frontend::check_program;
use dora_frontend::sema::{Sema, SemaCreationParams};

use crate::{TestResult, report_mismatch};

pub fn run_sema_test(path: &Path, content: &str, force: bool) -> TestResult {
    let out_path = path.with_extension("out");
    let expect_ok = content.lines().any(|line| line.trim() == "//= ok");

    let filename = PathBuf::from(path.file_name().unwrap_or_default());
    let args = SemaCreationParams::new()
        .set_program_path(filename.clone())
        .set_file_content(filename, content.to_string());
    let mut sa = Sema::new(args);

    check_program(&mut sa);

    let actual = sa.diag.borrow_mut().dump_to_string(&sa, true);

    let expected = fs::read_to_string(&out_path).ok();

    match expected {
        None => {
            // No .out file - check for //= ok directive
            if expect_ok {
                if actual.is_empty() {
                    println!("PASS {}", path.display());
                    return TestResult::Passed;
                } else {
                    let mut error = String::new();
                    error.push_str("=== Expected: (no errors due to //= ok)\n");
                    error.push_str("=== Actual:\n");
                    for line in actual.lines() {
                        error.push_str(&format!("  {}\n", line));
                    }
                    eprintln!("FAIL {}\n{}", path.display(), error);
                    return TestResult::Failed(error);
                }
            }

            if let Err(e) = fs::write(&out_path, &actual) {
                let error = format!("could not write .out file: {}", e);
                eprintln!("FAIL {}: {}", path.display(), error);
                return TestResult::Failed(error);
            }
            println!("UPDATE {}", path.display());
            TestResult::Updated
        }
        Some(_) if expect_ok => {
            let error = "both //= ok directive and .out file exist".to_string();
            eprintln!("FAIL {}: {}", path.display(), error);
            TestResult::Failed(error)
        }
        Some(_) if force => {
            let expected = fs::read_to_string(&out_path).unwrap_or_default();
            if actual != expected {
                if let Err(e) = fs::write(&out_path, &actual) {
                    let error = format!("could not write .out file: {}", e);
                    eprintln!("FAIL {}: {}", path.display(), error);
                    return TestResult::Failed(error);
                }
                println!("UPDATE {}", path.display());
                TestResult::Updated
            } else {
                println!("PASS {}", path.display());
                TestResult::Passed
            }
        }
        Some(expected) => {
            if actual == expected {
                println!("PASS {}", path.display());
                TestResult::Passed
            } else {
                let error = report_mismatch(&out_path, &expected, &actual);
                eprintln!("FAIL {}\n{}", path.display(), error);
                TestResult::Failed(error)
            }
        }
    }
}
