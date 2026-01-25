use std::fs;
use std::path::{Path, PathBuf};

use dora_frontend::check_program;
use dora_frontend::sema::{Sema, SemaCreationParams};

use crate::TestResult;

const SEPARATOR: &str = "\n//====================\n";

pub fn run_sema_test(path: &Path, content: &str, force: bool) -> TestResult {
    let (code, expected) = if let Some(pos) = content.find(SEPARATOR) {
        let code = &content[..pos];
        let expected_raw = &content[pos + SEPARATOR.len()..];

        if expected_raw.is_empty() {
            (code.to_string(), Some(String::new()))
        } else {
            let expected: String = expected_raw
                .lines()
                .map(|line| line.strip_prefix("// ").unwrap_or(line))
                .collect::<Vec<_>>()
                .join("\n")
                + "\n";
            (code.to_string(), Some(expected))
        }
    } else {
        (content.to_string(), None)
    };

    let filename = PathBuf::from(path.file_name().unwrap_or_default());
    let args = SemaCreationParams::new()
        .set_program_path(filename.clone())
        .set_file_content(filename, code.clone());
    let mut sa = Sema::new(args);

    check_program(&mut sa);

    let actual = sa.diag.borrow_mut().dump_to_string(&sa, true);

    let actual_commented = if actual.is_empty() {
        String::new()
    } else {
        actual
            .lines()
            .map(|line| format!("// {}", line))
            .collect::<Vec<_>>()
            .join("\n")
            + "\n"
    };

    match expected {
        None => {
            let new_content = format!("{}{}{}", code, SEPARATOR, actual_commented);
            if let Err(e) = fs::write(path, &new_content) {
                let error = format!("could not write file: {}", e);
                eprintln!("FAIL {}: {}", path.display(), error);
                return TestResult::Failed(error);
            }
            println!("UPDATE {}", path.display());
            TestResult::Updated
        }
        Some(_) if force => {
            let new_content = format!("{}{}{}", code, SEPARATOR, actual_commented);
            if new_content != content {
                if let Err(e) = fs::write(path, &new_content) {
                    let error = format!("could not write file: {}", e);
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
                let mut error = String::new();
                error.push_str("=== Expected:\n");
                for line in expected.lines() {
                    error.push_str(&format!("  {}\n", line));
                }
                error.push_str("=== Actual:\n");
                for line in actual.lines() {
                    error.push_str(&format!("  {}\n", line));
                }
                eprintln!("FAIL {}\n{}", path.display(), error);
                TestResult::Failed(error)
            }
        }
    }
}
