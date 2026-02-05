use std::fs;
use std::path::Path;

use dora_format::format_source;

use crate::{TestResult, report_mismatch};

pub fn run_format_test(path: &Path, content: &str, force: bool) -> TestResult {
    let out_path = path.with_extension("out");

    let actual = match format_source(content) {
        Ok(output) => output.to_string(),
        Err(errors) => {
            let mut error_msg = String::new();
            for error in errors {
                error_msg.push_str(&format!(
                    "Parse error at {}: {}\n",
                    error.span,
                    error.error.message()
                ));
            }
            eprintln!("FAIL {}: {}", path.display(), error_msg);
            return TestResult::Failed(error_msg);
        }
    };

    let expected = fs::read_to_string(&out_path).ok();

    match expected {
        None => {
            if let Err(e) = fs::write(&out_path, &actual) {
                let error = format!("could not write output file: {}", e);
                eprintln!("FAIL {}: {}", path.display(), error);
                return TestResult::Failed(error);
            }
            println!("UPDATE {}", path.display());
            TestResult::Updated
        }
        Some(_) if force => {
            let expected = fs::read_to_string(&out_path).unwrap_or_default();
            if actual != expected {
                if let Err(e) = fs::write(&out_path, &actual) {
                    let error = format!("could not write output file: {}", e);
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
