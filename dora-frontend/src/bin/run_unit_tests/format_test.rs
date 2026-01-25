use std::fs;
use std::path::Path;

use dora_format::format_source;

use crate::TestResult;

pub fn run_format_test(path: &Path, content: &str, force: bool) -> TestResult {
    let out_path = expected_output_path(path);

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

fn expected_output_path(input_path: &Path) -> std::path::PathBuf {
    let name = input_path.file_name().unwrap().to_str().unwrap();
    if name.ends_with(".in.dora") {
        let expected_name = name.replace(".in.dora", ".out.dora");
        input_path.with_file_name(expected_name)
    } else {
        input_path.with_extension("out.dora")
    }
}
