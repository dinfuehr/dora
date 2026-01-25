use std::fs;
use std::path::Path;
use std::sync::Arc;

use dora_parser::Parser;
use dora_parser::ast::printer::dump_file_to_string_with_trivia;

use crate::TestResult;

pub fn run_parse_test(path: &Path, content: &str, force: bool) -> TestResult {
    let out_path = path.with_extension("out");

    let actual = match parse_file(content) {
        Ok(output) => output,
        Err(err) => {
            eprintln!("FAIL {}: {}", path.display(), err);
            return TestResult::Failed(err);
        }
    };

    let expected = fs::read_to_string(&out_path).ok();

    match expected {
        None => {
            if let Err(e) = fs::write(&out_path, &actual) {
                let error = format!("could not write .out file: {}", e);
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

fn parse_file(content: &str) -> Result<String, String> {
    let content = Arc::new(content.to_string());
    let (file, errors) = Parser::from_shared_string(content).parse();

    if !errors.is_empty() {
        let mut error_msg = String::new();
        for error in errors {
            error_msg.push_str(&format!(
                "Parse error at {}: {}\n",
                error.span,
                error.error.message()
            ));
        }
        return Err(error_msg);
    }

    Ok(dump_file_to_string_with_trivia(&file, true))
}
