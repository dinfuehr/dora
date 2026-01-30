use std::fs;
use std::path::{Path, PathBuf};

use dora_bytecode::{FunctionId, TypeParamMode, display_fct, dump};
use dora_frontend::sema::{Sema, SemaCreationParams};
use dora_frontend::{check_program, emit_program};

use crate::TestResult;

pub fn run_bc_test(path: &Path, content: &str, force: bool) -> TestResult {
    let out_path = path.with_extension("out");

    let actual = match generate_bytecode(content, path) {
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

fn generate_bytecode(code: &str, path: &Path) -> Result<String, String> {
    let filename = PathBuf::from(path.file_name().unwrap_or_default());
    let args = SemaCreationParams::new()
        .set_program_path(filename.clone())
        .set_file_content(filename, code.to_string());
    let mut sa = Sema::new(args);

    let result = check_program(&mut sa);

    if sa.diag.borrow().has_errors() {
        let errors = sa.diag.borrow_mut().dump_to_string(&sa, true);
        return Err(format!("compilation errors:\n{}", errors));
    }

    assert!(result);

    let program = emit_program(sa);

    let mut output = String::new();

    for (idx, fct) in program.functions.iter().enumerate() {
        if fct.package_id != program.program_package_id {
            continue;
        }

        let fct_id: FunctionId = idx.into();
        let fct_name = display_fct(&program, fct_id);

        output.push_str(&format!("fn {}:\n", fct_name));

        if let Some(ref bytecode) = fct.bytecode {
            let mut buf = Vec::new();
            dump(&mut buf, &program, bytecode, TypeParamMode::Unknown).expect("dump failed");
            output.push_str(&String::from_utf8_lossy(&buf));
        } else {
            output.push_str("  <no bytecode>\n\n");
        }
    }

    Ok(output)
}
