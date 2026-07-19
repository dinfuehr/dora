use std::fmt::Write;
use std::fs;
use std::path::{Path, PathBuf};

use dora_bytecode::{
    BytecodeType, FunctionId, ModuleId, Program, TypeParamData, TypeParamMode, display_fct, dump,
    fmt_ty, module_path_name,
};
use dora_frontend::sema::{Sema, SemaCreationParams};
use dora_frontend::{check_program, emit_program};

use crate::{TestResult, report_mismatch};

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
                let error = report_mismatch(&out_path, &expected, &actual);
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
    dump_type_definitions(&mut output, &program);

    for (idx, fct) in program.functions.iter().enumerate() {
        if fct.package_id != program.program_package_id {
            continue;
        }

        let fct_id: FunctionId = idx.into();
        let fct_name = display_fct(&program, fct_id);

        output.push_str(&format!("fn {}:\n", fct_name));

        if let Some(ref bytecode) = fct.bytecode {
            let mut buf = Vec::new();
            let type_params = TypeParamMode::TypeParams(&fct.type_params);
            dump(&mut buf, &program, bytecode, type_params).expect("dump failed");
            output.push_str(&String::from_utf8_lossy(&buf));
        } else {
            output.push_str("  <no bytecode>\n\n");
        }
    }

    Ok(output)
}

fn dump_type_definitions(output: &mut String, program: &Program) {
    for cls in &program.classes {
        if cls.package_id == program.program_package_id {
            dump_type_definition(
                output,
                program,
                "class",
                cls.module_id,
                &cls.name,
                &cls.type_params,
                cls.fields
                    .iter()
                    .map(|field| (field.name.as_deref(), &field.ty)),
            );
        }
    }

    for struct_ in &program.structs {
        if struct_.package_id == program.program_package_id {
            dump_type_definition(
                output,
                program,
                "struct",
                struct_.module_id,
                &struct_.name,
                &struct_.type_params,
                struct_
                    .fields
                    .iter()
                    .map(|field| (field.name.as_deref(), &field.ty)),
            );
        }
    }
}

fn dump_type_definition<'a>(
    output: &mut String,
    program: &Program,
    kind: &str,
    module_id: ModuleId,
    name: &str,
    type_params: &'a TypeParamData,
    fields: impl Iterator<Item = (Option<&'a str>, &'a BytecodeType)>,
) {
    write!(
        output,
        "{} {}",
        kind,
        module_path_name(program, module_id, name)
    )
    .unwrap();

    if !type_params.names.is_empty() {
        write!(output, "[{}]", type_params.names.join(", ")).unwrap();
    }

    writeln!(output, ":").unwrap();

    for (idx, (name, ty)) in fields.enumerate() {
        let name = name
            .map(|name| name.to_string())
            .unwrap_or_else(|| idx.to_string());
        writeln!(
            output,
            "  {}: {}",
            name,
            fmt_ty(program, ty, TypeParamMode::TypeParams(type_params), false)
        )
        .unwrap();
    }

    writeln!(output).unwrap();
}
