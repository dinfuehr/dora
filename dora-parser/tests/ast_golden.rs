use std::fs;
use std::path::Path;

use std::sync::Arc;

use dora_parser::Parser;
use dora_parser::ast::printer::dump_file_to_string;

fn check_file(input_path: &Path, expected_path: &Path) -> bool {
    let input = fs::read_to_string(input_path).expect("read input");
    let content = Arc::new(input);
    let (file, errors) = Parser::from_shared_string(content).parse();
    if !errors.is_empty() {
        for error in errors {
            eprintln!("parse error at {}: {}", error.span, error.error.message());
        }
        return false;
    }

    let actual = dump_file_to_string(&file);
    let expected = fs::read_to_string(expected_path).expect("read expected output");
    if actual != expected {
        eprintln!("== INPUT {}", input_path.display());
        eprintln!("== EXPECTED {}", expected_path.display());
        eprintln!("== EXPECTED");
        eprintln!("{}", expected);
        eprintln!("== ACTUAL");
        eprintln!("{}", actual);
        return false;
    }

    true
}

#[test]
fn golden_files() {
    let data_dir = Path::new("tests/ast");
    if !data_dir.is_dir() {
        return;
    }

    let entries = fs::read_dir(data_dir).expect("tests/ast missing");
    let mut mismatches = 0;

    for entry in entries {
        let entry = entry.expect("read_dir entry");
        let input_path = entry.path();
        let file_name = input_path.file_name().and_then(|name| name.to_str());
        if !matches!(file_name, Some(name) if name.ends_with(".dora")) {
            continue;
        }

        let input_name = file_name.expect("input name");
        let stem = input_name.strip_suffix(".dora").expect("input stem");
        let output_path = data_dir.join(format!("{}.out", stem));

        if !check_file(&input_path, &output_path) {
            mismatches += 1;
        }
    }

    if mismatches > 0 {
        panic!("{} golden file(s) mismatched", mismatches);
    }
}
