use std::fs;
use std::path::Path;

use dora_format::format_source;

#[test]
fn golden_files() {
    let data_dir = Path::new("tests/data");
    let entries = fs::read_dir(data_dir).expect("tests/data missing");

    for entry in entries {
        let entry = entry.expect("read_dir entry");
        let input_path = entry.path();
        if input_path.extension().and_then(|ext| ext.to_str()) != Some("in") {
            continue;
        }

        let stem = input_path
            .file_stem()
            .and_then(|stem| stem.to_str())
            .expect("input stem");
        let output_path = data_dir.join(format!("{}.out", stem));

        let input = fs::read_to_string(&input_path).expect("read input");
        let expected = fs::read_to_string(&output_path).expect("read expected output");
        let actual = format_source(&input).expect("format input");

        assert_eq!(
            actual,
            expected,
            "format mismatch for {}",
            input_path.display()
        );

        let second_pass = format_source(&actual).expect("format second pass");
        assert_eq!(
            second_pass,
            actual,
            "formatter not idempotent for {}",
            input_path.display()
        );
    }
}

#[test]
fn rejects_invalid_input() {
    let input = "fn {";
    let result = format_source(input);
    assert!(result.is_err(), "invalid input should be rejected");
}
