use std::fs;
use std::path::Path;

use dora_format::format_source;

#[test]
#[ignore]
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
            actual.as_str(),
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

#[test]
fn formats_empty_input() {
    let input = "";
    let actual = format_source(input).expect("format empty input");
    assert_eq!(actual.as_str(), "");
}

#[test]
fn formats_empty_main() {
    let input = "fn  main (  ) {  }";
    let actual = format_source(input).expect("format empty input");
    assert_eq!(actual.as_str(), "fn main() {}");
}

#[test]
fn formats_empty_with_comment() {
    let input = "fn  main (  ) { // test\n  }";
    let actual = format_source(input).expect("format empty input");
    assert_eq!(actual.as_str(), "fn main() {\n    // test\n}");
}

#[test]
fn formats_fct_with_simple_let() {
    let input = "fn  main (  ) {  let  x  =  1 ; }";
    let actual = format_source(input).expect("format empty input");
    assert_eq!(actual.as_str(), "fn main() {\n    let x = 1;\n}");
}

#[test]
#[ignore]
fn formats_fct_on_same_line() {
    let input = "fn f(){} fn g(){}";
    let actual = format_source(input).expect("format empty input");
    assert_eq!(actual.as_str(), "fn f() {}\n\nfn g() {}\n\n");
}
