use std::fs;
use std::path::Path;

use dora_format::format_source;

fn assert_source(input: &str, expected: &str) {
    let actual = format_source(input).expect("format input");
    assert_eq!(actual.as_str(), expected);
}

#[test]
#[ignore]
fn golden_files() {
    let data_dir = Path::new("tests/data");
    let entries = fs::read_dir(data_dir).expect("tests/data missing");

    for entry in entries {
        let entry = entry.expect("read_dir entry");
        let input_path = entry.path();
        if input_path.extension().and_then(|ext| ext.to_str()) != Some("dora") {
            continue;
        }

        let stem = input_path
            .file_stem()
            .and_then(|stem| stem.to_str())
            .expect("input stem");
        let output_path = data_dir.join(format!("{}.out.dora", stem));

        let input = fs::read_to_string(&input_path).expect("read input");
        let actual = format_source(&input).expect("format input");
        let expected = fs::read_to_string(&output_path).expect("read expected output");
        assert_source(actual.as_str(), &expected);
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
    assert_source(input, "");
}

#[test]
fn formats_empty_main() {
    let input = "fn  main (  ) {  }";
    assert_source(input, "fn main() {}\n");
}

#[test]
fn formats_empty_with_comment() {
    let input = "fn  main (  ) { // test\n  }";
    assert_source(input, "fn main() {\n    // test\n}\n");
}

#[test]
fn formats_fct_with_simple_let() {
    let input = "fn  main (  ) {  let  x  =  1 ; }";
    assert_source(input, "fn main() {\n    let x = 1;\n}\n");
}

#[test]
fn formats_fct_with_multiple_stmts() {
    let input = "fn  main (  ) {  1;2;3;4 }";
    assert_source(input, "fn main() {\n    1;\n    2;\n    3;\n    4\n}\n");
}

#[test]
fn formats_fct_on_same_line() {
    let input = "fn f(){} fn g(){}";
    assert_source(input, "fn f() {}\n\nfn g() {}\n");
}
