use std::fs;
use std::path::Path;
use std::sync::Arc;

use dora_format::{doc, format_source, render};
use dora_parser::ast::File;
use dora_parser::ast::printer;
use dora_parser::{ParseErrorWithLocation, Parser};

fn check_source(input: &str, expected: &str) -> bool {
    let content = Arc::new(input.to_string());
    let parser = Parser::from_shared_string(content);
    let (file, errors) = parser.parse();

    if !errors.is_empty() {
        return false;
    }

    let root = file.root();
    let (arena, root_id) = doc::format(root);
    let formatted = Arc::new(render::render_doc(&arena, root_id));

    let (_formatted_file, formatted_errors) = parse(formatted.clone());
    if !formatted_errors.is_empty() {
        return false;
    }

    let actual = formatted.as_str();
    if actual != expected {
        println!("== WRONG");
        print!("{}", actual);
        println!("== EXPECTED");
        print!("{}", expected);
        println!("== AST");
        printer::dump_file(&file);
        println!("== DOC");
        println!("{}", doc::print::print_doc_to_string(&arena, root_id));
        return false;
    }

    true
}

fn assert_source(input: &str, expected: &str) {
    assert!(check_source(input, expected));
}

fn parse(code: Arc<String>) -> (File, Vec<ParseErrorWithLocation>) {
    let parser = Parser::from_shared_string(code);
    parser.parse()
}

#[test]
#[ignore]
fn golden_files() {
    let data_dir = Path::new("tests/data");
    let entries = fs::read_dir(data_dir).expect("tests/data missing");
    let mut mismatches = 0;

    for entry in entries {
        let entry = entry.expect("read_dir entry");
        let input_path = entry.path();
        let file_name = input_path.file_name().and_then(|name| name.to_str());
        if !matches!(file_name, Some(name) if name.ends_with(".in.dora")) {
            continue;
        }

        println!("==== {}", input_path.display());

        let input_name = file_name.expect("input name");
        let stem = input_name.strip_suffix(".in.dora").expect("input stem");
        let output_path = data_dir.join(format!("{}.out.dora", stem));

        let input = fs::read_to_string(&input_path).expect("read input");
        let expected = fs::read_to_string(&output_path).expect("read expected output");
        if !check_source(&input, &expected) {
            mismatches += 1;
        }
    }

    if mismatches > 0 {
        panic!("{} golden file(s) mismatched", mismatches);
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
