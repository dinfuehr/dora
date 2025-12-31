use std::fs;
use std::path::Path;
use std::sync::Arc;

use dora_format::{doc, format_source, render};
use dora_parser::ast::File;
use dora_parser::ast::printer;
use dora_parser::{ParseErrorWithLocation, Parser, compute_line_column, compute_line_starts};

fn check_source(input: &str, expected: &str) -> bool {
    let content = Arc::new(input.to_string());
    let parser = Parser::from_shared_string(content);
    let (file, errors) = parser.parse();

    if !errors.is_empty() {
        print_parse_errors("input", input, &errors);
        return false;
    }

    let root = file.root();
    let (formatted_arena, formatted_root_id) = doc::format(root);
    let formatted = Arc::new(render::render_doc(&formatted_arena, formatted_root_id));

    let (formatted_file, formatted_errors) = parse(formatted.clone());
    if !formatted_errors.is_empty() {
        print_parse_errors("formatted output", formatted.as_str(), &formatted_errors);
        return false;
    }

    if formatted.as_str() != expected {
        println!("== FORMATTED");
        print!("{}", formatted);
        println!("== EXPECTED");
        print!("{}", expected);
        println!("== AST");
        printer::dump_file(&file);
        println!("== DOC");
        println!(
            "{}",
            doc::print::print_doc_to_string(&formatted_arena, formatted_root_id)
        );
        return false;
    }

    let reformatted_root = formatted_file.root();
    let (reformatted_arena, reformatted_root_id) = doc::format(reformatted_root);
    let reformatted = render::render_doc(&reformatted_arena, reformatted_root_id);
    if reformatted != formatted.as_str() {
        println!("== REFORMAT NOT IDEMPOTENT");
        println!("== REFORMATTED");
        print!("{}", reformatted);
        println!("== FORMATTED");
        print!("{}", formatted);
        println!("== AST");
        printer::dump_file(&formatted_file);
        println!("== REFORMATTED DOC");
        println!(
            "{}",
            doc::print::print_doc_to_string(&reformatted_arena, reformatted_root_id)
        );
        println!("== FORMATTED DOC");
        println!(
            "{}",
            doc::print::print_doc_to_string(&formatted_arena, formatted_root_id)
        );
        return false;
    }

    true
}

fn assert_source(input: &str, expected: &str) {
    assert!(check_source(input, expected));
}

fn print_parse_errors(label: &str, content: &str, errors: &[ParseErrorWithLocation]) {
    println!("== PARSE ERROR ({})", label);
    let line_starts = compute_line_starts(content);
    for err in errors {
        let (line, col) = compute_line_column(&line_starts, err.span.start());
        println!("L{}:{} @ {} {:?}", line, col, err.span, err.error);
    }
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
fn formats_fct_with_literal_types() {
    let input = "fn  main (  ) {  let  f  =  1.5 ; let  s  =  \"hi\" ; let  b  =  true ; }";
    assert_source(
        input,
        "fn main() {\n    let f = 1.5;\n    let s = \"hi\";\n    let b = true;\n}\n",
    );
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
