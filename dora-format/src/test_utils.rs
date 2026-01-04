use std::sync::Arc;

use dora_parser::ast::File;
use dora_parser::ast::printer;
use dora_parser::{ParseErrorWithLocation, Parser, compute_line_column, compute_line_starts};

use crate::{doc, render};

pub fn check_source(input: &str, expected: &str) -> bool {
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

pub fn assert_source(input: &str, expected: &str) {
    assert!(check_source(input, expected));
}

fn print_parse_errors(label: &str, content: &str, errors: &[ParseErrorWithLocation]) {
    println!("== PARSE ERROR ({})", label);
    println!("{}", content);
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
