use std::sync::Arc;

use dora_parser::ast::File;
use dora_parser::{ParseErrorWithLocation, Parser};

pub mod doc;
pub mod render;
#[doc(hidden)]
pub mod test_utils;

pub fn format_source(input: &str) -> Result<Arc<String>, Vec<ParseErrorWithLocation>> {
    format_source_with_line_length(input, 90)
}

pub fn format_source_with_line_length(
    input: &str,
    line_length: u32,
) -> Result<Arc<String>, Vec<ParseErrorWithLocation>> {
    let content = Arc::new(input.to_string());
    let parser = Parser::from_shared_string(content);
    let (file, errors) = parser.parse();

    if !errors.is_empty() {
        return Err(errors);
    }

    let root = file.root();
    let (arena, root_id) = doc::format(root);

    let formatted = Arc::new(render::render_doc_with_line_length(
        &arena,
        root_id,
        line_length,
    ));

    let (_formatted_file, formatted_errors) = parse(formatted.clone());
    if !formatted_errors.is_empty() {
        println!("== FORMATTED");
        print!("{}", formatted);
        println!("== DOC");
        println!("{}", doc::print::print_doc_to_string(&arena, root_id));
    }
    assert!(formatted_errors.is_empty());

    Ok(formatted)
}

fn parse(code: Arc<String>) -> (File, Vec<ParseErrorWithLocation>) {
    let parser = Parser::from_shared_string(code);
    parser.parse()
}

#[cfg(test)]
mod tests {
    #[test]
    fn rejects_invalid_input() {
        let input = "fn {";
        let result = super::format_source(input);
        assert!(result.is_err(), "invalid input should be rejected");
    }
}
