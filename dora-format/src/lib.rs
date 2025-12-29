use std::sync::Arc;

use dora_parser::ast::File;
use dora_parser::{ParseErrorWithLocation, Parser};

mod doc;
mod render;

pub fn format_source(input: &str) -> Result<Arc<String>, Vec<ParseErrorWithLocation>> {
    let content = Arc::new(input.to_string());
    let parser = Parser::from_shared_string(content);
    let (file, errors) = parser.parse();

    if !errors.is_empty() {
        return Err(errors);
    }

    let root = file.root();
    let (arena, root_id) = doc::format(root);
    let formatted = Arc::new(render::render_doc(&arena, root_id));

    let (_formatted_file, formatted_errors) = parse(formatted.clone());
    assert!(formatted_errors.is_empty());

    Ok(formatted)
}

fn parse(code: Arc<String>) -> (File, Vec<ParseErrorWithLocation>) {
    let parser = Parser::from_shared_string(code);
    parser.parse()
}
