use std::sync::Arc;

use dora_parser::Parser;
use dora_parser::ast::{SyntaxElement, SyntaxNode};
use dora_parser::ParseErrorWithLocation;

fn emit_node(node: SyntaxNode, output: &mut String) {
    for element in node.children_with_tokens() {
        match element {
            SyntaxElement::Token(token) => output.push_str(token.text()),
            SyntaxElement::Node(child) => emit_node(child, output),
        }
    }
}

pub fn format_source(input: &str) -> Result<String, Vec<ParseErrorWithLocation>> {
    let content = Arc::new(input.to_string());
    let parser = Parser::from_shared_string(content);
    let (file, errors) = parser.parse();

    if !errors.is_empty() {
        return Err(errors);
    }

    let mut output = String::new();
    emit_node(file.root(), &mut output);
    Ok(output)
}
