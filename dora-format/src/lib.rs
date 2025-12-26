use std::sync::Arc;

use dora_parser::Parser;
use dora_parser::ParseErrorWithLocation;
use dora_parser::TokenKind;
use dora_parser::ast::{
    AstElement, AstElementList, AstUse, AstUseGroup, AstUsePath, AstUseTarget, SyntaxElement,
    SyntaxNode, SyntaxNodeBase,
};

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

    let root = file.root();
    let element_list = AstElementList::cast(root).expect("element list");
    let mut output = String::new();
    emit_element_list(element_list, &mut output);
    Ok(output)
}

fn emit_element_list(element_list: AstElementList, output: &mut String) {
    let list_node = element_list.syntax_node().clone();
    let list_span = list_node.full_span();
    let list_start = list_span.start() as usize;
    let list_end = list_span.end() as usize;
    let list_content = list_node.file().content();

    let items: Vec<AstElement> = element_list.items().collect();
    if items.is_empty() {
        emit_node(list_node, output);
        return;
    }

    let first_span = items[0].syntax_node().full_span();
    let first_start = first_span.start() as usize;
    if list_start < first_start {
        output.push_str(&list_content[list_start..first_start]);
    }

    let last_span = items.last().unwrap().syntax_node().full_span();
    let last_end = last_span.end() as usize;

    for item in items {
        emit_element(item, output);
    }

    if last_end < list_end {
        output.push_str(&list_content[last_end..list_end]);
    }
}

fn emit_element(element: AstElement, output: &mut String) {
    match element {
        AstElement::Use(use_item) => emit_use(use_item, output),
        AstElement::Alias(node) => emit_node(node.syntax_node().clone(), output),
        AstElement::Class(node) => emit_node(node.syntax_node().clone(), output),
        AstElement::Const(node) => emit_node(node.syntax_node().clone(), output),
        AstElement::Enum(node) => emit_node(node.syntax_node().clone(), output),
        AstElement::Error(node) => emit_node(node, output),
        AstElement::Extern(node) => emit_node(node.syntax_node().clone(), output),
        AstElement::Function(node) => emit_node(node.syntax_node().clone(), output),
        AstElement::Global(node) => emit_node(node.syntax_node().clone(), output),
        AstElement::Impl(node) => emit_node(node.syntax_node().clone(), output),
        AstElement::Module(node) => emit_node(node.syntax_node().clone(), output),
        AstElement::Struct(node) => emit_node(node.syntax_node().clone(), output),
        AstElement::Trait(node) => emit_node(node.syntax_node().clone(), output),
    }
}

fn emit_use(use_item: AstUse, output: &mut String) {
    let use_node = use_item.syntax_node().clone();
    for element in use_node.children_with_tokens() {
        match element {
            SyntaxElement::Token(token) => output.push_str(token.text()),
            SyntaxElement::Node(node) => {
                if node.syntax_kind() == TokenKind::USE_PATH {
                    let path_node = AstUsePath::cast(node).expect("use path node expected");
                    emit_use_path(&path_node, output);
                } else {
                    emit_use_node(node, output);
                }
            }
        }
    }
}

fn emit_use_node(node: SyntaxNode, output: &mut String) {
    if node.syntax_kind() == TokenKind::USE_GROUP {
        let group_node = AstUseGroup::cast(node).expect("use group node expected");
        if should_format_use_group(&group_node) {
            emit_use_group(group_node, output);
        } else {
            emit_node(group_node.syntax_node().clone(), output);
        }
        return;
    }

    for element in node.children_with_tokens() {
        match element {
            SyntaxElement::Token(token) => output.push_str(token.text()),
            SyntaxElement::Node(child) => emit_use_node(child, output),
        }
    }
}

fn should_format_use_group(group: &AstUseGroup) -> bool {
    for target in group.targets() {
        if target.path().next().is_some() {
            return false;
        }
        match target.target() {
            Some(AstUseTarget::UseName(_)) => {}
            _ => return false,
        }
    }
    true
}

fn emit_use_group(group: AstUseGroup, output: &mut String) {
    let mut items: Vec<(String, AstUsePath)> = group
        .targets()
        .map(|target| match target.target() {
            Some(AstUseTarget::UseName(name)) => (name.name().token_as_string(), target),
            _ => unreachable!("use group target not in simple form"),
        })
        .collect();
    items.sort_by(|left, right| left.0.cmp(&right.0));

    output.push('{');
    for (index, (_, path)) in items.iter().enumerate() {
        if index > 0 {
            output.push_str(", ");
        }
        emit_use_path(path, output);
    }
    output.push('}');
}

fn emit_use_path(path: &AstUsePath, output: &mut String) {
    let path_node = path.syntax_node().clone();
    for element in path_node.children_with_tokens() {
        match element {
            SyntaxElement::Token(token) => output.push_str(token.text()),
            SyntaxElement::Node(node) => {
                if node.syntax_kind() == TokenKind::USE_GROUP {
                    let group_node = AstUseGroup::cast(node).expect("use group node expected");
                    if should_format_use_group(&group_node) {
                        emit_use_group(group_node, output);
                    } else {
                        emit_node(group_node.syntax_node().clone(), output);
                    }
                } else {
                    emit_use_node(node, output);
                }
            }
        }
    }
}
