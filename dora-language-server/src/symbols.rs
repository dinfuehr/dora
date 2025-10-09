use std::sync::Arc;

use lsp_server::{Message, Request, Response};
use lsp_types::{DocumentSymbol, DocumentSymbolResponse, Position, Range, SymbolKind};

use dora_parser::{Span, compute_line_column, compute_line_starts};

use dora_frontend::sema::{Element, ElementId, FileContent, Sema, SemaFlags};

use crate::server::{MainLoopTask, ServerState, uri_to_file_path};

pub(super) fn workspace_symbol_request(_server_state: &mut ServerState, request: Request) {
    let result = serde_json::from_value::<lsp_types::WorkspaceSymbolParams>(request.params);

    match result {
        Ok(_result) => unimplemented!(),
        Err(..) => {
            eprintln!("broken params");
        }
    }
}

pub(super) fn document_symbol_request(server_state: &mut ServerState, request: Request) {
    eprintln!("got documentSymbol request on main thread");
    let result = serde_json::from_value::<lsp_types::DocumentSymbolParams>(request.params);
    match result {
        Ok(result) => {
            let path = uri_to_file_path(&result.text_document.uri);
            if let Some(content) = server_state.opened_files.get(&path) {
                let content = content.clone();
                eprintln!(
                    "got file for {} with {} lines",
                    path.display(),
                    content.lines().count()
                );
                let sender = server_state.threadpool_sender.clone();

                server_state.threadpool.execute(move || {
                    eprintln!("parse file on background thread.");
                    let symbols = parse_file(content);
                    eprintln!("parse done on background thread.");
                    let response = DocumentSymbolResponse::Nested(symbols);
                    let response = Response::new_ok(request.id, response);
                    sender
                        .send(MainLoopTask::SendResponse(Message::Response(response)))
                        .expect("send failed");
                });
            } else {
                eprintln!("unknown file {}", path.display());
            }
        }
        Err(_) => {
            eprintln!("broken params");
        }
    }
}

fn parse_file(content: Arc<String>) -> Vec<DocumentSymbol> {
    let mut sa = Sema::new(SemaFlags {
        packages: Vec::new(),
        program_file: Some(FileContent::Content(content.to_string())),
        boots: false,
        is_standard_library: false,
    });

    let file_id = sa.parse_single_file();
    let file = sa.file(file_id);
    let module = sa.module(file.module_id);
    let line_starts = compute_line_starts(&content);

    // Convert element IDs to DocumentSymbols directly
    module
        .children()
        .iter()
        .filter_map(|&element_id| element_to_symbol(&sa, &line_starts, element_id))
        .collect()
}

fn element_to_symbol(
    sa: &Sema,
    line_starts: &[u32],
    element_id: ElementId,
) -> Option<DocumentSymbol> {
    let element = sa.element(element_id);
    let file_id = element.file_id();
    let file = sa.file(file_id);
    let f = file.ast();
    let total_span = element.span();

    let (name, name_span, kind) = match element_id {
        ElementId::Class(id) => {
            let class = sa.class(id);
            let ast_id = class.ast_id.expect("missing ast_id");
            let node = f.node(ast_id).as_class();
            let name_id = node.name?;
            let ident_node = f.node(name_id).as_ident();
            (ident_node.name.clone(), ident_node.span, SymbolKind::CLASS)
        }
        ElementId::Struct(id) => {
            let struct_def = sa.struct_(id);
            let ast_id = struct_def.ast_id;
            let node = f.node(ast_id).as_struct();
            let name_id = node.name?;
            let ident_node = f.node(name_id).as_ident();
            (ident_node.name.clone(), ident_node.span, SymbolKind::STRUCT)
        }
        ElementId::Trait(id) => {
            let trait_def = sa.trait_(id);
            let ast_id = trait_def.ast_id;
            let node = f.node(ast_id).as_trait();
            let name_id = node.name?;
            let ident_node = f.node(name_id).as_ident();
            (
                ident_node.name.clone(),
                ident_node.span,
                SymbolKind::INTERFACE,
            )
        }
        ElementId::Enum(id) => {
            let enum_def = sa.enum_(id);
            let ast_id = enum_def.ast_id;
            let node = f.node(ast_id).as_enum();
            let name_id = node.name?;
            let ident_node = f.node(name_id).as_ident();
            (ident_node.name.clone(), ident_node.span, SymbolKind::ENUM)
        }
        ElementId::Fct(id) => {
            let fct = sa.fct(id);
            let ast_id = fct.ast_id.expect("missing ast_id");
            let node = f.node(ast_id).as_function();
            let name_id = node.name?;
            let ident_node = f.node(name_id).as_ident();
            (
                ident_node.name.clone(),
                ident_node.span,
                SymbolKind::FUNCTION,
            )
        }
        ElementId::Global(id) => {
            let global = sa.global(id);
            let ast_id = global.ast_id;
            let node = f.node(ast_id).as_global();
            let name_id = node.name?;
            let ident_node = f.node(name_id).as_ident();
            (
                ident_node.name.clone(),
                ident_node.span,
                SymbolKind::VARIABLE,
            )
        }
        ElementId::Const(id) => {
            let const_def = sa.const_(id);
            let ast_id = const_def.ast_id;
            let node = f.node(ast_id).as_const();
            let name_id = node.name?;
            let ident_node = f.node(name_id).as_ident();
            (
                ident_node.name.clone(),
                ident_node.span,
                SymbolKind::CONSTANT,
            )
        }
        ElementId::Impl(id) => {
            let impl_def = sa.impl_(id);
            let ast_id = impl_def.ast_id;
            let node = f.node(ast_id).as_impl();
            let mut name: String = "impl".into();

            if let Some(type_param_list_id) = node.type_params {
                let type_param_list = f
                    .node(type_param_list_id)
                    .to_type_param_list()
                    .expect("type param list expected");
                let span = type_param_list.span;
                let type_params_string =
                    &f.content().as_str()[span.start() as usize..span.end() as usize];
                name.push_str(type_params_string);
            }

            let trait_ty_id = node.trait_type?;
            let trait_ty = f.node(trait_ty_id);
            let span = trait_ty.span();
            let trait_ty_string = &f.content().as_str()[span.start() as usize..span.end() as usize];
            name.push_str(" ");
            name.push_str(trait_ty_string);
            name.push_str(" for");

            let name_span = f.node(node.extended_type).span();
            let extended_type_string =
                &f.content().as_str()[name_span.start() as usize..name_span.end() as usize];
            name.push_str(" ");
            name.push_str(extended_type_string);

            (name, name_span, SymbolKind::NAMESPACE)
        }
        ElementId::Extension(id) => {
            let extension = sa.extension(id);
            let ast_id = extension.ast_id;
            let node = f.node(ast_id).as_impl();
            let mut name = String::from("impl");

            if let Some(type_param_list_id) = node.type_params {
                let type_param_list = f
                    .node(type_param_list_id)
                    .to_type_param_list()
                    .expect("type param list expected");
                let span = type_param_list.span;
                let type_params_string =
                    &f.content().as_str()[span.start() as usize..span.end() as usize];
                name.push_str(type_params_string);
            }

            let name_span = f.node(node.extended_type).span();
            let extended_type_string =
                &f.content().as_str()[name_span.start() as usize..name_span.end() as usize];
            name.push_str(" ");
            name.push_str(extended_type_string);

            (name, name_span, SymbolKind::NAMESPACE)
        }
        ElementId::Alias(id) => {
            let alias = sa.alias(id);
            let ast_id = alias.ast_id;
            let node = f.node(ast_id).as_alias();
            let name_id = node.name?;
            let ident_node = f.node(name_id).as_ident();
            (
                ident_node.name.clone(),
                ident_node.span,
                SymbolKind::CONSTANT,
            )
        }
        ElementId::Field(id) => {
            let field = sa.field(id);
            let name = field.name?;
            let name_str = sa.interner.str(name).to_string();
            let span = field.span.expect("missing span");
            (name_str, span, SymbolKind::FIELD)
        }
        ElementId::Variant(id) => {
            let variant = sa.variant(id);
            let name = sa.interner.str(variant.name).to_string();
            let span = variant.span;
            (name, span, SymbolKind::ENUM_MEMBER)
        }
        _ => return None,
    };

    let range = range_from_span(line_starts, total_span);
    let selection_range = range_from_span(line_starts, name_span);

    let children: Vec<DocumentSymbol> = element
        .children()
        .iter()
        .filter_map(|&child_id| element_to_symbol(sa, line_starts, child_id))
        .collect();

    #[allow(deprecated)]
    Some(DocumentSymbol {
        name,
        kind,
        tags: None,
        detail: None,
        range,
        deprecated: None,
        selection_range,
        children: if children.is_empty() {
            None
        } else {
            Some(children)
        },
    })
}

fn range_from_span(line_starts: &[u32], span: Span) -> Range {
    let start = position_from_offset(line_starts, span.start());
    let end = position_from_offset(line_starts, span.end());

    Range { start, end }
}

fn position_from_offset(line_starts: &[u32], offset: u32) -> Position {
    let (line, column) = compute_line_column(&line_starts, offset);
    Position::new(line - 1, column - 1)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_file_empty() {
        let content = Arc::new("".to_string());
        let symbols = parse_file(content);
        assert_eq!(symbols.len(), 0);
    }

    #[test]
    fn test_parse_file_single_function() {
        let content = Arc::new("fn foo() {}".to_string());
        let symbols = parse_file(content);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "foo");
        assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);
        assert_eq!(symbols[0].children, None);
    }

    #[test]
    fn test_parse_file_multiple_functions() {
        let content = Arc::new("fn foo() {}\nfn bar() {}\nfn baz() {}".to_string());
        let symbols = parse_file(content);
        assert_eq!(symbols.len(), 3);
        assert_eq!(symbols[0].name, "foo");
        assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);
        assert_eq!(symbols[1].name, "bar");
        assert_eq!(symbols[1].kind, SymbolKind::FUNCTION);
        assert_eq!(symbols[2].name, "baz");
        assert_eq!(symbols[2].kind, SymbolKind::FUNCTION);
    }

    #[test]
    fn test_parse_file_impl_with_methods_and_aliases() {
        let content = Arc::new(
            "fn top_level() {}\nimpl Foo for Bar { fn method1() {} fn method2() {} type Alias1 = Int32; type Alias2 = String; }"
                .to_string(),
        );
        let symbols = parse_file(content);
        assert_eq!(symbols.len(), 2);

        assert_eq!(symbols[0].name, "top_level");
        assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);

        assert_eq!(symbols[1].name, "impl Foo for Bar");
        assert_eq!(symbols[1].kind, SymbolKind::NAMESPACE);

        let children = symbols[1].children.as_ref().unwrap();
        assert_eq!(children.len(), 4);

        assert_eq!(children[0].name, "method1");
        assert_eq!(children[0].kind, SymbolKind::FUNCTION);

        assert_eq!(children[1].name, "method2");
        assert_eq!(children[1].kind, SymbolKind::FUNCTION);

        assert_eq!(children[2].name, "Alias1");
        assert_eq!(children[2].kind, SymbolKind::CONSTANT);

        assert_eq!(children[3].name, "Alias2");
        assert_eq!(children[3].kind, SymbolKind::CONSTANT);
    }

    #[test]
    fn test_parse_file_impl_extension() {
        let content = Arc::new("impl Foo { fn method1() {} fn method2() {} }".to_string());
        let symbols = parse_file(content);
        assert_eq!(symbols.len(), 1);

        assert_eq!(symbols[0].name, "impl Foo");
        assert_eq!(symbols[0].kind, SymbolKind::NAMESPACE);

        let children = symbols[0].children.as_ref().unwrap();
        assert_eq!(children.len(), 2);

        assert_eq!(children[0].name, "method1");
        assert_eq!(children[0].kind, SymbolKind::FUNCTION);

        assert_eq!(children[1].name, "method2");
        assert_eq!(children[1].kind, SymbolKind::FUNCTION);
    }

    #[test]
    fn test_parse_file_struct_with_fields() {
        let content = Arc::new("struct Point { x: Int32, y: Int32 }".to_string());
        let symbols = parse_file(content);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "Point");
        assert_eq!(symbols[0].kind, SymbolKind::STRUCT);

        let children = symbols[0].children.as_ref().unwrap();
        assert_eq!(children.len(), 2);
        assert_eq!(children[0].name, "x");
        assert_eq!(children[0].kind, SymbolKind::FIELD);
        assert_eq!(children[1].name, "y");
        assert_eq!(children[1].kind, SymbolKind::FIELD);
    }

    #[test]
    fn test_parse_file_class_with_fields() {
        let content = Arc::new("class Person { name: String, age: Int32 }".to_string());
        let symbols = parse_file(content);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "Person");
        assert_eq!(symbols[0].kind, SymbolKind::CLASS);

        let children = symbols[0].children.as_ref().unwrap();
        assert_eq!(children.len(), 2);
        assert_eq!(children[0].name, "name");
        assert_eq!(children[0].kind, SymbolKind::FIELD);
        assert_eq!(children[1].name, "age");
        assert_eq!(children[1].kind, SymbolKind::FIELD);
    }

    #[test]
    fn test_parse_file_enum_with_variants() {
        let content = Arc::new("enum Color { Red, Green(Int32), Blue(Int32, Int32) }".to_string());
        let symbols = parse_file(content);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "Color");
        assert_eq!(symbols[0].kind, SymbolKind::ENUM);

        let children = symbols[0].children.as_ref().unwrap();
        assert_eq!(children.len(), 3);
        assert_eq!(children[0].name, "Red");
        assert_eq!(children[0].kind, SymbolKind::ENUM_MEMBER);
        assert_eq!(children[1].name, "Green");
        assert_eq!(children[1].kind, SymbolKind::ENUM_MEMBER);
        assert!(children[1].children.is_none());
        assert_eq!(children[2].name, "Blue");
        assert_eq!(children[2].kind, SymbolKind::ENUM_MEMBER);
    }

    #[test]
    fn test_parse_file_enum_with_named_fields() {
        let content = Arc::new("enum Color { Red, Green { value: Int32 } }".to_string());
        let symbols = parse_file(content);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "Color");
        assert_eq!(symbols[0].kind, SymbolKind::ENUM);

        let children = symbols[0].children.as_ref().unwrap();
        assert_eq!(children.len(), 2);
        assert_eq!(children[0].name, "Red");
        assert_eq!(children[0].kind, SymbolKind::ENUM_MEMBER);
        assert!(children[0].children.is_none());
        assert_eq!(children[1].name, "Green");
        assert_eq!(children[1].kind, SymbolKind::ENUM_MEMBER);

        let green_children = children[1].children.as_ref().unwrap();
        assert_eq!(green_children.len(), 1);
        assert_eq!(green_children[0].name, "value");
        assert_eq!(green_children[0].kind, SymbolKind::FIELD);
    }
}
