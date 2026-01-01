use std::path::PathBuf;

use lsp_server::{Message, Request, Response};
use lsp_types::{Location, OneOf, SymbolKind, WorkspaceSymbol, WorkspaceSymbolResponse};

use dora_parser::Span;
use dora_parser::ast::{File, SyntaxNodeBase};

use dora_frontend::Vfs;
use dora_frontend::sema::{Element, ElementId, Sema, SemaCreationParams};

use crate::position::span_to_range;
use crate::server::{MainThreadTask, ServerState, file_path_to_uri};

pub(super) fn workspace_symbol_request(server_state: &mut ServerState, request: Request) {
    let result = serde_json::from_value::<lsp_types::WorkspaceSymbolParams>(request.params);

    match result {
        Ok(result) => {
            let query = result.query;

            let sender = server_state.threadpool_sender.clone();
            let projects = server_state.projects.clone();
            let vfs = server_state.vfs.clone();

            server_state.threadpool.execute(move || {
                let mut symbols = Vec::new();

                for project in projects.iter() {
                    eprintln!(
                        "scan project {} at {} on background thread.",
                        project.name,
                        project.main.display()
                    );
                    scan_project(project.main.clone(), vfs.clone(), &query, &mut symbols);
                    eprintln!("scan project done.");
                }

                let response = WorkspaceSymbolResponse::Nested(symbols);
                let response: Response = Response::new_ok(request.id, response);
                sender
                    .send(MainThreadTask::SendResponse(Message::Response(response)))
                    .expect("send failed");
            });
        }
        Err(..) => {
            eprintln!("broken params");
        }
    }
}

fn scan_project(main: PathBuf, vfs: Vfs, _query: &str, elements: &mut Vec<WorkspaceSymbol>) {
    let sema_params = SemaCreationParams::new()
        .set_program_path(main)
        .set_vfs(vfs);
    let mut sa = Sema::new(sema_params);

    sa.parse_project();
    let module = sa.module(sa.program_module_id());

    for &element_id in module.children() {
        append_workspace_symbol_for_element(&sa, element_id, elements);
    }
}

fn append_workspace_symbol_for_element(
    sa: &Sema,
    element_id: ElementId,
    elements: &mut Vec<WorkspaceSymbol>,
) {
    let element = sa.element(element_id);
    let file_id = element.file_id();
    let file = sa.file(file_id);
    let f = file.ast();
    let content = f.content().as_str();
    let line_starts = &file.line_starts;
    let total_span = element.span();

    let result = compute_element_properties(sa, f, element_id);

    if result.is_none() {
        return;
    }

    let (name, name_span, kind) = result.unwrap();

    let _range = span_to_range(content, line_starts, total_span);
    let selection_range = span_to_range(content, line_starts, name_span);

    elements.push(WorkspaceSymbol {
        name,
        kind,
        tags: None,
        container_name: None,
        location: OneOf::Left(Location {
            uri: file_path_to_uri(&file.path),
            range: selection_range,
        }),
        data: None,
    });

    for &child_id in element.children() {
        append_workspace_symbol_for_element(sa, child_id, elements);
    }
}

fn compute_element_properties(
    sa: &Sema,
    f: &File,
    element_id: ElementId,
) -> Option<(String, Span, SymbolKind)> {
    let (name, span, kind) = match element_id {
        ElementId::Class(id) => {
            let class = sa.class(id);
            let node = class.ast(sa);
            let ident_node = node.name()?;
            (
                ident_node.text().to_string(),
                ident_node.span(),
                SymbolKind::CLASS,
            )
        }
        ElementId::Struct(id) => {
            let struct_def = sa.struct_(id);
            let node = struct_def.ast(sa);
            let ident_node = node.name()?;
            (
                ident_node.text().to_string(),
                ident_node.span(),
                SymbolKind::STRUCT,
            )
        }
        ElementId::Trait(id) => {
            let trait_def = sa.trait_(id);
            let node = trait_def.ast(sa);
            let ident_node = node.name()?;
            (
                ident_node.text().to_string(),
                ident_node.span(),
                SymbolKind::INTERFACE,
            )
        }
        ElementId::Enum(id) => {
            let enum_def = sa.enum_(id);
            let node = enum_def.ast(sa);
            let ident_node = node.name()?;
            (
                ident_node.text().to_string(),
                ident_node.span(),
                SymbolKind::ENUM,
            )
        }
        ElementId::Fct(id) => {
            let fct = sa.fct(id);
            let node = fct.ast(sa).as_function();
            let ident_node = node.name()?;
            (
                ident_node.text().to_string(),
                ident_node.span(),
                SymbolKind::FUNCTION,
            )
        }
        ElementId::Global(id) => {
            let global = sa.global(id);
            let node = global.ast(sa);
            let ident_node = node.name()?;
            (
                ident_node.text().to_string(),
                ident_node.span(),
                SymbolKind::VARIABLE,
            )
        }
        ElementId::Const(id) => {
            let const_def = sa.const_(id);
            let node = const_def.ast(sa);
            let ident_node = node.name()?;
            (
                ident_node.text().to_string(),
                ident_node.span(),
                SymbolKind::CONSTANT,
            )
        }
        ElementId::Impl(id) => {
            let impl_def = sa.impl_(id);
            let node = impl_def.ast(sa);
            let mut name: String = "impl".into();

            if let Some(type_param_list) = node.type_param_list() {
                let span = type_param_list.span();
                let type_params_string =
                    &f.content().as_str()[span.start() as usize..span.end() as usize];
                name.push_str(type_params_string);
            }

            let trait_ty = node.trait_type()?;
            let span = trait_ty.span();
            let trait_ty_string = &f.content().as_str()[span.start() as usize..span.end() as usize];
            name.push_str(" ");
            name.push_str(trait_ty_string);
            name.push_str(" for");

            let name_span = node.extended_type()?.span();
            let extended_type_string =
                &f.content().as_str()[name_span.start() as usize..name_span.end() as usize];
            name.push_str(" ");
            name.push_str(extended_type_string);

            (name, name_span, SymbolKind::NAMESPACE)
        }
        ElementId::Extension(id) => {
            let extension = sa.extension(id);
            let node = extension.ast(sa);
            let mut name = String::from("impl");

            if let Some(type_param_list) = node.type_param_list() {
                let span = type_param_list.span();
                let type_params_string =
                    &f.content().as_str()[span.start() as usize..span.end() as usize];
                name.push_str(type_params_string);
            }

            let name_span = node.extended_type()?.span();
            let extended_type_string =
                &f.content().as_str()[name_span.start() as usize..name_span.end() as usize];
            name.push_str(" ");
            name.push_str(extended_type_string);

            (name, name_span, SymbolKind::NAMESPACE)
        }
        ElementId::Alias(id) => {
            let alias = sa.alias(id);
            let node = alias.ast(sa);
            let ident_node = node.name()?;
            (
                ident_node.text().to_string(),
                ident_node.span(),
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
        ElementId::Module(id) => {
            let module = sa.module(id);
            let node = module.ast(sa);
            let ident_node = node.name()?;
            (
                ident_node.text().to_string(),
                ident_node.span(),
                SymbolKind::MODULE,
            )
        }
        ElementId::Use(..) => {
            return None;
        }
    };

    Some((name, span, kind))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;
    use std::sync::Arc;

    fn assert_location(symbol: &WorkspaceSymbol, uri: &str, line: u32, character: u32) {
        if let OneOf::Left(location) = &symbol.location {
            assert_eq!(
                location.uri.as_str(),
                uri,
                "URI mismatch for symbol '{}'",
                symbol.name
            );
            assert_eq!(
                location.range.start.line, line,
                "Line mismatch for symbol '{}'",
                symbol.name
            );
            assert_eq!(
                location.range.start.character, character,
                "Character mismatch for symbol '{}'",
                symbol.name
            );
        } else {
            panic!(
                "Expected location to be OneOf::Left for symbol '{}'",
                symbol.name
            );
        }
    }

    fn scan_workspace(content: &str) -> Vec<WorkspaceSymbol> {
        scan_workspace_with_files(content, &[])
    }

    fn scan_workspace_with_files(content: &str, files: &[(PathBuf, &str)]) -> Vec<WorkspaceSymbol> {
        use std::path::PathBuf;

        let mut sema_params =
            SemaCreationParams::new().set_program_content(Arc::new(content.to_string()));

        for (path, file_content) in files {
            sema_params = sema_params
                .set_file_content(PathBuf::from(path), Arc::new(file_content.to_string()));
        }

        let mut sa = Sema::new(sema_params);
        sa.parse_project();
        let module = sa.module(sa.program_module_id());

        let mut symbols = Vec::new();
        for &element_id in module.children() {
            append_workspace_symbol_for_element(&sa, element_id, &mut symbols);
        }
        symbols
    }

    #[test]
    fn test_workspace_empty() {
        let symbols = scan_workspace("");
        assert_eq!(symbols.len(), 0);
    }

    #[test]
    fn test_workspace_single_function() {
        let symbols = scan_workspace("fn foo() {}");
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "foo");
        assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);
    }

    #[test]
    fn test_workspace_multiple_functions() {
        let symbols = scan_workspace("fn foo() {}\nfn bar() {}\nfn baz() {}");
        assert_eq!(symbols.len(), 3);
        assert_eq!(symbols[0].name, "foo");
        assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);
        assert_eq!(symbols[1].name, "bar");
        assert_eq!(symbols[1].kind, SymbolKind::FUNCTION);
        assert_eq!(symbols[2].name, "baz");
        assert_eq!(symbols[2].kind, SymbolKind::FUNCTION);
    }

    #[test]
    fn test_workspace_struct_with_fields() {
        let symbols = scan_workspace("struct Point { x: Int32, y: Int32 }");
        assert_eq!(symbols.len(), 3); // struct + 2 fields
        assert_eq!(symbols[0].name, "Point");
        assert_eq!(symbols[0].kind, SymbolKind::STRUCT);
        assert_eq!(symbols[1].name, "x");
        assert_eq!(symbols[1].kind, SymbolKind::FIELD);
        assert_eq!(symbols[2].name, "y");
        assert_eq!(symbols[2].kind, SymbolKind::FIELD);
    }

    #[test]
    fn test_workspace_class_with_fields() {
        let symbols = scan_workspace("class Person { name: String, age: Int32 }");
        assert_eq!(symbols.len(), 3); // class + 2 fields
        assert_eq!(symbols[0].name, "Person");
        assert_eq!(symbols[0].kind, SymbolKind::CLASS);
        assert_eq!(symbols[1].name, "name");
        assert_eq!(symbols[1].kind, SymbolKind::FIELD);
        assert_eq!(symbols[2].name, "age");
        assert_eq!(symbols[2].kind, SymbolKind::FIELD);
    }

    #[test]
    fn test_workspace_enum_with_variants() {
        let symbols = scan_workspace("enum Color { Red, Green(Int32), Blue(Int32, Int32) }");
        assert_eq!(symbols.len(), 4); // enum + 3 variants
        assert_eq!(symbols[0].name, "Color");
        assert_eq!(symbols[0].kind, SymbolKind::ENUM);
        assert_eq!(symbols[1].name, "Red");
        assert_eq!(symbols[1].kind, SymbolKind::ENUM_MEMBER);
        assert_eq!(symbols[2].name, "Green");
        assert_eq!(symbols[2].kind, SymbolKind::ENUM_MEMBER);
        assert_eq!(symbols[3].name, "Blue");
        assert_eq!(symbols[3].kind, SymbolKind::ENUM_MEMBER);
    }

    #[test]
    fn test_workspace_enum_with_named_fields() {
        let symbols = scan_workspace("enum Color { Red, Green { value: Int32 } }");
        assert_eq!(symbols.len(), 4); // enum + 2 variants + 1 field
        assert_eq!(symbols[0].name, "Color");
        assert_eq!(symbols[0].kind, SymbolKind::ENUM);
        assert_eq!(symbols[1].name, "Red");
        assert_eq!(symbols[1].kind, SymbolKind::ENUM_MEMBER);
        assert_eq!(symbols[2].name, "Green");
        assert_eq!(symbols[2].kind, SymbolKind::ENUM_MEMBER);
        assert_eq!(symbols[3].name, "value");
        assert_eq!(symbols[3].kind, SymbolKind::FIELD);
    }

    #[test]
    fn test_workspace_impl_with_methods_and_aliases() {
        let symbols = scan_workspace(
            "fn top_level() {}\nimpl Foo for Bar { fn method1() {} fn method2() {} type Alias1 = Int32; type Alias2 = String; }",
        );
        assert_eq!(symbols.len(), 6); // 1 function + impl + 2 methods + 2 aliases
        assert_eq!(symbols[0].name, "top_level");
        assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);
        assert_eq!(symbols[1].name, "impl Foo for Bar");
        assert_eq!(symbols[1].kind, SymbolKind::NAMESPACE);
        assert_eq!(symbols[2].name, "method1");
        assert_eq!(symbols[2].kind, SymbolKind::FUNCTION);
        assert_eq!(symbols[3].name, "method2");
        assert_eq!(symbols[3].kind, SymbolKind::FUNCTION);
        assert_eq!(symbols[4].name, "Alias1");
        assert_eq!(symbols[4].kind, SymbolKind::CONSTANT);
        assert_eq!(symbols[5].name, "Alias2");
        assert_eq!(symbols[5].kind, SymbolKind::CONSTANT);
    }

    #[test]
    fn test_workspace_impl_extension() {
        let symbols = scan_workspace("impl Foo { fn method1() {} fn method2() {} }");
        assert_eq!(symbols.len(), 3); // impl + 2 methods
        assert_eq!(symbols[0].name, "impl Foo");
        assert_eq!(symbols[0].kind, SymbolKind::NAMESPACE);
        assert_eq!(symbols[1].name, "method1");
        assert_eq!(symbols[1].kind, SymbolKind::FUNCTION);
        assert_eq!(symbols[2].name, "method2");
        assert_eq!(symbols[2].kind, SymbolKind::FUNCTION);
    }

    #[test]
    fn test_workspace_mod_with_function() {
        let symbols = scan_workspace("mod foo { fn bar() {} }");
        assert_eq!(symbols.len(), 2); // module + function
        assert_eq!(symbols[0].name, "foo");
        assert_eq!(symbols[0].kind, SymbolKind::MODULE);
        assert_eq!(symbols[1].name, "bar");
        assert_eq!(symbols[1].kind, SymbolKind::FUNCTION);
    }

    #[test]
    fn test_workspace_trait_with_methods() {
        let symbols = scan_workspace("trait Drawable { fn draw(); fn resize(); type X; }");
        assert_eq!(symbols.len(), 4); // trait + 2 methods + 1 alias
        assert_eq!(symbols[0].name, "Drawable");
        assert_eq!(symbols[0].kind, SymbolKind::INTERFACE);
        assert_eq!(symbols[1].name, "draw");
        assert_eq!(symbols[1].kind, SymbolKind::FUNCTION);
        assert_eq!(symbols[2].name, "resize");
        assert_eq!(symbols[2].kind, SymbolKind::FUNCTION);
        assert_eq!(symbols[3].name, "X");
        assert_eq!(symbols[3].kind, SymbolKind::CONSTANT);
    }

    #[test]
    fn test_workspace_with_multiple_files() {
        let current = std::env::current_dir().unwrap();
        let main_path = current.join("main.dora");
        let foo_path = current.join("foo.dora");
        let symbols = scan_workspace_with_files(
            "mod foo; fn main() {}",
            &[(foo_path.clone(), "fn bar() {}")],
        );
        assert_eq!(symbols.len(), 3);

        assert_eq!(symbols[0].name, "foo");
        assert_eq!(symbols[0].kind, SymbolKind::MODULE);
        assert_location(&symbols[0], file_path_to_uri(&main_path).as_str(), 0, 4);

        assert_eq!(symbols[1].name, "bar");
        assert_eq!(symbols[1].kind, SymbolKind::FUNCTION);
        assert_location(&symbols[1], file_path_to_uri(&foo_path).as_str(), 0, 3);

        assert_eq!(symbols[2].name, "main");
        assert_eq!(symbols[2].kind, SymbolKind::FUNCTION);
        assert_location(&symbols[2], file_path_to_uri(&main_path).as_str(), 0, 12);
    }
}
