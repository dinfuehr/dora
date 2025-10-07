use std::sync::Arc;

use lsp_server::{Message, Request, Response};
use lsp_types::{DocumentSymbol, DocumentSymbolResponse, Position, Range, SymbolKind};

use dora_parser::ast::{self, AstNodeBase};
use dora_parser::{Parser, Span, compute_line_column, compute_line_starts};

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
    let line_starts = compute_line_starts(&content);
    let parser = Parser::from_shared_string(content.clone());
    let (file, _errors) = parser.parse();

    let mut scanner = SymbolScanner {
        symbols: Vec::new(),
        levels: Vec::new(),
        content,
    };

    ast::visit_node(&mut scanner, file.root());

    transform(&line_starts, scanner.symbols)
}

#[allow(unused)]
fn parse_file2(content: Arc<String>) -> Vec<DocumentSymbol> {
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
            // Build impl name similar to the old implementation
            let name = String::from("impl");
            let name_span = f.node(node.extended_type).span();
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

fn transform(line_starts: &[u32], symbols: Vec<Symbol>) -> Vec<DocumentSymbol> {
    symbols
        .into_iter()
        .map(|s| {
            let range = range_from_span(line_starts, s.total_span);
            let selection_range = range_from_span(line_starts, s.name_span);

            let children = s.children.map(|c| transform(line_starts, c));

            #[allow(deprecated)]
            DocumentSymbol {
                name: s.name,
                kind: convert_kind(s.kind),
                tags: None,
                detail: None,
                range,
                deprecated: None,
                selection_range,
                children,
            }
        })
        .collect()
}

fn convert_kind(kind: DoraSymbolKind) -> SymbolKind {
    match kind {
        DoraSymbolKind::Function => SymbolKind::FUNCTION,
        DoraSymbolKind::Class => SymbolKind::CLASS,
        DoraSymbolKind::ClassField => SymbolKind::FIELD,
        DoraSymbolKind::Const => SymbolKind::CONSTANT,
        DoraSymbolKind::Struct => SymbolKind::STRUCT,
        DoraSymbolKind::StructField => SymbolKind::FIELD,
        DoraSymbolKind::Enum => SymbolKind::ENUM,
        DoraSymbolKind::EnumVariant => SymbolKind::ENUM_MEMBER,
        DoraSymbolKind::Impl => SymbolKind::NAMESPACE,
        DoraSymbolKind::Module => SymbolKind::NAMESPACE,
        DoraSymbolKind::Trait => SymbolKind::INTERFACE,
        DoraSymbolKind::Global => SymbolKind::VARIABLE,
    }
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

struct Symbol {
    name: String,
    name_span: Span,
    kind: DoraSymbolKind,
    total_span: Span,
    children: Option<Vec<Symbol>>,
}

enum DoraSymbolKind {
    Function,
    Class,
    ClassField,
    Const,
    Struct,
    StructField,
    Enum,
    EnumVariant,
    Impl,
    Module,
    Trait,
    Global,
}

struct SymbolScanner {
    symbols: Vec<Symbol>,
    levels: Vec<usize>,
    content: Arc<String>,
}

impl SymbolScanner {
    fn add_symbol(
        &mut self,
        name: String,
        name_span: Span,
        kind: DoraSymbolKind,
        total_span: Span,
    ) {
        self.symbols.push(Symbol {
            name,
            name_span,
            kind,
            total_span,
            children: None,
        });
    }

    fn start_children(&mut self) {
        self.levels.push(self.symbols.len() - 1);
    }

    fn stop_children(&mut self) {
        let parent = self.levels.pop().expect("missing start");
        let children = self.symbols.drain(parent + 1..).collect();
        self.symbols.last_mut().expect("missing parent").children = Some(children);
    }
}

impl ast::Visitor for SymbolScanner {
    fn visit_module(&mut self, ast_node: ast::AstModule) {
        let f = ast_node.file();
        let node = ast_node.raw_node().as_module();
        let (name, name_span) = ensure_name(f, node.name, "<mod>", node.span);
        self.add_symbol(name, name_span, DoraSymbolKind::Module, node.span);

        self.start_children();
        ast::walk_children(self, ast_node);
        self.stop_children();
    }

    fn visit_trait(&mut self, ast_node: ast::AstTrait) {
        let f = ast_node.file();
        let node = ast_node.raw_node().as_trait();
        let (name, name_span) = ensure_name(f, node.name, "<trait>", node.span);
        self.add_symbol(name, name_span, DoraSymbolKind::Trait, node.span);
    }

    fn visit_global(&mut self, ast_node: ast::AstGlobal) {
        let f = ast_node.file();
        let node = ast_node.raw_node().as_global();
        let (name, name_span) = ensure_name(f, node.name, "<global>", node.span);
        self.add_symbol(name, name_span, DoraSymbolKind::Global, node.span);
    }

    fn visit_impl(&mut self, ast_node: ast::AstImpl) {
        let f = ast_node.file();
        let node = ast_node.raw_node().as_impl();
        let mut name: String = "impl".into();

        if let Some(type_param_list_id) = node.type_params {
            let type_param_list = f
                .node(type_param_list_id)
                .to_type_param_list()
                .expect("type param list expected");
            let span = type_param_list.span;
            let type_params_string =
                &self.content.as_str()[span.start() as usize..span.end() as usize];
            name.push_str(type_params_string);
        }

        if let Some(trait_ty_id) = node.trait_type {
            let trait_ty = f.node(trait_ty_id);
            let span = trait_ty.span();
            let trait_ty_string =
                &self.content.as_str()[span.start() as usize..span.end() as usize];
            name.push_str(" ");
            name.push_str(trait_ty_string);
            name.push_str(" for");
        }

        let name_span = f.node(node.extended_type).span();
        let extended_type_string =
            &self.content.as_str()[name_span.start() as usize..name_span.end() as usize];
        name.push_str(" ");
        name.push_str(extended_type_string);
        self.add_symbol(name, name_span, DoraSymbolKind::Impl, node.span);

        self.start_children();
        ast::walk_children(self, ast_node);
        self.stop_children();
    }

    fn visit_const(&mut self, ast_node: ast::AstConst) {
        let f = ast_node.file();
        let node = ast_node.raw_node().as_const();
        let (name, name_span) = ensure_name(f, node.name, "<const>", node.span);
        self.add_symbol(name, name_span, DoraSymbolKind::Const, node.span);
    }

    fn visit_class(&mut self, ast_node: ast::AstClass) {
        let f = ast_node.file();
        let node = ast_node.raw_node().as_class();
        let (name, name_span) = ensure_name(f, node.name, "<class>", node.span);
        self.add_symbol(name, name_span, DoraSymbolKind::Class, node.span);

        self.start_children();
        for &field_id in &node.fields {
            let field = f.node(field_id).as_field();
            let (name, name_span) = ensure_name(f, field.name, "<field>", field.span);
            self.add_symbol(name, name_span, DoraSymbolKind::ClassField, field.span);
        }
        self.stop_children();
    }

    fn visit_struct(&mut self, ast_node: ast::AstStruct) {
        let f = ast_node.file();
        let node = ast_node.raw_node().as_struct();
        let (name, name_span) = ensure_name(f, node.name, "<struct>", node.span);
        self.add_symbol(name, name_span, DoraSymbolKind::Struct, node.span);

        self.start_children();
        for &field_id in &node.fields {
            let field = f.node(field_id).as_field();
            let (name, name_span) = ensure_name(f, field.name, "<field>", field.span);
            self.add_symbol(name, name_span, DoraSymbolKind::StructField, field.span);
        }
        self.stop_children();
    }

    fn visit_function(&mut self, ast_node: ast::AstFunction) {
        let f = ast_node.file();
        let node = ast_node.raw_node().as_function();
        let (name, name_span) = ensure_name(f, node.name, "<fn>", node.span);
        self.add_symbol(name, name_span, DoraSymbolKind::Function, node.span);
    }

    fn visit_enum(&mut self, ast_node: ast::AstEnum) {
        let f = ast_node.file();
        let node = ast_node.raw_node().as_enum();
        let (name, name_span) = ensure_name(f, node.name, "<fn>", node.span);
        self.add_symbol(name, name_span, DoraSymbolKind::Enum, node.span);

        self.start_children();
        for variant in &node.variants {
            let (name, name_span) = ensure_name(f, variant.name, "<enum member>", variant.span);
            self.add_symbol(name, name_span, DoraSymbolKind::EnumVariant, node.span);
        }
        self.stop_children();
    }
}

fn ensure_name(
    f: &ast::File,
    ident_id: Option<ast::AstId>,
    default_name: &str,
    default_span: Span,
) -> (String, Span) {
    if let Some(ident_id) = ident_id {
        let ident_node = f.node(ident_id).as_ident();
        (ident_node.name.clone(), ident_node.span)
    } else {
        (default_name.into(), default_span)
    }
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
        let content = Arc::new("fn foo() {}\nfn bar() {}".to_string());
        let symbols = parse_file(content);
        assert_eq!(symbols.len(), 2);
        assert_eq!(symbols[0].name, "foo");
        assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);
        assert_eq!(symbols[1].name, "bar");
        assert_eq!(symbols[1].kind, SymbolKind::FUNCTION);
    }

    #[test]
    fn test_parse_file_class() {
        let content = Arc::new("class Foo {}".to_string());
        let symbols = parse_file(content);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "Foo");
        assert_eq!(symbols[0].kind, SymbolKind::CLASS);
    }

    #[test]
    fn test_parse_file_class_with_fields() {
        let content = Arc::new("class Foo { x: Int32, y: String }".to_string());
        let symbols = parse_file(content);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "Foo");
        assert_eq!(symbols[0].kind, SymbolKind::CLASS);

        let children = symbols[0].children.as_ref().unwrap();
        assert_eq!(children.len(), 2);
        assert_eq!(children[0].name, "x");
        assert_eq!(children[0].kind, SymbolKind::FIELD);
        assert_eq!(children[1].name, "y");
        assert_eq!(children[1].kind, SymbolKind::FIELD);
    }

    #[test]
    fn test_parse_file_struct() {
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
    fn test_parse_file_trait() {
        let content = Arc::new("trait Foo {}".to_string());
        let symbols = parse_file(content);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "Foo");
        assert_eq!(symbols[0].kind, SymbolKind::INTERFACE);
    }

    #[test]
    fn test_parse_file_enum() {
        let content = Arc::new("enum Color { Red, Green, Blue }".to_string());
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
        assert_eq!(children[2].name, "Blue");
        assert_eq!(children[2].kind, SymbolKind::ENUM_MEMBER);
    }

    #[test]
    fn test_parse_file_const() {
        let content = Arc::new("const PI: Float64 = 3.14;".to_string());
        let symbols = parse_file(content);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "PI");
        assert_eq!(symbols[0].kind, SymbolKind::CONSTANT);
    }

    #[test]
    fn test_parse_file_global() {
        let content = Arc::new("let counter: Int32 = 0;".to_string());
        let symbols = parse_file(content);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "counter");
        assert_eq!(symbols[0].kind, SymbolKind::VARIABLE);
    }

    #[test]
    fn test_parse_file_impl() {
        let content = Arc::new("impl Foo {}".to_string());
        let symbols = parse_file(content);
        assert_eq!(symbols.len(), 1);
        assert!(symbols[0].name.starts_with("impl"));
        assert_eq!(symbols[0].kind, SymbolKind::NAMESPACE);
    }

    #[test]
    fn test_parse_file_impl_with_methods() {
        let content = Arc::new("impl Foo { fn bar() {} fn baz() {} }".to_string());
        let symbols = parse_file(content);
        assert_eq!(symbols.len(), 1);
        assert!(symbols[0].name.starts_with("impl"));
        assert_eq!(symbols[0].kind, SymbolKind::NAMESPACE);

        let children = symbols[0].children.as_ref().unwrap();
        assert_eq!(children.len(), 2);
        assert_eq!(children[0].name, "bar");
        assert_eq!(children[0].kind, SymbolKind::FUNCTION);
        assert_eq!(children[1].name, "baz");
        assert_eq!(children[1].kind, SymbolKind::FUNCTION);
    }

    #[test]
    fn test_parse_file_module() {
        let content = Arc::new("mod foo {}".to_string());
        let symbols = parse_file(content);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "foo");
        assert_eq!(symbols[0].kind, SymbolKind::NAMESPACE);
    }

    #[test]
    fn test_parse_file_mixed_elements() {
        let content =
            Arc::new("fn foo() {}\nclass Bar {}\nstruct Baz {}\nenum Qux { A, B }".to_string());
        let symbols = parse_file(content);
        assert_eq!(symbols.len(), 4);
        assert_eq!(symbols[0].name, "foo");
        assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);
        assert_eq!(symbols[1].name, "Bar");
        assert_eq!(symbols[1].kind, SymbolKind::CLASS);
        assert_eq!(symbols[2].name, "Baz");
        assert_eq!(symbols[2].kind, SymbolKind::STRUCT);
        assert_eq!(symbols[3].name, "Qux");
        assert_eq!(symbols[3].kind, SymbolKind::ENUM);
    }

    #[test]
    fn test_parse_file2_empty() {
        let content = Arc::new("".to_string());
        let symbols = parse_file2(content);
        assert_eq!(symbols.len(), 0);
    }

    #[test]
    fn test_parse_file2_single_function() {
        let content = Arc::new("fn foo() {}".to_string());
        let symbols = parse_file2(content);
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "foo");
        assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);
        assert_eq!(symbols[0].children, None);
    }

    #[test]
    fn test_parse_file2_multiple_functions() {
        let content = Arc::new("fn foo() {}\nfn bar() {}\nfn baz() {}".to_string());
        let symbols = parse_file2(content);
        assert_eq!(symbols.len(), 3);
        assert_eq!(symbols[0].name, "foo");
        assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);
        assert_eq!(symbols[1].name, "bar");
        assert_eq!(symbols[1].kind, SymbolKind::FUNCTION);
        assert_eq!(symbols[2].name, "baz");
        assert_eq!(symbols[2].kind, SymbolKind::FUNCTION);
    }

    #[test]
    fn test_parse_file2_impl_with_methods_and_aliases() {
        let content = Arc::new(
            "fn top_level() {}\nimpl Foo for Bar { fn method1() {} fn method2() {} type Alias1 = Int32; type Alias2 = String; }"
                .to_string(),
        );
        let symbols = parse_file2(content);
        assert_eq!(symbols.len(), 2);

        assert_eq!(symbols[0].name, "top_level");
        assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);

        assert!(symbols[1].name.starts_with("impl"));
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
}
