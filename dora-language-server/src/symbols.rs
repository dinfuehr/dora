use std::sync::Arc;

use dora_parser::ast::visit::Visitor;
use lsp_server::{Message, Request, Response};
use lsp_types::{DocumentSymbol, DocumentSymbolResponse, Position, Range, SymbolKind};

use dora_parser::ast::{self, visit};
use dora_parser::{compute_line_column, compute_line_starts, Parser, Span};

use crate::{MainLoopTask, ServerState};

pub(super) fn document_symbol_request(server_state: &mut ServerState, request: Request) {
    let result = serde_json::from_value::<lsp_types::DocumentSymbolParams>(request.params);
    match result {
        Ok(result) => {
            let path = result
                .text_document
                .uri
                .to_file_path()
                .expect("file path expected");
            if let Some(content) = server_state.opened_files.get(&path) {
                let content = content.clone();
                let sender = server_state.threadpool_sender.clone();

                server_state.threadpool.execute(move || {
                    let symbols = parse_file(content);
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

    scanner.visit_file(&file);

    transform(&line_starts, scanner.symbols)
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

impl visit::Visitor for SymbolScanner {
    fn visit_module(&mut self, node: &Arc<ast::Module>) {
        let (name, name_span) = ensure_name(&node.name, "<mod>", node.span);
        self.add_symbol(name, name_span, DoraSymbolKind::Module, node.span);

        self.start_children();
        visit::walk_module(self, node);
        self.stop_children();
    }

    fn visit_trait(&mut self, node: &Arc<ast::Trait>) {
        let (name, name_span) = ensure_name(&node.name, "<trait>", node.span);
        self.add_symbol(name, name_span, DoraSymbolKind::Trait, node.span);
    }

    fn visit_global(&mut self, node: &Arc<ast::Global>) {
        let (name, name_span) = ensure_name(&node.name, "<global>", node.span);
        self.add_symbol(name, name_span, DoraSymbolKind::Global, node.span);
    }

    fn visit_impl(&mut self, node: &Arc<ast::Impl>) {
        let mut name: String = "impl".into();

        if let Some(ref type_params) = node.type_params {
            let span = type_params.span;
            let type_params_string =
                &self.content.as_str()[span.start() as usize..span.end() as usize];
            name.push_str(type_params_string);
        }

        if let Some(ref trait_ty) = node.trait_type {
            let span = trait_ty.span();
            let trait_ty_string =
                &self.content.as_str()[span.start() as usize..span.end() as usize];
            name.push_str(" ");
            name.push_str(trait_ty_string);
            name.push_str(" for");
        }

        let name_span = node.extended_type.span();
        let extended_type_string =
            &self.content.as_str()[name_span.start() as usize..name_span.end() as usize];
        name.push_str(" ");
        name.push_str(extended_type_string);
        self.add_symbol(name, name_span, DoraSymbolKind::Impl, node.span);

        self.start_children();
        visit::walk_impl(self, node);
        self.stop_children();
    }

    fn visit_const(&mut self, node: &Arc<ast::Const>) {
        let (name, name_span) = ensure_name(&node.name, "<const>", node.span);
        self.add_symbol(name, name_span, DoraSymbolKind::Const, node.span);
    }

    fn visit_class(&mut self, node: &Arc<ast::Class>) {
        let (name, name_span) = ensure_name(&node.name, "<class>", node.span);
        self.add_symbol(name, name_span, DoraSymbolKind::Class, node.span);

        self.start_children();
        for field in &node.fields {
            let (name, name_span) = ensure_name(&field.name, "<field>", field.span);
            self.add_symbol(name, name_span, DoraSymbolKind::ClassField, field.span);
        }
        self.stop_children();
    }

    fn visit_struct(&mut self, node: &Arc<ast::Struct>) {
        let (name, name_span) = ensure_name(&node.name, "<struct>", node.span);
        self.add_symbol(name, name_span, DoraSymbolKind::Struct, node.span);

        self.start_children();
        for field in &node.fields {
            let (name, name_span) = ensure_name(&field.name, "<field>", field.span);
            self.add_symbol(name, name_span, DoraSymbolKind::StructField, field.span);
        }
        self.stop_children();
    }

    fn visit_fct(&mut self, node: &Arc<ast::Function>) {
        let (name, name_span) = ensure_name(&node.name, "<fn>", node.span);
        self.add_symbol(name, name_span, DoraSymbolKind::Function, node.span);
    }

    fn visit_method(&mut self, node: &Arc<ast::Function>) {
        let (name, name_span) = ensure_name(&node.name, "<fn>", node.span);
        self.add_symbol(name, name_span, DoraSymbolKind::Function, node.span);
    }

    fn visit_enum(&mut self, node: &Arc<ast::Enum>) {
        let (name, name_span) = ensure_name(&node.name, "<fn>", node.span);
        self.add_symbol(name, name_span, DoraSymbolKind::Enum, node.span);

        self.start_children();
        for variant in &node.variants {
            let (name, name_span) = ensure_name(&variant.name, "<enum member>", variant.span);
            self.add_symbol(name, name_span, DoraSymbolKind::EnumVariant, node.span);
        }
        self.stop_children();
    }
}

fn ensure_name(
    ident: &Option<ast::Ident>,
    default_name: &str,
    default_span: Span,
) -> (String, Span) {
    if let Some(ident) = ident {
        (ident.name_as_string.clone(), ident.span)
    } else {
        (default_name.into(), default_span)
    }
}
