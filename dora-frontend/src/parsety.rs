use dora_parser::ast;

use crate::readty::read_type_path;
use crate::sema::SourceFileId;
use crate::sym::{ModuleSymTable, SymbolKind};
use crate::{ErrorMessage, Sema, SourceType, Span};
use std::cell::OnceCell;

#[allow(unused)]
pub struct ParsedType {
    id: ast::NodeId,
    span: Span,
    kind: ParsedTypeKind,
    ty: OnceCell<SourceType>,
}

#[allow(unused)]
pub enum ParsedTypeKind {
    This,

    Regular {
        symbol: SymbolKind,
        type_params: Vec<Box<ParsedType>>,
    },

    Tuple {
        subtypes: Vec<Box<ParsedType>>,
    },

    Lambda {
        params: Vec<Box<ParsedType>>,
        return_ty: Option<Box<ParsedType>>,
    },

    Error,
}

#[allow(unused)]
pub fn parse_type(
    sa: &Sema,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    t: &ast::TypeData,
) -> Box<ParsedType> {
    let kind = match *t {
        ast::TypeData::This(_) => ParsedTypeKind::This,
        ast::TypeData::Regular(ref node) => parse_type_regular(sa, table, file_id, node),
        ast::TypeData::Tuple(ref node) => parse_type_tuple(sa, table, file_id, node),
        ast::TypeData::Lambda(ref node) => parse_type_lambda(sa, table, file_id, node),
        ast::TypeData::Error { .. } => ParsedTypeKind::Error,
    };

    Box::new(ParsedType {
        id: t.id(),
        span: t.span(),
        kind,
        ty: OnceCell::new(),
    })
}

fn parse_type_regular(
    sa: &Sema,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    node: &ast::TypeRegularType,
) -> ParsedTypeKind {
    let sym = read_type_path(sa, table, file_id, node);

    if sym.is_err() {
        return ParsedTypeKind::Error;
    }

    let sym = sym.unwrap();

    let mut type_params = Vec::new();

    for param in &node.params {
        let ty = parse_type(sa, table, file_id, param);
        type_params.push(ty);
    }

    match sym {
        Some(
            SymbolKind::Class(..)
            | SymbolKind::Trait(..)
            | SymbolKind::Struct(..)
            | SymbolKind::Enum(..),
        ) => ParsedTypeKind::Regular {
            symbol: sym.expect("missing symbol"),
            type_params,
        },

        Some(SymbolKind::TypeParam(..) | SymbolKind::TypeAlias(..)) => {
            if !node.params.is_empty() {
                let msg = ErrorMessage::NoTypeParamsExpected;
                sa.report(file_id, node.span, msg);
            }

            ParsedTypeKind::Regular {
                symbol: sym.expect("missing symbol"),
                type_params: Vec::new(),
            }
        }

        Some(_) => {
            let name = node
                .path
                .names
                .last()
                .cloned()
                .unwrap()
                .name_as_string
                .clone();
            let msg = ErrorMessage::UnknownType(name);
            sa.report(file_id, node.span, msg);
            ParsedTypeKind::Error
        }

        None => {
            let name = node
                .path
                .names
                .last()
                .cloned()
                .unwrap()
                .name_as_string
                .clone();
            let msg = ErrorMessage::UnknownIdentifier(name);
            sa.report(file_id, node.span, msg);
            ParsedTypeKind::Error
        }
    }
}

fn parse_type_lambda(
    sa: &Sema,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    node: &ast::TypeLambdaType,
) -> ParsedTypeKind {
    let mut params = vec![];

    for param in &node.params {
        let ty = parse_type(sa, table, file_id, param);
        params.push(ty);
    }

    let return_ty = if let Some(ref ret) = node.ret {
        Some(parse_type(sa, table, file_id, ret))
    } else {
        None
    };

    ParsedTypeKind::Lambda { params, return_ty }
}

fn parse_type_tuple(
    sa: &Sema,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    node: &ast::TypeTupleType,
) -> ParsedTypeKind {
    let mut subtypes = Vec::new();

    for subtype in &node.subtypes {
        let ty = parse_type(sa, table, file_id, subtype);
        subtypes.push(ty);
    }

    ParsedTypeKind::Tuple { subtypes }
}
