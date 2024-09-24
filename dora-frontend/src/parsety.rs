use dora_parser::ast;

use crate::access::sym_accessible_from;
use crate::readty::read_type_path;
use crate::sema::{ModuleDefinitionId, SourceFileId, TypeParamDefinition};
use crate::sym::{ModuleSymTable, SymbolKind};
use crate::{
    check_type_params, replace_type, AliasReplacement, ErrorMessage, Sema, SourceType,
    SourceTypeArray, Span,
};
use std::cell::OnceCell;

#[allow(unused)]
pub struct ParsedType {
    id: ast::NodeId,
    span: Span,
    kind: ParsedTypeKind,
    parsed_ty: OnceCell<SourceType>,
    expanded_ty: OnceCell<SourceType>,
}

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
        parsed_ty: OnceCell::new(),
        expanded_ty: OnceCell::new(),
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

pub struct TypeContext<'a> {
    pub allow_self: bool,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub type_param_defs: &'a TypeParamDefinition,
}

pub fn check_parsed_type(sa: &Sema, ctxt: &TypeContext, parsed_ty: &ParsedType) -> SourceType {
    let ty = match parsed_ty.kind {
        ParsedTypeKind::This => check_parsed_type_this(sa, ctxt, parsed_ty),
        ParsedTypeKind::Regular { .. } => check_parsed_type_regular(sa, ctxt, parsed_ty),
        ParsedTypeKind::Tuple { .. } => check_parsed_type_tuple(sa, ctxt, parsed_ty),
        ParsedTypeKind::Lambda { .. } => check_parsed_type_lambda(sa, ctxt, parsed_ty),
        ParsedTypeKind::Error { .. } => SourceType::Error,
    };

    assert!(parsed_ty.parsed_ty.set(ty.clone()).is_ok());
    ty
}

fn check_parsed_type_this(sa: &Sema, ctxt: &TypeContext, parsed_ty: &ParsedType) -> SourceType {
    if ctxt.allow_self {
        SourceType::This
    } else {
        sa.report(
            ctxt.file_id,
            parsed_ty.span,
            ErrorMessage::SelfTypeUnavailable,
        );
        SourceType::Error
    }
}

fn check_parsed_type_regular(sa: &Sema, ctxt: &TypeContext, parsed_ty: &ParsedType) -> SourceType {
    let (sym, type_params) = match parsed_ty.kind {
        ParsedTypeKind::Regular {
            ref symbol,
            ref type_params,
        } => (symbol.clone(), type_params),
        _ => unreachable!(),
    };

    if !sym.is_type_param() && !sym_accessible_from(sa, sym.clone(), ctxt.module_id) {
        let msg = ErrorMessage::NotAccessible("xxx".into());
        sa.report(ctxt.file_id, parsed_ty.span, msg);
    }

    match sym {
        SymbolKind::TypeAlias(id) => {
            assert!(type_params.is_empty());
            SourceType::TypeAlias(id)
        }

        SymbolKind::TypeParam(id) => {
            assert!(type_params.is_empty());
            SourceType::TypeParam(id)
        }

        SymbolKind::Class(..)
        | SymbolKind::Enum(..)
        | SymbolKind::Struct(..)
        | SymbolKind::Trait(..) => {
            let type_params = type_params
                .iter()
                .map(|tp| check_parsed_type(sa, ctxt, tp))
                .collect::<Vec<_>>();
            let type_params = SourceTypeArray::with(type_params);

            let type_param_defs = sym_type_params(sa, sym.clone());

            if check_type_params(
                sa,
                type_param_defs,
                type_params.types(),
                ctxt.file_id,
                parsed_ty.span,
                ctxt.type_param_defs,
            ) {
                ty_for_sym(sa, sym, type_params)
            } else {
                SourceType::Error
            }
        }

        _ => unreachable!(),
    }
}

fn sym_type_params(sa: &Sema, sym: SymbolKind) -> &TypeParamDefinition {
    match sym {
        SymbolKind::Class(id) => sa.class(id).type_params(),
        SymbolKind::Struct(id) => sa.struct_(id).type_params(),
        SymbolKind::Enum(id) => sa.enum_(id).type_params(),
        SymbolKind::Trait(id) => sa.trait_(id).type_params(),
        _ => unimplemented!(),
    }
}

fn ty_for_sym(sa: &Sema, sym: SymbolKind, type_params: SourceTypeArray) -> SourceType {
    match sym {
        SymbolKind::Class(id) => SourceType::Class(id, type_params),
        SymbolKind::Struct(id) => {
            let struct_ = sa.struct_(id);
            if let Some(primitive_ty) = struct_.primitive_ty.clone() {
                assert!(type_params.is_empty());
                primitive_ty
            } else {
                SourceType::Struct(id, type_params)
            }
        }
        SymbolKind::Enum(id) => SourceType::Enum(id, type_params),
        SymbolKind::Trait(id) => SourceType::Trait(id, type_params),
        _ => unimplemented!(),
    }
}

fn check_parsed_type_tuple(sa: &Sema, ctxt: &TypeContext, parsed_ty: &ParsedType) -> SourceType {
    let subtypes = match parsed_ty.kind {
        ParsedTypeKind::Tuple { ref subtypes } => subtypes,
        _ => unreachable!(),
    };

    let subtypes = subtypes
        .iter()
        .map(|t| check_parsed_type(sa, ctxt, t))
        .collect::<Vec<_>>();
    let subtypes = SourceTypeArray::with(subtypes);

    if subtypes.is_empty() {
        SourceType::Unit
    } else {
        SourceType::Tuple(subtypes)
    }
}

fn check_parsed_type_lambda(sa: &Sema, ctxt: &TypeContext, parsed_ty: &ParsedType) -> SourceType {
    let (params, return_ty) = match parsed_ty.kind {
        ParsedTypeKind::Lambda {
            ref params,
            ref return_ty,
        } => (params, return_ty),
        _ => unreachable!(),
    };

    let params = params
        .iter()
        .map(|t| check_parsed_type(sa, ctxt, t))
        .collect::<Vec<_>>();
    let params = SourceTypeArray::with(params);

    let return_ty = if let Some(return_ty) = return_ty {
        check_parsed_type(sa, ctxt, return_ty)
    } else {
        SourceType::Unit
    };

    SourceType::Lambda(params, Box::new(return_ty))
}

pub fn expand_parsed_type(sa: &Sema, parsed_ty: &ParsedType) -> SourceType {
    let parsed_source_ty = parsed_ty
        .parsed_ty
        .get()
        .cloned()
        .expect("parsed type missing");
    let expanded_ty = replace_type(
        sa,
        parsed_source_ty,
        None,
        None,
        AliasReplacement::ReplaceWithActualType,
    );
    assert!(parsed_ty.expanded_ty.set(expanded_ty.clone()).is_ok());
    expanded_ty
}
