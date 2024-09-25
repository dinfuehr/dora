use dora_parser::ast;

use crate::access::sym_accessible_from;
use crate::readty::read_type_path;
use crate::sema::{ModuleDefinitionId, SourceFileId, TypeParamDefinition};
use crate::sym::{ModuleSymTable, SymbolKind};
use crate::{
    check_type_params, replace_type, AliasReplacement, ErrorMessage, Sema, SourceType,
    SourceTypeArray, Span,
};
use std::cell::RefCell;

#[derive(Debug)]
pub enum ParsedType {
    Fixed(SourceType),
    Ast(ParsedTypeAst),
}

impl ParsedType {
    pub fn new(ty: SourceType) -> Box<ParsedType> {
        Box::new(ParsedType::Fixed(ty))
    }

    pub fn ty(&self) -> SourceType {
        match self {
            ParsedType::Fixed(ty) => ty.clone(),
            ParsedType::Ast(ref ast) => ast.ty(),
        }
    }
}

#[derive(Debug)]
pub struct ParsedTypeAst {
    #[allow(unused)]
    id: ast::NodeId,
    span: Span,
    kind: ParsedTypeKind,
    ty: RefCell<Option<SourceType>>,
}

impl ParsedTypeAst {
    pub fn ty(&self) -> SourceType {
        self.ty.borrow().clone().expect("missing ty")
    }

    fn set_ty(&self, ty: SourceType) {
        let mut ty_field = self.ty.borrow_mut();
        *ty_field = Some(ty);
    }
}

#[derive(Debug)]
pub enum ParsedTypeKind {
    This,

    Regular {
        symbol: SymbolKind,
        type_params: Vec<Box<ParsedTypeAst>>,
    },

    Tuple {
        subtypes: Vec<Box<ParsedTypeAst>>,
    },

    Lambda {
        params: Vec<Box<ParsedTypeAst>>,
        return_ty: Option<Box<ParsedTypeAst>>,
    },

    Error,
}

pub fn parse_type(
    sa: &Sema,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    t: &ast::TypeData,
) -> Box<ParsedTypeAst> {
    let kind = match *t {
        ast::TypeData::This(_) => ParsedTypeKind::This,
        ast::TypeData::Regular(ref node) => parse_type_regular(sa, table, file_id, node),
        ast::TypeData::Tuple(ref node) => parse_type_tuple(sa, table, file_id, node),
        ast::TypeData::Lambda(ref node) => parse_type_lambda(sa, table, file_id, node),
        ast::TypeData::Error { .. } => ParsedTypeKind::Error,
    };

    Box::new(ParsedTypeAst {
        id: t.id(),
        span: t.span(),
        kind,
        ty: RefCell::new(None),
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

pub fn convert_parsed_type(sa: &Sema, ctxt: &TypeContext, parsed_ty: &ParsedTypeAst) -> SourceType {
    let ty = match parsed_ty.kind {
        ParsedTypeKind::This => convert_parsed_type_this(sa, ctxt, parsed_ty),
        ParsedTypeKind::Regular { .. } => convert_parsed_type_regular(sa, ctxt, parsed_ty),
        ParsedTypeKind::Tuple { .. } => convert_parsed_type_tuple(sa, ctxt, parsed_ty),
        ParsedTypeKind::Lambda { .. } => convert_parsed_type_lambda(sa, ctxt, parsed_ty),
        ParsedTypeKind::Error { .. } => SourceType::Error,
    };

    parsed_ty.set_ty(ty.clone());
    ty
}

fn convert_parsed_type_this(
    sa: &Sema,
    ctxt: &TypeContext,
    parsed_ty: &ParsedTypeAst,
) -> SourceType {
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

fn convert_parsed_type_regular(
    sa: &Sema,
    ctxt: &TypeContext,
    parsed_ty: &ParsedTypeAst,
) -> SourceType {
    let (sym, type_params) = match parsed_ty.kind {
        ParsedTypeKind::Regular {
            ref symbol,
            ref type_params,
        } => (symbol.clone(), type_params),
        _ => unreachable!(),
    };

    if !sym.is_type_param() && !sym_accessible_from(sa, sym.clone(), ctxt.module_id) {
        let msg = ErrorMessage::NotAccessible;
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
                .map(|tp| convert_parsed_type(sa, ctxt, tp))
                .collect::<Vec<_>>();
            let type_params = SourceTypeArray::with(type_params);

            ty_for_sym(sa, sym, type_params)
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
            if !type_params.is_empty() {
                SourceType::Struct(id, type_params)
            } else {
                let struct_ = sa.struct_(id);
                if let Some(primitive_ty) = struct_.primitive_ty.clone() {
                    assert!(type_params.is_empty());
                    primitive_ty
                } else {
                    SourceType::Struct(id, type_params)
                }
            }
        }
        SymbolKind::Enum(id) => SourceType::Enum(id, type_params),
        SymbolKind::Trait(id) => SourceType::Trait(id, type_params),
        _ => unimplemented!(),
    }
}

fn convert_parsed_type_tuple(
    sa: &Sema,
    ctxt: &TypeContext,
    parsed_ty: &ParsedTypeAst,
) -> SourceType {
    let subtypes = match parsed_ty.kind {
        ParsedTypeKind::Tuple { ref subtypes } => subtypes,
        _ => unreachable!(),
    };

    let subtypes = subtypes
        .iter()
        .map(|t| convert_parsed_type(sa, ctxt, t))
        .collect::<Vec<_>>();
    let subtypes = SourceTypeArray::with(subtypes);

    if subtypes.is_empty() {
        SourceType::Unit
    } else {
        SourceType::Tuple(subtypes)
    }
}

fn convert_parsed_type_lambda(
    sa: &Sema,
    ctxt: &TypeContext,
    parsed_ty: &ParsedTypeAst,
) -> SourceType {
    let (params, return_ty) = match parsed_ty.kind {
        ParsedTypeKind::Lambda {
            ref params,
            ref return_ty,
        } => (params, return_ty),
        _ => unreachable!(),
    };

    let params = params
        .iter()
        .map(|t| convert_parsed_type(sa, ctxt, t))
        .collect::<Vec<_>>();
    let params = SourceTypeArray::with(params);

    let return_ty = if let Some(return_ty) = return_ty {
        convert_parsed_type(sa, ctxt, return_ty)
    } else {
        SourceType::Unit
    };

    SourceType::Lambda(params, Box::new(return_ty))
}

pub fn check_parsed_type(sa: &Sema, ctxt: &TypeContext, parsed_ty: &ParsedTypeAst) -> SourceType {
    let ty = parsed_ty.ty();

    let checked_ty = match ty.clone() {
        SourceType::Any | SourceType::Ptr => {
            unreachable!()
        }
        SourceType::TypeAlias(..)
        | SourceType::Error
        | SourceType::Bool
        | SourceType::UInt8
        | SourceType::Char
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Unit
        | SourceType::TypeParam(..)
        | SourceType::This => ty,
        SourceType::Lambda(params, return_type) => {
            let (parsed_params, parsed_return_type) = match parsed_ty.kind {
                ParsedTypeKind::Lambda {
                    ref params,
                    ref return_ty,
                } => (params, return_ty),
                _ => unreachable!(),
            };

            assert_eq!(params.len(), parsed_params.len());
            let mut new_params = Vec::with_capacity(parsed_params.len());

            for idx in 0..parsed_params.len() {
                let parsed_param = &parsed_params[idx];
                assert_eq!(params[idx], parsed_param.ty());

                let ty = check_parsed_type(sa, ctxt, parsed_param);
                new_params.push(ty);
            }

            let new_params = SourceTypeArray::with(new_params);
            let new_return_type: SourceType = if let Some(parsed_return_type) = parsed_return_type {
                assert_eq!(*return_type, parsed_return_type.ty());
                check_parsed_type(sa, ctxt, parsed_return_type)
            } else {
                SourceType::Unit
            };

            SourceType::Lambda(new_params, Box::new(new_return_type))
        }
        SourceType::Tuple(subtypes) => {
            let parsed_subtypes = match parsed_ty.kind {
                ParsedTypeKind::Tuple { ref subtypes } => subtypes,
                _ => unreachable!(),
            };

            assert_eq!(subtypes.len(), parsed_subtypes.len());
            let mut new_type_params = Vec::with_capacity(parsed_subtypes.len());

            for idx in 0..parsed_subtypes.len() {
                let parsed_subtype = &parsed_subtypes[idx];
                assert_eq!(subtypes[idx], parsed_subtype.ty());

                let ty = check_parsed_type(sa, ctxt, parsed_subtype);
                new_type_params.push(ty);
            }

            SourceType::Tuple(SourceTypeArray::with(new_type_params))
        }
        SourceType::Class(_, type_params)
        | SourceType::Struct(_, type_params)
        | SourceType::Enum(_, type_params)
        | SourceType::Trait(_, type_params) => {
            check_parsed_type_record(sa, ctxt, parsed_ty, type_params)
        }
    };

    parsed_ty.set_ty(checked_ty.clone());
    checked_ty
}

fn check_parsed_type_record(
    sa: &Sema,
    ctxt: &TypeContext,
    parsed_ty: &ParsedTypeAst,
    type_params: SourceTypeArray,
) -> SourceType {
    let (symbol, parsed_type_params) = match parsed_ty.kind {
        ParsedTypeKind::Regular {
            ref symbol,
            ref type_params,
            ..
        } => (symbol.clone(), type_params),
        _ => unreachable!(),
    };

    assert_eq!(type_params.len(), parsed_type_params.len());
    let mut new_type_params = Vec::with_capacity(parsed_type_params.len());

    for idx in 0..type_params.len() {
        let parsed_type_param = &parsed_type_params[idx];
        assert_eq!(type_params[idx], parsed_type_param.ty());

        let ty = check_parsed_type(sa, ctxt, parsed_type_param);
        new_type_params.push(ty);
    }

    let new_type_params = SourceTypeArray::with(new_type_params);
    let type_param_defs = sym_type_params(sa, symbol.clone());

    if check_type_params(
        sa,
        type_param_defs,
        new_type_params.types(),
        ctxt.file_id,
        parsed_ty.span,
        ctxt.type_param_defs,
    ) {
        ty_for_sym(sa, symbol, new_type_params)
    } else {
        SourceType::Error
    }
}

pub fn expand_parsed_type(sa: &Sema, parsed_ty: &ParsedTypeAst) -> SourceType {
    let parsed_source_ty = parsed_ty.ty();
    let expanded_ty = replace_type(
        sa,
        parsed_source_ty,
        None,
        None,
        AliasReplacement::ReplaceWithActualType,
    );
    parsed_ty.set_ty(expanded_ty.clone());
    expanded_ty
}
