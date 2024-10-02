use dora_parser::ast;

use crate::access::sym_accessible_from;
use crate::readty::read_type_path;
use crate::sema::{
    is_object_safe, ModuleDefinitionId, SourceFileId, TraitDefinitionId, TypeParamDefinition,
};
use crate::sym::{ModuleSymTable, SymbolKind};
use crate::{check_type_params, ErrorMessage, Name, Sema, SourceType, SourceTypeArray, Span};
use std::cell::{OnceCell, RefCell};

#[derive(Clone, Debug)]
pub struct ParsedType {
    ast: Option<ast::Type>,
    parsed_ast: OnceCell<Box<ParsedTypeAst>>,
    ty: RefCell<Option<SourceType>>,
}

impl ParsedType {
    pub fn new_ty(ty: SourceType) -> ParsedType {
        ParsedType {
            ast: None,
            parsed_ast: OnceCell::new(),
            ty: RefCell::new(Some(ty)),
        }
    }

    pub fn new_ast(ast: ast::Type) -> ParsedType {
        ParsedType {
            ast: Some(ast),
            parsed_ast: OnceCell::new(),
            ty: RefCell::new(None),
        }
    }

    fn ast(&self) -> Option<&ast::Type> {
        self.ast.as_ref()
    }

    pub fn span(&self) -> Span {
        self.ast().expect("missing ast node").span()
    }

    fn parsed_ast(&self) -> Option<&ParsedTypeAst> {
        self.parsed_ast.get().map(|ast| &**ast)
    }

    pub fn ty(&self) -> SourceType {
        self.ty.borrow().as_ref().cloned().expect("missing type")
    }

    pub fn set_ty(&self, ty: SourceType) {
        *self.ty.borrow_mut() = Some(ty);
    }
}

#[derive(Clone, Debug)]
pub struct ParsedTypeAst {
    #[allow(unused)]
    id: ast::NodeId,
    span: Span,
    kind: ParsedTypeKind,
}

#[derive(Clone, Debug)]
pub enum ParsedTypeKind {
    This,

    Regular {
        symbol: SymbolKind,
        type_arguments: Vec<ParsedTypeArgument>,
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

#[derive(Clone, Debug)]
pub struct ParsedTypeArgument {
    name: Option<Name>,
    ty: Box<ParsedTypeAst>,
}

pub fn parse_type(
    sa: &Sema,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    parsed_ty: &ParsedType,
) {
    if let Some(node) = parsed_ty.ast.as_ref() {
        let ast = parse_type_inner(sa, table, file_id, node);
        assert!(parsed_ty.parsed_ast.set(ast).is_ok());

        let ty = convert_type_inner(sa, parsed_ty.parsed_ast().unwrap());
        parsed_ty.set_ty(ty);
    }
}

fn parse_type_inner(
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

    match sym {
        Some(SymbolKind::Trait(trait_id)) => {
            parse_type_regular_trait(sa, table, file_id, trait_id, node)
        }

        Some(SymbolKind::Class(..) | SymbolKind::Struct(..) | SymbolKind::Enum(..)) => {
            parse_type_regular_with_arguments(
                sa,
                table,
                file_id,
                sym.expect("missing symbol"),
                node,
            )
        }

        Some(SymbolKind::TypeParam(..) | SymbolKind::TypeAlias(..)) => {
            if !node.params.is_empty() {
                let msg = ErrorMessage::NoTypeParamsExpected;
                sa.report(file_id, node.span, msg);
            }

            ParsedTypeKind::Regular {
                symbol: sym.expect("missing symbol"),
                type_arguments: Vec::new(),
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

fn parse_type_regular_trait(
    sa: &Sema,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    trait_id: TraitDefinitionId,
    node: &ast::TypeRegularType,
) -> ParsedTypeKind {
    let mut type_arguments = Vec::new();
    let mut found_binding = false;

    for param in &node.params {
        let name = if let Some(ref name) = param.name {
            found_binding = true;
            Some(sa.interner.intern(&name.name_as_string))
        } else {
            if found_binding {
                let msg = ErrorMessage::WrongOrderOfGenericsAndBindings;
                sa.report(file_id, param.span, msg);
                return ParsedTypeKind::Error;
            }

            None
        };

        let ty = parse_type_inner(sa, table, file_id, &param.ty);
        let ty_arg = ParsedTypeArgument { name, ty };
        type_arguments.push(ty_arg);
    }

    ParsedTypeKind::Regular {
        symbol: SymbolKind::Trait(trait_id),
        type_arguments,
    }
}

fn parse_type_regular_with_arguments(
    sa: &Sema,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    symbol: SymbolKind,
    node: &ast::TypeRegularType,
) -> ParsedTypeKind {
    let mut type_arguments = Vec::new();

    for param in &node.params {
        if param.name.is_some() {
            let msg = ErrorMessage::UnexpectedTypeBinding;
            sa.report(file_id, param.span, msg);
            return ParsedTypeKind::Error;
        }

        let ty = parse_type_inner(sa, table, file_id, &param.ty);
        let ty_arg = ParsedTypeArgument { name: None, ty };
        type_arguments.push(ty_arg);
    }

    ParsedTypeKind::Regular {
        symbol,
        type_arguments,
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
        let ty = parse_type_inner(sa, table, file_id, param);
        params.push(ty);
    }

    let return_ty = if let Some(ref ret) = node.ret {
        Some(parse_type_inner(sa, table, file_id, ret))
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
        let ty = parse_type_inner(sa, table, file_id, subtype);
        subtypes.push(ty);
    }

    ParsedTypeKind::Tuple { subtypes }
}

pub struct TypeContext<'a> {
    pub allow_self: bool,
    pub module_id: ModuleDefinitionId,
    pub file_id: SourceFileId,
    pub type_param_definition: &'a TypeParamDefinition,
}

fn convert_type_inner(sa: &Sema, parsed_ty: &ParsedTypeAst) -> SourceType {
    match parsed_ty.kind {
        ParsedTypeKind::This => SourceType::This,
        ParsedTypeKind::Regular { .. } => convert_type_regular(sa, parsed_ty),
        ParsedTypeKind::Tuple { .. } => convert_type_tuple(sa, parsed_ty),
        ParsedTypeKind::Lambda { .. } => convert_type_lambda(sa, parsed_ty),
        ParsedTypeKind::Error { .. } => SourceType::Error,
    }
}

fn convert_type_regular(sa: &Sema, parsed_ty: &ParsedTypeAst) -> SourceType {
    let (sym, type_params) = match parsed_ty.kind {
        ParsedTypeKind::Regular {
            ref symbol,
            type_arguments: ref type_params,
        } => (symbol.clone(), type_params),
        _ => unreachable!(),
    };

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
                .map(|tp| {
                    assert!(tp.name.is_none());
                    convert_type_inner(sa, &tp.ty)
                })
                .collect::<Vec<_>>();
            let type_params = SourceTypeArray::with(type_params);

            ty_for_sym(sa, sym, type_params)
        }

        _ => unreachable!(),
    }
}

fn sym_type_param_definition(sa: &Sema, sym: SymbolKind) -> &TypeParamDefinition {
    match sym {
        SymbolKind::Class(id) => sa.class(id).type_param_definition(),
        SymbolKind::Struct(id) => sa.struct_(id).type_param_definition(),
        SymbolKind::Enum(id) => sa.enum_(id).type_param_definition(),
        SymbolKind::Trait(id) => sa.trait_(id).type_param_definition(),
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

fn convert_type_tuple(sa: &Sema, parsed_ty: &ParsedTypeAst) -> SourceType {
    let subtypes = match parsed_ty.kind {
        ParsedTypeKind::Tuple { ref subtypes } => subtypes,
        _ => unreachable!(),
    };

    let subtypes = subtypes
        .iter()
        .map(|t| convert_type_inner(sa, t))
        .collect::<Vec<_>>();
    let subtypes = SourceTypeArray::with(subtypes);

    if subtypes.is_empty() {
        SourceType::Unit
    } else {
        SourceType::Tuple(subtypes)
    }
}

fn convert_type_lambda(sa: &Sema, parsed_ty: &ParsedTypeAst) -> SourceType {
    let (params, return_ty) = match parsed_ty.kind {
        ParsedTypeKind::Lambda {
            ref params,
            ref return_ty,
        } => (params, return_ty),
        _ => unreachable!(),
    };

    let params = params
        .iter()
        .map(|t| convert_type_inner(sa, t))
        .collect::<Vec<_>>();
    let params = SourceTypeArray::with(params);

    let return_ty = if let Some(return_ty) = return_ty {
        convert_type_inner(sa, return_ty)
    } else {
        SourceType::Unit
    };

    SourceType::Lambda(params, Box::new(return_ty))
}

pub fn check_type(sa: &Sema, ctxt: &TypeContext, parsed_ty: &ParsedType) -> SourceType {
    if let Some(parsed_ty_ast) = parsed_ty.parsed_ast() {
        let new_ty = check_type_inner(sa, ctxt, parsed_ty.ty(), parsed_ty_ast);
        parsed_ty.set_ty(new_ty.clone());
        new_ty
    } else {
        parsed_ty.ty()
    }
}

fn check_type_inner(
    sa: &Sema,
    ctxt: &TypeContext,
    ty: SourceType,
    parsed_ty: &ParsedTypeAst,
) -> SourceType {
    match ty.clone() {
        SourceType::Any | SourceType::Ptr => {
            unreachable!()
        }
        SourceType::This => {
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
        SourceType::Error | SourceType::Unit | SourceType::TypeParam(..) => ty,
        SourceType::TypeAlias(..)
        | SourceType::Bool
        | SourceType::UInt8
        | SourceType::Char
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Int32
        | SourceType::Int64 => {
            let symbol = match &parsed_ty.kind {
                ParsedTypeKind::Regular { symbol, .. } => symbol.clone(),
                _ => unreachable!(),
            };

            if !sym_accessible_from(sa, symbol, ctxt.module_id) {
                let msg = ErrorMessage::NotAccessible;
                sa.report(ctxt.file_id, parsed_ty.span, msg);
            }

            ty
        }
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
                let ty = check_type_inner(sa, ctxt, params[idx].clone(), parsed_param);
                new_params.push(ty);
            }

            let new_params = SourceTypeArray::with(new_params);
            let new_return_type: SourceType = if let Some(parsed_return_type) = parsed_return_type {
                check_type_inner(sa, ctxt, *return_type, parsed_return_type)
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
                let ty = check_type_inner(sa, ctxt, subtypes[idx].clone(), parsed_subtype);
                new_type_params.push(ty);
            }

            SourceType::Tuple(SourceTypeArray::with(new_type_params))
        }
        SourceType::Class(_, type_params)
        | SourceType::Struct(_, type_params)
        | SourceType::Enum(_, type_params) => check_type_record(sa, ctxt, parsed_ty, type_params),
        SourceType::Trait(trait_id, type_params) => {
            let result_ty = check_type_record(sa, ctxt, parsed_ty, type_params);

            if !is_object_safe(sa, trait_id) {
                sa.report(
                    ctxt.file_id,
                    parsed_ty.span,
                    ErrorMessage::TraitNotObjectSafe,
                );
            }

            result_ty
        }
    }
}

fn check_type_record(
    sa: &Sema,
    ctxt: &TypeContext,
    parsed_ty: &ParsedTypeAst,
    type_params: SourceTypeArray,
) -> SourceType {
    let (symbol, parsed_type_params) = match parsed_ty.kind {
        ParsedTypeKind::Regular {
            ref symbol,
            type_arguments: ref type_params,
            ..
        } => (symbol.clone(), type_params),
        _ => unreachable!(),
    };

    if !sym_accessible_from(sa, symbol.clone(), ctxt.module_id) {
        let msg = ErrorMessage::NotAccessible;
        sa.report(ctxt.file_id, parsed_ty.span, msg);
    }

    assert_eq!(type_params.len(), parsed_type_params.len());
    let mut new_type_params = Vec::with_capacity(parsed_type_params.len());

    for idx in 0..type_params.len() {
        let parsed_type_arg = &parsed_type_params[idx];
        assert!(parsed_type_arg.name.is_none());
        let ty = check_type_inner(sa, ctxt, type_params[idx].clone(), &parsed_type_arg.ty);
        new_type_params.push(ty);
    }

    let new_type_params = SourceTypeArray::with(new_type_params);
    let type_param_defs = sym_type_param_definition(sa, symbol.clone());

    if check_type_params(
        sa,
        type_param_defs,
        new_type_params.types(),
        ctxt.file_id,
        parsed_ty.span,
        ctxt.type_param_definition,
    ) {
        ty_for_sym(sa, symbol, new_type_params)
    } else {
        SourceType::Error
    }
}

pub fn check_trait_type(sa: &Sema, ctxt: &TypeContext, parsed_ty: &ParsedType) -> SourceType {
    let parsed_ty_ast = parsed_ty.parsed_ast().expect("missing ast node");
    let new_ty = check_trait_type_inner(sa, ctxt, parsed_ty.ty(), parsed_ty_ast);
    parsed_ty.set_ty(new_ty.clone());
    new_ty
}

fn check_trait_type_inner(
    sa: &Sema,
    ctxt: &TypeContext,
    ty: SourceType,
    parsed_ty: &ParsedTypeAst,
) -> SourceType {
    match ty.clone() {
        SourceType::Any | SourceType::Ptr => {
            unreachable!()
        }
        SourceType::Error => SourceType::Error,
        SourceType::This
        | SourceType::TypeAlias(..)
        | SourceType::Unit
        | SourceType::TypeParam(..)
        | SourceType::Bool
        | SourceType::UInt8
        | SourceType::Char
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Lambda(..)
        | SourceType::Tuple(..)
        | SourceType::Class(..)
        | SourceType::Struct(..)
        | SourceType::Enum(..) => {
            let msg = ErrorMessage::BoundExpected;
            sa.report(ctxt.file_id, parsed_ty.span, msg);
            SourceType::Error
        }
        SourceType::Trait(_, type_params) => check_type_record(sa, ctxt, parsed_ty, type_params),
    }
}

pub fn expand_type(
    sa: &Sema,
    parsed_ty: &ParsedType,
    replace_self: Option<SourceType>,
) -> SourceType {
    let new_ty = expand_st(sa, parsed_ty.ty(), replace_self);
    parsed_ty.set_ty(new_ty.clone());
    new_ty
}

fn expand_st(sa: &Sema, ty: SourceType, replace_self: Option<SourceType>) -> SourceType {
    match ty {
        SourceType::Class(cls_id, type_params) => {
            SourceType::Class(cls_id, expand_sta(sa, type_params, replace_self))
        }

        SourceType::Trait(trait_id, type_params) => {
            SourceType::Trait(trait_id, expand_sta(sa, type_params, replace_self))
        }

        SourceType::Struct(struct_id, type_params) => {
            SourceType::Struct(struct_id, expand_sta(sa, type_params, replace_self))
        }

        SourceType::Enum(enum_id, type_params) => {
            SourceType::Enum(enum_id, expand_sta(sa, type_params, replace_self))
        }

        SourceType::Lambda(params, return_type) => SourceType::Lambda(
            expand_sta(sa, params, replace_self.clone()),
            Box::new(expand_st(sa, *return_type, replace_self)),
        ),

        SourceType::Tuple(subtypes) => SourceType::Tuple(expand_sta(sa, subtypes, replace_self)),

        SourceType::TypeAlias(id) => {
            let alias = sa.alias(id);
            if alias.parent.is_trait() {
                ty
            } else {
                expand_st(sa, alias.ty(), replace_self)
            }
        }

        SourceType::Unit
        | SourceType::UInt8
        | SourceType::Bool
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Error
        | SourceType::TypeParam(..) => ty,
        SourceType::This => replace_self.expect("self expected"),

        SourceType::Any | SourceType::Ptr => {
            panic!("unexpected type = {:?}", ty);
            // unreachable!()
        }
    }
}

fn expand_sta(
    sa: &Sema,
    array: SourceTypeArray,
    replace_self: Option<SourceType>,
) -> SourceTypeArray {
    let new_array = array
        .iter()
        .map(|ty| expand_st(sa, ty, replace_self.clone()))
        .collect::<Vec<_>>();
    SourceTypeArray::with(new_array)
}

#[cfg(test)]
mod tests {
    use crate::tests::*;
    use crate::ErrorMessage;

    #[test]
    fn class_type_with_named_type_arg() {
        err(
            "
            class Foo[T](value: T)
            fn f(x: Foo[T = Int64]) {}
        ",
            (3, 25),
            ErrorMessage::UnexpectedTypeBinding,
        )
    }

    #[test]
    fn trait_type_with_named_before_generic_arg() {
        err(
            "
            trait Foo[T] {}
            fn f(x: Foo[T = Int64, Float32]) {}
        ",
            (3, 36),
            ErrorMessage::WrongOrderOfGenericsAndBindings,
        )
    }
}
