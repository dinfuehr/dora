use std::cell::{OnceCell, RefCell};
use std::collections::{HashMap, HashSet};

use crate::access::{sym_accessible_from, trait_accessible_from};
use crate::sema::{
    implements_trait, is_trait_object_safe, parent_element_or_self, AliasDefinitionId, Element,
    SourceFileId, TraitDefinition, TraitDefinitionId, TypeParamDefinition,
};
use crate::sym::{ModuleSymTable, SymbolKind};
use crate::{
    parse_path, replace_type, specialize_type, ErrorMessage, Name, PathKind, Sema, SourceType,
    SourceTypeArray, Span, TraitType,
};

use dora_parser::ast;

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
pub struct ParsedTraitType {
    ast: Option<ast::Type>,
    parsed_ast: OnceCell<Box<ParsedTypeAst>>,
    ty: RefCell<Option<TraitType>>,
}

impl ParsedTraitType {
    pub fn new_ast(ast: ast::Type) -> ParsedTraitType {
        ParsedTraitType {
            ast: Some(ast),
            parsed_ast: OnceCell::new(),
            ty: RefCell::new(None),
        }
    }

    pub fn ty(&self) -> Option<TraitType> {
        self.ty.borrow().as_ref().cloned()
    }

    pub fn set_ty(&self, ty: Option<TraitType>) {
        *self.ty.borrow_mut() = ty;
    }

    pub fn span(&self) -> Span {
        self.parsed_ast().expect("missing ast node").span
    }

    fn parsed_ast(&self) -> Option<&ParsedTypeAst> {
        self.parsed_ast.get().map(|ast| &**ast)
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
#[allow(unused)]
pub enum ParsedTypeKind {
    This,

    Regular {
        symbol: SymbolKind,
        kind: PathKind,
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
    span: Span,
}

pub fn parse_type(
    sa: &Sema,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    element: &dyn Element,
    allow_self: bool,
    parsed_ty: &ParsedType,
) {
    if let Some(node) = parsed_ty.ast.as_ref() {
        let ast = parse_type_inner(sa, table, file_id, element, allow_self, node);
        assert!(parsed_ty.parsed_ast.set(ast).is_ok());

        let ty = convert_type_inner(sa, file_id, parsed_ty.parsed_ast().unwrap());
        parsed_ty.set_ty(ty);
    }
}

pub fn parse_trait_type(
    sa: &Sema,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    element: &dyn Element,
    allow_self: bool,
    parsed_ty: &ParsedTraitType,
) {
    parse_trait_type_inner(sa, table, file_id, element, allow_self, parsed_ty, false);
}

pub fn parse_trait_bound_type(
    sa: &Sema,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    element: &dyn Element,
    allow_self: bool,
    parsed_ty: &ParsedTraitType,
) {
    parse_trait_type_inner(sa, table, file_id, element, allow_self, parsed_ty, true);
}

fn parse_trait_type_inner(
    sa: &Sema,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    element: &dyn Element,
    allow_self: bool,
    parsed_ty: &ParsedTraitType,
    allow_bindings: bool,
) {
    let node = parsed_ty.ast.as_ref().expect("missing ast node");
    let parsed_ast = parse_type_inner(sa, table, file_id, element, allow_self, node);
    assert!(parsed_ty.parsed_ast.set(parsed_ast).is_ok());

    let parsed_ast = parsed_ty.parsed_ast().expect("missing ast");

    match &parsed_ast.kind {
        ParsedTypeKind::Regular {
            symbol: SymbolKind::Trait(trait_id),
            type_arguments,
            ..
        } => {
            let trait_ty =
                convert_trait_type(sa, file_id, *trait_id, &type_arguments, allow_bindings);
            parsed_ty.set_ty(trait_ty);
        }

        ParsedTypeKind::Error => {}

        _ => {
            let msg = ErrorMessage::BoundExpected;
            sa.report(file_id, parsed_ast.span, msg);
        }
    }
}

fn parse_type_inner(
    sa: &Sema,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    element: &dyn Element,
    allow_self: bool,
    t: &ast::TypeData,
) -> Box<ParsedTypeAst> {
    let kind = match *t {
        ast::TypeData::Regular(ref node) => {
            parse_type_regular(sa, table, file_id, element, allow_self, node)
        }
        ast::TypeData::Tuple(ref node) => {
            parse_type_tuple(sa, table, file_id, element, allow_self, node)
        }
        ast::TypeData::Lambda(ref node) => {
            parse_type_lambda(sa, table, file_id, element, allow_self, node)
        }
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
    element: &dyn Element,
    allow_self: bool,
    node: &ast::TypeRegularType,
) -> ParsedTypeKind {
    let path_kind = parse_path(sa, table, file_id, element, allow_self, node);

    if path_kind.is_err() {
        return ParsedTypeKind::Error;
    }

    let path_kind = path_kind.unwrap();

    match path_kind {
        PathKind::Symbol(sym) => match sym {
            SymbolKind::Trait(..)
            | SymbolKind::Class(..)
            | SymbolKind::Struct(..)
            | SymbolKind::Enum(..)
            | SymbolKind::Alias(..) => parse_type_regular_with_arguments(
                sa, table, file_id, element, allow_self, sym, node,
            ),

            SymbolKind::TypeParam(id) => {
                if !node.params.is_empty() {
                    let msg = ErrorMessage::NoTypeParamsExpected;
                    sa.report(file_id, node.span, msg);
                }

                ParsedTypeKind::Regular {
                    symbol: SymbolKind::TypeParam(id),
                    kind: PathKind::TypeParam(id),
                    type_arguments: Vec::new(),
                }
            }

            _ => {
                let msg = ErrorMessage::ExpectedTypeName;
                sa.report(file_id, node.span, msg);
                ParsedTypeKind::Error
            }
        },

        PathKind::GenericAssoc {
            trait_ty,
            assoc_id,
            tp_id,
        } => ParsedTypeKind::Regular {
            symbol: SymbolKind::Trait(trait_ty.trait_id), // Placeholder
            kind: PathKind::GenericAssoc {
                tp_id,
                trait_ty,
                assoc_id,
            },
            type_arguments: Vec::new(),
        },

        PathKind::Class(..)
        | PathKind::Enum(..)
        | PathKind::Struct(..)
        | PathKind::Alias(..)
        | PathKind::Trait(..)
        | PathKind::TypeParam(..) => unreachable!(),

        PathKind::Self_ => ParsedTypeKind::This,
    }
}

fn parse_type_regular_with_arguments(
    sa: &Sema,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    element: &dyn Element,
    allow_self: bool,
    symbol: SymbolKind,
    node: &ast::TypeRegularType,
) -> ParsedTypeKind {
    let mut type_arguments = Vec::new();

    for param in &node.params {
        let name = if let Some(ref name) = param.name {
            Some(sa.interner.intern(&name.name_as_string))
        } else {
            None
        };

        let ty = parse_type_inner(sa, table, file_id, element, allow_self, &param.ty);
        let ty_arg = ParsedTypeArgument {
            name,
            ty,
            span: param.span,
        };
        type_arguments.push(ty_arg);
    }

    let path_kind = match symbol {
        SymbolKind::Alias(id) => PathKind::Alias(id),
        SymbolKind::Trait(id) => PathKind::Trait(id),
        SymbolKind::Class(id) => PathKind::Class(id),
        SymbolKind::Struct(id) => PathKind::Struct(id),
        SymbolKind::Enum(id) => PathKind::Enum(id),
        _ => unreachable!(),
    };

    ParsedTypeKind::Regular {
        symbol: symbol,
        kind: path_kind,
        type_arguments,
    }
}

fn parse_type_lambda(
    sa: &Sema,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    element: &dyn Element,
    allow_self: bool,
    node: &ast::TypeLambdaType,
) -> ParsedTypeKind {
    let mut params = vec![];

    for param in &node.params {
        let ty = parse_type_inner(sa, table, file_id, element, allow_self, param);
        params.push(ty);
    }

    let return_ty = if let Some(ref ret) = node.ret {
        Some(parse_type_inner(
            sa, table, file_id, element, allow_self, ret,
        ))
    } else {
        None
    };

    ParsedTypeKind::Lambda { params, return_ty }
}

fn parse_type_tuple(
    sa: &Sema,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    element: &dyn Element,
    allow_self: bool,
    node: &ast::TypeTupleType,
) -> ParsedTypeKind {
    let mut subtypes = Vec::new();

    for subtype in &node.subtypes {
        let ty = parse_type_inner(sa, table, file_id, element, allow_self, subtype);
        subtypes.push(ty);
    }

    ParsedTypeKind::Tuple { subtypes }
}

fn convert_type_inner(sa: &Sema, file_id: SourceFileId, parsed_ty: &ParsedTypeAst) -> SourceType {
    match parsed_ty.kind {
        ParsedTypeKind::This => SourceType::This,
        ParsedTypeKind::Regular { .. } => convert_type_regular(sa, file_id, parsed_ty),
        ParsedTypeKind::Tuple { .. } => convert_type_tuple(sa, file_id, parsed_ty),
        ParsedTypeKind::Lambda { .. } => convert_type_lambda(sa, file_id, parsed_ty),
        ParsedTypeKind::Error { .. } => SourceType::Error,
    }
}

fn convert_type_regular(sa: &Sema, file_id: SourceFileId, parsed_ty: &ParsedTypeAst) -> SourceType {
    let (sym, path_kind, type_params) = match parsed_ty.kind {
        ParsedTypeKind::Regular {
            ref symbol,
            ref kind,
            type_arguments: ref type_params,
            ..
        } => (symbol.clone(), kind.clone(), type_params),
        _ => {
            unreachable!()
        }
    };

    match path_kind {
        PathKind::TypeParam(id) => {
            assert!(type_params.is_empty());
            SourceType::TypeParam(id)
        }

        PathKind::Trait(trait_id) => {
            convert_type_regular_trait_object(sa, file_id, parsed_ty, trait_id, type_params)
        }

        PathKind::Alias(..) | PathKind::Class(..) | PathKind::Enum(..) | PathKind::Struct(..) => {
            let mut source_type_arguments = Vec::with_capacity(type_params.len());

            for ty_arg in type_params {
                if ty_arg.name.is_some() {
                    sa.report(file_id, ty_arg.span, ErrorMessage::UnexpectedTypeBinding);
                    return SourceType::Error;
                }

                let ty = convert_type_inner(sa, file_id, &ty_arg.ty);
                source_type_arguments.push(ty);
            }

            let sym_element = get_path_kind_element(sa, path_kind);
            let type_param_definition = sym_element.type_param_definition();

            if type_param_definition.type_param_count() == source_type_arguments.len() {
                let type_params = SourceTypeArray::with(source_type_arguments);
                ty_for_sym(sa, sym, type_params)
            } else {
                let msg = ErrorMessage::WrongNumberTypeParams(
                    type_param_definition.type_param_count(),
                    source_type_arguments.len(),
                );
                sa.report(file_id, parsed_ty.span, msg);
                SourceType::Error
            }
        }

        PathKind::GenericAssoc {
            tp_id,
            trait_ty,
            assoc_id,
        } => SourceType::GenericAssoc {
            tp_id,
            trait_ty,
            assoc_id,
        },

        _ => unreachable!(),
    }
}

fn convert_type_regular_trait_object(
    sa: &Sema,
    file_id: SourceFileId,
    parsed_ty: &ParsedTypeAst,
    trait_id: TraitDefinitionId,
    type_params: &Vec<ParsedTypeArgument>,
) -> SourceType {
    let trait_ = sa.trait_(trait_id);
    let mut idx = 0;
    let mut trait_type_params = Vec::new();

    while idx < type_params.len() {
        let type_param = &type_params[idx];

        if type_param.name.is_some() {
            break;
        }

        let ty = convert_type_inner(sa, file_id, &type_param.ty);
        trait_type_params.push(ty);
        idx += 1;
    }

    let mut used_aliases = HashMap::new();

    while idx < type_params.len() {
        let type_param = &type_params[idx];

        if type_param.name.is_none() {
            sa.report(file_id, type_param.span, ErrorMessage::TypeBindingOrder);
            return SourceType::Error;
        }

        let name = type_param.name.expect("name expected");

        if let Some(&alias_id) = trait_.alias_names().get(&name) {
            if !used_aliases.contains_key(&alias_id) {
                let ty = convert_type_inner(sa, file_id, &type_param.ty);
                used_aliases.insert(alias_id, ty);
            } else {
                let msg = ErrorMessage::DuplicateTypeBinding;
                sa.report(file_id, type_param.span, msg);
                return SourceType::Error;
            }
        } else {
            let msg = ErrorMessage::UnknownTypeBinding;
            sa.report(file_id, type_param.span, msg);
            return SourceType::Error;
        }

        idx += 1;
    }

    let mut bindings = Vec::new();

    for alias_id in trait_.aliases() {
        if let Some(ty) = used_aliases.remove(&alias_id) {
            bindings.push(ty);
        } else {
            let name = sa.alias(*alias_id).name;
            let name = sa.interner.str(name).to_string();
            let msg = ErrorMessage::MissingTypeBinding(name);
            sa.report(file_id, parsed_ty.span, msg);
            return SourceType::Error;
        }
    }

    SourceType::TraitObject(trait_id, trait_type_params.into(), bindings.into())
}

fn get_path_kind_element(sa: &Sema, sym: PathKind) -> &dyn Element {
    match sym {
        PathKind::Class(id) => sa.class(id),
        PathKind::Struct(id) => sa.struct_(id),
        PathKind::Enum(id) => sa.enum_(id),
        PathKind::Alias(id) => sa.alias(id),
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
        SymbolKind::Alias(id) => {
            let alias = sa.alias(id);

            if alias.parent.is_none() {
                SourceType::Alias(id, type_params)
            } else {
                SourceType::Assoc(id, type_params)
            }
        }
        _ => unimplemented!(),
    }
}

fn convert_type_tuple(sa: &Sema, file_id: SourceFileId, parsed_ty: &ParsedTypeAst) -> SourceType {
    let subtypes = match parsed_ty.kind {
        ParsedTypeKind::Tuple { ref subtypes } => subtypes,
        _ => unreachable!(),
    };

    let subtypes = subtypes
        .iter()
        .map(|t| convert_type_inner(sa, file_id, t))
        .collect::<Vec<_>>();
    let subtypes = SourceTypeArray::with(subtypes);

    if subtypes.is_empty() {
        SourceType::Unit
    } else {
        SourceType::Tuple(subtypes)
    }
}

fn convert_type_lambda(sa: &Sema, file_id: SourceFileId, parsed_ty: &ParsedTypeAst) -> SourceType {
    let (params, return_ty) = match parsed_ty.kind {
        ParsedTypeKind::Lambda {
            ref params,
            ref return_ty,
        } => (params, return_ty),
        _ => unreachable!(),
    };

    let params = params
        .iter()
        .map(|t| convert_type_inner(sa, file_id, t))
        .collect::<Vec<_>>();
    let params = SourceTypeArray::with(params);

    let return_ty = if let Some(return_ty) = return_ty {
        convert_type_inner(sa, file_id, return_ty)
    } else {
        SourceType::Unit
    };

    SourceType::Lambda(params, Box::new(return_ty))
}

fn convert_trait_type(
    sa: &Sema,
    file_id: SourceFileId,
    trait_id: TraitDefinitionId,
    type_params: &Vec<ParsedTypeArgument>,
    allow_bindings: bool,
) -> Option<TraitType> {
    let trait_ = sa.trait_(trait_id);
    let mut idx = 0;
    let mut generics = Vec::new();
    let mut bindings: Vec<(AliasDefinitionId, SourceType)> = Vec::new();

    while idx < type_params.len() {
        let type_param = &type_params[idx];

        if type_param.name.is_some() {
            break;
        }

        let ty = convert_type_inner(sa, file_id, &type_param.ty);
        generics.push(ty);
        idx += 1;
    }

    let mut used_aliases = HashSet::new();

    while idx < type_params.len() {
        let type_param = &type_params[idx];

        if type_param.name.is_none() {
            sa.report(file_id, type_param.span, ErrorMessage::TypeBindingOrder);
            return None;
        } else if !allow_bindings {
            sa.report(
                file_id,
                type_param.span,
                ErrorMessage::UnexpectedTypeBinding,
            );
            return None;
        }

        assert!(allow_bindings);
        let name = type_param.name.expect("name expected");

        if let Some(&alias_id) = trait_.alias_names().get(&name) {
            if used_aliases.insert(alias_id) {
                let ty = convert_type_inner(sa, file_id, &type_param.ty);
                bindings.push((alias_id, ty));
            } else {
                let msg = ErrorMessage::DuplicateTypeBinding;
                sa.report(file_id, type_param.span, msg);
                return None;
            }
        } else {
            let msg = ErrorMessage::UnknownTypeBinding;
            sa.report(file_id, type_param.span, msg);
            return None;
        }

        idx += 1;
    }

    let type_params = SourceTypeArray::with(generics);
    Some(TraitType {
        trait_id,
        type_params,
        bindings,
    })
}

pub fn check_type(sa: &Sema, ctxt_element: &dyn Element, parsed_ty: &ParsedType) -> SourceType {
    if let Some(parsed_ty_ast) = parsed_ty.parsed_ast() {
        let new_ty = check_type_inner(sa, ctxt_element, parsed_ty.ty(), parsed_ty_ast);
        parsed_ty.set_ty(new_ty.clone());
        new_ty
    } else {
        parsed_ty.ty()
    }
}

fn check_type_inner(
    sa: &Sema,
    ctxt_element: &dyn Element,
    ty: SourceType,
    parsed_ty: &ParsedTypeAst,
) -> SourceType {
    match ty.clone() {
        SourceType::Any | SourceType::Ptr => {
            unreachable!()
        }
        SourceType::This => SourceType::This,
        SourceType::Assoc(..)
        | SourceType::Error
        | SourceType::Unit
        | SourceType::TypeParam(..)
        | SourceType::GenericAssoc { .. } => ty,
        SourceType::Bool
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

            if !sym_accessible_from(sa, symbol, ctxt_element.module_id()) {
                let msg = ErrorMessage::NotAccessible;
                sa.report(ctxt_element.file_id(), parsed_ty.span, msg);
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
                let ty = check_type_inner(sa, ctxt_element, params[idx].clone(), parsed_param);
                new_params.push(ty);
            }

            let new_params = SourceTypeArray::with(new_params);
            let new_return_type: SourceType = if let Some(parsed_return_type) = parsed_return_type {
                check_type_inner(sa, ctxt_element, *return_type, parsed_return_type)
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
                let ty = check_type_inner(sa, ctxt_element, subtypes[idx].clone(), parsed_subtype);
                new_type_params.push(ty);
            }

            SourceType::Tuple(SourceTypeArray::with(new_type_params))
        }
        SourceType::Class(_, type_params)
        | SourceType::Struct(_, type_params)
        | SourceType::Enum(_, type_params)
        | SourceType::Alias(_, type_params) => {
            check_type_record(sa, ctxt_element, parsed_ty, type_params)
        }
        SourceType::TraitObject(trait_id, type_params, bindings) => {
            check_type_trait_object(sa, ctxt_element, parsed_ty, trait_id, type_params, bindings)
        }
    }
}

fn check_type_record(
    sa: &Sema,
    ctxt_element: &dyn Element,
    parsed_ty: &ParsedTypeAst,
    type_params: SourceTypeArray,
) -> SourceType {
    let (symbol, path_kind, parsed_type_params) = match parsed_ty.kind {
        ParsedTypeKind::Regular {
            ref symbol,
            ref kind,
            type_arguments: ref type_params,
            ..
        } => (symbol.clone(), kind.clone(), type_params),
        _ => unreachable!(),
    };

    if !sym_accessible_from(sa, symbol.clone(), ctxt_element.module_id()) {
        let msg = ErrorMessage::NotAccessible;
        sa.report(ctxt_element.file_id(), parsed_ty.span, msg);
    }

    assert_eq!(type_params.len(), parsed_type_params.len());
    let mut new_type_params = Vec::with_capacity(parsed_type_params.len());

    for idx in 0..type_params.len() {
        let parsed_type_arg = &parsed_type_params[idx];
        assert!(parsed_type_arg.name.is_none());
        let ty = check_type_inner(
            sa,
            ctxt_element,
            type_params[idx].clone(),
            &parsed_type_arg.ty,
        );
        new_type_params.push(ty);
    }

    let new_type_params = SourceTypeArray::with(new_type_params);
    let callee_element = get_path_kind_element(sa, path_kind);
    let callee_type_param_definition = callee_element.type_param_definition();

    if check_type_params(
        sa,
        callee_element,
        callee_type_param_definition,
        new_type_params.types(),
        ctxt_element,
        parsed_ty.span,
    ) {
        ty_for_sym(sa, symbol, new_type_params)
    } else {
        SourceType::Error
    }
}

fn check_type_trait_object(
    sa: &Sema,
    ctxt_element: &dyn Element,
    parsed_ty: &ParsedTypeAst,
    trait_id: TraitDefinitionId,
    type_params: SourceTypeArray,
    bindings: SourceTypeArray,
) -> SourceType {
    let trait_ = sa.trait_(trait_id);

    let parsed_type_arguments = match parsed_ty.kind {
        ParsedTypeKind::Regular {
            type_arguments: ref parsed_type_arguments,
            ..
        } => parsed_type_arguments,
        _ => unreachable!(),
    };

    if !trait_accessible_from(sa, trait_id, ctxt_element.module_id()) {
        let msg = ErrorMessage::NotAccessible;
        sa.report(ctxt_element.file_id(), parsed_ty.span, msg);
    }

    if !is_trait_object_safe(sa, trait_id) {
        sa.report(
            ctxt_element.file_id(),
            parsed_ty.span,
            ErrorMessage::TraitNotObjectSafe,
        );
        return SourceType::Error;
    }

    assert_eq!(
        type_params.len() + bindings.len(),
        parsed_type_arguments.len()
    );
    let mut new_type_params = Vec::with_capacity(type_params.len());

    for (idx, arg) in type_params.iter().enumerate() {
        let parsed_type_arg = &parsed_type_arguments[idx];
        assert!(parsed_type_arg.name.is_none());
        let ty = check_type_inner(sa, ctxt_element, arg.clone(), &parsed_type_arg.ty);
        new_type_params.push(ty);
    }

    let mut new_bindings = Vec::with_capacity(bindings.len());
    let type_param_count = type_params.len();

    for (idx, arg) in bindings.iter().enumerate() {
        let parsed_type_arg = &parsed_type_arguments[type_param_count + idx];
        assert!(parsed_type_arg.name.is_some());
        let alias_id = trait_.aliases()[0];
        let ty = check_type_inner(sa, ctxt_element, arg.clone(), &parsed_type_arg.ty);
        new_bindings.push((alias_id, ty));
    }

    let result = if check_trait_type_param_definition(
        sa,
        ctxt_element,
        trait_,
        &new_type_params,
        &new_bindings,
        ctxt_element.file_id(),
        parsed_ty.span,
        ctxt_element.type_param_definition(),
    ) {
        let new_bindings: Vec<SourceType> = new_bindings.into_iter().map(|b| b.1).collect();
        SourceType::TraitObject(trait_id, new_type_params.into(), new_bindings.into())
    } else {
        SourceType::Error
    };

    result
}

pub fn check_trait_type(sa: &Sema, element: &dyn Element, parsed_ty: &ParsedTraitType) {
    let parsed_ty_ast = parsed_ty.parsed_ast().expect("missing ast node");

    if let Some(trait_ty) = parsed_ty.ty() {
        let new_ty = check_trait_type_inner(sa, element, trait_ty, parsed_ty_ast);
        parsed_ty.set_ty(new_ty);
    }
}

fn check_trait_type_inner(
    sa: &Sema,
    ctxt_element: &dyn Element,
    trait_ty: TraitType,
    parsed_ty: &ParsedTypeAst,
) -> Option<TraitType> {
    let trait_ = sa.trait_(trait_ty.trait_id);

    let parsed_type_params = match parsed_ty.kind {
        ParsedTypeKind::Regular {
            type_arguments: ref type_params,
            ..
        } => type_params,
        _ => unreachable!(),
    };

    if !trait_accessible_from(sa, trait_ty.trait_id, ctxt_element.module_id()) {
        let msg = ErrorMessage::NotAccessible;
        sa.report(ctxt_element.file_id(), parsed_ty.span, msg);
    }

    assert_eq!(
        trait_ty.type_params.len() + trait_ty.bindings.len(),
        parsed_type_params.len()
    );
    let mut new_type_params = Vec::with_capacity(parsed_type_params.len());

    for (idx, arg) in trait_ty.type_params.iter().enumerate() {
        let parsed_type_arg = &parsed_type_params[idx];
        assert!(parsed_type_arg.name.is_none());
        let ty = check_type_inner(sa, ctxt_element, arg, &parsed_type_arg.ty);
        new_type_params.push(ty);
    }

    let mut new_bindings: Vec<(AliasDefinitionId, SourceType)> =
        Vec::with_capacity(trait_ty.bindings.len());

    for (idx, (alias_id, ty)) in trait_ty.bindings.iter().enumerate() {
        let parsed_type_arg = &parsed_type_params[trait_ty.type_params.len() + idx];
        assert!(parsed_type_arg.name.is_some());
        let ty = check_type_inner(sa, ctxt_element, ty.clone(), &parsed_type_arg.ty);
        new_bindings.push((*alias_id, ty));
    }

    if check_trait_type_param_definition(
        sa,
        ctxt_element,
        trait_,
        &new_type_params,
        &new_bindings,
        ctxt_element.file_id(),
        parsed_ty.span,
        ctxt_element.type_param_definition(),
    ) {
        Some(TraitType {
            trait_id: trait_ty.trait_id,
            type_params: new_type_params.into(),
            bindings: new_bindings,
        })
    } else {
        None
    }
}

fn check_type_params(
    sa: &Sema,
    _callee_element: &dyn Element,
    callee_type_param_definition: &TypeParamDefinition,
    type_arguments: &[SourceType],
    ctxt_element: &dyn Element,
    span: Span,
) -> bool {
    assert_eq!(
        callee_type_param_definition.type_param_count(),
        type_arguments.len()
    );

    let type_arguments = SourceTypeArray::with(type_arguments.to_vec());
    let ctxt_type_param_definition = ctxt_element.type_param_definition();

    let mut success = true;

    for bound in callee_type_param_definition.bounds() {
        let tp_ty = bound.ty();

        if let Some(trait_ty) = bound.trait_ty() {
            let tp_ty = specialize_type(sa, tp_ty, &type_arguments);

            if !implements_trait(sa, tp_ty.clone(), ctxt_element, trait_ty.clone()) {
                let name = tp_ty.name_with_type_params(sa, ctxt_type_param_definition);
                let trait_name = trait_ty.name_with_type_params(sa, ctxt_type_param_definition);
                let msg = ErrorMessage::TypeNotImplementingTrait(name, trait_name);
                sa.report(ctxt_element.file_id(), span, msg);
                success = false;
            }
        }
    }

    success
}

fn check_trait_type_param_definition(
    sa: &Sema,
    element: &dyn Element,
    trait_: &TraitDefinition,
    generic_arguments: &[SourceType],
    type_bindings: &[(AliasDefinitionId, SourceType)],
    file_id: SourceFileId,
    span: Span,
    context_type_param_definition: &TypeParamDefinition,
) -> bool {
    let type_param_definition = trait_.type_param_definition();
    let type_arguments = SourceTypeArray::with(generic_arguments.to_vec());

    let mut success = true;

    for bound in type_param_definition.bounds() {
        let tp_ty = bound.ty();

        if tp_ty.is_self() {
            continue;
        }

        if let Some(trait_ty) = bound.trait_ty() {
            let tp_ty = specialize_type(sa, tp_ty, &type_arguments);

            if !implements_trait(sa, tp_ty.clone(), element, trait_ty.clone()) {
                let name = tp_ty.name_with_type_params(sa, context_type_param_definition);
                let trait_name = trait_ty.name_with_type_params(sa, context_type_param_definition);
                let msg = ErrorMessage::TypeNotImplementingTrait(name, trait_name);
                sa.report(file_id, span, msg);
                success = false;
            }
        }
    }

    for (alias_id, ty) in type_bindings {
        let alias = sa.alias(*alias_id);

        for bound in alias.bounds() {
            if let Some(trait_ty) = bound.ty() {
                if !implements_trait(sa, ty.clone(), element, trait_ty.clone()) {
                    let name = ty.name_with_type_params(sa, context_type_param_definition);
                    let trait_name =
                        trait_ty.name_with_type_params(sa, context_type_param_definition);
                    let msg = ErrorMessage::TypeNotImplementingTrait(name, trait_name);
                    sa.report(file_id, span, msg);
                    success = false;
                }
            }
        }
    }

    success
}

pub fn expand_type(
    sa: &Sema,
    element: &dyn Element,
    parsed_ty: &ParsedType,
    replace_self: Option<SourceType>,
) -> SourceType {
    let new_ty = expand_st(sa, element, parsed_ty.ty(), replace_self);
    parsed_ty.set_ty(new_ty.clone());
    new_ty
}

pub fn expand_trait_type(
    sa: &Sema,
    element: &dyn Element,
    parsed_ty: &ParsedTraitType,
    replace_self: Option<SourceType>,
) {
    if let Some(trait_ty) = parsed_ty.ty() {
        let new_type_params = expand_sta(sa, element, &trait_ty.type_params, replace_self.clone());
        let new_bindings = trait_ty
            .bindings
            .into_iter()
            .map(|(id, ty)| (id, expand_st(sa, element, ty, replace_self.clone())))
            .collect();
        let new_trait_ty = TraitType {
            trait_id: trait_ty.trait_id,
            type_params: new_type_params,
            bindings: new_bindings,
        };
        parsed_ty.set_ty(Some(new_trait_ty));
    }
}

fn expand_st(
    sa: &Sema,
    element: &dyn Element,
    ty: SourceType,
    replace_self: Option<SourceType>,
) -> SourceType {
    match &ty {
        SourceType::Class(cls_id, type_params) => {
            SourceType::Class(*cls_id, expand_sta(sa, element, type_params, replace_self))
        }

        SourceType::TraitObject(trait_id, type_params, bindings) => SourceType::TraitObject(
            *trait_id,
            expand_sta(sa, element, type_params, replace_self.clone()),
            expand_sta(sa, element, bindings, replace_self),
        ),

        SourceType::Struct(struct_id, type_params) => SourceType::Struct(
            *struct_id,
            expand_sta(sa, element, type_params, replace_self),
        ),

        SourceType::Enum(enum_id, type_params) => {
            SourceType::Enum(*enum_id, expand_sta(sa, element, type_params, replace_self))
        }

        SourceType::Lambda(params, return_type) => SourceType::Lambda(
            expand_sta(sa, element, params, replace_self.clone()),
            Box::new(expand_st(
                sa,
                element,
                return_type.as_ref().clone(),
                replace_self,
            )),
        ),

        SourceType::Tuple(subtypes) => {
            SourceType::Tuple(expand_sta(sa, element, subtypes, replace_self))
        }

        SourceType::Alias(id, type_params) => {
            let alias = sa.alias(*id);
            assert!(alias.parent.is_none());

            let alias_ty = replace_type(sa, alias.ty(), Some(type_params), None);
            expand_st(sa, element, alias_ty, replace_self)
        }

        SourceType::Assoc(id, type_params) => {
            let alias = sa.alias(*id);
            let trait_id = alias.parent.to_trait_id().expect("trait expected");
            assert!(type_params.is_empty());

            let element = parent_element_or_self(sa, element);
            if let Some(impl_) = element.to_impl() {
                let impl_alias_id = impl_.trait_alias_map().get(&id).cloned();
                if let Some(impl_alias_id) = impl_alias_id {
                    let impl_alias = sa.alias(impl_alias_id);
                    expand_st(sa, element, impl_alias.ty(), replace_self)
                } else {
                    SourceType::Error
                }
            } else if let Some(trait_) = element.to_trait() {
                assert_eq!(trait_id, trait_.id());
                ty
            } else {
                unreachable!()
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
        | SourceType::TypeParam(..)
        | SourceType::GenericAssoc { .. } => ty,
        SourceType::This => replace_self.expect("self expected"),

        SourceType::Any | SourceType::Ptr => {
            panic!("unexpected type = {:?}", ty);
            // unreachable!()
        }
    }
}

fn expand_sta(
    sa: &Sema,
    element: &dyn Element,
    array: &SourceTypeArray,
    replace_self: Option<SourceType>,
) -> SourceTypeArray {
    let new_array = array
        .iter()
        .map(|ty| expand_st(sa, element, ty, replace_self.clone()))
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
            class Foo[T](T)
            fn f(x: Foo[T = Int64]) {}
        ",
            (3, 25),
            ErrorMessage::UnexpectedTypeBinding,
        );
    }

    #[test]
    fn trait_object_type_with_named_before_generic_arg() {
        err(
            "
            trait Foo[T] { type X; }
            fn f(x: Foo[X = Int64, Float32]) {}
        ",
            (3, 36),
            ErrorMessage::TypeBindingOrder,
        );
    }

    #[test]
    fn trait_object_type_with_unknown_named_arg() {
        err(
            "
            trait Foo[T] {}
            fn f(x: Foo[Float32, T = Int64]) {}
        ",
            (3, 34),
            ErrorMessage::UnknownTypeBinding,
        );
    }

    #[test]
    fn trait_object_type_with_same_binding() {
        err(
            "
            trait Foo[T] {
                type T;
            }
            fn f(x: Foo[Float32, T = Int64, T = Int64]) {}
        ",
            (5, 45),
            ErrorMessage::DuplicateTypeBinding,
        );
    }

    #[test]
    fn trait_impl_type_with_binding() {
        err(
            "
            trait Foo[T] {
                type X;
            }
            impl Foo[X = Int64] for Int64 {}
        ",
            (5, 22),
            ErrorMessage::UnexpectedTypeBinding,
        );
    }

    #[test]
    fn trait_bound_type_with_binding() {
        ok("
            trait Foo[T] {
                type X;
            }
            struct Bar[T](T)
            fn f[T](x: Bar[T]) where T: Foo[String, X=Int64] {}
        ");
    }

    #[test]
    fn trait_bound_type_missing_binding() {
        ok("
            trait Foo[T] {
                type X;
            }
            struct Bar[T](T)
            fn f[T](x: Bar[T]) where T: Foo[String] {}
        ");
    }

    #[test]
    fn trait_bound_type_duplicate_binding() {
        err(
            "
            trait Foo[T] {
                type X;
            }
            struct Bar[T](T)
            fn f[T](x: Bar[T]) where T: Foo[String, X=Int64, X=Int64] {}
        ",
            (6, 62),
            ErrorMessage::DuplicateTypeBinding,
        );
    }

    #[test]
    fn trait_bound_type_binding_before_generic() {
        err(
            "
            trait Foo[T] {
                type X;
            }
            struct Bar[T](T)
            fn f[T](x: Bar[T]) where T: Foo[X=Int64, Int64] {}
        ",
            (6, 54),
            ErrorMessage::TypeBindingOrder,
        );
    }

    #[test]
    fn trait_alias_through_self_in_clause() {
        ok("
            trait Bar {}
            trait Foo where Self::X: Bar {
                type X;
            }
        ");
    }

    #[test]
    fn trait_alias_through_self_in_clause_unknown() {
        err(
            "
            trait Bar {}
            trait Foo where Self::Y: Bar {
                type X;
            }
        ",
            (3, 35),
            ErrorMessage::UnknownAlias,
        );
    }

    #[test]
    fn extension_alias_through_self_unknown() {
        err(
            "
            struct Foo
            trait Bar {}
            impl Foo where Self::X: Bar {}
        ",
            (4, 34),
            ErrorMessage::UnexpectedAlias,
        );
    }

    #[test]
    fn impl_alias_through_self_method() {
        ok("
            trait Foo {
                type X;
                fn get(): Self::X;
            }
            impl Foo for String {
                type X = String;
                fn get(): Self::X { self }
            }
        ");
    }

    #[test]
    fn impl_alias_through_self_method_unknown() {
        err(
            "
            trait Foo {}
            trait Bar {}
            impl Foo for String where Self::X: Bar {}
        ",
            (4, 45),
            ErrorMessage::UnexpectedAlias,
        );
    }

    #[test]
    fn trait_alias_through_self_in_method() {
        ok("
            trait Foo {
                type X;
                fn get(): Self::X;
            }
        ");
    }

    #[test]
    fn trait_alias_through_self_in_method_unknown() {
        err(
            "
            trait Foo {
                type X;
                fn get(): Self::Y;
            }
        ",
            (4, 33),
            ErrorMessage::UnknownAlias,
        );
    }
}
