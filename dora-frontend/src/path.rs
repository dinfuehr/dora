use dora_parser::{ast, Span};

use crate::access::sym_accessible_from;
use crate::sema::{
    parent_element_or_self, AliasDefinitionId, ClassDefinitionId, Element, EnumDefinitionId, Sema,
    SourceFileId, StructDefinitionId, TraitDefinitionId, TypeParamId,
};
use crate::{ErrorMessage, ModuleSymTable, Name, SymbolKind, TraitType};

#[derive(Clone, Debug)]
pub enum PathKind {
    Self_,
    Class(ClassDefinitionId),
    Enum(EnumDefinitionId),
    Struct(StructDefinitionId),
    Trait(TraitDefinitionId),
    Alias(AliasDefinitionId),
    TypeParam(TypeParamId),
    GenericAssoc {
        tp_id: TypeParamId,
        trait_ty: TraitType,
        assoc_id: AliasDefinitionId,
    },
    Symbol(SymbolKind),
    Assoc {
        name: Name,
    },
}

pub fn parse_path(
    sa: &Sema,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    element: &dyn Element,
    allow_self: bool,
    regular: &ast::TypeRegularType,
) -> Result<PathKind, ()> {
    let segments = &regular.path.segments;
    let first = segments.first().expect("no segment");

    match first.as_ref() {
        ast::PathSegmentData::Self_(..) => {
            parse_path_self(sa, file_id, element, allow_self, regular)
        }

        ast::PathSegmentData::Ident(..) => parse_path_ident(sa, file_id, table, element, regular),

        ast::PathSegmentData::Error { .. } => Err(()),
    }
}

fn parse_path_self(
    sa: &Sema,
    file_id: SourceFileId,
    element: &dyn Element,
    allow_self: bool,
    regular: &ast::TypeRegularType,
) -> Result<PathKind, ()> {
    let segments = &regular.path.segments;
    assert!(segments[0].is_self());

    if !allow_self {
        sa.report(
            file_id,
            regular.path.span,
            ErrorMessage::SelfTypeUnavailable,
        );
        return Err(());
    }

    if segments.len() == 1 {
        return Ok(PathKind::Self_);
    }

    assert_eq!(segments.len(), 2);
    let segment_name = segments.get(1).expect("missing name");

    let name = expect_ident(sa, file_id, segment_name)?;
    let alias_id = lookup_alias_on_self(sa, file_id, regular.path.span, element, name)?;

    if let Some(alias_id) = alias_id {
        Ok(PathKind::Symbol(SymbolKind::Alias(alias_id)))
    } else {
        sa.report(file_id, segment_name.span(), ErrorMessage::UnknownAssoc);
        Err(())
    }
}

fn parse_path_ident(
    sa: &Sema,
    file_id: SourceFileId,
    table: &ModuleSymTable,
    element: &dyn Element,
    regular: &ast::TypeRegularType,
) -> Result<PathKind, ()> {
    let segments = &regular.path.segments;
    let node = segments[0].to_ident().expect("ident expected");

    let ast_ident = sa
        .node(file_id, node.name)
        .to_ident()
        .expect("ident expected");
    let first_name = sa.interner.intern(&ast_ident.name);
    let sym = table.get(first_name);

    if sym.is_none() {
        let msg = ErrorMessage::UnknownIdentifier(ast_ident.name.clone());
        sa.report(file_id, node.span, msg);
        return Err(());
    }

    let mut previous_sym = sym.expect("missing symbol");
    let mut result: Option<PathKind> = None;

    for (idx, segment) in segments.iter().enumerate().skip(1) {
        if previous_sym.is_module() {
            let name = expect_ident(sa, file_id, segment)?;

            let module_id = previous_sym.to_module().expect("expected module");
            let module = sa.module(module_id);
            let current_sym = module.table().get(name);

            if let Some(current_sym) = current_sym {
                if sym_accessible_from(sa, current_sym.clone(), module_id) {
                    previous_sym = current_sym;
                } else {
                    let module = sa.module(module_id);
                    let ast_ident = sa
                        .node(file_id, node.name)
                        .to_ident()
                        .expect("ident expected");
                    let msg = ErrorMessage::NotAccessibleInModule(
                        module.name(sa),
                        ast_ident.name.clone(),
                    );
                    sa.report(file_id, node.span, msg);
                    return Err(());
                }
            } else {
                let module = sa.module(module_id);
                let name = sa.interner.str(name).to_string();
                let module_name = module.name(sa);
                sa.report(
                    file_id,
                    segment.span(),
                    ErrorMessage::UnknownIdentifierInModule(module_name, name),
                );
                return Err(());
            }
        } else if let SymbolKind::TypeParam(id) = previous_sym {
            let name = expect_ident(sa, file_id, segment)?;

            let mut available =
                lookup_alias_on_type_param(sa, element, id, name).unwrap_or(Vec::new());

            if available.len() == 1 {
                let (trait_ty, assoc_id) = available.pop().expect("element expected");
                previous_sym = SymbolKind::Alias(assoc_id);
                result = Some(PathKind::GenericAssoc {
                    tp_id: id,
                    trait_ty,
                    assoc_id,
                });
            } else {
                unimplemented!()
            }
        } else {
            let msg = ErrorMessage::ExpectedPath;
            let start = segments[0].span().start();
            let end = segments[idx - 1].span().end();
            let span = Span::new(start, end);
            sa.report(file_id, span, msg);
            return Err(());
        }
    }

    if let Some(path_kind) = result {
        Ok(path_kind)
    } else {
        Ok(PathKind::Symbol(previous_sym))
    }
}

fn lookup_alias_on_self<'a>(
    sa: &'a Sema,
    file_id: SourceFileId,
    span: Span,
    element: &'a dyn Element,
    name: Name,
) -> Result<Option<AliasDefinitionId>, ()> {
    let element = parent_element_or_self(sa, element);

    if let Some(trait_) = element.to_trait() {
        if let Some(alias_id) = trait_.alias_names().get(&name).cloned() {
            return Ok(Some(alias_id));
        }

        for bound in trait_.type_param_definition.bounds_for_self() {
            let trait_id = bound.trait_id;
            let trait_ = sa.trait_(trait_id);

            if let Some(id) = trait_.alias_names().get(&name) {
                return Ok(Some(*id));
            }
        }

        Ok(None)
    } else if let Some(impl_) = element.to_impl() {
        if let Some(trait_id) = impl_.parsed_trait_ty().trait_id() {
            let trait_ = sa.trait_(trait_id);
            Ok(trait_.alias_names().get(&name).cloned())
        } else {
            Err(())
        }
    } else {
        sa.report(file_id, span, ErrorMessage::UnexpectedAssoc);
        Err(())
    }
}

fn lookup_alias_on_type_param<'a>(
    sa: &'a Sema,
    element: &'a dyn Element,
    id: TypeParamId,
    name: Name,
) -> Option<Vec<(TraitType, AliasDefinitionId)>> {
    let type_param_definition = element.type_param_definition();
    let mut results = Vec::with_capacity(2);

    for bound in type_param_definition.bounds_for_type_param(id) {
        let trait_id = bound.trait_id;
        let trait_ = sa.trait_(trait_id);

        if let Some(id) = trait_.alias_names().get(&name) {
            results.push((bound, *id));
        }
    }

    Some(results)
}

fn expect_ident(sa: &Sema, file_id: SourceFileId, segment: &ast::PathSegment) -> Result<Name, ()> {
    match segment.as_ref() {
        ast::PathSegmentData::Self_(ref node) => {
            sa.report(file_id, node.span, ErrorMessage::ExpectedPath);
            Err(())
        }
        ast::PathSegmentData::Ident(ref node) => {
            let ast_ident = sa
                .node(file_id, node.name)
                .to_ident()
                .expect("param expected");
            let name = sa.interner.intern(&ast_ident.name);
            Ok(name)
        }
        ast::PathSegmentData::Error { .. } => Err(()),
    }
}
