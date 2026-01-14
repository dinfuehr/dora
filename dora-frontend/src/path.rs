use dora_parser::ast::SyntaxNodeBase;
use dora_parser::{Span, ast};

use crate::access::sym_accessible_from;
use crate::args;
use crate::error::diagnostics::{
    EXPECTED_PATH, NOT_ACCESSIBLE_IN_MODULE, SELF_TYPE_UNAVAILABLE, UNEXPECTED_ASSOC,
    UNKNOWN_ASSOC, UNKNOWN_IDENTIFIER, UNKNOWN_IDENTIFIER_IN_MODULE,
};
use crate::sema::{
    AliasDefinitionId, ClassDefinitionId, Element, EnumDefinitionId, Sema, SourceFileId,
    StructDefinitionId, TraitDefinitionId, TypeParamId, parent_element_or_self,
};
use crate::{ModuleSymTable, Name, SymbolKind, TraitType};

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
    regular: ast::AstPathType,
) -> Result<PathKind, ()> {
    let segments = regular.path();
    let first_segment = segments.segments().next().expect("no segment");

    match first_segment {
        ast::TypePathSegment::UpcaseThis(..) => {
            parse_path_self(sa, file_id, element, allow_self, regular)
        }
        ast::TypePathSegment::Name(..) => parse_path_ident(sa, file_id, table, element, regular),
        ast::TypePathSegment::Error(..) => Err(()),
    }
}

fn parse_path_self(
    sa: &Sema,
    file_id: SourceFileId,
    element: &dyn Element,
    allow_self: bool,
    regular: ast::AstPathType,
) -> Result<PathKind, ()> {
    let path = regular.path();
    let mut segments = path.segments();
    let first_segment = segments.next().unwrap();
    assert!(matches!(
        first_segment,
        ast::TypePathSegment::UpcaseThis(..)
    ));

    if !allow_self {
        sa.report(
            file_id,
            regular.path().span(),
            &SELF_TYPE_UNAVAILABLE,
            args!(),
        );
        return Err(());
    }

    let segment_name = if let Some(segment) = segments.next() {
        segment
    } else {
        return Ok(PathKind::Self_);
    };

    let name = expect_ident(sa, file_id, segment_name.clone())?;
    let alias_id = lookup_alias_on_self(sa, file_id, regular.path().span(), element, name)?;

    if let Some(alias_id) = alias_id {
        Ok(PathKind::Symbol(SymbolKind::Alias(alias_id)))
    } else {
        let segment_span = segment_name.span();
        sa.report(file_id, segment_span, &UNKNOWN_ASSOC, args!());
        Err(())
    }
}

fn parse_path_ident(
    sa: &Sema,
    file_id: SourceFileId,
    table: &ModuleSymTable,
    element: &dyn Element,
    regular: ast::AstPathType,
) -> Result<PathKind, ()> {
    let path = regular.path();
    let mut segments = path.segments();
    let first_segment = segments.next().unwrap();
    let first_name = match &first_segment {
        ast::TypePathSegment::Name(token) => sa.interner.intern(token.text()),
        _ => unreachable!(),
    };
    let sym = table.get(first_name);

    if sym.is_none() {
        let name = sa.interner.str(first_name).to_string();
        sa.report(
            file_id,
            first_segment.span(),
            &UNKNOWN_IDENTIFIER,
            args!(name),
        );
        return Err(());
    }

    let mut previous_sym = sym.expect("missing symbol");
    let mut result: Option<PathKind> = None;

    for segment in segments {
        if previous_sym.is_module() {
            let name = expect_ident(sa, file_id, segment.clone())?;

            let module_id = previous_sym.to_module().expect("expected module");
            let module = sa.module(module_id);
            let current_sym = module.table().get(name);

            if let Some(current_sym) = current_sym {
                if sym_accessible_from(sa, current_sym.clone(), module_id) {
                    previous_sym = current_sym;
                } else {
                    let module = sa.module(module_id);
                    let segment_name = match &segment {
                        ast::TypePathSegment::Name(token) => token.text().to_string(),
                        _ => "<missing name>".to_string(),
                    };
                    let module_name = module.name(sa);
                    sa.report(
                        file_id,
                        first_segment.span(),
                        &NOT_ACCESSIBLE_IN_MODULE,
                        args!(module_name, segment_name),
                    );
                    return Err(());
                }
            } else {
                let module = sa.module(module_id);
                let name = sa.interner.str(name).to_string();
                let module_name = module.name(sa);
                sa.report(
                    file_id,
                    segment.span(),
                    &UNKNOWN_IDENTIFIER_IN_MODULE,
                    args!(module_name, name),
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
            let start = first_segment.span().start();
            let end = segment.span().end();
            let span = Span::new(start, end);
            sa.report(file_id, span, &EXPECTED_PATH, args!());
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
        sa.report(file_id, span, &UNEXPECTED_ASSOC, args!());
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

fn expect_ident(
    sa: &Sema,
    file_id: SourceFileId,
    segment: ast::TypePathSegment,
) -> Result<Name, ()> {
    match segment {
        ast::TypePathSegment::UpcaseThis(..) => {
            sa.report(file_id, segment.span(), &EXPECTED_PATH, args!());
            Err(())
        }
        ast::TypePathSegment::Name(segment) => {
            let name = sa.interner.intern(segment.text());
            Ok(name)
        }
        ast::TypePathSegment::Error(..) => Err(()),
    }
}
