use std::collections::HashMap;

use dora_parser::ast;

use crate::access::sym_accessible_from;
use crate::sema::{AliasDefinitionId, Element, FctParent, Sema, SourceFileId};
use crate::{ErrorMessage, ModuleSymTable, Name, SymbolKind};

pub enum PathKind {
    Self_,
    Symbol(SymbolKind),
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

        ast::PathSegmentData::Ident(ref node) => {
            let first_name = sa.interner.intern(&node.name.name_as_string);
            let sym = table.get(first_name);

            if sym.is_none() {
                let msg = ErrorMessage::UnknownIdentifier(node.name.name_as_string.clone());
                sa.report(file_id, node.span, msg);
                return Err(());
            }

            let mut previous_sym = sym.expect("missing symbol");

            for (idx, segment) in segments.iter().enumerate().skip(1) {
                if !previous_sym.is_module() {
                    let msg = ErrorMessage::ExpectedPath;
                    sa.report(file_id, segments[idx - 1].span(), msg);
                    return Err(());
                }

                let name = expect_ident(sa, file_id, segment)?;

                let module_id = previous_sym.to_module().expect("expected module");
                let module = sa.module(module_id);
                let current_sym = module.table().get(name);

                if let Some(current_sym) = current_sym {
                    if sym_accessible_from(sa, current_sym.clone(), module_id) {
                        previous_sym = current_sym;
                    } else {
                        let module = sa.module(module_id);
                        let name = node.name.name_as_string.clone();
                        let msg = ErrorMessage::NotAccessibleInModule(module.name(sa), name);
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
            }

            Ok(PathKind::Symbol(previous_sym))
        }

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

    if allow_self {
        if let Some(second) = segments.get(1) {
            let name = expect_ident(sa, file_id, second)?;

            let aliases = available_aliases(sa, element);
            if let Some(aliases) = aliases {
                let alias_id = aliases.get(&name).cloned();

                if let Some(alias_id) = alias_id {
                    Ok(PathKind::Symbol(SymbolKind::TypeAlias(alias_id)))
                } else {
                    sa.report(file_id, second.span(), ErrorMessage::UnknownAlias);
                    Err(())
                }
            } else {
                sa.report(file_id, second.span(), ErrorMessage::UnexpectedAlias);
                Err(())
            }
        } else {
            Ok(PathKind::Self_)
        }
    } else {
        sa.report(
            file_id,
            regular.path.span,
            ErrorMessage::SelfTypeUnavailable,
        );
        Err(())
    }
}

fn available_aliases<'a>(
    sa: &'a Sema,
    element: &'a dyn Element,
) -> Option<&'a HashMap<Name, AliasDefinitionId>> {
    if let Some(trait_) = element.to_trait() {
        Some(trait_.alias_names())
    } else if let Some(fct) = element.to_fct() {
        match fct.parent {
            FctParent::Trait(trait_id) => Some(sa.trait_(trait_id).alias_names()),
            FctParent::Impl(impl_id) => {
                let impl_ = sa.impl_(impl_id);
                if let Some(trait_ty) = impl_.trait_ty() {
                    let trait_ = sa.trait_(trait_ty.trait_id);
                    Some(trait_.alias_names())
                } else {
                    None
                }
            }
            FctParent::Function => None,
            FctParent::Extension(..) | FctParent::None => None,
        }
    } else {
        None
    }
}

fn expect_ident(sa: &Sema, file_id: SourceFileId, segment: &ast::PathSegment) -> Result<Name, ()> {
    match segment.as_ref() {
        ast::PathSegmentData::Self_(ref node) => {
            sa.report(file_id, node.span, ErrorMessage::ExpectedPath);
            Err(())
        }
        ast::PathSegmentData::Ident(ref node) => {
            let name = sa.interner.intern(&node.name.name_as_string);
            Ok(name)
        }
        ast::PathSegmentData::Error { .. } => Err(()),
    }
}
