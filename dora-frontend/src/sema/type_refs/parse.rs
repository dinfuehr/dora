#![allow(dead_code)]

use crate::ModuleSymTable;
use crate::Name;
use crate::access::sym_accessible_from;
use crate::sema::{
    AliasDefinitionId, Element, ErrorMessage, ModuleDefinitionId, Sema, SourceFileId, TypeParamId,
    parent_element_or_self,
};
use crate::sym::SymbolKind;

use super::{TypeRef, TypeRefId, type_ref_span};

pub(crate) fn parse_type_ref(
    sa: &Sema,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    element: &dyn Element,
    allow_self: bool,
    type_ref_id: TypeRefId,
) {
    match &sa.type_refs[type_ref_id] {
        TypeRef::Path {
            path,
            type_arguments,
        } => {
            if let Some(sym) = resolve_path_symbol(sa, table, element, path, file_id, type_ref_id) {
                sa.set_type_ref_symbol(type_ref_id, sym);
            }

            for arg in type_arguments {
                parse_type_ref(sa, table, file_id, element, allow_self, arg.ty);
            }
        }
        TypeRef::QualifiedPath { ty, trait_ty, name } => {
            parse_type_ref(sa, table, file_id, element, allow_self, *ty);
            parse_type_ref(sa, table, file_id, element, allow_self, *trait_ty);

            if let Some(SymbolKind::Trait(trait_id)) = sa.type_ref_symbol(*trait_ty) {
                let trait_ = sa.trait_(trait_id);
                if let Some(alias_id) = trait_.alias_names().get(name) {
                    sa.set_type_ref_symbol(type_ref_id, SymbolKind::Alias(*alias_id));
                }
            }
        }
        TypeRef::Assoc { name } => {
            if let Some(alias_id) = lookup_alias_on_self(sa, element, *name) {
                sa.set_type_ref_symbol(type_ref_id, SymbolKind::Alias(alias_id));
            } else {
                sa.report(
                    file_id,
                    type_ref_span(sa, file_id, type_ref_id),
                    ErrorMessage::UnknownAssoc,
                );
            }
        }
        TypeRef::Tuple { subtypes } => {
            for subtype in subtypes {
                parse_type_ref(sa, table, file_id, element, allow_self, *subtype);
            }
        }
        TypeRef::Lambda { params, return_ty } => {
            for param in params {
                parse_type_ref(sa, table, file_id, element, allow_self, *param);
            }
            parse_type_ref(sa, table, file_id, element, allow_self, *return_ty);
        }
        TypeRef::Ref { ty } => {
            parse_type_ref(sa, table, file_id, element, allow_self, *ty);
        }
        TypeRef::This => {
            if !allow_self {
                sa.report(
                    file_id,
                    type_ref_span(sa, file_id, type_ref_id),
                    ErrorMessage::SelfTypeUnavailable,
                );
            }
        }
        TypeRef::Error => {}
    }
}

fn resolve_path_symbol(
    sa: &Sema,
    table: &ModuleSymTable,
    element: &dyn Element,
    path: &[Name],
    file_id: SourceFileId,
    type_ref_id: TypeRefId,
) -> Option<SymbolKind> {
    let mut iter = path.iter();
    let first = match iter.next() {
        Some(first) => first,
        None => return None,
    };
    let mut sym = match table.get(*first) {
        Some(sym) => sym,
        None => {
            report_unknown_symbol(sa, file_id, type_ref_id, *first);
            return None;
        }
    };

    while let Some(name) = iter.next() {
        match sym {
            SymbolKind::Module(module_id) => {
                let module = sa.module(module_id);
                let current_sym = match module.table().get(*name) {
                    Some(sym) => sym,
                    None => {
                        report_unknown_symbol(sa, file_id, type_ref_id, *name);
                        return None;
                    }
                };
                if !sym_accessible_from(sa, current_sym.clone(), module_id) {
                    report_inaccessible_symbol(sa, file_id, type_ref_id, module_id, *name);
                    return None;
                }
                sym = current_sym;
            }
            SymbolKind::TypeParam(id) => {
                if iter.next().is_some() {
                    sa.report(
                        file_id,
                        type_ref_span(sa, file_id, type_ref_id),
                        ErrorMessage::ExpectedPath,
                    );
                    return None;
                }

                if let Some(alias_id) = lookup_alias_on_type_param(sa, element, id, *name) {
                    return Some(SymbolKind::Alias(alias_id));
                }

                report_unknown_symbol(sa, file_id, type_ref_id, *name);
                return None;
            }
            _ => {
                sa.report(
                    file_id,
                    type_ref_span(sa, file_id, type_ref_id),
                    ErrorMessage::ExpectedPath,
                );
                return None;
            }
        }
    }

    match sym {
        SymbolKind::Trait(..)
        | SymbolKind::Class(..)
        | SymbolKind::Struct(..)
        | SymbolKind::Enum(..)
        | SymbolKind::Alias(..)
        | SymbolKind::TypeParam(..) => Some(sym),
        _ => {
            sa.report(
                file_id,
                type_ref_span(sa, file_id, type_ref_id),
                ErrorMessage::ExpectedTypeName,
            );
            None
        }
    }
}

fn report_unknown_symbol(sa: &Sema, file_id: SourceFileId, type_ref_id: TypeRefId, name: Name) {
    let name = sa.interner.str(name).to_string();
    sa.report(
        file_id,
        type_ref_span(sa, file_id, type_ref_id),
        ErrorMessage::UnknownIdentifier(name),
    );
}

fn report_inaccessible_symbol(
    sa: &Sema,
    file_id: SourceFileId,
    type_ref_id: TypeRefId,
    module_id: ModuleDefinitionId,
    name: Name,
) {
    let module = sa.module(module_id);
    let module_name = module.name(sa);
    let name = sa.interner.str(name).to_string();
    sa.report(
        file_id,
        type_ref_span(sa, file_id, type_ref_id),
        ErrorMessage::NotAccessibleInModule(module_name, name),
    );
}

fn lookup_alias_on_self(sa: &Sema, element: &dyn Element, name: Name) -> Option<AliasDefinitionId> {
    let element = parent_element_or_self(sa, element);

    if let Some(trait_) = element.to_trait() {
        if let Some(alias_id) = trait_.alias_names().get(&name) {
            return Some(*alias_id);
        }

        for bound in trait_.type_param_definition.bounds_for_self() {
            let trait_id = bound.trait_id;
            let trait_ = sa.trait_(trait_id);

            if let Some(id) = trait_.alias_names().get(&name) {
                return Some(*id);
            }
        }
    } else if let Some(impl_) = element.to_impl() {
        if let Some(trait_id) = impl_.parsed_trait_ty().trait_id() {
            let trait_ = sa.trait_(trait_id);
            return trait_.alias_names().get(&name).cloned();
        }
    }

    None
}

fn lookup_alias_on_type_param(
    sa: &Sema,
    element: &dyn Element,
    id: TypeParamId,
    name: Name,
) -> Option<AliasDefinitionId> {
    let type_param_definition = element.type_param_definition();
    let mut results = Vec::with_capacity(2);

    for bound in type_param_definition.bounds_for_type_param(id) {
        let trait_id = bound.trait_id;
        let trait_ = sa.trait_(trait_id);

        if let Some(id) = trait_.alias_names().get(&name) {
            results.push(*id);
        }
    }

    if results.len() == 1 {
        results.pop()
    } else {
        None
    }
}
