#![allow(dead_code)]

use crate::ModuleSymTable;
use crate::Name;
use crate::TraitType;
use crate::access::sym_accessible_from;
use crate::args;
use crate::error::diagnostics::{
    EXPECTED_PATH, EXPECTED_TYPE_NAME, NOT_ACCESSIBLE_IN_MODULE, UNEXPECTED_ASSOC, UNKNOWN_ASSOC,
    UNKNOWN_IDENTIFIER, UNKNOWN_IDENTIFIER_IN_MODULE,
};
use crate::sema::{
    AliasDefinitionId, Element, ModuleDefinitionId, Sema, SourceFileId, TypeParamId,
    parent_element_or_self,
};
use crate::sym::SymbolKind;

use super::{TypeRef, TypeRefArena, TypeRefId, TypeSymbol, type_ref_span};

pub(crate) fn parse_type_ref(
    sa: &Sema,
    type_refs: &TypeRefArena,
    table: &ModuleSymTable,
    file_id: SourceFileId,
    element: &dyn Element,
    type_ref_id: TypeRefId,
) {
    match type_refs.type_ref(type_ref_id) {
        TypeRef::Path {
            path,
            type_arguments,
        } => {
            if let Some(type_symbol) =
                resolve_path_symbol(sa, type_refs, table, element, path, file_id, type_ref_id)
            {
                type_refs.set_symbol(type_ref_id, type_symbol);
            }

            for arg in type_arguments {
                parse_type_ref(sa, type_refs, table, file_id, element, arg.ty);
            }
        }
        TypeRef::QualifiedPath { ty, trait_ty, name } => {
            parse_type_ref(sa, type_refs, table, file_id, element, *ty);
            parse_type_ref(sa, type_refs, table, file_id, element, *trait_ty);

            if let Some(TypeSymbol::Symbol(SymbolKind::Trait(trait_id))) =
                type_refs.symbol(*trait_ty)
            {
                let trait_ = sa.trait_(trait_id);
                if let Some(alias_id) = trait_.alias_names().get(name) {
                    type_refs.set_symbol(type_ref_id, TypeSymbol::Assoc(*alias_id));
                } else {
                    let span = get_qualified_path_name_span(sa, type_refs, file_id, type_ref_id);
                    sa.report(file_id, span, &UNKNOWN_ASSOC, args!());
                }
            }
        }
        TypeRef::Assoc { name } => {
            lookup_alias_on_self(sa, type_refs, file_id, element, type_ref_id, *name);
        }
        TypeRef::Tuple { subtypes } => {
            for subtype in subtypes {
                parse_type_ref(sa, type_refs, table, file_id, element, *subtype);
            }
        }
        TypeRef::Lambda { params, return_ty } => {
            for param in params {
                parse_type_ref(sa, type_refs, table, file_id, element, *param);
            }
            parse_type_ref(sa, type_refs, table, file_id, element, *return_ty);
        }
        TypeRef::Ref { ty } => {
            parse_type_ref(sa, type_refs, table, file_id, element, *ty);
        }
        TypeRef::This => {}
        TypeRef::Error => {}
    }
}

fn resolve_path_symbol(
    sa: &Sema,
    type_refs: &TypeRefArena,
    table: &ModuleSymTable,
    element: &dyn Element,
    path: &[Name],
    file_id: SourceFileId,
    type_ref_id: TypeRefId,
) -> Option<TypeSymbol> {
    if path.is_empty() {
        return None;
    }

    let first = path[0];
    let mut sym = match table.get(first) {
        Some(sym) => sym,
        None => {
            report_unknown_symbol(sa, type_refs, file_id, type_ref_id, first, 0);
            return None;
        }
    };

    for (index, name) in path.iter().enumerate().skip(1) {
        match sym {
            SymbolKind::Module(module_id) => {
                let module = sa.module(module_id);
                let current_sym = match module.table().get(*name) {
                    Some(sym) => sym,
                    None => {
                        report_unknown_symbol_in_module(
                            sa,
                            type_refs,
                            file_id,
                            type_ref_id,
                            module_id,
                            *name,
                            index,
                        );
                        return None;
                    }
                };
                if !sym_accessible_from(sa, current_sym.clone(), module_id) {
                    report_inaccessible_symbol(
                        sa,
                        type_refs,
                        file_id,
                        type_ref_id,
                        module_id,
                        *name,
                        index,
                    );
                    return None;
                }
                sym = current_sym;
            }
            SymbolKind::TypeParam(tp_id) => {
                if index + 1 < path.len() {
                    sa.report(
                        file_id,
                        type_ref_span(sa, type_refs, file_id, type_ref_id),
                        &EXPECTED_PATH,
                        args!(),
                    );
                    return None;
                }

                if let Some((alias_id, trait_ty)) =
                    lookup_alias_on_type_param(sa, element, tp_id, *name)
                {
                    return Some(TypeSymbol::GenericAssoc {
                        alias_id,
                        tp_id,
                        trait_ty,
                    });
                }
                report_unknown_symbol(sa, type_refs, file_id, type_ref_id, *name, index);
                return None;
            }
            _ => {
                sa.report(
                    file_id,
                    type_ref_span(sa, type_refs, file_id, type_ref_id),
                    &EXPECTED_PATH,
                    args!(),
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
        | SymbolKind::TypeParam(..) => Some(TypeSymbol::Symbol(sym)),
        _ => {
            sa.report(
                file_id,
                type_ref_span(sa, type_refs, file_id, type_ref_id),
                &EXPECTED_TYPE_NAME,
                args!(),
            );
            None
        }
    }
}

fn report_unknown_symbol(
    sa: &Sema,
    type_refs: &TypeRefArena,
    file_id: SourceFileId,
    type_ref_id: TypeRefId,
    name: Name,
    index: usize,
) {
    let name_str = sa.interner.str(name).to_string();
    let span = get_path_segment_span(sa, type_refs, file_id, type_ref_id, index);
    sa.report(file_id, span, &UNKNOWN_IDENTIFIER, args!(name_str));
}

fn report_unknown_symbol_in_module(
    sa: &Sema,
    type_refs: &TypeRefArena,
    file_id: SourceFileId,
    type_ref_id: TypeRefId,
    module_id: ModuleDefinitionId,
    name: Name,
    index: usize,
) {
    let module = sa.module(module_id);
    let module_name = module.name(sa);
    let name_str = sa.interner.str(name).to_string();
    let span = get_path_segment_span(sa, type_refs, file_id, type_ref_id, index);
    sa.report(
        file_id,
        span,
        &UNKNOWN_IDENTIFIER_IN_MODULE,
        args!(module_name, name_str),
    );
}

fn report_inaccessible_symbol(
    sa: &Sema,
    type_refs: &TypeRefArena,
    file_id: SourceFileId,
    type_ref_id: TypeRefId,
    module_id: ModuleDefinitionId,
    name: Name,
    index: usize,
) {
    let module = sa.module(module_id);
    let module_name = module.name(sa);
    let name_str = sa.interner.str(name).to_string();
    let span = get_path_segment_span(sa, type_refs, file_id, type_ref_id, index);
    sa.report(
        file_id,
        span,
        &NOT_ACCESSIBLE_IN_MODULE,
        args!(module_name, name_str),
    );
}

/// Get the span of a specific path segment in a path type by index.
fn get_path_segment_span(
    sa: &Sema,
    type_refs: &TypeRefArena,
    file_id: SourceFileId,
    type_ref_id: TypeRefId,
    index: usize,
) -> dora_parser::Span {
    use dora_parser::ast::AstPathType;

    let syntax_node_ptr = type_refs
        .syntax_node_ptr(type_ref_id)
        .expect("missing syntax node ptr");
    let path_type = sa.syntax::<AstPathType>(file_id, syntax_node_ptr);

    path_type
        .path()
        .segments()
        .nth(index)
        .expect("path segment index out of bounds")
        .span()
}

/// Get the span of the name in a qualified path type (e.g., `Bar` in `<T as Foo>::Bar`).
fn get_qualified_path_name_span(
    sa: &Sema,
    type_refs: &TypeRefArena,
    file_id: SourceFileId,
    type_ref_id: TypeRefId,
) -> dora_parser::Span {
    use dora_parser::ast::AstQualifiedPathType;

    let syntax_node_ptr = type_refs
        .syntax_node_ptr(type_ref_id)
        .expect("missing syntax node ptr");
    let qp = sa.syntax::<AstQualifiedPathType>(file_id, syntax_node_ptr);

    qp.name().expect("missing name in qualified path").span()
}

fn lookup_alias_on_self(
    sa: &Sema,
    type_refs: &TypeRefArena,
    file_id: SourceFileId,
    element: &dyn Element,
    type_ref_id: TypeRefId,
    name: Name,
) {
    let element = parent_element_or_self(sa, element);

    if let Some(trait_) = element.to_trait() {
        if let Some(alias_id) = trait_.alias_names().get(&name) {
            type_refs.set_symbol(type_ref_id, TypeSymbol::Assoc(*alias_id));
            return;
        }

        for bound in trait_.type_param_definition.bounds_for_self() {
            let trait_id = bound.trait_id;
            let trait_ = sa.trait_(trait_id);

            if let Some(id) = trait_.alias_names().get(&name) {
                type_refs.set_symbol(type_ref_id, TypeSymbol::Assoc(*id));
                return;
            }
        }

        let span = type_ref_span(sa, type_refs, file_id, type_ref_id);
        sa.report(file_id, span, &UNKNOWN_ASSOC, args!());
    } else if let Some(impl_) = element.to_impl() {
        if let Some(trait_id) = impl_.parsed_trait_ty().trait_id() {
            let trait_ = sa.trait_(trait_id);
            if let Some(alias_id) = trait_.alias_names().get(&name) {
                type_refs.set_symbol(type_ref_id, TypeSymbol::Assoc(*alias_id));
                return;
            }
            let span = type_ref_span(sa, type_refs, file_id, type_ref_id);
            sa.report(file_id, span, &UNKNOWN_ASSOC, args!());
        } else {
            // impl without a trait - Self::X not allowed
            let span = type_ref_span(sa, type_refs, file_id, type_ref_id);
            sa.report(file_id, span, &UNEXPECTED_ASSOC, args!());
        }
    } else {
        // Not in a trait or impl context - Self::X not allowed
        let span = type_ref_span(sa, type_refs, file_id, type_ref_id);
        sa.report(file_id, span, &UNEXPECTED_ASSOC, args!());
    }
}

fn lookup_alias_on_type_param(
    sa: &Sema,
    element: &dyn Element,
    id: TypeParamId,
    name: Name,
) -> Option<(AliasDefinitionId, TraitType)> {
    let type_param_definition = element.type_param_definition();
    let mut results = Vec::with_capacity(2);

    for bound in type_param_definition.bounds_for_type_param(id) {
        let trait_id = bound.trait_id;
        let trait_ = sa.trait_(trait_id);

        if let Some(alias_id) = trait_.alias_names().get(&name) {
            results.push((*alias_id, bound.clone()));
        }
    }

    if results.len() == 1 {
        results.pop()
    } else {
        None
    }
}
