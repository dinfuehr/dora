use std::collections::HashMap;

use crate::parsety::ty_for_sym;
use crate::sema::{Element, Sema, SourceFileId, TraitDefinitionId};
use crate::sym::SymbolKind;
use crate::{ErrorMessage, SourceType, SourceTypeArray, TraitType};

use super::{TypeArgument, TypeRef, TypeRefId, type_ref_span};

pub(crate) fn convert_type_ref(
    sa: &Sema,
    file_id: SourceFileId,
    type_ref_id: TypeRefId,
) -> SourceType {
    match &sa.type_refs[type_ref_id] {
        TypeRef::This => SourceType::This,
        TypeRef::Error => SourceType::Error,
        TypeRef::Ref { .. } => SourceType::Error,
        TypeRef::Tuple { subtypes } => {
            let subtypes = subtypes
                .iter()
                .map(|subtype| convert_type_ref(sa, file_id, *subtype))
                .collect::<Vec<_>>();
            let subtypes = SourceTypeArray::with(subtypes);

            if subtypes.is_empty() {
                SourceType::Unit
            } else {
                SourceType::Tuple(subtypes)
            }
        }
        TypeRef::Lambda { params, return_ty } => {
            let params = params
                .iter()
                .map(|param| convert_type_ref(sa, file_id, *param))
                .collect::<Vec<_>>();
            let params = SourceTypeArray::with(params);
            let return_ty = convert_type_ref(sa, file_id, *return_ty);

            SourceType::Lambda(params, Box::new(return_ty))
        }
        TypeRef::Path { type_arguments, .. } => {
            let symbol = match sa.type_ref_symbol(type_ref_id) {
                Some(symbol) => symbol,
                None => return SourceType::Error,
            };

            convert_type_ref_symbol(sa, file_id, type_ref_id, symbol, type_arguments)
        }
        TypeRef::Assoc { .. } => {
            let symbol = match sa.type_ref_symbol(type_ref_id) {
                Some(symbol) => symbol,
                None => return SourceType::Error,
            };

            convert_type_ref_symbol(sa, file_id, type_ref_id, symbol, &[])
        }
        TypeRef::QualifiedPath { ty, trait_ty, .. } => {
            convert_type_ref_qualified_path(sa, file_id, type_ref_id, *ty, *trait_ty)
        }
    }
}

fn convert_type_ref_symbol(
    sa: &Sema,
    file_id: SourceFileId,
    type_ref_id: TypeRefId,
    symbol: SymbolKind,
    type_arguments: &[TypeArgument],
) -> SourceType {
    let span = type_ref_span(sa, file_id, type_ref_id);

    match symbol {
        SymbolKind::TypeParam(id) => {
            if !type_arguments.is_empty() {
                sa.report(file_id, span, ErrorMessage::NoTypeParamsExpected);
                return SourceType::Error;
            }

            SourceType::TypeParam(id)
        }
        SymbolKind::Trait(trait_id) => {
            convert_type_ref_trait_object(sa, file_id, type_ref_id, trait_id, type_arguments)
        }
        SymbolKind::Class(..)
        | SymbolKind::Struct(..)
        | SymbolKind::Enum(..)
        | SymbolKind::Alias(..) => {
            let mut new_type_params = Vec::with_capacity(type_arguments.len());

            for arg in type_arguments {
                if arg.name.is_some() {
                    sa.report(file_id, span, ErrorMessage::UnexpectedTypeBinding);
                    return SourceType::Error;
                }

                let ty = convert_type_ref(sa, file_id, arg.ty);
                new_type_params.push(ty);
            }

            let new_type_params = SourceTypeArray::with(new_type_params);
            let callee_element = get_symbol_element(sa, symbol);
            let callee_type_param_definition = callee_element.type_param_definition();

            if callee_type_param_definition.type_param_count() == new_type_params.len() {
                ty_for_sym(sa, symbol, new_type_params)
            } else {
                let msg = ErrorMessage::WrongNumberTypeParams(
                    callee_type_param_definition.type_param_count(),
                    new_type_params.len(),
                );
                sa.report(file_id, span, msg);
                SourceType::Error
            }
        }
        _ => unreachable!(),
    }
}

fn convert_type_ref_trait_object(
    sa: &Sema,
    file_id: SourceFileId,
    type_ref_id: TypeRefId,
    trait_id: TraitDefinitionId,
    type_arguments: &[TypeArgument],
) -> SourceType {
    let trait_ = sa.trait_(trait_id);
    let span = type_ref_span(sa, file_id, type_ref_id);
    let mut idx = 0;
    let mut trait_type_params = Vec::new();

    while idx < type_arguments.len() {
        let arg = &type_arguments[idx];

        if arg.name.is_some() {
            break;
        }

        let ty = convert_type_ref(sa, file_id, arg.ty);
        trait_type_params.push(ty);
        idx += 1;
    }

    let mut used_aliases = HashMap::new();

    while idx < type_arguments.len() {
        let arg = &type_arguments[idx];

        if arg.name.is_none() {
            sa.report(file_id, span, ErrorMessage::TypeBindingOrder);
            return SourceType::Error;
        }

        let name = arg.name.expect("name expected");

        if let Some(&alias_id) = trait_.alias_names().get(&name) {
            if used_aliases.contains_key(&alias_id) {
                sa.report(file_id, span, ErrorMessage::DuplicateTypeBinding);
                return SourceType::Error;
            }

            let ty = convert_type_ref(sa, file_id, arg.ty);
            used_aliases.insert(alias_id, ty);
        } else {
            sa.report(file_id, span, ErrorMessage::UnknownTypeBinding);
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
            sa.report(file_id, span, ErrorMessage::MissingTypeBinding(name));
            return SourceType::Error;
        }
    }

    SourceType::TraitObject(trait_id, trait_type_params.into(), bindings.into())
}

fn convert_type_ref_qualified_path(
    sa: &Sema,
    file_id: SourceFileId,
    type_ref_id: TypeRefId,
    ty: TypeRefId,
    trait_ty: TypeRefId,
) -> SourceType {
    let assoc_id = match sa.type_ref_symbol(type_ref_id) {
        Some(SymbolKind::Alias(assoc_id)) => assoc_id,
        _ => return SourceType::Error,
    };

    let _ = convert_type_ref(sa, file_id, ty);
    let trait_ty = convert_type_ref(sa, file_id, trait_ty);

    let trait_ty = match trait_ty {
        SourceType::TraitObject(..) => TraitType::new_ty(sa, trait_ty),
        _ => return SourceType::Error,
    };

    SourceType::Assoc { trait_ty, assoc_id }
}

fn get_symbol_element(sa: &Sema, sym: SymbolKind) -> &dyn Element {
    match sym {
        SymbolKind::Class(id) => sa.class(id),
        SymbolKind::Struct(id) => sa.struct_(id),
        SymbolKind::Enum(id) => sa.enum_(id),
        SymbolKind::Alias(id) => sa.alias(id),
        _ => unimplemented!(),
    }
}
