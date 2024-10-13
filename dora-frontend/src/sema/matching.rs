use crate::sema::{
    implements_trait, maybe_alias_ty, ExtensionDefinitionId, Sema, TypeParamDefinition, TypeParamId,
};
use crate::{SourceType, SourceTypeArray, SymbolKind};

pub fn extension_matches(
    sa: &Sema,
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    extension_id: ExtensionDefinitionId,
) -> Option<SourceTypeArray> {
    let extension = sa.extension(extension_id);
    let bindings = block_matches_ty(
        sa,
        check_ty,
        check_type_param_defs,
        extension.ty().clone(),
        extension.type_param_definition(),
    );

    bindings.map(|bindings| {
        SourceTypeArray::with(
            bindings
                .into_iter()
                .map(|t| t.expect("missing binding"))
                .collect(),
        )
    })
}

pub fn block_matches_ty(
    sa: &Sema,
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    ext_ty: SourceType,
    ext_type_param_defs: &TypeParamDefinition,
) -> Option<Vec<Option<SourceType>>> {
    let mut bindings = vec![None; ext_type_param_defs.type_param_count()];

    let result = match_types(
        sa,
        check_ty,
        check_type_param_defs,
        ext_ty.clone(),
        ext_type_param_defs,
        &mut bindings,
    );

    if result {
        Some(bindings)
    } else {
        None
    }
}

pub fn match_arrays(
    sa: &Sema,
    check_array: &SourceTypeArray,
    check_type_param_definition: &TypeParamDefinition,
    extended_array: &SourceTypeArray,
    extended_type_param_definition: &TypeParamDefinition,
    bindings: &mut [Option<SourceType>],
) -> bool {
    if check_array.len() != extended_array.len() {
        return false;
    }

    for (check_ty, extended_ty) in check_array.iter().zip(extended_array.iter()) {
        if !match_types(
            sa,
            check_ty,
            check_type_param_definition,
            extended_ty,
            extended_type_param_definition,
            bindings,
        ) {
            return false;
        }
    }

    true
}

fn match_types(
    sa: &Sema,
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    ext_ty: SourceType,
    ext_type_param_defs: &TypeParamDefinition,
    bindings: &mut [Option<SourceType>],
) -> bool {
    let check_ty = maybe_alias_ty(sa, check_ty);
    let ext_ty = maybe_alias_ty(sa, ext_ty);

    if let SourceType::TypeParam(ext_tp_id) = ext_ty {
        let binding = bindings[ext_tp_id.index()].clone();

        if let Some(binding) = binding {
            match_concrete_types(
                sa,
                check_ty,
                check_type_param_defs,
                binding,
                ext_type_param_defs,
                bindings,
            )
        } else {
            let result = if check_ty.is_type_param() {
                match_type_params(
                    sa,
                    check_ty.clone(),
                    check_type_param_defs,
                    ext_ty,
                    ext_type_param_defs,
                )
            } else {
                match_concrete_against_bounds(
                    sa,
                    check_ty.clone(),
                    check_type_param_defs,
                    ext_tp_id,
                    ext_type_param_defs,
                )
            };

            bindings[ext_tp_id.index()] = Some(check_ty);

            result
        }
    } else {
        if check_ty.is_type_param() {
            false
        } else {
            match_concrete_types(
                sa,
                check_ty,
                check_type_param_defs,
                ext_ty,
                ext_type_param_defs,
                bindings,
            )
        }
    }
}

fn match_type_params(
    _sa: &Sema,
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    ext_ty: SourceType,
    ext_type_param_defs: &TypeParamDefinition,
) -> bool {
    let ext_tp_id = ext_ty.type_param_id().expect("expected type param");
    let check_tp_id = check_ty.type_param_id().expect("expected type param");

    for trait_ty in ext_type_param_defs.bounds_for_type_param(ext_tp_id) {
        if !check_type_param_defs.implements_trait(check_tp_id, trait_ty) {
            return false;
        }
    }

    true
}

fn match_concrete_against_bounds(
    sa: &Sema,
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    ext_tp_id: TypeParamId,
    ext_type_param_defs: &TypeParamDefinition,
) -> bool {
    for trait_ty in ext_type_param_defs.bounds_for_type_param(ext_tp_id) {
        if !implements_trait(sa, check_ty.clone(), check_type_param_defs, trait_ty) {
            return false;
        }
    }

    true
}

fn match_concrete_types(
    sa: &Sema,
    check_ty: SourceType,
    check_type_param_defs: &TypeParamDefinition,
    ext_ty: SourceType,
    ext_type_param_defs: &TypeParamDefinition,
    bindings: &mut [Option<SourceType>],
) -> bool {
    if check_ty.is_error() || ext_ty.is_error() {
        return true;
    }

    match check_ty {
        SourceType::Unit
        | SourceType::Bool
        | SourceType::Char
        | SourceType::UInt8
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::TypeParam(_) => check_ty == ext_ty,

        SourceType::Lambda(check_params, check_ret_type) => match ext_ty {
            SourceType::Lambda(ext_params, ext_ret_type) => {
                match_arrays(
                    sa,
                    &check_params,
                    check_type_param_defs,
                    &ext_params,
                    ext_type_param_defs,
                    bindings,
                ) && match_types(
                    sa,
                    *check_ret_type,
                    check_type_param_defs,
                    *ext_ret_type,
                    ext_type_param_defs,
                    bindings,
                )
            }

            _ => false,
        },

        SourceType::Alias(..) | SourceType::Assoc(..) => {
            unimplemented!()
        }

        SourceType::Tuple(check_subtypes) => match ext_ty {
            SourceType::Tuple(ext_subtypes) => match_arrays(
                sa,
                &check_subtypes,
                check_type_param_defs,
                &ext_subtypes,
                ext_type_param_defs,
                bindings,
            ),
            _ => false,
        },

        SourceType::Struct(..) | SourceType::Enum(..) | SourceType::Class(..) => {
            let check_sym = sym_for_ty(check_ty.clone()).expect("type expected");
            let ext_sym = sym_for_ty(ext_ty.clone());

            if Some(check_sym) != ext_sym {
                return false;
            }

            match_arrays(
                sa,
                &check_ty.type_params(),
                check_type_param_defs,
                &ext_ty.type_params(),
                ext_type_param_defs,
                bindings,
            )
        }

        SourceType::TraitObject(check_trait_id, check_type_params, check_bindings) => {
            match ext_ty {
                SourceType::TraitObject(ext_trait_id, ext_type_params, ext_bindings) => {
                    check_trait_id == ext_trait_id
                        && match_arrays(
                            sa,
                            &check_type_params,
                            check_type_param_defs,
                            &ext_type_params,
                            ext_type_param_defs,
                            bindings,
                        )
                        && match_arrays(
                            sa,
                            &check_bindings,
                            check_type_param_defs,
                            &ext_bindings,
                            ext_type_param_defs,
                            bindings,
                        )
                }
                _ => false,
            }
        }

        SourceType::Ptr | SourceType::Error | SourceType::This | SourceType::Any => {
            unreachable!()
        }
    }
}

fn sym_for_ty(ty: SourceType) -> Option<SymbolKind> {
    match ty {
        SourceType::Class(id, ..) => Some(SymbolKind::Class(id)),
        SourceType::Struct(id, ..) => Some(SymbolKind::Struct(id)),
        SourceType::Enum(id, ..) => Some(SymbolKind::Enum(id)),
        SourceType::TraitObject(id, ..) => Some(SymbolKind::Trait(id)),
        _ => None,
    }
}
