use crate::sema::{Element, Sema, TraitDefinitionId};
use crate::{SourceType, SourceTypeArray, TraitType};

pub fn specialize_trait_type(sa: &Sema, ty: TraitType, type_params: &SourceTypeArray) -> TraitType {
    TraitType {
        trait_id: ty.trait_id,
        type_params: replace_sta(sa, ty.type_params, Some(type_params), None),
        bindings: ty
            .bindings
            .into_iter()
            .map(|(id, ty)| (id, replace_type(sa, ty, Some(type_params), None)))
            .collect(),
    }
}

pub fn specialize_type(sa: &Sema, ty: SourceType, type_params: &SourceTypeArray) -> SourceType {
    replace_type(sa, ty, Some(type_params), None)
}

pub fn specialize_type_array(
    sa: &Sema,
    types: &SourceTypeArray,
    type_params: &SourceTypeArray,
) -> SourceTypeArray {
    let new_types = types
        .iter()
        .map(|ty| specialize_type(sa, ty, type_params))
        .collect();

    SourceTypeArray::with(new_types)
}

pub fn replace_type(
    sa: &Sema,
    ty: SourceType,
    type_params: Option<&SourceTypeArray>,
    self_ty: Option<SourceType>,
) -> SourceType {
    match ty {
        SourceType::Class(cls_id, cls_type_params) => SourceType::Class(
            cls_id,
            replace_sta(sa, cls_type_params, type_params, self_ty),
        ),

        SourceType::TraitObject(trait_id, trait_type_params, bindings) => SourceType::TraitObject(
            trait_id,
            replace_sta(sa, trait_type_params, type_params, self_ty.clone()),
            replace_sta(sa, bindings, type_params, self_ty),
        ),

        SourceType::Struct(struct_id, struct_type_params) => SourceType::Struct(
            struct_id,
            replace_sta(sa, struct_type_params, type_params, self_ty),
        ),

        SourceType::Enum(enum_id, enum_type_params) => SourceType::Enum(
            enum_id,
            replace_sta(sa, enum_type_params, type_params, self_ty),
        ),

        SourceType::Alias(alias_id, alias_type_params) => SourceType::Alias(
            alias_id,
            replace_sta(sa, alias_type_params, type_params, self_ty),
        ),

        SourceType::Assoc(alias_id, alias_type_params) => SourceType::Assoc(
            alias_id,
            replace_sta(sa, alias_type_params, type_params, self_ty),
        ),

        SourceType::Lambda(params, return_type) => SourceType::Lambda(
            replace_sta(sa, params, type_params, self_ty.clone()),
            Box::new(replace_type(sa, *return_type, type_params, self_ty)),
        ),

        SourceType::Tuple(subtypes) => {
            SourceType::Tuple(replace_sta(sa, subtypes, type_params, self_ty))
        }

        SourceType::This => {
            if let Some(self_ty) = self_ty {
                self_ty
            } else {
                ty
            }
        }

        SourceType::TypeParam(id) => {
            if let Some(type_params) = type_params {
                type_params[id.index()].clone()
            } else {
                ty
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
        | SourceType::Error => ty,

        SourceType::Any | SourceType::Ptr | SourceType::GenericAssoc(..) => unreachable!(),
    }
}

fn replace_sta(
    sa: &Sema,
    array: SourceTypeArray,
    type_params: Option<&SourceTypeArray>,
    self_ty: Option<SourceType>,
) -> SourceTypeArray {
    let new_array = array
        .iter()
        .map(|ty| replace_type(sa, ty, type_params.clone(), self_ty.clone()))
        .collect::<Vec<_>>();
    SourceTypeArray::with(new_array)
}

pub fn specialize_ty_for_trait_object(
    sa: &Sema,
    ty: SourceType,
    trait_id: TraitDefinitionId,
    type_params: &SourceTypeArray,
    assoc_types: &SourceTypeArray,
) -> SourceType {
    match ty {
        SourceType::Class(cls_id, cls_type_params) => SourceType::Class(
            cls_id,
            specialize_ty_for_trait_object_array(
                sa,
                cls_type_params,
                trait_id,
                type_params,
                assoc_types,
            ),
        ),

        SourceType::TraitObject(trait_id, trait_type_params, bindings) => SourceType::TraitObject(
            trait_id,
            specialize_ty_for_trait_object_array(
                sa,
                trait_type_params,
                trait_id,
                type_params,
                assoc_types,
            ),
            specialize_ty_for_trait_object_array(sa, bindings, trait_id, type_params, assoc_types),
        ),

        SourceType::Struct(struct_id, struct_type_params) => SourceType::Struct(
            struct_id,
            specialize_ty_for_trait_object_array(
                sa,
                struct_type_params,
                trait_id,
                type_params,
                assoc_types,
            ),
        ),

        SourceType::Enum(enum_id, enum_type_params) => SourceType::Enum(
            enum_id,
            specialize_ty_for_trait_object_array(
                sa,
                enum_type_params,
                trait_id,
                type_params,
                assoc_types,
            ),
        ),

        SourceType::Alias(alias_id, alias_type_params) => SourceType::Alias(
            alias_id,
            specialize_ty_for_trait_object_array(
                sa,
                alias_type_params,
                trait_id,
                type_params,
                assoc_types,
            ),
        ),

        SourceType::Assoc(alias_id, alias_type_params) => {
            let alias = sa.alias(alias_id);
            assert!(alias_type_params.is_empty());
            assoc_types[alias.idx_in_trait()].clone()
        }

        SourceType::Lambda(params, return_type) => SourceType::Lambda(
            specialize_ty_for_trait_object_array(sa, params, trait_id, type_params, assoc_types),
            Box::new(specialize_ty_for_trait_object(
                sa,
                *return_type,
                trait_id,
                type_params,
                assoc_types,
            )),
        ),

        SourceType::Tuple(subtypes) => SourceType::Tuple(specialize_ty_for_trait_object_array(
            sa,
            subtypes,
            trait_id,
            type_params,
            assoc_types,
        )),

        SourceType::TypeParam(id) => type_params[id.index()].clone(),

        SourceType::Unit
        | SourceType::UInt8
        | SourceType::Bool
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Error => ty,

        SourceType::This | SourceType::Any | SourceType::Ptr | SourceType::GenericAssoc(..) => {
            unreachable!()
        }
    }
}

fn specialize_ty_for_trait_object_array(
    sa: &Sema,
    array: SourceTypeArray,
    trait_id: TraitDefinitionId,
    type_params: &SourceTypeArray,
    assoc_types: &SourceTypeArray,
) -> SourceTypeArray {
    let new_array = array
        .iter()
        .map(|ty| specialize_ty_for_trait_object(sa, ty, trait_id, type_params, assoc_types))
        .collect::<Vec<_>>();
    SourceTypeArray::with(new_array)
}

pub fn specialize_for_element(
    sa: &Sema,
    ty: SourceType,
    element: &dyn Element,
    type_params_for_element: &SourceTypeArray,
) -> SourceType {
    match ty {
        SourceType::Class(cls_id, cls_type_params) => SourceType::Class(
            cls_id,
            specialize_for_element_array(sa, cls_type_params, element, type_params_for_element),
        ),

        SourceType::TraitObject(trait_id, trait_type_params, bindings) => SourceType::TraitObject(
            trait_id,
            specialize_for_element_array(sa, trait_type_params, element, type_params_for_element),
            specialize_for_element_array(sa, bindings, element, type_params_for_element),
        ),

        SourceType::Struct(struct_id, struct_type_params) => SourceType::Struct(
            struct_id,
            specialize_for_element_array(sa, struct_type_params, element, type_params_for_element),
        ),

        SourceType::Enum(enum_id, enum_type_params) => SourceType::Enum(
            enum_id,
            specialize_for_element_array(sa, enum_type_params, element, type_params_for_element),
        ),

        SourceType::Alias(alias_id, alias_type_params) => SourceType::Alias(
            alias_id,
            specialize_for_element_array(sa, alias_type_params, element, type_params_for_element),
        ),

        SourceType::Lambda(params, return_type) => SourceType::Lambda(
            specialize_for_element_array(sa, params, element, type_params_for_element),
            Box::new(specialize_for_element(
                sa,
                *return_type,
                element,
                type_params_for_element,
            )),
        ),

        SourceType::Tuple(subtypes) => SourceType::Tuple(specialize_for_element_array(
            sa,
            subtypes,
            element,
            type_params_for_element,
        )),

        SourceType::This => {
            assert!(element.is_trait());
            SourceType::This
        }

        SourceType::TypeParam(id) => type_params_for_element[id.index()].clone(),

        SourceType::Unit
        | SourceType::UInt8
        | SourceType::Bool
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Error => ty,

        SourceType::Any
        | SourceType::Ptr
        | SourceType::Assoc(..)
        | SourceType::GenericAssoc(..) => unreachable!(),
    }
}

fn specialize_for_element_array(
    sa: &Sema,
    array: SourceTypeArray,
    element: &dyn Element,
    type_params_for_element: &SourceTypeArray,
) -> SourceTypeArray {
    let new_array = array
        .iter()
        .map(|ty| specialize_for_element(sa, ty, element, type_params_for_element))
        .collect::<Vec<_>>();
    SourceTypeArray::with(new_array)
}
