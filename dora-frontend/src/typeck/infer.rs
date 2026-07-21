use crate::error::diagnostics::CANNOT_INFER_TYPE;
use crate::sema::{
    TypeContext, TypeRef, TypeRefId, check_type_ref, convert_type_ref, parse_type_ref,
    type_ref_span,
};
use crate::{SourceType, SourceTypeArray, TraitType, args};

use super::{TypeCheck, TypeVarId, TypeVariable};

impl TypeCheck<'_> {
    pub(super) fn read_type_with_inference(
        &mut self,
        id: TypeRefId,
        type_variables: &mut Vec<TypeVarId>,
    ) -> SourceType {
        let type_refs = self.body.type_refs();
        parse_type_ref(
            self.sa,
            type_refs,
            &self.symtable,
            self.file_id,
            self.element,
            id,
            TypeContext::FunctionBody,
        );

        if matches!(type_refs.type_ref(id), TypeRef::Infer) {
            let type_var_id = TypeVarId(self.type_variables.len());
            self.type_variables.push(TypeVariable {
                value: None,
                type_ref_id: id,
            });
            type_variables.push(type_var_id);
            return SourceType::TypeVar(type_var_id);
        }

        let ty = convert_type_ref(
            self.sa,
            type_refs,
            self.element,
            id,
            TypeContext::FunctionBody,
        );
        let allow_self = self.self_ty.is_some();
        let ty = check_type_ref(self.sa, type_refs, self.element, id, ty, allow_self);
        crate::parsety::expand_st(self.sa, self.element, ty, self.self_ty.clone())
    }

    pub(crate) fn resolve_type(&mut self, ty: SourceType) -> SourceType {
        match ty {
            SourceType::TypeVar(id) => {
                let value = self
                    .type_variables
                    .get(id.0)
                    .expect("type variable not registered")
                    .value
                    .clone();

                if let Some(value) = value {
                    let value = self.resolve_type(value);
                    self.type_variables
                        .get_mut(id.0)
                        .expect("type variable not registered")
                        .value = Some(value.clone());
                    value
                } else {
                    SourceType::TypeVar(id)
                }
            }
            SourceType::Class(id, type_params) => {
                SourceType::Class(id, self.resolve_type_array(type_params))
            }
            SourceType::Struct(id, type_params) => {
                SourceType::Struct(id, self.resolve_type_array(type_params))
            }
            SourceType::Enum(id, type_params) => {
                SourceType::Enum(id, self.resolve_type_array(type_params))
            }
            SourceType::Tuple(subtypes) => SourceType::Tuple(self.resolve_type_array(subtypes)),
            SourceType::TraitObject(id, type_params, bindings) => SourceType::TraitObject(
                id,
                self.resolve_type_array(type_params),
                self.resolve_type_array(bindings),
            ),
            SourceType::Alias(id, type_params) => {
                SourceType::Alias(id, self.resolve_type_array(type_params))
            }
            SourceType::Assoc { trait_ty, assoc_id } => SourceType::Assoc {
                trait_ty: self.resolve_trait_type(trait_ty),
                assoc_id,
            },
            SourceType::GenericAssoc {
                ty,
                trait_ty,
                assoc_id,
            } => SourceType::GenericAssoc {
                ty: Box::new(self.resolve_type(*ty)),
                trait_ty: self.resolve_trait_type(trait_ty),
                assoc_id,
            },
            SourceType::Lambda(params, return_type, is_variadic) => SourceType::Lambda(
                self.resolve_type_array(params),
                Box::new(self.resolve_type(*return_type)),
                is_variadic,
            ),
            SourceType::Ref(inner) => SourceType::Ref(Box::new(self.resolve_type(*inner))),
            ty => ty,
        }
    }

    fn resolve_trait_type(&mut self, trait_ty: TraitType) -> TraitType {
        let type_params = self.resolve_type_array(trait_ty.type_params);
        let bindings = trait_ty
            .bindings
            .into_iter()
            .map(|(id, ty)| (id, self.resolve_type(ty)))
            .collect();
        TraitType {
            trait_id: trait_ty.trait_id,
            type_params,
            bindings,
        }
    }

    pub(crate) fn resolve_type_array(&mut self, types: SourceTypeArray) -> SourceTypeArray {
        SourceTypeArray::with(types.iter().map(|ty| self.resolve_type(ty)).collect())
    }

    pub(crate) fn expected_type_for_inference(&mut self, ty: SourceType) -> SourceType {
        let ty = self.resolve_type(ty);
        if type_contains_variable(&ty, None) {
            SourceType::Any
        } else {
            ty
        }
    }

    pub(crate) fn unify_types(&mut self, lhs: SourceType, rhs: SourceType) -> bool {
        let lhs = self.resolve_type(lhs);
        let rhs = self.resolve_type(rhs);

        match (lhs, rhs) {
            (SourceType::Error, _) | (_, SourceType::Error) => true,
            (SourceType::TypeVar(id), ty) | (ty, SourceType::TypeVar(id)) => {
                self.bind_type_variable(id, ty)
            }
            (SourceType::Class(lhs_id, lhs), SourceType::Class(rhs_id, rhs))
                if lhs_id == rhs_id =>
            {
                self.unify_type_arrays(lhs, rhs)
            }
            (SourceType::Struct(lhs_id, lhs), SourceType::Struct(rhs_id, rhs))
                if lhs_id == rhs_id =>
            {
                self.unify_type_arrays(lhs, rhs)
            }
            (SourceType::Enum(lhs_id, lhs), SourceType::Enum(rhs_id, rhs)) if lhs_id == rhs_id => {
                self.unify_type_arrays(lhs, rhs)
            }
            (SourceType::Alias(lhs_id, lhs), SourceType::Alias(rhs_id, rhs))
                if lhs_id == rhs_id =>
            {
                self.unify_type_arrays(lhs, rhs)
            }
            (
                SourceType::TraitObject(lhs_id, lhs, lhs_bindings),
                SourceType::TraitObject(rhs_id, rhs, rhs_bindings),
            ) if lhs_id == rhs_id => {
                self.unify_type_arrays(lhs, rhs)
                    && self.unify_type_arrays(lhs_bindings, rhs_bindings)
            }
            (SourceType::Tuple(lhs), SourceType::Tuple(rhs)) => self.unify_type_arrays(lhs, rhs),
            (SourceType::Ref(lhs), SourceType::Ref(rhs)) => self.unify_types(*lhs, *rhs),
            (
                SourceType::Lambda(lhs_params, lhs_return, lhs_variadic),
                SourceType::Lambda(rhs_params, rhs_return, rhs_variadic),
            ) => {
                lhs_variadic == rhs_variadic
                    && self.unify_type_arrays(lhs_params, rhs_params)
                    && self.unify_types(*lhs_return, *rhs_return)
            }
            (lhs, rhs) => lhs == rhs,
        }
    }

    pub(crate) fn unify_type_arrays(&mut self, lhs: SourceTypeArray, rhs: SourceTypeArray) -> bool {
        lhs.len() == rhs.len()
            && lhs
                .iter()
                .zip(rhs.iter())
                .all(|(lhs, rhs)| self.unify_types(lhs, rhs))
    }

    fn bind_type_variable(&mut self, id: TypeVarId, ty: SourceType) -> bool {
        if ty == SourceType::TypeVar(id) {
            return true;
        }

        if type_contains_variable(&ty, Some(id)) {
            return false;
        }

        self.type_variables
            .get_mut(id.0)
            .expect("type variable not registered")
            .value = Some(ty);
        true
    }

    pub(crate) fn report_unresolved_type_variables(
        &mut self,
        type_variables: &[TypeVarId],
    ) -> bool {
        let mut succeeded = true;

        for &id in type_variables {
            let resolved = self.resolve_type(SourceType::TypeVar(id));
            if type_contains_variable(&resolved, None) {
                let type_ref_id = self
                    .type_variables
                    .get(id.0)
                    .expect("type variable not registered")
                    .type_ref_id;
                self.report(
                    type_ref_span(self.sa, self.body.type_refs(), self.file_id, type_ref_id),
                    &CANNOT_INFER_TYPE,
                    args!(),
                );
                succeeded = false;
            }
        }

        succeeded
    }
}

fn type_contains_variable(ty: &SourceType, expected: Option<TypeVarId>) -> bool {
    match ty {
        SourceType::TypeVar(id) => expected.is_none_or(|expected| expected == *id),
        SourceType::Class(_, type_params)
        | SourceType::Struct(_, type_params)
        | SourceType::Enum(_, type_params)
        | SourceType::Alias(_, type_params)
        | SourceType::Tuple(type_params) => type_params
            .iter()
            .any(|ty| type_contains_variable(&ty, expected)),
        SourceType::TraitObject(_, type_params, bindings) => type_params
            .iter()
            .chain(bindings.iter())
            .any(|ty| type_contains_variable(&ty, expected)),
        SourceType::Lambda(params, return_type, _) => {
            params
                .iter()
                .any(|ty| type_contains_variable(&ty, expected))
                || type_contains_variable(return_type, expected)
        }
        SourceType::Assoc { trait_ty, .. } => {
            trait_ty
                .type_params
                .iter()
                .any(|ty| type_contains_variable(&ty, expected))
                || trait_ty
                    .bindings
                    .iter()
                    .any(|(_, ty)| type_contains_variable(ty, expected))
        }
        SourceType::GenericAssoc { ty, trait_ty, .. } => {
            type_contains_variable(ty, expected)
                || trait_ty
                    .type_params
                    .iter()
                    .any(|ty| type_contains_variable(&ty, expected))
                || trait_ty
                    .bindings
                    .iter()
                    .any(|(_, ty)| type_contains_variable(ty, expected))
        }
        SourceType::Ref(inner) => type_contains_variable(inner, expected),
        SourceType::Error
        | SourceType::Any
        | SourceType::Unit
        | SourceType::Bool
        | SourceType::Char
        | SourceType::UInt8
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Ptr
        | SourceType::This
        | SourceType::TypeParam(_) => false,
    }
}
