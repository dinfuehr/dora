use std::cell::RefCell;

use crate::args;
use crate::error::diagnostics::TYPE_NOT_IMPLEMENTING_TRAIT;
use crate::sema::{
    AliasDefinitionId, Element, Sema, SourceFileId, TraitDefinition, TraitDefinitionId,
    TypeParamDefinition, TypeRefId, check_trait_type_ref, check_type_ref, convert_trait_type_ref,
    convert_type_ref, implements_trait, parent_element_or_self, parse_type_ref,
};
use crate::sym::{ModuleSymTable, SymbolKind};
use crate::{
    SourceType, SourceTypeArray, Span, TraitType, replace_type, specialize_trait_type,
    specialize_type,
};

#[derive(Clone, Debug)]
pub struct ParsedType {
    type_ref_id: Option<TypeRefId>,
    ty: RefCell<Option<SourceType>>,
}

impl ParsedType {
    pub fn new_ty(ty: SourceType) -> ParsedType {
        ParsedType {
            type_ref_id: None,
            ty: RefCell::new(Some(ty)),
        }
    }

    pub fn new(type_ref_id: TypeRefId) -> ParsedType {
        ParsedType {
            type_ref_id: Some(type_ref_id),
            ty: RefCell::new(None),
        }
    }

    pub fn new_opt(type_ref_id: Option<TypeRefId>) -> ParsedType {
        if type_ref_id.is_some() {
            ParsedType {
                type_ref_id,
                ty: RefCell::new(None),
            }
        } else {
            ParsedType {
                type_ref_id: None,
                ty: RefCell::new(SourceType::Error.into()),
            }
        }
    }

    pub fn ty(&self) -> SourceType {
        self.ty.borrow().as_ref().cloned().expect("missing type")
    }

    pub fn maybe_ty(&self) -> Option<SourceType> {
        self.ty.borrow().as_ref().cloned()
    }

    pub fn set_ty(&self, ty: SourceType) {
        *self.ty.borrow_mut() = Some(ty);
    }

    pub fn parse(&self, sa: &Sema, table: &ModuleSymTable, element: &dyn Element) {
        if let Some(type_ref_id) = self.type_ref_id {
            let type_refs = element.type_ref_arena();
            let file_id = element.file_id();
            parse_type_ref(sa, type_refs, table, file_id, element, type_ref_id);
            let ty = convert_type_ref(sa, type_refs, element, type_ref_id);
            self.set_ty(ty);
        }
    }

    pub fn check(&self, sa: &Sema, element: &dyn Element, allow_self: bool) -> SourceType {
        if let Some(type_ref_id) = self.type_ref_id {
            let type_refs = element.type_ref_arena();
            let ty = self.ty();

            let ty = check_type_ref(sa, type_refs, element, type_ref_id, ty, allow_self);
            self.set_ty(ty.clone());
            return ty;
        }
        self.ty()
    }

    pub fn expand(
        &self,
        sa: &Sema,
        element: &dyn Element,
        replace_self: Option<SourceType>,
    ) -> SourceType {
        let new_ty = expand_st(sa, element, self.ty(), replace_self);
        self.set_ty(new_ty.clone());
        new_ty
    }
}

#[derive(Clone, Debug)]
pub struct ParsedTraitType {
    type_ref_id: Option<TypeRefId>,
    ty: RefCell<Option<TraitType>>,
}

impl ParsedTraitType {
    pub fn new(type_ref_id: TypeRefId) -> ParsedTraitType {
        ParsedTraitType {
            type_ref_id: Some(type_ref_id),
            ty: RefCell::new(None),
        }
    }

    pub fn new_ty(ty: Option<TraitType>) -> ParsedTraitType {
        ParsedTraitType {
            type_ref_id: None,
            ty: RefCell::new(ty),
        }
    }

    pub fn type_ref_id(&self) -> Option<TypeRefId> {
        self.type_ref_id
    }

    pub fn ty(&self) -> Option<TraitType> {
        self.ty.borrow().as_ref().cloned()
    }

    pub fn set_ty(&self, ty: Option<TraitType>) {
        *self.ty.borrow_mut() = ty;
    }

    /// Get the trait ID from the underlying TraitType, if available.
    pub fn trait_id(&self) -> Option<TraitDefinitionId> {
        self.ty().map(|t| t.trait_id)
    }

    /// Parse the trait type reference and convert to TraitType.
    /// `allow_bindings` should be true for trait bounds (e.g., `T: Foo[X=Int]`)
    /// and false for impl trait types (e.g., `impl Foo for Bar`).
    pub fn parse(
        &self,
        sa: &Sema,
        table: &ModuleSymTable,
        element: &dyn Element,
        allow_bindings: bool,
    ) {
        if let Some(type_ref_id) = self.type_ref_id {
            let type_refs = element.type_ref_arena();
            let file_id = element.file_id();
            parse_type_ref(sa, type_refs, table, file_id, element, type_ref_id);
            let ty = convert_trait_type_ref(sa, type_refs, element, type_ref_id, allow_bindings);
            self.set_ty(ty);
        }
    }

    /// Check the trait type parameters meet their bounds.
    pub fn check(&self, sa: &Sema, element: &dyn Element) {
        if let Some(type_ref_id) = self.type_ref_id {
            if let Some(trait_ty) = self.ty() {
                let type_refs = element.type_ref_arena();
                let new_ty = check_trait_type_ref(sa, type_refs, element, type_ref_id, trait_ty);
                self.set_ty(new_ty);
            }
        }
    }
}

pub fn parse_trait_type(
    sa: &Sema,
    table: &ModuleSymTable,
    element: &dyn Element,
    _allow_self: bool,
    parsed_ty: &ParsedTraitType,
) {
    parsed_ty.parse(sa, table, element, false);
}

pub fn parse_trait_bound_type(
    sa: &Sema,
    table: &ModuleSymTable,
    element: &dyn Element,
    _allow_self: bool,
    parsed_ty: &ParsedTraitType,
) {
    parsed_ty.parse(sa, table, element, true);
}

pub(crate) fn ty_for_sym(sa: &Sema, sym: SymbolKind, type_params: SourceTypeArray) -> SourceType {
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
                let trait_id = alias.parent.to_trait_id().expect("trait expected");
                let trait_ty = TraitType::from_trait_id(trait_id);
                SourceType::Assoc {
                    trait_ty,
                    assoc_id: id,
                }
            }
        }
        _ => unimplemented!(),
    }
}

pub fn check_trait_type(sa: &Sema, element: &dyn Element, parsed_ty: &ParsedTraitType) {
    parsed_ty.check(sa, element);
}

pub(crate) fn check_type_params(
    sa: &Sema,
    _callee_element: &dyn Element,
    callee_type_param_definition: &TypeParamDefinition,
    type_arguments: &[SourceType],
    ctxt_element: &dyn Element,
    span: impl Fn() -> Span,
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
                sa.report(
                    ctxt_element.file_id(),
                    span(),
                    &TYPE_NOT_IMPLEMENTING_TRAIT,
                    args!(name, trait_name),
                );
                success = false;
            }
        }
    }

    success
}

pub(crate) fn check_trait_type_param_definition(
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
            let trait_ty = specialize_trait_type(sa, trait_ty, &type_arguments);

            if !implements_trait(sa, tp_ty.clone(), element, trait_ty.clone()) {
                let name = tp_ty.name_with_type_params(sa, context_type_param_definition);
                let trait_name = trait_ty.name_with_type_params(sa, context_type_param_definition);
                sa.report(
                    file_id,
                    span,
                    &TYPE_NOT_IMPLEMENTING_TRAIT,
                    args!(name, trait_name),
                );
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
                    sa.report(
                        file_id,
                        span,
                        &TYPE_NOT_IMPLEMENTING_TRAIT,
                        args!(name, trait_name),
                    );
                    success = false;
                }
            }
        }
    }

    success
}

pub fn expand_parsed_trait_type(
    sa: &Sema,
    element: &dyn Element,
    parsed_ty: &ParsedTraitType,
    replace_self: Option<SourceType>,
) {
    if let Some(trait_ty) = parsed_ty.ty() {
        let new_trait_ty = expand_trait_ty(sa, element, &trait_ty, replace_self);
        parsed_ty.set_ty(Some(new_trait_ty));
    }
}

fn expand_trait_ty(
    sa: &Sema,
    element: &dyn Element,
    trait_ty: &TraitType,
    replace_self: Option<SourceType>,
) -> TraitType {
    let new_type_params = expand_sta(sa, element, &trait_ty.type_params, replace_self.clone());
    let new_bindings = trait_ty
        .bindings
        .iter()
        .map(|(id, ty)| {
            (
                *id,
                expand_st(sa, element, ty.clone(), replace_self.clone()),
            )
        })
        .collect::<Vec<_>>();
    TraitType {
        trait_id: trait_ty.trait_id,
        type_params: new_type_params,
        bindings: new_bindings,
    }
}

pub(crate) fn expand_st(
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

        SourceType::Assoc { assoc_id, .. } => {
            let element = parent_element_or_self(sa, element);
            if let Some(impl_) = element.to_impl() {
                let impl_alias_id = impl_.trait_alias_map().get(&assoc_id).cloned();
                if let Some(impl_alias_id) = impl_alias_id {
                    let impl_alias = sa.alias(impl_alias_id);
                    expand_st(sa, element, impl_alias.ty(), replace_self)
                } else {
                    SourceType::Error
                }
            } else if element.is_trait() {
                ty
            } else {
                unreachable!()
            }
        }

        SourceType::GenericAssoc {
            ty,
            trait_ty,
            assoc_id,
        } => {
            if let Some((_, resolved_ty)) = trait_ty.bindings.iter().find(|(x, _)| *x == *assoc_id)
            {
                expand_st(sa, element, resolved_ty.clone(), replace_self)
            } else {
                SourceType::GenericAssoc {
                    ty: Box::new(expand_st(
                        sa,
                        element,
                        ty.as_ref().clone(),
                        replace_self.clone(),
                    )),
                    trait_ty: expand_trait_ty(sa, element, trait_ty, replace_self),
                    assoc_id: *assoc_id,
                }
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
        | SourceType::TypeParam(..) => ty,
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
    use crate::args;
    use crate::error::diagnostics::{
        DUPLICATE_TYPE_BINDING, TYPE_BINDING_ORDER, UNEXPECTED_ASSOC, UNEXPECTED_TYPE_BINDING,
        UNKNOWN_ASSOC, UNKNOWN_TYPE_BINDING,
    };
    use crate::tests::*;

    #[test]
    fn class_type_with_named_type_arg() {
        err(
            "
            class Foo[T](T)
            fn f(x: Foo[T = Int64]) {}
        ",
            (3, 25),
            9,
            crate::ErrorLevel::Error,
            &UNEXPECTED_TYPE_BINDING,
            args!(),
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
            7,
            crate::ErrorLevel::Error,
            &TYPE_BINDING_ORDER,
            args!(),
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
            9,
            crate::ErrorLevel::Error,
            &UNKNOWN_TYPE_BINDING,
            args!(),
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
            9,
            crate::ErrorLevel::Error,
            &DUPLICATE_TYPE_BINDING,
            args!(),
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
            9,
            crate::ErrorLevel::Error,
            &UNEXPECTED_TYPE_BINDING,
            args!(),
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
            7,
            crate::ErrorLevel::Error,
            &DUPLICATE_TYPE_BINDING,
            args!(),
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
            5,
            crate::ErrorLevel::Error,
            &TYPE_BINDING_ORDER,
            args!(),
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
            (3, 29),
            7,
            crate::ErrorLevel::Error,
            &UNKNOWN_ASSOC,
            args!(),
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
            (4, 28),
            7,
            crate::ErrorLevel::Error,
            &UNEXPECTED_ASSOC,
            args!(),
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
            (4, 39),
            7,
            crate::ErrorLevel::Error,
            &UNKNOWN_ASSOC,
            args!(),
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
            (4, 27),
            7,
            crate::ErrorLevel::Error,
            &UNKNOWN_ASSOC,
            args!(),
        );
    }

    #[test]
    fn qualified_path_unknown_alias() {
        err(
            "
            trait Foo {}
            trait Bar: Foo {
                fn bar(): [Self as Foo]::Baz;
            }
        ",
            (4, 42),
            3,
            crate::ErrorLevel::Error,
            &UNKNOWN_ASSOC,
            args!(),
        );
    }

    #[test]
    fn qualified_path() {
        ok("
            trait Foo {
                type Baz;
            }
            trait Bar: Foo {
                fn bar(): [Self as Foo]::Baz;
            }
        ");
    }
}
