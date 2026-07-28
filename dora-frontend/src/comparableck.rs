use crate::element_collector::Annotations;
use crate::error::diagnostics::TYPE_NOT_IMPLEMENTING_TRAIT;
use crate::sema::{
    DerivedMethod, DerivedTarget, Element, ElementId, FctDefinition, FctParent, ImplDefinition,
    Param, Params, Sema, TypeParamDefinition, TypeRefArena, implements_trait,
};
use crate::{SourceType, SourceTypeArray, TraitType, args};

pub fn create_impls(sa: &mut Sema) {
    for target in targets(sa) {
        create_comparable_impl(sa, target);
    }
}

pub fn check_fields(sa: &Sema) {
    for target in targets(sa) {
        check_comparable_fields(sa, target);
    }
}

fn targets(sa: &Sema) -> Vec<DerivedTarget> {
    let mut targets = Vec::new();

    targets.extend(
        sa.classes
            .iter()
            .filter(|(_, class)| class.derive_comparable)
            .map(|(id, _)| DerivedTarget::Class(id)),
    );
    targets.extend(
        sa.structs
            .iter()
            .filter(|(_, struct_)| struct_.derive_comparable)
            .map(|(id, _)| DerivedTarget::Struct(id)),
    );
    targets.extend(
        sa.enums
            .iter()
            .filter(|(_, enum_)| enum_.derive_comparable)
            .map(|(id, _)| DerivedTarget::Enum(id)),
    );

    targets
}

fn create_comparable_impl(sa: &mut Sema, target: DerivedTarget) {
    let (
        package_id,
        module_id,
        file_id,
        declaration_span,
        span,
        type_param_definition_id,
        extended_ty,
        is_simple_enum,
    ) = {
        let element = target.element(sa);
        (
            element.package_id(),
            element.module_id(),
            element.file_id(),
            target.annotation_span(sa, "Comparable"),
            element.span(),
            element.type_param_definition_id(),
            target.ty(sa),
            target.is_simple_enum(sa),
        )
    };
    let comparable_trait_id = sa.known.traits.comparable();
    let trait_ty = TraitType::from_trait_id(comparable_trait_id);

    let impl_ = ImplDefinition::new_synthetic(
        package_id,
        module_id,
        file_id,
        declaration_span,
        span,
        type_param_definition_id,
        trait_ty,
        extended_ty.clone(),
        false,
    );
    let impl_id = sa.impls.alloc(impl_);
    assert!(sa.impls[impl_id].id.set(impl_id).is_ok());
    sa.impls[impl_id].set_type_refs(TypeRefArena::new());

    let method_type_param_definition = TypeParamDefinition::new(sa, Some(type_param_definition_id));
    let method_type_param_definition_id = sa
        .type_param_definitions
        .alloc(method_type_param_definition);
    let method_name = sa.interner.intern("cmp");
    let mut modifiers = Annotations::default();
    modifiers.is_internal = true;
    modifiers.is_force_inline = is_simple_enum;
    let params = Params::new(
        vec![Param::new_ty(SourceType::This), Param::new_ty(extended_ty)],
        true,
        false,
    );
    let return_type = SourceType::Enum(sa.known.enums.ordering(), SourceTypeArray::empty());
    let mut method = FctDefinition::new_no_source(
        package_id,
        module_id,
        file_id,
        declaration_span,
        span,
        None,
        modifiers,
        method_name,
        method_type_param_definition_id,
        params,
        return_type,
        FctParent::Impl(impl_id),
        false,
    );
    method.derived_method = Some(DerivedMethod::Comparable(target));
    let method_id = sa.fcts.alloc(method);
    sa.fcts[method_id].id = Some(method_id);
    sa.fcts[method_id].set_type_refs(TypeRefArena::new());

    assert!(sa.impls[impl_id].methods.set(vec![method_id]).is_ok());
    assert!(sa.impls[impl_id].aliases.set(Vec::new()).is_ok());
    assert!(
        sa.impls[impl_id]
            .children
            .set(vec![ElementId::Fct(method_id)])
            .is_ok()
    );
}

fn check_comparable_fields(sa: &Sema, target: DerivedTarget) {
    let element = target.element(sa);
    let comparable_trait_ty = TraitType::from_trait_id(sa.known.traits.comparable());

    for field_id in target.field_ids(sa) {
        let field = sa.field(field_id);
        let field_ty = field.ty();

        if !field_ty.is_error()
            && !implements_trait(sa, field_ty.clone(), element, comparable_trait_ty.clone())
        {
            sa.report(
                element.file_id(),
                field.span(),
                &TYPE_NOT_IMPLEMENTING_TRAIT,
                args!(
                    field_ty.name_with_type_params(sa, element.type_param_definition(sa)),
                    comparable_trait_ty
                        .name_with_type_params(sa, element.type_param_definition(sa))
                ),
            );
        }
    }
}
