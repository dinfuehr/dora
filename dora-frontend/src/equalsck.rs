use dora_parser::ast::SyntaxNodeBase;

use crate::element_collector::Annotations;
use crate::error::diagnostics::TYPE_NOT_IMPLEMENTING_TRAIT;
use crate::sema::{
    ClassDefinitionId, DerivedEquals, Element, ElementId, EnumDefinitionId, FctDefinition,
    FctParent, FieldDefinitionId, ImplDefinition, Intrinsic, Param, Params, Sema,
    StructDefinitionId, TypeParamDefinition, TypeRefArena, implements_trait,
};
use crate::{SourceType, TraitType, args};

#[derive(Clone, Copy)]
enum EqualsTarget {
    Class(ClassDefinitionId),
    Struct(StructDefinitionId),
    Enum(EnumDefinitionId),
}

pub fn create_impls(sa: &mut Sema) {
    for target in targets(sa) {
        create_equals_impl(sa, target);
    }
}

pub fn check_fields(sa: &Sema) {
    for target in targets(sa) {
        check_equals_fields(sa, target);
    }
}

fn targets(sa: &Sema) -> Vec<EqualsTarget> {
    let mut targets = Vec::new();

    targets.extend(
        sa.classes
            .iter()
            .filter(|(_, class)| class.derive_equals)
            .map(|(id, _)| EqualsTarget::Class(id)),
    );
    targets.extend(
        sa.structs
            .iter()
            .filter(|(_, struct_)| struct_.derive_equals)
            .map(|(id, _)| EqualsTarget::Struct(id)),
    );
    targets.extend(
        sa.enums
            .iter()
            .filter(|(_, enum_)| enum_.derive_equals)
            .map(|(id, _)| EqualsTarget::Enum(id)),
    );

    targets
}

fn create_equals_impl(sa: &mut Sema, target: EqualsTarget) {
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
            target.annotation_span(sa),
            element.span(),
            element.type_param_definition_id(),
            target.ty(sa),
            target.is_simple_enum(sa),
        )
    };
    let equals_trait_id = sa.known.traits.equals();
    let trait_ty = TraitType::from_trait_id(equals_trait_id);

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
    let method_name = sa.interner.intern("equals");
    let mut modifiers = Annotations::default();
    modifiers.is_internal = true;
    modifiers.is_force_inline = is_simple_enum;
    let params = Params::new(
        vec![Param::new_ty(SourceType::This), Param::new_ty(extended_ty)],
        true,
        false,
    );
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
        SourceType::Bool,
        FctParent::Impl(impl_id),
        false,
    );
    method.derived_equals = target.derived_equals(sa);
    let method_id = sa.fcts.alloc(method);
    sa.fcts[method_id].id = Some(method_id);
    sa.fcts[method_id].set_type_refs(TypeRefArena::new());

    if is_simple_enum {
        assert!(sa.fcts[method_id].intrinsic.set(Intrinsic::EnumEq).is_ok());
    }
    assert!(sa.impls[impl_id].methods.set(vec![method_id]).is_ok());
    assert!(sa.impls[impl_id].aliases.set(Vec::new()).is_ok());
    assert!(
        sa.impls[impl_id]
            .children
            .set(vec![ElementId::Fct(method_id)])
            .is_ok()
    );
}

fn check_equals_fields(sa: &Sema, target: EqualsTarget) {
    let element = target.element(sa);
    let equals_trait_ty = TraitType::from_trait_id(sa.known.traits.equals());

    for field_id in target.field_ids(sa) {
        let field = sa.field(field_id);
        let field_ty = field.ty();

        if !field_ty.is_error()
            && !implements_trait(sa, field_ty.clone(), element, equals_trait_ty.clone())
        {
            sa.report(
                element.file_id(),
                field.span(),
                &TYPE_NOT_IMPLEMENTING_TRAIT,
                args!(
                    field_ty.name_with_type_params(sa, element.type_param_definition(sa)),
                    equals_trait_ty.name_with_type_params(sa, element.type_param_definition(sa))
                ),
            );
        }
    }
}

impl EqualsTarget {
    fn element(self, sa: &Sema) -> &dyn Element {
        match self {
            EqualsTarget::Class(id) => sa.class(id),
            EqualsTarget::Struct(id) => sa.struct_(id),
            EqualsTarget::Enum(id) => sa.enum_(id),
        }
    }

    fn ty(self, sa: &Sema) -> SourceType {
        let type_params = self
            .element(sa)
            .type_param_definition(sa)
            .identity_type_params(sa);

        match self {
            EqualsTarget::Class(id) => SourceType::Class(id, type_params),
            EqualsTarget::Struct(id) => SourceType::Struct(id, type_params),
            EqualsTarget::Enum(id) => SourceType::Enum(id, type_params),
        }
    }

    fn annotation_span(self, sa: &Sema) -> crate::Span {
        let modifier_list = match self {
            EqualsTarget::Class(id) => sa.class(id).ast(sa).modifier_list(),
            EqualsTarget::Struct(id) => sa.struct_(id).ast(sa).modifier_list(),
            EqualsTarget::Enum(id) => sa.enum_(id).ast(sa).modifier_list(),
        };

        modifier_list
            .and_then(|list| list.find_modifier("Equals"))
            .expect("missing Equals annotation")
            .span()
    }

    fn is_simple_enum(self, sa: &Sema) -> bool {
        match self {
            EqualsTarget::Enum(id) => sa.enum_(id).is_simple_enum(),
            EqualsTarget::Class(_) | EqualsTarget::Struct(_) => false,
        }
    }

    fn derived_equals(self, sa: &Sema) -> Option<DerivedEquals> {
        match self {
            EqualsTarget::Class(id) => Some(DerivedEquals::Class(id)),
            EqualsTarget::Struct(id) => Some(DerivedEquals::Struct(id)),
            EqualsTarget::Enum(id) if !sa.enum_(id).is_simple_enum() => {
                Some(DerivedEquals::Enum(id))
            }
            EqualsTarget::Enum(_) => None,
        }
    }

    fn field_ids(self, sa: &Sema) -> Vec<FieldDefinitionId> {
        match self {
            EqualsTarget::Class(id) => sa.class(id).field_ids().to_vec(),
            EqualsTarget::Struct(id) => sa.struct_(id).field_ids().to_vec(),
            EqualsTarget::Enum(id) => sa
                .enum_(id)
                .variant_ids()
                .iter()
                .flat_map(|&variant_id| sa.variant(variant_id).field_ids())
                .copied()
                .collect(),
        }
    }
}
