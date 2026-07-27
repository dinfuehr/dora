use dora_bytecode::{
    BytecodeBody, BytecodeType, BytecodeTypeArray, ConstPoolEntry, Location, Register,
};

use crate::program_emitter::Emitter;
use crate::sema::{
    ClassDefinitionId, Element, EnumDefinitionId, FctDefinition, FctDefinitionId, Sema,
    StructDefinitionId, find_impl,
};
use crate::{SourceType, TraitType};

use super::BytecodeBuilder;

pub fn generate_class_equals(
    sa: &Sema,
    emitter: &mut Emitter,
    fct: &FctDefinition,
    class_id: ClassDefinitionId,
) -> BytecodeBody {
    generate_aggregate_equals(sa, emitter, fct, AggregateEqualsTarget::Class(class_id))
}

pub fn generate_struct_equals(
    sa: &Sema,
    emitter: &mut Emitter,
    fct: &FctDefinition,
    struct_id: StructDefinitionId,
) -> BytecodeBody {
    generate_aggregate_equals(sa, emitter, fct, AggregateEqualsTarget::Struct(struct_id))
}

#[derive(Clone, Copy)]
enum AggregateEqualsTarget {
    Class(ClassDefinitionId),
    Struct(StructDefinitionId),
}

fn generate_aggregate_equals(
    sa: &Sema,
    emitter: &mut Emitter,
    fct: &FctDefinition,
    target: AggregateEqualsTarget,
) -> BytecodeBody {
    let element = target.element(sa);
    let type_params = element.type_param_definition(sa).identity_type_params(sa);
    let aggregate_ty = target.ty(type_params.clone());
    let bytecode_aggregate_ty = emitter.convert_ty(sa, aggregate_ty);
    let bytecode_type_params = emitter.convert_tya(sa, &type_params);
    let location = sa.compute_loc(fct.file_id, fct.span);

    let equals_trait_ty = TraitType::from_trait_id(sa.known.traits.equals());
    let equals_method_name = sa.interner.intern("equals");
    let equals_method_id = sa
        .trait_(equals_trait_ty.trait_id)
        .get_method(equals_method_name, false)
        .expect("Equals::equals missing");

    let mut builder = BytecodeBuilder::new();
    builder.push_scope();

    let lhs = builder.alloc_var(bytecode_aggregate_ty.clone());
    let rhs = builder.alloc_var(bytecode_aggregate_ty);
    let condition = builder.alloc_var(BytecodeType::Bool);
    let false_label = builder.create_label();

    for &field_id in target.field_ids(sa) {
        let field = sa.field(field_id);
        let field_ty = field.ty();
        let bytecode_field_ty = emitter.convert_ty(sa, field_ty.clone());
        let lhs_field = builder.alloc_temp(bytecode_field_ty.clone());
        let rhs_field = builder.alloc_temp(bytecode_field_ty);
        let field_idx = target.add_field_const(
            sa,
            emitter,
            &mut builder,
            bytecode_type_params.clone(),
            field.index.0.try_into().expect("field index overflow"),
        );

        builder.emit_load_field(lhs_field, lhs, field_idx, location);
        builder.emit_load_field(rhs_field, rhs, field_idx, location);
        emit_equals_call(
            sa,
            emitter,
            &mut builder,
            element,
            &equals_trait_ty,
            equals_method_id,
            field_ty,
            condition,
            lhs_field,
            rhs_field,
            location,
        );
        builder.emit_jump_if_false(condition, false_label);
        builder.free_temp(rhs_field);
        builder.free_temp(lhs_field);
    }

    builder.emit_const_true(condition);
    builder.emit_ret(condition);
    builder.bind_label(false_label);
    builder.emit_const_false(condition);
    builder.emit_ret(condition);
    builder.pop_scope();
    builder.generate()
}

impl AggregateEqualsTarget {
    fn element(self, sa: &Sema) -> &dyn Element {
        match self {
            AggregateEqualsTarget::Class(id) => sa.class(id),
            AggregateEqualsTarget::Struct(id) => sa.struct_(id),
        }
    }

    fn ty(self, type_params: crate::SourceTypeArray) -> SourceType {
        match self {
            AggregateEqualsTarget::Class(id) => SourceType::Class(id, type_params),
            AggregateEqualsTarget::Struct(id) => SourceType::Struct(id, type_params),
        }
    }

    fn field_ids(self, sa: &Sema) -> &[crate::sema::FieldDefinitionId] {
        match self {
            AggregateEqualsTarget::Class(id) => sa.class(id).field_ids(),
            AggregateEqualsTarget::Struct(id) => sa.struct_(id).field_ids(),
        }
    }

    fn add_field_const(
        self,
        sa: &Sema,
        emitter: &mut Emitter,
        builder: &mut BytecodeBuilder,
        type_params: BytecodeTypeArray,
        field_index: u32,
    ) -> dora_bytecode::ConstPoolIdx {
        match self {
            AggregateEqualsTarget::Class(id) => builder.add_const_field_types(
                emitter.convert_class_id(sa, id),
                type_params,
                field_index,
            ),
            AggregateEqualsTarget::Struct(id) => builder.add_const_struct_field(
                emitter.convert_struct_id(sa, id),
                type_params,
                field_index,
            ),
        }
    }
}

pub fn generate_enum_equals(
    sa: &Sema,
    emitter: &mut Emitter,
    fct: &FctDefinition,
    enum_id: EnumDefinitionId,
) -> BytecodeBody {
    let enum_ = sa.enum_(enum_id);
    assert!(!enum_.is_simple_enum());

    let type_params = enum_.type_param_definition(sa).identity_type_params(sa);
    let enum_ty = SourceType::Enum(enum_id, type_params.clone());
    let bytecode_enum_ty = emitter.convert_ty(sa, enum_ty);
    let bytecode_enum_id = emitter.convert_enum_id(sa, enum_id);
    let bytecode_type_params = emitter.convert_tya(sa, &type_params);
    let enum_const = ConstPoolEntry::Enum(bytecode_enum_id, bytecode_type_params.clone());
    let location = sa.compute_loc(fct.file_id, fct.span);

    let equals_trait_ty = TraitType::from_trait_id(sa.known.traits.equals());
    let equals_method_name = sa.interner.intern("equals");
    let equals_method_id = sa
        .trait_(equals_trait_ty.trait_id)
        .get_method(equals_method_name, false)
        .expect("Equals::equals missing");

    let mut builder = BytecodeBuilder::new();
    builder.push_scope();

    let lhs = builder.alloc_var(bytecode_enum_ty.clone());
    let rhs = builder.alloc_var(bytecode_enum_ty);
    let lhs_variant = builder.alloc_var(BytecodeType::Int32);
    let rhs_variant = builder.alloc_var(BytecodeType::Int32);
    let condition = builder.alloc_var(BytecodeType::Bool);
    let false_label = builder.create_label();
    let variant_labels = enum_
        .variant_ids()
        .iter()
        .map(|_| builder.create_label())
        .collect::<Vec<_>>();
    debug_assert!(
        enum_
            .variant_ids()
            .iter()
            .enumerate()
            .all(|(index, &variant_id)| sa.variant(variant_id).index as usize == index)
    );

    let enum_idx = builder.add_const(enum_const);
    builder.emit_load_enum_variant(lhs_variant, lhs, enum_idx, location);
    builder.emit_load_enum_variant(rhs_variant, rhs, enum_idx, location);
    builder.emit_test_eq(condition, lhs_variant, rhs_variant);
    builder.emit_jump_if_false(condition, false_label);
    let jump_table_idx = builder.add_const_jump_table(variant_labels.clone(), false_label);
    builder.emit_switch(lhs_variant, jump_table_idx);

    for (&variant_id, variant_label) in enum_.variant_ids().iter().zip(variant_labels) {
        let variant = sa.variant(variant_id);
        builder.bind_label(variant_label);

        for (field_index, &field_id) in variant.field_ids().iter().enumerate() {
            let field_ty = sa.field(field_id).ty();
            let bytecode_field_ty = emitter.convert_ty(sa, field_ty.clone());
            let lhs_field = builder.alloc_temp(bytecode_field_ty.clone());
            let rhs_field = builder.alloc_temp(bytecode_field_ty);
            let field_idx = builder.add_const_enum_element(
                bytecode_enum_id,
                bytecode_type_params.clone(),
                variant.index,
                field_index.try_into().expect("enum field index overflow"),
            );

            builder.emit_load_enum_element(lhs_field, lhs, field_idx, location);
            builder.emit_load_enum_element(rhs_field, rhs, field_idx, location);
            emit_equals_call(
                sa,
                emitter,
                &mut builder,
                enum_,
                &equals_trait_ty,
                equals_method_id,
                field_ty,
                condition,
                lhs_field,
                rhs_field,
                location,
            );
            builder.emit_jump_if_false(condition, false_label);
            builder.free_temp(rhs_field);
            builder.free_temp(lhs_field);
        }

        builder.emit_const_true(condition);
        builder.emit_ret(condition);
    }

    builder.bind_label(false_label);
    builder.emit_const_false(condition);
    builder.emit_ret(condition);
    builder.pop_scope();
    builder.generate()
}

#[allow(clippy::too_many_arguments)]
fn emit_equals_call(
    sa: &Sema,
    emitter: &mut Emitter,
    builder: &mut BytecodeBuilder,
    element: &dyn Element,
    equals_trait_ty: &TraitType,
    equals_method_id: FctDefinitionId,
    field_ty: SourceType,
    condition: Register,
    lhs: Register,
    rhs: Register,
    location: Location,
) {
    let is_generic = field_ty.is_type_param() || field_ty.is_assoc() || field_ty.is_generic_assoc();
    let callee_idx = if is_generic {
        builder.add_const(ConstPoolEntry::Generic {
            object_type: emitter.convert_ty(sa, field_ty),
            trait_ty: emitter.convert_trait_ty(sa, equals_trait_ty),
            fct_id: emitter.convert_function_id(sa, equals_method_id),
            fct_type_params: BytecodeTypeArray::empty(),
        })
    } else {
        let impl_match = find_impl(
            sa,
            element,
            field_ty,
            element.type_param_definition(sa),
            equals_trait_ty.clone(),
        )
        .expect("field should implement Equals");
        let method_id = sa
            .impl_(impl_match.id)
            .get_method_for_trait_method_id(equals_method_id)
            .expect("Equals implementation missing equals method");
        builder.add_const(ConstPoolEntry::Fct(
            emitter.convert_function_id(sa, method_id),
            emitter.convert_tya(sa, &impl_match.bindings),
        ))
    };

    if is_generic {
        builder.emit_invoke_generic_direct(condition, callee_idx, &[lhs, rhs], location);
    } else {
        builder.emit_invoke_direct(condition, callee_idx, &[lhs, rhs], location);
    }
}
