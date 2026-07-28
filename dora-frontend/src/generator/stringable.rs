use dora_bytecode::{BytecodeBody, BytecodeType, BytecodeTypeArray, Register};

use crate::program_emitter::Emitter;
use crate::sema::{
    ClassDefinitionId, Element, EnumDefinitionId, FctDefinition, Sema, StructDefinitionId,
};
use crate::{SourceType, SourceTypeArray, TraitType};

use super::{BytecodeBuilder, emit_trait_method_call};

pub fn generate_class_stringable(
    sa: &Sema,
    emitter: &mut Emitter,
    fct: &FctDefinition,
    class_id: ClassDefinitionId,
) -> BytecodeBody {
    generate_aggregate_stringable(sa, emitter, fct, AggregateStringableTarget::Class(class_id))
}

pub fn generate_struct_stringable(
    sa: &Sema,
    emitter: &mut Emitter,
    fct: &FctDefinition,
    struct_id: StructDefinitionId,
) -> BytecodeBody {
    generate_aggregate_stringable(
        sa,
        emitter,
        fct,
        AggregateStringableTarget::Struct(struct_id),
    )
}

#[derive(Clone, Copy)]
enum AggregateStringableTarget {
    Class(ClassDefinitionId),
    Struct(StructDefinitionId),
}

fn generate_aggregate_stringable(
    sa: &Sema,
    emitter: &mut Emitter,
    fct: &FctDefinition,
    target: AggregateStringableTarget,
) -> BytecodeBody {
    let element = target.element(sa);
    let type_params = element.type_param_definition(sa).identity_type_params(sa);
    let aggregate_ty = target.ty(type_params.clone());
    let bytecode_aggregate_ty = emitter.convert_ty(sa, aggregate_ty);
    let bytecode_type_params = emitter.convert_tya(sa, &type_params);
    let string_ty = bytecode_string_ty(sa, emitter);
    let buffer_ty = bytecode_string_buffer_ty(sa, emitter);
    let location = sa.compute_loc(fct.file_id, fct.span);

    let stringable_trait_ty = TraitType::from_trait_id(sa.known.traits.stringable());
    let to_string_name = sa.interner.intern("to_string");
    let to_string_method_id = sa
        .trait_(stringable_trait_ty.trait_id)
        .get_method(to_string_name, false)
        .expect("Stringable::to_string missing");

    let mut builder = BytecodeBuilder::new();
    builder.push_scope();

    let value = builder.alloc_var(bytecode_aggregate_ty);
    let result = builder.alloc_var(string_ty.clone());
    let buffer = builder.alloc_var(buffer_ty);
    let part = builder.alloc_var(string_ty);
    let unit = builder.alloc_var(BytecodeType::Unit);

    emit_new_buffer(sa, emitter, &mut builder, buffer, location);
    emit_append_literal(
        sa,
        emitter,
        &mut builder,
        buffer,
        part,
        unit,
        format!("{}(", target.name(sa)),
        location,
    );

    for (field_index, &field_id) in target.field_ids(sa).iter().enumerate() {
        if field_index > 0 {
            emit_append_literal(
                sa,
                emitter,
                &mut builder,
                buffer,
                part,
                unit,
                ", ".to_string(),
                location,
            );
        }

        let field = sa.field(field_id);
        let field_ty = field.ty();
        let bytecode_field_ty = emitter.convert_ty(sa, field_ty.clone());
        let field_value = builder.alloc_temp(bytecode_field_ty);
        let field_idx = target.add_field_const(
            sa,
            emitter,
            &mut builder,
            bytecode_type_params.clone(),
            field.index.0.try_into().expect("field index overflow"),
        );

        builder.emit_load_field(field_value, value, field_idx, location);
        emit_trait_method_call(
            sa,
            emitter,
            &mut builder,
            element,
            &stringable_trait_ty,
            to_string_method_id,
            field_ty,
            part,
            &[field_value],
            location,
        );
        emit_append_part(sa, emitter, &mut builder, buffer, part, unit, location);
        builder.free_temp(field_value);
    }

    emit_append_literal(
        sa,
        emitter,
        &mut builder,
        buffer,
        part,
        unit,
        ")".to_string(),
        location,
    );
    emit_buffer_to_string(sa, emitter, &mut builder, result, buffer, location);
    builder.emit_ret(result);
    builder.pop_scope();
    builder.generate()
}

impl AggregateStringableTarget {
    fn element(self, sa: &Sema) -> &dyn Element {
        match self {
            AggregateStringableTarget::Class(id) => sa.class(id),
            AggregateStringableTarget::Struct(id) => sa.struct_(id),
        }
    }

    fn ty(self, type_params: SourceTypeArray) -> SourceType {
        match self {
            AggregateStringableTarget::Class(id) => SourceType::Class(id, type_params),
            AggregateStringableTarget::Struct(id) => SourceType::Struct(id, type_params),
        }
    }

    fn name(self, sa: &Sema) -> String {
        let name = match self {
            AggregateStringableTarget::Class(id) => sa.class(id).name,
            AggregateStringableTarget::Struct(id) => sa.struct_(id).name,
        };

        sa.interner.str(name).to_string()
    }

    fn field_ids(self, sa: &Sema) -> &[crate::sema::FieldDefinitionId] {
        match self {
            AggregateStringableTarget::Class(id) => sa.class(id).field_ids(),
            AggregateStringableTarget::Struct(id) => sa.struct_(id).field_ids(),
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
            AggregateStringableTarget::Class(id) => builder.add_const_field_types(
                emitter.convert_class_id(sa, id),
                type_params,
                field_index,
            ),
            AggregateStringableTarget::Struct(id) => builder.add_const_struct_field(
                emitter.convert_struct_id(sa, id),
                type_params,
                field_index,
            ),
        }
    }
}

pub fn generate_enum_stringable(
    sa: &Sema,
    emitter: &mut Emitter,
    fct: &FctDefinition,
    enum_id: EnumDefinitionId,
) -> BytecodeBody {
    let enum_ = sa.enum_(enum_id);
    let type_params = enum_.type_param_definition(sa).identity_type_params(sa);
    let enum_ty = SourceType::Enum(enum_id, type_params.clone());
    let bytecode_enum_ty = emitter.convert_ty(sa, enum_ty);
    let bytecode_enum_id = emitter.convert_enum_id(sa, enum_id);
    let bytecode_type_params = emitter.convert_tya(sa, &type_params);
    let string_ty = bytecode_string_ty(sa, emitter);
    let buffer_ty = bytecode_string_buffer_ty(sa, emitter);
    let location = sa.compute_loc(fct.file_id, fct.span);

    let stringable_trait_ty = TraitType::from_trait_id(sa.known.traits.stringable());
    let to_string_name = sa.interner.intern("to_string");
    let to_string_method_id = sa
        .trait_(stringable_trait_ty.trait_id)
        .get_method(to_string_name, false)
        .expect("Stringable::to_string missing");

    let mut builder = BytecodeBuilder::new();
    builder.push_scope();

    let value = builder.alloc_var(bytecode_enum_ty);
    let result = builder.alloc_var(string_ty.clone());
    let buffer = builder.alloc_var(buffer_ty);
    let part = builder.alloc_var(string_ty);
    let unit = builder.alloc_var(BytecodeType::Unit);
    let variant_index = builder.alloc_var(BytecodeType::Int32);
    let finish_label = builder.create_label();
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

    emit_new_buffer(sa, emitter, &mut builder, buffer, location);
    let enum_idx = builder.add_const_enum(bytecode_enum_id, bytecode_type_params.clone());
    builder.emit_load_enum_variant(variant_index, value, enum_idx, location);
    let jump_table_idx = builder.add_const_jump_table(variant_labels.clone(), finish_label);
    builder.emit_switch(variant_index, jump_table_idx);

    for (&variant_id, variant_label) in enum_.variant_ids().iter().zip(variant_labels) {
        let variant = sa.variant(variant_id);
        builder.bind_label(variant_label);

        let variant_name = sa.interner.str(variant.name);
        let has_fields = !variant.field_ids().is_empty();
        emit_append_literal(
            sa,
            emitter,
            &mut builder,
            buffer,
            part,
            unit,
            if has_fields {
                format!("{variant_name}(")
            } else {
                variant_name.to_string()
            },
            location,
        );

        for (field_index, &field_id) in variant.field_ids().iter().enumerate() {
            if field_index > 0 {
                emit_append_literal(
                    sa,
                    emitter,
                    &mut builder,
                    buffer,
                    part,
                    unit,
                    ", ".to_string(),
                    location,
                );
            }

            let field_ty = sa.field(field_id).ty();
            let bytecode_field_ty = emitter.convert_ty(sa, field_ty.clone());
            let field_value = builder.alloc_temp(bytecode_field_ty);
            let field_idx = builder.add_const_enum_element(
                bytecode_enum_id,
                bytecode_type_params.clone(),
                variant.index,
                field_index.try_into().expect("enum field index overflow"),
            );

            builder.emit_load_enum_element(field_value, value, field_idx, location);
            emit_trait_method_call(
                sa,
                emitter,
                &mut builder,
                enum_,
                &stringable_trait_ty,
                to_string_method_id,
                field_ty,
                part,
                &[field_value],
                location,
            );
            emit_append_part(sa, emitter, &mut builder, buffer, part, unit, location);
            builder.free_temp(field_value);
        }

        if has_fields {
            emit_append_literal(
                sa,
                emitter,
                &mut builder,
                buffer,
                part,
                unit,
                ")".to_string(),
                location,
            );
        }
        builder.emit_jump(finish_label);
    }

    builder.bind_label(finish_label);
    emit_buffer_to_string(sa, emitter, &mut builder, result, buffer, location);
    builder.emit_ret(result);
    builder.pop_scope();
    builder.generate()
}

fn bytecode_string_ty(sa: &Sema, emitter: &mut Emitter) -> BytecodeType {
    emitter.convert_ty(
        sa,
        SourceType::Class(sa.known.classes.string(), SourceTypeArray::empty()),
    )
}

fn bytecode_string_buffer_ty(sa: &Sema, emitter: &mut Emitter) -> BytecodeType {
    emitter.convert_ty(
        sa,
        SourceType::Class(sa.known.classes.string_buffer(), SourceTypeArray::empty()),
    )
}

fn emit_new_buffer(
    sa: &Sema,
    emitter: &mut Emitter,
    builder: &mut BytecodeBuilder,
    buffer: Register,
    location: dora_bytecode::Location,
) {
    let fct_id = sa.known.functions.string_buffer_empty();
    let fct_idx = builder.add_const_fct(emitter.convert_function_id(sa, fct_id));
    builder.emit_invoke_static(buffer, fct_idx, &[], location);
}

#[allow(clippy::too_many_arguments)]
fn emit_append_literal(
    sa: &Sema,
    emitter: &mut Emitter,
    builder: &mut BytecodeBuilder,
    buffer: Register,
    part: Register,
    unit: Register,
    value: String,
    location: dora_bytecode::Location,
) {
    builder.emit_const_string(part, value);
    emit_append_part(sa, emitter, builder, buffer, part, unit, location);
}

#[allow(clippy::too_many_arguments)]
fn emit_append_part(
    sa: &Sema,
    emitter: &mut Emitter,
    builder: &mut BytecodeBuilder,
    buffer: Register,
    part: Register,
    unit: Register,
    location: dora_bytecode::Location,
) {
    let fct_id = sa.known.functions.string_buffer_append();
    let fct_idx = builder.add_const_fct(emitter.convert_function_id(sa, fct_id));
    builder.emit_invoke_direct(unit, fct_idx, &[buffer, part], location);
}

fn emit_buffer_to_string(
    sa: &Sema,
    emitter: &mut Emitter,
    builder: &mut BytecodeBuilder,
    result: Register,
    buffer: Register,
    location: dora_bytecode::Location,
) {
    let fct_id = sa.known.functions.string_buffer_to_string();
    let fct_idx = builder.add_const_fct(emitter.convert_function_id(sa, fct_id));
    builder.emit_invoke_direct(result, fct_idx, &[buffer], location);
}
