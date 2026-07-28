use dora_bytecode::{BytecodeBody, BytecodeType, BytecodeTypeArray, ConstPoolEntry};

use crate::program_emitter::Emitter;
use crate::sema::{
    ClassDefinitionId, Element, EnumDefinitionId, FctDefinition, Sema, StructDefinitionId,
};
use crate::{SourceType, TraitType};

use super::{BytecodeBuilder, emit_trait_method_call};

pub fn generate_class_hash(
    sa: &Sema,
    emitter: &mut Emitter,
    fct: &FctDefinition,
    class_id: ClassDefinitionId,
) -> BytecodeBody {
    generate_aggregate_hash(sa, emitter, fct, AggregateHashTarget::Class(class_id))
}

pub fn generate_struct_hash(
    sa: &Sema,
    emitter: &mut Emitter,
    fct: &FctDefinition,
    struct_id: StructDefinitionId,
) -> BytecodeBody {
    generate_aggregate_hash(sa, emitter, fct, AggregateHashTarget::Struct(struct_id))
}

#[derive(Clone, Copy)]
enum AggregateHashTarget {
    Class(ClassDefinitionId),
    Struct(StructDefinitionId),
}

fn generate_aggregate_hash(
    sa: &Sema,
    emitter: &mut Emitter,
    fct: &FctDefinition,
    target: AggregateHashTarget,
) -> BytecodeBody {
    let element = target.element(sa);
    let type_params = element.type_param_definition(sa).identity_type_params(sa);
    let aggregate_ty = target.ty(type_params.clone());
    let bytecode_aggregate_ty = emitter.convert_ty(sa, aggregate_ty);
    let bytecode_type_params = emitter.convert_tya(sa, &type_params);
    let location = sa.compute_loc(fct.file_id, fct.span);

    let hash_trait_ty = TraitType::from_trait_id(sa.known.traits.hash());
    let hash_method_name = sa.interner.intern("hash");
    let hash_method_id = sa
        .trait_(hash_trait_ty.trait_id)
        .get_method(hash_method_name, false)
        .expect("Hash::hash missing");

    let mut builder = BytecodeBuilder::new();
    builder.push_scope();

    let value = builder.alloc_var(bytecode_aggregate_ty);
    let result = builder.alloc_var(BytecodeType::Int32);
    builder.emit_const_int32(result, 0);

    for &field_id in target.field_ids(sa) {
        let field = sa.field(field_id);
        let field_ty = field.ty();
        let bytecode_field_ty = emitter.convert_ty(sa, field_ty.clone());
        let field_value = builder.alloc_temp(bytecode_field_ty);
        let field_hash = builder.alloc_temp(BytecodeType::Int32);
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
            &hash_trait_ty,
            hash_method_id,
            field_ty,
            field_hash,
            &[field_value],
            location,
        );
        builder.emit_xor(result, result, field_hash);
        builder.free_temp(field_hash);
        builder.free_temp(field_value);
    }

    builder.emit_ret(result);
    builder.pop_scope();
    builder.generate()
}

impl AggregateHashTarget {
    fn element(self, sa: &Sema) -> &dyn Element {
        match self {
            AggregateHashTarget::Class(id) => sa.class(id),
            AggregateHashTarget::Struct(id) => sa.struct_(id),
        }
    }

    fn ty(self, type_params: crate::SourceTypeArray) -> SourceType {
        match self {
            AggregateHashTarget::Class(id) => SourceType::Class(id, type_params),
            AggregateHashTarget::Struct(id) => SourceType::Struct(id, type_params),
        }
    }

    fn field_ids(self, sa: &Sema) -> &[crate::sema::FieldDefinitionId] {
        match self {
            AggregateHashTarget::Class(id) => sa.class(id).field_ids(),
            AggregateHashTarget::Struct(id) => sa.struct_(id).field_ids(),
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
            AggregateHashTarget::Class(id) => builder.add_const_field_types(
                emitter.convert_class_id(sa, id),
                type_params,
                field_index,
            ),
            AggregateHashTarget::Struct(id) => builder.add_const_struct_field(
                emitter.convert_struct_id(sa, id),
                type_params,
                field_index,
            ),
        }
    }
}

pub fn generate_enum_hash(
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
    let enum_const = ConstPoolEntry::Enum(bytecode_enum_id, bytecode_type_params.clone());
    let location = sa.compute_loc(fct.file_id, fct.span);

    let hash_trait_ty = TraitType::from_trait_id(sa.known.traits.hash());
    let hash_method_name = sa.interner.intern("hash");
    let hash_method_id = sa
        .trait_(hash_trait_ty.trait_id)
        .get_method(hash_method_name, false)
        .expect("Hash::hash missing");

    let mut builder = BytecodeBuilder::new();
    builder.push_scope();

    let value = builder.alloc_var(bytecode_enum_ty);
    let variant = builder.alloc_var(BytecodeType::Int32);
    let result = builder.alloc_var(BytecodeType::Int32);
    let default_label = builder.create_label();
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
    builder.emit_load_enum_variant(variant, value, enum_idx, location);
    builder.emit_mov(result, variant);
    let jump_table_idx = builder.add_const_jump_table(variant_labels.clone(), default_label);
    builder.emit_switch(variant, jump_table_idx);

    for (&variant_id, variant_label) in enum_.variant_ids().iter().zip(variant_labels) {
        let variant = sa.variant(variant_id);
        builder.bind_label(variant_label);

        for (field_index, &field_id) in variant.field_ids().iter().enumerate() {
            let field_ty = sa.field(field_id).ty();
            let bytecode_field_ty = emitter.convert_ty(sa, field_ty.clone());
            let field_value = builder.alloc_temp(bytecode_field_ty);
            let field_hash = builder.alloc_temp(BytecodeType::Int32);
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
                &hash_trait_ty,
                hash_method_id,
                field_ty,
                field_hash,
                &[field_value],
                location,
            );
            builder.emit_xor(result, result, field_hash);
            builder.free_temp(field_hash);
            builder.free_temp(field_value);
        }

        builder.emit_ret(result);
    }

    builder.bind_label(default_label);
    builder.emit_ret(result);
    builder.pop_scope();
    builder.generate()
}
