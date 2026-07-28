use dora_bytecode::{BytecodeBody, BytecodeType, BytecodeTypeArray, EnumId};

use crate::program_emitter::Emitter;
use crate::sema::{
    ClassDefinitionId, Element, EnumDefinitionId, FctDefinition, Sema, StructDefinitionId,
};
use crate::{SourceType, SourceTypeArray, TraitType};

use super::{BytecodeBuilder, emit_trait_method_call};

pub fn generate_class_comparable(
    sa: &Sema,
    emitter: &mut Emitter,
    fct: &FctDefinition,
    class_id: ClassDefinitionId,
) -> BytecodeBody {
    generate_aggregate_comparable(sa, emitter, fct, AggregateComparableTarget::Class(class_id))
}

pub fn generate_struct_comparable(
    sa: &Sema,
    emitter: &mut Emitter,
    fct: &FctDefinition,
    struct_id: StructDefinitionId,
) -> BytecodeBody {
    generate_aggregate_comparable(
        sa,
        emitter,
        fct,
        AggregateComparableTarget::Struct(struct_id),
    )
}

#[derive(Clone, Copy)]
enum AggregateComparableTarget {
    Class(ClassDefinitionId),
    Struct(StructDefinitionId),
}

fn generate_aggregate_comparable(
    sa: &Sema,
    emitter: &mut Emitter,
    fct: &FctDefinition,
    target: AggregateComparableTarget,
) -> BytecodeBody {
    let element = target.element(sa);
    let type_params = element.type_param_definition(sa).identity_type_params(sa);
    let aggregate_ty = target.ty(type_params.clone());
    let bytecode_aggregate_ty = emitter.convert_ty(sa, aggregate_ty);
    let bytecode_type_params = emitter.convert_tya(sa, &type_params);
    let ordering = OrderingInfo::new(sa, emitter);
    let location = sa.compute_loc(fct.file_id, fct.span);

    let comparable_trait_ty = TraitType::from_trait_id(sa.known.traits.comparable());
    let comparable_method_name = sa.interner.intern("cmp");
    let comparable_method_id = sa
        .trait_(comparable_trait_ty.trait_id)
        .get_method(comparable_method_name, false)
        .expect("Comparable::cmp missing");

    let mut builder = BytecodeBuilder::new();
    builder.push_scope();

    let lhs = builder.alloc_var(bytecode_aggregate_ty.clone());
    let rhs = builder.alloc_var(bytecode_aggregate_ty);
    let result = builder.alloc_var(ordering.ty.clone());
    let result_variant = builder.alloc_var(BytecodeType::Int32);
    let equal_variant = builder.alloc_var(BytecodeType::Int32);
    let condition = builder.alloc_var(BytecodeType::Bool);
    let return_label = builder.create_label();
    let ordering_enum_idx = builder.add_const_enum(ordering.enum_id, BytecodeTypeArray::empty());
    builder.emit_const_int32(
        equal_variant,
        ordering.equal.try_into().expect("variant index overflow"),
    );

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
        emit_trait_method_call(
            sa,
            emitter,
            &mut builder,
            element,
            &comparable_trait_ty,
            comparable_method_id,
            field_ty,
            result,
            &[lhs_field, rhs_field],
            location,
        );
        builder.emit_load_enum_variant(result_variant, result, ordering_enum_idx, location);
        builder.emit_test_ne(condition, result_variant, equal_variant);
        builder.emit_jump_if_true(condition, return_label);
        builder.free_temp(rhs_field);
        builder.free_temp(lhs_field);
    }

    ordering.emit_variant(&mut builder, result, ordering.equal, location);
    builder.bind_label(return_label);
    builder.emit_ret(result);
    builder.pop_scope();
    builder.generate()
}

impl AggregateComparableTarget {
    fn element(self, sa: &Sema) -> &dyn Element {
        match self {
            AggregateComparableTarget::Class(id) => sa.class(id),
            AggregateComparableTarget::Struct(id) => sa.struct_(id),
        }
    }

    fn ty(self, type_params: SourceTypeArray) -> SourceType {
        match self {
            AggregateComparableTarget::Class(id) => SourceType::Class(id, type_params),
            AggregateComparableTarget::Struct(id) => SourceType::Struct(id, type_params),
        }
    }

    fn field_ids(self, sa: &Sema) -> &[crate::sema::FieldDefinitionId] {
        match self {
            AggregateComparableTarget::Class(id) => sa.class(id).field_ids(),
            AggregateComparableTarget::Struct(id) => sa.struct_(id).field_ids(),
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
            AggregateComparableTarget::Class(id) => builder.add_const_field_types(
                emitter.convert_class_id(sa, id),
                type_params,
                field_index,
            ),
            AggregateComparableTarget::Struct(id) => builder.add_const_struct_field(
                emitter.convert_struct_id(sa, id),
                type_params,
                field_index,
            ),
        }
    }
}

pub fn generate_enum_comparable(
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
    let ordering = OrderingInfo::new(sa, emitter);
    let location = sa.compute_loc(fct.file_id, fct.span);

    let comparable_trait_ty = TraitType::from_trait_id(sa.known.traits.comparable());
    let comparable_method_name = sa.interner.intern("cmp");
    let comparable_method_id = sa
        .trait_(comparable_trait_ty.trait_id)
        .get_method(comparable_method_name, false)
        .expect("Comparable::cmp missing");

    let mut builder = BytecodeBuilder::new();
    builder.push_scope();

    let lhs = builder.alloc_var(bytecode_enum_ty.clone());
    let rhs = builder.alloc_var(bytecode_enum_ty);
    let lhs_variant = builder.alloc_var(BytecodeType::Int32);
    let rhs_variant = builder.alloc_var(BytecodeType::Int32);
    let result = builder.alloc_var(ordering.ty.clone());
    let result_variant = builder.alloc_var(BytecodeType::Int32);
    let equal_variant = builder.alloc_var(BytecodeType::Int32);
    let condition = builder.alloc_var(BytecodeType::Bool);
    let same_variant_label = builder.create_label();
    let less_label = builder.create_label();
    let equal_label = builder.create_label();
    let return_label = builder.create_label();
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

    let enum_idx = builder.add_const_enum(bytecode_enum_id, bytecode_type_params.clone());
    let ordering_enum_idx = builder.add_const_enum(ordering.enum_id, BytecodeTypeArray::empty());
    builder.emit_const_int32(
        equal_variant,
        ordering.equal.try_into().expect("variant index overflow"),
    );
    builder.emit_load_enum_variant(lhs_variant, lhs, enum_idx, location);
    builder.emit_load_enum_variant(rhs_variant, rhs, enum_idx, location);
    builder.emit_test_eq(condition, lhs_variant, rhs_variant);
    builder.emit_jump_if_true(condition, same_variant_label);
    builder.emit_test_lt(condition, lhs_variant, rhs_variant);
    builder.emit_jump_if_true(condition, less_label);
    ordering.emit_variant(&mut builder, result, ordering.greater, location);
    builder.emit_ret(result);
    builder.bind_label(less_label);
    ordering.emit_variant(&mut builder, result, ordering.less, location);
    builder.emit_ret(result);

    builder.bind_label(same_variant_label);
    let jump_table_idx = builder.add_const_jump_table(variant_labels.clone(), equal_label);
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
            emit_trait_method_call(
                sa,
                emitter,
                &mut builder,
                enum_,
                &comparable_trait_ty,
                comparable_method_id,
                field_ty,
                result,
                &[lhs_field, rhs_field],
                location,
            );
            builder.emit_load_enum_variant(result_variant, result, ordering_enum_idx, location);
            builder.emit_test_ne(condition, result_variant, equal_variant);
            builder.emit_jump_if_true(condition, return_label);
            builder.free_temp(rhs_field);
            builder.free_temp(lhs_field);
        }

        ordering.emit_variant(&mut builder, result, ordering.equal, location);
        builder.emit_ret(result);
    }

    builder.bind_label(equal_label);
    ordering.emit_variant(&mut builder, result, ordering.equal, location);
    builder.emit_ret(result);
    builder.bind_label(return_label);
    builder.emit_ret(result);
    builder.pop_scope();
    builder.generate()
}

struct OrderingInfo {
    enum_id: EnumId,
    ty: BytecodeType,
    less: u32,
    equal: u32,
    greater: u32,
}

impl OrderingInfo {
    fn new(sa: &Sema, emitter: &mut Emitter) -> OrderingInfo {
        let enum_id = sa.known.enums.ordering();
        let ordering = sa.enum_(enum_id);
        let less_name = sa.interner.intern("Less");
        let equal_name = sa.interner.intern("Equal");
        let greater_name = sa.interner.intern("Greater");

        OrderingInfo {
            enum_id: emitter.convert_enum_id(sa, enum_id),
            ty: emitter.convert_ty(sa, SourceType::Enum(enum_id, SourceTypeArray::empty())),
            less: *ordering
                .name_to_value()
                .get(&less_name)
                .expect("Ordering::Less missing"),
            equal: *ordering
                .name_to_value()
                .get(&equal_name)
                .expect("Ordering::Equal missing"),
            greater: *ordering
                .name_to_value()
                .get(&greater_name)
                .expect("Ordering::Greater missing"),
        }
    }

    fn emit_variant(
        &self,
        builder: &mut BytecodeBuilder,
        dest: dora_bytecode::Register,
        variant: u32,
        location: dora_bytecode::Location,
    ) {
        let idx = builder.add_const_enum_variant(self.enum_id, BytecodeTypeArray::empty(), variant);
        builder.emit_new_enum(dest, idx, &[], location);
    }
}
