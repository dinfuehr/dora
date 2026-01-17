use super::BytecodeBuilder;
use dora_bytecode::{BytecodeType, Label, Register};

use crate::sema::{
    AltPattern, ClassDefinitionId, CtorPatternField, EnumDefinitionId, FieldIndex, IdentType,
    Pattern, PatternId, StructDefinitionId, TuplePattern, VarId, VarLocation,
};
use crate::specialize::specialize_type;
use crate::ty::SourceType;
use crate::ty::SourceTypeArray;

use super::AstBytecodeGen;
use super::{emit_mov, gen_fatal_error, store_in_context, var_reg};

pub(super) fn setup_pattern_vars(g: &mut AstBytecodeGen, pattern_id: PatternId) {
    let pattern = g.analysis.pattern(pattern_id);

    match pattern {
        Pattern::Ident(_) => {
            let ident_type = g.analysis.get_ident(pattern_id);

            match ident_type {
                Some(IdentType::EnumVariant(..))
                | Some(IdentType::Struct(..))
                | Some(IdentType::Class(..)) => {
                    // Do nothing - no fields to set up.
                }

                Some(IdentType::Var(var_id)) => {
                    setup_pattern_var(g, var_id);
                }

                _ => unreachable!(),
            }
        }

        Pattern::LitBool(..)
        | Pattern::LitChar(..)
        | Pattern::LitFloat(..)
        | Pattern::LitInt(..)
        | Pattern::LitStr(..)
        | Pattern::Underscore
        | Pattern::Rest => {
            // nothing to do
        }

        Pattern::Error => unreachable!(),

        Pattern::Ctor(ctor) => {
            for field in &ctor.fields {
                if let Some(subpattern_id) = field.pattern {
                    setup_pattern_vars(g, subpattern_id);
                }
            }
        }

        Pattern::Tuple(tuple) => {
            for &subpattern_id in &tuple.patterns {
                setup_pattern_vars(g, subpattern_id);
            }
        }

        Pattern::Alt(alt) => {
            // All alternative patterns define the same vars, so just allocate
            // registers for the first subpattern.
            if let Some(&first) = alt.patterns.first() {
                setup_pattern_vars(g, first);
            }
        }
    }
}

fn setup_pattern_var(g: &mut AstBytecodeGen, var_id: VarId) {
    let vars = g.analysis.vars();
    let var = vars.get_var(var_id);

    match var.location {
        VarLocation::Context(..) => {
            // Nothing to do here.
        }

        VarLocation::Stack => {
            g.allocate_register_for_var(var_id);
        }
    }
}

pub(super) fn destruct_pattern_or_fail(
    g: &mut AstBytecodeGen,
    pattern_id: PatternId,
    value: Register,
    ty: SourceType,
) {
    let mismatch_lbl = destruct_pattern(g, pattern_id, value, ty, None);
    if let Some(mismatch_lbl) = mismatch_lbl {
        let merge_lbl = g.builder.create_label();
        g.builder.emit_jump(merge_lbl);
        g.builder.bind_label(mismatch_lbl);
        gen_fatal_error(
            g,
            "pattern matching failure",
            g.span_for_pattern(pattern_id),
        );
        g.builder.bind_label(merge_lbl);
    }
}

pub(super) fn destruct_pattern(
    g: &mut AstBytecodeGen,
    pattern_id: PatternId,
    value: Register,
    ty: SourceType,
    exit: Option<Label>,
) -> Option<Label> {
    let mut pck = PatternCheckContext { exit };
    destruct_pattern_inner(g, &mut pck, pattern_id, value, ty);
    pck.exit
}

fn destruct_pattern_inner(
    g: &mut AstBytecodeGen,
    pck: &mut PatternCheckContext,
    pattern_id: PatternId,
    value: Register,
    ty: SourceType,
) {
    let pattern = g.analysis.pattern(pattern_id);

    match pattern {
        Pattern::Ident(_) => {
            let ident_type = g.analysis.get_ident(pattern_id);

            match ident_type {
                Some(IdentType::EnumVariant(enum_id, enum_type_params, variant_id)) => {
                    destruct_pattern_enum(
                        g,
                        pck,
                        pattern_id,
                        value,
                        enum_id,
                        &enum_type_params,
                        variant_id,
                    );
                }

                Some(IdentType::Struct(struct_id, struct_type_params)) => {
                    destruct_pattern_struct(
                        g,
                        pck,
                        pattern_id,
                        value,
                        struct_id,
                        &struct_type_params,
                    );
                }

                Some(IdentType::Class(class_id, class_type_params)) => {
                    destruct_pattern_class(g, pck, pattern_id, value, class_id, &class_type_params);
                }

                Some(IdentType::Var(var_id)) => {
                    destruct_pattern_var(g, pattern_id, value, var_id);
                }

                _ => unreachable!(),
            }
        }

        Pattern::LitBool(bool_value) => {
            let mismatch_lbl = pck.ensure_label(&mut g.builder);
            if *bool_value {
                g.builder.emit_jump_if_false(value, mismatch_lbl);
            } else {
                g.builder.emit_jump_if_true(value, mismatch_lbl);
            }
        }

        Pattern::LitChar(_) => {
            let mismatch_lbl = pck.ensure_label(&mut g.builder);
            let char_value = g.analysis.const_value(pattern_id).to_char();
            let tmp = g.alloc_temp(BytecodeType::Bool);
            let expected = g.alloc_temp(BytecodeType::Char);
            g.builder.emit_const_char(expected, char_value);
            g.builder.emit_test_eq(tmp, value, expected);
            g.builder.emit_jump_if_false(tmp, mismatch_lbl);
            g.builder.free_temp(tmp);
            g.builder.free_temp(expected);
        }

        Pattern::LitFloat(_) => {
            let bty = g.emitter.convert_ty_reg(ty);
            assert!(bty == BytecodeType::Float32 || bty == BytecodeType::Float64);
            let mismatch_lbl = pck.ensure_label(&mut g.builder);
            let const_value = g
                .analysis
                .const_value(pattern_id)
                .to_f64()
                .expect("float expected");
            let tmp = g.alloc_temp(BytecodeType::Bool);
            let expected = g.alloc_temp(bty.clone());
            match bty {
                BytecodeType::Float32 => g.builder.emit_const_float32(expected, const_value as f32),
                BytecodeType::Float64 => g.builder.emit_const_float64(expected, const_value),
                _ => unreachable!(),
            }
            g.builder.emit_test_eq(tmp, value, expected);
            g.builder.emit_jump_if_false(tmp, mismatch_lbl);
            g.builder.free_temp(tmp);
            g.builder.free_temp(expected);
        }

        Pattern::LitStr(_) => {
            let mismatch_lbl = pck.ensure_label(&mut g.builder);
            let const_value = g
                .analysis
                .const_value(pattern_id)
                .to_string()
                .expect("string expected")
                .to_string();
            let tmp = g.alloc_temp(BytecodeType::Bool);
            let expected = g.alloc_temp(BytecodeType::Ptr);
            g.builder.emit_const_string(expected, const_value);
            let fct_id = g.sa.known.functions.string_equals();
            let idx = g
                .builder
                .add_const_fct(g.emitter.convert_function_id(fct_id));
            g.builder.emit_push_register(value);
            g.builder.emit_push_register(expected);
            let loc = g.loc_for_pattern(pattern_id);
            g.builder.emit_invoke_direct(tmp, idx, loc);
            g.builder.emit_jump_if_false(tmp, mismatch_lbl);
            g.builder.free_temp(tmp);
            g.builder.free_temp(expected);
        }

        Pattern::LitInt(_) => {
            let bty = g.emitter.convert_ty_reg(ty);
            let mismatch_lbl = pck.ensure_label(&mut g.builder);
            let const_value = g.analysis.const_value(pattern_id);
            let tmp = g.alloc_temp(BytecodeType::Bool);
            let expected = g.alloc_temp(bty.clone());
            match bty {
                BytecodeType::Float32 => {
                    let v = const_value.to_f64().expect("float expected") as f32;
                    g.builder.emit_const_float32(expected, v);
                }
                BytecodeType::Float64 => {
                    let v = const_value.to_f64().expect("float expected");
                    g.builder.emit_const_float64(expected, v)
                }
                BytecodeType::UInt8 => {
                    let v = const_value.to_i64().expect("int expected") as u8;
                    g.builder.emit_const_uint8(expected, v)
                }
                BytecodeType::Int32 => {
                    let v = const_value.to_i64().expect("int expected") as i32;
                    g.builder.emit_const_int32(expected, v)
                }
                BytecodeType::Int64 => {
                    let v = const_value.to_i64().expect("int expected");
                    g.builder.emit_const_int64(expected, v)
                }
                _ => unreachable!(),
            }
            g.builder.emit_test_eq(tmp, value, expected);
            g.builder.emit_jump_if_false(tmp, mismatch_lbl);
            g.builder.free_temp(tmp);
            g.builder.free_temp(expected);
        }

        Pattern::Underscore => {
            // nothing to do
        }

        Pattern::Alt(alt) => {
            destruct_pattern_alt(g, pck, alt, value, ty);
        }

        Pattern::Ctor(ctor) => {
            let ident_type = g.analysis.get_ident(pattern_id).expect("missing ident");

            match ident_type {
                IdentType::EnumVariant(enum_id, enum_type_params, variant_id) => {
                    destruct_pattern_enum(
                        g,
                        pck,
                        pattern_id,
                        value,
                        enum_id,
                        &enum_type_params,
                        variant_id,
                    );
                }

                IdentType::Struct(struct_id, struct_type_params) => {
                    destruct_pattern_struct_with_fields(
                        g,
                        pck,
                        pattern_id,
                        &ctor.fields,
                        value,
                        struct_id,
                        &struct_type_params,
                    );
                }

                IdentType::Class(class_id, class_type_params) => {
                    destruct_pattern_class_with_fields(
                        g,
                        pck,
                        pattern_id,
                        &ctor.fields,
                        value,
                        class_id,
                        &class_type_params,
                    );
                }

                _ => unreachable!(),
            }
        }

        Pattern::Tuple(tuple) => {
            destruct_pattern_tuple(g, pck, tuple, value, ty);
        }

        Pattern::Rest => unreachable!(),
        Pattern::Error => unreachable!(),
    }
}

fn destruct_pattern_alt(
    g: &mut AstBytecodeGen,
    pck: &mut PatternCheckContext,
    alt: &AltPattern,
    value: Register,
    ty: SourceType,
) {
    let alts_len = alt.patterns.len();
    let mut alt_labels = Vec::with_capacity(alts_len + 1);
    let match_lbl = g.builder.create_label();

    for _ in 0..alts_len {
        alt_labels.push(g.builder.create_label());
    }

    alt_labels.push(pck.ensure_label(&mut g.builder));

    for (idx, &alt_pattern_id) in alt.patterns.iter().enumerate() {
        let current_lbl = alt_labels[idx];
        g.builder.bind_label(current_lbl);

        let next_lbl = alt_labels[idx + 1];

        let mut alt_pck = PatternCheckContext {
            exit: Some(next_lbl),
        };
        destruct_pattern_inner(g, &mut alt_pck, alt_pattern_id, value, ty.clone());
        g.builder.emit_jump(match_lbl);
    }

    g.builder.bind_label(match_lbl);
}

fn destruct_pattern_enum(
    g: &mut AstBytecodeGen,
    pck: &mut PatternCheckContext,
    pattern_id: PatternId,
    value: Register,
    enum_id: EnumDefinitionId,
    enum_type_params: &SourceTypeArray,
    variant_idx: u32,
) {
    let enum_ = g.sa.enum_(enum_id);

    let bc_enum_id = g.emitter.convert_enum_id(enum_id);
    let bc_enum_type_params = g.emitter.convert_tya(enum_type_params);

    let match_reg = g.alloc_temp(BytecodeType::Bool);
    let actual_variant_reg = g.alloc_temp(BytecodeType::Int32);
    let idx = g
        .builder
        .add_const_enum(bc_enum_id, bc_enum_type_params.clone());
    let loc = g.loc_for_pattern(pattern_id);
    g.builder
        .emit_load_enum_variant(actual_variant_reg, value, idx, loc);

    let expected_variant_reg = g.alloc_temp(BytecodeType::Int32);
    g.builder
        .emit_const_int32(expected_variant_reg, variant_idx as i32);
    g.builder
        .emit_test_eq(match_reg, actual_variant_reg, expected_variant_reg);
    let lbl = pck.ensure_label(&mut g.builder);
    g.builder.emit_jump_if_false(match_reg, lbl);

    let variant_id = enum_.variant_id_at(variant_idx as usize);
    let variant = g.sa.variant(variant_id);

    // Get subpatterns from HIR
    let pattern = g.analysis.pattern(pattern_id);
    if let Some(fields) = get_ctor_fields(pattern) {
        iterate_ctor_fields(
            g,
            pck,
            pattern_id,
            fields,
            |g, pck, field_idx, subpattern_id| {
                let field_id = variant.field_id(FieldIndex(field_idx));
                let field = g.sa.field(field_id);
                let element_ty = field.ty();
                let element_ty = specialize_type(g.sa, element_ty, enum_type_params);
                let bty = g.emitter.convert_ty_reg(element_ty.clone());
                let field_reg = g.alloc_temp(bty);

                let cp_idx = g.builder.add_const_enum_element(
                    bc_enum_id,
                    bc_enum_type_params.clone(),
                    variant_idx,
                    field_idx as u32,
                );

                let loc = g.loc_for_pattern(pattern_id);
                g.builder
                    .emit_load_enum_element(field_reg, value, cp_idx, loc);

                destruct_pattern_inner(g, pck, subpattern_id, field_reg, element_ty);

                g.free_temp(field_reg);
            },
        );
    }

    g.free_temp(actual_variant_reg);
    g.free_temp(expected_variant_reg);
    g.free_temp(match_reg);
}

fn destruct_pattern_struct(
    g: &mut AstBytecodeGen,
    pck: &mut PatternCheckContext,
    pattern_id: PatternId,
    value: Register,
    struct_id: StructDefinitionId,
    struct_type_params: &SourceTypeArray,
) {
    let pattern = g.analysis.pattern(pattern_id);
    if let Some(fields) = get_ctor_fields(pattern) {
        destruct_pattern_struct_with_fields(
            g,
            pck,
            pattern_id,
            fields,
            value,
            struct_id,
            struct_type_params,
        );
    }
}

fn destruct_pattern_struct_with_fields(
    g: &mut AstBytecodeGen,
    pck: &mut PatternCheckContext,
    pattern_id: PatternId,
    fields: &[CtorPatternField],
    value: Register,
    struct_id: StructDefinitionId,
    struct_type_params: &SourceTypeArray,
) {
    let struct_ = g.sa.struct_(struct_id);

    iterate_ctor_fields(
        g,
        pck,
        pattern_id,
        fields,
        |g, pck, field_idx, subpattern_id| {
            let field_id = struct_.field_id(FieldIndex(field_idx));
            let field_ty = g.sa.field(field_id).ty();
            let field_ty = specialize_type(g.sa, field_ty, struct_type_params);
            let register_ty = g.emitter.convert_ty_reg(field_ty.clone());
            let cp_idx = g.builder.add_const_struct_field(
                g.emitter.convert_struct_id(struct_id),
                g.convert_tya(struct_type_params),
                field_idx as u32,
            );
            let temp_reg = g.alloc_temp(register_ty);
            g.builder.emit_load_struct_field(temp_reg, value, cp_idx);
            destruct_pattern_inner(g, pck, subpattern_id, temp_reg, field_ty);
            g.free_temp(temp_reg);
        },
    );
}

fn destruct_pattern_class(
    g: &mut AstBytecodeGen,
    pck: &mut PatternCheckContext,
    pattern_id: PatternId,
    value: Register,
    class_id: ClassDefinitionId,
    class_type_params: &SourceTypeArray,
) {
    let pattern = g.analysis.pattern(pattern_id);
    if let Some(fields) = get_ctor_fields(pattern) {
        destruct_pattern_class_with_fields(
            g,
            pck,
            pattern_id,
            fields,
            value,
            class_id,
            class_type_params,
        );
    }
}

fn destruct_pattern_class_with_fields(
    g: &mut AstBytecodeGen,
    pck: &mut PatternCheckContext,
    pattern_id: PatternId,
    fields: &[CtorPatternField],
    value: Register,
    class_id: ClassDefinitionId,
    class_type_params: &SourceTypeArray,
) {
    let class = g.sa.class(class_id);

    iterate_ctor_fields(
        g,
        pck,
        pattern_id,
        fields,
        |g, pck, field_idx, subpattern_id| {
            let field_id = class.field_id(FieldIndex(field_idx));
            let field_ty = g.sa.field(field_id).ty();
            let field_ty = specialize_type(g.sa, field_ty, class_type_params);
            let register_ty = g.emitter.convert_ty_reg(field_ty.clone());
            let cp_idx = g.builder.add_const_field_types(
                g.emitter.convert_class_id(class_id),
                g.convert_tya(class_type_params),
                field_idx as u32,
            );
            let temp_reg = g.alloc_temp(register_ty);
            let loc = g.loc_for_pattern(pattern_id);
            g.builder.emit_load_field(temp_reg, value, cp_idx, loc);
            destruct_pattern_inner(g, pck, subpattern_id, temp_reg, field_ty);
            g.free_temp(temp_reg);
        },
    );
}

fn destruct_pattern_tuple(
    g: &mut AstBytecodeGen,
    pck: &mut PatternCheckContext,
    tuple: &TuplePattern,
    value: Register,
    ty: SourceType,
) {
    if ty.is_unit() {
        assert!(tuple.patterns.is_empty());
    } else {
        let tuple_subtypes = ty.tuple_subtypes().expect("tuple expected");

        for &subpattern_id in &tuple.patterns {
            let subpattern = g.analysis.pattern(subpattern_id);
            if matches!(subpattern, Pattern::Rest | Pattern::Underscore) {
                // Do nothing.
            } else {
                let field_id = g
                    .analysis
                    .get_field_id(subpattern_id)
                    .expect("missing field_id");
                let subtype = tuple_subtypes[field_id].clone();
                let register_ty = g.emitter.convert_ty_reg(subtype.clone());
                let cp_idx = g
                    .builder
                    .add_const_tuple_element(g.emitter.convert_ty(ty.clone()), field_id as u32);
                let temp_reg = g.alloc_temp(register_ty);
                g.builder.emit_load_tuple_element(temp_reg, value, cp_idx);
                destruct_pattern_inner(g, pck, subpattern_id, temp_reg, subtype);
                g.free_temp(temp_reg);
            }
        }
    }
}

fn destruct_pattern_var(
    g: &mut AstBytecodeGen,
    pattern_id: PatternId,
    value: Register,
    var_id: VarId,
) {
    let vars = g.analysis.vars();
    let var = vars.get_var(var_id);

    if !var.ty.is_unit() {
        match var.location {
            VarLocation::Context(scope_id, field_id) => {
                let loc = g.loc_for_pattern(pattern_id);
                store_in_context(g, value, scope_id, field_id, loc);
            }

            VarLocation::Stack => {
                let var_reg = var_reg(g, var_id);
                emit_mov(g, var_reg, value);
            }
        }
    }
}

fn get_ctor_fields(pattern: &Pattern) -> Option<&[CtorPatternField]> {
    match pattern {
        Pattern::Ctor(ctor) => Some(&ctor.fields),
        Pattern::Ident(..) => None,
        _ => None,
    }
}

fn iterate_ctor_fields<F>(
    g: &mut AstBytecodeGen,
    pck: &mut PatternCheckContext,
    _pattern_id: PatternId,
    fields: &[CtorPatternField],
    mut f: F,
) where
    F: FnMut(&mut AstBytecodeGen, &mut PatternCheckContext, usize, PatternId),
{
    for field in fields {
        let subpattern_id = match field.pattern {
            Some(id) => id,
            None => continue,
        };

        let subpattern = g.analysis.pattern(subpattern_id);
        if matches!(subpattern, Pattern::Rest | Pattern::Underscore) {
            // Do nothing.
        } else {
            let field_idx = g
                .analysis
                .get_field_id(subpattern_id)
                .expect("missing field_id");
            f(g, pck, field_idx, subpattern_id);
        }
    }
}

struct PatternCheckContext {
    exit: Option<Label>,
}

impl PatternCheckContext {
    fn ensure_label(&mut self, b: &mut BytecodeBuilder) -> Label {
        if self.exit.is_none() {
            self.exit = Some(b.create_label());
        }

        self.exit.expect("missing label")
    }
}
