use super::BytecodeBuilder;
use dora_bytecode::{BytecodeType, Label, Register};
use dora_parser::ast::{self, AstId};

use crate::sema::{
    ClassDefinitionId, EnumDefinitionId, FieldIndex, IdentType, StructDefinitionId, VarId,
    VarLocation,
};
use crate::specialize::specialize_type;
use crate::ty::SourceType;
use crate::ty::SourceTypeArray;

use super::AstBytecodeGen;
use super::expr::{emit_mov, gen_fatal_error, store_in_context, var_reg};

impl<'a> AstBytecodeGen<'a> {
    pub(super) fn setup_pattern_vars(&mut self, pattern_id: AstId) {
        let pattern = self.node(pattern_id);

        match pattern {
            ast::Ast::IdentPattern(..) => {
                let ident_type = self.analysis.map_idents.get(pattern_id);

                match ident_type {
                    Some(IdentType::EnumVariant(..)) => {
                        // Do nothing.
                    }

                    Some(IdentType::Var(var_id)) => {
                        self.setup_pattern_var(*var_id);
                    }

                    _ => unreachable!(),
                }
            }

            ast::Ast::LitPattern(..) | ast::Ast::UnderscorePattern(..) | ast::Ast::Rest(..) => {
                // nothing to do
            }

            ast::Ast::Error(..) => unreachable!(),

            ast::Ast::CtorPattern(p) => {
                if let Some(ctor_field_list_id) = p.param_list {
                    let ctor_field_list = self.node(ctor_field_list_id).as_ctor_field_list();
                    for &ctor_field_id in &ctor_field_list.items {
                        let ctor_field = self
                            .node(ctor_field_id)
                            .to_ctor_field()
                            .expect("field expected");
                        self.setup_pattern_vars(ctor_field.pattern);
                    }
                }
            }

            ast::Ast::TuplePattern(tuple) => {
                for &param_id in &tuple.params {
                    self.setup_pattern_vars(param_id);
                }
            }

            ast::Ast::Alt(p) => {
                // All alternative patterns define the same vars, so just allocate
                // registers for the first subpattern.
                self.setup_pattern_vars(p.alts[0]);
            }

            _ => unreachable!(),
        }
    }

    pub(super) fn setup_pattern_var(&mut self, var_id: VarId) {
        let var = self.analysis.vars.get_var(var_id);

        match var.location {
            VarLocation::Context(..) => {
                // Nothing to do here.
            }

            VarLocation::Stack => {
                self.allocate_register_for_var(var_id);
            }
        }
    }

    pub(super) fn destruct_pattern_or_fail(
        &mut self,
        pattern_id: AstId,
        value: Register,
        ty: SourceType,
    ) {
        let pattern = self.node(pattern_id);
        let mismatch_lbl = self.destruct_pattern(pattern_id, value, ty, None);
        if let Some(mismatch_lbl) = mismatch_lbl {
            let merge_lbl = self.builder.create_label();
            self.builder.emit_jump(merge_lbl);
            self.builder.bind_label(mismatch_lbl);
            gen_fatal_error(self, "pattern matching failure", pattern.span());
            self.builder.bind_label(merge_lbl);
        }
    }

    pub(super) fn destruct_pattern(
        &mut self,
        pattern_id: AstId,
        value: Register,
        ty: SourceType,
        exit: Option<Label>,
    ) -> Option<Label> {
        let mut pck = PatternCheckContext { exit };
        self.destruct_pattern_alt(&mut pck, pattern_id, value, ty);
        pck.exit
    }

    fn destruct_pattern_alt(
        &mut self,
        pck: &mut PatternCheckContext,
        pattern_id: AstId,
        value: Register,
        ty: SourceType,
    ) {
        let pattern = self.node(pattern_id);

        match pattern {
            ast::Ast::IdentPattern(..) => {
                let ident_type = self.analysis.map_idents.get(pattern_id);

                match ident_type {
                    Some(IdentType::EnumVariant(enum_id, enum_type_params, variant_id)) => {
                        self.destruct_pattern_enum(
                            pck,
                            pattern_id,
                            value,
                            ty,
                            *enum_id,
                            enum_type_params,
                            *variant_id,
                        );
                    }

                    Some(IdentType::Var(var_id)) => {
                        self.destruct_pattern_var(pck, pattern_id, value, ty, *var_id)
                    }

                    _ => unreachable!(),
                }
            }

            ast::Ast::LitPattern(p) => match p.kind {
                ast::PatternLitKind::Bool => {
                    let mismatch_lbl = pck.ensure_label(&mut self.builder);
                    let p = self
                        .node(p.expr)
                        .to_lit_bool()
                        .expect("expected bool literal");
                    if p.value {
                        self.builder.emit_jump_if_false(value, mismatch_lbl);
                    } else {
                        self.builder.emit_jump_if_true(value, mismatch_lbl);
                    }
                }

                ast::PatternLitKind::Char => {
                    let mismatch_lbl = pck.ensure_label(&mut self.builder);
                    let char_value = self.analysis.const_value(pattern_id).to_char();
                    let tmp = self.alloc_temp(BytecodeType::Bool);
                    let expected = self.alloc_temp(BytecodeType::Char);
                    self.builder.emit_const_char(expected, char_value);
                    self.builder.emit_test_eq(tmp, value, expected);
                    self.builder.emit_jump_if_false(tmp, mismatch_lbl);
                    self.builder.free_temp(tmp);
                    self.builder.free_temp(expected);
                }

                ast::PatternLitKind::Float => {
                    let ty = self.emitter.convert_ty_reg(ty);
                    assert!(ty == BytecodeType::Float32 || ty == BytecodeType::Float64);
                    let mismatch_lbl = pck.ensure_label(&mut self.builder);
                    let const_value = self
                        .analysis
                        .const_value(pattern_id)
                        .to_f64()
                        .expect("float expected");
                    let tmp = self.alloc_temp(BytecodeType::Bool);
                    let expected = self.alloc_temp(ty.clone());
                    match ty {
                        BytecodeType::Float32 => self
                            .builder
                            .emit_const_float32(expected, const_value as f32),
                        BytecodeType::Float64 => {
                            self.builder.emit_const_float64(expected, const_value)
                        }
                        _ => unreachable!(),
                    }
                    self.builder.emit_test_eq(tmp, value, expected);
                    self.builder.emit_jump_if_false(tmp, mismatch_lbl);
                    self.builder.free_temp(tmp);
                    self.builder.free_temp(expected);
                }

                ast::PatternLitKind::String => {
                    let mismatch_lbl = pck.ensure_label(&mut self.builder);
                    let const_value = self
                        .analysis
                        .const_value(pattern_id)
                        .to_string()
                        .expect("float expected")
                        .to_string();
                    let tmp = self.alloc_temp(BytecodeType::Bool);
                    let expected = self.alloc_temp(BytecodeType::Ptr);
                    self.builder.emit_const_string(expected, const_value);
                    let fct_id = self.sa.known.functions.string_equals();
                    let idx = self
                        .builder
                        .add_const_fct(self.emitter.convert_function_id(fct_id));
                    self.builder.emit_push_register(value);
                    self.builder.emit_push_register(expected);
                    self.builder.emit_invoke_direct(tmp, idx, self.loc(p.span));
                    self.builder.emit_jump_if_false(tmp, mismatch_lbl);
                    self.builder.free_temp(tmp);
                    self.builder.free_temp(expected);
                }

                ast::PatternLitKind::Int => {
                    let ty = self.emitter.convert_ty_reg(ty);
                    let mismatch_lbl = pck.ensure_label(&mut self.builder);
                    let const_value = self.analysis.const_value(pattern_id);
                    let tmp = self.alloc_temp(BytecodeType::Bool);
                    let expected = self.alloc_temp(ty.clone());
                    match ty {
                        BytecodeType::Float32 => {
                            let value = const_value.to_f64().expect("float expected") as f32;
                            self.builder.emit_const_float32(expected, value);
                        }
                        BytecodeType::Float64 => {
                            let value = const_value.to_f64().expect("float expected");
                            self.builder.emit_const_float64(expected, value)
                        }
                        BytecodeType::UInt8 => {
                            let value = const_value.to_i64().expect("float expected") as u8;
                            self.builder.emit_const_uint8(expected, value)
                        }
                        BytecodeType::Int32 => {
                            let value = const_value.to_i64().expect("float expected") as i32;
                            self.builder.emit_const_int32(expected, value)
                        }
                        BytecodeType::Int64 => {
                            let value = const_value.to_i64().expect("float expected");
                            self.builder.emit_const_int64(expected, value)
                        }
                        _ => unreachable!(),
                    }
                    self.builder.emit_test_eq(tmp, value, expected);
                    self.builder.emit_jump_if_false(tmp, mismatch_lbl);
                    self.builder.free_temp(tmp);
                    self.builder.free_temp(expected);
                }
            },

            ast::Ast::UnderscorePattern(_) => {
                // nothing to do
            }

            ast::Ast::Alt(p) => {
                let mut alt_labels = Vec::with_capacity(p.alts.len() + 1);
                let match_lbl = self.builder.create_label();

                for _ in &p.alts {
                    alt_labels.push(self.builder.create_label());
                }

                alt_labels.push(pck.ensure_label(&mut self.builder));

                for (idx, &alt_id) in p.alts.iter().enumerate() {
                    let current_lbl = alt_labels[idx];
                    self.builder.bind_label(current_lbl);

                    let next_lbl = alt_labels[idx + 1];

                    let mut alt_pck = PatternCheckContext {
                        exit: Some(next_lbl),
                    };
                    self.destruct_pattern_alt(&mut alt_pck, alt_id, value, ty.clone());
                    self.builder.emit_jump(match_lbl);
                }

                self.builder.bind_label(match_lbl);
            }

            ast::Ast::CtorPattern(..) => {
                let ident_type = self.analysis.map_idents.get(pattern_id).unwrap();

                match ident_type {
                    IdentType::EnumVariant(enum_id, enum_type_params, variant_id) => {
                        self.destruct_pattern_enum(
                            pck,
                            pattern_id,
                            value,
                            ty,
                            *enum_id,
                            enum_type_params,
                            *variant_id,
                        );
                    }

                    IdentType::Struct(struct_id, struct_type_params) => {
                        self.destruct_pattern_struct(
                            pck,
                            pattern_id,
                            value,
                            ty,
                            *struct_id,
                            struct_type_params,
                        );
                    }

                    IdentType::Class(class_id, class_type_params) => {
                        self.destruct_pattern_class(
                            pck,
                            pattern_id,
                            value,
                            ty,
                            *class_id,
                            class_type_params,
                        );
                    }

                    _ => unreachable!(),
                }
            }

            ast::Ast::TuplePattern(..) => {
                self.destruct_pattern_tuple(pck, pattern_id, value, ty);
            }

            ast::Ast::Rest(..) => unreachable!(),

            _ => unreachable!(),
        }
    }

    fn destruct_pattern_enum(
        &mut self,
        pck: &mut PatternCheckContext,
        pattern_id: AstId,
        value: Register,
        _ty: SourceType,
        enum_id: EnumDefinitionId,
        enum_type_params: &SourceTypeArray,
        variant_idx: u32,
    ) {
        let enum_ = self.sa.enum_(enum_id);

        let bc_enum_id = self.emitter.convert_enum_id(enum_id);
        let bc_enum_type_params = self.emitter.convert_tya(enum_type_params);

        let match_reg = self.alloc_temp(BytecodeType::Bool);
        let actual_variant_reg = self.alloc_temp(BytecodeType::Int32);
        let idx = self
            .builder
            .add_const_enum(bc_enum_id, bc_enum_type_params.clone());
        self.builder.emit_load_enum_variant(
            actual_variant_reg,
            value,
            idx,
            self.loc(self.span(pattern_id)),
        );

        let expected_variant_reg = self.alloc_temp(BytecodeType::Int32);
        self.builder
            .emit_const_int32(expected_variant_reg, variant_idx as i32);
        self.builder
            .emit_test_eq(match_reg, actual_variant_reg, expected_variant_reg);
        let lbl = pck.ensure_label(&mut self.builder);
        self.builder.emit_jump_if_false(match_reg, lbl);

        let variant_id = enum_.variant_id_at(variant_idx as usize);
        let variant = self.sa.variant(variant_id);

        iterate_subpatterns(self, pattern_id, |g, idx, param_id| {
            let field_id = variant.field_id(FieldIndex(idx));
            let field = g.sa.field(field_id);
            let element_ty = field.ty();
            let element_ty = specialize_type(g.sa, element_ty, enum_type_params);
            let ty = g.emitter.convert_ty_reg(element_ty.clone());
            let field_reg = g.alloc_temp(ty);

            let idx = g.builder.add_const_enum_element(
                bc_enum_id,
                bc_enum_type_params.clone(),
                variant_idx,
                idx as u32,
            );

            g.builder
                .emit_load_enum_element(field_reg, value, idx, g.loc(g.span(pattern_id)));

            let param = g.node(param_id).as_ctor_field();
            g.destruct_pattern_alt(pck, param.pattern, field_reg, element_ty);
            g.free_temp(field_reg);
        });

        self.free_temp(actual_variant_reg);
        self.free_temp(expected_variant_reg);
        self.free_temp(match_reg);
    }

    fn destruct_pattern_struct(
        &mut self,
        pck: &mut PatternCheckContext,
        pattern_id: AstId,
        value: Register,
        _ty: SourceType,
        struct_id: StructDefinitionId,
        struct_type_params: &SourceTypeArray,
    ) {
        let struct_ = self.sa.struct_(struct_id);

        iterate_subpatterns(self, pattern_id, |g, idx, field_ast_id| {
            let field_id = struct_.field_id(FieldIndex(idx));
            let field_ty = g.sa.field(field_id).ty();
            let field_ty = specialize_type(g.sa, field_ty, struct_type_params);
            let register_ty = g.emitter.convert_ty_reg(field_ty.clone());
            let idx = g.builder.add_const_struct_field(
                g.emitter.convert_struct_id(struct_id),
                g.convert_tya(struct_type_params),
                idx as u32,
            );
            let temp_reg = g.alloc_temp(register_ty);
            g.builder.emit_load_struct_field(temp_reg, value, idx);
            let field = g
                .node(field_ast_id)
                .to_ctor_field()
                .expect("field expected");
            g.destruct_pattern_alt(pck, field.pattern, temp_reg, field_ty);
            g.free_temp(temp_reg);
        })
    }

    fn destruct_pattern_class(
        &mut self,
        pck: &mut PatternCheckContext,
        pattern_id: AstId,
        value: Register,
        _ty: SourceType,
        class_id: ClassDefinitionId,
        class_type_params: &SourceTypeArray,
    ) {
        let class = self.sa.class(class_id);

        iterate_subpatterns(self, pattern_id, |g, idx, field_ast_id| {
            let field_id = class.field_id(FieldIndex(idx));
            let field_ty = g.sa.field(field_id).ty();
            let field_ty = specialize_type(g.sa, field_ty, class_type_params);
            let register_ty = g.emitter.convert_ty_reg(field_ty.clone());
            let idx = g.builder.add_const_field_types(
                g.emitter.convert_class_id(class_id),
                g.convert_tya(class_type_params),
                idx as u32,
            );
            let temp_reg = g.alloc_temp(register_ty);
            g.builder
                .emit_load_field(temp_reg, value, idx, g.loc(g.span(pattern_id)));
            let field = g
                .node(field_ast_id)
                .to_ctor_field()
                .expect("field expected");
            g.destruct_pattern_alt(pck, field.pattern, temp_reg, field_ty);
            g.free_temp(temp_reg);
        })
    }

    fn destruct_pattern_tuple(
        &mut self,
        pck: &mut PatternCheckContext,
        pattern_id: AstId,
        value: Register,
        ty: SourceType,
    ) {
        let pattern = self
            .node(pattern_id)
            .to_tuple_pattern()
            .expect("tuple expected");
        let subpatterns = pattern.params.as_slice();

        if ty.is_unit() {
            assert!(subpatterns.is_empty());
        } else {
            let tuple_subtypes = ty.tuple_subtypes().expect("tuple expected");

            for &subpattern_id in subpatterns {
                let subpattern = self.node(subpattern_id);

                if subpattern.is_rest() || subpattern.is_underscore_pattern() {
                    // Do nothing.
                } else {
                    let field_id = self
                        .analysis
                        .map_field_ids
                        .get(subpattern_id)
                        .cloned()
                        .expect("missing field_id");
                    let subtype = tuple_subtypes[field_id].clone();
                    let register_ty = self.emitter.convert_ty_reg(subtype.clone());
                    let cp_idx = self.builder.add_const_tuple_element(
                        self.emitter.convert_ty(ty.clone()),
                        field_id as u32,
                    );
                    let temp_reg = self.alloc_temp(register_ty);
                    self.builder
                        .emit_load_tuple_element(temp_reg, value, cp_idx);
                    self.destruct_pattern_alt(pck, subpattern_id, temp_reg, subtype);
                    self.free_temp(temp_reg);
                }
            }
        }
    }

    fn destruct_pattern_var(
        &mut self,
        _pck: &mut PatternCheckContext,
        pattern_id: AstId,
        value: Register,
        _ty: SourceType,
        var_id: VarId,
    ) {
        let var = self.analysis.vars.get_var(var_id);

        if !var.ty.is_unit() {
            match var.location {
                VarLocation::Context(scope_id, field_id) => {
                    store_in_context(
                        self,
                        value,
                        scope_id,
                        field_id,
                        self.loc(self.span(pattern_id)),
                    );
                }

                VarLocation::Stack => {
                    let var_reg = var_reg(self, var_id);
                    emit_mov(self, var_reg, value);
                }
            }
        }
    }
}

fn iterate_subpatterns<'a, F>(g: &mut AstBytecodeGen<'a>, pattern_id: AstId, mut f: F)
where
    F: FnMut(&mut AstBytecodeGen<'a>, usize, AstId),
{
    let pattern = g.node(pattern_id);

    if let Some(pattern) = pattern.to_ctor_pattern() {
        if let Some(ctor_field_list_id) = pattern.param_list {
            let ctor_field_list = g.node(ctor_field_list_id).as_ctor_field_list();
            for &ctor_field_id in &ctor_field_list.items {
                let ctor_field = g
                    .node(ctor_field_id)
                    .to_ctor_field()
                    .expect("field expected");
                let subpattern = g.node(ctor_field.pattern);

                if subpattern.is_rest() || subpattern.is_underscore_pattern() {
                    // Do nothing.
                } else {
                    let field_id = g
                        .analysis
                        .map_field_ids
                        .get(ctor_field_id)
                        .cloned()
                        .expect("missing field_id");
                    f(g, field_id, ctor_field_id);
                }
            }
        }
    }
}

struct PatternCheckContext {
    exit: Option<Label>,
}

impl PatternCheckContext {
    #[allow(unused)]
    fn ensure_label(&mut self, b: &mut BytecodeBuilder) -> Label {
        if self.exit.is_none() {
            self.exit = Some(b.create_label());
        }

        self.exit.expect("missing label")
    }
}
