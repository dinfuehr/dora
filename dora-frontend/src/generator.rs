use std::collections::HashMap;
use std::convert::TryInto;
use std::sync::Arc;

use dora_parser::ast::CmpOp;
use dora_parser::{ast, Span};

use self::bytecode::BytecodeBuilder;
use self::expr::{
    gen_expr, gen_expr_bin_cmp, gen_expr_condition, gen_fatal_error, gen_intrinsic_bin,
    gen_method_bin,
};
use crate::sema::{
    emit_as_bytecode_operation, new_identity_type_params, AnalysisData, CallType,
    ClassDefinitionId, ConstDefinitionId, ContextFieldId, Element, EnumDefinitionId, FctDefinition,
    FctDefinitionId, FieldId, GlobalDefinition, GlobalDefinitionId, IdentType, Intrinsic,
    LazyContextData, OuterContextIdx, ScopeId, Sema, SourceFileId, StructDefinitionId, VarId,
    VarLocation,
};
use crate::specialize::{replace_type, specialize_type};
use crate::ty::{SourceType, SourceTypeArray, TraitType};
use crate::{expr_always_returns, expr_block_always_returns, specialize_ty_for_trait_object};
use dora_bytecode::{
    AliasId, BytecodeFunction, BytecodeTraitType, BytecodeType, BytecodeTypeArray, ClassId,
    ConstPoolEntry, ConstPoolIdx, EnumId, FunctionId, GlobalId, Label, Location, Register,
    StructId, TraitId,
};

mod bytecode;
mod expr;
#[cfg(test)]
pub mod tests;

pub struct LoopLabels {
    cond: Label,
    end: Label,
}

impl LoopLabels {
    fn new(cond: Label, end: Label) -> LoopLabels {
        LoopLabels { cond, end }
    }
}

pub fn generate_fct_id(sa: &Sema, id: FctDefinitionId) -> BytecodeFunction {
    let fct = sa.fct(id);
    let analysis = fct.analysis();

    generate_fct(sa, &fct, analysis)
}

pub fn generate_fct(sa: &Sema, fct: &FctDefinition, src: &AnalysisData) -> BytecodeFunction {
    let ast_bytecode_generator = AstBytecodeGen {
        sa,
        type_params_len: fct.type_param_definition().type_param_count(),
        is_lambda: fct.is_lambda(),
        return_type: fct.return_type(),
        file_id: fct.file_id,
        span: fct.span,
        analysis: src,

        builder: BytecodeBuilder::new(),
        loops: Vec::new(),
        var_registers: HashMap::new(),
        unit_register: None,
        entered_contexts: Vec::new(),
    };
    ast_bytecode_generator.generate_fct(fct.ast().expect("body expected"))
}

pub fn generate_global_initializer(
    sa: &Sema,
    global: &GlobalDefinition,
    src: &AnalysisData,
) -> BytecodeFunction {
    let ast_bytecode_generator = AstBytecodeGen {
        sa,
        type_params_len: 0,
        is_lambda: false,
        return_type: global.ty(),
        file_id: global.file_id,
        span: global.span,
        analysis: src,

        builder: BytecodeBuilder::new(),
        loops: Vec::new(),
        var_registers: HashMap::new(),
        unit_register: None,
        entered_contexts: Vec::new(),
    };

    ast_bytecode_generator.generate_global_initializer(global.initial_value_expr())
}

const SELF_VAR_ID: VarId = VarId(0);

struct EnteredContext {
    context_data: LazyContextData,
    register: Option<Register>,
}

struct AstBytecodeGen<'a> {
    sa: &'a Sema,
    type_params_len: usize,
    is_lambda: bool,
    return_type: SourceType,
    file_id: SourceFileId,
    span: Span,
    analysis: &'a AnalysisData,

    builder: BytecodeBuilder,
    loops: Vec<LoopLabels>,
    var_registers: HashMap<VarId, Register>,
    entered_contexts: Vec<EnteredContext>,
    unit_register: Option<Register>,
}

impl<'a> AstBytecodeGen<'a> {
    fn loc(&self, span: Span) -> Location {
        self.sa.compute_loc(self.file_id, span)
    }

    fn generate_fct(mut self, ast: &ast::Function) -> BytecodeFunction {
        self.push_scope();
        self.create_params(ast);
        self.enter_function_context();
        self.store_params_in_context(ast);
        self.emit_function_body(ast);
        self.leave_function_context();
        self.pop_scope();
        self.builder.generate()
    }

    fn generate_global_initializer(mut self, expr: &ast::ExprData) -> BytecodeFunction {
        self.push_scope();
        self.builder.set_params(Vec::new());
        self.enter_function_context();
        self.emit_global_initializer(expr);
        self.leave_function_context();
        self.pop_scope();
        self.builder.generate()
    }

    fn create_params(&mut self, ast: &ast::Function) {
        let mut params = Vec::new();

        if self.analysis.has_self() {
            let var_self = self.analysis.vars.get_self();
            let var_ty = var_self.ty.clone();

            let bty = bty_from_ty(var_ty.clone());
            params.push(bty);

            self.allocate_register_for_var(SELF_VAR_ID);
        }

        for param in &ast.params {
            let var_id = *self.analysis.map_vars.get(param.id).unwrap();
            let ty = self.var_ty(var_id);

            let bty = bty_from_ty(ty.clone());
            params.push(bty);

            self.allocate_register_for_var(var_id);
        }

        self.builder.set_params(params);
    }

    fn store_params_in_context(&mut self, ast: &ast::Function) {
        let next_register_idx = if self.analysis.has_self() {
            let var_self = self.analysis.vars.get_self();
            let reg = Register(0);

            match var_self.location {
                VarLocation::Context(scope_id, field_id) => {
                    self.store_in_context(reg, scope_id, field_id, self.loc(self.span));
                }

                VarLocation::Stack => {
                    // Nothing to do.
                }
            }

            1
        } else {
            0
        };

        for (param_idx, param) in ast.params.iter().enumerate() {
            let var_id = *self.analysis.map_vars.get(param.id).unwrap();
            let var = self.analysis.vars.get_var(var_id);
            let reg = Register(next_register_idx + param_idx);

            match var.location {
                VarLocation::Context(scope_id, field_id) => {
                    self.store_in_context(reg, scope_id, field_id, self.loc(self.span));
                }

                VarLocation::Stack => {
                    // Nothing to do.
                }
            }
        }
    }

    fn emit_function_body(&mut self, ast: &ast::Function) {
        let bty_return_type = bty_from_ty(self.return_type.clone());
        self.builder.set_return_type(bty_return_type);

        let mut needs_return = true;

        let block = ast.block.as_ref().expect("missing block");
        let block = block.to_block().expect("block node expected");

        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }

        if let Some(ref value) = block.expr {
            let reg = gen_expr(self, value, DataDest::Alloc);

            if !expr_block_always_returns(block) {
                self.builder.emit_ret(reg);
            }

            needs_return = false;
            self.free_if_temp(reg);
        }

        if needs_return && self.return_type.is_unit() {
            let dest = self.ensure_unit_register();
            self.builder.emit_ret(dest);
        }
    }

    fn emit_global_initializer(&mut self, expr: &ast::ExprData) {
        let result = gen_expr(self, expr, DataDest::Alloc);
        self.builder.emit_ret(result);
        self.free_if_temp(result);
    }

    fn enter_function_context(&mut self) {
        let context_data = self.analysis.function_context_data();
        self.enter_context(context_data);
    }

    fn enter_block_context(&mut self, id: ast::NodeId) {
        let context_data = self
            .analysis
            .map_block_contexts
            .get(id)
            .cloned()
            .expect("missing context");
        self.enter_context(context_data);
    }

    fn enter_context(&mut self, context_data: LazyContextData) {
        let register = if context_data.has_class_id() {
            Some(self.create_context(context_data.clone()))
        } else {
            None
        };

        self.entered_contexts.push(EnteredContext {
            context_data,
            register,
        });
    }

    fn leave_function_context(&mut self) {
        let context_data = self.analysis.function_context_data();
        self.leave_context(context_data);
    }

    fn leave_block_context(&mut self, id: ast::NodeId) {
        let context_data = self
            .analysis
            .map_block_contexts
            .get(id)
            .cloned()
            .expect("missing context");
        self.leave_context(context_data);
    }

    fn leave_context(&mut self, context_data: LazyContextData) {
        let entered_context = self.entered_contexts.pop().expect("missing context");

        if context_data.has_class_id() {
            assert!(entered_context.register.is_some());
        } else {
            assert!(entered_context.register.is_none());
        }
    }

    fn create_context(&mut self, context_data: LazyContextData) -> Register {
        let class_id = context_data.class_id();

        let context_register = self.builder.alloc_global(BytecodeType::Ptr);
        let idx = self.builder.add_const_cls_types(
            ClassId(class_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&self.identity_type_params()),
        );
        self.builder
            .emit_new_object(context_register, idx, self.loc(self.span));

        if context_data.has_parent_slot() {
            // Load context field of lambda object in self.
            let temp_parent_context_reg = self.alloc_temp(BytecodeType::Ptr);

            let parent_context_reg = if let Some(parent_context_reg) = self.last_context_register()
            {
                parent_context_reg
            } else {
                let self_reg = self.var_reg(SELF_VAR_ID);

                let lambda_cls_id = self.sa.known.classes.lambda();
                let idx = self.builder.add_const_field_types(
                    ClassId(lambda_cls_id.index().try_into().expect("overflow")),
                    BytecodeTypeArray::empty(),
                    0,
                );
                self.builder.emit_load_field(
                    temp_parent_context_reg,
                    self_reg,
                    idx,
                    self.loc(self.span),
                );

                temp_parent_context_reg
            };

            // Store value in parent field of context object.
            assert!(context_data.has_parent_slot());
            let idx = self.builder.add_const_field_types(
                ClassId(class_id.index().try_into().expect("overflow")),
                bty_array_from_ty(&self.identity_type_params()),
                0,
            );
            self.builder.emit_store_field(
                parent_context_reg,
                context_register,
                idx,
                self.loc(self.span),
            );

            self.free_temp(temp_parent_context_reg);
        }

        context_register
    }

    fn visit_stmt(&mut self, stmt: &ast::StmtData) {
        match *stmt {
            ast::StmtData::Expr(ref expr) => self.visit_stmt_expr(expr),
            ast::StmtData::Let(ref stmt) => self.visit_stmt_let(stmt),
        }
    }

    fn setup_pattern_vars(&mut self, pattern: &ast::Pattern) {
        match pattern {
            ast::Pattern::Ident(ref ident) => {
                let ident_type = self.analysis.map_idents.get(ident.id);

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

            ast::Pattern::LitBool(..)
            | ast::Pattern::LitChar(..)
            | ast::Pattern::LitString(..)
            | ast::Pattern::LitInt(..)
            | ast::Pattern::LitFloat(..)
            | ast::Pattern::Underscore(..)
            | ast::Pattern::Rest(..) => {
                // nothing to do
            }

            ast::Pattern::Error(..) => unreachable!(),

            ast::Pattern::ClassOrStructOrEnum(ref p) => {
                if let Some(ref params) = p.params {
                    for param in params {
                        self.setup_pattern_vars(&param.pattern);
                    }
                }
            }

            ast::Pattern::Tuple(ref tuple) => {
                for param in &tuple.params {
                    self.setup_pattern_vars(&param);
                }
            }

            ast::Pattern::Alt(ref p) => {
                // All alternative patterns define the same vars, so just allocate
                // registers for the first subpattern.
                self.setup_pattern_vars(p.alts.first().expect("missing alt"));
            }
        }
    }

    fn setup_pattern_var(&mut self, var_id: VarId) {
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

    fn allocate_register_for_var(&mut self, var_id: VarId) {
        let var = self.analysis.vars.get_var(var_id);
        let bty: BytecodeType = register_bty_from_ty(var.ty.clone());
        let reg = self.alloc_var(bty);
        self.set_var_reg(var_id, reg);
    }

    fn destruct_pattern_or_fail(
        &mut self,
        pattern: &ast::Pattern,
        value: Register,
        ty: SourceType,
    ) {
        let mismatch_lbl = self.destruct_pattern(pattern, value, ty, None);
        if let Some(mismatch_lbl) = mismatch_lbl {
            let merge_lbl = self.builder.create_label();
            self.builder.emit_jump(merge_lbl);
            self.builder.bind_label(mismatch_lbl);
            gen_fatal_error(self, "pattern matching failure", pattern.span());
            self.builder.bind_label(merge_lbl);
        }
    }

    fn destruct_pattern(
        &mut self,
        pattern: &ast::Pattern,
        value: Register,
        ty: SourceType,
        exit: Option<Label>,
    ) -> Option<Label> {
        let mut pck = PatternCheckContext { exit };
        self.destruct_pattern_alt(&mut pck, pattern, value, ty);
        pck.exit
    }

    fn destruct_pattern_alt(
        &mut self,
        pck: &mut PatternCheckContext,
        pattern: &ast::Pattern,
        value: Register,
        ty: SourceType,
    ) {
        match pattern {
            ast::Pattern::Ident(ref ident) => {
                let ident_type = self.analysis.map_idents.get(ident.id);

                match ident_type {
                    Some(IdentType::EnumVariant(enum_id, enum_type_params, variant_id)) => {
                        self.destruct_pattern_enum(
                            pck,
                            pattern,
                            value,
                            ty,
                            *enum_id,
                            enum_type_params,
                            *variant_id,
                        );
                    }

                    Some(IdentType::Var(var_id)) => {
                        self.destruct_pattern_var(pck, pattern, value, ty, *var_id)
                    }

                    _ => unreachable!(),
                }
            }
            ast::Pattern::LitBool(ref p) => {
                let mismatch_lbl = pck.ensure_label(&mut self.builder);
                let p = p.expr.to_lit_bool().expect("expected bool literal");
                if p.value {
                    self.builder.emit_jump_if_false(value, mismatch_lbl);
                } else {
                    self.builder.emit_jump_if_true(value, mismatch_lbl);
                }
            }

            ast::Pattern::LitChar(ref p) => {
                let mismatch_lbl = pck.ensure_label(&mut self.builder);
                let char_value = self.analysis.const_value(p.id).to_char();
                let tmp = self.alloc_temp(BytecodeType::Bool);
                let expected = self.alloc_temp(BytecodeType::Char);
                self.builder.emit_const_char(expected, char_value);
                self.builder.emit_test_eq(tmp, value, expected);
                self.builder.emit_jump_if_false(tmp, mismatch_lbl);
                self.builder.free_temp(tmp);
                self.builder.free_temp(expected);
            }

            ast::Pattern::LitFloat(ref p) => {
                let ty = register_bty_from_ty(ty);
                assert!(ty == BytecodeType::Float32 || ty == BytecodeType::Float64);
                let mismatch_lbl = pck.ensure_label(&mut self.builder);
                let const_value = self
                    .analysis
                    .const_value(p.id)
                    .to_f64()
                    .expect("float expected");
                let tmp = self.alloc_temp(BytecodeType::Bool);
                let expected = self.alloc_temp(ty.clone());
                match ty {
                    BytecodeType::Float32 => self
                        .builder
                        .emit_const_float32(expected, const_value as f32),
                    BytecodeType::Float64 => self.builder.emit_const_float64(expected, const_value),
                    _ => unreachable!(),
                }
                self.builder.emit_test_eq(tmp, value, expected);
                self.builder.emit_jump_if_false(tmp, mismatch_lbl);
                self.builder.free_temp(tmp);
                self.builder.free_temp(expected);
            }

            ast::Pattern::LitString(ref p) => {
                let mismatch_lbl = pck.ensure_label(&mut self.builder);
                let const_value = self
                    .analysis
                    .const_value(p.id)
                    .to_string()
                    .expect("float expected")
                    .to_string();
                let tmp = self.alloc_temp(BytecodeType::Bool);
                let expected = self.alloc_temp(BytecodeType::Ptr);
                self.builder.emit_const_string(expected, const_value);
                let fct_id = self.sa.known.functions.string_equals();
                let idx = self
                    .builder
                    .add_const_fct(FunctionId(fct_id.index().try_into().expect("overflow")));
                self.builder.emit_push_register(value);
                self.builder.emit_push_register(expected);
                self.builder.emit_invoke_direct(tmp, idx, self.loc(p.span));
                self.builder.emit_jump_if_false(tmp, mismatch_lbl);
                self.builder.free_temp(tmp);
                self.builder.free_temp(expected);
            }

            ast::Pattern::LitInt(ref p) => {
                let ty = register_bty_from_ty(ty);
                let mismatch_lbl = pck.ensure_label(&mut self.builder);
                let const_value = self.analysis.const_value(p.id);
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

            ast::Pattern::Underscore(_) => {
                // nothing to do
            }

            ast::Pattern::Error(..) => unreachable!(),

            ast::Pattern::Alt(ref p) => {
                let mut alt_labels = Vec::with_capacity(p.alts.len() + 1);
                let match_lbl = self.builder.create_label();

                for _ in &p.alts {
                    alt_labels.push(self.builder.create_label());
                }

                alt_labels.push(pck.ensure_label(&mut self.builder));

                for (idx, alt) in p.alts.iter().enumerate() {
                    let current_lbl = alt_labels[idx];
                    self.builder.bind_label(current_lbl);

                    let next_lbl = alt_labels[idx + 1];

                    let mut alt_pck = PatternCheckContext {
                        exit: Some(next_lbl),
                    };
                    self.destruct_pattern_alt(&mut alt_pck, alt.as_ref(), value, ty.clone());
                    self.builder.emit_jump(match_lbl);
                }

                self.builder.bind_label(match_lbl);
            }

            ast::Pattern::ClassOrStructOrEnum(ref p) => {
                let ident_type = self.analysis.map_idents.get(p.id).unwrap();

                match ident_type {
                    IdentType::EnumVariant(enum_id, enum_type_params, variant_id) => {
                        self.destruct_pattern_enum(
                            pck,
                            pattern,
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
                            pattern,
                            value,
                            ty,
                            *struct_id,
                            struct_type_params,
                        );
                    }

                    IdentType::Class(class_id, class_type_params) => {
                        self.destruct_pattern_class(
                            pck,
                            pattern,
                            value,
                            ty,
                            *class_id,
                            class_type_params,
                        );
                    }

                    _ => unreachable!(),
                }
            }

            ast::Pattern::Tuple(ref p) => {
                self.destruct_pattern_tuple(pck, p, value, ty);
            }

            ast::Pattern::Rest(..) => unreachable!(),
        }
    }

    fn destruct_pattern_enum(
        &mut self,
        pck: &mut PatternCheckContext,
        pattern: &ast::Pattern,
        value: Register,
        _ty: SourceType,
        enum_id: EnumDefinitionId,
        enum_type_params: &SourceTypeArray,
        variant_idx: u32,
    ) {
        let enum_ = self.sa.enum_(enum_id);

        let bc_enum_id = EnumId(enum_id.index().try_into().expect("overflow"));
        let bc_enum_type_params = bty_array_from_ty(enum_type_params);

        let match_reg = self.alloc_temp(BytecodeType::Bool);
        let actual_variant_reg = self.alloc_temp(BytecodeType::Int32);
        let idx = self
            .builder
            .add_const_enum(bc_enum_id, bc_enum_type_params.clone());
        self.builder.emit_load_enum_variant(
            actual_variant_reg,
            value,
            idx,
            self.loc(pattern.span()),
        );

        let expected_variant_reg = self.alloc_temp(BytecodeType::Int32);
        self.builder
            .emit_const_int32(expected_variant_reg, variant_idx as i32);
        self.builder
            .emit_test_eq(match_reg, actual_variant_reg, expected_variant_reg);
        let lbl = pck.ensure_label(&mut self.builder);
        self.builder.emit_jump_if_false(match_reg, lbl);

        let variant = &enum_.variants[variant_idx as usize];

        iterate_subpatterns(self.analysis, pattern, |idx, param| {
            let element_ty = variant.fields[idx].parsed_type.ty();
            let element_ty = specialize_type(self.sa, element_ty, enum_type_params);
            let ty = register_bty_from_ty(element_ty.clone());
            let field_reg = self.alloc_temp(ty);

            let idx = self.builder.add_const_enum_element(
                bc_enum_id,
                bc_enum_type_params.clone(),
                variant_idx,
                idx as u32,
            );

            self.builder
                .emit_load_enum_element(field_reg, value, idx, self.loc(pattern.span()));

            self.destruct_pattern_alt(pck, &param.pattern, field_reg, element_ty);
            self.free_temp(field_reg);
        });

        self.free_temp(actual_variant_reg);
        self.free_temp(expected_variant_reg);
        self.free_temp(match_reg);
    }

    fn destruct_pattern_struct(
        &mut self,
        pck: &mut PatternCheckContext,
        pattern: &ast::Pattern,
        value: Register,
        _ty: SourceType,
        struct_id: StructDefinitionId,
        struct_type_params: &SourceTypeArray,
    ) {
        let struct_ = self.sa.struct_(struct_id);

        iterate_subpatterns(self.analysis, pattern, |idx, field| {
            let field_ty = struct_.fields[idx].ty();
            let field_ty = specialize_type(self.sa, field_ty, struct_type_params);
            let register_ty = register_bty_from_ty(field_ty.clone());
            let idx = self.builder.add_const_struct_field(
                StructId(struct_id.index().try_into().expect("overflow")),
                bty_array_from_ty(struct_type_params),
                idx as u32,
            );
            let temp_reg = self.alloc_temp(register_ty);
            self.builder.emit_load_struct_field(temp_reg, value, idx);
            self.destruct_pattern_alt(pck, &field.pattern, temp_reg, field_ty);
            self.free_temp(temp_reg);
        })
    }

    fn destruct_pattern_class(
        &mut self,
        pck: &mut PatternCheckContext,
        pattern: &ast::Pattern,
        value: Register,
        _ty: SourceType,
        class_id: ClassDefinitionId,
        class_type_params: &SourceTypeArray,
    ) {
        let class = self.sa.class(class_id);

        iterate_subpatterns(self.analysis, pattern, |idx, field_pattern| {
            let field_ty = class.fields[idx].ty();
            let field_ty = specialize_type(self.sa, field_ty, class_type_params);
            let register_ty = register_bty_from_ty(field_ty.clone());
            let idx = self.builder.add_const_field_types(
                ClassId(class_id.index().try_into().expect("overflow")),
                bty_array_from_ty(class_type_params),
                idx as u32,
            );
            let temp_reg = self.alloc_temp(register_ty);
            self.builder
                .emit_load_field(temp_reg, value, idx, self.loc(pattern.span()));
            self.destruct_pattern_alt(pck, &field_pattern.pattern, temp_reg, field_ty);
            self.free_temp(temp_reg);
        })
    }

    fn destruct_pattern_tuple(
        &mut self,
        pck: &mut PatternCheckContext,
        pattern: &ast::PatternTuple,
        value: Register,
        ty: SourceType,
    ) {
        let subpatterns = pattern.params.as_slice();

        if ty.is_unit() {
            assert!(subpatterns.is_empty());
        } else {
            let tuple_subtypes = ty.tuple_subtypes().expect("tuple expected");

            for subpattern in subpatterns {
                if subpattern.is_rest() || subpattern.is_underscore() {
                    // Do nothing.
                } else {
                    let field_id = self
                        .analysis
                        .map_field_ids
                        .get(subpattern.id())
                        .cloned()
                        .expect("missing field_id");
                    let subtype = tuple_subtypes[field_id].clone();
                    let register_ty = register_bty_from_ty(subtype.clone());
                    let cp_idx = self
                        .builder
                        .add_const_tuple_element(bty_from_ty(ty.clone()), field_id as u32);
                    let temp_reg = self.alloc_temp(register_ty);
                    self.builder
                        .emit_load_tuple_element(temp_reg, value, cp_idx);
                    self.destruct_pattern_alt(pck, &subpattern, temp_reg, subtype);
                    self.free_temp(temp_reg);
                }
            }
        }
    }

    fn destruct_pattern_var(
        &mut self,
        _pck: &mut PatternCheckContext,
        pattern: &ast::Pattern,
        value: Register,
        _ty: SourceType,
        var_id: VarId,
    ) {
        let var = self.analysis.vars.get_var(var_id);

        if !var.ty.is_unit() {
            match var.location {
                VarLocation::Context(scope_id, field_id) => {
                    self.store_in_context(value, scope_id, field_id, self.loc(pattern.span()));
                }

                VarLocation::Stack => {
                    let var_reg = self.var_reg(var_id);
                    self.emit_mov(var_reg, value);
                }
            }
        }
    }

    fn visit_expr_for(&mut self, stmt: &ast::ExprForType, _dest: DataDest) -> Register {
        self.push_scope();
        let for_type_info = self.analysis.map_fors.get(stmt.id).unwrap().clone();

        // Emit: <obj> = <expr> (for <var> in <expr> { ... })
        let object_reg = gen_expr(self, &stmt.expr, DataDest::Alloc);

        let iterator_reg = if let Some((iter_fct_id, iter_type_params)) = for_type_info.iter {
            // Emit: <iterator> = <obj>.iter();
            let iterator_reg = self.alloc_var(BytecodeType::Ptr);
            self.builder.emit_push_register(object_reg);
            let fct_idx = self.builder.add_const_fct_types(
                FunctionId(iter_fct_id.index().try_into().expect("overflow")),
                bty_array_from_ty(&iter_type_params),
            );
            self.builder
                .emit_invoke_direct(iterator_reg, fct_idx, self.loc(stmt.expr.span()));
            iterator_reg
        } else {
            // Object is already the iterator - just use it
            object_reg
        };

        let lbl_cond = self.builder.define_label();
        self.builder.emit_loop_start();

        self.enter_block_context(stmt.id);

        let iterator_type = for_type_info.iterator_type.clone();
        let iterator_type_params = bty_array_from_ty(&iterator_type.type_params());

        self.builder.emit_push_register(iterator_reg);

        let lbl_end = self.builder.create_label();

        let value_ty = for_type_info.value_type.clone();
        let option_type_params = SourceTypeArray::single(value_ty.clone());

        // Emit: <next-temp> = <iterator>.next()
        let next_result_ty = register_bty_from_ty(for_type_info.next_type.clone());
        let next_result_reg = self.alloc_temp(next_result_ty);

        let fct_idx = self.builder.add_const_fct_types(
            FunctionId(
                for_type_info
                    .next
                    .expect("missing fct id")
                    .index()
                    .try_into()
                    .expect("overflow"),
            ),
            iterator_type_params,
        );

        self.builder.emit_push_register(iterator_reg);
        self.emit_invoke_direct(
            for_type_info.next_type.clone(),
            next_result_reg,
            fct_idx,
            self.loc(stmt.expr.span()),
        );

        // Emit: if <next-result>.isNone() then goto lbl_end
        let cond_reg = self.alloc_temp(BytecodeType::Bool);
        let fct_idx = self.builder.add_const_fct_types(
            FunctionId(
                self.sa
                    .known
                    .functions
                    .option_is_none()
                    .index()
                    .try_into()
                    .expect("overflow"),
            ),
            bty_array_from_ty(&option_type_params),
        );
        self.builder.emit_push_register(next_result_reg);
        self.builder
            .emit_invoke_direct(cond_reg, fct_idx, self.loc(stmt.expr.span()));
        self.builder.emit_jump_if_true(cond_reg, lbl_end);
        self.free_temp(cond_reg);

        // Emit: <value-reg> = <next-result>.unwrap()
        if value_ty.is_unit() {
            self.free_temp(next_result_reg);
        } else {
            let value_ty = register_bty_from_ty(value_ty);
            let value_reg = self.alloc_var(value_ty);
            let fct_idx = self.builder.add_const_fct_types(
                FunctionId(
                    self.sa
                        .known
                        .functions
                        .option_unwrap()
                        .index()
                        .try_into()
                        .expect("overflow"),
                ),
                bty_array_from_ty(&option_type_params),
            );
            self.builder.emit_push_register(next_result_reg);
            self.builder
                .emit_invoke_direct(value_reg, fct_idx, self.loc(stmt.expr.span()));
            self.free_temp(next_result_reg);

            self.setup_pattern_vars(&stmt.pattern);
            self.destruct_pattern_or_fail(&stmt.pattern, value_reg, for_type_info.value_type);
        }

        self.loops.push(LoopLabels::new(lbl_cond, lbl_end));
        self.emit_expr_for_effect(&stmt.block);
        self.loops.pop().unwrap();

        self.builder.emit_jump_loop(lbl_cond);
        self.builder.bind_label(lbl_end);

        self.leave_block_context(stmt.id);
        self.pop_scope();

        self.free_if_temp(object_reg);
        self.ensure_unit_register()
    }

    fn visit_stmt_let(&mut self, stmt: &ast::StmtLetType) {
        self.setup_pattern_vars(&stmt.pattern);

        if let Some(ref expr) = stmt.expr {
            let ty = self.ty(expr.id());
            let value = gen_expr(self, expr, DataDest::Alloc);
            self.destruct_pattern_or_fail(&stmt.pattern, value, ty);
            self.free_if_temp(value);
        }
    }

    fn visit_expr_while(&mut self, stmt: &ast::ExprWhileType, _dest: DataDest) -> Register {
        let cond_lbl = self.builder.define_label();
        let end_lbl = self.builder.create_label();
        self.builder.emit_loop_start();
        self.enter_block_context(stmt.id);

        gen_expr_condition(self, &stmt.cond, end_lbl);

        self.loops.push(LoopLabels::new(cond_lbl, end_lbl));
        self.emit_expr_for_effect(&stmt.block);
        self.loops.pop().unwrap();
        self.builder.emit_jump_loop(cond_lbl);
        self.builder.bind_label(end_lbl);
        self.leave_block_context(stmt.id);
        self.ensure_unit_register()
    }

    fn visit_stmt_expr(&mut self, stmt: &ast::StmtExprType) {
        let reg = gen_expr(self, &stmt.expr, DataDest::Alloc);
        self.free_if_temp(reg);
    }

    fn visit_expr_return(&mut self, ret: &ast::ExprReturnType, _dest: DataDest) -> Register {
        let result_reg = if let Some(ref expr) = ret.expr {
            gen_expr(self, expr, DataDest::Alloc)
        } else {
            self.ensure_unit_register()
        };

        self.builder.emit_ret(result_reg);
        self.free_if_temp(result_reg);

        self.ensure_unit_register()
    }

    fn visit_expr_break(&mut self, _stmt: &ast::ExprBreakType, _dest: DataDest) -> Register {
        let end = self.loops.last().unwrap().end;
        self.builder.emit_jump(end);
        self.ensure_unit_register()
    }

    fn visit_expr_continue(&mut self, _stmt: &ast::ExprContinueType, _dest: DataDest) -> Register {
        let cond = self.loops.last().unwrap().cond;
        self.builder.emit_jump_loop(cond);
        self.ensure_unit_register()
    }

    fn emit_expr_for_effect(&mut self, expr: &ast::ExprData) {
        let reg = gen_expr(self, expr, DataDest::Alloc);
        self.free_if_temp(reg);
    }

    fn visit_expr_type_param(&mut self, expr: &ast::ExprTypeParamType, dest: DataDest) -> Register {
        let ident_type = self.analysis.map_idents.get(expr.id).cloned().unwrap();

        match ident_type {
            IdentType::EnumVariant(enum_id, type_params, variant_idx) => {
                self.emit_new_enum(enum_id, type_params, variant_idx, self.loc(expr.span), dest)
            }

            _ => unreachable!(),
        }
    }

    fn visit_expr_template(&mut self, expr: &ast::ExprTemplateType, dest: DataDest) -> Register {
        let buffer_register = self.ensure_register(dest, BytecodeType::Ptr);

        // build StringBuffer::empty() call
        let fct_id = self.sa.known.functions.string_buffer_empty();
        let fct_idx = self
            .builder
            .add_const_fct(FunctionId(fct_id.index().try_into().expect("overflow")));
        self.builder
            .emit_invoke_static(buffer_register, fct_idx, self.loc(expr.span));

        let part_register = self.alloc_temp(BytecodeType::Ptr);

        for part in &expr.parts {
            if let Some(ref lit_str) = part.to_lit_str() {
                let value = self
                    .analysis
                    .const_value(lit_str.id)
                    .to_string()
                    .expect("string expected")
                    .to_string();
                self.builder.emit_const_string(part_register, value);
            } else {
                let ty = self.ty(part.id());

                if ty.cls_id() == Some(self.sa.known.classes.string()) {
                    gen_expr(self, part, DataDest::Reg(part_register));
                } else if ty.is_type_param() {
                    let type_list_id = match ty {
                        SourceType::TypeParam(id) => id,
                        _ => unreachable!(),
                    };

                    let expr_register = gen_expr(self, part, DataDest::Alloc);
                    self.builder.emit_push_register(expr_register);

                    // build toString() call
                    let name = self.sa.interner.intern("toString");
                    let trait_id = self.sa.known.traits.stringable();
                    let trait_ = self.sa.trait_(trait_id);
                    let to_string_id = trait_
                        .get_method(name, false)
                        .expect("Stringable::toString() not found");

                    let fct_idx = self.builder.add_const(ConstPoolEntry::Generic(
                        type_list_id.index() as u32,
                        FunctionId(to_string_id.index().try_into().expect("overflow")),
                        BytecodeTypeArray::empty(),
                        BytecodeTypeArray::empty(),
                    ));

                    self.builder.emit_invoke_generic_direct(
                        part_register,
                        fct_idx,
                        self.loc(part.span()),
                    );

                    self.free_if_temp(expr_register);
                } else {
                    let expr_register = gen_expr(self, part, DataDest::Alloc);
                    self.builder.emit_push_register(expr_register);

                    // build toString() call
                    let (to_string_id, type_params) = self
                        .analysis
                        .map_templates
                        .get(part.id())
                        .expect("missing toString id");

                    let type_params = bty_array_from_ty(&type_params);

                    let fct_idx = self.builder.add_const_fct_types(
                        FunctionId(to_string_id.index().try_into().expect("overflow")),
                        type_params,
                    );
                    self.builder
                        .emit_invoke_direct(part_register, fct_idx, self.loc(part.span()));

                    self.free_if_temp(expr_register);
                }
            }

            // build StringBuffer::append() call
            let fct_id = self.sa.known.functions.string_buffer_append();
            let fct_idx = self
                .builder
                .add_const_fct(FunctionId(fct_id.index().try_into().expect("overflow")));
            self.builder.emit_push_register(buffer_register);
            self.builder.emit_push_register(part_register);
            let dest_reg = self.ensure_unit_register();
            self.builder
                .emit_invoke_direct(dest_reg, fct_idx, self.loc(expr.span));
        }

        self.free_temp(part_register);

        // build StringBuffer::toString() call
        let fct_id = self.sa.known.functions.string_buffer_to_string();
        let fct_idx = self
            .builder
            .add_const_fct(FunctionId(fct_id.index().try_into().expect("overflow")));
        self.builder.emit_push_register(buffer_register);
        self.builder
            .emit_invoke_direct(buffer_register, fct_idx, self.loc(expr.span));

        buffer_register
    }

    fn visit_expr_path(&mut self, expr: &ast::ExprPathType, dest: DataDest) -> Register {
        let ident_type = self.analysis.map_idents.get(expr.id).cloned().unwrap();

        match ident_type {
            IdentType::EnumVariant(enum_id, type_params, variant_idx) => {
                self.emit_new_enum(enum_id, type_params, variant_idx, self.loc(expr.span), dest)
            }

            IdentType::Const(const_id) => self.visit_expr_ident_const(const_id, dest),

            IdentType::Global(global_id) => {
                self.visit_expr_ident_global(global_id, dest, self.loc(expr.span))
            }

            IdentType::StructField(..)
            | IdentType::Struct(..)
            | IdentType::Field(..)
            | IdentType::Fct(..)
            | IdentType::Class(..)
            | IdentType::Var(..)
            | IdentType::Context(..) => unreachable!(),
        }
    }

    fn emit_new_enum(
        &mut self,
        enum_id: EnumDefinitionId,
        type_params: SourceTypeArray,
        variant_idx: u32,
        location: Location,
        dest: DataDest,
    ) -> Register {
        let type_params = bty_array_from_ty(&type_params);
        let enum_id = EnumId(enum_id.index().try_into().expect("overflow"));
        let bty = BytecodeType::Enum(enum_id, type_params.clone());
        let dest = self.ensure_register(dest, bty);
        let idx = self
            .builder
            .add_const_enum_variant(enum_id, type_params, variant_idx);
        self.builder.emit_new_enum(dest, idx, location);
        dest
    }

    fn visit_expr_conv(&mut self, expr: &ast::ExprConvType, dest: DataDest) -> Register {
        let object_type = self.ty(expr.object.id());
        let check_type = self.ty(expr.data_type.id());
        assert!(check_type.is_trait_object());

        let check_type = bty_from_ty(check_type);

        let object = gen_expr(self, &expr.object, DataDest::Alloc);
        let idx = self
            .builder
            .add_const_trait(check_type.clone(), bty_from_ty(object_type));
        let dest = self.ensure_register(dest, check_type);
        self.builder
            .emit_new_trait_object(dest, idx, object, self.loc(expr.span));
        self.free_if_temp(object);
        dest
    }

    fn visit_expr_is(&mut self, node: &ast::ExprIsType, dest: DataDest) -> Register {
        let ty = self.ty(node.value.id());
        let value_reg = gen_expr(self, &node.value, DataDest::Alloc);

        self.push_scope();
        let mismatch_lbl = self.builder.create_label();
        let merge_lbl = self.builder.create_label();
        self.destruct_pattern(&node.pattern, value_reg, ty, Some(mismatch_lbl));
        let dest = self.ensure_register(dest, BytecodeType::Bool);
        self.builder.emit_const_true(dest);
        self.builder.emit_jump(merge_lbl);
        self.builder.bind_label(mismatch_lbl);
        self.builder.emit_const_false(dest);
        self.builder.bind_label(merge_lbl);
        self.pop_scope();

        self.free_if_temp(value_reg);

        dest
    }

    fn last_context_register(&self) -> Option<Register> {
        self.entered_contexts
            .iter()
            .rev()
            .find_map(|ec| ec.register)
    }

    fn visit_expr_lambda(&mut self, node: &ast::Function, dest: DataDest) -> Register {
        let dest = self.ensure_register(dest, BytecodeType::Ptr);

        let lambda_fct_id = self
            .analysis
            .map_lambdas
            .get(node.id)
            .expect("missing lambda id")
            .fct_id();

        let lambda_fct = self.sa.fct(lambda_fct_id);
        let lambda_analysis = lambda_fct.analysis();

        let mut outer_context_reg: Option<Register> = None;

        if lambda_analysis.needs_context_slot_in_lambda_object() {
            if let Some(context_register) = self.last_context_register() {
                self.builder.emit_push_register(context_register.clone());
            } else {
                // This lambda doesn't have a context object on its own, simply
                // pass down the parent context (the context in the lambda object).
                assert!(self.is_lambda);
                assert!(self.analysis.needs_context_slot_in_lambda_object());
                outer_context_reg = Some(self.alloc_temp(BytecodeType::Ptr));
                let lambda_cls_id = self.sa.known.classes.lambda();
                let idx = self.builder.add_const_field_types(
                    ClassId(lambda_cls_id.index().try_into().expect("overflow")),
                    BytecodeTypeArray::empty(),
                    0,
                );
                self.builder.emit_load_field(
                    outer_context_reg.expect("missing reg"),
                    self.var_reg(SELF_VAR_ID),
                    idx,
                    self.loc(node.span),
                );
                self.builder
                    .emit_push_register(outer_context_reg.expect("missing reg"));
            }
        }

        let idx = self.builder.add_const_fct_types(
            FunctionId(lambda_fct_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&self.identity_type_params()),
        );
        self.builder.emit_new_lambda(dest, idx, self.loc(node.span));

        if let Some(outer_context_reg) = outer_context_reg {
            self.free_if_temp(outer_context_reg);
        }

        dest
    }

    fn visit_expr_if(&mut self, expr: &ast::ExprIfType, dest: DataDest) -> Register {
        let ty = self.ty(expr.id);

        let dest = self.ensure_register(dest, register_bty_from_ty(ty));
        let else_lbl = self.builder.create_label();

        self.push_scope();

        gen_expr_condition(self, &expr.cond, else_lbl);

        gen_expr(self, &expr.then_block, DataDest::Reg(dest));

        self.pop_scope();

        if let Some(ref else_block) = expr.else_block {
            let end_lbl = self.builder.create_label();

            if !expr_always_returns(&expr.then_block) {
                self.builder.emit_jump(end_lbl);
            }

            self.builder.bind_label(else_lbl);
            gen_expr(self, else_block, DataDest::Reg(dest));
            self.builder.bind_label(end_lbl);
        } else {
            self.builder.bind_label(else_lbl);
        }

        dest
    }

    fn visit_expr_block(&mut self, block: &ast::ExprBlockType, dest: DataDest) -> Register {
        self.push_scope();

        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }

        let result = if let Some(ref expr) = block.expr {
            gen_expr(self, expr, dest)
        } else {
            self.ensure_unit_register()
        };

        self.pop_scope();

        result
    }

    fn visit_expr_dot(&mut self, expr: &ast::ExprDotType, dest: DataDest) -> Register {
        let object_ty = self.ty(expr.lhs.id());

        if object_ty.is_tuple() {
            return self.visit_expr_dot_tuple(expr, object_ty, dest);
        }

        if let Some((struct_id, type_params)) = object_ty.to_struct() {
            return self.visit_expr_dot_struct(expr, struct_id, type_params, dest);
        }

        let (cls_ty, field_id) = {
            let ident_type = self.analysis.map_idents.get(expr.id).unwrap();

            match ident_type {
                IdentType::Field(ty, field) => (ty.clone(), *field),
                _ => unreachable!(),
            }
        };

        let (cls_id, type_params) = cls_ty.to_class().expect("class expected");

        let field_idx = self.builder.add_const_field_types(
            ClassId(cls_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&type_params),
            field_id.0 as u32,
        );

        let field_ty = self.ty(expr.id);

        let field_bc_ty: BytecodeType = register_bty_from_ty(field_ty);
        let dest = self.ensure_register(dest, field_bc_ty);
        let obj = gen_expr(self, &expr.lhs, DataDest::Alloc);

        self.builder
            .emit_load_field(dest, obj, field_idx, self.loc(expr.op_span));
        self.free_if_temp(obj);

        dest
    }

    fn visit_expr_dot_struct(
        &mut self,
        expr: &ast::ExprDotType,
        struct_id: StructDefinitionId,
        type_params: SourceTypeArray,
        dest: DataDest,
    ) -> Register {
        let struct_obj = gen_expr(self, &expr.lhs, DataDest::Alloc);

        let ident_type = self.analysis.map_idents.get(expr.id).unwrap();

        let field_idx = match ident_type {
            IdentType::StructField(_, field_idx) => *field_idx,
            _ => unreachable!(),
        };

        let fty = self.ty(expr.id);

        if fty.is_unit() {
            self.free_if_temp(struct_obj);
            return self.ensure_unit_register();
        }

        let ty: BytecodeType = register_bty_from_ty(fty);
        let dest = self.ensure_register(dest, ty);
        let const_idx = self.builder.add_const_struct_field(
            StructId(struct_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&type_params),
            field_idx.0 as u32,
        );
        self.builder
            .emit_load_struct_field(dest, struct_obj, const_idx);

        self.free_if_temp(struct_obj);

        dest
    }

    fn visit_expr_dot_tuple(
        &mut self,
        expr: &ast::ExprDotType,
        tuple_ty: SourceType,
        dest: DataDest,
    ) -> Register {
        let tuple = gen_expr(self, &expr.lhs, DataDest::Alloc);
        let value_i64 = self
            .analysis
            .const_value(expr.rhs.id())
            .to_i64()
            .expect("integer expected");
        let idx: u32 = value_i64.try_into().expect("too large");

        let subtypes: SourceTypeArray = tuple_ty.tuple_subtypes().expect("tuple expected");
        let ty = subtypes[idx as usize].clone();

        let ty: BytecodeType = register_bty_from_ty(ty);
        let dest = self.ensure_register(dest, ty);
        let idx = self
            .builder
            .add_const_tuple_element(bty_from_ty(tuple_ty), idx);
        self.builder.emit_load_tuple_element(dest, tuple, idx);

        self.free_if_temp(tuple);

        dest
    }

    fn visit_expr_assert(&mut self, expr: &ast::ExprCallType, _dest: DataDest) -> Register {
        let assert_reg = gen_expr(self, &expr.args[0].expr, DataDest::Alloc);
        self.builder.emit_push_register(assert_reg);
        let fid = self.sa.known.functions.assert();
        let idx = self
            .builder
            .add_const_fct(FunctionId(fid.index().try_into().expect("overflow")));
        let dest = self.ensure_unit_register();
        self.builder
            .emit_invoke_static(dest, idx, self.loc(expr.span));
        self.free_if_temp(assert_reg);
        dest
    }

    fn visit_expr_call(&mut self, expr: &ast::ExprCallType, dest: DataDest) -> Register {
        if let Some(info) = self.get_intrinsic(expr.id) {
            if emit_as_bytecode_operation(info.intrinsic) {
                return self.visit_expr_call_intrinsic(expr, info, dest);
            }
        }

        let call_type = self.analysis.map_calls.get(expr.id).unwrap().clone();

        match *call_type {
            CallType::NewEnum(ref enum_ty, variant_idx) => {
                return self.visit_expr_call_enum(expr, enum_ty.clone(), variant_idx, dest);
            }

            CallType::NewStruct(struct_id, ref type_params) => {
                return self.visit_expr_call_struct(expr, struct_id, type_params, dest);
            }

            CallType::NewClass(cls_id, ref type_params) => {
                return self.visit_expr_call_class(expr, cls_id, type_params, dest);
            }

            CallType::Lambda(ref params, ref return_type) => {
                return self.visit_expr_call_lambda(
                    expr,
                    params.clone(),
                    return_type.clone(),
                    dest,
                );
            }

            CallType::Expr(..)
            | CallType::Method(..)
            | CallType::GenericMethod(..)
            | CallType::GenericStaticMethod(..)
            | CallType::GenericMethodSelf(..)
            | CallType::GenericStaticMethodSelf(..)
            | CallType::TraitObjectMethod(..)
            | CallType::Fct(..) => {}

            _ => panic!("unknown call type = {:?}", call_type),
        }

        // Find method that is called
        let callee_id = call_type.fct_id().expect("FctId missing");
        let callee = self.sa.fct(callee_id);

        let callee_idx = self.add_const_pool_entry_for_call(&callee, &call_type);

        // Determine types for arguments and return values
        let (arg_types, _return_type) = self.determine_callee_types(&call_type, &*callee);
        let return_type = self.analysis.ty(expr.id);

        // Allocate register for result
        let return_reg = self.ensure_register(dest, register_bty_from_ty(return_type.clone()));

        // Evaluate object/self argument
        let object_argument = self.emit_call_object_argument(expr, &call_type);

        // Evaluate function arguments
        let arguments = self.emit_call_arguments(expr, &*callee, &call_type, &arg_types);

        if let Some(obj_reg) = object_argument {
            self.builder.emit_push_register(obj_reg);
        }
        for &arg_reg in &arguments {
            self.builder.emit_push_register(arg_reg);
        }

        // Emit the actual Invoke(Direct|Static|Virtual)XXX instruction
        self.emit_call_inst(return_reg, callee_idx, &call_type, self.loc(expr.span));

        if let Some(obj_reg) = object_argument {
            self.free_if_temp(obj_reg);
        }

        for arg_reg in arguments {
            self.free_if_temp(arg_reg);
        }

        return_reg
    }

    fn visit_expr_call_enum(
        &mut self,
        expr: &ast::ExprCallType,
        enum_ty: SourceType,
        variant_idx: u32,
        dest: DataDest,
    ) -> Register {
        let mut arguments = Vec::new();

        for arg in &expr.args {
            arguments.push(gen_expr(self, &arg.expr, DataDest::Alloc));
        }

        for &arg_reg in &arguments {
            self.builder.emit_push_register(arg_reg);
        }

        let (enum_id, type_params) = enum_ty.to_enum().expect("enum expected");

        let idx = self.builder.add_const_enum_variant(
            EnumId(enum_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&type_params),
            variant_idx,
        );
        let bytecode_ty = register_bty_from_ty(enum_ty);
        let dest_reg = self.ensure_register(dest, bytecode_ty);
        self.builder
            .emit_new_enum(dest_reg, idx, self.loc(expr.span));

        for arg_reg in arguments {
            self.free_if_temp(arg_reg);
        }

        dest_reg
    }

    fn visit_expr_call_lambda(
        &mut self,
        node: &ast::ExprCallType,
        params: SourceTypeArray,
        return_type: SourceType,
        dest: DataDest,
    ) -> Register {
        let mut arguments = Vec::new();

        let lambda_object = gen_expr(self, node.callee(), DataDest::Alloc);
        arguments.push(lambda_object);

        for arg in &node.args {
            arguments.push(gen_expr(self, &arg.expr, DataDest::Alloc));
        }

        for &arg_reg in &arguments {
            self.builder.emit_push_register(arg_reg);
        }

        let idx = self
            .builder
            .add_const_lambda(bty_array_from_ty(&params), bty_from_ty(return_type.clone()));

        let dest_reg = if return_type.is_unit() {
            let dest = self.ensure_unit_register();
            self.builder
                .emit_invoke_lambda(dest, idx, self.loc(node.span));
            dest
        } else {
            let bytecode_ty = register_bty_from_ty(return_type);
            let dest_reg = self.ensure_register(dest, bytecode_ty);
            self.builder
                .emit_invoke_lambda(dest_reg, idx, self.loc(node.span));
            dest_reg
        };

        for arg_reg in arguments {
            self.free_if_temp(arg_reg);
        }

        dest_reg
    }

    fn visit_expr_call_struct(
        &mut self,
        expr: &ast::ExprCallType,
        struct_id: StructDefinitionId,
        type_params: &SourceTypeArray,
        dest: DataDest,
    ) -> Register {
        let mut arguments = Vec::new();

        for arg in &expr.args {
            arguments.push(gen_expr(self, &arg.expr, DataDest::Alloc));
        }

        for &arg_reg in &arguments {
            self.builder.emit_push_register(arg_reg);
        }

        let struct_id = StructId(struct_id.index().try_into().expect("overflow"));

        let idx = self
            .builder
            .add_const_struct(struct_id, bty_array_from_ty(&type_params));
        let bytecode_ty = BytecodeType::Struct(struct_id, bty_array_from_ty(type_params));
        let dest_reg = self.ensure_register(dest, bytecode_ty);
        self.builder
            .emit_new_struct(dest_reg, idx, self.loc(expr.span));

        for arg_reg in arguments {
            self.free_if_temp(arg_reg);
        }

        dest_reg
    }

    fn visit_expr_call_class(
        &mut self,
        expr: &ast::ExprCallType,
        cls_id: ClassDefinitionId,
        type_params: &SourceTypeArray,
        dest: DataDest,
    ) -> Register {
        let mut arguments: Vec<Option<Register>> = vec![None; expr.args.len()];

        for arg in &expr.args {
            let reg = gen_expr(self, &arg.expr, DataDest::Alloc);
            let target_idx = self
                .analysis
                .map_argument
                .get(arg.id)
                .expect("missing argument idx")
                .clone();

            arguments[target_idx] = Some(reg);
        }

        for &arg_reg in &arguments {
            let arg_reg = arg_reg.expect("missing register");
            self.builder.emit_push_register(arg_reg);
        }

        let cls_id = ClassId(cls_id.index().try_into().expect("overflow"));
        let idx = self
            .builder
            .add_const_cls_types(cls_id, bty_array_from_ty(type_params));
        let dest_reg = self.ensure_register(dest, BytecodeType::Ptr);
        self.builder
            .emit_new_object_initialized(dest_reg, idx, self.loc(expr.span));

        for arg_reg in arguments {
            self.free_if_temp(arg_reg.expect("missing register"));
        }

        dest_reg
    }

    fn determine_callee_types(
        &mut self,
        call_type: &CallType,
        fct: &FctDefinition,
    ) -> (Vec<SourceType>, SourceType) {
        let return_type = self.specialize_type_for_call(&call_type, fct.return_type());

        let mut arg_types = Vec::with_capacity(fct.params_with_self().len());

        if fct.has_hidden_self_argument() {
            let self_type = match call_type {
                CallType::TraitObjectMethod(trait_ty, _) => {
                    // trait methods use Self as type for self argument but specialize_type_for_call can't handle Self.
                    assert!(fct.params_with_self()[0].ty().is_self() && !fct.is_static);
                    trait_ty.clone()
                }
                _ => {
                    let arg = fct.params_with_self()[0].ty().clone();
                    self.specialize_type_for_call(&call_type, arg.clone())
                }
            };

            arg_types.push(self_type);
        }

        for arg in fct.params_without_self() {
            let arg = self.specialize_type_for_call(&call_type, arg.ty());
            arg_types.push(arg);
        }

        (arg_types, return_type)
    }

    fn emit_call_object_argument(
        &mut self,
        expr: &ast::ExprCallType,
        call_type: &CallType,
    ) -> Option<Register> {
        match *call_type {
            CallType::Method(..)
            | CallType::GenericMethod(..)
            | CallType::GenericMethodSelf(..)
            | CallType::TraitObjectMethod(..) => {
                let obj_expr = expr.object().unwrap_or(expr.callee());
                let reg = gen_expr(self, obj_expr, DataDest::Alloc);

                Some(reg)
            }
            CallType::Expr(_, _, _) => Some(gen_expr(self, &expr.callee, DataDest::Alloc)),
            CallType::GenericStaticMethod(..)
            | CallType::GenericStaticMethodSelf(..)
            | CallType::Fct(..) => None,
            _ => panic!("unexpected call type {:?}", call_type),
        }
    }

    fn emit_call_arguments(
        &mut self,
        expr: &ast::ExprCallType,
        callee: &FctDefinition,
        call_type: &CallType,
        arg_types: &[SourceType],
    ) -> Vec<Register> {
        let mut registers = Vec::new();

        // self was already emitted, needs to be ignored here.
        let arg_start_offset = match *call_type {
            CallType::Expr(..) | CallType::Method(..) | CallType::GenericMethod(..) => 1,
            _ => 0,
        };

        // Calculate number of non-variadic arguments
        let non_variadic_arguments = if callee.params.is_variadic() {
            arg_types.len() - arg_start_offset - 1
        } else {
            arg_types.len()
        };

        // Evaluate non-variadic arguments and track registers.
        for arg in expr.args.iter().take(non_variadic_arguments) {
            let reg = gen_expr(self, &arg.expr, DataDest::Alloc);
            registers.push(reg);
        }

        if callee.params.is_variadic() {
            let array_reg = self.emit_array_with_variadic_arguments(
                expr,
                arg_types,
                non_variadic_arguments,
                DataDest::Alloc,
            );
            registers.push(array_reg);
        }

        registers
    }

    fn emit_array_with_variadic_arguments(
        &mut self,
        expr: &ast::ExprCallType,
        arg_types: &[SourceType],
        non_variadic_arguments: usize,
        dest: DataDest,
    ) -> Register {
        let variadic_arguments = expr.args.len() - non_variadic_arguments;

        // We need array of elements
        let element_ty = arg_types.last().cloned().unwrap();
        let ty = self.sa.known.array_ty(element_ty.clone());
        let (cls_id, type_params) = ty.to_class().expect("class expected");
        let cls_idx = self.builder.add_const_cls_types(
            ClassId(cls_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&type_params),
        );

        // Store length in a register
        let length_reg = self.alloc_temp(BytecodeType::Int64);
        self.builder
            .emit_const_int64(length_reg, variadic_arguments as i64);

        // Allocate array of given length
        let array_reg = self.ensure_register(dest, BytecodeType::Ptr);
        self.builder
            .emit_new_array(array_reg, length_reg, cls_idx, self.loc(expr.span));

        let index_reg = self.alloc_temp(BytecodeType::Int64);

        // Evaluate rest arguments and store them in array
        for (idx, arg) in expr.args.iter().skip(non_variadic_arguments).enumerate() {
            let arg_reg = gen_expr(self, &arg.expr, DataDest::Alloc);
            self.builder.emit_const_int64(index_reg, idx as i64);
            self.builder
                .emit_store_array(arg_reg, array_reg, index_reg, self.loc(expr.span));
            self.free_if_temp(arg_reg);
        }

        self.free_if_temp(index_reg);
        self.free_if_temp(length_reg);

        array_reg
    }

    fn emit_call_inst(
        &mut self,
        return_reg: Register,
        callee_idx: ConstPoolIdx,
        call_type: &CallType,
        location: Location,
    ) {
        match *call_type {
            CallType::Method(..) => {
                self.builder
                    .emit_invoke_direct(return_reg, callee_idx, location);
            }
            CallType::Fct(..) => {
                self.builder
                    .emit_invoke_static(return_reg, callee_idx, location);
            }
            CallType::Expr(..) => {
                self.builder
                    .emit_invoke_direct(return_reg, callee_idx, location);
            }
            CallType::TraitObjectMethod(..) => {
                self.builder
                    .emit_invoke_virtual(return_reg, callee_idx, location);
            }
            CallType::GenericMethod(..) | CallType::GenericMethodSelf(..) => {
                self.builder
                    .emit_invoke_generic_direct(return_reg, callee_idx, location);
            }
            CallType::GenericStaticMethod(..) | CallType::GenericStaticMethodSelf(..) => {
                self.builder
                    .emit_invoke_generic_static(return_reg, callee_idx, location);
            }
            CallType::NewClass(..)
            | CallType::NewStruct(..)
            | CallType::NewEnum(..)
            | CallType::Intrinsic(..)
            | CallType::Lambda(..) => unreachable!(),
        }
    }

    fn emit_mov(&mut self, dest: Register, src: Register) {
        if dest != src {
            self.builder.emit_mov(dest, src);
        }
    }

    fn emit_invoke_direct(
        &mut self,
        return_type: SourceType,
        return_reg: Register,
        callee_id: ConstPoolIdx,
        location: Location,
    ) {
        if return_type.is_unit() {
            let reg = self.ensure_unit_register();
            self.builder.emit_invoke_direct(reg, callee_id, location);
        } else {
            self.builder
                .emit_invoke_direct(return_reg, callee_id, location);
        }
    }

    fn emit_invoke_generic_direct(
        &mut self,
        return_type: SourceType,
        return_reg: Register,
        callee_id: ConstPoolIdx,
        location: Location,
    ) {
        if return_type.is_unit() {
            let dest = self.ensure_unit_register();
            self.builder
                .emit_invoke_generic_direct(dest, callee_id, location);
        } else {
            self.builder
                .emit_invoke_generic_direct(return_reg, callee_id, location);
        }
    }

    fn visit_expr_self(&mut self, expr: &ast::ExprSelfType, dest: DataDest) -> Register {
        if self.is_lambda {
            let ident = self
                .analysis
                .map_idents
                .get(expr.id)
                .expect("missing ident");
            let (level, context_idx) = match ident {
                IdentType::Context(level, context_idx) => (*level, *context_idx),
                _ => unreachable!(),
            };
            self.visit_expr_ident_context(level, context_idx, dest, self.loc(expr.span))
        } else {
            let var_reg = self.var_reg(SELF_VAR_ID);

            if dest.is_alloc() {
                return var_reg;
            }

            let dest = dest.reg();

            self.emit_mov(dest, var_reg);

            dest
        }
    }

    fn visit_expr_lit_char(&mut self, lit: &ast::ExprLitCharType, dest: DataDest) -> Register {
        let dest = self.ensure_register(dest, BytecodeType::Char);

        let value = self.analysis.const_value(lit.id).to_char();
        self.builder.emit_const_char(dest, value);

        dest
    }

    fn visit_expr_lit_int(
        &mut self,
        lit: &ast::ExprLitIntType,
        dest: DataDest,
        _neg: bool,
    ) -> Register {
        let ty = self.analysis.ty(lit.id);
        let value = self.analysis.const_value(lit.id);

        let ty = match ty {
            SourceType::UInt8 => BytecodeType::UInt8,
            SourceType::Int32 => BytecodeType::Int32,
            SourceType::Int64 => BytecodeType::Int64,
            SourceType::Float32 => {
                let dest = self.ensure_register(dest, BytecodeType::Float32);
                self.builder
                    .emit_const_float32(dest, value.to_f64().expect("float expected") as f32);
                return dest;
            }
            SourceType::Float64 => {
                let dest = self.ensure_register(dest, BytecodeType::Float64);
                self.builder
                    .emit_const_float64(dest, value.to_f64().expect("float expected"));
                return dest;
            }
            _ => unreachable!(),
        };

        let dest = self.ensure_register(dest, ty.clone());
        let value_i64 = value.to_i64().expect("integer expected");

        match ty {
            BytecodeType::UInt8 => self.builder.emit_const_uint8(dest, value_i64 as u8),
            BytecodeType::Int32 => self.builder.emit_const_int32(dest, value_i64 as i32),
            BytecodeType::Int64 => self.builder.emit_const_int64(dest, value_i64),
            _ => unreachable!(),
        }

        dest
    }

    fn visit_expr_lit_float(&mut self, lit: &ast::ExprLitFloatType, dest: DataDest) -> Register {
        let ty = self.analysis.ty(lit.id);
        let value_f64 = self
            .analysis
            .const_value(lit.id)
            .to_f64()
            .expect("float expected");

        let ty = match ty {
            SourceType::Float32 => BytecodeType::Float32,
            SourceType::Float64 => BytecodeType::Float64,
            _ => unreachable!(),
        };

        let dest = self.ensure_register(dest, ty.clone());

        match ty {
            BytecodeType::Float32 => self.builder.emit_const_float32(dest, value_f64 as f32),
            BytecodeType::Float64 => self.builder.emit_const_float64(dest, value_f64),
            _ => unreachable!(),
        }

        dest
    }

    fn visit_expr_lit_string(&mut self, lit: &ast::ExprLitStrType, dest: DataDest) -> Register {
        let dest = self.ensure_register(dest, BytecodeType::Ptr);
        let value = self
            .analysis
            .const_value(lit.id)
            .to_string()
            .expect("string expected")
            .to_string();
        self.builder.emit_const_string(dest, value);

        dest
    }

    fn visit_expr_lit_bool(&mut self, lit: &ast::ExprLitBoolType, dest: DataDest) -> Register {
        let dest = self.ensure_register(dest, BytecodeType::Bool);

        if lit.value {
            self.builder.emit_const_true(dest);
        } else {
            self.builder.emit_const_false(dest);
        }

        dest
    }

    fn visit_expr_tuple(&mut self, e: &ast::ExprTupleType, dest: DataDest) -> Register {
        if e.values.is_empty() {
            return self.ensure_unit_register();
        }

        let ty = self.ty(e.id);

        let result_ty: BytecodeType = register_bty_from_ty(ty.clone());
        let result = self.ensure_register(dest, result_ty);

        let mut values = Vec::with_capacity(e.values.len());

        for value in &e.values {
            let value_ty = self.ty(value.id());
            let reg = gen_expr(self, value, DataDest::Alloc);

            if !value_ty.is_unit() {
                values.push(reg);
            }
        }

        for &value in &values {
            self.builder.emit_push_register(value);
        }

        let subtypes = ty.tuple_subtypes().expect("tuple expected");
        let idx = self.builder.add_const_tuple(bty_array_from_ty(&subtypes));
        self.builder.emit_new_tuple(result, idx, self.loc(e.span));

        for arg_reg in values {
            self.free_if_temp(arg_reg);
        }

        result
    }

    fn visit_expr_un(&mut self, expr: &ast::ExprUnType, dest: DataDest) -> Register {
        if expr.op == ast::UnOp::Neg && expr.opnd.is_lit_int() {
            self.visit_expr_lit_int(expr.opnd.to_lit_int().unwrap(), dest, true)
        } else if let Some(intrinsic) = self.get_intrinsic(expr.id) {
            self.emit_intrinsic_un(&expr.opnd, intrinsic, self.loc(expr.span), dest)
        } else {
            self.visit_expr_un_method(expr, dest)
        }
    }

    fn visit_expr_un_method(&mut self, expr: &ast::ExprUnType, dest: DataDest) -> Register {
        let opnd = gen_expr(self, &expr.opnd, DataDest::Alloc);

        let call_type = self.analysis.map_calls.get(expr.id).unwrap();
        let callee_id = call_type.fct_id().expect("FctId missing");

        let callee = self.sa.fct(callee_id);

        let callee_idx = self.add_const_pool_entry_for_call(&callee, &call_type);

        let function_return_type: SourceType =
            self.specialize_type_for_call(call_type, callee.return_type());

        let function_return_type_bc: BytecodeType =
            register_bty_from_ty(function_return_type.clone());
        let dest = self.ensure_register(dest, function_return_type_bc);

        self.builder.emit_push_register(opnd);

        if call_type.is_generic_method() {
            self.emit_invoke_generic_direct(
                function_return_type,
                dest,
                callee_idx,
                self.loc(expr.span),
            );
        } else {
            self.emit_invoke_direct(function_return_type, dest, callee_idx, self.loc(expr.span));
        }

        self.free_if_temp(opnd);

        dest
    }

    fn visit_expr_bin(&mut self, expr: &ast::ExprBinType, dest: DataDest) -> Register {
        if expr.op.is_any_assign() {
            self.visit_expr_assign(expr, dest)
        } else if let ast::BinOp::Cmp(cmp_op) = expr.op {
            if cmp_op == CmpOp::Is || cmp_op == CmpOp::IsNot {
                self.emit_bin_is(expr, dest)
            } else {
                gen_expr_bin_cmp(self, expr, cmp_op, dest)
            }
        } else if expr.op == ast::BinOp::Or {
            self.emit_bin_or(expr, dest)
        } else if expr.op == ast::BinOp::And {
            self.emit_bin_and(expr, dest)
        } else if let Some(info) = self.get_intrinsic(expr.id) {
            self.emit_intrinsic_bin(&expr.lhs, &expr.rhs, info, self.loc(expr.span), dest)
        } else {
            self.visit_expr_bin_method(expr, dest)
        }
    }

    fn visit_expr_bin_method(&mut self, expr: &ast::ExprBinType, dest: DataDest) -> Register {
        let lhs = gen_expr(self, &expr.lhs, DataDest::Alloc);
        let rhs = gen_expr(self, &expr.rhs, DataDest::Alloc);

        let call_type = self.analysis.map_calls.get(expr.id).unwrap();
        let callee_id = call_type.fct_id().expect("FctId missing");

        let callee = self.sa.fct(callee_id);

        let callee_idx = self.add_const_pool_entry_for_call(&callee, &call_type);

        let function_return_type: SourceType =
            self.specialize_type_for_call(call_type, callee.return_type());

        let function_return_type_bc: BytecodeType =
            register_bty_from_ty(function_return_type.clone());

        let return_type = match expr.op {
            ast::BinOp::Cmp(_) => BytecodeType::Bool,
            _ => function_return_type_bc.clone(),
        };

        let dest = self.ensure_register(dest, return_type.clone());

        let result = if function_return_type_bc == return_type {
            dest
        } else {
            let function_result_register_ty: BytecodeType =
                register_bty_from_ty(function_return_type.clone());
            self.alloc_temp(function_result_register_ty)
        };

        self.builder.emit_push_register(lhs);
        self.builder.emit_push_register(rhs);

        if call_type.is_generic_method() {
            self.emit_invoke_generic_direct(
                function_return_type,
                result,
                callee_idx,
                self.loc(expr.span),
            );
        } else {
            self.emit_invoke_direct(
                function_return_type,
                result,
                callee_idx,
                self.loc(expr.span),
            );
        }

        self.free_if_temp(lhs);
        self.free_if_temp(rhs);

        match expr.op {
            ast::BinOp::Cmp(ast::CmpOp::Eq) => assert_eq!(result, dest),
            ast::BinOp::Cmp(ast::CmpOp::Ne) => {
                assert_eq!(result, dest);
                self.builder.emit_not(dest, dest);
            }

            ast::BinOp::Cmp(op) => {
                assert_ne!(result, dest);
                let zero = self.alloc_temp(BytecodeType::Int32);
                self.builder.emit_const_int32(zero, 0);

                match op {
                    ast::CmpOp::Lt => self.builder.emit_test_lt(dest, result, zero),
                    ast::CmpOp::Le => self.builder.emit_test_le(dest, result, zero),
                    ast::CmpOp::Gt => self.builder.emit_test_gt(dest, result, zero),
                    ast::CmpOp::Ge => self.builder.emit_test_ge(dest, result, zero),
                    ast::CmpOp::Eq | ast::CmpOp::Ne | ast::CmpOp::Is | ast::CmpOp::IsNot => {
                        unreachable!()
                    }
                }

                self.free_temp(zero);
                self.free_temp(result);
            }
            _ => assert_eq!(result, dest),
        }

        dest
    }

    fn visit_expr_call_intrinsic(
        &mut self,
        expr: &ast::ExprCallType,
        info: IntrinsicInfo,
        dest: DataDest,
    ) -> Register {
        let intrinsic = info.intrinsic;
        let call_type = self.analysis.map_calls.get(expr.id).unwrap().clone();

        if call_type.is_method() {
            let object = expr.object().unwrap();

            match expr.args.len() {
                0 => self.emit_intrinsic_un(object, info, self.loc(expr.span), dest),
                1 => self.emit_intrinsic_bin(
                    object,
                    &expr.args[0].expr,
                    info,
                    self.loc(expr.span),
                    dest,
                ),
                2 => {
                    assert_eq!(intrinsic, Intrinsic::ArraySet);
                    self.emit_intrinsic_array_set(
                        expr.object().unwrap(),
                        &expr.args[0].expr,
                        &expr.args[1].expr,
                        self.loc(expr.span),
                        dest,
                    )
                }
                _ => unreachable!(),
            }
        } else {
            match intrinsic {
                Intrinsic::Assert => self.visit_expr_assert(expr, dest),

                Intrinsic::ArrayGet => self.emit_intrinsic_array_get(
                    &expr.callee,
                    &expr.args[0].expr,
                    self.loc(expr.span),
                    dest,
                ),

                Intrinsic::ArrayNewOfSize => self.emit_intrinsic_new_array(expr, dest),

                Intrinsic::ArrayWithValues => {
                    let ty = self.ty(expr.id);

                    let (cls_id, type_params) = ty.to_class().expect("class expected");
                    assert_eq!(cls_id, self.sa.known.classes.array());
                    assert_eq!(1, type_params.len());
                    let element_ty = type_params[0].clone();
                    self.emit_array_with_variadic_arguments(expr, &[element_ty], 0, dest)
                }

                _ => panic!("unimplemented intrinsic {:?}", intrinsic),
            }
        }
    }

    fn emit_intrinsic_new_array(&mut self, expr: &ast::ExprCallType, dest: DataDest) -> Register {
        // We need array of elements
        let element_ty = self.ty(expr.id);
        let (cls_id, type_params) = element_ty.to_class().expect("class expected");
        let cls_idx = self.builder.add_const_cls_types(
            ClassId(cls_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&type_params),
        );

        let array_reg = self.ensure_register(dest, BytecodeType::Ptr);
        let length_reg = gen_expr(self, &expr.args[0].expr, DataDest::Alloc);

        self.builder
            .emit_new_array(array_reg, length_reg, cls_idx, self.loc(expr.span));

        self.free_if_temp(length_reg);

        array_reg
    }

    fn emit_bin_is(&mut self, expr: &ast::ExprBinType, dest: DataDest) -> Register {
        let dest = self.ensure_register(dest, BytecodeType::Bool);

        let lhs_reg = gen_expr(self, &expr.lhs, DataDest::Alloc);
        let rhs_reg = gen_expr(self, &expr.rhs, DataDest::Alloc);

        self.builder.emit_test_identity(dest, lhs_reg, rhs_reg);

        if expr.op == ast::BinOp::Cmp(ast::CmpOp::IsNot) {
            self.builder.emit_not(dest, dest);
        }

        self.free_if_temp(lhs_reg);
        self.free_if_temp(rhs_reg);

        dest
    }

    fn emit_bin_or(&mut self, expr: &ast::ExprBinType, dest: DataDest) -> Register {
        let end_lbl = self.builder.create_label();
        let dest = self.ensure_register(dest, BytecodeType::Bool);

        gen_expr(self, &expr.lhs, DataDest::Reg(dest));
        self.builder.emit_jump_if_true(dest, end_lbl);
        gen_expr(self, &expr.rhs, DataDest::Reg(dest));
        self.builder.bind_label(end_lbl);

        dest
    }

    fn emit_bin_and(&mut self, expr: &ast::ExprBinType, dest: DataDest) -> Register {
        let end_lbl = self.builder.create_label();
        let dest = self.ensure_register(dest, BytecodeType::Bool);

        self.push_scope();

        if let Some(is_expr) = expr.lhs.to_is() {
            self.builder.emit_const_false(dest);
            let value = gen_expr(self, &is_expr.value, DataDest::Alloc);
            let ty = self.ty(is_expr.value.id());
            self.setup_pattern_vars(&is_expr.pattern);
            self.destruct_pattern(&is_expr.pattern, value, ty, Some(end_lbl));
            self.free_if_temp(value);
        } else {
            gen_expr(self, &expr.lhs, DataDest::Reg(dest));
            self.builder.emit_jump_if_false(dest, end_lbl);
        }

        gen_expr(self, &expr.rhs, DataDest::Reg(dest));
        self.builder.bind_label(end_lbl);

        self.pop_scope();

        dest
    }

    fn emit_intrinsic_array_get(
        &mut self,
        obj: &ast::ExprData,
        idx: &ast::ExprData,
        location: Location,
        dest: DataDest,
    ) -> Register {
        let ty = self.ty(obj.id());
        let ty: BytecodeType = if ty.cls_id() == Some(self.sa.known.classes.string()) {
            BytecodeType::UInt8
        } else {
            let ty = ty.type_params();
            let ty = ty[0].clone();

            register_bty_from_ty(ty)
        };

        let dest = self.ensure_register(dest, ty.clone());

        let arr = gen_expr(self, obj, DataDest::Alloc);
        let idx = gen_expr(self, idx, DataDest::Alloc);

        self.builder.emit_load_array(dest, arr, idx, location);

        self.free_if_temp(arr);
        self.free_if_temp(idx);

        dest
    }

    fn emit_intrinsic_array_set(
        &mut self,
        arr: &ast::ExprData,
        idx: &ast::ExprData,
        src: &ast::ExprData,
        location: Location,
        _dest: DataDest,
    ) -> Register {
        let arr = gen_expr(self, arr, DataDest::Alloc);
        let idx = gen_expr(self, idx, DataDest::Alloc);
        let src = gen_expr(self, src, DataDest::Alloc);

        self.builder.emit_store_array(src, arr, idx, location);

        self.free_if_temp(arr);
        self.free_if_temp(idx);
        self.free_if_temp(src);

        self.ensure_unit_register()
    }

    fn emit_intrinsic_un(
        &mut self,
        opnd: &ast::ExprData,
        info: IntrinsicInfo,
        location: Location,
        dest: DataDest,
    ) -> Register {
        let intrinsic = info.intrinsic;

        let fct = self.sa.fct(info.fct_id.expect("missing method"));
        let ty = fct.return_type_bty();
        let dest = self.ensure_register(dest, ty);

        let src = gen_expr(self, opnd, DataDest::Alloc);

        match intrinsic {
            Intrinsic::ArrayLen | Intrinsic::StrLen => {
                self.builder.emit_array_length(dest, src, location);
            }
            Intrinsic::Int32Neg
            | Intrinsic::Int64Neg
            | Intrinsic::Float32Neg
            | Intrinsic::Float64Neg => self.builder.emit_neg(dest, src, location),
            Intrinsic::BoolNot | Intrinsic::Int32Not | Intrinsic::Int64Not => {
                self.builder.emit_not(dest, src)
            }
            Intrinsic::Float32IsNan => self.builder.emit_test_ne(dest, src, src),
            Intrinsic::Float64IsNan => self.builder.emit_test_ne(dest, src, src),
            _ => {
                panic!("unimplemented intrinsic {:?}", intrinsic);
            }
        }

        self.free_if_temp(src);

        dest
    }

    fn emit_intrinsic_bin(
        &mut self,
        lhs: &ast::ExprData,
        rhs: &ast::ExprData,
        info: IntrinsicInfo,
        location: Location,
        dest: DataDest,
    ) -> Register {
        let intrinsic = info.intrinsic;

        match intrinsic {
            Intrinsic::ArrayGet | Intrinsic::StrGet => {
                return self.emit_intrinsic_array_get(lhs, rhs, location, dest);
            }

            _ => {}
        }

        let fct_id = info.fct_id.expect("missing function");
        let fct = self.sa.fct(fct_id);

        let result_type = fct.return_type_bty();

        let dest = self.ensure_register(dest, result_type);

        let lhs_reg = gen_expr(self, lhs, DataDest::Alloc);
        let rhs_reg = gen_expr(self, rhs, DataDest::Alloc);

        gen_intrinsic_bin(self, intrinsic, dest, lhs_reg, rhs_reg, location);

        self.free_if_temp(lhs_reg);
        self.free_if_temp(rhs_reg);

        dest
    }

    fn visit_expr_assign(&mut self, expr: &ast::ExprBinType, _dest: DataDest) -> Register {
        if expr.lhs.is_ident() {
            let value_reg = gen_expr(self, &expr.rhs, DataDest::Alloc);
            let ident_type = self.analysis.map_idents.get(expr.lhs.id()).unwrap();
            match ident_type {
                &IdentType::Var(var_id) => {
                    self.visit_expr_assign_var(expr, var_id, value_reg);
                }
                &IdentType::Context(level, field_id) => {
                    self.visit_expr_assign_context(expr, level, field_id, value_reg);
                }
                &IdentType::Global(gid) => {
                    self.visit_expr_assign_global(expr, gid, value_reg);
                }
                _ => unreachable!(),
            }
            self.free_if_temp(value_reg);
        } else if expr.lhs.is_path() {
            let value_reg = gen_expr(self, &expr.rhs, DataDest::Alloc);
            let ident_type = self.analysis.map_idents.get(expr.lhs.id()).unwrap();
            match ident_type {
                &IdentType::Global(gid) => {
                    self.visit_expr_assign_global(expr, gid, value_reg);
                }
                _ => unreachable!(),
            }
            self.free_if_temp(value_reg);
        } else {
            match *expr.lhs {
                ast::ExprData::Dot(ref dot) => self.visit_expr_assign_dot(expr, dot),
                ast::ExprData::Call(ref call) => self.visit_expr_assign_call(expr, call),
                _ => unreachable!(),
            };
        }

        self.ensure_unit_register()
    }

    fn visit_expr_assign_call(&mut self, expr: &ast::ExprBinType, call_expr: &ast::ExprCallType) {
        let object = &call_expr.callee;
        let index = &call_expr.args[0].expr;
        let value = &expr.rhs;

        let obj_reg = gen_expr(self, object, DataDest::Alloc);
        let idx_reg = gen_expr(self, index, DataDest::Alloc);
        let val_reg = gen_expr(self, value, DataDest::Alloc);

        let array_assignment = self
            .analysis
            .map_array_assignments
            .get(expr.id)
            .expect("missing assignment data")
            .clone();

        let location = self.loc(expr.span);

        let assign_value = if expr.op != ast::BinOp::Assign {
            let ty = register_bty_from_ty(array_assignment.item_ty.expect("missing item type"));
            let current = self.alloc_temp(ty);

            let call_type = array_assignment.index_get.expect("missing index_get");
            let fct_id = call_type.fct_id().unwrap();
            let fct = self.sa.fct(fct_id);

            if let Some(intrinsic) = fct.intrinsic.get() {
                assert_eq!(*intrinsic, Intrinsic::ArrayGet);
                self.builder
                    .emit_load_array(current, obj_reg, idx_reg, location);
            } else {
                let obj_ty = self.ty(object.id());

                self.builder.emit_push_register(obj_reg);
                self.builder.emit_push_register(idx_reg);

                let type_params = obj_ty.type_params();

                let callee_idx = self.builder.add_const_fct_types(
                    FunctionId(fct_id.index().try_into().expect("overflow")),
                    bty_array_from_ty(&type_params),
                );
                self.builder
                    .emit_invoke_direct(current, callee_idx, location);
            }

            if let Some(info) = self.get_intrinsic(expr.id) {
                gen_intrinsic_bin(self, info.intrinsic, current, current, val_reg, location);
            } else {
                gen_method_bin(self, expr, current, current, val_reg, location);
            }

            current
        } else {
            val_reg
        };

        let call_type = array_assignment.index_set.expect("missing index_set");
        let fct_id = call_type.fct_id().unwrap();
        let fct = self.sa.fct(fct_id);

        if let Some(intrinsic) = fct.intrinsic.get() {
            assert_eq!(*intrinsic, Intrinsic::ArraySet);
            self.builder
                .emit_store_array(assign_value, obj_reg, idx_reg, location);
        } else {
            let obj_ty = self.ty(object.id());

            self.builder.emit_push_register(obj_reg);
            self.builder.emit_push_register(idx_reg);
            self.builder.emit_push_register(assign_value);

            let type_params = obj_ty.type_params();

            let callee_idx = self.builder.add_const_fct_types(
                FunctionId(fct_id.index().try_into().expect("overflow")),
                bty_array_from_ty(&type_params),
            );
            let dest = self.ensure_unit_register();
            self.builder.emit_invoke_direct(dest, callee_idx, location);
        }

        self.free_if_temp(obj_reg);
        self.free_if_temp(idx_reg);
        self.free_if_temp(val_reg);
        self.free_if_temp(assign_value);
    }

    fn visit_expr_assign_dot(&mut self, expr: &ast::ExprBinType, dot: &ast::ExprDotType) {
        let (cls_ty, field_id) = {
            let ident_type = self.analysis.map_idents.get(dot.id).cloned().unwrap();
            match ident_type {
                IdentType::Field(class, field) => (class, field),
                _ => unreachable!(),
            }
        };

        let (cls_id, type_params) = cls_ty.to_class().expect("class expected");

        let field_idx = self.builder.add_const_field_types(
            ClassId(cls_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&type_params),
            field_id.0 as u32,
        );

        let obj = gen_expr(self, &dot.lhs, DataDest::Alloc);
        let value = gen_expr(self, &expr.rhs, DataDest::Alloc);

        let location = self.loc(expr.span);

        let assign_value = if expr.op != ast::BinOp::Assign {
            let cls = self.sa.class(cls_id);
            let ty = cls.fields[field_id.0].ty();
            let ty = register_bty_from_ty(ty);
            let current = self.alloc_temp(ty);
            self.builder
                .emit_load_field(current, obj, field_idx, location);

            if let Some(info) = self.get_intrinsic(expr.id) {
                gen_intrinsic_bin(self, info.intrinsic, current, current, value, location);
            } else {
                gen_method_bin(self, expr, current, current, value, location);
            }

            current
        } else {
            value
        };

        self.builder
            .emit_store_field(assign_value, obj, field_idx, location);

        if expr.op != ast::BinOp::Assign {
            self.free_temp(assign_value);
        }

        self.free_if_temp(obj);
        self.free_if_temp(value);
    }

    fn visit_expr_assign_context(
        &mut self,
        expr: &ast::ExprBinType,
        outer_context_id: OuterContextIdx,
        context_field_id: ContextFieldId,
        value: Register,
    ) {
        let location = self.loc(expr.span);

        let assign_value = if expr.op != ast::BinOp::Assign {
            let current =
                self.load_from_outer_context(outer_context_id, context_field_id, location);

            if let Some(info) = self.get_intrinsic(expr.id) {
                gen_intrinsic_bin(self, info.intrinsic, current, current, value, location);
            } else {
                gen_method_bin(self, expr, current, current, value, location);
            }

            current
        } else {
            value
        };

        self.store_in_outer_context(outer_context_id, context_field_id, assign_value, location);

        if expr.op != ast::BinOp::Assign {
            self.free_temp(assign_value);
        }
    }

    fn visit_expr_assign_var(&mut self, expr: &ast::ExprBinType, var_id: VarId, value: Register) {
        let var = self.analysis.vars.get_var(var_id);

        let assign_value = if expr.op != ast::BinOp::Assign {
            let current = match var.location {
                VarLocation::Context(scope_id, field_id) => {
                    let ty = register_bty_from_ty(var.ty.clone());
                    let dest_reg = self.alloc_temp(ty);
                    self.load_from_context(dest_reg, scope_id, field_id, self.loc(expr.span));
                    dest_reg
                }

                VarLocation::Stack => self.var_reg(var_id),
            };

            let location = self.loc(expr.span);

            if let Some(info) = self.get_intrinsic(expr.id) {
                gen_intrinsic_bin(self, info.intrinsic, current, current, value, location);
            } else {
                gen_method_bin(self, expr, current, current, value, location);
            }

            current
        } else {
            value
        };

        match var.location {
            VarLocation::Context(scope_id, field_id) => {
                self.store_in_context(assign_value, scope_id, field_id, self.loc(expr.span));
            }

            VarLocation::Stack => {
                let var_reg = self.var_reg(var_id);
                self.builder.emit_mov(var_reg, assign_value);
            }
        }

        if expr.op != ast::BinOp::Assign {
            self.free_if_temp(assign_value);
        }
    }

    fn visit_expr_assign_global(
        &mut self,
        expr: &ast::ExprBinType,
        gid: GlobalDefinitionId,
        value: Register,
    ) {
        let bc_gid = GlobalId(gid.index().try_into().expect("overflow"));
        let location = self.loc(expr.span);

        let assign_value = if expr.op != ast::BinOp::Assign {
            let global = self.sa.global(gid);
            let ty = register_bty_from_ty(global.ty());
            let current = self.alloc_temp(ty);
            self.builder.emit_load_global(current, bc_gid, location);

            if let Some(info) = self.get_intrinsic(expr.id) {
                gen_intrinsic_bin(self, info.intrinsic, current, current, value, location);
            } else {
                gen_method_bin(self, expr, current, current, value, location);
            }

            current
        } else {
            value
        };

        self.builder.emit_store_global(assign_value, bc_gid);

        if expr.op != ast::BinOp::Assign {
            self.free_temp(assign_value);
        }
    }

    fn visit_expr_ident(&mut self, ident: &ast::ExprIdentType, dest: DataDest) -> Register {
        let ident_type = self.analysis.map_idents.get(ident.id).unwrap();

        match ident_type {
            &IdentType::Var(var_id) => {
                self.visit_expr_ident_var(var_id, dest, self.loc(ident.span))
            }
            &IdentType::Context(level, field_id) => {
                self.visit_expr_ident_context(level, field_id, dest, self.loc(ident.span))
            }
            &IdentType::Global(gid) => {
                self.visit_expr_ident_global(gid, dest, self.loc(ident.span))
            }
            &IdentType::Const(cid) => self.visit_expr_ident_const(cid, dest),
            &IdentType::EnumVariant(enum_id, ref type_params, variant_idx) => self.emit_new_enum(
                enum_id,
                type_params.clone(),
                variant_idx,
                self.loc(ident.span),
                dest,
            ),

            &IdentType::Field(..) => unreachable!(),
            &IdentType::Struct(..) => unreachable!(),
            &IdentType::StructField(..) => unreachable!(),

            &IdentType::Fct(..) => unreachable!(),
            &IdentType::Class(..) => unreachable!(),
        }
    }

    fn visit_expr_ident_context(
        &mut self,
        context_id: OuterContextIdx,
        field_id: ContextFieldId,
        dest: DataDest,
        location: Location,
    ) -> Register {
        assert!(self.is_lambda);
        let self_reg = self.var_reg(SELF_VAR_ID);

        // Load context field of lambda object (in self register).
        let outer_context_reg = self.alloc_temp(BytecodeType::Ptr);
        let lambda_cls_id = self.sa.known.classes.lambda();
        let idx = self.builder.add_const_field_types(
            ClassId(lambda_cls_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty(),
            0,
        );
        self.builder
            .emit_load_field(outer_context_reg, self_reg, idx, location);

        assert!(context_id.0 < self.analysis.outer_contexts.len());

        for outer_context_class in self
            .analysis
            .outer_contexts
            .iter()
            .skip(context_id.0 + 1)
            .rev()
        {
            if outer_context_class.has_class_id() {
                let outer_cls_id = outer_context_class.class_id();
                let idx = self.builder.add_const_field_types(
                    ClassId(outer_cls_id.index().try_into().expect("overflow")),
                    bty_array_from_ty(&self.identity_type_params()),
                    0,
                );
                assert!(outer_context_class.has_parent_slot());
                self.builder
                    .emit_load_field(outer_context_reg, outer_context_reg, idx, location);
            }
        }

        let outer_context_info = self.analysis.outer_contexts[context_id.0].clone();
        let outer_cls_id = outer_context_info.class_id();

        let outer_cls = self.sa.class(outer_cls_id);
        let field_id = field_id_from_context_idx(field_id, outer_context_info.has_parent_slot());
        let field = &outer_cls.fields[field_id];

        let ty: BytecodeType = register_bty_from_ty(field.ty());
        let value_reg = self.ensure_register(dest, ty);

        let idx = self.builder.add_const_field_types(
            ClassId(outer_cls_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&self.identity_type_params()),
            field_id.0 as u32,
        );
        self.builder
            .emit_load_field(value_reg, outer_context_reg, idx, location);

        self.free_temp(outer_context_reg);

        value_reg
    }

    fn visit_expr_ident_const(&mut self, const_id: ConstDefinitionId, dest: DataDest) -> Register {
        let const_ = self.sa.const_(const_id);
        let ty = const_.ty();

        let bytecode_ty = register_bty_from_ty(ty.clone());
        let dest = self.ensure_register(dest, bytecode_ty);

        match ty {
            SourceType::Bool => {
                if const_.value().to_bool() {
                    self.builder.emit_const_true(dest);
                } else {
                    self.builder.emit_const_false(dest);
                }
            }

            SourceType::Char => {
                self.builder.emit_const_char(dest, const_.value().to_char());
            }

            SourceType::UInt8 => {
                self.builder.emit_const_uint8(
                    dest,
                    const_.value().to_i64().expect("integer expected") as u8,
                );
            }

            SourceType::Int32 => {
                self.builder.emit_const_int32(
                    dest,
                    const_.value().to_i64().expect("integer expected") as i32,
                );
            }

            SourceType::Int64 => {
                self.builder
                    .emit_const_int64(dest, const_.value().to_i64().expect("integer expected"));
            }

            SourceType::Float32 => {
                self.builder.emit_const_float32(
                    dest,
                    const_.value().to_f64().expect("float expected") as f32,
                );
            }

            SourceType::Float64 => {
                self.builder
                    .emit_const_float64(dest, const_.value().to_f64().expect("float expected"));
            }

            _ => unimplemented!(),
        }

        dest
    }

    fn visit_expr_ident_global(
        &mut self,
        gid: GlobalDefinitionId,
        dest: DataDest,
        location: Location,
    ) -> Register {
        let global_var = self.sa.global(gid);

        let ty: BytecodeType = register_bty_from_ty(global_var.ty());
        let dest = self.ensure_register(dest, ty);

        self.builder.emit_load_global(
            dest,
            GlobalId(gid.index().try_into().expect("overflow")),
            location,
        );

        dest
    }

    fn visit_expr_ident_var(
        &mut self,
        var_id: VarId,
        dest: DataDest,
        location: Location,
    ) -> Register {
        let var = self.analysis.vars.get_var(var_id);

        match var.location {
            VarLocation::Context(scope_id, field_idx) => {
                let ty = register_bty_from_ty(var.ty.clone());
                let dest_reg = self.ensure_register(dest, ty);
                self.load_from_context(dest_reg, scope_id, field_idx, location);
                dest_reg
            }

            VarLocation::Stack => {
                let var_reg = self.var_reg(var_id);

                if dest.is_alloc() {
                    return var_reg;
                }

                let dest = dest.reg();
                self.emit_mov(dest, var_reg);

                dest
            }
        }
    }

    fn store_in_outer_context(
        &mut self,
        level: OuterContextIdx,
        context_idx: ContextFieldId,
        value: Register,
        location: Location,
    ) {
        let self_reg = self.var_reg(SELF_VAR_ID);

        // Load context field of lambda object in self.
        let outer_context_reg = self.alloc_temp(BytecodeType::Ptr);
        let lambda_cls_id = self.sa.known.classes.lambda();
        let idx = self.builder.add_const_field_types(
            ClassId(lambda_cls_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty(),
            0,
        );
        self.builder
            .emit_load_field(outer_context_reg, self_reg, idx, location);

        assert!(level.0 < self.analysis.outer_contexts.len());

        for outer_context_class in self.analysis.outer_contexts.iter().skip(level.0 + 1).rev() {
            if outer_context_class.has_class_id() {
                let outer_cls_id = outer_context_class.class_id();

                let idx = self.builder.add_const_field_types(
                    ClassId(outer_cls_id.index().try_into().expect("overflow")),
                    bty_array_from_ty(&self.identity_type_params()),
                    0,
                );
                self.builder
                    .emit_load_field(outer_context_reg, outer_context_reg, idx, location);
            }
        }

        // Store value in context field
        let outer_context_info = self.analysis.outer_contexts[level.0].clone();

        let field_id = field_id_from_context_idx(context_idx, outer_context_info.has_parent_slot());
        let idx = self.builder.add_const_field_types(
            ClassId(
                outer_context_info
                    .class_id()
                    .index()
                    .try_into()
                    .expect("overflow"),
            ),
            bty_array_from_ty(&self.identity_type_params()),
            field_id.0 as u32,
        );
        self.builder
            .emit_store_field(value, outer_context_reg, idx, location);

        self.free_temp(outer_context_reg);
    }

    fn load_from_outer_context(
        &mut self,
        context_id: OuterContextIdx,
        field_id: ContextFieldId,
        location: Location,
    ) -> Register {
        assert!(self.is_lambda);
        let self_reg = self.var_reg(SELF_VAR_ID);

        // Load context field of lambda object (in self register).
        let outer_context_reg = self.alloc_temp(BytecodeType::Ptr);
        let lambda_cls_id = self.sa.known.classes.lambda();
        let idx = self.builder.add_const_field_types(
            ClassId(lambda_cls_id.index().try_into().expect("overflow")),
            BytecodeTypeArray::empty(),
            0,
        );
        self.builder
            .emit_load_field(outer_context_reg, self_reg, idx, location);

        assert!(context_id.0 < self.analysis.outer_contexts.len());

        for outer_context_class in self
            .analysis
            .outer_contexts
            .iter()
            .skip(context_id.0 + 1)
            .rev()
        {
            if outer_context_class.has_class_id() {
                let outer_cls_id = outer_context_class.class_id();
                let idx = self.builder.add_const_field_types(
                    ClassId(outer_cls_id.index().try_into().expect("overflow")),
                    bty_array_from_ty(&self.identity_type_params()),
                    0,
                );
                assert!(outer_context_class.has_parent_slot());
                self.builder
                    .emit_load_field(outer_context_reg, outer_context_reg, idx, location);
            }
        }

        let outer_context_info = self.analysis.outer_contexts[context_id.0].clone();
        let outer_cls_id = outer_context_info.class_id();

        let outer_cls = self.sa.class(outer_cls_id);
        let field_id = field_id_from_context_idx(field_id, outer_context_info.has_parent_slot());
        let field = &outer_cls.fields[field_id];

        let ty: BytecodeType = register_bty_from_ty(field.ty());
        let dest = self.alloc_temp(ty);

        let idx = self.builder.add_const_field_types(
            ClassId(outer_cls_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&self.identity_type_params()),
            field_id.0 as u32,
        );
        self.builder
            .emit_load_field(dest, outer_context_reg, idx, location);

        self.free_temp(outer_context_reg);

        dest
    }

    fn store_in_context(
        &mut self,
        src: Register,
        scope_id: ScopeId,
        field_id: ContextFieldId,
        location: Location,
    ) {
        let entered_context = &self.entered_contexts[scope_id.0];
        let context_register = entered_context.register.expect("missing register");
        let context_data = entered_context.context_data.clone();
        let cls_id = context_data.class_id();
        let field_id = field_id_from_context_idx(field_id, context_data.has_parent_slot());
        let field_idx = self.builder.add_const_field_types(
            ClassId(cls_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&self.identity_type_params()),
            field_id.0 as u32,
        );
        self.builder
            .emit_store_field(src, context_register, field_idx, location);
    }

    fn load_from_context(
        &mut self,
        dest: Register,
        scope_id: ScopeId,
        field_id: ContextFieldId,
        location: Location,
    ) {
        // Load context object.
        let entered_context = &self.entered_contexts[scope_id.0];
        let context_register = entered_context.register.expect("missing register");
        let context_data = entered_context.context_data.clone();
        let cls_id = context_data.class_id();
        let field_id = field_id_from_context_idx(field_id, context_data.has_parent_slot());
        let field_idx = self.builder.add_const_field_types(
            ClassId(cls_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&self.identity_type_params()),
            field_id.0 as u32,
        );
        self.builder
            .emit_load_field(dest, context_register, field_idx, location);
    }

    fn var_reg(&self, var_id: VarId) -> Register {
        *self
            .var_registers
            .get(&var_id)
            .expect("no register for var found")
    }

    fn set_var_reg(&mut self, var_id: VarId, reg: Register) {
        let old = self.var_registers.insert(var_id, reg);
        assert!(old.is_none());
    }

    fn ensure_register(&mut self, dest: DataDest, ty: BytecodeType) -> Register {
        match dest {
            DataDest::Alloc => self.alloc_temp(ty),
            DataDest::Reg(reg) => reg,
        }
    }

    fn add_const_pool_entry_for_call(
        &mut self,
        fct: &FctDefinition,
        call_type: &CallType,
    ) -> ConstPoolIdx {
        match call_type {
            CallType::GenericStaticMethod(id, .., ref trait_type_params, ref fct_type_params)
            | CallType::GenericMethod(id, .., ref trait_type_params, ref fct_type_params) => {
                self.builder.add_const(ConstPoolEntry::Generic(
                    id.index() as u32,
                    FunctionId(fct.id().index().try_into().expect("overflow")),
                    bty_array_from_ty(&trait_type_params),
                    bty_array_from_ty(&fct_type_params),
                ))
            }
            CallType::GenericMethodSelf(_, fct_id, ref trait_type_params, ref fct_type_params)
            | CallType::GenericStaticMethodSelf(
                _,
                fct_id,
                ref trait_type_params,
                ref fct_type_params,
            ) => self.builder.add_const(ConstPoolEntry::GenericSelf(
                FunctionId(fct_id.index().try_into().expect("overflow")),
                bty_array_from_ty(&trait_type_params),
                bty_array_from_ty(&fct_type_params),
            )),
            CallType::TraitObjectMethod(ref trait_object_ty, _) => {
                self.builder.add_const(ConstPoolEntry::TraitObjectMethod(
                    bty_from_ty(trait_object_ty.clone()),
                    FunctionId(fct.id().index().try_into().expect("overflow")),
                ))
            }

            CallType::Method(.., ref type_params)
            | CallType::Expr(.., ref type_params)
            | CallType::Fct(.., ref type_params) => {
                assert_eq!(
                    fct.type_param_definition().type_param_count(),
                    type_params.len()
                );
                self.builder.add_const_fct_types(
                    FunctionId(fct.id().index().try_into().expect("overflow")),
                    bty_array_from_ty(&type_params),
                )
            }

            _ => panic!("unexpected call type {:?}", call_type),
        }
    }

    fn specialize_type_for_call(&self, call_type: &CallType, ty: SourceType) -> SourceType {
        match call_type {
            CallType::Fct(_, ref type_params)
            | CallType::Expr(_, _, ref type_params)
            | CallType::Method(_, _, ref type_params) => specialize_type(self.sa, ty, type_params),

            CallType::TraitObjectMethod(trait_ty, _actual_object_ty) => {
                let (trait_id, type_params, assoc_types) = match trait_ty {
                    SourceType::TraitObject(trait_id, type_params, assoc_types) => {
                        (*trait_id, type_params, assoc_types)
                    }
                    _ => unreachable!(),
                };
                specialize_ty_for_trait_object(self.sa, ty, trait_id, type_params, assoc_types)
            }
            CallType::GenericMethod(
                id,
                _trait_id,
                _method_id,
                ref trait_type_params,
                ref fct_type_params,
            )
            | CallType::GenericStaticMethod(
                id,
                _trait_id,
                _method_id,
                ref trait_type_params,
                ref fct_type_params,
            ) => replace_type(
                self.sa,
                ty,
                Some(&trait_type_params.connect(fct_type_params)),
                Some(SourceType::TypeParam(*id)),
            ),

            CallType::GenericMethodSelf(
                _trait_id,
                _fct_id,
                ref trait_type_params,
                ref fct_type_params,
            )
            | CallType::GenericStaticMethodSelf(
                _trait_id,
                _fct_id,
                ref trait_type_params,
                ref fct_type_params,
            ) => replace_type(
                self.sa,
                ty,
                Some(&trait_type_params.connect(fct_type_params)),
                None,
            ),

            CallType::Lambda(..)
            | CallType::NewClass(..)
            | CallType::NewStruct(..)
            | CallType::NewEnum(..)
            | CallType::Intrinsic(..) => {
                unreachable!()
            }
        }
    }

    fn ty(&self, id: ast::NodeId) -> SourceType {
        self.analysis.ty(id)
    }

    fn var_ty(&self, id: VarId) -> SourceType {
        self.analysis.vars.get_var(id).ty.clone()
    }

    fn get_intrinsic(&self, id: ast::NodeId) -> Option<IntrinsicInfo> {
        let call_type = self.analysis.map_calls.get(id).expect("missing CallType");

        if let Some(intrinsic) = call_type.to_intrinsic() {
            return Some(intrinsic.into());
        }

        let fid = if let Some(fct_id) = call_type.fct_id() {
            fct_id
        } else {
            return None;
        };

        let fct = self.sa.fct(fid);

        if let Some(intrinsic) = fct.intrinsic.get().cloned() {
            return Some(IntrinsicInfo::with_fct(intrinsic, fid));
        }

        None
    }

    fn identity_type_params(&self) -> SourceTypeArray {
        new_identity_type_params(0, self.type_params_len)
    }

    fn ensure_unit_register(&mut self) -> Register {
        if let Some(register) = self.unit_register {
            return register;
        }

        let register = self.builder.alloc_global(BytecodeType::Unit);
        self.unit_register = Some(register);
        register
    }

    fn push_scope(&mut self) {
        self.builder.push_scope();
    }

    fn pop_scope(&mut self) {
        self.builder.pop_scope();
    }

    fn alloc_var(&mut self, ty: BytecodeType) -> Register {
        assert!(!ty.is_class());
        self.builder.alloc_var(ty)
    }

    fn alloc_temp(&mut self, ty: BytecodeType) -> Register {
        assert!(!ty.is_class());
        self.builder.alloc_temp(ty)
    }

    fn free_if_temp(&mut self, reg: Register) {
        self.builder.free_if_temp(reg);
    }

    fn free_temp(&mut self, reg: Register) {
        self.builder.free_temp(reg);
    }
}

struct IntrinsicInfo {
    intrinsic: Intrinsic,
    fct_id: Option<FctDefinitionId>,
}

impl IntrinsicInfo {
    fn with_fct(intrinsic: Intrinsic, fct_id: FctDefinitionId) -> IntrinsicInfo {
        IntrinsicInfo {
            intrinsic,
            fct_id: Some(fct_id),
        }
    }
}

impl From<Intrinsic> for IntrinsicInfo {
    fn from(intrinsic: Intrinsic) -> IntrinsicInfo {
        IntrinsicInfo {
            intrinsic,
            fct_id: None,
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum DataDest {
    // Allocate a new register and store result in it.
    Alloc,

    // Store the result in the given register.
    Reg(Register),
}

impl DataDest {
    fn is_alloc(&self) -> bool {
        match self {
            DataDest::Reg(_) => false,
            DataDest::Alloc => true,
        }
    }

    fn reg(&self) -> Register {
        match self {
            DataDest::Alloc => panic!("not a register"),
            DataDest::Reg(reg) => *reg,
        }
    }
}

pub fn bty_array_from_ty(ty: &SourceTypeArray) -> BytecodeTypeArray {
    let mut bytecode_subtypes = Vec::with_capacity(ty.len());
    for subtype in ty.iter() {
        bytecode_subtypes.push(bty_from_ty(subtype));
    }
    BytecodeTypeArray::new(bytecode_subtypes)
}

pub fn convert_trait_type(trait_ty: &TraitType) -> BytecodeTraitType {
    BytecodeTraitType {
        trait_id: TraitId(trait_ty.trait_id.index().try_into().expect("overflow")),
        type_params: bty_array_from_ty(&trait_ty.type_params),
        bindings: trait_ty
            .bindings
            .iter()
            .map(|(alias_id, ty)| {
                (
                    AliasId(alias_id.index().try_into().expect("overflow")),
                    bty_from_ty(ty.clone()),
                )
            })
            .collect::<Vec<_>>(),
    }
}

pub fn bty_from_ty(ty: SourceType) -> BytecodeType {
    match ty {
        SourceType::Unit => BytecodeType::Unit,
        SourceType::Bool => BytecodeType::Bool,
        SourceType::UInt8 => BytecodeType::UInt8,
        SourceType::Char => BytecodeType::Char,
        SourceType::Int32 => BytecodeType::Int32,
        SourceType::Int64 => BytecodeType::Int64,
        SourceType::Float32 => BytecodeType::Float32,
        SourceType::Float64 => BytecodeType::Float64,
        SourceType::Class(class_id, type_params) => BytecodeType::Class(
            ClassId(class_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&type_params),
        ),
        SourceType::TraitObject(trait_id, type_params, bindings) => BytecodeType::TraitObject(
            TraitId(trait_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&type_params),
            bty_array_from_ty(&bindings),
        ),
        SourceType::Enum(enum_id, type_params) => BytecodeType::Enum(
            EnumId(enum_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&type_params),
        ),
        SourceType::Struct(struct_id, type_params) => BytecodeType::Struct(
            StructId(struct_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&type_params),
        ),
        SourceType::Tuple(subtypes) => BytecodeType::Tuple(bty_array_from_ty(&subtypes)),
        SourceType::TypeParam(idx) => BytecodeType::TypeParam(idx.index() as u32),
        SourceType::Lambda(params, return_type) => BytecodeType::Lambda(
            bty_array_from_ty(&params),
            Box::new(bty_from_ty(*return_type)),
        ),
        SourceType::Ptr => BytecodeType::Ptr,
        SourceType::This => BytecodeType::This,
        SourceType::Alias(id, type_params) => {
            assert!(type_params.is_empty());
            BytecodeType::TypeAlias(AliasId(id.index().try_into().expect("overflow")))
        }
        SourceType::Assoc(id, type_params) => {
            assert!(type_params.is_empty());
            BytecodeType::Assoc(
                AliasId(id.index().try_into().expect("overflow")),
                bty_array_from_ty(&type_params),
            )
        }
        SourceType::GenericAssoc {
            tp_id,
            trait_ty,
            assoc_id,
        } => BytecodeType::GenericAssoc {
            type_param_id: tp_id.index().try_into().expect("overflow"),
            trait_ty: convert_trait_type(&trait_ty),
            assoc_id: AliasId(assoc_id.index().try_into().expect("overflow")),
        },
        _ => panic!("SourceType {:?} cannot be converted to BytecodeType", ty),
    }
}

pub fn register_bty_from_ty(ty: SourceType) -> BytecodeType {
    match ty {
        SourceType::Unit => BytecodeType::Unit,
        SourceType::Bool => BytecodeType::Bool,
        SourceType::UInt8 => BytecodeType::UInt8,
        SourceType::Char => BytecodeType::Char,
        SourceType::Int32 => BytecodeType::Int32,
        SourceType::Int64 => BytecodeType::Int64,
        SourceType::Float32 => BytecodeType::Float32,
        SourceType::Float64 => BytecodeType::Float64,
        SourceType::Class(_, _) => BytecodeType::Ptr,
        SourceType::TraitObject(trait_id, type_params, bindings) => BytecodeType::TraitObject(
            TraitId(trait_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&type_params),
            bty_array_from_ty(&bindings),
        ),
        SourceType::Enum(enum_id, type_params) => BytecodeType::Enum(
            EnumId(enum_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&type_params),
        ),
        SourceType::Struct(struct_id, type_params) => BytecodeType::Struct(
            StructId(struct_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&type_params),
        ),
        SourceType::Tuple(subtypes) => BytecodeType::Tuple(bty_array_from_ty(&subtypes)),
        SourceType::TypeParam(idx) => BytecodeType::TypeParam(idx.index() as u32),
        SourceType::Lambda(_, _) => BytecodeType::Ptr,
        SourceType::Ptr => BytecodeType::Ptr,
        SourceType::This => BytecodeType::This,
        SourceType::GenericAssoc {
            tp_id,
            trait_ty,
            assoc_id,
        } => BytecodeType::GenericAssoc {
            type_param_id: tp_id.index().try_into().expect("overflow"),
            trait_ty: convert_trait_type(&trait_ty),
            assoc_id: AliasId(assoc_id.index().try_into().expect("overflow")),
        },
        SourceType::Assoc(assoc_id, type_params) => BytecodeType::Assoc(
            AliasId(assoc_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&type_params),
        ),
        _ => panic!("SourceType {:?} cannot be converted to BytecodeType", ty),
    }
}

fn field_id_from_context_idx(context_idx: ContextFieldId, has_outer_context_slot: bool) -> FieldId {
    let start_idx = if has_outer_context_slot { 1 } else { 0 };
    let ContextFieldId(context_idx) = context_idx;
    FieldId(start_idx + context_idx)
}

fn get_subpatterns(p: &ast::Pattern) -> Option<&Vec<Arc<ast::PatternField>>> {
    match p {
        ast::Pattern::Underscore(..)
        | ast::Pattern::LitBool(..)
        | ast::Pattern::LitChar(..)
        | ast::Pattern::LitString(..)
        | ast::Pattern::LitInt(..)
        | ast::Pattern::LitFloat(..)
        | ast::Pattern::Rest(..)
        | ast::Pattern::Alt(..)
        | ast::Pattern::Tuple(..)
        | ast::Pattern::Error(..) => {
            unreachable!()
        }
        ast::Pattern::Ident(..) => None,
        ast::Pattern::ClassOrStructOrEnum(p) => p.params.as_ref(),
    }
}

fn iterate_subpatterns<F>(a: &AnalysisData, p: &ast::Pattern, mut f: F)
where
    F: FnMut(usize, &ast::PatternField),
{
    if let Some(subpatterns) = get_subpatterns(p) {
        for subpattern in subpatterns {
            if subpattern.pattern.is_rest() || subpattern.pattern.is_underscore() {
                // Do nothing.
            } else {
                let field_id = a
                    .map_field_ids
                    .get(subpattern.id)
                    .cloned()
                    .expect("missing field_id");
                f(field_id, subpattern.as_ref());
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
