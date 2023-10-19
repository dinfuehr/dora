use std::collections::HashMap;
use std::convert::TryInto;

use dora_parser::ast::CmpOp;
use dora_parser::{ast, Span};

use self::expr::{gen_expr, gen_expr_bin_cmp};
use crate::sema::{
    emit_as_bytecode_operation, AnalysisData, CallType, ClassDefinitionId, ConstDefinitionId,
    ContextIdx, EnumDefinitionId, FctDefinition, FctDefinitionId, FieldId, GlobalDefinition,
    GlobalDefinitionId, IdentType, Sema, SourceFileId, StructDefinitionId, TypeParamId, VarId,
    VarLocation,
};
use crate::specialize::specialize_type;
use crate::ty::{SourceType, SourceTypeArray};
use crate::{expr_always_returns, expr_block_always_returns};
use dora_bytecode::{
    BytecodeBuilder, BytecodeFunction, BytecodeType, BytecodeTypeArray, ClassId, ConstPoolEntry,
    ConstPoolIdx, EnumId, FunctionId, GlobalId, Intrinsic, Label, Location, Register, StructId,
    TraitId,
};

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
    let fct = sa.fcts.idx(id);
    let fct = fct.read();
    let analysis = fct.analysis();

    generate_fct(sa, &fct, analysis)
}

pub fn generate_fct(sa: &Sema, fct: &FctDefinition, src: &AnalysisData) -> BytecodeFunction {
    let ast_bytecode_generator = AstBytecodeGen {
        sa,
        type_params_len: fct.type_params.len(),
        is_lambda: fct.is_lambda(),
        return_type: Some(fct.return_type.clone()),
        file_id: fct.file_id,
        span: fct.span,
        analysis: src,

        builder: BytecodeBuilder::new(),
        loops: Vec::new(),
        var_registers: HashMap::new(),
        unit_register: None,
        context_register: None,
    };
    ast_bytecode_generator.generate_fct(&fct.ast)
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
        return_type: Some(global.ty.clone()),
        file_id: global.file_id,
        span: global.span,
        analysis: src,

        builder: BytecodeBuilder::new(),
        loops: Vec::new(),
        var_registers: HashMap::new(),
        unit_register: None,
        context_register: None,
    };
    let expr = global
        .ast
        .initial_value
        .as_ref()
        .expect("missing initializer");
    ast_bytecode_generator.generate_global_initializer(expr)
}

const SELF_VAR_ID: VarId = VarId(0);

struct AstBytecodeGen<'a> {
    sa: &'a Sema,
    type_params_len: usize,
    is_lambda: bool,
    return_type: Option<SourceType>,
    file_id: SourceFileId,
    span: Span,
    analysis: &'a AnalysisData,

    builder: BytecodeBuilder,
    loops: Vec<LoopLabels>,
    var_registers: HashMap<VarId, Register>,
    context_register: Option<Register>,
    unit_register: Option<Register>,
}

impl<'a> AstBytecodeGen<'a> {
    fn loc(&self, span: Span) -> Location {
        self.sa.compute_loc(self.file_id, span)
    }

    fn generate_fct(mut self, ast: &ast::Function) -> BytecodeFunction {
        self.push_scope();
        self.create_params(ast);
        self.create_context();
        self.initialize_params(ast);
        self.emit_function_body(ast);
        self.pop_scope();
        self.builder.generate()
    }

    fn generate_global_initializer(mut self, expr: &ast::ExprData) -> BytecodeFunction {
        self.push_scope();
        self.builder.set_params(Vec::new());
        self.create_context();
        self.emit_global_initializer(expr);
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
            let register_bty = register_bty_from_ty(var_ty);
            let reg = self.alloc_var(register_bty);
            self.var_registers.insert(SELF_VAR_ID, reg);
        }

        for param in &ast.params {
            let var_id = *self.analysis.map_vars.get(param.id).unwrap();
            let ty = self.var_ty(var_id);

            let bty = bty_from_ty(ty.clone());
            let register_bty = register_bty_from_ty(ty);
            params.push(bty);
            let reg = self.alloc_var(register_bty);
            self.var_registers.insert(var_id, reg);
        }

        self.builder.set_params(params);
    }

    fn initialize_params(&mut self, ast: &ast::Function) {
        let next_register_idx = if self.analysis.has_self() {
            let var_self = self.analysis.vars.get_self();
            let reg = Register(0);

            match var_self.location {
                VarLocation::Context(context_idx) => {
                    self.store_in_context(reg, context_idx, self.loc(self.span));
                }

                VarLocation::Stack => {
                    self.var_registers.insert(SELF_VAR_ID, reg);
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
                VarLocation::Context(context_idx) => {
                    self.store_in_context(reg, context_idx, self.loc(self.span));
                }

                VarLocation::Stack => {
                    self.var_registers.insert(var_id, reg);
                }
            }
        }
    }

    fn emit_function_body(&mut self, ast: &ast::Function) {
        let return_type = self
            .return_type
            .as_ref()
            .expect("missing return type")
            .clone();
        let bty_return_type = if return_type.is_unit() {
            None
        } else {
            Some(bty_from_ty(return_type.clone()))
        };
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
                self.emit_ret_value(reg);
            }

            needs_return = false;
            self.free_if_temp(reg);
        }

        if needs_return && return_type.is_unit() {
            let dest = self.ensure_unit_register();
            self.builder.emit_ret(dest);
        }
    }

    fn emit_global_initializer(&mut self, expr: &ast::ExprData) {
        let result = gen_expr(self, expr, DataDest::Alloc);
        self.builder.emit_ret(result);
        self.free_if_temp(result);
    }

    fn create_context(&mut self) {
        if let Some(cls_id) = self.analysis.context_cls_id {
            let context_register = self.builder.alloc_global(BytecodeType::Ptr);
            let idx = self.builder.add_const_cls_types(
                ClassId(cls_id.index().try_into().expect("overflow")),
                bty_array_from_ty(&self.identity_type_params()),
            );
            self.builder
                .emit_new_object(context_register, idx, self.loc(self.span));
            self.context_register = Some(context_register);

            if self.analysis.context_has_outer_context_slot() {
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
                    .emit_load_field(outer_context_reg, self_reg, idx, self.loc(self.span));

                // Store value in outer_context field of context object.
                let idx = self.builder.add_const_field_types(
                    ClassId(cls_id.index().try_into().expect("overflow")),
                    bty_array_from_ty(&self.identity_type_params()),
                    0,
                );
                self.builder.emit_store_field(
                    outer_context_reg,
                    context_register,
                    idx,
                    self.loc(self.span),
                );

                self.free_temp(outer_context_reg);
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &ast::StmtData) {
        match *stmt {
            ast::StmtData::Expr(ref expr) => self.visit_stmt_expr(expr),
            ast::StmtData::Let(ref stmt) => self.visit_stmt_let(stmt),
        }
    }

    fn visit_expr_for(&mut self, expr: &ast::ExprForType, _dest: DataDest) -> Register {
        self.visit_expr_for_iterator(expr);
        self.ensure_unit_register()
    }

    fn visit_stmt_for_pattern_setup(&mut self, pattern: &ast::LetPattern) {
        match pattern {
            ast::LetPattern::Ident(ref ident) => {
                let var_id = *self.analysis.map_vars.get(ident.id).unwrap();
                let var = self.analysis.vars.get_var(var_id);

                if !var.ty.is_unit() {
                    let bty: BytecodeType = register_bty_from_ty(var.ty.clone());

                    match var.location {
                        VarLocation::Context(_) => {
                            // Nothing to do here.
                        }

                        VarLocation::Stack => {
                            let var_reg = self.alloc_var(bty);
                            self.var_registers.insert(var_id, var_reg);
                        }
                    }
                }
            }
            ast::LetPattern::Underscore(_) => {
                // nothing to do
            }
            ast::LetPattern::Tuple(ref tuple) => {
                for part in &tuple.parts {
                    self.visit_stmt_for_pattern_setup(part);
                }
            }
        }
    }

    fn visit_stmt_for_pattern_assign_iterator(
        &mut self,
        pattern: &ast::LetPattern,
        next_reg: Register,
        next_ty: SourceType,
    ) {
        match pattern {
            ast::LetPattern::Ident(ref ident) => {
                let var_id = *self.analysis.map_vars.get(ident.id).unwrap();
                let var = self.analysis.vars.get_var(var_id);

                if !var.ty.is_unit() {
                    match var.location {
                        VarLocation::Context(context_idx) => {
                            self.store_in_context(next_reg, context_idx, self.loc(ident.span));
                        }

                        VarLocation::Stack => {
                            let var_reg = self.var_reg(var_id);
                            self.emit_mov(var_reg, next_reg);
                        }
                    }
                }
            }

            ast::LetPattern::Underscore(_) => {
                // nothing to do
            }

            ast::LetPattern::Tuple(ref tuple) => {
                assert!(tuple.parts.len() > 0);
                self.destruct_tuple_pattern(tuple, next_reg, next_ty);
            }
        }
    }

    fn destruct_tuple_pattern(
        &mut self,
        tuple: &ast::LetTupleType,
        tuple_reg: Register,
        tuple_ty: SourceType,
    ) {
        let tuple_subtypes = tuple_ty.tuple_subtypes();

        for (idx, part) in tuple.parts.iter().enumerate() {
            match &**part {
                ast::LetPattern::Ident(ref ident) => {
                    let var_id = *self.analysis.map_vars.get(ident.id).unwrap();
                    let ty = self.var_ty(var_id);

                    if !ty.is_unit() {
                        let bytecode_ty: BytecodeType = register_bty_from_ty(ty);
                        let var_reg = self.alloc_var(bytecode_ty);
                        self.var_registers.insert(var_id, var_reg);
                        let idx = self
                            .builder
                            .add_const_tuple_element(bty_from_ty(tuple_ty.clone()), idx as u32);

                        self.builder
                            .emit_load_tuple_element(var_reg, tuple_reg, idx);
                    }
                }

                ast::LetPattern::Underscore(_) => {
                    // nothing to do
                }

                ast::LetPattern::Tuple(ref tuple) => {
                    let ty = tuple_subtypes[idx].clone();

                    if !ty.is_unit() {
                        let register_ty = register_bty_from_ty(ty.clone());
                        let temp_reg = self.alloc_temp(register_ty);
                        let idx = self
                            .builder
                            .add_const_tuple_element(bty_from_ty(tuple_ty.clone()), idx as u32);
                        self.builder
                            .emit_load_tuple_element(temp_reg, tuple_reg, idx);
                        self.destruct_tuple_pattern(tuple, temp_reg, ty);
                        self.free_temp(temp_reg);
                    }
                }
            }
        }
    }

    fn visit_expr_for_iterator(&mut self, stmt: &ast::ExprForType) {
        self.push_scope();
        let for_type_info = self.analysis.map_fors.get(stmt.id).unwrap().clone();

        // Emit: <obj> = <expr> (for <var> in <expr> { ... })
        let object_reg = gen_expr(self, &stmt.expr, DataDest::Alloc);

        let iterator_reg = if let Some(make_iterator) = for_type_info.make_iterator {
            let object_type = self.ty(stmt.expr.id());
            let object_type_params = bty_array_from_ty(&object_type.type_params());

            // Emit: <iterator> = <obj>.makeIterator();
            let iterator_reg = self.alloc_var(BytecodeType::Ptr);
            self.builder.emit_push_register(object_reg);
            let fct_idx = self
                .builder
                .add_const_fct_types(FunctionId(make_iterator.0 as u32), object_type_params);
            self.builder
                .emit_invoke_direct(iterator_reg, fct_idx, self.loc(stmt.expr.span()));
            iterator_reg
        } else {
            // Object is already the iterator - just use it
            object_reg
        };

        let lbl_cond = self.builder.define_label();
        self.builder.emit_loop_start();

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
            FunctionId(for_type_info.next.0 as u32),
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
            FunctionId(self.sa.known.functions.option_is_none().0 as u32),
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
                FunctionId(self.sa.known.functions.option_unwrap().0 as u32),
                bty_array_from_ty(&option_type_params),
            );
            self.builder.emit_push_register(next_result_reg);
            self.builder
                .emit_invoke_direct(value_reg, fct_idx, self.loc(stmt.expr.span()));
            self.free_temp(next_result_reg);

            self.visit_stmt_for_pattern_setup(&stmt.pattern);
            self.visit_stmt_for_pattern_assign_iterator(
                &stmt.pattern,
                value_reg,
                for_type_info.value_type,
            );
        }

        self.loops.push(LoopLabels::new(lbl_cond, lbl_end));
        self.emit_expr_for_effect(&stmt.block);
        self.loops.pop().unwrap();

        self.builder.emit_jump_loop(lbl_cond);
        self.builder.bind_label(lbl_end);

        self.pop_scope();

        self.free_if_temp(object_reg);
    }

    fn visit_stmt_let(&mut self, stmt: &ast::StmtLetType) {
        match &*stmt.pattern {
            ast::LetPattern::Ident(ref ident) => {
                self.visit_stmt_let_ident(stmt, ident);
            }

            ast::LetPattern::Underscore(_) => {
                self.visit_stmt_let_underscore(stmt);
            }

            ast::LetPattern::Tuple(ref tuple) => {
                self.visit_stmt_let_pattern(stmt, tuple);
            }
        }
    }

    fn visit_stmt_let_ident(&mut self, stmt: &ast::StmtLetType, ident: &ast::LetIdentType) {
        let var_id = *self.analysis.map_vars.get(ident.id).unwrap();
        let var = self.analysis.vars.get_var(var_id);

        let ty: BytecodeType = register_bty_from_ty(var.ty.clone());

        match var.location {
            VarLocation::Context(context_idx) => {
                if let Some(ref expr) = stmt.expr {
                    let value_reg = gen_expr(self, expr, DataDest::Alloc);
                    self.store_in_context(value_reg, context_idx, self.loc(ident.span));
                    self.free_if_temp(value_reg);
                }
            }

            VarLocation::Stack => {
                let dest = if var.ty.is_unit() {
                    DataDest::Effect
                } else {
                    let var_reg = self.alloc_var(ty);

                    self.var_registers.insert(var_id, var_reg);

                    DataDest::Reg(var_reg)
                };

                if let Some(ref expr) = stmt.expr {
                    gen_expr(self, expr, dest);
                }
            }
        }
    }

    fn visit_stmt_let_underscore(&mut self, stmt: &ast::StmtLetType) {
        if let Some(ref expr) = stmt.expr {
            gen_expr(self, expr, DataDest::Effect);
        }
    }

    fn visit_stmt_let_pattern(&mut self, stmt: &ast::StmtLetType, pattern: &ast::LetTupleType) {
        if let Some(ref expr) = stmt.expr {
            let ty = self.ty(expr.id());

            if ty.is_unit() {
                gen_expr(self, expr, DataDest::Effect);
            } else {
                let tuple_reg = gen_expr(self, expr, DataDest::Alloc);
                self.destruct_tuple_pattern(pattern, tuple_reg, ty);
                self.free_if_temp(tuple_reg);
            }
        } else {
            self.visit_stmt_let_tuple_init(pattern);
        }
    }

    fn visit_stmt_let_tuple_init(&mut self, tuple: &ast::LetTupleType) {
        for part in &tuple.parts {
            match &**part {
                ast::LetPattern::Ident(ref ident) => {
                    let var_id = *self.analysis.map_vars.get(ident.id).unwrap();
                    let ty = self.var_ty(var_id);
                    let ty: BytecodeType = register_bty_from_ty(ty);
                    let var_reg = self.alloc_var(ty.clone());
                    self.var_registers.insert(var_id, var_reg);
                }

                ast::LetPattern::Underscore(_) => {
                    // nothing to do
                }

                ast::LetPattern::Tuple(ref tuple) => {
                    self.visit_stmt_let_tuple_init(tuple);
                }
            }
        }
    }

    fn visit_expr_while(&mut self, stmt: &ast::ExprWhileType, _dest: DataDest) -> Register {
        let cond_lbl = self.builder.define_label();
        let end_lbl = self.builder.create_label();
        self.builder.emit_loop_start();
        let cond_reg = gen_expr(self, &stmt.cond, DataDest::Alloc);
        self.builder.emit_jump_if_false(cond_reg, end_lbl);
        self.free_if_temp(cond_reg);
        self.loops.push(LoopLabels::new(cond_lbl, end_lbl));
        self.emit_expr_for_effect(&stmt.block);
        self.loops.pop().unwrap();
        self.builder.emit_jump_loop(cond_lbl);
        self.builder.bind_label(end_lbl);
        self.ensure_unit_register()
    }

    fn visit_stmt_expr(&mut self, stmt: &ast::StmtExprType) {
        let reg = gen_expr(self, &stmt.expr, DataDest::Effect);
        self.free_if_temp(reg);
    }

    fn visit_expr_return(&mut self, ret: &ast::ExprReturnType, _dest: DataDest) -> Register {
        if let Some(ref expr) = ret.expr {
            let result_reg = gen_expr(self, expr, DataDest::Alloc);
            self.emit_ret_value(result_reg);
            self.free_if_temp(result_reg);
        } else {
            let dest = self.ensure_unit_register();
            self.builder.emit_ret(dest);
        }
        self.ensure_unit_register()
    }

    fn emit_ret_value(&mut self, result_reg: Register) {
        let ret_ty = self
            .return_type
            .as_ref()
            .expect("missing return type")
            .clone();

        if ret_ty.is_unit() {
            let dest = self.ensure_unit_register();
            self.builder.emit_ret(dest);
            return;
        }

        self.builder.emit_ret(result_reg);
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
        let reg = gen_expr(self, expr, DataDest::Effect);
        self.free_if_temp(reg);
    }

    fn visit_expr_type_param(&mut self, expr: &ast::ExprTypeParamType, dest: DataDest) -> Register {
        let ident_type = self.analysis.map_idents.get(expr.id).cloned().unwrap();

        match ident_type {
            IdentType::EnumValue(enum_id, type_params, variant_idx) => {
                self.emit_new_enum(enum_id, type_params, variant_idx, self.loc(expr.span), dest)
            }

            _ => unreachable!(),
        }
    }

    fn visit_expr_template(&mut self, expr: &ast::ExprTemplateType, dest: DataDest) -> Register {
        let buffer_register = self.ensure_register(dest, BytecodeType::Ptr);

        // build StringBuffer::empty() call
        let fct_id = self.sa.known.functions.string_buffer_empty();
        let fct_idx = self.builder.add_const_fct(FunctionId(fct_id.0 as u32));
        self.builder
            .emit_invoke_static(buffer_register, fct_idx, self.loc(expr.span));

        let part_register = self.alloc_temp(BytecodeType::Ptr);

        for part in &expr.parts {
            if let Some(ref lit_str) = part.to_lit_str() {
                let value = self.analysis.literal_string(lit_str.id);
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
                    let trait_ = self.sa.traits[trait_id].read();
                    let to_string_id = trait_
                        .find_method(self.sa, name, false)
                        .expect("Stringable::toString() not found");

                    let fct_idx = self.builder.add_const_generic(
                        type_list_id.0 as u32,
                        FunctionId(to_string_id.0 as u32),
                        BytecodeTypeArray::empty(),
                    );

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
                    let to_string_id = self
                        .analysis
                        .map_templates
                        .get(part.id())
                        .expect("missing toString id");

                    let fct_idx = self
                        .builder
                        .add_const_fct(FunctionId(to_string_id.0 as u32));
                    self.builder
                        .emit_invoke_direct(part_register, fct_idx, self.loc(part.span()));

                    self.free_if_temp(expr_register);
                }
            }

            // build StringBuffer::append() call
            let fct_id = self.sa.known.functions.string_buffer_append();
            let fct_idx = self.builder.add_const_fct(FunctionId(fct_id.0 as u32));
            self.builder.emit_push_register(buffer_register);
            self.builder.emit_push_register(part_register);
            let dest_reg = self.ensure_unit_register();
            self.builder
                .emit_invoke_direct(dest_reg, fct_idx, self.loc(expr.span));
        }

        self.free_temp(part_register);

        // build StringBuffer::toString() call
        let fct_id = self.sa.known.functions.string_buffer_to_string();
        let fct_idx = self.builder.add_const_fct(FunctionId(fct_id.0 as u32));
        self.builder.emit_push_register(buffer_register);
        self.builder
            .emit_invoke_direct(buffer_register, fct_idx, self.loc(expr.span));

        buffer_register
    }

    fn visit_expr_path(&mut self, expr: &ast::ExprPathType, dest: DataDest) -> Register {
        let ident_type = self.analysis.map_idents.get(expr.id).cloned().unwrap();

        match ident_type {
            IdentType::EnumValue(enum_id, type_params, variant_idx) => {
                self.emit_new_enum(enum_id, type_params, variant_idx, self.loc(expr.span), dest)
            }

            IdentType::Const(const_id) => self.visit_expr_ident_const(const_id, dest),

            _ => {
                panic!("ident_type = {:?}", ident_type);
            }
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
        let enum_id = EnumId(enum_id.0);
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

        let (trait_id, type_params) = match check_type {
            SourceType::Trait(trait_id, ref type_params) => (trait_id, type_params.clone()),
            _ => unreachable!(),
        };

        let trait_id = TraitId(trait_id.0);

        let object = gen_expr(self, &expr.object, DataDest::Alloc);
        let idx = self.builder.add_const_trait(
            TraitId(trait_id.0),
            bty_array_from_ty(&check_type.type_params()),
            bty_from_ty(object_type),
        );
        let ty = BytecodeType::Trait(trait_id, bty_array_from_ty(&type_params));
        let dest = self.ensure_register(dest, ty);
        self.builder
            .emit_new_trait_object(dest, idx, object, self.loc(expr.span));
        self.free_if_temp(object);
        dest
    }

    fn visit_expr_lambda(&mut self, node: &ast::Function, dest: DataDest) -> Register {
        let dest = self.ensure_register(dest, BytecodeType::Ptr);

        let lambda_fct_id = *self
            .analysis
            .map_lambdas
            .get(node.id)
            .expect("missing lambda id");

        let lambda_fct = self.sa.fcts.idx(lambda_fct_id);
        let lambda_fct = lambda_fct.read();
        let lambda_analysis = lambda_fct.analysis();

        if lambda_analysis.outer_context_access() {
            self.builder
                .emit_push_register(self.context_register.expect("missing context"));
        }

        let idx = self.builder.add_const_fct_types(
            FunctionId(lambda_fct_id.0 as u32),
            bty_array_from_ty(&self.identity_type_params()),
        );
        self.builder.emit_new_lambda(dest, idx, self.loc(node.span));

        dest
    }

    fn visit_expr_if(&mut self, expr: &ast::ExprIfType, dest: DataDest) -> Register {
        let ty = self.ty(expr.id);

        if let Some(ref else_block) = expr.else_block {
            let dest = match dest {
                DataDest::Effect => DataDest::Effect,
                DataDest::Alloc | DataDest::Reg(..) => {
                    DataDest::Reg(self.ensure_register(dest, register_bty_from_ty(ty)))
                }
            };

            let else_lbl = self.builder.create_label();
            let end_lbl = self.builder.create_label();

            let cond_reg = gen_expr(self, &expr.cond, DataDest::Alloc);
            self.builder.emit_jump_if_false(cond_reg, else_lbl);
            self.free_if_temp(cond_reg);

            gen_expr(self, &expr.then_block, dest);

            if !expr_always_returns(&expr.then_block) {
                self.builder.emit_jump(end_lbl);
            }

            self.builder.bind_label(else_lbl);
            gen_expr(self, else_block, dest);
            self.builder.bind_label(end_lbl);

            match dest {
                DataDest::Effect => Register::invalid(),
                DataDest::Reg(reg) => reg,
                DataDest::Alloc => unreachable!(),
            }
        } else {
            // Without else-branch there can't be return value
            assert!(ty.is_unit());

            let end_lbl = self.builder.create_label();
            let cond_reg = gen_expr(self, &expr.cond, DataDest::Alloc);
            self.builder.emit_jump_if_false(cond_reg, end_lbl);
            self.free_if_temp(cond_reg);

            self.emit_expr_for_effect(&expr.then_block);

            self.builder.bind_label(end_lbl);
            Register::invalid()
        }
    }

    fn visit_expr_block(&mut self, block: &ast::ExprBlockType, dest: DataDest) -> Register {
        self.push_scope();

        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }

        let result = if let Some(ref expr) = block.expr {
            gen_expr(self, expr, dest)
        } else {
            Register::invalid()
        };

        self.pop_scope();

        result
    }

    fn visit_expr_dot(&mut self, expr: &ast::ExprDotType, dest: DataDest) -> Register {
        let object_ty = self.ty(expr.lhs.id());

        if object_ty.is_tuple() {
            return self.visit_expr_dot_tuple(expr, object_ty, dest);
        }

        if let Some(struct_id) = object_ty.struct_id() {
            let type_params = object_ty.type_params();
            return self.visit_expr_dot_struct(expr, struct_id, type_params, dest);
        }

        let (cls_ty, field_id) = {
            let ident_type = self.analysis.map_idents.get(expr.id).unwrap();

            match ident_type {
                IdentType::Field(ty, field) => (ty.clone(), *field),
                _ => unreachable!(),
            }
        };

        let cls_id = cls_ty.cls_id().expect("class expected");
        let type_params = cls_ty.type_params();
        let field_idx = self.builder.add_const_field_types(
            ClassId(cls_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&type_params),
            field_id.0 as u32,
        );

        let field_ty = {
            let cls = &self.sa.classes[cls_id];
            let field = &cls.fields[field_id.to_usize()];
            field.ty()
        };

        let field_ty = specialize_type(self.sa, field_ty, &type_params);

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

        let struct_ = &self.sa.structs[struct_id];
        let field = &struct_.fields[field_idx.to_usize()];
        let ty = specialize_type(self.sa, field.ty(), &type_params);

        if ty.is_unit() {
            assert!(dest.is_unit());
            self.free_if_temp(struct_obj);
            return Register::invalid();
        }

        let ty: BytecodeType = register_bty_from_ty(ty);
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
        let (value_i64, _) = self.analysis.literal_value(expr.rhs.id());
        let idx: u32 = value_i64.try_into().expect("too large");

        let subtypes = tuple_ty.tuple_subtypes();
        let ty = subtypes[idx as usize].clone();

        if ty.is_unit() {
            assert!(dest.is_unit());
            self.free_if_temp(tuple);
            return Register::invalid();
        }

        let ty: BytecodeType = register_bty_from_ty(ty);
        let dest = self.ensure_register(dest, ty);
        let idx = self
            .builder
            .add_const_tuple_element(bty_from_ty(tuple_ty), idx);
        self.builder.emit_load_tuple_element(dest, tuple, idx);

        self.free_if_temp(tuple);

        dest
    }

    fn visit_expr_assert(&mut self, expr: &ast::ExprCallType, dest: DataDest) {
        assert!(dest.is_unit());
        let assert_reg = gen_expr(self, &*expr.args[0], DataDest::Alloc);
        self.builder.emit_push_register(assert_reg);
        let fid = self.sa.known.functions.assert();
        let idx = self.builder.add_const_fct(FunctionId(fid.0 as u32));
        let dest = self.ensure_unit_register();
        self.builder
            .emit_invoke_static(dest, idx, self.loc(expr.span));
        self.free_if_temp(assert_reg);
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
            | CallType::TraitObjectMethod(..)
            | CallType::Fct(..) => {}

            _ => panic!("unknown call type = {:?}", call_type),
        }

        // Find method that is called
        let callee_id = call_type.fct_id().expect("FctId missing");

        let callee = self.sa.fcts.idx(callee_id);
        let callee = callee.read();

        let callee_idx = self.add_const_pool_entry_for_call(&callee, &call_type);

        // Determine types for arguments and return values
        let (arg_types, return_type) = self.determine_callee_types(&call_type, &*callee);

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
            arguments.push(gen_expr(self, arg, DataDest::Alloc));
        }

        for &arg_reg in &arguments {
            self.builder.emit_push_register(arg_reg);
        }

        let enum_id = enum_ty.enum_id().expect("enum expected");
        let type_params = enum_ty.type_params();

        let idx = self.builder.add_const_enum_variant(
            EnumId(enum_id.0),
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

        let lambda_object = gen_expr(self, node.object_or_callee(), DataDest::Alloc);
        arguments.push(lambda_object);

        for arg in &node.args {
            arguments.push(gen_expr(self, arg, DataDest::Alloc));
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
            arguments.push(gen_expr(self, arg, DataDest::Alloc));
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
        let mut arguments = Vec::new();

        for arg in &expr.args {
            arguments.push(gen_expr(self, arg, DataDest::Alloc));
        }

        for &arg_reg in &arguments {
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
            self.free_if_temp(arg_reg);
        }

        dest_reg
    }

    fn determine_callee_types(
        &mut self,
        call_type: &CallType,
        fct: &FctDefinition,
    ) -> (Vec<SourceType>, SourceType) {
        let return_type = self.specialize_type_for_call(&call_type, fct.return_type.clone());

        let mut arg_types = Vec::with_capacity(fct.params_with_self().len());

        if fct.has_hidden_self_argument() {
            let self_type = match call_type {
                CallType::TraitObjectMethod(trait_ty, _) => {
                    // trait methods use Self as type for self argument but specialize_type_for_call can't handle Self.
                    assert!(fct.params_with_self()[0].is_self() && !fct.is_static);
                    trait_ty.clone()
                }
                _ => {
                    let arg = fct.params_with_self()[0].clone();
                    self.specialize_type_for_call(&call_type, arg.clone())
                }
            };

            arg_types.push(self_type);
        }

        for arg in fct.params_without_self() {
            let arg = self.specialize_type_for_call(&call_type, arg.clone());
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
            CallType::Method(_, _, _)
            | CallType::GenericMethod(_, _, _)
            | CallType::TraitObjectMethod(_, _) => {
                let obj_expr = expr.object().expect("method target required");
                let reg = gen_expr(self, obj_expr, DataDest::Alloc);

                Some(reg)
            }
            CallType::Expr(_, _, _) => Some(gen_expr(self, &expr.callee, DataDest::Alloc)),
            CallType::GenericStaticMethod(..) | CallType::Fct(..) => None,
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
            CallType::Expr(_, _, _)
            | CallType::Method(_, _, _)
            | CallType::GenericMethod(_, _, _) => 1,
            _ => 0,
        };

        // Calculate number of non-variadic arguments
        let non_variadic_arguments = if callee.is_variadic {
            arg_types.len() - arg_start_offset - 1
        } else {
            arg_types.len()
        };

        // Evaluate non-variadic arguments and track registers.
        for arg in expr.args.iter().take(non_variadic_arguments) {
            let reg = gen_expr(self, arg, DataDest::Alloc);
            registers.push(reg);
        }

        if callee.is_variadic {
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
        let cls_id = ty.cls_id().expect("class expected");
        let type_params = ty.type_params();
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
            .emit_new_array(array_reg, cls_idx, length_reg, self.loc(expr.span));

        if element_ty.is_unit() {
            // Evaluate rest arguments
            for arg in expr.args.iter().skip(non_variadic_arguments) {
                gen_expr(self, arg, DataDest::Effect);
            }
        } else {
            let index_reg = self.alloc_temp(BytecodeType::Int64);

            // Evaluate rest arguments and store them in array
            for (idx, arg) in expr.args.iter().skip(non_variadic_arguments).enumerate() {
                let arg_reg = gen_expr(self, arg, DataDest::Alloc);
                self.builder.emit_const_int64(index_reg, idx as i64);
                self.builder
                    .emit_store_array(arg_reg, array_reg, index_reg, self.loc(expr.span));
                self.free_if_temp(arg_reg);
            }

            self.free_if_temp(index_reg);
        }

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
            CallType::Method(_, _, _) => {
                self.builder
                    .emit_invoke_direct(return_reg, callee_idx, location);
            }
            CallType::Fct(_, _) => {
                self.builder
                    .emit_invoke_static(return_reg, callee_idx, location);
            }
            CallType::Expr(_, _, _) => {
                self.builder
                    .emit_invoke_direct(return_reg, callee_idx, location);
            }
            CallType::TraitObjectMethod(_, _) => {
                self.builder
                    .emit_invoke_virtual(return_reg, callee_idx, location);
            }
            CallType::GenericMethod(_, _, _) => {
                self.builder
                    .emit_invoke_generic_direct(return_reg, callee_idx, location);
            }
            CallType::GenericStaticMethod(_, _, _) => {
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
        if dest.is_effect() {
            return Register::invalid();
        }

        if self.is_lambda {
            let ident = self
                .analysis
                .map_idents
                .get(expr.id)
                .expect("missing ident");
            let (distance, context_idx) = match ident {
                IdentType::Context(distance, context_idx) => (*distance, *context_idx),
                _ => unreachable!(),
            };
            self.visit_expr_ident_context(distance, context_idx, dest, self.loc(expr.span))
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
        if dest.is_effect() {
            return Register::invalid();
        }

        let dest = self.ensure_register(dest, BytecodeType::Char);

        let value = self.analysis.literal_char(lit.id);
        self.builder.emit_const_char(dest, value);

        dest
    }

    fn visit_expr_lit_int(
        &mut self,
        lit: &ast::ExprLitIntType,
        dest: DataDest,
        _neg: bool,
    ) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

        let ty = self.analysis.ty(lit.id);
        let (value_i64, value_f64) = self.analysis.literal_value(lit.id);

        let ty = match ty {
            SourceType::UInt8 => BytecodeType::UInt8,
            SourceType::Int32 => BytecodeType::Int32,
            SourceType::Int64 => BytecodeType::Int64,
            SourceType::Float32 => {
                let dest = self.ensure_register(dest, BytecodeType::Float32);
                self.builder.emit_const_float32(dest, value_f64 as f32);
                return dest;
            }
            SourceType::Float64 => {
                let dest = self.ensure_register(dest, BytecodeType::Float64);
                self.builder.emit_const_float64(dest, value_f64);
                return dest;
            }
            _ => unreachable!(),
        };

        let dest = self.ensure_register(dest, ty.clone());

        match ty {
            BytecodeType::UInt8 => self.builder.emit_const_uint8(dest, value_i64 as u8),
            BytecodeType::Int32 => self.builder.emit_const_int32(dest, value_i64 as i32),
            BytecodeType::Int64 => self.builder.emit_const_int64(dest, value_i64),
            _ => unreachable!(),
        }

        dest
    }

    fn visit_expr_lit_float(&mut self, lit: &ast::ExprLitFloatType, dest: DataDest) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

        let ty = self.analysis.ty(lit.id);
        let (_, value_f64) = self.analysis.literal_value(lit.id);

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
        if dest.is_effect() {
            return Register::invalid();
        }

        let dest = self.ensure_register(dest, BytecodeType::Ptr);
        let value = self.analysis.literal_string(lit.id);
        self.builder.emit_const_string(dest, value);

        dest
    }

    fn visit_expr_lit_bool(&mut self, lit: &ast::ExprLitBoolType, dest: DataDest) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

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
            assert!(dest.is_unit());
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

        let subtypes = ty.tuple_subtypes();
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

        let callee = self.sa.fcts.idx(callee_id);
        let callee = callee.read();

        let callee_idx = self.add_const_pool_entry_for_call(&callee, &call_type);

        let function_return_type: SourceType =
            self.specialize_type_for_call(call_type, callee.return_type.clone());

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

        let callee = self.sa.fcts.idx(callee_id);
        let callee = callee.read();

        let callee_idx = self.add_const_pool_entry_for_call(&callee, &call_type);

        let function_return_type: SourceType =
            self.specialize_type_for_call(call_type, callee.return_type.clone());

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
                1 => {
                    self.emit_intrinsic_bin(object, &expr.args[0], info, self.loc(expr.span), dest)
                }
                2 => {
                    assert_eq!(intrinsic, Intrinsic::ArraySet);
                    self.emit_intrinsic_array_set(
                        expr.object().unwrap(),
                        &expr.args[0],
                        &expr.args[1],
                        self.loc(expr.span),
                        dest,
                    )
                }
                _ => unreachable!(),
            }
        } else {
            match intrinsic {
                Intrinsic::Assert => {
                    self.visit_expr_assert(expr, dest);
                    Register::invalid()
                }

                Intrinsic::ArrayGet => self.emit_intrinsic_bin(
                    &expr.callee,
                    &expr.args[0],
                    info,
                    self.loc(expr.span),
                    dest,
                ),

                Intrinsic::ArrayNewOfSize => self.emit_intrinsic_new_array(expr, dest),

                Intrinsic::ArrayWithValues => {
                    let ty = self.ty(expr.id);
                    assert_eq!(
                        ty.cls_id().expect("class expected"),
                        self.sa.known.classes.array()
                    );
                    let type_params = ty.type_params();
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
        let cls_id = element_ty.cls_id().expect("class expected");
        let type_params = element_ty.type_params();
        let cls_idx = self.builder.add_const_cls_types(
            ClassId(cls_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&type_params),
        );

        let array_reg = self.ensure_register(dest, BytecodeType::Ptr);
        let length_reg = gen_expr(self, &expr.args[0], DataDest::Alloc);

        self.builder
            .emit_new_array(array_reg, cls_idx, length_reg, self.loc(expr.span));

        self.free_if_temp(length_reg);

        array_reg
    }

    fn emit_bin_is(&mut self, expr: &ast::ExprBinType, dest: DataDest) -> Register {
        if dest.is_effect() {
            self.emit_expr_for_effect(&expr.lhs);
            self.emit_expr_for_effect(&expr.rhs);
            return Register::invalid();
        }

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
        if dest.is_effect() {
            let end_lbl = self.builder.create_label();
            let dest = gen_expr(self, &expr.lhs, DataDest::Alloc);
            self.builder.emit_jump_if_true(dest, end_lbl);
            self.free_if_temp(dest);

            self.emit_expr_for_effect(&expr.rhs);
            self.builder.bind_label(end_lbl);

            Register::invalid()
        } else {
            let end_lbl = self.builder.create_label();
            let dest = self.ensure_register(dest, BytecodeType::Bool);

            gen_expr(self, &expr.lhs, DataDest::Reg(dest));
            self.builder.emit_jump_if_true(dest, end_lbl);
            gen_expr(self, &expr.rhs, DataDest::Reg(dest));
            self.builder.bind_label(end_lbl);

            dest
        }
    }

    fn emit_bin_and(&mut self, expr: &ast::ExprBinType, dest: DataDest) -> Register {
        if dest.is_effect() {
            let end_lbl = self.builder.create_label();
            let dest = gen_expr(self, &expr.lhs, DataDest::Alloc);
            self.builder.emit_jump_if_false(dest, end_lbl);
            self.free_if_temp(dest);

            self.emit_expr_for_effect(&expr.rhs);
            self.builder.bind_label(end_lbl);

            Register::invalid()
        } else {
            let end_lbl = self.builder.create_label();
            let dest = self.ensure_register(dest, BytecodeType::Bool);

            gen_expr(self, &expr.lhs, DataDest::Reg(dest));
            self.builder.emit_jump_if_false(dest, end_lbl);
            gen_expr(self, &expr.rhs, DataDest::Reg(dest));
            self.builder.bind_label(end_lbl);

            dest
        }
    }

    fn emit_intrinsic_array_set(
        &mut self,
        arr: &ast::ExprData,
        idx: &ast::ExprData,
        src: &ast::ExprData,
        location: Location,
        dest: DataDest,
    ) -> Register {
        assert!(dest.is_unit());

        let arr = gen_expr(self, arr, DataDest::Alloc);
        let idx = gen_expr(self, idx, DataDest::Alloc);
        let src = gen_expr(self, src, DataDest::Alloc);

        self.builder.emit_store_array(src, arr, idx, location);

        self.free_if_temp(arr);
        self.free_if_temp(idx);
        self.free_if_temp(src);

        Register::invalid()
    }

    fn emit_intrinsic_un(
        &mut self,
        opnd: &ast::ExprData,
        info: IntrinsicInfo,
        location: Location,
        dest: DataDest,
    ) -> Register {
        let intrinsic = info.intrinsic;

        let fct = self.sa.fcts.idx(info.fct_id.expect("missing method"));
        let fct = fct.read();
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
                let ty = self.ty(lhs.id());
                let ty: BytecodeType = if ty.cls_id() == Some(self.sa.known.classes.string()) {
                    BytecodeType::UInt8
                } else {
                    let ty = ty.type_params();
                    let ty = ty[0].clone();

                    register_bty_from_ty(ty)
                };

                let dest = self.ensure_register(dest, ty.clone());

                let arr = gen_expr(self, lhs, DataDest::Alloc);
                let idx = gen_expr(self, rhs, DataDest::Alloc);

                self.builder.emit_load_array(dest, arr, idx, location);

                self.free_if_temp(arr);
                self.free_if_temp(idx);

                return dest;
            }

            _ => {}
        }

        let fct_id = info.fct_id.expect("missing function");
        let fct = self.sa.fcts.idx(fct_id);
        let fct = fct.read();

        let result_type = fct.return_type_bty();

        let dest = self.ensure_register(dest, result_type);

        let lhs_reg = gen_expr(self, lhs, DataDest::Alloc);
        let rhs_reg = gen_expr(self, rhs, DataDest::Alloc);

        match intrinsic {
            Intrinsic::UInt8Eq
            | Intrinsic::BoolEq
            | Intrinsic::CharEq
            | Intrinsic::Int32Eq
            | Intrinsic::Int64Eq
            | Intrinsic::Float32Eq
            | Intrinsic::Float64Eq => self.builder.emit_test_eq(dest, lhs_reg, rhs_reg),
            Intrinsic::Int32Add => self.builder.emit_add(dest, lhs_reg, rhs_reg, location),
            Intrinsic::Int32Sub => self.builder.emit_sub(dest, lhs_reg, rhs_reg, location),
            Intrinsic::Int32Mul => self.builder.emit_mul(dest, lhs_reg, rhs_reg, location),
            Intrinsic::Int32Div => self.builder.emit_div(dest, lhs_reg, rhs_reg, location),
            Intrinsic::Int32Mod => self.builder.emit_mod(dest, lhs_reg, rhs_reg, location),
            Intrinsic::Int32Or => self.builder.emit_or(dest, lhs_reg, rhs_reg),
            Intrinsic::Int32And => self.builder.emit_and(dest, lhs_reg, rhs_reg),
            Intrinsic::Int32Xor => self.builder.emit_xor(dest, lhs_reg, rhs_reg),
            Intrinsic::Int32Shl => self.builder.emit_shl(dest, lhs_reg, rhs_reg),
            Intrinsic::Int32Shr => self.builder.emit_shr(dest, lhs_reg, rhs_reg),
            Intrinsic::Int32Sar => self.builder.emit_sar(dest, lhs_reg, rhs_reg),

            Intrinsic::Int64Add => self.builder.emit_add(dest, lhs_reg, rhs_reg, location),
            Intrinsic::Int64Sub => self.builder.emit_sub(dest, lhs_reg, rhs_reg, location),
            Intrinsic::Int64Mul => self.builder.emit_mul(dest, lhs_reg, rhs_reg, location),
            Intrinsic::Int64Div => self.builder.emit_div(dest, lhs_reg, rhs_reg, location),
            Intrinsic::Int64Mod => self.builder.emit_mod(dest, lhs_reg, rhs_reg, location),
            Intrinsic::Int64Or => self.builder.emit_or(dest, lhs_reg, rhs_reg),
            Intrinsic::Int64And => self.builder.emit_and(dest, lhs_reg, rhs_reg),
            Intrinsic::Int64Xor => self.builder.emit_xor(dest, lhs_reg, rhs_reg),
            Intrinsic::Int64Shl => self.builder.emit_shl(dest, lhs_reg, rhs_reg),
            Intrinsic::Int64Shr => self.builder.emit_shr(dest, lhs_reg, rhs_reg),
            Intrinsic::Int64Sar => self.builder.emit_sar(dest, lhs_reg, rhs_reg),

            Intrinsic::Float32Add => self.builder.emit_add(dest, lhs_reg, rhs_reg, location),
            Intrinsic::Float32Sub => self.builder.emit_sub(dest, lhs_reg, rhs_reg, location),
            Intrinsic::Float32Mul => self.builder.emit_mul(dest, lhs_reg, rhs_reg, location),
            Intrinsic::Float32Div => self.builder.emit_div(dest, lhs_reg, rhs_reg, location),

            Intrinsic::Float64Add => self.builder.emit_add(dest, lhs_reg, rhs_reg, location),
            Intrinsic::Float64Sub => self.builder.emit_sub(dest, lhs_reg, rhs_reg, location),
            Intrinsic::Float64Mul => self.builder.emit_mul(dest, lhs_reg, rhs_reg, location),
            Intrinsic::Float64Div => self.builder.emit_div(dest, lhs_reg, rhs_reg, location),

            _ => unimplemented!(),
        }

        self.free_if_temp(lhs_reg);
        self.free_if_temp(rhs_reg);

        dest
    }

    fn visit_expr_assign(&mut self, expr: &ast::ExprBinType, dest: DataDest) -> Register {
        assert!(dest.is_unit());

        if expr.lhs.is_ident() {
            let ident_type = self.analysis.map_idents.get(expr.lhs.id()).unwrap();
            match ident_type {
                &IdentType::Var(var_id) => self.visit_expr_assign_var(expr, var_id),
                &IdentType::Context(distance, field_id) => {
                    self.visit_expr_assign_context(expr, distance, field_id)
                }
                &IdentType::Global(gid) => self.visit_expr_assign_global(expr, gid),
                _ => unreachable!(),
            }
        } else {
            match *expr.lhs {
                ast::ExprData::Dot(ref dot) => self.visit_expr_assign_dot(expr, dot),
                ast::ExprData::Call(ref call) => self.visit_expr_assign_call(expr, call),
                _ => unreachable!(),
            };
        }

        Register::invalid()
    }

    fn visit_expr_assign_call(&mut self, expr: &ast::ExprBinType, call_expr: &ast::ExprCallType) {
        let object = &call_expr.callee;
        let index = &call_expr.args[0];
        let value = &expr.rhs;

        if let Some(info) = self.get_intrinsic(expr.id) {
            match info.intrinsic {
                Intrinsic::ArraySet => {
                    self.emit_intrinsic_array_set(
                        object,
                        index,
                        value,
                        self.loc(expr.span),
                        DataDest::Effect,
                    );
                }
                _ => panic!("unexpected intrinsic {:?}", info.intrinsic),
            }
        } else {
            let call_type = self.analysis.map_calls.get(expr.id).unwrap();
            let fct_id = call_type.fct_id().unwrap();

            let obj_reg = gen_expr(self, object, DataDest::Alloc);
            let idx_reg = gen_expr(self, index, DataDest::Alloc);
            let val_reg = gen_expr(self, value, DataDest::Alloc);

            let obj_ty = self.ty(object.id());

            self.builder.emit_push_register(obj_reg);
            self.builder.emit_push_register(idx_reg);
            self.builder.emit_push_register(val_reg);

            let type_params = obj_ty.type_params();

            let callee_idx = self
                .builder
                .add_const_fct_types(FunctionId(fct_id.0 as u32), bty_array_from_ty(&type_params));
            let dest = self.ensure_unit_register();
            self.builder
                .emit_invoke_direct(dest, callee_idx, self.loc(expr.span));

            self.free_if_temp(obj_reg);
            self.free_if_temp(idx_reg);
            self.free_if_temp(val_reg);
        }
    }

    fn visit_expr_assign_dot(&mut self, expr: &ast::ExprBinType, dot: &ast::ExprDotType) {
        let (cls_ty, field_id) = {
            let ident_type = self.analysis.map_idents.get(dot.id).cloned().unwrap();
            match ident_type {
                IdentType::Field(class, field) => (class, field),
                _ => unreachable!(),
            }
        };

        let cls_id = cls_ty.cls_id().expect("class expected");
        let type_params = cls_ty.type_params();
        let field_idx = self.builder.add_const_field_types(
            ClassId(cls_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&type_params),
            field_id.0 as u32,
        );

        let obj = gen_expr(self, &dot.lhs, DataDest::Alloc);
        let src = gen_expr(self, &expr.rhs, DataDest::Alloc);

        self.builder
            .emit_store_field(src, obj, field_idx, self.loc(expr.span));

        self.free_if_temp(obj);
        self.free_if_temp(src);
    }

    fn visit_expr_assign_context(
        &mut self,
        expr: &ast::ExprBinType,
        distance: usize,
        context_idx: ContextIdx,
    ) {
        let value_reg = gen_expr(self, &expr.rhs, DataDest::Alloc);

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
            .emit_load_field(outer_context_reg, self_reg, idx, self.loc(expr.span));

        assert!(distance >= 1);

        let mut distance_left = distance;
        let mut outer_cls_idx = self.analysis.outer_context_infos.len() - 1;

        while distance_left > 1 {
            let outer_cls_id = self.analysis.outer_context_infos[outer_cls_idx]
                .read()
                .as_ref()
                .expect("uninitialized context class id")
                .context_cls_id;

            let idx = self.builder.add_const_field_types(
                ClassId(outer_cls_id.index().try_into().expect("overflow")),
                bty_array_from_ty(&self.identity_type_params()),
                0,
            );
            self.builder.emit_load_field(
                outer_context_reg,
                outer_context_reg,
                idx,
                self.loc(expr.span),
            );

            distance_left -= 1;
            outer_cls_idx -= 1;
        }

        assert_eq!(distance_left, 1);

        // Store value in context field
        let outer_context_info = self.analysis.outer_context_infos[outer_cls_idx]
            .read()
            .as_ref()
            .expect("uninitialized context class id")
            .clone();

        let field_id =
            field_id_from_context_idx(context_idx, outer_context_info.has_outer_context_slot);
        let idx = self.builder.add_const_field_types(
            ClassId(
                outer_context_info
                    .context_cls_id
                    .index()
                    .try_into()
                    .expect("overflow"),
            ),
            bty_array_from_ty(&self.identity_type_params()),
            field_id.0 as u32,
        );
        self.builder
            .emit_store_field(value_reg, outer_context_reg, idx, self.loc(expr.span));

        self.free_temp(outer_context_reg);
        self.free_if_temp(value_reg);
    }

    fn visit_expr_assign_var(&mut self, expr: &ast::ExprBinType, var_id: VarId) {
        let var = self.analysis.vars.get_var(var_id);

        match var.location {
            VarLocation::Context(context_idx) => {
                let value_reg = gen_expr(self, &expr.rhs, DataDest::Alloc);
                self.store_in_context(value_reg, context_idx, self.loc(expr.span));
                self.free_if_temp(value_reg);
            }

            VarLocation::Stack => {
                let dest = if var.ty.is_unit() {
                    DataDest::Effect
                } else {
                    let var_reg = self.var_reg(var_id);
                    DataDest::Reg(var_reg)
                };

                gen_expr(self, &expr.rhs, dest);
            }
        }
    }

    fn visit_expr_assign_global(&mut self, expr: &ast::ExprBinType, gid: GlobalDefinitionId) {
        let global_var = self.sa.globals.idx(gid);
        let global_var = global_var.read();

        let dest = if global_var.ty.is_unit() {
            DataDest::Effect
        } else {
            DataDest::Alloc
        };

        let src = gen_expr(self, &expr.rhs, dest);

        if !global_var.ty.is_unit() {
            self.builder.emit_store_global(src, GlobalId(gid.0));
        }

        self.free_if_temp(src);
    }

    fn visit_expr_ident(&mut self, ident: &ast::ExprIdentType, dest: DataDest) -> Register {
        let ident_type = self.analysis.map_idents.get(ident.id).unwrap();

        match ident_type {
            &IdentType::Var(var_id) => {
                self.visit_expr_ident_var(var_id, dest, self.loc(ident.span))
            }
            &IdentType::Context(distance, field_id) => {
                self.visit_expr_ident_context(distance, field_id, dest, self.loc(ident.span))
            }
            &IdentType::Global(gid) => {
                self.visit_expr_ident_global(gid, dest, self.loc(ident.span))
            }
            &IdentType::Const(cid) => self.visit_expr_ident_const(cid, dest),
            &IdentType::EnumValue(enum_id, ref type_params, variant_idx) => self.emit_new_enum(
                enum_id,
                type_params.clone(),
                variant_idx,
                self.loc(ident.span),
                dest,
            ),

            &IdentType::Field(_, _) => unreachable!(),
            &IdentType::Struct(_) => unreachable!(),
            &IdentType::StructField(_, _) => unreachable!(),

            &IdentType::Fct(_, _) => unreachable!(),
            &IdentType::Class(_, _) => unreachable!(),
        }
    }

    fn visit_expr_ident_context(
        &mut self,
        distance: usize,
        context_idx: ContextIdx,
        dest: DataDest,
        location: Location,
    ) -> Register {
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

        assert!(distance >= 1);

        let mut outer_cls_idx = self.analysis.outer_context_infos.len() - 1;
        let mut distance_left = distance;

        while distance_left > 1 {
            let outer_cls_id = self.analysis.outer_context_infos[outer_cls_idx]
                .read()
                .as_ref()
                .expect("uninitialized context class id")
                .context_cls_id;
            let idx = self.builder.add_const_field_types(
                ClassId(outer_cls_id.index().try_into().expect("overflow")),
                bty_array_from_ty(&self.identity_type_params()),
                0,
            );
            self.builder
                .emit_load_field(outer_context_reg, outer_context_reg, idx, location);

            distance_left -= 1;
            outer_cls_idx -= 1;
        }

        assert_eq!(distance_left, 1);

        let outer_context_info = self.analysis.outer_context_infos[outer_cls_idx]
            .read()
            .as_ref()
            .expect("uninitialized context class id")
            .clone();
        let outer_cls_id = outer_context_info.context_cls_id;

        let outer_cls = &self.sa.classes[outer_cls_id];
        let field_id =
            field_id_from_context_idx(context_idx, outer_context_info.has_outer_context_slot);
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
        if dest.is_effect() {
            return Register::invalid();
        }

        let const_ = &self.sa.consts[const_id];
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
                self.builder
                    .emit_const_uint8(dest, const_.value().to_int() as u8);
            }

            SourceType::Int32 => {
                self.builder
                    .emit_const_int32(dest, const_.value().to_int() as i32);
            }

            SourceType::Int64 => {
                self.builder.emit_const_int64(dest, const_.value().to_int());
            }

            SourceType::Float32 => {
                self.builder
                    .emit_const_float32(dest, const_.value().to_float() as f32);
            }

            SourceType::Float64 => {
                self.builder
                    .emit_const_float64(dest, const_.value().to_float());
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
        if dest.is_effect() {
            return Register::invalid();
        }

        let global_var = self.sa.globals.idx(gid);
        let global_var = global_var.read();

        if global_var.ty.is_unit() {
            assert!(dest.is_alloc());
            return Register::invalid();
        }

        let ty: BytecodeType = register_bty_from_ty(global_var.ty.clone());
        let dest = self.ensure_register(dest, ty);

        self.builder
            .emit_load_global(dest, GlobalId(gid.0), location);

        dest
    }

    fn visit_expr_ident_var(
        &mut self,
        var_id: VarId,
        dest: DataDest,
        location: Location,
    ) -> Register {
        let var = self.analysis.vars.get_var(var_id);

        if dest.is_effect() {
            return Register::invalid();
        }

        if var.ty.is_unit() {
            assert!(dest.is_alloc());
            return self.ensure_unit_register();
        }

        match var.location {
            VarLocation::Context(context_idx) => {
                let ty = register_bty_from_ty(var.ty.clone());
                let dest_reg = self.ensure_register(dest, ty);
                self.load_from_context(dest_reg, context_idx, location);
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

    fn store_in_context(&mut self, src: Register, context_idx: ContextIdx, location: Location) {
        let context_register = self.context_register.expect("context register missing");
        let cls_id = self.analysis.context_cls_id.expect("class missing");
        let field_id =
            field_id_from_context_idx(context_idx, self.analysis.context_has_outer_context_slot());
        let field_idx = self.builder.add_const_field_types(
            ClassId(cls_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&self.identity_type_params()),
            field_id.0 as u32,
        );
        self.builder
            .emit_store_field(src, context_register, field_idx, location);
    }

    fn load_from_context(&mut self, dest: Register, context_idx: ContextIdx, location: Location) {
        // Load context object.
        let context_register = self.context_register.expect("context register missing");
        let cls_id = self.analysis.context_cls_id.expect("class missing");
        let field_id =
            field_id_from_context_idx(context_idx, self.analysis.context_has_outer_context_slot());
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

    fn ensure_register(&mut self, dest: DataDest, ty: BytecodeType) -> Register {
        match dest {
            DataDest::Effect | DataDest::Alloc => {
                if ty.is_unit() {
                    self.ensure_unit_register()
                } else {
                    self.alloc_temp(ty)
                }
            }
            DataDest::Reg(reg) => reg,
        }
    }

    fn determine_call_type_params(&self, call_type: &CallType) -> SourceTypeArray {
        match call_type {
            CallType::Method(_, _, ref type_params) => type_params.clone(),

            CallType::Fct(_, ref type_params) => type_params.clone(),

            CallType::Expr(_, _, ref type_params) => type_params.clone(),

            CallType::TraitObjectMethod(_, _) => SourceTypeArray::empty(),
            CallType::GenericMethod(_, _, _) => SourceTypeArray::empty(),
            CallType::GenericStaticMethod(_, _, _) => SourceTypeArray::empty(),

            CallType::NewClass(..)
            | CallType::NewStruct(..)
            | CallType::NewEnum(..)
            | CallType::Intrinsic(..)
            | CallType::Lambda(..) => unreachable!(),
        }
    }

    fn add_const_pool_entry_for_call(
        &mut self,
        fct: &FctDefinition,
        call_type: &CallType,
    ) -> ConstPoolIdx {
        let type_params = self.determine_call_type_params(call_type);
        assert_eq!(fct.type_params.len(), type_params.len());

        match *call_type {
            CallType::GenericStaticMethod(id, _, _) | CallType::GenericMethod(id, _, _) => {
                self.builder.add_const_generic(
                    id.0 as u32,
                    FunctionId(fct.id().0 as u32),
                    bty_array_from_ty(&type_params),
                )
            }
            CallType::TraitObjectMethod(ref trait_object_ty, _) => {
                self.builder.add_const(ConstPoolEntry::TraitObjectMethod(
                    bty_from_ty(trait_object_ty.clone()),
                    FunctionId(fct.id().0 as u32),
                    bty_array_from_ty(&type_params),
                ))
            }

            CallType::Method(..) | CallType::Expr(..) | CallType::Fct(..) => {
                self.builder.add_const_fct_types(
                    FunctionId(fct.id().0 as u32),
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

            CallType::TraitObjectMethod(trait_ty, _) => {
                let container_type_params = trait_ty.type_params();
                specialize_type(self.sa, ty, &container_type_params)
            }
            CallType::GenericMethod(id, _, _) | CallType::GenericStaticMethod(id, _, _) => {
                debug_assert!(ty.is_concrete_type() || ty.is_self());
                if ty.is_self() {
                    SourceType::TypeParam(*id)
                } else {
                    ty
                }
            }

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

        let fct = self.sa.fcts.idx(fid);
        let fct = fct.read();

        if let Some(intrinsic) = fct.intrinsic {
            return Some(IntrinsicInfo::with_fct(intrinsic, fid));
        }

        None
    }

    fn identity_type_params(&self) -> SourceTypeArray {
        if self.type_params_len == 0 {
            return SourceTypeArray::empty();
        }

        let type_params = (0..self.type_params_len)
            .into_iter()
            .map(|idx| SourceType::TypeParam(TypeParamId(idx)))
            .collect::<Vec<SourceType>>();

        SourceTypeArray::with(type_params)
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
    // Do not store result. Only interested in side-effects of an expression.
    Effect,

    // Allocate a new register and store result in it.
    Alloc,

    // Store the result in the given register.
    Reg(Register),
}

impl DataDest {
    fn is_unit(&self) -> bool {
        match self {
            DataDest::Effect => true,
            DataDest::Reg(_) => false,
            DataDest::Alloc => true,
        }
    }

    fn is_effect(&self) -> bool {
        match self {
            DataDest::Effect => true,
            DataDest::Reg(_) => false,
            DataDest::Alloc => false,
        }
    }

    fn is_alloc(&self) -> bool {
        match self {
            DataDest::Effect => false,
            DataDest::Reg(_) => false,
            DataDest::Alloc => true,
        }
    }

    fn reg(&self) -> Register {
        match self {
            DataDest::Effect | DataDest::Alloc => panic!("not a register"),
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
        SourceType::Trait(trait_id, type_params) => {
            BytecodeType::Trait(TraitId(trait_id.0), bty_array_from_ty(&type_params))
        }
        SourceType::Enum(enum_id, type_params) => {
            BytecodeType::Enum(EnumId(enum_id.0), bty_array_from_ty(&type_params))
        }
        SourceType::Struct(struct_id, type_params) => BytecodeType::Struct(
            StructId(struct_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&type_params),
        ),
        SourceType::Tuple(subtypes) => BytecodeType::Tuple(bty_array_from_ty(&subtypes)),
        SourceType::TypeParam(idx) => BytecodeType::TypeParam(idx.to_usize() as u32),
        SourceType::Lambda(params, return_type) => BytecodeType::Lambda(
            bty_array_from_ty(&params),
            Box::new(bty_from_ty(*return_type)),
        ),
        SourceType::Ptr => BytecodeType::Ptr,
        SourceType::This => BytecodeType::This,
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
        SourceType::Trait(trait_id, type_params) => {
            BytecodeType::Trait(TraitId(trait_id.0), bty_array_from_ty(&type_params))
        }
        SourceType::Enum(enum_id, type_params) => {
            BytecodeType::Enum(EnumId(enum_id.0), bty_array_from_ty(&type_params))
        }
        SourceType::Struct(struct_id, type_params) => BytecodeType::Struct(
            StructId(struct_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&type_params),
        ),
        SourceType::Tuple(subtypes) => BytecodeType::Tuple(bty_array_from_ty(&subtypes)),
        SourceType::TypeParam(idx) => BytecodeType::TypeParam(idx.to_usize() as u32),
        SourceType::Lambda(_, _) => BytecodeType::Ptr,
        SourceType::Ptr => BytecodeType::Ptr,
        _ => panic!("SourceType {:?} cannot be converted to BytecodeType", ty),
    }
}

fn field_id_from_context_idx(context_idx: ContextIdx, has_outer_context_slot: bool) -> FieldId {
    let start_idx = if has_outer_context_slot { 1 } else { 0 };
    let ContextIdx(context_idx) = context_idx;
    FieldId(start_idx + context_idx)
}
