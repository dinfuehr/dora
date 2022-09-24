use dora_parser::lexer::position::Position;
use std::collections::HashMap;
use std::convert::TryInto;

use dora_parser::ast;

use crate::bytecode::{
    BytecodeBuilder, BytecodeFunction, BytecodeType, ConstPoolIdx, Label, Register,
};
use crate::language::sem_analysis::{
    find_impl, AnalysisData, CallType, ClassDefinitionId, ConstDefinitionId, ContextIdx,
    EnumDefinitionId, FctDefinition, FctDefinitionId, FieldId, GlobalDefinitionId, IdentType,
    Intrinsic, SemAnalysis, StructDefinitionId, TypeParamId, VarId,
};
use crate::language::specialize::specialize_type;
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::language::{expr_always_returns, expr_block_always_returns};

use super::sem_analysis::VarLocation;

pub struct LoopLabels {
    cond: Label,
    end: Label,
}

impl LoopLabels {
    fn new(cond: Label, end: Label) -> LoopLabels {
        LoopLabels { cond, end }
    }
}

pub fn generate_fct(sa: &SemAnalysis, id: FctDefinitionId) -> BytecodeFunction {
    let fct = sa.fcts.idx(id);
    let fct = fct.read();
    let analysis = fct.analysis();

    generate(sa, &fct, analysis)
}

pub fn generate(sa: &SemAnalysis, fct: &FctDefinition, src: &AnalysisData) -> BytecodeFunction {
    let ast_bytecode_generator = AstBytecodeGen {
        sa,
        fct,
        analysis: src,

        builder: BytecodeBuilder::new(),
        loops: Vec::new(),
        var_registers: HashMap::new(),
        unit_register: None,
        context_register: None,
    };
    ast_bytecode_generator.generate(&fct.ast)
}

const SELF_VAR_ID: VarId = VarId(0);

struct AstBytecodeGen<'a> {
    sa: &'a SemAnalysis,
    fct: &'a FctDefinition,
    analysis: &'a AnalysisData,

    builder: BytecodeBuilder,
    loops: Vec<LoopLabels>,
    var_registers: HashMap<VarId, Register>,
    context_register: Option<Register>,
    unit_register: Option<Register>,
}

impl<'a> AstBytecodeGen<'a> {
    fn generate(mut self, ast: &ast::Function) -> BytecodeFunction {
        self.push_scope();

        let mut params = Vec::new();

        if self.fct.has_self() {
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

        self.create_context();

        let next_register_idx = if self.fct.has_self() {
            let var_self = self.analysis.vars.get_self();
            let reg = Register(0);

            match var_self.location {
                VarLocation::Context(context_idx) => {
                    self.store_in_context(reg, context_idx, self.fct.pos);
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
                    self.store_in_context(reg, context_idx, self.fct.pos);
                }

                VarLocation::Stack => {
                    self.var_registers.insert(var_id, reg);
                }
            }
        }

        if let Some(ref block) = ast.block {
            for stmt in &block.stmts {
                self.visit_stmt(stmt);
            }

            if let Some(ref value) = block.expr {
                let reg = self.visit_expr(value, DataDest::Alloc);

                if !expr_block_always_returns(block) {
                    self.emit_ret_value(reg);
                }

                self.free_if_temp(reg);
            }
        } else {
            unreachable!();
        }

        let return_type = if self.fct.return_type.is_unit() {
            None
        } else {
            Some(bty_from_ty(self.fct.return_type.clone()))
        };
        self.builder.set_return_type(return_type);

        if self.fct.return_type.is_unit() {
            let dest = self.ensure_unit_register();
            self.builder.emit_ret(dest);
        }

        self.pop_scope();
        self.builder.generate(self.sa)
    }

    fn create_context(&mut self) {
        if let Some(cls_id) = self.analysis.context_cls_id {
            let context_register = self.builder.alloc_global(BytecodeType::Ptr);
            let idx = self
                .builder
                .add_const_cls_types(cls_id, self.identity_type_params());
            self.builder
                .emit_new_object(context_register, idx, self.fct.pos);
            self.context_register = Some(context_register);

            if self.analysis.context_has_outer_context_slot() {
                let self_reg = self.var_reg(SELF_VAR_ID);

                // Load context field of lambda object in self.
                let outer_context_reg = self.alloc_temp(BytecodeType::Ptr);
                let lambda_cls_id = self.sa.known.classes.lambda();
                let idx = self.builder.add_const_field_types(
                    lambda_cls_id,
                    SourceTypeArray::empty(),
                    FieldId(0),
                );
                self.builder
                    .emit_load_field(outer_context_reg, self_reg, idx, self.fct.pos);

                // Store value in outer_context field of context object.
                let idx = self.builder.add_const_field_types(
                    cls_id,
                    self.identity_type_params(),
                    FieldId(0),
                );
                self.builder.emit_store_field(
                    outer_context_reg,
                    context_register,
                    idx,
                    self.fct.pos,
                );

                self.free_temp(outer_context_reg);
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &ast::Stmt) {
        match *stmt {
            ast::Stmt::Return(ref ret) => self.visit_stmt_return(ret),
            ast::Stmt::Break(ref stmt) => self.visit_stmt_break(stmt),
            ast::Stmt::Continue(ref stmt) => self.visit_stmt_continue(stmt),
            ast::Stmt::Expr(ref expr) => self.visit_stmt_expr(expr),
            ast::Stmt::Let(ref stmt) => self.visit_stmt_let(stmt),
            ast::Stmt::While(ref stmt) => self.visit_stmt_while(stmt),
            ast::Stmt::For(ref stmt) => self.visit_stmt_for(stmt),
        }
    }

    fn visit_stmt_for(&mut self, stmt: &ast::StmtForType) {
        self.visit_stmt_for_iterator(stmt);
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

    fn visit_stmt_for_pattern_assign_array(
        &mut self,
        pattern: &ast::LetPattern,
        array_reg: Register,
        index_reg: Register,
        ty: SourceType,
    ) {
        match pattern {
            ast::LetPattern::Ident(ref ident) => {
                let var_id = *self.analysis.map_vars.get(ident.id).unwrap();
                let var_ty = self.var_ty(var_id);

                if !var_ty.is_unit() {
                    let var_reg = self.var_reg(var_id);
                    self.builder
                        .emit_load_array(var_reg, array_reg, index_reg, ident.pos);
                }
            }

            ast::LetPattern::Underscore(_) => {
                // nothing to do
            }

            ast::LetPattern::Tuple(ref tuple) => {
                if tuple.parts.len() > 0 {
                    let bytecode_ty: BytecodeType = register_bty_from_ty(ty.clone());
                    let tuple_reg = self.alloc_temp(bytecode_ty.clone());
                    self.builder
                        .emit_load_array(tuple_reg, array_reg, index_reg, tuple.pos);
                    self.destruct_tuple_pattern(tuple, tuple_reg, ty);
                    self.free_temp(tuple_reg);
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
                            self.store_in_context(next_reg, context_idx, ident.pos);
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
                        let idx = self.builder.add_const_tuple_element(tuple_ty.clone(), idx);

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
                        let idx = self.builder.add_const_tuple_element(tuple_ty.clone(), idx);
                        self.builder
                            .emit_load_tuple_element(temp_reg, tuple_reg, idx);
                        self.destruct_tuple_pattern(tuple, temp_reg, ty);
                        self.free_temp(temp_reg);
                    }
                }
            }
        }
    }

    fn visit_stmt_for_iterator(&mut self, stmt: &ast::StmtForType) {
        self.push_scope();
        let for_type_info = self.analysis.map_fors.get(stmt.id).unwrap().clone();

        // Emit: <obj> = <expr> (for <var> in <expr> { ... })
        let object_reg = self.visit_expr(&stmt.expr, DataDest::Alloc);

        let iterator_reg = if let Some(make_iterator) = for_type_info.make_iterator {
            let object_type = self.ty(stmt.expr.id());
            let object_type_params = object_type.type_params();

            // Emit: <iterator> = <obj>.makeIterator();
            let iterator_reg = self.alloc_var(BytecodeType::Ptr);
            self.builder.emit_push_register(object_reg);
            let fct_idx = self
                .builder
                .add_const_fct_types(make_iterator, object_type_params);
            self.builder
                .emit_invoke_direct(iterator_reg, fct_idx, stmt.expr.pos());
            iterator_reg
        } else {
            // Object is already the iterator - just use it
            object_reg
        };

        let lbl_cond = self.builder.define_label();
        self.builder.emit_loop_start();

        let iterator_type = for_type_info.iterator_type.clone();
        let iterator_type_params = iterator_type.type_params();

        self.builder.emit_push_register(iterator_reg);

        let lbl_end = self.builder.create_label();

        let value_ty = for_type_info.value_type.clone();
        let option_type_params = SourceTypeArray::single(value_ty.clone());

        // Emit: <next-temp> = <iterator>.next()
        let next_result_ty = register_bty_from_ty(for_type_info.next_type.clone());
        let next_result_reg = self.alloc_temp(next_result_ty);

        let fct_idx = self
            .builder
            .add_const_fct_types(for_type_info.next, iterator_type_params);

        self.builder.emit_push_register(iterator_reg);
        self.emit_invoke_direct(
            for_type_info.next_type.clone(),
            next_result_reg,
            fct_idx,
            stmt.expr.pos(),
        );

        // Emit: if <next-result>.isNone() then goto lbl_end
        let cond_reg = self.alloc_temp(BytecodeType::Bool);
        let fct_idx = self.builder.add_const_fct_types(
            self.sa.known.functions.option_is_none(),
            option_type_params.clone(),
        );
        self.builder.emit_push_register(next_result_reg);
        self.builder
            .emit_invoke_direct(cond_reg, fct_idx, stmt.expr.pos());
        self.builder.emit_jump_if_true(cond_reg, lbl_end);
        self.free_temp(cond_reg);

        // Emit: <value-reg> = <next-result>.unwrap()
        if value_ty.is_unit() {
            self.free_temp(next_result_reg);
        } else {
            let value_ty = register_bty_from_ty(value_ty);
            let value_reg = self.alloc_var(value_ty);
            let fct_idx = self
                .builder
                .add_const_fct_types(self.sa.known.functions.option_unwrap(), option_type_params);
            self.builder.emit_push_register(next_result_reg);
            self.builder
                .emit_invoke_direct(value_reg, fct_idx, stmt.expr.pos());
            self.free_temp(next_result_reg);

            self.visit_stmt_for_pattern_setup(&stmt.pattern);
            self.visit_stmt_for_pattern_assign_iterator(
                &stmt.pattern,
                value_reg,
                for_type_info.value_type,
            );
        }

        self.loops.push(LoopLabels::new(lbl_cond, lbl_end));
        self.visit_stmt(&stmt.block);
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
                    let value_reg = self.visit_expr(expr, DataDest::Alloc);
                    self.store_in_context(value_reg, context_idx, ident.pos);
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
                    self.visit_expr(expr, dest);
                }
            }
        }
    }

    fn visit_stmt_let_underscore(&mut self, stmt: &ast::StmtLetType) {
        if let Some(ref expr) = stmt.expr {
            self.visit_expr(expr, DataDest::Effect);
        }
    }

    fn visit_stmt_let_pattern(&mut self, stmt: &ast::StmtLetType, pattern: &ast::LetTupleType) {
        if let Some(ref expr) = stmt.expr {
            let ty = self.ty(expr.id());

            if ty.is_unit() {
                self.visit_expr(expr, DataDest::Effect);
            } else {
                let tuple_reg = self.visit_expr(expr, DataDest::Alloc);
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

    fn visit_stmt_while(&mut self, stmt: &ast::StmtWhileType) {
        let cond_lbl = self.builder.define_label();
        let end_lbl = self.builder.create_label();
        self.builder.emit_loop_start();
        let cond_reg = self.visit_expr(&stmt.cond, DataDest::Alloc);
        self.builder.emit_jump_if_false(cond_reg, end_lbl);
        self.free_if_temp(cond_reg);
        self.loops.push(LoopLabels::new(cond_lbl, end_lbl));
        self.visit_stmt(&stmt.block);
        self.loops.pop().unwrap();
        self.builder.emit_jump_loop(cond_lbl);
        self.builder.bind_label(end_lbl);
    }

    fn visit_stmt_expr(&mut self, stmt: &ast::StmtExprType) {
        let reg = self.visit_expr(&stmt.expr, DataDest::Effect);
        self.free_if_temp(reg);
    }

    fn visit_stmt_return(&mut self, ret: &ast::StmtReturnType) {
        if let Some(ref expr) = ret.expr {
            let result_reg = self.visit_expr(expr, DataDest::Alloc);
            self.emit_ret_value(result_reg);
            self.free_if_temp(result_reg);
        } else {
            let dest = self.ensure_unit_register();
            self.builder.emit_ret(dest);
        }
    }

    fn emit_ret_value(&mut self, result_reg: Register) {
        let ret_ty = self.fct.return_type.clone();

        if ret_ty.is_unit() {
            let dest = self.ensure_unit_register();
            self.builder.emit_ret(dest);
            return;
        }

        self.builder.emit_ret(result_reg);
    }

    fn visit_stmt_break(&mut self, _stmt: &ast::StmtBreakType) {
        let end = self.loops.last().unwrap().end;
        self.builder.emit_jump(end);
    }

    fn visit_stmt_continue(&mut self, _stmt: &ast::StmtContinueType) {
        let cond = self.loops.last().unwrap().cond;
        self.builder.emit_jump_loop(cond);
    }

    fn visit_expr(&mut self, expr: &ast::Expr, dest: DataDest) -> Register {
        match *expr {
            ast::Expr::Un(ref un) => self.visit_expr_un(un, dest),
            ast::Expr::Bin(ref bin) => self.visit_expr_bin(bin, dest),
            ast::Expr::Dot(ref field) => self.visit_expr_dot(field, dest),
            ast::Expr::Block(ref block) => self.visit_expr_block(block, dest),
            ast::Expr::If(ref expr) => self.visit_expr_if(expr, dest),
            ast::Expr::Template(ref template) => self.visit_expr_template(template, dest),
            ast::Expr::TypeParam(ref expr) => self.visit_expr_type_param(expr, dest),
            ast::Expr::Path(ref path) => self.visit_expr_path(path, dest),
            ast::Expr::LitChar(ref lit) => self.visit_expr_lit_char(lit, dest),
            ast::Expr::LitInt(ref lit) => self.visit_expr_lit_int(lit, dest, false),
            ast::Expr::LitFloat(ref lit) => self.visit_expr_lit_float(lit, dest),
            ast::Expr::LitStr(ref lit) => self.visit_expr_lit_string(lit, dest),
            ast::Expr::LitBool(ref lit) => self.visit_expr_lit_bool(lit, dest),
            ast::Expr::Ident(ref ident) => self.visit_expr_ident(ident, dest),
            ast::Expr::Call(ref call) => self.visit_expr_call(call, dest),
            ast::Expr::This(ref expr) => self.visit_expr_self(expr, dest),
            ast::Expr::Conv(ref conv) => self.visit_expr_conv(conv, dest),
            ast::Expr::Tuple(ref tuple) => self.visit_expr_tuple(tuple, dest),
            ast::Expr::Paren(ref paren) => self.visit_expr(&paren.expr, dest),
            ast::Expr::Match(ref expr) => self.visit_expr_match(expr, dest),
            ast::Expr::Lambda(ref node) => self.visit_expr_lambda(node, dest),
        }
    }

    fn emit_expr_for_effect(&mut self, expr: &ast::Expr) {
        let reg = self.visit_expr(expr, DataDest::Effect);
        self.free_if_temp(reg);
    }

    fn visit_expr_type_param(&mut self, expr: &ast::ExprTypeParamType, dest: DataDest) -> Register {
        let ident_type = self.analysis.map_idents.get(expr.id).cloned().unwrap();

        match ident_type {
            IdentType::EnumValue(enum_id, type_params, variant_idx) => {
                self.emit_new_enum(enum_id, type_params, variant_idx, expr.pos, dest)
            }

            _ => unreachable!(),
        }
    }

    fn visit_expr_template(&mut self, expr: &ast::ExprTemplateType, dest: DataDest) -> Register {
        let buffer_register = self.ensure_register(dest, BytecodeType::Ptr);

        // build StringBuffer::empty() call
        let fct_id = self.sa.known.functions.string_buffer_empty();
        let fct_idx = self.builder.add_const_fct(fct_id);
        self.builder
            .emit_invoke_static(buffer_register, fct_idx, expr.pos);

        let part_register = self.alloc_temp(BytecodeType::Ptr);

        for part in &expr.parts {
            if let Some(ref lit_str) = part.to_lit_str() {
                let value = lit_str.value.clone();
                self.builder.emit_const_string(part_register, value);
            } else {
                let ty = self.ty(part.id());

                if ty.cls_id() == Some(self.sa.known.classes.string()) {
                    self.visit_expr(part, DataDest::Reg(part_register));
                } else if ty.is_type_param() {
                    let type_list_id = match ty {
                        SourceType::TypeParam(id) => id,
                        _ => unreachable!(),
                    };

                    let expr_register = self.visit_expr(part, DataDest::Alloc);
                    self.builder.emit_push_register(expr_register);

                    // build toString() call
                    let name = self.sa.interner.intern("toString");
                    let trait_id = self.sa.known.traits.stringable();
                    let trait_ = self.sa.traits[trait_id].read();
                    let to_string_id = trait_
                        .find_method(self.sa, name, false)
                        .expect("Stringable::toString() not found");

                    let fct_idx = self.builder.add_const_generic(
                        type_list_id,
                        to_string_id,
                        SourceTypeArray::empty(),
                    );

                    self.builder
                        .emit_invoke_generic_direct(part_register, fct_idx, part.pos());

                    self.free_if_temp(expr_register);
                } else {
                    let expr_register = self.visit_expr(part, DataDest::Alloc);
                    self.builder.emit_push_register(expr_register);

                    // build toString() call
                    let name = self.sa.interner.intern("toString");
                    let stringable_impl_id = find_impl(
                        self.sa,
                        ty,
                        &self.fct.type_params,
                        SourceType::new_trait(self.sa.known.traits.stringable()),
                    )
                    .expect("impl of Stringable not found");
                    let impl_ = self.sa.impls[stringable_impl_id].read();
                    let to_string_id = impl_
                        .instance_names
                        .get(&name)
                        .cloned()
                        .expect("method toString() not found");

                    let fct_idx = self.builder.add_const_fct(to_string_id);
                    self.builder
                        .emit_invoke_direct(part_register, fct_idx, part.pos());

                    self.free_if_temp(expr_register);
                }
            }

            // build StringBuffer::append() call
            let fct_id = self.sa.known.functions.string_buffer_append();
            let fct_idx = self.builder.add_const_fct(fct_id);
            self.builder.emit_push_register(buffer_register);
            self.builder.emit_push_register(part_register);
            let dest_reg = self.ensure_unit_register();
            self.builder.emit_invoke_direct(dest_reg, fct_idx, expr.pos);
        }

        self.free_temp(part_register);

        // build StringBuffer::toString() call
        let fct_id = self.sa.known.functions.string_buffer_to_string();
        let fct_idx = self.builder.add_const_fct(fct_id);
        self.builder.emit_push_register(buffer_register);
        self.builder
            .emit_invoke_direct(buffer_register, fct_idx, expr.pos);

        buffer_register
    }

    fn visit_expr_path(&mut self, expr: &ast::ExprPathType, dest: DataDest) -> Register {
        let ident_type = self.analysis.map_idents.get(expr.id).cloned().unwrap();

        match ident_type {
            IdentType::EnumValue(enum_id, type_params, variant_idx) => {
                self.emit_new_enum(enum_id, type_params, variant_idx, expr.pos, dest)
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
        variant_idx: usize,
        pos: Position,
        dest: DataDest,
    ) -> Register {
        let bty = BytecodeType::Enum(enum_id, type_params.clone());
        let dest = self.ensure_register(dest, bty);
        let idx = self
            .builder
            .add_const_enum_variant(enum_id, type_params, variant_idx);
        self.builder.emit_new_enum(dest, idx, pos);
        dest
    }

    fn visit_expr_conv(&mut self, expr: &ast::ExprConvType, dest: DataDest) -> Register {
        let object_type = self.ty(expr.object.id());
        let check_type = self.ty(expr.data_type.id());

        let (trait_id, type_params) = match check_type {
            SourceType::Trait(trait_id, ref type_params) => (trait_id, type_params.clone()),
            _ => unreachable!(),
        };

        let object = self.visit_expr(&expr.object, DataDest::Alloc);
        let idx = self
            .builder
            .add_const_trait(trait_id, check_type.type_params(), object_type);
        let ty = BytecodeType::Trait(trait_id, type_params.clone());
        let dest = self.ensure_register(dest, ty);
        self.builder
            .emit_new_trait_object(dest, idx, object, expr.pos);
        self.free_if_temp(object);
        dest
    }

    fn visit_expr_match(&mut self, node: &ast::ExprMatchType, dest: DataDest) -> Register {
        let result_ty = self.ty(node.id);
        let enum_ty = self.ty(node.expr.id());
        let enum_id = enum_ty.enum_id().expect("enum expected");

        let dest = if result_ty.is_unit() {
            None
        } else {
            let result_bc_ty = register_bty_from_ty(result_ty);
            let dest = self.ensure_register(dest, result_bc_ty);
            Some(dest)
        };

        let end_lbl = self.builder.create_label();

        let expr_reg = self.visit_expr(&node.expr, DataDest::Alloc);

        let variant_reg = self.alloc_temp(BytecodeType::Int32);
        let idx = self.builder.add_const_enum(enum_id, enum_ty.type_params());
        self.builder
            .emit_load_enum_variant(variant_reg, expr_reg, idx, node.pos);

        let mut next_lbl = self.builder.create_label();

        for (idx, case) in node.cases.iter().enumerate() {
            debug_assert_eq!(case.patterns.len(), 1);
            let pattern = case.patterns.first().expect("pattern missing");
            match pattern.data {
                ast::MatchPatternData::Underscore => {
                    self.builder.bind_label(next_lbl);

                    if let Some(dest) = dest {
                        self.visit_expr(&case.value, DataDest::Reg(dest));
                    } else {
                        self.visit_expr(&case.value, DataDest::Effect);
                    }

                    self.builder.emit_jump(end_lbl);
                }

                ast::MatchPatternData::Ident(ref ident) => {
                    let variant_idx: i32 = {
                        let ident_type = self.analysis.map_idents.get(pattern.id).unwrap();

                        match ident_type {
                            IdentType::EnumValue(_, _, variant_idx) => {
                                (*variant_idx).try_into().unwrap()
                            }
                            _ => unreachable!(),
                        }
                    };

                    self.builder.bind_label(next_lbl);
                    next_lbl = self.builder.create_label();

                    if idx != node.cases.len() - 1 {
                        let tmp_reg = self.alloc_temp(BytecodeType::Int32);
                        let cmp_reg = self.alloc_temp(BytecodeType::Bool);
                        self.builder.emit_const_int32(tmp_reg, variant_idx);
                        self.builder.emit_test_eq(cmp_reg, variant_reg, tmp_reg);
                        self.builder.emit_jump_if_false(cmp_reg, next_lbl);
                        self.free_temp(tmp_reg);
                        self.free_temp(cmp_reg);
                    }

                    self.push_scope();

                    if let Some(ref params) = ident.params {
                        for (subtype_idx, param) in params.iter().enumerate() {
                            if let Some(_) = param.name {
                                let idx = self.builder.add_const_enum_element(
                                    enum_id,
                                    enum_ty.type_params(),
                                    variant_idx as usize,
                                    subtype_idx,
                                );

                                let var_id = *self.analysis.map_vars.get(param.id).unwrap();

                                let ty = self.var_ty(var_id);

                                if !ty.is_unit() {
                                    let ty: BytecodeType = register_bty_from_ty(ty);
                                    let var_reg = self.alloc_var(ty);

                                    self.var_registers.insert(var_id, var_reg);

                                    self.builder
                                        .emit_load_enum_element(var_reg, expr_reg, idx, param.pos);
                                }
                            }
                        }
                    }

                    if let Some(dest) = dest {
                        self.visit_expr(&case.value, DataDest::Reg(dest));
                    } else {
                        self.visit_expr(&case.value, DataDest::Effect);
                    }

                    self.pop_scope();

                    self.builder.emit_jump(end_lbl);
                }
            }
        }

        self.builder.bind_label(end_lbl);
        self.free_temp(variant_reg);
        self.free_if_temp(expr_reg);

        dest.unwrap_or(Register::invalid())
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

        let idx = self
            .builder
            .add_const_fct_types(lambda_fct_id, self.identity_type_params());
        self.builder.emit_new_lambda(dest, idx, node.pos);

        dest
    }

    fn visit_expr_if(&mut self, expr: &ast::ExprIfType, dest: DataDest) -> Register {
        let ty = self.ty(expr.id);

        if let Some(ref else_block) = expr.else_block {
            let dest = if ty.is_unit() {
                Register::invalid()
            } else {
                self.ensure_register(dest, register_bty_from_ty(ty))
            };

            let else_lbl = self.builder.create_label();
            let end_lbl = self.builder.create_label();

            let cond_reg = self.visit_expr(&expr.cond, DataDest::Alloc);
            self.builder.emit_jump_if_false(cond_reg, else_lbl);
            self.free_if_temp(cond_reg);

            self.visit_expr(&expr.then_block, DataDest::Reg(dest));

            if !expr_always_returns(&expr.then_block) {
                self.builder.emit_jump(end_lbl);
            }

            self.builder.bind_label(else_lbl);
            self.visit_expr(else_block, DataDest::Reg(dest));
            self.builder.bind_label(end_lbl);

            dest
        } else {
            // Without else-branch there can't be return value
            assert!(ty.is_unit());

            let end_lbl = self.builder.create_label();
            let cond_reg = self.visit_expr(&expr.cond, DataDest::Alloc);
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
            self.visit_expr(expr, dest)
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
        let field_idx = self
            .builder
            .add_const_field_types(cls_id, type_params.clone(), field_id);

        let field_ty = {
            let cls = self.sa.classes.idx(cls_id);
            let cls = cls.read();

            let field = &cls.fields[field_id.to_usize()];
            field.ty.clone()
        };

        let field_ty = specialize_type(self.sa, field_ty, &type_params);

        let field_bc_ty: BytecodeType = register_bty_from_ty(field_ty);
        let dest = self.ensure_register(dest, field_bc_ty);
        let obj = self.visit_expr(&expr.lhs, DataDest::Alloc);

        self.builder.emit_load_field(dest, obj, field_idx, expr.pos);
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
        let struct_obj = self.visit_expr(&expr.lhs, DataDest::Alloc);

        let ident_type = self.analysis.map_idents.get(expr.id).unwrap();

        let field_idx = match ident_type {
            IdentType::StructField(_, field_idx) => *field_idx,
            _ => unreachable!(),
        };

        let struct_ = self.sa.structs.idx(struct_id);
        let struct_ = struct_.read();
        let field = &struct_.fields[field_idx.to_usize()];
        let ty = specialize_type(self.sa, field.ty.clone(), &type_params);

        if ty.is_unit() {
            assert!(dest.is_unit());
            self.free_if_temp(struct_obj);
            return Register::invalid();
        }

        let ty: BytecodeType = register_bty_from_ty(ty);
        let dest = self.ensure_register(dest, ty);
        let const_idx = self
            .builder
            .add_const_struct_field(struct_id, type_params, field_idx);
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
        let tuple = self.visit_expr(&expr.lhs, DataDest::Alloc);
        let idx: usize = expr
            .rhs
            .to_lit_int()
            .unwrap()
            .value
            .try_into()
            .expect("too large");

        let subtypes = tuple_ty.tuple_subtypes();
        let ty = subtypes[idx].clone();

        if ty.is_unit() {
            assert!(dest.is_unit());
            self.free_if_temp(tuple);
            return Register::invalid();
        }

        let ty: BytecodeType = register_bty_from_ty(ty);
        let dest = self.ensure_register(dest, ty);
        let idx = self.builder.add_const_tuple_element(tuple_ty, idx);
        self.builder.emit_load_tuple_element(dest, tuple, idx);

        self.free_if_temp(tuple);

        dest
    }

    fn visit_expr_assert(&mut self, expr: &ast::ExprCallType, dest: DataDest) {
        assert!(dest.is_unit());
        let assert_reg = self.visit_expr(&*expr.args[0].expr, DataDest::Alloc);
        self.builder.emit_push_register(assert_reg);
        let fid = self.sa.known.functions.assert();
        let idx = self.builder.add_const_fct(fid);
        let dest = self.ensure_unit_register();
        self.builder.emit_invoke_static(dest, idx, expr.pos);
        self.free_if_temp(assert_reg);
    }

    fn visit_expr_call(&mut self, expr: &ast::ExprCallType, dest: DataDest) -> Register {
        if let Some(info) = self.get_intrinsic(expr.id) {
            if !info.intrinsic.emit_as_function_in_bytecode() {
                return self.visit_expr_call_intrinsic(expr, info, dest);
            }
        }

        let call_type = self.analysis.map_calls.get(expr.id).unwrap().clone();

        match *call_type {
            CallType::Enum(ref enum_ty, variant_idx) => {
                return self.visit_expr_call_enum(expr, enum_ty.clone(), variant_idx, dest);
            }

            CallType::Struct(struct_id, ref type_params) => {
                return self.visit_expr_call_struct(expr, struct_id, type_params, dest);
            }

            CallType::Class2Ctor(cls_id, ref type_params) => {
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

            _ => {}
        }

        // Find method that is called
        let callee_id = self.determine_callee(&call_type);

        let callee = self.sa.fcts.idx(callee_id);
        let callee = callee.read();

        // Create FctDefId for this Fct
        let callee_idx = self.specialize_call(&callee, &call_type);

        // Determine types for arguments and return values
        let (arg_types, _, return_type) = self.determine_callee_types(&call_type, &*callee);

        // Allocate register for result
        let return_reg = if return_type.is_unit() {
            Register::invalid()
        } else {
            self.ensure_register(dest, register_bty_from_ty(return_type.clone()))
        };

        // Evaluate object/self argument
        let object_argument = self.emit_call_object_argument(expr, &call_type);

        // Evaluate function arguments
        let arguments = self.emit_call_arguments(expr, &*callee, &call_type, &arg_types);

        // Allocate object for constructor calls
        self.emit_call_allocate(expr.pos, &call_type, &arg_types, object_argument);

        if let Some(obj_reg) = object_argument {
            self.builder.emit_push_register(obj_reg);
        }
        for &arg_reg in &arguments {
            self.builder.emit_push_register(arg_reg);
        }

        // Emit the actual Invoke(Direct|Static|Virtual)XXX instruction
        self.emit_call_inst(&call_type, return_type, expr.pos, callee_idx, return_reg);

        // Store result
        let result_reg = self.emit_call_result(&call_type, dest, return_reg, object_argument);

        if let Some(obj_reg) = object_argument {
            if obj_reg != result_reg {
                self.free_if_temp(obj_reg);
            }
        }

        for arg_reg in arguments {
            self.free_if_temp(arg_reg);
        }

        result_reg
    }

    fn visit_expr_call_enum(
        &mut self,
        expr: &ast::ExprCallType,
        enum_ty: SourceType,
        variant_idx: usize,
        dest: DataDest,
    ) -> Register {
        let mut arguments = Vec::new();

        for arg in &expr.args {
            arguments.push(self.visit_expr(&arg.expr, DataDest::Alloc));
        }

        for &arg_reg in &arguments {
            self.builder.emit_push_register(arg_reg);
        }

        let enum_id = enum_ty.enum_id().expect("enum expected");
        let type_params = enum_ty.type_params();

        let idx = self
            .builder
            .add_const_enum_variant(enum_id, type_params, variant_idx);
        let bytecode_ty = register_bty_from_ty(enum_ty);
        let dest_reg = self.ensure_register(dest, bytecode_ty);
        self.builder.emit_new_enum(dest_reg, idx, expr.pos);

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

        let lambda_object = self.visit_expr(node.object_or_callee(), DataDest::Alloc);
        arguments.push(lambda_object);

        for arg in &node.args {
            arguments.push(self.visit_expr(&arg.expr, DataDest::Alloc));
        }

        for &arg_reg in &arguments {
            self.builder.emit_push_register(arg_reg);
        }

        let idx = self.builder.add_const_lambda(params, return_type.clone());

        let dest_reg = if return_type.is_unit() {
            let dest = self.ensure_unit_register();
            self.builder.emit_invoke_lambda(dest, idx, node.pos);
            dest
        } else {
            let bytecode_ty = register_bty_from_ty(return_type);
            let dest_reg = self.ensure_register(dest, bytecode_ty);
            self.builder.emit_invoke_lambda(dest_reg, idx, node.pos);
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
            arguments.push(self.visit_expr(&arg.expr, DataDest::Alloc));
        }

        for &arg_reg in &arguments {
            self.builder.emit_push_register(arg_reg);
        }

        let idx = self
            .builder
            .add_const_struct(struct_id, type_params.clone());
        let bytecode_ty = BytecodeType::Struct(struct_id, type_params.clone());
        let dest_reg = self.ensure_register(dest, bytecode_ty);
        self.builder.emit_new_struct(dest_reg, idx, expr.pos);

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
            arguments.push(self.visit_expr(&arg.expr, DataDest::Alloc));
        }

        for &arg_reg in &arguments {
            self.builder.emit_push_register(arg_reg);
        }

        let idx = self
            .builder
            .add_const_cls_types(cls_id, type_params.clone());
        let dest_reg = self.ensure_register(dest, BytecodeType::Ptr);
        self.builder
            .emit_new_object_initialized(dest_reg, idx, expr.pos);

        for arg_reg in arguments {
            self.free_if_temp(arg_reg);
        }

        dest_reg
    }

    fn determine_callee(&mut self, call_type: &CallType) -> FctDefinitionId {
        call_type.fct_id().expect("FctId missing")
    }

    fn determine_callee_types(
        &mut self,
        call_type: &CallType,
        fct: &FctDefinition,
    ) -> (Vec<SourceType>, Vec<BytecodeType>, SourceType) {
        let return_type = self.specialize_type_for_call(&call_type, fct.return_type.clone());

        let mut arg_types = Vec::with_capacity(fct.params_with_self().len());

        if fct.has_self() {
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

        let arg_bytecode_types = arg_types
            .iter()
            .filter(|ty| !ty.is_unit())
            .map(|ty| bty_from_ty(ty.clone()))
            .collect::<Vec<BytecodeType>>();

        (arg_types, arg_bytecode_types, return_type)
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
                let reg = self.visit_expr(obj_expr, DataDest::Alloc);

                Some(reg)
            }
            CallType::Expr(_, _, _) => Some(self.visit_expr(&expr.callee, DataDest::Alloc)),
            CallType::Ctor(_, _) => {
                // Need to use new register for allocated object.
                // Otherwise code like `x = SomeClass(x)` would break.
                Some(self.alloc_temp(BytecodeType::Ptr))
            }
            _ => None,
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
            CallType::Ctor(_, _)
            | CallType::Expr(_, _, _)
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
            let reg = self.visit_expr(&arg.expr, DataDest::Alloc);
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
        let cls_idx = self.builder.add_const_cls_types(cls_id, type_params);

        // Store length in a register
        let length_reg = self.alloc_temp(BytecodeType::Int64);
        self.builder
            .emit_const_int64(length_reg, variadic_arguments as i64);

        // Allocate array of given length
        let array_reg = self.ensure_register(dest, BytecodeType::Ptr);
        self.builder
            .emit_new_array(array_reg, cls_idx, length_reg, expr.pos);

        if element_ty.is_unit() {
            // Evaluate rest arguments
            for arg in expr.args.iter().skip(non_variadic_arguments) {
                self.visit_expr(&arg.expr, DataDest::Effect);
            }
        } else {
            let index_reg = self.alloc_temp(BytecodeType::Int64);

            // Evaluate rest arguments and store them in array
            for (idx, arg) in expr.args.iter().skip(non_variadic_arguments).enumerate() {
                let arg_reg = self.visit_expr(&arg.expr, DataDest::Alloc);
                self.builder.emit_const_int64(index_reg, idx as i64);
                self.builder
                    .emit_store_array(arg_reg, array_reg, index_reg, expr.pos);
                self.free_if_temp(arg_reg);
            }

            self.free_if_temp(index_reg);
        }

        self.free_if_temp(length_reg);

        array_reg
    }

    fn emit_call_allocate(
        &mut self,
        pos: Position,
        call_type: &CallType,
        arg_types: &[SourceType],
        object_reg: Option<Register>,
    ) {
        match *call_type {
            CallType::Ctor(_, _) => {
                let ty = arg_types.first().cloned().unwrap();

                let cls_id = ty.cls_id().expect("should be class");
                let type_params = ty.type_params();

                let idx = self.builder.add_const_cls_types(cls_id, type_params);
                self.builder
                    .emit_new_object(object_reg.expect("reg missing"), idx, pos);
            }
            _ => {}
        }
    }

    fn emit_call_inst(
        &mut self,
        call_type: &CallType,
        return_type: SourceType,
        pos: Position,
        callee_idx: ConstPoolIdx,
        return_reg: Register,
    ) {
        match *call_type {
            CallType::CtorParent(_, _) | CallType::Ctor(_, _) => {
                let dest_reg = self.ensure_unit_register();
                self.builder.emit_invoke_direct(dest_reg, callee_idx, pos);
            }

            CallType::Method(_, _, _) => {
                self.emit_invoke_direct(return_type, return_reg, callee_idx, pos);
            }
            CallType::ModuleMethod(_, _, _) | CallType::Fct(_, _) => {
                self.emit_invoke_static(return_type, return_reg, callee_idx, pos);
            }
            CallType::Expr(_, _, _) => {
                self.emit_invoke_direct(return_type, return_reg, callee_idx, pos);
            }
            CallType::TraitObjectMethod(_, _) => {
                self.emit_invoke_virtual(return_type, return_reg, callee_idx, pos);
            }
            CallType::GenericMethod(_, _, _) => {
                self.emit_invoke_generic_direct(return_type, return_reg, callee_idx, pos);
            }
            CallType::GenericStaticMethod(_, _, _) => {
                self.emit_invoke_generic_static(return_type, return_reg, callee_idx, pos);
            }
            CallType::Enum(_, _) => unreachable!(),
            CallType::Intrinsic(_) => unreachable!(),
            CallType::Struct(_, _) => unreachable!(),
            CallType::Lambda(_, _) => unreachable!(),
            CallType::Class2Ctor(_, _) => unreachable!(),
        }
    }

    fn emit_call_result(
        &mut self,
        call_type: &CallType,
        dest: DataDest,
        return_reg: Register,
        obj_reg: Option<Register>,
    ) -> Register {
        if call_type.is_ctor_new() {
            let obj_reg = obj_reg.unwrap();
            match dest {
                DataDest::Effect => {
                    self.free_if_temp(obj_reg);
                    Register::invalid()
                }
                DataDest::Alloc => obj_reg,
                DataDest::Reg(dest_reg) => {
                    self.builder.emit_mov(dest_reg, obj_reg);
                    self.free_if_temp(obj_reg);
                    dest_reg
                }
            }
        } else {
            return_reg
        }
    }

    fn emit_mov(&mut self, dest: Register, src: Register) {
        if dest != src {
            self.builder.emit_mov(dest, src);
        }
    }

    fn emit_invoke_virtual(
        &mut self,
        return_type: SourceType,
        return_reg: Register,
        callee_id: ConstPoolIdx,
        pos: Position,
    ) {
        if return_type.is_unit() {
            let reg = self.ensure_unit_register();
            self.builder.emit_invoke_virtual(reg, callee_id, pos);
        } else {
            self.builder.emit_invoke_virtual(return_reg, callee_id, pos);
        }
    }

    fn emit_invoke_direct(
        &mut self,
        return_type: SourceType,
        return_reg: Register,
        callee_id: ConstPoolIdx,
        pos: Position,
    ) {
        if return_type.is_unit() {
            let reg = self.ensure_unit_register();
            self.builder.emit_invoke_direct(reg, callee_id, pos);
        } else {
            self.builder.emit_invoke_direct(return_reg, callee_id, pos);
        }
    }

    fn emit_invoke_static(
        &mut self,
        return_type: SourceType,
        return_reg: Register,
        callee_id: ConstPoolIdx,
        pos: Position,
    ) {
        if return_type.is_unit() {
            let reg = self.ensure_unit_register();
            self.builder.emit_invoke_static(reg, callee_id, pos);
        } else {
            self.builder.emit_invoke_static(return_reg, callee_id, pos);
        }
    }

    fn emit_invoke_generic_static(
        &mut self,
        return_type: SourceType,
        return_reg: Register,
        callee_id: ConstPoolIdx,
        pos: Position,
    ) {
        if return_type.is_unit() {
            let dest = self.ensure_unit_register();
            self.builder
                .emit_invoke_generic_static(dest, callee_id, pos);
        } else {
            self.builder
                .emit_invoke_generic_static(return_reg, callee_id, pos);
        }
    }

    fn emit_invoke_generic_direct(
        &mut self,
        return_type: SourceType,
        return_reg: Register,
        callee_id: ConstPoolIdx,
        pos: Position,
    ) {
        if return_type.is_unit() {
            let dest = self.ensure_unit_register();
            self.builder
                .emit_invoke_generic_direct(dest, callee_id, pos);
        } else {
            self.builder
                .emit_invoke_generic_direct(return_reg, callee_id, pos);
        }
    }

    fn visit_expr_self(&mut self, expr: &ast::ExprSelfType, dest: DataDest) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

        if self.fct.is_lambda() {
            let ident = self
                .analysis
                .map_idents
                .get(expr.id)
                .expect("missing ident");
            let (distance, context_idx) = match ident {
                IdentType::Context(distance, context_idx) => (*distance, *context_idx),
                _ => unreachable!(),
            };
            self.visit_expr_ident_context(distance, context_idx, dest, expr.pos)
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

        self.builder.emit_const_char(dest, lit.value);

        dest
    }

    fn visit_expr_lit_int(
        &mut self,
        lit: &ast::ExprLitIntType,
        dest: DataDest,
        neg: bool,
    ) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

        let ty = self.analysis.ty(lit.id);

        let ty = match ty {
            SourceType::UInt8 => BytecodeType::UInt8,
            SourceType::Int32 => BytecodeType::Int32,
            SourceType::Int64 => BytecodeType::Int64,
            SourceType::Float32 => {
                let dest = self.ensure_register(dest, BytecodeType::Float32);
                let value = lit.value as f32;
                let value = if neg { -value } else { value };
                self.builder.emit_const_float32(dest, value);
                return dest;
            }
            SourceType::Float64 => {
                let dest = self.ensure_register(dest, BytecodeType::Float64);
                let value = lit.value as f64;
                let value = if neg { -value } else { value };
                self.builder.emit_const_float64(dest, value);
                return dest;
            }
            _ => unreachable!(),
        };

        let dest = self.ensure_register(dest, ty.clone());

        let value = if neg {
            (lit.value as i64).wrapping_neg()
        } else {
            lit.value as i64
        };

        match ty {
            BytecodeType::UInt8 => self.builder.emit_const_uint8(dest, value as u8),
            BytecodeType::Int32 => self.builder.emit_const_int32(dest, value as i32),
            BytecodeType::Int64 => self.builder.emit_const_int64(dest, value),
            _ => unreachable!(),
        }

        dest
    }

    fn visit_expr_lit_float(&mut self, lit: &ast::ExprLitFloatType, dest: DataDest) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

        let ty = self.analysis.ty(lit.id);

        let ty = match ty {
            SourceType::Float32 => BytecodeType::Float32,
            SourceType::Float64 => BytecodeType::Float64,
            _ => unreachable!(),
        };

        let dest = self.ensure_register(dest, ty.clone());

        match ty {
            BytecodeType::Float32 => self.builder.emit_const_float32(dest, lit.value as f32),
            BytecodeType::Float64 => self.builder.emit_const_float64(dest, lit.value),
            _ => unreachable!(),
        }

        dest
    }

    fn visit_expr_lit_string(&mut self, lit: &ast::ExprLitStrType, dest: DataDest) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

        let dest = self.ensure_register(dest, BytecodeType::Ptr);
        self.builder.emit_const_string(dest, lit.value.clone());

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
            let reg = self.visit_expr(value, DataDest::Alloc);

            if !value_ty.is_unit() {
                values.push(reg);
            }
        }

        for &value in &values {
            self.builder.emit_push_register(value);
        }

        let subtypes = ty.tuple_subtypes();
        let idx = self.builder.add_const_tuple(subtypes);
        self.builder.emit_new_tuple(result, idx, e.pos);

        for arg_reg in values {
            self.free_if_temp(arg_reg);
        }

        result
    }

    fn visit_expr_un(&mut self, expr: &ast::ExprUnType, dest: DataDest) -> Register {
        if expr.op == ast::UnOp::Neg && expr.opnd.is_lit_int() {
            self.visit_expr_lit_int(expr.opnd.to_lit_int().unwrap(), dest, true)
        } else if let Some(intrinsic) = self.get_intrinsic(expr.id) {
            self.emit_intrinsic_un(&expr.opnd, intrinsic, expr.pos, dest)
        } else {
            self.visit_expr_un_method(expr, dest)
        }
    }

    fn visit_expr_un_method(&mut self, expr: &ast::ExprUnType, dest: DataDest) -> Register {
        let opnd = self.visit_expr(&expr.opnd, DataDest::Alloc);

        let call_type = self.analysis.map_calls.get(expr.id).unwrap();
        let callee_id = self.determine_callee(call_type);

        let callee = self.sa.fcts.idx(callee_id);
        let callee = callee.read();

        // Create FctDefId for this callee
        let callee_idx = self.specialize_call(&callee, &call_type);

        let function_return_type: SourceType =
            self.specialize_type_for_call(call_type, callee.return_type.clone());

        let function_return_type_bc: BytecodeType =
            register_bty_from_ty(function_return_type.clone());
        let dest = self.ensure_register(dest, function_return_type_bc);

        self.builder.emit_push_register(opnd);
        self.emit_invoke_direct(function_return_type, dest, callee_idx, expr.pos);

        self.free_if_temp(opnd);

        dest
    }

    fn visit_expr_bin(&mut self, expr: &ast::ExprBinType, dest: DataDest) -> Register {
        if expr.op.is_any_assign() {
            self.visit_expr_assign(expr, dest)
        } else if expr.op == ast::BinOp::Cmp(ast::CmpOp::Is)
            || expr.op == ast::BinOp::Cmp(ast::CmpOp::IsNot)
        {
            self.emit_bin_is(expr, dest)
        } else if expr.op == ast::BinOp::Or {
            self.emit_bin_or(expr, dest)
        } else if expr.op == ast::BinOp::And {
            self.emit_bin_and(expr, dest)
        } else if let Some(info) = self.get_intrinsic(expr.id) {
            self.emit_intrinsic_bin(&expr.lhs, &expr.rhs, info, Some(expr.op), expr.pos, dest)
        } else {
            self.visit_expr_bin_method(expr, dest)
        }
    }

    fn visit_expr_bin_method(&mut self, expr: &ast::ExprBinType, dest: DataDest) -> Register {
        let lhs = self.visit_expr(&expr.lhs, DataDest::Alloc);
        let rhs = self.visit_expr(&expr.rhs, DataDest::Alloc);

        let call_type = self.analysis.map_calls.get(expr.id).unwrap();
        let callee_id = self.determine_callee(call_type);

        let callee = self.sa.fcts.idx(callee_id);
        let callee = callee.read();

        // Create FctDefId for this callee
        let callee_idx = self.specialize_call(&callee, &call_type);

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

        self.emit_invoke_direct(function_return_type, result, callee_idx, expr.pos);

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
                0 => self.emit_intrinsic_un(object, info, expr.pos, dest),
                1 => {
                    self.emit_intrinsic_bin(object, &expr.args[0].expr, info, None, expr.pos, dest)
                }
                2 => {
                    assert_eq!(intrinsic, Intrinsic::ArraySet);
                    self.emit_intrinsic_array_set(
                        expr.object().unwrap(),
                        &expr.args[0].expr,
                        &expr.args[1].expr,
                        expr.pos,
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
                    &expr.args[0].expr,
                    info,
                    None,
                    expr.pos,
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
        let cls_idx = self.builder.add_const_cls_types(cls_id, type_params);

        let array_reg = self.ensure_register(dest, BytecodeType::Ptr);
        let length_reg = self.visit_expr(&expr.args[0].expr, DataDest::Alloc);

        self.builder
            .emit_new_array(array_reg, cls_idx, length_reg, expr.pos);

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

        let lhs_reg = self.visit_expr(&expr.lhs, DataDest::Alloc);
        let rhs_reg = self.visit_expr(&expr.rhs, DataDest::Alloc);

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
            let dest = self.visit_expr(&expr.lhs, DataDest::Alloc);
            self.builder.emit_jump_if_true(dest, end_lbl);
            self.free_if_temp(dest);

            self.emit_expr_for_effect(&expr.rhs);
            self.builder.bind_label(end_lbl);

            Register::invalid()
        } else {
            let end_lbl = self.builder.create_label();
            let dest = self.ensure_register(dest, BytecodeType::Bool);

            self.visit_expr(&expr.lhs, DataDest::Reg(dest));
            self.builder.emit_jump_if_true(dest, end_lbl);
            self.visit_expr(&expr.rhs, DataDest::Reg(dest));
            self.builder.bind_label(end_lbl);

            dest
        }
    }

    fn emit_bin_and(&mut self, expr: &ast::ExprBinType, dest: DataDest) -> Register {
        if dest.is_effect() {
            let end_lbl = self.builder.create_label();
            let dest = self.visit_expr(&expr.lhs, DataDest::Alloc);
            self.builder.emit_jump_if_false(dest, end_lbl);
            self.free_if_temp(dest);

            self.emit_expr_for_effect(&expr.rhs);
            self.builder.bind_label(end_lbl);

            Register::invalid()
        } else {
            let end_lbl = self.builder.create_label();
            let dest = self.ensure_register(dest, BytecodeType::Bool);

            self.visit_expr(&expr.lhs, DataDest::Reg(dest));
            self.builder.emit_jump_if_false(dest, end_lbl);
            self.visit_expr(&expr.rhs, DataDest::Reg(dest));
            self.builder.bind_label(end_lbl);

            dest
        }
    }

    fn emit_intrinsic_array_set(
        &mut self,
        arr: &ast::Expr,
        idx: &ast::Expr,
        src: &ast::Expr,
        pos: Position,
        dest: DataDest,
    ) -> Register {
        assert!(dest.is_unit());

        let arr = self.visit_expr(arr, DataDest::Alloc);
        let idx = self.visit_expr(idx, DataDest::Alloc);
        let src = self.visit_expr(src, DataDest::Alloc);

        self.builder.emit_store_array(src, arr, idx, pos);

        self.free_if_temp(arr);
        self.free_if_temp(idx);
        self.free_if_temp(src);

        Register::invalid()
    }

    fn emit_intrinsic_un(
        &mut self,
        opnd: &ast::Expr,
        info: IntrinsicInfo,
        pos: Position,
        dest: DataDest,
    ) -> Register {
        let intrinsic = info.intrinsic;

        match intrinsic {
            Intrinsic::Int32Plus
            | Intrinsic::Int64Plus
            | Intrinsic::Float32Plus
            | Intrinsic::Float64Plus => {
                return self.visit_expr(opnd, dest);
            }
            _ => {}
        }

        let ty = intrinsic.result_type();
        let dest = self.ensure_register(dest, ty);

        let src = self.visit_expr(opnd, DataDest::Alloc);

        match intrinsic {
            Intrinsic::ArrayLen => {
                self.builder.emit_array_length(dest, src, pos);
            }
            Intrinsic::StrLen => {
                self.builder.emit_string_length(dest, src, pos);
            }
            Intrinsic::Int32Neg
            | Intrinsic::Int64Neg
            | Intrinsic::Float32Neg
            | Intrinsic::Float64Neg => self.builder.emit_neg(dest, src),
            Intrinsic::BoolNot | Intrinsic::Int32Not | Intrinsic::Int64Not => {
                self.builder.emit_not(dest, src)
            }
            Intrinsic::Int32ToInt32 => self.builder.emit_mov(dest, src),
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
        lhs: &ast::Expr,
        rhs: &ast::Expr,
        info: IntrinsicInfo,
        op: Option<ast::BinOp>,
        pos: Position,
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

                let arr = self.visit_expr(lhs, DataDest::Alloc);
                let idx = self.visit_expr(rhs, DataDest::Alloc);

                if intrinsic == Intrinsic::ArrayGet {
                    self.builder.emit_load_array(dest, arr, idx, pos);
                } else {
                    self.builder.emit_load_string_uint8(dest, arr, idx, pos);
                }

                self.free_if_temp(arr);
                self.free_if_temp(idx);

                return dest;
            }

            _ => {}
        }

        if dest.is_effect() {
            self.emit_expr_for_effect(lhs);
            self.emit_expr_for_effect(rhs);
            return Register::invalid();
        }

        let result_type = if op.is_some() {
            match intrinsic {
                Intrinsic::ByteCmp
                | Intrinsic::CharCmp
                | Intrinsic::Int32Cmp
                | Intrinsic::Int64Cmp
                | Intrinsic::Float32Cmp
                | Intrinsic::Float64Cmp => BytecodeType::Bool,
                _ => intrinsic.result_type(),
            }
        } else {
            intrinsic.result_type()
        };

        let dest = self.ensure_register(dest, result_type);

        let lhs_reg = self.visit_expr(lhs, DataDest::Alloc);
        let rhs_reg = self.visit_expr(rhs, DataDest::Alloc);

        match intrinsic {
            Intrinsic::BoolEq => match op {
                Some(ast::BinOp::Cmp(ast::CmpOp::Eq)) => {
                    self.builder.emit_test_eq(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Ne)) => {
                    self.builder.emit_test_ne(dest, lhs_reg, rhs_reg)
                }
                _ => unreachable!(),
            },
            Intrinsic::ByteEq => match op {
                Some(ast::BinOp::Cmp(ast::CmpOp::Eq)) => {
                    self.builder.emit_test_eq(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Ne)) => {
                    self.builder.emit_test_ne(dest, lhs_reg, rhs_reg)
                }
                _ => unreachable!(),
            },
            Intrinsic::ByteCmp => match op {
                Some(ast::BinOp::Cmp(ast::CmpOp::Lt)) => {
                    self.builder.emit_test_lt(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Le)) => {
                    self.builder.emit_test_le(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Gt)) => {
                    self.builder.emit_test_gt(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Ge)) => {
                    self.builder.emit_test_ge(dest, lhs_reg, rhs_reg)
                }
                None => {
                    let fct_id = info.fct_id.expect("fct_id missing");
                    let ty = self.ty(lhs.id());
                    let type_params = ty.type_params();
                    let idx = self.builder.add_const_fct_types(fct_id, type_params);
                    self.builder.emit_push_register(lhs_reg);
                    self.builder.emit_push_register(rhs_reg);
                    self.builder.emit_invoke_direct(dest, idx, pos);
                }
                _ => unreachable!(),
            },
            Intrinsic::CharEq => match op {
                Some(ast::BinOp::Cmp(ast::CmpOp::Eq)) => {
                    self.builder.emit_test_eq(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Ne)) => {
                    self.builder.emit_test_ne(dest, lhs_reg, rhs_reg)
                }
                _ => unreachable!(),
            },
            Intrinsic::CharCmp => match op {
                Some(ast::BinOp::Cmp(ast::CmpOp::Lt)) => {
                    self.builder.emit_test_lt(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Le)) => {
                    self.builder.emit_test_le(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Gt)) => {
                    self.builder.emit_test_gt(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Ge)) => {
                    self.builder.emit_test_ge(dest, lhs_reg, rhs_reg)
                }
                None => {
                    let fct_id = info.fct_id.expect("fct_id missing");
                    let ty = self.ty(lhs.id());
                    let type_params = ty.type_params();
                    let idx = self.builder.add_const_fct_types(fct_id, type_params);
                    self.builder.emit_push_register(lhs_reg);
                    self.builder.emit_push_register(rhs_reg);
                    self.builder.emit_invoke_direct(dest, idx, pos);
                }
                _ => unreachable!(),
            },
            Intrinsic::EnumEq => self.builder.emit_test_eq(dest, lhs_reg, rhs_reg),
            Intrinsic::EnumNe => self.builder.emit_test_ne(dest, lhs_reg, rhs_reg),
            Intrinsic::Int32Eq => match op {
                Some(ast::BinOp::Cmp(ast::CmpOp::Eq)) => {
                    self.builder.emit_test_eq(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Ne)) => {
                    self.builder.emit_test_ne(dest, lhs_reg, rhs_reg)
                }
                None => self.builder.emit_test_eq(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::Int32Cmp => match op {
                Some(ast::BinOp::Cmp(ast::CmpOp::Lt)) => {
                    self.builder.emit_test_lt(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Le)) => {
                    self.builder.emit_test_le(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Gt)) => {
                    self.builder.emit_test_gt(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Ge)) => {
                    self.builder.emit_test_ge(dest, lhs_reg, rhs_reg)
                }
                Some(_) => unreachable!(),
                None => {
                    let fct_id = info.fct_id.expect("fct_id missing");
                    let ty = self.ty(lhs.id());
                    let type_params = ty.type_params();
                    let idx = self.builder.add_const_fct_types(fct_id, type_params);
                    self.builder.emit_push_register(lhs_reg);
                    self.builder.emit_push_register(rhs_reg);
                    self.builder.emit_invoke_direct(dest, idx, pos);
                }
            },
            Intrinsic::Int64Eq => match op {
                Some(ast::BinOp::Cmp(ast::CmpOp::Eq)) => {
                    self.builder.emit_test_eq(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Ne)) => {
                    self.builder.emit_test_ne(dest, lhs_reg, rhs_reg)
                }
                None => self.builder.emit_test_eq(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::Int64Cmp => match op {
                Some(ast::BinOp::Cmp(ast::CmpOp::Lt)) => {
                    self.builder.emit_test_lt(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Le)) => {
                    self.builder.emit_test_le(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Gt)) => {
                    self.builder.emit_test_gt(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Ge)) => {
                    self.builder.emit_test_ge(dest, lhs_reg, rhs_reg)
                }
                Some(_) => unreachable!(),
                None => {
                    let fct_id = info.fct_id.expect("fct_id missing");
                    let ty = self.ty(lhs.id());
                    let type_params = ty.type_params();
                    let idx = self.builder.add_const_fct_types(fct_id, type_params);
                    self.builder.emit_push_register(lhs_reg);
                    self.builder.emit_push_register(rhs_reg);
                    self.builder.emit_invoke_direct(dest, idx, pos);
                }
            },
            Intrinsic::Float32Eq => match op {
                Some(ast::BinOp::Cmp(ast::CmpOp::Eq)) => {
                    self.builder.emit_test_eq(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Ne)) => {
                    self.builder.emit_test_ne(dest, lhs_reg, rhs_reg)
                }
                None => self.builder.emit_test_eq(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::Float32Cmp => match op {
                Some(ast::BinOp::Cmp(ast::CmpOp::Lt)) => {
                    self.builder.emit_test_lt(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Le)) => {
                    self.builder.emit_test_le(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Gt)) => {
                    self.builder.emit_test_gt(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Ge)) => {
                    self.builder.emit_test_ge(dest, lhs_reg, rhs_reg)
                }
                None => {
                    let fct_id = info.fct_id.expect("fct_id missing");
                    let ty = self.ty(lhs.id());
                    let type_params = ty.type_params();
                    let idx = self.builder.add_const_fct_types(fct_id, type_params);
                    self.builder.emit_push_register(lhs_reg);
                    self.builder.emit_push_register(rhs_reg);
                    self.builder.emit_invoke_direct(dest, idx, pos)
                }
                _ => unreachable!(),
            },
            Intrinsic::Float64Eq => match op {
                Some(ast::BinOp::Cmp(ast::CmpOp::Eq)) => {
                    self.builder.emit_test_eq(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Ne)) => {
                    self.builder.emit_test_ne(dest, lhs_reg, rhs_reg)
                }
                None => self.builder.emit_test_eq(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::Float64Cmp => match op {
                Some(ast::BinOp::Cmp(ast::CmpOp::Lt)) => {
                    self.builder.emit_test_lt(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Le)) => {
                    self.builder.emit_test_le(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Gt)) => {
                    self.builder.emit_test_gt(dest, lhs_reg, rhs_reg)
                }
                Some(ast::BinOp::Cmp(ast::CmpOp::Ge)) => {
                    self.builder.emit_test_ge(dest, lhs_reg, rhs_reg)
                }
                None => {
                    let fct_id = info.fct_id.expect("fct_id missing");
                    let ty = self.ty(lhs.id());
                    let type_params = ty.type_params();
                    let idx = self.builder.add_const_fct_types(fct_id, type_params);
                    self.builder.emit_push_register(lhs_reg);
                    self.builder.emit_push_register(rhs_reg);
                    self.builder.emit_invoke_direct(dest, idx, pos);
                }

                _ => unreachable!(),
            },
            Intrinsic::Int32Add => self.builder.emit_add(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Int32Sub => self.builder.emit_sub(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Int32Mul => self.builder.emit_mul(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Int32Div => self.builder.emit_div(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Int32Mod => self.builder.emit_mod(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Int32Or => self.builder.emit_or(dest, lhs_reg, rhs_reg),
            Intrinsic::Int32And => self.builder.emit_and(dest, lhs_reg, rhs_reg),
            Intrinsic::Int32Xor => self.builder.emit_xor(dest, lhs_reg, rhs_reg),
            Intrinsic::Int32Shl => self.builder.emit_shl(dest, lhs_reg, rhs_reg),
            Intrinsic::Int32Shr => self.builder.emit_shr(dest, lhs_reg, rhs_reg),
            Intrinsic::Int32Sar => self.builder.emit_sar(dest, lhs_reg, rhs_reg),

            Intrinsic::Int64Add => self.builder.emit_add(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Int64Sub => self.builder.emit_sub(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Int64Mul => self.builder.emit_mul(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Int64Div => self.builder.emit_div(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Int64Mod => self.builder.emit_mod(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Int64Or => self.builder.emit_or(dest, lhs_reg, rhs_reg),
            Intrinsic::Int64And => self.builder.emit_and(dest, lhs_reg, rhs_reg),
            Intrinsic::Int64Xor => self.builder.emit_xor(dest, lhs_reg, rhs_reg),
            Intrinsic::Int64Shl => self.builder.emit_shl(dest, lhs_reg, rhs_reg),
            Intrinsic::Int64Shr => self.builder.emit_shr(dest, lhs_reg, rhs_reg),
            Intrinsic::Int64Sar => self.builder.emit_sar(dest, lhs_reg, rhs_reg),

            Intrinsic::Float32Add => self.builder.emit_add(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Float32Sub => self.builder.emit_sub(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Float32Mul => self.builder.emit_mul(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Float32Div => self.builder.emit_div(dest, lhs_reg, rhs_reg, pos),

            Intrinsic::Float64Add => self.builder.emit_add(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Float64Sub => self.builder.emit_sub(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Float64Mul => self.builder.emit_mul(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Float64Div => self.builder.emit_div(dest, lhs_reg, rhs_reg, pos),

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
                ast::Expr::Dot(ref dot) => self.visit_expr_assign_dot(expr, dot),
                ast::Expr::Call(ref call) => self.visit_expr_assign_call(expr, call),
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
                        &index.expr,
                        value,
                        expr.pos,
                        DataDest::Effect,
                    );
                }
                _ => panic!("unexpected intrinsic {:?}", info.intrinsic),
            }
        } else {
            let call_type = self.analysis.map_calls.get(expr.id).unwrap();
            let fct_id = call_type.fct_id().unwrap();

            let obj_reg = self.visit_expr(object, DataDest::Alloc);
            let idx_reg = self.visit_expr(&index.expr, DataDest::Alloc);
            let val_reg = self.visit_expr(value, DataDest::Alloc);

            let obj_ty = self.ty(object.id());

            self.builder.emit_push_register(obj_reg);
            self.builder.emit_push_register(idx_reg);
            self.builder.emit_push_register(val_reg);

            let type_params = obj_ty.type_params();

            let callee_idx = self.builder.add_const_fct_types(fct_id, type_params);
            let dest = self.ensure_unit_register();
            self.builder.emit_invoke_direct(dest, callee_idx, expr.pos);

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
        let field_idx = self
            .builder
            .add_const_field_types(cls_id, type_params.clone(), field_id);

        let obj = self.visit_expr(&dot.lhs, DataDest::Alloc);
        let src = self.visit_expr(&expr.rhs, DataDest::Alloc);

        self.builder.emit_store_field(src, obj, field_idx, expr.pos);

        self.free_if_temp(obj);
        self.free_if_temp(src);
    }

    fn visit_expr_assign_context(
        &mut self,
        expr: &ast::ExprBinType,
        distance: usize,
        context_idx: ContextIdx,
    ) {
        let value_reg = self.visit_expr(&expr.rhs, DataDest::Alloc);

        let self_reg = self.var_reg(SELF_VAR_ID);

        // Load context field of lambda object in self.
        let outer_context_reg = self.alloc_temp(BytecodeType::Ptr);
        let lambda_cls_id = self.sa.known.classes.lambda();
        let idx =
            self.builder
                .add_const_field_types(lambda_cls_id, SourceTypeArray::empty(), FieldId(0));
        self.builder
            .emit_load_field(outer_context_reg, self_reg, idx, expr.pos);

        assert!(distance >= 1);

        let mut outer_fct_id = self.fct.parent.fct_id();
        let mut distance_left = distance;

        while distance_left > 1 {
            let outer_fct = self.sa.fcts.idx(outer_fct_id);
            let outer_fct = outer_fct.read();
            let outer_cls_id = outer_fct
                .analysis()
                .context_cls_id
                .expect("context class missing");

            let idx = self.builder.add_const_field_types(
                outer_cls_id,
                self.identity_type_params(),
                FieldId(0),
            );
            self.builder
                .emit_load_field(outer_context_reg, outer_context_reg, idx, expr.pos);

            distance_left -= 1;
            outer_fct_id = outer_fct.parent.fct_id();
        }

        assert_eq!(distance_left, 1);

        // Store value in context field
        let outer_fct = self.sa.fcts.idx(outer_fct_id);
        let outer_fct = outer_fct.read();
        let analysis = outer_fct.analysis();
        let outer_cls_id = analysis.context_cls_id.expect("context class missing");

        let field_id =
            field_id_from_context_idx(context_idx, analysis.context_has_outer_context_slot());
        let idx =
            self.builder
                .add_const_field_types(outer_cls_id, self.identity_type_params(), field_id);
        self.builder
            .emit_store_field(value_reg, outer_context_reg, idx, expr.pos);

        self.free_temp(outer_context_reg);
        self.free_if_temp(value_reg);
    }

    fn visit_expr_assign_var(&mut self, expr: &ast::ExprBinType, var_id: VarId) {
        let var = self.analysis.vars.get_var(var_id);

        match var.location {
            VarLocation::Context(context_idx) => {
                let value_reg = self.visit_expr(&expr.rhs, DataDest::Alloc);
                self.store_in_context(value_reg, context_idx, expr.pos);
                self.free_if_temp(value_reg);
            }

            VarLocation::Stack => {
                let dest = if var.ty.is_unit() {
                    DataDest::Effect
                } else {
                    let var_reg = self.var_reg(var_id);
                    DataDest::Reg(var_reg)
                };

                self.visit_expr(&expr.rhs, dest);
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

        let src = self.visit_expr(&expr.rhs, dest);

        if !global_var.ty.is_unit() {
            self.builder.emit_store_global(src, gid);
        }

        self.free_if_temp(src);
    }

    fn visit_expr_ident(&mut self, ident: &ast::ExprIdentType, dest: DataDest) -> Register {
        let ident_type = self.analysis.map_idents.get(ident.id).unwrap();

        match ident_type {
            &IdentType::Var(var_id) => self.visit_expr_ident_var(var_id, dest, ident.pos),
            &IdentType::Context(distance, field_id) => {
                self.visit_expr_ident_context(distance, field_id, dest, ident.pos)
            }
            &IdentType::Global(gid) => self.visit_expr_ident_global(gid, dest),
            &IdentType::Const(cid) => self.visit_expr_ident_const(cid, dest),
            &IdentType::EnumValue(enum_id, ref type_params, variant_idx) => {
                self.emit_new_enum(enum_id, type_params.clone(), variant_idx, ident.pos, dest)
            }

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
        pos: Position,
    ) -> Register {
        let self_reg = self.var_reg(SELF_VAR_ID);

        // Load context field of lambda object (in self register).
        let outer_context_reg = self.alloc_temp(BytecodeType::Ptr);
        let lambda_cls_id = self.sa.known.classes.lambda();
        let idx =
            self.builder
                .add_const_field_types(lambda_cls_id, SourceTypeArray::empty(), FieldId(0));
        self.builder
            .emit_load_field(outer_context_reg, self_reg, idx, pos);

        assert!(distance >= 1);

        let mut outer_fct_id = self.fct.parent.fct_id();
        let mut distance_left = distance;

        while distance_left > 1 {
            let outer_fct = self.sa.fcts.idx(outer_fct_id);
            let outer_fct = outer_fct.read();
            let outer_cls_id = outer_fct
                .analysis()
                .context_cls_id
                .expect("context class missing");

            let idx = self.builder.add_const_field_types(
                outer_cls_id,
                self.identity_type_params(),
                FieldId(0),
            );
            self.builder
                .emit_load_field(outer_context_reg, outer_context_reg, idx, pos);

            distance_left -= 1;
            outer_fct_id = outer_fct.parent.fct_id();
        }

        assert_eq!(distance_left, 1);

        let outer_fct = self.sa.fcts.idx(outer_fct_id);
        let outer_fct = outer_fct.read();
        let analysis = outer_fct.analysis();
        let outer_cls_id = analysis.context_cls_id.expect("context class missing");

        let outer_cls = self.sa.classes.idx(outer_cls_id);
        let outer_cls = outer_cls.read();

        let field_id =
            field_id_from_context_idx(context_idx, analysis.context_has_outer_context_slot());
        let field = &outer_cls.fields[field_id];

        let ty: BytecodeType = register_bty_from_ty(field.ty.clone());
        let value_reg = self.ensure_register(dest, ty);

        let idx =
            self.builder
                .add_const_field_types(outer_cls_id, self.identity_type_params(), field_id);
        self.builder
            .emit_load_field(value_reg, outer_context_reg, idx, pos);

        self.free_temp(outer_context_reg);

        value_reg
    }

    fn visit_expr_ident_const(&mut self, const_id: ConstDefinitionId, dest: DataDest) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

        let const_ = self.sa.consts.idx(const_id);
        let const_ = const_.read();
        let ty = const_.ty.clone();

        let bytecode_ty = register_bty_from_ty(ty.clone());
        let dest = self.ensure_register(dest, bytecode_ty);

        match ty {
            SourceType::Bool => {
                if const_.value.to_bool() {
                    self.builder.emit_const_true(dest);
                } else {
                    self.builder.emit_const_false(dest);
                }
            }

            SourceType::Char => {
                self.builder.emit_const_char(dest, const_.value.to_char());
            }

            SourceType::UInt8 => {
                self.builder
                    .emit_const_uint8(dest, const_.value.to_int() as u8);
            }

            SourceType::Int32 => {
                self.builder
                    .emit_const_int32(dest, const_.value.to_int() as i32);
            }

            SourceType::Int64 => {
                self.builder.emit_const_int64(dest, const_.value.to_int());
            }

            SourceType::Float32 => {
                self.builder
                    .emit_const_float32(dest, const_.value.to_float() as f32);
            }

            SourceType::Float64 => {
                self.builder
                    .emit_const_float64(dest, const_.value.to_float());
            }

            _ => unimplemented!(),
        }

        dest
    }

    fn visit_expr_ident_global(&mut self, gid: GlobalDefinitionId, dest: DataDest) -> Register {
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

        self.builder.emit_load_global(dest, gid);

        dest
    }

    fn visit_expr_ident_var(&mut self, var_id: VarId, dest: DataDest, pos: Position) -> Register {
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
                self.load_from_context(dest_reg, context_idx, pos);
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

    fn store_in_context(&mut self, src: Register, context_idx: ContextIdx, pos: Position) {
        let context_register = self.context_register.expect("context register missing");
        let cls_id = self.analysis.context_cls_id.expect("class missing");
        let field_id =
            field_id_from_context_idx(context_idx, self.analysis.context_has_outer_context_slot());
        let field_idx =
            self.builder
                .add_const_field_types(cls_id, self.identity_type_params(), field_id);
        self.builder
            .emit_store_field(src, context_register, field_idx, pos);
    }

    fn load_from_context(&mut self, dest: Register, context_idx: ContextIdx, pos: Position) {
        // Load context object.
        let context_register = self.context_register.expect("context register missing");
        let cls_id = self.analysis.context_cls_id.expect("class missing");
        let field_id =
            field_id_from_context_idx(context_idx, self.analysis.context_has_outer_context_slot());
        let field_idx =
            self.builder
                .add_const_field_types(cls_id, self.identity_type_params(), field_id);
        self.builder
            .emit_load_field(dest, context_register, field_idx, pos);
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
            CallType::CtorParent(ty, _) | CallType::Ctor(ty, _) => ty.type_params(),

            CallType::Method(_, _, ref type_params) => type_params.clone(),

            CallType::ModuleMethod(ty, _, ref fct_type_params) => {
                let cls_type_params = ty.type_params();
                assert!(cls_type_params.is_empty());
                fct_type_params.clone()
            }

            CallType::Fct(_, ref type_params) => type_params.clone(),

            CallType::Expr(_, _, ref type_params) => type_params.clone(),

            CallType::TraitObjectMethod(_, _) => SourceTypeArray::empty(),
            CallType::GenericMethod(_, _, _) => SourceTypeArray::empty(),
            CallType::GenericStaticMethod(_, _, _) => SourceTypeArray::empty(),

            CallType::Enum(_, _) => unreachable!(),
            CallType::Intrinsic(_) => unreachable!(),
            CallType::Struct(_, _) => unreachable!(),
            CallType::Lambda(_, _) => unreachable!(),
            CallType::Class2Ctor(_, _) => unreachable!(),
        }
    }

    fn specialize_call(&mut self, fct: &FctDefinition, call_type: &CallType) -> ConstPoolIdx {
        let type_params = self.determine_call_type_params(call_type);
        assert_eq!(fct.type_params.len(), type_params.len());

        match *call_type {
            CallType::GenericStaticMethod(id, _, _) | CallType::GenericMethod(id, _, _) => {
                self.builder.add_const_generic(id, fct.id(), type_params)
            }
            _ => self.builder.add_const_fct_types(fct.id(), type_params),
        }
    }

    fn specialize_type_for_call(&self, call_type: &CallType, ty: SourceType) -> SourceType {
        match call_type {
            CallType::Fct(_, ref type_params) => specialize_type(self.sa, ty, type_params),

            CallType::Method(_, _, ref type_params) => specialize_type(self.sa, ty, type_params),

            CallType::ModuleMethod(cls_ty, _, ref fct_type_params) => {
                let cls_type_params = cls_ty.type_params();
                let type_params = cls_type_params.connect(fct_type_params);
                specialize_type(self.sa, ty, &type_params)
            }

            CallType::CtorParent(cls_ty, _) | CallType::Ctor(cls_ty, _) => {
                let cls_type_params = cls_ty.type_params();
                specialize_type(self.sa, ty, &cls_type_params)
            }

            CallType::Expr(_, _, ref type_params) => specialize_type(self.sa, ty, type_params),

            CallType::TraitObjectMethod(trait_ty, _) => {
                let container_type_params = trait_ty.type_params();
                specialize_type(self.sa, ty, &container_type_params)
            }
            CallType::GenericMethod(id, _, _) | CallType::GenericStaticMethod(id, _, _) => {
                debug_assert!(ty.is_concrete_type(self.sa) || ty.is_self());
                if ty.is_self() {
                    SourceType::TypeParam(*id)
                } else {
                    ty
                }
            }

            CallType::Enum(_, _) => unreachable!(),
            CallType::Intrinsic(_) => unreachable!(),
            CallType::Struct(_, _) => unreachable!(),
            CallType::Lambda(_, _) => unreachable!(),
            CallType::Class2Ctor(_, _) => unreachable!(),
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

        // the function we compile right now is never an intrinsic
        if self.fct.id() == fid {
            return None;
        }

        let fct = self.sa.fcts.idx(fid);
        let fct = fct.read();

        if let Some(intrinsic) = fct.intrinsic {
            return Some(IntrinsicInfo::with_fct(intrinsic, fid));
        }

        None
    }

    fn identity_type_params(&self) -> SourceTypeArray {
        let len = self.fct.type_params.len();

        if len == 0 {
            return SourceTypeArray::empty();
        }

        let type_params = (0..len)
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
        SourceType::Class(class_id, type_params) => BytecodeType::Class(class_id, type_params),
        SourceType::Trait(trait_id, type_params) => BytecodeType::Trait(trait_id, type_params),
        SourceType::Enum(enum_id, type_params) => BytecodeType::Enum(enum_id, type_params),
        SourceType::Struct(struct_id, type_params) => BytecodeType::Struct(struct_id, type_params),
        SourceType::Tuple(subtypes) => BytecodeType::Tuple(subtypes),
        SourceType::TypeParam(idx) => BytecodeType::TypeParam(idx.to_usize() as u32),
        SourceType::Lambda(params, return_type) => BytecodeType::Lambda(params, return_type),
        SourceType::Ptr => BytecodeType::Ptr,
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
        SourceType::Trait(trait_id, type_params) => BytecodeType::Trait(trait_id, type_params),
        SourceType::Enum(enum_id, type_params) => BytecodeType::Enum(enum_id, type_params),
        SourceType::Struct(struct_id, type_params) => BytecodeType::Struct(struct_id, type_params),
        SourceType::Tuple(subtypes) => BytecodeType::Tuple(subtypes),
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
