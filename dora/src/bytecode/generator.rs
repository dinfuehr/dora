use dora_parser::lexer::position::Position;
use std::collections::HashMap;
use std::convert::TryInto;

use dora_parser::ast::*;

use crate::bytecode::{
    BytecodeBuilder, BytecodeFunction, BytecodeType, ConstPoolIdx, Label, Register,
};
use crate::semck::specialize::specialize_type;
use crate::semck::{expr_always_returns, expr_block_always_returns};
use crate::ty::{find_impl, SourceType, SourceTypeArray};
use crate::vm::{
    AnalysisData, CallType, ConstId, EnumId, FctDefinition, FctDefinitionId, GlobalDefinitionId,
    IdentType, Intrinsic, SemAnalysis, StructId, TupleId, VarId,
};

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
        src,

        gen: BytecodeBuilder::new(&sa.args),
        loops: Vec::new(),
        var_registers: HashMap::new(),
    };
    ast_bytecode_generator.generate(&fct.ast)
}

struct AstBytecodeGen<'a> {
    sa: &'a SemAnalysis,
    fct: &'a FctDefinition,
    src: &'a AnalysisData,

    gen: BytecodeBuilder,
    loops: Vec<LoopLabels>,
    var_registers: HashMap<VarId, Register>,
}

impl<'a> AstBytecodeGen<'a> {
    fn generate(mut self, ast: &Function) -> BytecodeFunction {
        let mut arguments = 0;
        self.push_scope();

        if self.fct.has_self() {
            let var_self = self.src.var_self();
            let var_ty = var_self.ty.clone();

            if !var_ty.is_unit() {
                let var_id = var_self.id;
                let bty = BytecodeType::from_ty(self.sa, var_ty);
                let reg = self.alloc_var(bty);
                self.var_registers.insert(var_id, reg);
                arguments += 1;
            }
        }

        for param in &ast.params {
            let var_id = *self.src.map_vars.get(param.id).unwrap();
            let ty = self.var_ty(var_id);

            if ty.is_unit() {
                // no register needed for unit
            } else {
                let bty = BytecodeType::from_ty(self.sa, ty);
                let reg = self.alloc_var(bty);
                self.var_registers.insert(var_id, reg);
                arguments += 1;
            }
        }

        self.gen.set_arguments(arguments);

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

        if self.fct.return_type.is_unit() {
            self.gen.emit_ret_void();
        }

        self.pop_scope();
        self.gen.generate(self.sa)
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        match *stmt {
            Stmt::Return(ref ret) => self.visit_stmt_return(ret),
            Stmt::Break(ref stmt) => self.visit_stmt_break(stmt),
            Stmt::Continue(ref stmt) => self.visit_stmt_continue(stmt),
            Stmt::Expr(ref expr) => self.visit_stmt_expr(expr),
            Stmt::Let(ref stmt) => self.visit_stmt_let(stmt),
            Stmt::While(ref stmt) => self.visit_stmt_while(stmt),
            Stmt::For(ref stmt) => self.visit_stmt_for(stmt),
        }
    }

    fn visit_stmt_for(&mut self, stmt: &StmtForType) {
        if self.src.map_fors.get(stmt.id).is_some() {
            self.visit_stmt_for_iterator(stmt);
        } else {
            self.visit_stmt_for_array(stmt);
        }
    }

    fn visit_stmt_for_array(&mut self, stmt: &StmtForType) {
        self.push_scope();

        let array_reg = self.alloc_var(BytecodeType::Ptr);
        let index_reg = self.alloc_var(BytecodeType::Int64);
        let length_reg = self.alloc_var(BytecodeType::Int64);

        self.visit_stmt_for_pattern_setup(&stmt.pattern);

        // evaluate and store array
        self.visit_expr(&stmt.expr, DataDest::Reg(array_reg));

        // calculate array length
        self.gen
            .emit_array_length(length_reg, array_reg, stmt.expr.pos());

        // initialized index to 0
        self.gen.emit_const_int64(index_reg, 0);

        let lbl_cond = self.gen.define_label();
        self.gen.emit_loop_start();
        let lbl_end = self.gen.create_label();

        // if idx >= length then goto end
        let tmp_reg = self.alloc_temp(BytecodeType::Bool);
        self.gen.emit_test_lt_int64(tmp_reg, index_reg, length_reg);
        self.gen.emit_jump_if_false(tmp_reg, lbl_end);
        self.free_temp(tmp_reg);

        // type of expression: Array[Something]
        let ty = self.ty(stmt.expr.id());
        // get type of element: Something for Array[Something]
        let ty = ty.type_params(self.sa).types().first().cloned().unwrap();
        self.visit_stmt_for_pattern_assign_array(&stmt.pattern, array_reg, index_reg, ty);

        self.loops.push(LoopLabels::new(lbl_cond, lbl_end));
        self.visit_stmt(&stmt.block);
        self.loops.pop().unwrap();

        // increment index
        let tmp_reg = self.alloc_temp(BytecodeType::Int64);
        self.gen.emit_const_int64(tmp_reg, 1);
        self.gen
            .emit_add_int64(index_reg, index_reg, tmp_reg, stmt.pos);
        self.free_temp(tmp_reg);

        // jump to loop header
        self.gen.emit_jump_loop(lbl_cond);
        self.gen.bind_label(lbl_end);

        self.pop_scope();
    }

    fn visit_stmt_for_pattern_setup(&mut self, pattern: &LetPattern) {
        match pattern {
            LetPattern::Ident(ref ident) => {
                let for_var_id = *self.src.map_vars.get(ident.id).unwrap();
                let var_ty = self.var_ty(for_var_id);

                if !var_ty.is_unit() {
                    let bty: BytecodeType = BytecodeType::from_ty(self.sa, var_ty);
                    let var_reg = self.alloc_var(bty);
                    self.var_registers.insert(for_var_id, var_reg);
                }
            }
            LetPattern::Underscore(_) => {
                // nothing to do
            }
            LetPattern::Tuple(ref tuple) => {
                for part in &tuple.parts {
                    self.visit_stmt_for_pattern_setup(part);
                }
            }
        }
    }

    fn visit_stmt_for_pattern_assign_array(
        &mut self,
        pattern: &LetPattern,
        array_reg: Register,
        index_reg: Register,
        ty: SourceType,
    ) {
        match pattern {
            LetPattern::Ident(ref ident) => {
                let var_id = *self.src.map_vars.get(ident.id).unwrap();
                let var_ty = self.var_ty(var_id);

                if !var_ty.is_unit() {
                    let var_reg = self.var_reg(var_id);
                    self.gen
                        .emit_load_array(var_reg, array_reg, index_reg, ident.pos);
                }
            }

            LetPattern::Underscore(_) => {
                // nothing to do
            }

            LetPattern::Tuple(ref tuple) => {
                if tuple.parts.len() > 0 {
                    let bytecode_ty: BytecodeType = BytecodeType::from_ty(self.sa, ty.clone());
                    let tuple_reg = self.alloc_temp(bytecode_ty.clone());
                    self.gen
                        .emit_load_array(tuple_reg, array_reg, index_reg, tuple.pos);
                    self.destruct_tuple_pattern(tuple, tuple_reg, ty);
                    self.free_temp(tuple_reg);
                }
            }
        }
    }

    fn visit_stmt_for_pattern_assign_iterator(
        &mut self,
        pattern: &LetPattern,
        next_reg: Register,
        next_ty: SourceType,
    ) {
        match pattern {
            LetPattern::Ident(ref ident) => {
                let var_id = *self.src.map_vars.get(ident.id).unwrap();
                let var_ty = self.var_ty(var_id);

                if !var_ty.is_unit() {
                    let var_reg = self.var_reg(var_id);
                    self.emit_mov(var_reg, next_reg)
                }
            }

            LetPattern::Underscore(_) => {
                // nothing to do
            }

            LetPattern::Tuple(ref tuple) => {
                assert!(tuple.parts.len() > 0);
                self.destruct_tuple_pattern(tuple, next_reg, next_ty);
            }
        }
    }

    fn destruct_tuple_pattern(
        &mut self,
        tuple: &LetTupleType,
        tuple_reg: Register,
        tuple_ty: SourceType,
    ) {
        let tuple_id = tuple_ty.tuple_id().expect("type should be tuple");

        for (idx, part) in tuple.parts.iter().enumerate() {
            match &**part {
                LetPattern::Ident(ref ident) => {
                    let var_id = *self.src.map_vars.get(ident.id).unwrap();
                    let ty = self.var_ty(var_id);

                    if !ty.is_unit() {
                        let bytecode_ty: BytecodeType = BytecodeType::from_ty(self.sa, ty);
                        let var_reg = self.alloc_var(bytecode_ty);
                        self.var_registers.insert(var_id, var_reg);

                        self.gen
                            .emit_load_tuple_element(var_reg, tuple_reg, tuple_id, idx as u32);
                    }
                }

                LetPattern::Underscore(_) => {
                    // nothing to do
                }

                LetPattern::Tuple(ref tuple) => {
                    let ty = self.sa.tuples.lock().get_ty(tuple_id, idx);

                    if !ty.is_unit() {
                        let temp_reg = self.alloc_temp(BytecodeType::from_ty(self.sa, ty.clone()));
                        self.gen
                            .emit_load_tuple_element(temp_reg, tuple_reg, tuple_id, idx as u32);
                        self.destruct_tuple_pattern(tuple, temp_reg, ty);
                        self.free_temp(temp_reg);
                    }
                }
            }
        }
    }

    fn visit_stmt_for_iterator(&mut self, stmt: &StmtForType) {
        self.push_scope();
        let for_type_info = self.src.map_fors.get(stmt.id).unwrap().clone();

        // Emit: <obj> = <expr> (for <var> in <expr> { ... })
        let object_reg = self.visit_expr(&stmt.expr, DataDest::Alloc);

        let iterator_reg = if let Some(make_iterator) = for_type_info.make_iterator {
            let object_type = self.ty(stmt.expr.id());
            let object_type_params = object_type.type_params(self.sa);

            // Emit: <iterator> = <obj>.makeIterator();
            let iterator_reg = self.alloc_var(BytecodeType::Ptr);
            self.gen.emit_push_register(object_reg);
            let fct_idx = self
                .gen
                .add_const_fct_types(make_iterator, object_type_params);
            self.gen
                .emit_invoke_direct(iterator_reg, fct_idx, stmt.expr.pos());
            iterator_reg
        } else {
            // Object is already the iterator - just use it
            object_reg
        };

        let lbl_cond = self.gen.define_label();
        self.gen.emit_loop_start();

        let iterator_type = for_type_info.iterator_type.clone();
        let iterator_type_params = iterator_type.type_params(self.sa);

        self.gen.emit_push_register(iterator_reg);

        let lbl_end = self.gen.create_label();

        // Emit: <cond> = <iterator>.hasNext() & jump to lbl_end if false
        let cond_reg = self.alloc_temp(BytecodeType::Bool);
        let fct_idx = self
            .gen
            .add_const_fct_types(for_type_info.has_next, iterator_type_params.clone());
        self.gen
            .emit_invoke_direct(cond_reg, fct_idx, stmt.expr.pos());
        self.gen.emit_jump_if_false(cond_reg, lbl_end);
        self.free_temp(cond_reg);

        // Emit: <var> = <iterator>.next()
        let next_ty = for_type_info.next_type.clone();
        let fct_idx = self
            .gen
            .add_const_fct_types(for_type_info.next, iterator_type_params.clone());
        if next_ty.is_unit() {
            self.gen.emit_push_register(iterator_reg);
            self.gen.emit_invoke_direct_void(fct_idx, stmt.expr.pos());
        } else {
            let next_bytecode_ty: BytecodeType = BytecodeType::from_ty(self.sa, next_ty.clone());
            let next_reg = self.alloc_var(next_bytecode_ty);

            self.gen.emit_push_register(iterator_reg);
            self.emit_invoke_direct(next_ty.clone(), next_reg, fct_idx, stmt.expr.pos());

            self.visit_stmt_for_pattern_setup(&stmt.pattern);
            self.visit_stmt_for_pattern_assign_iterator(&stmt.pattern, next_reg, next_ty);
        }

        self.loops.push(LoopLabels::new(lbl_cond, lbl_end));
        self.visit_stmt(&stmt.block);
        self.loops.pop().unwrap();

        self.gen.emit_jump_loop(lbl_cond);
        self.gen.bind_label(lbl_end);

        self.pop_scope();

        self.free_if_temp(object_reg);
    }

    fn visit_stmt_let(&mut self, stmt: &StmtLetType) {
        match &*stmt.pattern {
            LetPattern::Ident(ref ident) => {
                self.visit_stmt_let_ident(stmt, ident);
            }

            LetPattern::Underscore(_) => {
                self.visit_stmt_let_underscore(stmt);
            }

            LetPattern::Tuple(ref tuple) => {
                self.visit_stmt_let_pattern(stmt, tuple);
            }
        }
    }

    fn visit_stmt_let_ident(&mut self, stmt: &StmtLetType, ident: &LetIdentType) {
        let var_id = *self.src.map_vars.get(ident.id).unwrap();
        let ty = self.var_ty(var_id);

        let dest = if ty.is_unit() {
            DataDest::Effect
        } else {
            let ty: BytecodeType = BytecodeType::from_ty(self.sa, ty);
            let var_reg = self.alloc_var(ty);

            self.var_registers.insert(var_id, var_reg);

            DataDest::Reg(var_reg)
        };

        if let Some(ref expr) = stmt.expr {
            self.visit_expr(expr, dest);
        }
    }

    fn visit_stmt_let_underscore(&mut self, stmt: &StmtLetType) {
        if let Some(ref expr) = stmt.expr {
            self.visit_expr(expr, DataDest::Effect);
        }
    }

    fn visit_stmt_let_pattern(&mut self, stmt: &StmtLetType, pattern: &LetTupleType) {
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

    fn visit_stmt_let_tuple_init(&mut self, tuple: &LetTupleType) {
        for part in &tuple.parts {
            match &**part {
                LetPattern::Ident(ref ident) => {
                    let var_id = *self.src.map_vars.get(ident.id).unwrap();
                    let ty: BytecodeType = BytecodeType::from_ty(self.sa, self.var_ty(var_id));
                    let var_reg = self.alloc_var(ty.clone());
                    self.var_registers.insert(var_id, var_reg);
                }

                LetPattern::Underscore(_) => {
                    // nothing to do
                }

                LetPattern::Tuple(ref tuple) => {
                    self.visit_stmt_let_tuple_init(tuple);
                }
            }
        }
    }

    fn visit_stmt_while(&mut self, stmt: &StmtWhileType) {
        let cond_lbl = self.gen.define_label();
        let end_lbl = self.gen.create_label();
        self.gen.emit_loop_start();
        let cond_reg = self.visit_expr(&stmt.cond, DataDest::Alloc);
        self.gen.emit_jump_if_false(cond_reg, end_lbl);
        self.free_if_temp(cond_reg);
        self.loops.push(LoopLabels::new(cond_lbl, end_lbl));
        self.visit_stmt(&stmt.block);
        self.loops.pop().unwrap();
        self.gen.emit_jump_loop(cond_lbl);
        self.gen.bind_label(end_lbl);
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExprType) {
        let reg = self.visit_expr(&stmt.expr, DataDest::Effect);
        self.free_if_temp(reg);
    }

    fn visit_stmt_return(&mut self, ret: &StmtReturnType) {
        if let Some(ref expr) = ret.expr {
            let result_reg = self.visit_expr(expr, DataDest::Alloc);
            self.emit_ret_value(result_reg);
            self.free_if_temp(result_reg);
        } else {
            self.gen.emit_ret_void();
        }
    }

    fn emit_ret_value(&mut self, result_reg: Register) {
        let ret_ty = self.fct.return_type.clone();

        if ret_ty.is_unit() {
            self.gen.emit_ret_void();
            return;
        }

        self.gen.emit_ret(result_reg);
    }

    fn visit_stmt_break(&mut self, _stmt: &StmtBreakType) {
        let end = self.loops.last().unwrap().end;
        self.gen.emit_jump(end);
    }

    fn visit_stmt_continue(&mut self, _stmt: &StmtContinueType) {
        let cond = self.loops.last().unwrap().cond;
        self.gen.emit_jump_loop(cond);
    }

    fn visit_expr(&mut self, expr: &Expr, dest: DataDest) -> Register {
        match *expr {
            Expr::Un(ref un) => self.visit_expr_un(un, dest),
            Expr::Bin(ref bin) => self.visit_expr_bin(bin, dest),
            Expr::Dot(ref field) => self.visit_expr_dot(field, dest),
            Expr::Block(ref block) => self.visit_expr_block(block, dest),
            Expr::If(ref expr) => self.visit_expr_if(expr, dest),
            Expr::Template(ref template) => self.visit_expr_template(template, dest),
            Expr::TypeParam(ref expr) => self.visit_expr_type_param(expr, dest),
            Expr::Path(ref path) => self.visit_expr_path(path, dest),
            Expr::LitChar(ref lit) => self.visit_expr_lit_char(lit, dest),
            Expr::LitInt(ref lit) => self.visit_expr_lit_int(lit, dest, false),
            Expr::LitFloat(ref lit) => self.visit_expr_lit_float(lit, dest),
            Expr::LitStr(ref lit) => self.visit_expr_lit_string(lit, dest),
            Expr::LitBool(ref lit) => self.visit_expr_lit_bool(lit, dest),
            Expr::Ident(ref ident) => self.visit_expr_ident(ident, dest),
            Expr::Call(ref call) => self.visit_expr_call(call, dest),
            Expr::Delegation(ref call) => self.visit_expr_delegation(call, dest),
            Expr::This(_) => self.visit_expr_self(dest),
            Expr::Super(_) => self.visit_expr_self(dest),
            Expr::Conv(ref conv) => self.visit_expr_conv(conv, dest),
            Expr::Tuple(ref tuple) => self.visit_expr_tuple(tuple, dest),
            Expr::Paren(ref paren) => self.visit_expr(&paren.expr, dest),
            Expr::Match(ref expr) => self.visit_expr_match(expr, dest),
            Expr::Lambda(_) => unimplemented!(),
        }
    }

    fn emit_expr_for_effect(&mut self, expr: &Expr) {
        let reg = self.visit_expr(expr, DataDest::Effect);
        self.free_if_temp(reg);
    }

    fn visit_expr_type_param(&mut self, expr: &ExprTypeParamType, dest: DataDest) -> Register {
        let ident_type = self.src.map_idents.get(expr.id).cloned().unwrap();

        match ident_type {
            IdentType::EnumValue(enum_id, type_params, variant_id) => {
                self.emit_new_enum(enum_id, type_params, variant_id, expr.pos, dest)
            }

            _ => unreachable!(),
        }
    }

    fn visit_expr_template(&mut self, expr: &ExprTemplateType, dest: DataDest) -> Register {
        let buffer_register = self.ensure_register(dest, BytecodeType::Ptr);

        // build StringBuffer::empty() call
        let fct_id = self.sa.known.functions.string_buffer_empty;
        let fct_idx = self.gen.add_const_fct(fct_id);
        self.gen
            .emit_invoke_static(buffer_register, fct_idx, expr.pos);

        let part_register = self.alloc_temp(BytecodeType::Ptr);

        for part in &expr.parts {
            if let Some(ref lit_str) = part.to_lit_str() {
                let value = lit_str.value.clone();
                self.gen.emit_const_string(part_register, value);
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
                    self.gen.emit_push_register(expr_register);

                    // build toString() call
                    let name = self.sa.interner.intern("toString");
                    let trait_id = self.sa.known.traits.stringable;
                    let xtrait = self.sa.traits[trait_id].read();
                    let to_string_id = xtrait
                        .find_method(self.sa, name, false)
                        .expect("Stringable::toString() not found");

                    let fct_idx = self.gen.add_const_generic(
                        type_list_id,
                        to_string_id,
                        SourceTypeArray::empty(),
                    );

                    self.gen
                        .emit_invoke_generic_direct(part_register, fct_idx, part.pos());

                    self.free_if_temp(expr_register);
                } else {
                    let expr_register = self.visit_expr(part, DataDest::Alloc);
                    self.gen.emit_push_register(expr_register);

                    // build toString() call
                    let name = self.sa.interner.intern("toString");
                    let stringable_impl_id = find_impl(
                        self.sa,
                        ty,
                        &self.fct.type_params,
                        self.sa.known.traits.stringable,
                    )
                    .expect("impl of Stringable not found");
                    let ximpl = self.sa.impls[stringable_impl_id].read();
                    let to_string_id = ximpl
                        .instance_names
                        .get(&name)
                        .cloned()
                        .expect("method toString() not found");

                    let fct_idx = self.gen.add_const_fct(to_string_id);
                    self.gen
                        .emit_invoke_direct(part_register, fct_idx, part.pos());

                    self.free_if_temp(expr_register);
                }
            }

            // build StringBuffer::append() call
            let fct_id = self.sa.known.functions.string_buffer_append;
            let fct_idx = self.gen.add_const_fct(fct_id);
            self.gen.emit_push_register(buffer_register);
            self.gen.emit_push_register(part_register);
            self.gen.emit_invoke_direct_void(fct_idx, expr.pos);
        }

        self.free_temp(part_register);

        // build StringBuffer::toString() call
        let fct_id = self.sa.known.functions.string_buffer_to_string;
        let fct_idx = self.gen.add_const_fct(fct_id);
        self.gen.emit_push_register(buffer_register);
        self.gen
            .emit_invoke_direct(buffer_register, fct_idx, expr.pos);

        buffer_register
    }

    fn visit_expr_path(&mut self, expr: &ExprPathType, dest: DataDest) -> Register {
        let ident_type = self.src.map_idents.get(expr.id).cloned().unwrap();

        match ident_type {
            IdentType::EnumValue(enum_id, type_params, variant_id) => {
                self.emit_new_enum(enum_id, type_params, variant_id, expr.pos, dest)
            }

            _ => unreachable!(),
        }
    }

    fn emit_new_enum(
        &mut self,
        enum_id: EnumId,
        type_params: SourceTypeArray,
        variant_id: usize,
        pos: Position,
        dest: DataDest,
    ) -> Register {
        let xenum = &self.sa.enums[enum_id];
        let xenum = xenum.read();

        if xenum.simple_enumeration {
            let dest = self.ensure_register(dest, BytecodeType::Int32);
            self.gen.emit_const_int32(dest, variant_id as i32);
            dest
        } else {
            let bty = BytecodeType::Enum(enum_id, type_params.clone());
            let dest = self.ensure_register(dest, bty);
            let idx = self
                .gen
                .add_const_enum_variant(enum_id, type_params, variant_id);
            self.gen.emit_new_enum(dest, idx, pos);
            dest
        }
    }

    fn visit_expr_conv(&mut self, expr: &ExprConvType, dest: DataDest) -> Register {
        let object_type = self.ty(expr.object.id());
        let check_type = self.ty(expr.data_type.id());

        if let SourceType::Trait(trait_id, _list_id) = check_type {
            let object = self.visit_expr(&expr.object, DataDest::Alloc);
            let idx =
                self.gen
                    .add_const_trait(trait_id, check_type.type_params(self.sa), object_type);
            let dest = self.ensure_register(dest, BytecodeType::Ptr);
            self.gen.emit_new_trait_object(dest, idx, object, expr.pos);
            self.free_if_temp(object);
            return dest;
        }

        let conv = self.src.map_convs.get(expr.id).clone().unwrap();
        let ty = conv.check_type.clone();
        let cls_id = ty.cls_id().expect("class expected");
        let type_params = ty.type_params(self.sa);
        let cls_idx = self.gen.add_const_cls_types(cls_id, type_params);

        if expr.is {
            let object = self.visit_expr(&expr.object, DataDest::Alloc);
            let result = self.ensure_register(dest, BytecodeType::Bool);
            self.gen.emit_instance_of(result, object, cls_idx);

            result
        } else {
            let dest = match dest {
                DataDest::Effect => DataDest::Alloc,
                DataDest::Reg(reg) => DataDest::Reg(reg),
                DataDest::Alloc => DataDest::Alloc,
            };

            let object = self.visit_expr(&expr.object, dest);
            self.gen.emit_checked_cast(object, cls_idx, expr.pos);
            object
        }
    }

    fn visit_expr_match(&mut self, node: &ExprMatchType, dest: DataDest) -> Register {
        let result_ty = self.ty(node.id);
        let enum_ty = self.ty(node.expr.id());
        let enum_id = enum_ty.enum_id().expect("enum expected");

        let dest = if result_ty.is_unit() {
            None
        } else {
            let result_bc_ty = BytecodeType::from_ty(self.sa, result_ty);
            let dest = self.ensure_register(dest, result_bc_ty);
            Some(dest)
        };

        let end_lbl = self.gen.create_label();

        let expr_reg = self.visit_expr(&node.expr, DataDest::Alloc);

        let variant_reg = self.alloc_temp(BytecodeType::Int32);
        let idx = self
            .gen
            .add_const_enum(enum_id, enum_ty.type_params(self.sa));
        self.gen
            .emit_load_enum_variant(variant_reg, expr_reg, idx, node.pos);

        let mut next_lbl = self.gen.create_label();

        for (idx, case) in node.cases.iter().enumerate() {
            match case.pattern.data {
                MatchPatternData::Underscore => {
                    self.gen.bind_label(next_lbl);

                    if let Some(dest) = dest {
                        self.visit_expr(&case.value, DataDest::Reg(dest));
                    } else {
                        self.visit_expr(&case.value, DataDest::Effect);
                    }

                    self.gen.emit_jump(end_lbl);
                }

                MatchPatternData::Ident(ref ident) => {
                    let variant_id: i32 = {
                        let ident_type = self.src.map_idents.get(case.pattern.id).unwrap();

                        match ident_type {
                            IdentType::EnumValue(_, _, variant_id) => {
                                (*variant_id).try_into().unwrap()
                            }
                            _ => unreachable!(),
                        }
                    };

                    self.gen.bind_label(next_lbl);
                    next_lbl = self.gen.create_label();

                    if idx != node.cases.len() - 1 {
                        let tmp_reg = self.alloc_temp(BytecodeType::Int32);
                        let cmp_reg = self.alloc_temp(BytecodeType::Bool);
                        self.gen.emit_const_int32(tmp_reg, variant_id);
                        self.gen.emit_test_eq_int32(cmp_reg, variant_reg, tmp_reg);
                        self.gen.emit_jump_if_false(cmp_reg, next_lbl);
                        self.free_temp(tmp_reg);
                        self.free_temp(cmp_reg);
                    }

                    self.push_scope();

                    if let Some(ref params) = ident.params {
                        for (subtype_idx, param) in params.iter().enumerate() {
                            if let Some(_) = param.name {
                                let idx = self.gen.add_const_enum_variant(
                                    enum_id,
                                    enum_ty.type_params(self.sa),
                                    variant_id as usize,
                                );

                                let var_id = *self.src.map_vars.get(param.id).unwrap();

                                let ty = self.var_ty(var_id);

                                if !ty.is_unit() {
                                    let ty: BytecodeType = BytecodeType::from_ty(self.sa, ty);
                                    let var_reg = self.alloc_var(ty);

                                    self.var_registers.insert(var_id, var_reg);

                                    self.gen.emit_load_enum_element(
                                        var_reg,
                                        expr_reg,
                                        idx,
                                        subtype_idx as u32,
                                        param.pos,
                                    );
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

                    self.gen.emit_jump(end_lbl);
                }
            }
        }

        self.gen.bind_label(end_lbl);
        self.free_temp(variant_reg);
        self.free_if_temp(expr_reg);

        dest.unwrap_or(Register::invalid())
    }

    fn visit_expr_if(&mut self, expr: &ExprIfType, dest: DataDest) -> Register {
        let ty = self.ty(expr.id);

        if let Some(ref else_block) = expr.else_block {
            let dest = if ty.is_unit() {
                Register::invalid()
            } else {
                self.ensure_register(dest, BytecodeType::from_ty(self.sa, ty))
            };

            let else_lbl = self.gen.create_label();
            let end_lbl = self.gen.create_label();

            let cond_reg = self.visit_expr(&expr.cond, DataDest::Alloc);
            self.gen.emit_jump_if_false(cond_reg, else_lbl);
            self.free_if_temp(cond_reg);

            self.visit_expr(&expr.then_block, DataDest::Reg(dest));

            if !expr_always_returns(&expr.then_block) {
                self.gen.emit_jump(end_lbl);
            }

            self.gen.bind_label(else_lbl);
            self.visit_expr(else_block, DataDest::Reg(dest));
            self.gen.bind_label(end_lbl);

            dest
        } else {
            // Without else-branch there can't be return value
            assert!(ty.is_unit());

            let end_lbl = self.gen.create_label();
            let cond_reg = self.visit_expr(&expr.cond, DataDest::Alloc);
            self.gen.emit_jump_if_false(cond_reg, end_lbl);
            self.free_if_temp(cond_reg);

            self.emit_expr_for_effect(&expr.then_block);

            self.gen.bind_label(end_lbl);
            Register::invalid()
        }
    }

    fn visit_expr_block(&mut self, block: &ExprBlockType, dest: DataDest) -> Register {
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

    fn visit_expr_dot(&mut self, expr: &ExprDotType, dest: DataDest) -> Register {
        let object_ty = self.ty(expr.lhs.id());

        if let Some(tuple_id) = object_ty.tuple_id() {
            return self.visit_expr_dot_tuple(expr, tuple_id, dest);
        }

        if let Some(struct_id) = object_ty.struct_id() {
            let type_params = object_ty.type_params(self.sa);
            return self.visit_expr_dot_struct(expr, struct_id, type_params, dest);
        }

        let (cls_ty, field_id) = {
            let ident_type = self.src.map_idents.get(expr.id).unwrap();

            match ident_type {
                IdentType::Field(ty, field) => (ty.clone(), *field),
                _ => unreachable!(),
            }
        };

        let cls_id = cls_ty.cls_id().expect("class expected");
        let type_params = cls_ty.type_params(self.sa);
        let field_idx = self
            .gen
            .add_const_field_types(cls_id, type_params.clone(), field_id);

        let field_ty = {
            let cls = self.sa.classes.idx(cls_id);
            let cls = cls.read();

            let field = &cls.fields[field_id.to_usize()];
            field.ty.clone()
        };

        let field_ty = specialize_type(self.sa, field_ty, &type_params);

        if field_ty.is_unit() {
            assert!(dest.is_unit());
            let obj = self.visit_expr(&expr.lhs, DataDest::Alloc);
            self.gen.emit_nil_check(obj, expr.pos);
            self.free_if_temp(obj);
            return Register::invalid();
        }

        let field_bc_ty: BytecodeType = BytecodeType::from_ty(self.sa, field_ty);

        let dest = self.ensure_register(dest, field_bc_ty);
        let obj = self.visit_expr(&expr.lhs, DataDest::Alloc);

        self.gen.emit_load_field(dest, obj, field_idx, expr.pos);
        self.free_if_temp(obj);

        dest
    }

    fn visit_expr_dot_struct(
        &mut self,
        expr: &ExprDotType,
        struct_id: StructId,
        type_params: SourceTypeArray,
        dest: DataDest,
    ) -> Register {
        let struct_obj = self.visit_expr(&expr.lhs, DataDest::Alloc);

        let ident_type = self.src.map_idents.get(expr.id).unwrap();

        let field_idx = match ident_type {
            IdentType::StructField(_, field_idx) => *field_idx,
            _ => unreachable!(),
        };

        let xstruct = self.sa.structs.idx(struct_id);
        let xstruct = xstruct.read();
        let field = &xstruct.fields[field_idx.to_usize()];
        let ty = specialize_type(self.sa, field.ty.clone(), &type_params);

        if ty.is_unit() {
            assert!(dest.is_unit());
            self.free_if_temp(struct_obj);
            return Register::invalid();
        }

        let ty: BytecodeType = BytecodeType::from_ty(self.sa, ty);
        let dest = self.ensure_register(dest, ty);
        let const_idx = self
            .gen
            .add_const_struct_field(struct_id, type_params, field_idx);
        self.gen.emit_load_struct_field(dest, struct_obj, const_idx);

        self.free_if_temp(struct_obj);

        dest
    }

    fn visit_expr_dot_tuple(
        &mut self,
        expr: &ExprDotType,
        tuple_id: TupleId,
        dest: DataDest,
    ) -> Register {
        let tuple = self.visit_expr(&expr.lhs, DataDest::Alloc);
        let idx = expr.rhs.to_lit_int().unwrap().value as u32;

        let ty = self.sa.tuples.lock().get_ty(tuple_id, idx as usize);

        if ty.is_unit() {
            assert!(dest.is_unit());
            self.free_if_temp(tuple);
            return Register::invalid();
        }

        let ty: BytecodeType = BytecodeType::from_ty(self.sa, ty);
        let dest = self.ensure_register(dest, ty);
        self.gen.emit_load_tuple_element(dest, tuple, tuple_id, idx);

        self.free_if_temp(tuple);

        dest
    }

    fn visit_expr_assert(&mut self, expr: &ExprCallType, dest: DataDest) {
        assert!(dest.is_unit());
        let assert_reg = self.visit_expr(&*expr.args[0], DataDest::Alloc);
        self.gen.emit_assert(assert_reg, expr.pos);
        self.free_if_temp(assert_reg);
    }

    fn visit_expr_call(&mut self, expr: &ExprCallType, dest: DataDest) -> Register {
        if let Some(info) = self.get_intrinsic(expr.id) {
            if !info.intrinsic.emit_as_function_in_bytecode() {
                return self.visit_expr_call_intrinsic(expr, info, dest);
            }
        }

        let call_type = self.src.map_calls.get(expr.id).unwrap().clone();

        match *call_type {
            CallType::Enum(ref enum_ty, variant_id) => {
                return self.visit_expr_call_enum(expr, enum_ty.clone(), variant_id, dest);
            }

            CallType::Struct(struct_id, ref type_params) => {
                return self.visit_expr_call_struct(expr, struct_id, type_params, dest);
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
            self.ensure_register(dest, BytecodeType::from_ty(self.sa, return_type.clone()))
        };

        // Evaluate object/self argument
        let object_argument = self.emit_call_object_argument(expr, &call_type);

        // Evaluate function arguments
        let arguments = self.emit_call_arguments(expr, &*callee, &call_type, &arg_types);

        // Allocate object for constructor calls
        self.emit_call_allocate(expr.pos, &call_type, &arg_types, object_argument);

        if let Some(obj_reg) = object_argument {
            self.gen.emit_push_register(obj_reg);
        }
        for &arg_reg in &arguments {
            self.gen.emit_push_register(arg_reg);
        }

        // Emit the actual Invoke(Direct|Static|Virtual)XXX instruction
        self.emit_call_inst(
            expr,
            &*callee,
            &call_type,
            return_type,
            expr.pos,
            callee_idx,
            return_reg,
        );

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
        expr: &ExprCallType,
        enum_ty: SourceType,
        variant_id: usize,
        dest: DataDest,
    ) -> Register {
        let mut arguments = Vec::new();

        for arg in &expr.args {
            let ty = self.ty(arg.id());

            if ty.is_unit() {
                self.visit_expr(arg, DataDest::Effect);
            } else {
                arguments.push(self.visit_expr(arg, DataDest::Alloc));
            }
        }

        for &arg_reg in &arguments {
            self.gen.emit_push_register(arg_reg);
        }

        let enum_id = enum_ty.enum_id().expect("enum expected");
        let type_params = enum_ty.type_params(self.sa);

        let idx = self
            .gen
            .add_const_enum_variant(enum_id, type_params, variant_id);
        let bytecode_ty = BytecodeType::from_ty(self.sa, enum_ty);
        let dest_reg = self.ensure_register(dest, bytecode_ty);
        self.gen.emit_new_enum(dest_reg, idx, expr.pos);

        for arg_reg in arguments {
            self.free_if_temp(arg_reg);
        }

        dest_reg
    }

    fn visit_expr_call_struct(
        &mut self,
        expr: &ExprCallType,
        struct_id: StructId,
        type_params: &SourceTypeArray,
        dest: DataDest,
    ) -> Register {
        let mut arguments = Vec::new();

        for arg in &expr.args {
            arguments.push(self.visit_expr(arg, DataDest::Alloc));
        }

        for &arg_reg in &arguments {
            self.gen.emit_push_register(arg_reg);
        }

        let idx = self.gen.add_const_struct(struct_id, type_params.clone());
        let bytecode_ty = BytecodeType::Struct(struct_id, type_params.clone());
        let dest_reg = self.ensure_register(dest, bytecode_ty);
        self.gen.emit_new_struct(dest_reg, idx, expr.pos);

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
            .map(|ty| BytecodeType::from_ty(self.sa, ty.clone()))
            .collect::<Vec<BytecodeType>>();

        (arg_types, arg_bytecode_types, return_type)
    }

    fn emit_call_object_argument(
        &mut self,
        expr: &ExprCallType,
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
        expr: &ExprCallType,
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
        let non_variadic_arguments = if callee.variadic_arguments {
            arg_types.len() - arg_start_offset - 1
        } else {
            arg_types.len()
        };

        // Evaluate non-variadic arguments and track registers.
        for (idx, arg) in expr.args.iter().take(non_variadic_arguments).enumerate() {
            let ty = arg_types[idx + arg_start_offset].clone();

            if ty.is_unit() {
                self.emit_expr_for_effect(arg);
            } else {
                let reg = self.visit_expr(arg, DataDest::Alloc);
                registers.push(reg);
            };
        }

        if callee.variadic_arguments {
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
        expr: &ExprCallType,
        arg_types: &[SourceType],
        non_variadic_arguments: usize,
        dest: DataDest,
    ) -> Register {
        let variadic_arguments = expr.args.len() - non_variadic_arguments;

        // We need array of elements
        let element_ty = arg_types.last().cloned().unwrap();
        let ty = self.sa.known.array_ty(self.sa, element_ty.clone());
        let cls_id = ty.cls_id().expect("class expected");
        let type_params = ty.type_params(self.sa);
        let cls_idx = self.gen.add_const_cls_types(cls_id, type_params);

        // Store length in a register
        let length_reg = self.alloc_temp(BytecodeType::Int64);
        self.gen
            .emit_const_int64(length_reg, variadic_arguments as i64);

        // Allocate array of given length
        let array_reg = self.ensure_register(dest, BytecodeType::Ptr);
        self.gen
            .emit_new_array(array_reg, cls_idx, length_reg, expr.pos);

        if element_ty.is_unit() {
            // Evaluate rest arguments
            for arg in expr.args.iter().skip(non_variadic_arguments) {
                self.visit_expr(arg, DataDest::Effect);
            }
        } else {
            let index_reg = self.alloc_temp(BytecodeType::Int64);

            // Evaluate rest arguments and store them in array
            for (idx, arg) in expr.args.iter().skip(non_variadic_arguments).enumerate() {
                let arg_reg = self.visit_expr(arg, DataDest::Alloc);
                self.gen.emit_const_int64(index_reg, idx as i64);
                self.gen
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
                let type_params = ty.type_params(self.sa);

                let idx = self.gen.add_const_cls_types(cls_id, type_params);
                self.gen
                    .emit_new_object(object_reg.expect("reg missing"), idx, pos);
            }
            _ => {}
        }
    }

    fn emit_call_inst(
        &mut self,
        expr: &ExprCallType,
        fct: &FctDefinition,
        call_type: &CallType,
        return_type: SourceType,
        pos: Position,
        callee_idx: ConstPoolIdx,
        return_reg: Register,
    ) {
        match *call_type {
            CallType::CtorParent(_, _) | CallType::Ctor(_, _) => {
                self.gen.emit_invoke_direct_void(callee_idx, pos);
            }

            CallType::Method(_, _, _) => {
                let is_super_call = expr
                    .object()
                    .map(|object| object.is_super())
                    .unwrap_or(false);

                if is_super_call {
                    self.emit_invoke_direct(return_type, return_reg, callee_idx, pos);
                } else if fct.is_virtual() {
                    self.emit_invoke_virtual(return_type, return_reg, callee_idx, pos);
                } else {
                    self.emit_invoke_direct(return_type, return_reg, callee_idx, pos);
                }
            }
            CallType::ModuleMethod(_, _, _) | CallType::Fct(_, _) => {
                self.emit_invoke_static(return_type, return_reg, callee_idx, pos);
            }
            CallType::Expr(_, _, _) => {
                if fct.is_virtual() {
                    self.emit_invoke_virtual(return_type, return_reg, callee_idx, pos);
                } else {
                    self.emit_invoke_direct(return_type, return_reg, callee_idx, pos);
                }
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
            CallType::Lambda => unreachable!(),
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
                    self.gen.emit_mov(dest_reg, obj_reg);
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
            self.gen.emit_mov(dest, src);
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
            self.gen.emit_invoke_virtual_void(callee_id, pos);
        } else {
            self.gen.emit_invoke_virtual(return_reg, callee_id, pos);
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
            self.gen.emit_invoke_direct_void(callee_id, pos);
        } else {
            self.gen.emit_invoke_direct(return_reg, callee_id, pos);
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
            self.gen.emit_invoke_static_void(callee_id, pos);
        } else {
            self.gen.emit_invoke_static(return_reg, callee_id, pos);
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
            self.gen.emit_invoke_generic_static_void(callee_id, pos);
        } else {
            self.gen
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
            self.gen.emit_invoke_generic_direct_void(callee_id, pos);
        } else {
            self.gen
                .emit_invoke_generic_direct(return_reg, callee_id, pos);
        }
    }

    fn visit_expr_delegation(&mut self, expr: &ExprDelegationType, dest: DataDest) -> Register {
        assert!(dest.is_unit());
        let call_type = self.src.map_calls.get(expr.id).unwrap().clone();
        let fct_id = call_type.fct_id().unwrap();

        let callee_id = fct_id;
        let callee = self.sa.fcts.idx(callee_id);
        let callee = callee.read();

        let callee_idx = self.specialize_call(&callee, &call_type);

        assert!(callee.return_type.is_unit());
        let arg_types = callee
            .params_with_self()
            .iter()
            .map(|arg| {
                BytecodeType::from_ty(
                    self.sa,
                    self.specialize_type_for_call(&call_type, arg.clone()),
                )
            })
            .collect::<Vec<BytecodeType>>();
        let num_args = arg_types.len();

        assert!(num_args > 0);

        assert!(callee.has_self());
        let self_id = self.src.var_self().id;
        let self_reg = self.var_reg(self_id);

        let arg_regs = expr
            .args
            .iter()
            .map(|arg| self.visit_expr(arg, DataDest::Alloc))
            .collect::<Vec<_>>();
        self.gen.emit_push_register(self_reg);
        for &reg in &arg_regs {
            self.gen.emit_push_register(reg);
        }

        match *call_type {
            CallType::CtorParent(_, _) => {
                self.gen.emit_invoke_direct_void(callee_idx, expr.pos);
            }

            _ => unreachable!(),
        }

        for arg in arg_regs {
            self.free_if_temp(arg);
        }

        Register::invalid()
    }

    fn visit_expr_self(&mut self, dest: DataDest) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

        let var_id = self.src.var_self().id;
        let var_reg = self.var_reg(var_id);

        if dest.is_alloc() {
            return var_reg;
        }

        let dest = dest.reg();

        self.emit_mov(dest, var_reg);

        dest
    }

    fn visit_expr_lit_char(&mut self, lit: &ExprLitCharType, dest: DataDest) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

        let dest = self.ensure_register(dest, BytecodeType::Char);

        self.gen.emit_const_char(dest, lit.value);

        dest
    }

    fn visit_expr_lit_int(&mut self, lit: &ExprLitIntType, dest: DataDest, neg: bool) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

        let ty = self.src.ty(lit.id);

        let ty = match ty {
            SourceType::UInt8 => BytecodeType::UInt8,
            SourceType::Int32 => BytecodeType::Int32,
            SourceType::Int64 => BytecodeType::Int64,
            _ => unreachable!(),
        };

        let dest = self.ensure_register(dest, ty.clone());

        if lit.value == 0 {
            match ty {
                BytecodeType::UInt8 => self.gen.emit_const_zero_uint8(dest),
                BytecodeType::Int32 => self.gen.emit_const_zero_int32(dest),
                BytecodeType::Int64 => self.gen.emit_const_zero_int64(dest),
                _ => unreachable!(),
            }
        } else {
            let value = if neg {
                (lit.value as i64).wrapping_neg()
            } else {
                lit.value as i64
            };

            match ty {
                BytecodeType::UInt8 => self.gen.emit_const_uint8(dest, value as u8),
                BytecodeType::Int32 => self.gen.emit_const_int32(dest, value as i32),
                BytecodeType::Int64 => self.gen.emit_const_int64(dest, value),
                _ => unreachable!(),
            }
        }

        dest
    }

    fn visit_expr_lit_float(&mut self, lit: &ExprLitFloatType, dest: DataDest) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

        let ty = self.src.ty(lit.id);

        let ty = match ty {
            SourceType::Float32 => BytecodeType::Float32,
            SourceType::Float64 => BytecodeType::Float64,
            _ => unreachable!(),
        };

        let dest = self.ensure_register(dest, ty.clone());

        if lit.value == 0_f64 {
            match ty {
                BytecodeType::Float32 => self.gen.emit_const_zero_float32(dest),
                BytecodeType::Float64 => self.gen.emit_const_zero_float64(dest),
                _ => unreachable!(),
            }
        } else {
            match ty {
                BytecodeType::Float32 => self.gen.emit_const_float32(dest, lit.value as f32),
                BytecodeType::Float64 => self.gen.emit_const_float64(dest, lit.value),
                _ => unreachable!(),
            }
        }

        dest
    }

    fn visit_expr_lit_string(&mut self, lit: &ExprLitStrType, dest: DataDest) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

        let dest = self.ensure_register(dest, BytecodeType::Ptr);
        self.gen.emit_const_string(dest, lit.value.clone());

        dest
    }

    fn visit_expr_lit_bool(&mut self, lit: &ExprLitBoolType, dest: DataDest) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

        let dest = self.ensure_register(dest, BytecodeType::Bool);

        if lit.value {
            self.gen.emit_const_true(dest);
        } else {
            self.gen.emit_const_false(dest);
        }

        dest
    }

    fn visit_expr_tuple(&mut self, e: &ExprTupleType, dest: DataDest) -> Register {
        if e.values.is_empty() {
            assert!(dest.is_unit());
            return Register::invalid();
        }

        let ty = self.ty(e.id);
        let tuple_id = ty.tuple_id().unwrap();

        let result_ty: BytecodeType = BytecodeType::Tuple(tuple_id);
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
            self.gen.emit_push_register(value);
        }

        self.gen.emit_new_tuple(result, tuple_id, e.pos);

        for arg_reg in values {
            self.free_if_temp(arg_reg);
        }

        result
    }

    fn visit_expr_un(&mut self, expr: &ExprUnType, dest: DataDest) -> Register {
        if expr.op == UnOp::Neg && expr.opnd.is_lit_int() {
            self.visit_expr_lit_int(expr.opnd.to_lit_int().unwrap(), dest, true)
        } else if let Some(intrinsic) = self.get_intrinsic(expr.id) {
            self.emit_intrinsic_un(&expr.opnd, intrinsic, expr.pos, dest)
        } else {
            self.visit_expr_un_method(expr, dest)
        }
    }

    fn visit_expr_un_method(&mut self, expr: &ExprUnType, dest: DataDest) -> Register {
        let opnd = self.visit_expr(&expr.opnd, DataDest::Alloc);

        let call_type = self.src.map_calls.get(expr.id).unwrap();
        let callee_id = self.determine_callee(call_type);

        let callee = self.sa.fcts.idx(callee_id);
        let callee = callee.read();

        // Create FctDefId for this callee
        let callee_idx = self.specialize_call(&callee, &call_type);

        let function_return_type: SourceType =
            self.specialize_type_for_call(call_type, callee.return_type.clone());

        let function_return_type_bc: BytecodeType =
            BytecodeType::from_ty(self.sa, function_return_type.clone());

        let dest = self.ensure_register(dest, function_return_type_bc);

        self.gen.emit_push_register(opnd);
        self.emit_invoke_direct(function_return_type, dest, callee_idx, expr.pos);

        self.free_if_temp(opnd);

        dest
    }

    fn visit_expr_bin(&mut self, expr: &ExprBinType, dest: DataDest) -> Register {
        if expr.op.is_any_assign() {
            self.visit_expr_assign(expr, dest)
        } else if expr.op == BinOp::Cmp(CmpOp::Is) || expr.op == BinOp::Cmp(CmpOp::IsNot) {
            self.emit_bin_is(expr, dest)
        } else if expr.op == BinOp::Or {
            self.emit_bin_or(expr, dest)
        } else if expr.op == BinOp::And {
            self.emit_bin_and(expr, dest)
        } else if let Some(info) = self.get_intrinsic(expr.id) {
            self.emit_intrinsic_bin(&expr.lhs, &expr.rhs, info, Some(expr.op), expr.pos, dest)
        } else {
            self.visit_expr_bin_method(expr, dest)
        }
    }

    fn visit_expr_bin_method(&mut self, expr: &ExprBinType, dest: DataDest) -> Register {
        let lhs = self.visit_expr(&expr.lhs, DataDest::Alloc);
        let rhs = self.visit_expr(&expr.rhs, DataDest::Alloc);

        let call_type = self.src.map_calls.get(expr.id).unwrap();
        let callee_id = self.determine_callee(call_type);

        let callee = self.sa.fcts.idx(callee_id);
        let callee = callee.read();

        // Create FctDefId for this callee
        let callee_idx = self.specialize_call(&callee, &call_type);

        let function_return_type: SourceType =
            self.specialize_type_for_call(call_type, callee.return_type.clone());

        let function_return_type_bc: BytecodeType =
            BytecodeType::from_ty(self.sa, function_return_type.clone());

        let return_type = match expr.op {
            BinOp::Cmp(_) => BytecodeType::Bool,
            _ => function_return_type_bc.clone(),
        };

        let dest = self.ensure_register(dest, return_type.clone());

        let result = if function_return_type_bc == return_type {
            dest
        } else {
            self.alloc_temp(function_return_type_bc)
        };

        self.gen.emit_push_register(lhs);
        self.gen.emit_push_register(rhs);

        self.emit_invoke_direct(function_return_type, result, callee_idx, expr.pos);

        self.free_if_temp(lhs);
        self.free_if_temp(rhs);

        match expr.op {
            BinOp::Cmp(CmpOp::Eq) => assert_eq!(result, dest),
            BinOp::Cmp(CmpOp::Ne) => {
                assert_eq!(result, dest);
                self.gen.emit_not_bool(dest, dest);
            }

            BinOp::Cmp(op) => {
                assert_ne!(result, dest);
                let zero = self.alloc_temp(BytecodeType::Int32);
                self.gen.emit_const_int32(zero, 0);

                match op {
                    CmpOp::Lt => self.gen.emit_test_lt_int32(dest, result, zero),
                    CmpOp::Le => self.gen.emit_test_le_int32(dest, result, zero),
                    CmpOp::Gt => self.gen.emit_test_gt_int32(dest, result, zero),
                    CmpOp::Ge => self.gen.emit_test_ge_int32(dest, result, zero),
                    CmpOp::Eq | CmpOp::Ne | CmpOp::Is | CmpOp::IsNot => unreachable!(),
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
        expr: &ExprCallType,
        info: IntrinsicInfo,
        dest: DataDest,
    ) -> Register {
        let intrinsic = info.intrinsic;
        let call_type = self.src.map_calls.get(expr.id).unwrap().clone();

        if call_type.is_method() {
            let object = expr.object().unwrap();

            match expr.args.len() {
                0 => self.emit_intrinsic_un(object, info, expr.pos, dest),
                1 => self.emit_intrinsic_bin(object, &expr.args[0], info, None, expr.pos, dest),
                2 => {
                    assert_eq!(intrinsic, Intrinsic::ArraySet);
                    self.emit_intrinsic_array_set(
                        expr.object().unwrap(),
                        &expr.args[0],
                        &expr.args[1],
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

                Intrinsic::ArrayGet => {
                    self.emit_intrinsic_bin(&expr.callee, &expr.args[0], info, None, expr.pos, dest)
                }

                Intrinsic::ArrayNewOfSize => self.emit_intrinsic_new_array(expr, dest),

                Intrinsic::ArrayWithValues => {
                    let ty = self.ty(expr.id);
                    assert_eq!(
                        ty.cls_id().expect("class expected"),
                        self.sa.known.classes.array()
                    );
                    let type_params = ty.type_params(self.sa);
                    assert_eq!(1, type_params.len());
                    let element_ty = type_params[0].clone();
                    self.emit_array_with_variadic_arguments(expr, &[element_ty], 0, dest)
                }

                _ => panic!("unimplemented intrinsic {:?}", intrinsic),
            }
        }
    }

    fn emit_intrinsic_new_array(&mut self, expr: &ExprCallType, dest: DataDest) -> Register {
        // We need array of elements
        let element_ty = self.ty(expr.id);
        let cls_id = element_ty.cls_id().expect("class expected");
        let type_params = element_ty.type_params(self.sa);
        let cls_idx = self.gen.add_const_cls_types(cls_id, type_params);

        let array_reg = self.ensure_register(dest, BytecodeType::Ptr);
        let length_reg = self.visit_expr(&expr.args[0], DataDest::Alloc);

        self.gen
            .emit_new_array(array_reg, cls_idx, length_reg, expr.pos);

        self.free_if_temp(length_reg);

        array_reg
    }

    fn emit_bin_is(&mut self, expr: &ExprBinType, dest: DataDest) -> Register {
        if dest.is_effect() {
            self.emit_expr_for_effect(&expr.lhs);
            self.emit_expr_for_effect(&expr.rhs);
            return Register::invalid();
        }

        let dest = self.ensure_register(dest, BytecodeType::Bool);

        let lhs_reg = self.visit_expr(&expr.lhs, DataDest::Alloc);
        let rhs_reg = self.visit_expr(&expr.rhs, DataDest::Alloc);

        self.gen.emit_test_identity(dest, lhs_reg, rhs_reg);

        if expr.op == BinOp::Cmp(CmpOp::IsNot) {
            self.gen.emit_not_bool(dest, dest);
        }

        self.free_if_temp(lhs_reg);
        self.free_if_temp(rhs_reg);

        dest
    }

    fn emit_bin_or(&mut self, expr: &ExprBinType, dest: DataDest) -> Register {
        if dest.is_effect() {
            let end_lbl = self.gen.create_label();
            let dest = self.visit_expr(&expr.lhs, DataDest::Alloc);
            self.gen.emit_jump_if_true(dest, end_lbl);
            self.free_if_temp(dest);

            self.emit_expr_for_effect(&expr.rhs);
            self.gen.bind_label(end_lbl);

            Register::invalid()
        } else {
            let end_lbl = self.gen.create_label();
            let dest = self.ensure_register(dest, BytecodeType::Bool);

            self.visit_expr(&expr.lhs, DataDest::Reg(dest));
            self.gen.emit_jump_if_true(dest, end_lbl);
            self.visit_expr(&expr.rhs, DataDest::Reg(dest));
            self.gen.bind_label(end_lbl);

            dest
        }
    }

    fn emit_bin_and(&mut self, expr: &ExprBinType, dest: DataDest) -> Register {
        if dest.is_effect() {
            let end_lbl = self.gen.create_label();
            let dest = self.visit_expr(&expr.lhs, DataDest::Alloc);
            self.gen.emit_jump_if_false(dest, end_lbl);
            self.free_if_temp(dest);

            self.emit_expr_for_effect(&expr.rhs);
            self.gen.bind_label(end_lbl);

            Register::invalid()
        } else {
            let end_lbl = self.gen.create_label();
            let dest = self.ensure_register(dest, BytecodeType::Bool);

            self.visit_expr(&expr.lhs, DataDest::Reg(dest));
            self.gen.emit_jump_if_false(dest, end_lbl);
            self.visit_expr(&expr.rhs, DataDest::Reg(dest));
            self.gen.bind_label(end_lbl);

            dest
        }
    }

    fn emit_intrinsic_array_set(
        &mut self,
        arr: &Expr,
        idx: &Expr,
        src: &Expr,
        pos: Position,
        dest: DataDest,
    ) -> Register {
        assert!(dest.is_unit());

        let ty = self.ty(arr.id());
        let ty = ty.type_params(self.sa);
        let ty = ty[0].clone();
        let ty: Option<BytecodeType> = if ty.is_unit() {
            None
        } else {
            Some(BytecodeType::from_ty(self.sa, ty))
        };

        let arr = self.visit_expr(arr, DataDest::Alloc);
        let idx = self.visit_expr(idx, DataDest::Alloc);
        let src = self.visit_expr(src, DataDest::Alloc);

        if ty.is_none() {
            self.gen.emit_array_bound_check(arr, idx, pos);

            self.free_if_temp(arr);
            self.free_if_temp(idx);
            self.free_if_temp(src);

            return Register::invalid();
        }

        self.gen.emit_store_array(src, arr, idx, pos);

        self.free_if_temp(arr);
        self.free_if_temp(idx);
        self.free_if_temp(src);

        Register::invalid()
    }

    fn emit_intrinsic_un(
        &mut self,
        opnd: &Expr,
        info: IntrinsicInfo,
        pos: Position,
        dest: DataDest,
    ) -> Register {
        let intrinsic = info.intrinsic;

        if dest.is_effect() {
            match intrinsic {
                Intrinsic::ArrayLen | Intrinsic::StrLen => {
                    let src = self.visit_expr(opnd, DataDest::Alloc);
                    self.gen.emit_nil_check(src, pos);
                    self.free_if_temp(src);
                    return Register::invalid();
                }

                _ => {}
            }

            self.emit_expr_for_effect(opnd);
            return Register::invalid();
        }

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
            Intrinsic::ArrayLen | Intrinsic::StrLen => {
                self.gen.emit_array_length(dest, src, pos);
            }
            Intrinsic::Int32Neg => self.gen.emit_neg_int32(dest, src),
            Intrinsic::Int64Neg => self.gen.emit_neg_int64(dest, src),
            Intrinsic::Float32Neg => self.gen.emit_neg_float32(dest, src),
            Intrinsic::Float64Neg => self.gen.emit_neg_float64(dest, src),
            Intrinsic::BoolNot => self.gen.emit_not_bool(dest, src),
            Intrinsic::Int32Not => self.gen.emit_not_int32(dest, src),
            Intrinsic::Int64Not => self.gen.emit_not_int64(dest, src),
            Intrinsic::ByteToChar => self.gen.emit_extend_byte_to_char(dest, src),
            Intrinsic::ByteToInt32 => self.gen.emit_extend_byte_to_int32(dest, src),
            Intrinsic::ByteToInt64 => self.gen.emit_extend_byte_to_int64(dest, src),
            Intrinsic::Int32ToInt64 => self.gen.emit_extend_int32_to_int64(dest, src),
            Intrinsic::CharToInt32 => self.gen.emit_cast_char_to_int32(dest, src),
            Intrinsic::CharToInt64 => self.gen.emit_extend_char_to_int64(dest, src),
            Intrinsic::Int32ToByte => self.gen.emit_cast_int32_to_uint8(dest, src),
            Intrinsic::Int32ToChar => self.gen.emit_cast_int32_to_char(dest, src),
            Intrinsic::Int32ToInt32 => self.gen.emit_mov(dest, src),
            Intrinsic::Int64ToByte => self.gen.emit_cast_int64_to_uint8(dest, src),
            Intrinsic::Int64ToChar => self.gen.emit_cast_int64_to_char(dest, src),
            Intrinsic::Int64ToInt32 => self.gen.emit_cast_int64_to_int32(dest, src),
            Intrinsic::Float32IsNan => self.gen.emit_test_ne_float32(dest, src, src),
            Intrinsic::Float64IsNan => self.gen.emit_test_ne_float64(dest, src, src),
            _ => {
                panic!("unimplemented intrinsic {:?}", intrinsic);
            }
        }

        self.free_if_temp(src);

        dest
    }

    fn emit_intrinsic_bin(
        &mut self,
        lhs: &Expr,
        rhs: &Expr,
        info: IntrinsicInfo,
        op: Option<BinOp>,
        pos: Position,
        dest: DataDest,
    ) -> Register {
        let intrinsic = info.intrinsic;

        match intrinsic {
            Intrinsic::ArrayGet | Intrinsic::StrGet => {
                let ty = self.ty(lhs.id());
                let ty: Option<BytecodeType> =
                    if ty.cls_id() == Some(self.sa.known.classes.string()) {
                        Some(BytecodeType::UInt8)
                    } else {
                        let ty = ty.type_params(self.sa);
                        let ty = ty[0].clone();

                        if ty.is_unit() {
                            assert!(dest.is_unit());
                            None
                        } else {
                            Some(BytecodeType::from_ty(self.sa, ty))
                        }
                    };

                let dest = if let Some(ref ty) = ty {
                    Some(self.ensure_register(dest, ty.clone()))
                } else {
                    None
                };

                let arr = self.visit_expr(lhs, DataDest::Alloc);
                let idx = self.visit_expr(rhs, DataDest::Alloc);

                if ty.is_none() {
                    self.gen.emit_array_bound_check(arr, idx, pos);

                    self.free_if_temp(arr);
                    self.free_if_temp(idx);

                    return Register::invalid();
                }

                let dest = dest.unwrap();

                self.gen.emit_load_array(dest, arr, idx, pos);

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
                Some(BinOp::Cmp(CmpOp::Eq)) => self.gen.emit_test_eq_bool(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Ne)) => self.gen.emit_test_ne_bool(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::ByteEq => match op {
                Some(BinOp::Cmp(CmpOp::Eq)) => self.gen.emit_test_eq_uint8(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Ne)) => self.gen.emit_test_ne_uint8(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::ByteCmp => match op {
                Some(BinOp::Cmp(CmpOp::Lt)) => self.gen.emit_test_lt_uint8(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Le)) => self.gen.emit_test_le_uint8(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Gt)) => self.gen.emit_test_gt_uint8(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Ge)) => self.gen.emit_test_ge_uint8(dest, lhs_reg, rhs_reg),
                None => {
                    let fct_id = info.fct_id.expect("fct_id missing");
                    let ty = self.ty(lhs.id());
                    let type_params = ty.type_params(self.sa);
                    let idx = self.gen.add_const_fct_types(fct_id, type_params);
                    self.gen.emit_push_register(lhs_reg);
                    self.gen.emit_push_register(rhs_reg);
                    self.gen.emit_invoke_direct(dest, idx, pos);
                }
                _ => unreachable!(),
            },
            Intrinsic::CharEq => match op {
                Some(BinOp::Cmp(CmpOp::Eq)) => self.gen.emit_test_eq_char(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Ne)) => self.gen.emit_test_ne_char(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::CharCmp => match op {
                Some(BinOp::Cmp(CmpOp::Lt)) => self.gen.emit_test_lt_char(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Le)) => self.gen.emit_test_le_char(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Gt)) => self.gen.emit_test_gt_char(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Ge)) => self.gen.emit_test_ge_char(dest, lhs_reg, rhs_reg),
                None => {
                    let fct_id = info.fct_id.expect("fct_id missing");
                    let ty = self.ty(lhs.id());
                    let type_params = ty.type_params(self.sa);
                    let idx = self.gen.add_const_fct_types(fct_id, type_params);
                    self.gen.emit_push_register(lhs_reg);
                    self.gen.emit_push_register(rhs_reg);
                    self.gen.emit_invoke_direct(dest, idx, pos);
                }
                _ => unreachable!(),
            },
            Intrinsic::EnumEq => self.gen.emit_test_eq_enum(dest, lhs_reg, rhs_reg),
            Intrinsic::EnumNe => self.gen.emit_test_ne_enum(dest, lhs_reg, rhs_reg),
            Intrinsic::Int32Eq => match op {
                Some(BinOp::Cmp(CmpOp::Eq)) => self.gen.emit_test_eq_int32(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Ne)) => self.gen.emit_test_ne_int32(dest, lhs_reg, rhs_reg),
                None => self.gen.emit_test_eq_int32(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::Int32Cmp => match op {
                Some(BinOp::Cmp(CmpOp::Lt)) => self.gen.emit_test_lt_int32(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Le)) => self.gen.emit_test_le_int32(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Gt)) => self.gen.emit_test_gt_int32(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Ge)) => self.gen.emit_test_ge_int32(dest, lhs_reg, rhs_reg),
                Some(_) => unreachable!(),
                None => {
                    let fct_id = info.fct_id.expect("fct_id missing");
                    let ty = self.ty(lhs.id());
                    let type_params = ty.type_params(self.sa);
                    let idx = self.gen.add_const_fct_types(fct_id, type_params);
                    self.gen.emit_push_register(lhs_reg);
                    self.gen.emit_push_register(rhs_reg);
                    self.gen.emit_invoke_direct(dest, idx, pos);
                }
            },
            Intrinsic::Int64Eq => match op {
                Some(BinOp::Cmp(CmpOp::Eq)) => self.gen.emit_test_eq_int64(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Ne)) => self.gen.emit_test_ne_int64(dest, lhs_reg, rhs_reg),
                None => self.gen.emit_test_eq_int64(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::Int64Cmp => match op {
                Some(BinOp::Cmp(CmpOp::Lt)) => self.gen.emit_test_lt_int64(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Le)) => self.gen.emit_test_le_int64(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Gt)) => self.gen.emit_test_gt_int64(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Ge)) => self.gen.emit_test_ge_int64(dest, lhs_reg, rhs_reg),
                Some(_) => unreachable!(),
                None => {
                    let fct_id = info.fct_id.expect("fct_id missing");
                    let ty = self.ty(lhs.id());
                    let type_params = ty.type_params(self.sa);
                    let idx = self.gen.add_const_fct_types(fct_id, type_params);
                    self.gen.emit_push_register(lhs_reg);
                    self.gen.emit_push_register(rhs_reg);
                    self.gen.emit_invoke_direct(dest, idx, pos);
                }
            },
            Intrinsic::Float32Eq => match op {
                Some(BinOp::Cmp(CmpOp::Eq)) => {
                    self.gen.emit_test_eq_float32(dest, lhs_reg, rhs_reg)
                }
                Some(BinOp::Cmp(CmpOp::Ne)) => {
                    self.gen.emit_test_ne_float32(dest, lhs_reg, rhs_reg)
                }
                None => self.gen.emit_test_eq_float32(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::Float32Cmp => match op {
                Some(BinOp::Cmp(CmpOp::Lt)) => {
                    self.gen.emit_test_lt_float32(dest, lhs_reg, rhs_reg)
                }
                Some(BinOp::Cmp(CmpOp::Le)) => {
                    self.gen.emit_test_le_float32(dest, lhs_reg, rhs_reg)
                }
                Some(BinOp::Cmp(CmpOp::Gt)) => {
                    self.gen.emit_test_gt_float32(dest, lhs_reg, rhs_reg)
                }
                Some(BinOp::Cmp(CmpOp::Ge)) => {
                    self.gen.emit_test_ge_float32(dest, lhs_reg, rhs_reg)
                }
                None => {
                    let fct_id = info.fct_id.expect("fct_id missing");
                    let ty = self.ty(lhs.id());
                    let type_params = ty.type_params(self.sa);
                    let idx = self.gen.add_const_fct_types(fct_id, type_params);
                    self.gen.emit_push_register(lhs_reg);
                    self.gen.emit_push_register(rhs_reg);
                    self.gen.emit_invoke_direct(dest, idx, pos)
                }
                _ => unreachable!(),
            },
            Intrinsic::Float64Eq => match op {
                Some(BinOp::Cmp(CmpOp::Eq)) => {
                    self.gen.emit_test_eq_float64(dest, lhs_reg, rhs_reg)
                }
                Some(BinOp::Cmp(CmpOp::Ne)) => {
                    self.gen.emit_test_ne_float64(dest, lhs_reg, rhs_reg)
                }
                None => self.gen.emit_test_eq_float64(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::Float64Cmp => match op {
                Some(BinOp::Cmp(CmpOp::Lt)) => {
                    self.gen.emit_test_lt_float64(dest, lhs_reg, rhs_reg)
                }
                Some(BinOp::Cmp(CmpOp::Le)) => {
                    self.gen.emit_test_le_float64(dest, lhs_reg, rhs_reg)
                }
                Some(BinOp::Cmp(CmpOp::Gt)) => {
                    self.gen.emit_test_gt_float64(dest, lhs_reg, rhs_reg)
                }
                Some(BinOp::Cmp(CmpOp::Ge)) => {
                    self.gen.emit_test_ge_float64(dest, lhs_reg, rhs_reg)
                }
                None => {
                    let fct_id = info.fct_id.expect("fct_id missing");
                    let ty = self.ty(lhs.id());
                    let type_params = ty.type_params(self.sa);
                    let idx = self.gen.add_const_fct_types(fct_id, type_params);
                    self.gen.emit_push_register(lhs_reg);
                    self.gen.emit_push_register(rhs_reg);
                    self.gen.emit_invoke_direct(dest, idx, pos);
                }

                _ => unreachable!(),
            },
            Intrinsic::Int32Add => self.gen.emit_add_int32(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Int32Sub => self.gen.emit_sub_int32(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Int32Mul => self.gen.emit_mul_int32(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Int32Div => self.gen.emit_div_int32(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Int32Mod => self.gen.emit_mod_int32(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Int32Or => self.gen.emit_or_int32(dest, lhs_reg, rhs_reg),
            Intrinsic::Int32And => self.gen.emit_and_int32(dest, lhs_reg, rhs_reg),
            Intrinsic::Int32Xor => self.gen.emit_xor_int32(dest, lhs_reg, rhs_reg),
            Intrinsic::Int32Shl => self.gen.emit_shl_int32(dest, lhs_reg, rhs_reg),
            Intrinsic::Int32Shr => self.gen.emit_shr_int32(dest, lhs_reg, rhs_reg),
            Intrinsic::Int32Sar => self.gen.emit_sar_int32(dest, lhs_reg, rhs_reg),
            Intrinsic::Int32RotateLeft => self.gen.emit_rol_int32(dest, lhs_reg, rhs_reg),
            Intrinsic::Int32RotateRight => self.gen.emit_ror_int32(dest, lhs_reg, rhs_reg),
            Intrinsic::Int64Add => self.gen.emit_add_int64(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Int64Sub => self.gen.emit_sub_int64(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Int64Mul => self.gen.emit_mul_int64(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Int64Div => self.gen.emit_div_int64(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Int64Mod => self.gen.emit_mod_int64(dest, lhs_reg, rhs_reg, pos),
            Intrinsic::Int64Or => self.gen.emit_or_int64(dest, lhs_reg, rhs_reg),
            Intrinsic::Int64And => self.gen.emit_and_int64(dest, lhs_reg, rhs_reg),
            Intrinsic::Int64Xor => self.gen.emit_xor_int64(dest, lhs_reg, rhs_reg),
            Intrinsic::Int64Shl => self.gen.emit_shl_int64(dest, lhs_reg, rhs_reg),
            Intrinsic::Int64Shr => self.gen.emit_shr_int64(dest, lhs_reg, rhs_reg),
            Intrinsic::Int64Sar => self.gen.emit_sar_int64(dest, lhs_reg, rhs_reg),
            Intrinsic::Int64RotateLeft => self.gen.emit_rol_int64(dest, lhs_reg, rhs_reg),
            Intrinsic::Int64RotateRight => self.gen.emit_ror_int64(dest, lhs_reg, rhs_reg),
            Intrinsic::Float32Add => self.gen.emit_add_float32(dest, lhs_reg, rhs_reg),
            Intrinsic::Float32Sub => self.gen.emit_sub_float32(dest, lhs_reg, rhs_reg),
            Intrinsic::Float32Mul => self.gen.emit_mul_float32(dest, lhs_reg, rhs_reg),
            Intrinsic::Float32Div => self.gen.emit_div_float32(dest, lhs_reg, rhs_reg),
            Intrinsic::Float64Add => self.gen.emit_add_float64(dest, lhs_reg, rhs_reg),
            Intrinsic::Float64Sub => self.gen.emit_sub_float64(dest, lhs_reg, rhs_reg),
            Intrinsic::Float64Mul => self.gen.emit_mul_float64(dest, lhs_reg, rhs_reg),
            Intrinsic::Float64Div => self.gen.emit_div_float64(dest, lhs_reg, rhs_reg),
            _ => unimplemented!(),
        }

        self.free_if_temp(lhs_reg);
        self.free_if_temp(rhs_reg);

        dest
    }

    fn visit_expr_assign(&mut self, expr: &ExprBinType, dest: DataDest) -> Register {
        assert!(dest.is_unit());

        if expr.lhs.is_ident() {
            let ident_type = self.src.map_idents.get(expr.lhs.id()).unwrap();
            match ident_type {
                &IdentType::Var(var_id) => self.visit_expr_assign_var(expr, var_id),
                &IdentType::Global(gid) => self.visit_expr_assign_global(expr, gid),
                _ => unreachable!(),
            }
        } else {
            match *expr.lhs {
                Expr::Dot(ref dot) => self.visit_expr_assign_dot(expr, dot),
                Expr::Call(ref call) => self.visit_expr_assign_call(expr, call),
                _ => unreachable!(),
            };
        }

        Register::invalid()
    }

    fn visit_expr_assign_call(&mut self, expr: &ExprBinType, call_expr: &ExprCallType) {
        let object = &call_expr.callee;
        let index = &call_expr.args[0];
        let value = &expr.rhs;

        if let Some(info) = self.get_intrinsic(expr.id) {
            match info.intrinsic {
                Intrinsic::ArraySet => {
                    self.emit_intrinsic_array_set(object, index, value, expr.pos, DataDest::Effect);
                }
                _ => panic!("unexpected intrinsic {:?}", info.intrinsic),
            }
        } else {
            let call_type = self.src.map_calls.get(expr.id).unwrap();
            let fct_id = call_type.fct_id().unwrap();

            let obj_reg = self.visit_expr(object, DataDest::Alloc);
            let idx_reg = self.visit_expr(index, DataDest::Alloc);
            let val_reg = self.visit_expr(value, DataDest::Alloc);

            let obj_ty = self.ty(object.id());

            self.gen.emit_push_register(obj_reg);
            self.gen.emit_push_register(idx_reg);
            self.gen.emit_push_register(val_reg);

            let type_params = obj_ty.type_params(self.sa);

            let callee_idx = self.gen.add_const_fct_types(fct_id, type_params);
            self.gen.emit_invoke_direct_void(callee_idx, expr.pos);

            self.free_if_temp(obj_reg);
            self.free_if_temp(idx_reg);
            self.free_if_temp(val_reg);
        }
    }

    fn visit_expr_assign_dot(&mut self, expr: &ExprBinType, dot: &ExprDotType) {
        let (cls_ty, field_id) = {
            let ident_type = self.src.map_idents.get(dot.id).cloned().unwrap();
            match ident_type {
                IdentType::Field(class, field) => (class, field),
                _ => unreachable!(),
            }
        };

        let cls_id = cls_ty.cls_id().expect("class expected");
        let type_params = cls_ty.type_params(self.sa);
        let field_idx = self
            .gen
            .add_const_field_types(cls_id, type_params.clone(), field_id);

        let cls = self.sa.classes.idx(cls_id);
        let cls = cls.read();
        let field = &cls.fields[field_id.to_usize()];
        let field_ty = field.ty.clone();
        let field_ty = specialize_type(self.sa, field_ty, &type_params);

        let field_ty: Option<BytecodeType> = if field_ty.is_unit() {
            None
        } else {
            Some(BytecodeType::from_ty(self.sa, field_ty))
        };

        let obj = self.visit_expr(&dot.lhs, DataDest::Alloc);
        let src = self.visit_expr(&expr.rhs, DataDest::Alloc);

        if field_ty.is_none() {
            self.gen.emit_nil_check(obj, expr.pos);
            return;
        }

        self.gen.emit_store_field(src, obj, field_idx, expr.pos);

        self.free_if_temp(obj);
        self.free_if_temp(src);
    }

    fn visit_expr_assign_var(&mut self, expr: &ExprBinType, var_id: VarId) {
        let ty = self.var_ty(var_id);

        let dest = if ty.is_unit() {
            DataDest::Effect
        } else {
            let var_reg = self.var_reg(var_id);
            DataDest::Reg(var_reg)
        };

        self.visit_expr(&expr.rhs, dest);
    }

    fn visit_expr_assign_global(&mut self, expr: &ExprBinType, gid: GlobalDefinitionId) {
        let glob = self.sa.globals.idx(gid);
        let glob = glob.read();

        let dest = if glob.ty.is_unit() {
            DataDest::Effect
        } else {
            DataDest::Alloc
        };

        let src = self.visit_expr(&expr.rhs, dest);

        if !glob.ty.is_unit() {
            self.gen.emit_store_global(src, gid);
        }

        self.free_if_temp(src);
    }

    fn visit_expr_ident(&mut self, ident: &ExprIdentType, dest: DataDest) -> Register {
        let ident_type = self.src.map_idents.get(ident.id).unwrap();

        match ident_type {
            &IdentType::Var(varid) => self.visit_expr_ident_var(varid, dest),
            &IdentType::Global(gid) => self.visit_expr_ident_global(gid, dest),
            &IdentType::Const(cid) => self.visit_expr_ident_const(cid, dest),
            &IdentType::EnumValue(enum_id, ref type_params, variant_id) => {
                self.emit_new_enum(enum_id, type_params.clone(), variant_id, ident.pos, dest)
            }
            &IdentType::Module(_) => unimplemented!(),

            &IdentType::Field(_, _) => unreachable!(),
            &IdentType::Struct(_) => unreachable!(),
            &IdentType::StructField(_, _) => unreachable!(),

            &IdentType::Fct(_, _) => unreachable!(),
            &IdentType::Class(_, _) => unreachable!(),
        }
    }

    fn visit_expr_ident_const(&mut self, const_id: ConstId, dest: DataDest) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

        let xconst = self.sa.consts.idx(const_id);
        let xconst = xconst.read();
        let ty = xconst.ty.clone();

        let bytecode_ty = BytecodeType::from_ty(self.sa, ty.clone());
        let dest = self.ensure_register(dest, bytecode_ty);

        match ty {
            SourceType::Bool => {
                if xconst.value.to_bool() {
                    self.gen.emit_const_true(dest);
                } else {
                    self.gen.emit_const_false(dest);
                }
            }

            SourceType::Char => {
                self.gen.emit_const_char(dest, xconst.value.to_char());
            }

            SourceType::UInt8 => {
                self.gen.emit_const_uint8(dest, xconst.value.to_int() as u8);
            }

            SourceType::Int32 => {
                self.gen
                    .emit_const_int32(dest, xconst.value.to_int() as i32);
            }

            SourceType::Int64 => {
                self.gen.emit_const_int64(dest, xconst.value.to_int());
            }

            SourceType::Float32 => {
                self.gen
                    .emit_const_float32(dest, xconst.value.to_float() as f32);
            }

            SourceType::Float64 => {
                self.gen.emit_const_float64(dest, xconst.value.to_float());
            }

            _ => unimplemented!(),
        }

        dest
    }

    fn visit_expr_ident_global(&mut self, gid: GlobalDefinitionId, dest: DataDest) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

        let glob = self.sa.globals.idx(gid);
        let glob = glob.read();

        if glob.ty.is_unit() {
            assert!(dest.is_alloc());
            return Register::invalid();
        }

        let ty: BytecodeType = BytecodeType::from_ty(self.sa, glob.ty.clone());
        let dest = self.ensure_register(dest, ty);

        self.gen.emit_load_global(dest, gid);

        dest
    }

    fn visit_expr_ident_var(&mut self, var_id: VarId, dest: DataDest) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

        let ty = self.var_ty(var_id);

        if ty.is_unit() {
            assert!(dest.is_alloc());
            return Register::invalid();
        }

        let var_reg = self.var_reg(var_id);

        if dest.is_alloc() {
            return var_reg;
        }

        let dest = dest.reg();
        self.emit_mov(dest, var_reg);

        dest
    }

    fn var_reg(&self, var_id: VarId) -> Register {
        *self
            .var_registers
            .get(&var_id)
            .expect("no register for var found")
    }

    fn ensure_register(&mut self, dest: DataDest, ty: BytecodeType) -> Register {
        match dest {
            DataDest::Effect | DataDest::Alloc => self.alloc_temp(ty),
            DataDest::Reg(reg) => reg,
        }
    }

    fn determine_call_type_params(&self, call_type: &CallType) -> SourceTypeArray {
        match call_type {
            CallType::CtorParent(ty, _) | CallType::Ctor(ty, _) => ty.type_params(self.sa),

            CallType::Method(_, _, ref type_params) => type_params.clone(),

            CallType::ModuleMethod(ty, _, ref fct_type_params) => {
                let cls_type_params = ty.type_params(self.sa);
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
            CallType::Lambda => unreachable!(),
        }
    }

    fn specialize_call(&mut self, fct: &FctDefinition, call_type: &CallType) -> ConstPoolIdx {
        let type_params = self.determine_call_type_params(call_type);
        assert_eq!(fct.type_params.len(), type_params.len());

        match *call_type {
            CallType::GenericStaticMethod(id, _, _) | CallType::GenericMethod(id, _, _) => {
                self.gen.add_const_generic(id, fct.id, type_params)
            }
            _ => self.gen.add_const_fct_types(fct.id, type_params),
        }
    }

    fn specialize_type_for_call(&self, call_type: &CallType, ty: SourceType) -> SourceType {
        match call_type {
            CallType::Fct(_, ref type_params) => specialize_type(self.sa, ty, type_params),

            CallType::Method(_, _, ref type_params) => specialize_type(self.sa, ty, type_params),

            CallType::ModuleMethod(cls_ty, _, ref fct_type_params) => {
                let cls_type_params = cls_ty.type_params(self.sa);
                let type_params = cls_type_params.connect(fct_type_params);
                specialize_type(self.sa, ty, &type_params)
            }

            CallType::CtorParent(cls_ty, _) | CallType::Ctor(cls_ty, _) => {
                let cls_type_params = cls_ty.type_params(self.sa);
                specialize_type(self.sa, ty, &cls_type_params)
            }

            CallType::Expr(_, _, ref type_params) => specialize_type(self.sa, ty, type_params),

            CallType::TraitObjectMethod(trait_ty, _) => {
                let container_type_params = trait_ty.type_params(self.sa);
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
            CallType::Lambda => unreachable!(),
        }
    }

    fn ty(&self, id: NodeId) -> SourceType {
        self.src.ty(id)
    }

    fn var_ty(&self, id: VarId) -> SourceType {
        self.src.vars[id].ty.clone()
    }

    fn get_intrinsic(&self, id: NodeId) -> Option<IntrinsicInfo> {
        let call_type = self.src.map_calls.get(id).expect("missing CallType");

        if let Some(intrinsic) = call_type.to_intrinsic() {
            return Some(intrinsic.into());
        }

        let fid = if let Some(fct_id) = call_type.fct_id() {
            fct_id
        } else {
            return None;
        };

        // the function we compile right now is never an intrinsic
        if self.fct.id == fid {
            return None;
        }

        let fct = self.sa.fcts.idx(fid);
        let fct = fct.read();

        if let Some(intrinsic) = fct.intrinsic {
            return Some(IntrinsicInfo::with_fct(intrinsic, fid));
        }

        None
    }

    fn push_scope(&mut self) {
        self.gen.push_scope();
    }

    fn pop_scope(&mut self) {
        self.gen.pop_scope();
    }

    fn alloc_var(&mut self, ty: BytecodeType) -> Register {
        self.gen.alloc_var(ty)
    }

    fn alloc_temp(&mut self, ty: BytecodeType) -> Register {
        self.gen.alloc_temp(ty)
    }

    fn free_if_temp(&mut self, reg: Register) {
        self.gen.free_if_temp(reg);
    }

    fn free_temp(&mut self, reg: Register) {
        self.gen.free_temp(reg);
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
