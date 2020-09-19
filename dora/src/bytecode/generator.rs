use dora_parser::lexer::position::Position;
use std::collections::HashMap;

use dora_parser::ast::Expr::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::*;

use crate::bytecode::{BytecodeBuilder, BytecodeFunction, BytecodeType, Label, Register};
use crate::semck::specialize::{specialize_class_ty, specialize_type};
use crate::semck::{expr_always_returns, expr_block_always_returns};
use crate::size::InstanceSize;
use crate::ty::{BuiltinType, TypeList};
use crate::vm::{
    CallType, ConstId, Fct, FctDef, FctDefId, FctId, FctKind, FctSrc, GlobalId, IdentType,
    Intrinsic, TraitId, TupleId, VarId, VM,
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

pub fn generate_fct<'ast>(vm: &VM<'ast>, id: FctId, type_params: &TypeList) -> BytecodeFunction {
    let fct = vm.fcts.idx(id);
    let fct = fct.read();
    let src = fct.src();
    let src = src.read();

    generate(vm, &fct, &src, type_params)
}

pub fn generate<'ast>(
    vm: &VM<'ast>,
    fct: &Fct<'ast>,
    src: &FctSrc,
    type_params: &TypeList,
) -> BytecodeFunction {
    let ast_bytecode_generator = AstBytecodeGen {
        vm,
        fct,
        ast: fct.ast,
        src,

        type_params,

        gen: BytecodeBuilder::new(&vm.args),
        loops: Vec::new(),
        var_registers: HashMap::new(),
        generic_mode: false,
    };
    ast_bytecode_generator.generate()
}

pub fn generate_generic_fct<'ast>(vm: &VM<'ast>, id: FctId) -> BytecodeFunction {
    let fct = vm.fcts.idx(id);
    let fct = fct.read();
    let src = fct.src();
    let src = src.read();

    generate_generic(vm, &fct, &src)
}

pub fn generate_generic<'ast>(vm: &VM<'ast>, fct: &Fct<'ast>, src: &FctSrc) -> BytecodeFunction {
    let ast_bytecode_generator = AstBytecodeGen {
        vm,
        fct,
        ast: fct.ast,
        src,

        type_params: &TypeList::empty(),

        gen: BytecodeBuilder::new(&vm.args),
        loops: Vec::new(),
        var_registers: HashMap::new(),
        generic_mode: true,
    };
    ast_bytecode_generator.generate()
}

struct AstBytecodeGen<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    fct: &'a Fct<'ast>,
    ast: &'ast Function,
    src: &'a FctSrc,

    type_params: &'a TypeList,

    gen: BytecodeBuilder,
    loops: Vec<LoopLabels>,
    var_registers: HashMap<VarId, Register>,

    // true when generic bytecode should be generated
    generic_mode: bool,
}

impl<'a, 'ast> AstBytecodeGen<'a, 'ast> {
    fn generate(mut self) -> BytecodeFunction {
        let mut arguments = 0;
        self.push_scope();

        if self.fct.has_self() {
            let var_self = self.src.var_self();
            let var_ty = self.specialize_type(var_self.ty);
            let var_id = var_self.id;
            let reg = self.alloc_var(var_ty.into());
            self.var_registers.insert(var_id, reg);
            arguments += 1;
        }

        for param in &self.ast.params {
            let var_id = *self.src.map_vars.get(param.id).unwrap();
            let ty = self.var_ty(var_id);

            if ty.is_unit() {
                // no register needed for unit
            } else {
                let ty: BytecodeType = ty.into();
                let reg = self.alloc_var(ty);
                self.var_registers.insert(var_id, reg);
                arguments += 1;
            }
        }

        self.gen.set_arguments(arguments);

        if let Some(ref block) = self.ast.block {
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
        self.gen.generate(self.vm)
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        match *stmt {
            StmtReturn(ref ret) => self.visit_stmt_return(ret),
            StmtBreak(ref stmt) => self.visit_stmt_break(stmt),
            StmtContinue(ref stmt) => self.visit_stmt_continue(stmt),
            StmtExpr(ref expr) => self.visit_stmt_expr(expr),
            StmtLet(ref stmt) => self.visit_stmt_let(stmt),
            StmtWhile(ref stmt) => self.visit_stmt_while(stmt),
            StmtFor(ref stmt) => self.visit_stmt_for(stmt),
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
        let lbl_end = self.gen.create_label();

        // if idx >= length then goto end
        let tmp_reg = self.alloc_temp(BytecodeType::Bool);
        self.gen.emit_test_lt_int64(tmp_reg, index_reg, length_reg);
        self.gen.emit_jump_if_false(tmp_reg, lbl_end);
        self.free_temp(tmp_reg);

        // load current array element
        let ty = self.ty(stmt.expr.id());
        self.visit_stmt_for_pattern_assign_array(&stmt.pattern, array_reg, index_reg, ty);

        self.loops.push(LoopLabels::new(lbl_cond, lbl_end));
        self.visit_stmt(&stmt.block);
        self.loops.pop().unwrap();

        // increment index
        let tmp_reg = self.alloc_temp(BytecodeType::Int64);
        self.gen.emit_const_int64(tmp_reg, 1);
        self.gen.emit_add_int64(index_reg, index_reg, tmp_reg);
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
                    let var_ty: BytecodeType = var_ty.into();
                    let var_reg = self.alloc_var(var_ty);
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
        ty: BuiltinType,
    ) {
        match pattern {
            LetPattern::Ident(ref ident) => {
                let var_id = *self.src.map_vars.get(ident.id).unwrap();
                let var_ty = self.var_ty(var_id);

                if !var_ty.is_unit() {
                    let var_ty: BytecodeType = var_ty.into();
                    let var_reg = self.var_reg(var_id);
                    self.emit_load_array(var_ty, var_reg, array_reg, index_reg, ident.pos);
                }
            }

            LetPattern::Underscore(_) => {
                // nothing to do
            }

            LetPattern::Tuple(ref tuple) => {
                if tuple.parts.len() > 0 {
                    let bytecode_ty: BytecodeType = ty.into();
                    let tuple_reg = self.alloc_temp(bytecode_ty);
                    self.emit_load_array(bytecode_ty, tuple_reg, array_reg, index_reg, tuple.pos);
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
        next_ty: BuiltinType,
    ) {
        match pattern {
            LetPattern::Ident(ref ident) => {
                let var_id = *self.src.map_vars.get(ident.id).unwrap();
                let var_ty = self.var_ty(var_id);

                if !var_ty.is_unit() {
                    let var_ty: BytecodeType = var_ty.into();
                    let var_reg = self.var_reg(var_id);
                    self.emit_mov(var_ty.into(), var_reg, next_reg)
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
        tuple_ty: BuiltinType,
    ) {
        let tuple_id = tuple_ty.tuple_id().expect("type should be tuple");

        for (idx, part) in tuple.parts.iter().enumerate() {
            match &**part {
                LetPattern::Ident(ref ident) => {
                    let var_id = *self.src.map_vars.get(ident.id).unwrap();
                    let ty = self.var_ty(var_id);

                    if !ty.is_unit() {
                        let bytecode_ty: BytecodeType = ty.into();
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
                    let (ty, _) = self.vm.tuples.lock().get_at(tuple_id, idx);

                    if !ty.is_unit() {
                        let temp_reg = self.alloc_temp(ty.into());
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
        self.gen.emit_push_register(object_reg);

        let iterator_reg = if let Some(make_iterator) = for_type_info.make_iterator {
            let object_type = self.ty(stmt.expr.id());
            let object_type_params = object_type.type_params(self.vm);

            // Emit: <iterator> = <obj>.makeIterator();
            let iterator_reg = self.alloc_var(BytecodeType::Ptr);
            self.gen.emit_invoke_direct(
                iterator_reg,
                FctDef::fct_id_types(self.vm, make_iterator, object_type_params),
                stmt.expr.pos(),
            );
            iterator_reg
        } else {
            // Object is already the iterator - just use it
            object_reg
        };

        let iterator_type = self.specialize_type(for_type_info.iterator_type);
        let iterator_type_params = iterator_type.type_params(self.vm);

        self.gen.emit_push_register(iterator_reg);

        let lbl_cond = self.gen.define_label();
        let lbl_end = self.gen.create_label();

        // Emit: <cond> = <iterator>.hasNext() & jump to lbl_end if false
        let cond_reg = self.alloc_temp(BytecodeType::Bool);
        self.gen.emit_invoke_direct(
            cond_reg,
            FctDef::fct_id_types(
                self.vm,
                for_type_info.has_next,
                iterator_type_params.clone(),
            ),
            stmt.expr.pos(),
        );
        self.gen.emit_jump_if_false(cond_reg, lbl_end);
        self.free_temp(cond_reg);

        // Emit: <var> = <iterator>.next()
        let next_ty = self.specialize_type(for_type_info.next_type);

        if next_ty.is_unit() {
            self.gen.emit_push_register(iterator_reg);
            self.gen.emit_invoke_direct_void(
                FctDef::fct_id_types(self.vm, for_type_info.next, iterator_type_params),
                stmt.expr.pos(),
            );
        } else {
            let next_bytecode_ty: BytecodeType = next_ty.into();
            let next_reg = self.alloc_var(next_bytecode_ty);

            self.gen.emit_push_register(iterator_reg);
            self.emit_invoke_direct(
                next_ty,
                next_reg,
                FctDef::fct_id_types(self.vm, for_type_info.next, iterator_type_params),
                stmt.expr.pos(),
            );

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
            let ty: BytecodeType = ty.into();
            let var_reg = self.alloc_var(ty);

            self.var_registers.insert(var_id, var_reg);

            DataDest::Reg(var_reg)
        };

        if let Some(ref expr) = stmt.expr {
            self.visit_expr(expr, dest);
        } else if ty.reference_type() {
            self.gen.emit_const_nil(dest.reg());
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
                    let ty = self.var_ty(var_id).into();
                    let var_reg = self.alloc_var(ty);
                    self.var_registers.insert(var_id, var_reg);

                    if ty.is_ptr() {
                        self.gen.emit_const_nil(var_reg);
                    }
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
        let ret_ty = self.specialize_type(self.fct.return_type);

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
            ExprUn(ref un) => self.visit_expr_un(un, dest),
            ExprBin(ref bin) => self.visit_expr_bin(bin, dest),
            ExprDot(ref field) => self.visit_expr_dot(field, dest),
            ExprBlock(ref block) => self.visit_expr_block(block, dest),
            ExprIf(ref expr) => self.visit_expr_if(expr, dest),
            ExprTemplate(ref template) => self.visit_expr_template(template, dest),
            ExprTypeParam(_) => unreachable!(),
            ExprPath(ref path) => self.visit_expr_path(path, dest),
            ExprLitChar(ref lit) => self.visit_expr_lit_char(lit, dest),
            ExprLitInt(ref lit) => self.visit_expr_lit_int(lit, dest, false),
            ExprLitFloat(ref lit) => self.visit_expr_lit_float(lit, dest),
            ExprLitStr(ref lit) => self.visit_expr_lit_string(lit, dest),
            ExprLitBool(ref lit) => self.visit_expr_lit_bool(lit, dest),
            ExprIdent(ref ident) => self.visit_expr_ident(ident, dest),
            ExprCall(ref call) => self.visit_expr_call(call, dest),
            ExprDelegation(ref call) => self.visit_expr_delegation(call, dest),
            ExprSelf(_) => self.visit_expr_self(dest),
            ExprSuper(_) => self.visit_expr_self(dest),
            ExprConv(ref conv) => self.visit_expr_conv(conv, dest),
            ExprNil(ref nil) => self.visit_expr_nil(nil, dest),
            ExprTuple(ref tuple) => self.visit_expr_tuple(tuple, dest),
            ExprParen(ref paren) => self.visit_expr(&paren.expr, dest),
            ExprLambda(_) => unimplemented!(),
        }
    }

    fn emit_expr_for_effect(&mut self, expr: &Expr) {
        let reg = self.visit_expr(expr, DataDest::Effect);
        self.free_if_temp(reg);
    }

    fn visit_expr_template(&mut self, expr: &ExprTemplateType, dest: DataDest) -> Register {
        let buffer_register = self.ensure_register(dest, BytecodeType::Ptr);

        // build StringBuffer::empty() call
        let fct_id = self.vm.vips.fct.string_buffer_empty;
        self.gen
            .emit_invoke_static(buffer_register, FctDef::fct_id(self.vm, fct_id), expr.pos);

        let part_register = self.alloc_temp(BytecodeType::Ptr);

        for part in &expr.parts {
            if let Some(ref lit_str) = part.to_lit_str() {
                let value = lit_str.value.clone();
                self.gen.emit_const_string(part_register, value);
            } else {
                let ty = self.ty(part.id());

                if ty.cls_id(self.vm) == Some(self.vm.vips.string_class) {
                    self.visit_expr(part, DataDest::Reg(part_register));
                } else {
                    let expr_register = self.visit_expr(part, DataDest::Alloc);
                    self.gen.emit_push_register(expr_register);

                    // build toString() call
                    let cls_id = ty.cls_id(self.vm).expect("no cls_id found for type");
                    let cls = self.vm.classes.idx(cls_id);
                    let cls = cls.read();
                    let name = self.vm.interner.intern("toString");
                    let to_string_id = cls
                        .find_trait_method(self.vm, self.vm.vips.stringable_trait, name, false)
                        .expect("toString() method not found");

                    if ty.reference_type() {
                        self.gen.emit_invoke_direct(
                            part_register,
                            FctDef::fct_id(self.vm, to_string_id),
                            part.pos(),
                        );
                    } else {
                        self.gen.emit_invoke_static(
                            part_register,
                            FctDef::fct_id(self.vm, to_string_id),
                            part.pos(),
                        );
                    }

                    self.free_if_temp(expr_register);
                }
            }

            // build StringBuffer::append() call
            let fct_id = self.vm.vips.fct.string_buffer_append;
            self.gen.emit_push_register(buffer_register);
            self.gen.emit_push_register(part_register);
            self.gen
                .emit_invoke_direct_void(FctDef::fct_id(self.vm, fct_id), expr.pos);
        }

        self.free_temp(part_register);

        // build StringBuffer::toString() call
        let fct_id = self.vm.vips.fct.string_buffer_to_string;
        self.gen.emit_push_register(buffer_register);
        self.gen
            .emit_invoke_static(buffer_register, FctDef::fct_id(self.vm, fct_id), expr.pos);

        buffer_register
    }

    fn visit_expr_path(&mut self, expr: &ExprPathType, dest: DataDest) -> Register {
        let ident_type = self.src.map_idents.get(expr.id).unwrap();

        match ident_type {
            &IdentType::EnumValue(_, value) => {
                let dest = self.ensure_register(dest, BytecodeType::Int32);
                self.gen.emit_const_int32(dest, value as i32);
                dest
            }

            _ => unreachable!(),
        }
    }

    fn visit_expr_conv(&mut self, expr: &ExprConvType, dest: DataDest) -> Register {
        let conv = *self.src.map_convs.get(expr.id).unwrap();
        let ty = self.specialize_type(conv.check_type);
        let cls_def_id = specialize_class_ty(self.vm, ty);

        if expr.is {
            let object = self.visit_expr(&expr.object, DataDest::Alloc);
            let result = self.ensure_register(dest, BytecodeType::Bool);
            self.gen.emit_instance_of(result, object, cls_def_id);

            result
        } else {
            let dest = match dest {
                DataDest::Effect => DataDest::Alloc,
                DataDest::Reg(reg) => DataDest::Reg(reg),
                DataDest::Alloc => DataDest::Alloc,
            };

            let object = self.visit_expr(&expr.object, dest);
            self.gen.emit_checked_cast(object, cls_def_id, expr.pos);
            object
        }
    }

    fn visit_expr_if(&mut self, expr: &ExprIfType, dest: DataDest) -> Register {
        let ty = self.ty(expr.id);

        if let Some(ref else_block) = expr.else_block {
            let dest = if ty.is_unit() {
                Register::invalid()
            } else {
                self.ensure_register(dest, ty.into())
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

        let (cls_ty, field_id) = {
            let ident_type = self.src.map_idents.get(expr.id).unwrap();

            match ident_type {
                &IdentType::Field(ty, field) => (ty, field),
                _ => unreachable!(),
            }
        };

        let cls_ty = self.specialize_type(cls_ty);
        let cls_def_id = specialize_class_ty(self.vm, cls_ty);

        let field_ty = {
            let cls = self.vm.class_defs.idx(cls_def_id);
            let cls = cls.read();

            let field = &cls.fields[field_id.idx()];
            field.ty
        };

        if field_ty.is_unit() {
            assert!(dest.is_unit());
            let obj = self.visit_expr(&expr.lhs, DataDest::Alloc);
            self.gen.emit_nil_check(obj, expr.pos);
            self.free_if_temp(obj);
            return Register::invalid();
        }

        let field_bc_ty: BytecodeType = field_ty.into();

        let dest = self.ensure_register(dest, field_bc_ty);
        let obj = self.visit_expr(&expr.lhs, DataDest::Alloc);

        self.gen
            .emit_load_field(dest, obj, cls_def_id, field_id, expr.pos);
        self.free_if_temp(obj);

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

        let (ty, _) = self.vm.tuples.lock().get_at(tuple_id, idx as usize);

        if ty.is_unit() {
            assert!(dest.is_unit());
            self.free_if_temp(tuple);
            return Register::invalid();
        }

        let ty: BytecodeType = ty.into();
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
            return self.visit_expr_call_intrinsic(expr, info, dest);
        }

        let call_type = self.src.map_calls.get(expr.id).unwrap().clone();

        // Find method that is called
        let callee_id = self.determine_callee(&call_type);

        let callee = self.vm.fcts.idx(callee_id);
        let callee = callee.read();

        // Create FctDefId for this Fct
        let callee_def_id = self.specialize_call(&callee, &call_type);

        // Determine types for arguments and return values
        let (arg_types, arg_bytecode_types, return_type) =
            self.determine_callee_types(&call_type, &*callee);

        // Allocate register for result
        let return_reg = if return_type.is_unit() {
            Register::invalid()
        } else {
            self.ensure_register(dest, return_type.into())
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
            &arg_bytecode_types,
            return_type,
            expr.pos,
            callee_def_id,
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

    fn determine_callee(&mut self, call_type: &CallType) -> FctId {
        match *call_type {
            CallType::GenericMethod(id, trait_id, trait_fct_id) => {
                if self.generic_mode {
                    trait_fct_id
                } else {
                    // This happens for calls like (T: SomeTrait).method()
                    // Find the exact method that is called
                    let object_type = self.specialize_type(BuiltinType::TypeParam(id));
                    self.find_trait_impl(trait_fct_id, trait_id, object_type)
                }
            }
            CallType::GenericStaticMethod(list_id, trait_id, trait_fct_id) => {
                if self.generic_mode {
                    trait_fct_id
                } else {
                    let ty = self.type_params[list_id.to_usize()];
                    let cls_id = ty.cls_id(self.vm).expect("no cls_id for type");

                    let cls = self.vm.classes.idx(cls_id);
                    let cls = cls.read();

                    let mut impl_fct_id: Option<FctId> = None;

                    for &impl_id in &cls.impls {
                        let ximpl = self.vm.impls[impl_id].read();

                        if ximpl.trait_id != Some(trait_id) {
                            continue;
                        }

                        for &fid in &ximpl.methods {
                            let method = self.vm.fcts.idx(fid);
                            let method = method.read();

                            if method.impl_for == Some(trait_fct_id) {
                                impl_fct_id = Some(fid);
                                break;
                            }
                        }
                    }

                    impl_fct_id.expect("no impl_fct_id found")
                }
            }
            _ => call_type.fct_id().unwrap(),
        }
    }

    fn determine_callee_types(
        &mut self,
        call_type: &CallType,
        fct: &Fct,
    ) -> (Vec<BuiltinType>, Vec<BytecodeType>, BuiltinType) {
        let return_type = self.specialize_type_for_call(&call_type, fct.return_type);

        let arg_types = fct
            .params_with_self()
            .iter()
            .map(|&arg| self.specialize_type_for_call(&call_type, arg))
            .collect::<Vec<BuiltinType>>();

        let arg_bytecode_types = arg_types
            .iter()
            .filter(|ty| !ty.is_unit())
            .map(|&ty| ty.into())
            .collect::<Vec<BytecodeType>>();

        (arg_types, arg_bytecode_types, return_type)
    }

    fn emit_call_object_argument(
        &mut self,
        expr: &ExprCallType,
        call_type: &CallType,
    ) -> Option<Register> {
        match *call_type {
            CallType::Method(_, _, _) | CallType::GenericMethod(_, _, _) => {
                let obj_expr = expr.object().expect("method target required");
                let reg = self.visit_expr(obj_expr, DataDest::Alloc);

                Some(reg)
            }
            CallType::Expr(_, _) => Some(self.visit_expr(&expr.callee, DataDest::Alloc)),
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
        callee: &Fct,
        call_type: &CallType,
        arg_types: &[BuiltinType],
    ) -> Vec<Register> {
        let mut registers = Vec::new();

        // self was already emitted, needs to be ignored here.
        let arg_start_offset = match *call_type {
            CallType::Ctor(_, _)
            | CallType::Expr(_, _)
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
            let ty = arg_types[idx + arg_start_offset];

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
        arg_types: &[BuiltinType],
        non_variadic_arguments: usize,
        dest: DataDest,
    ) -> Register {
        let variadic_arguments = expr.args.len() - non_variadic_arguments;

        // We need array of elements
        let element_ty = arg_types.last().cloned().unwrap();
        let ty = self.vm.vips.array_ty(self.vm, element_ty);
        let cls_def_id = specialize_class_ty(self.vm, ty);

        // Store length in a register
        let length_reg = self.alloc_temp(BytecodeType::Int64);
        self.gen
            .emit_const_int64(length_reg, variadic_arguments as i64);

        // Allocate array of given length
        let array_reg = self.ensure_register(dest, BytecodeType::Ptr);
        self.gen
            .emit_new_array(array_reg, cls_def_id, length_reg, expr.pos);

        if element_ty.is_unit() {
            // Evaluate rest arguments
            for arg in expr.args.iter().skip(non_variadic_arguments) {
                self.visit_expr(arg, DataDest::Effect);
            }
        } else {
            let bytecode_ty: BytecodeType = element_ty.into();
            let index_reg = self.alloc_temp(BytecodeType::Int64);

            // Evaluate rest arguments and store them in array
            for (idx, arg) in expr.args.iter().skip(non_variadic_arguments).enumerate() {
                let arg_reg = self.visit_expr(arg, DataDest::Alloc);
                self.gen.emit_const_int64(index_reg, idx as i64);
                self.emit_store_array(bytecode_ty, array_reg, index_reg, arg_reg, expr.pos);
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
        arg_types: &[BuiltinType],
        object_reg: Option<Register>,
    ) {
        match *call_type {
            CallType::Ctor(_, _) => {
                let ty = arg_types.first().cloned().unwrap();
                let object_reg = object_reg.unwrap();
                let cls_def_id = specialize_class_ty(self.vm, ty);

                let cls = self.vm.class_defs.idx(cls_def_id);
                let cls = cls.read();

                match cls.size {
                    InstanceSize::Fixed(_) => {
                        self.gen.emit_new_object(object_reg, cls_def_id, pos);
                    }
                    _ => {
                        panic!("unimplemented size {:?}", cls.size);
                    }
                }
            }
            _ => {}
        }
    }

    fn emit_call_inst(
        &mut self,
        expr: &ExprCallType,
        fct: &Fct,
        call_type: &CallType,
        arg_bytecode_types: &[BytecodeType],
        return_type: BuiltinType,
        pos: Position,
        fct_def_id: FctDefId,
        return_reg: Register,
    ) {
        match *call_type {
            CallType::CtorParent(_, _) | CallType::Ctor(_, _) => {
                self.gen.emit_invoke_direct_void(fct_def_id, pos);
            }

            CallType::Method(_, _, _) => {
                let is_super_call = expr
                    .object()
                    .map(|object| object.is_super())
                    .unwrap_or(false);

                if is_super_call {
                    self.emit_invoke_direct(return_type, return_reg, fct_def_id, pos);
                } else if fct.is_virtual() {
                    self.emit_invoke_virtual(return_type, return_reg, fct_def_id, pos);
                } else if arg_bytecode_types[0] != BytecodeType::Ptr {
                    self.emit_invoke_static(return_type, return_reg, fct_def_id, pos);
                } else {
                    self.emit_invoke_direct(return_type, return_reg, fct_def_id, pos);
                }
            }
            CallType::ModuleMethod(_, _, _) => {
                if arg_bytecode_types.is_empty() || arg_bytecode_types[0] != BytecodeType::Ptr {
                    self.emit_invoke_static(return_type, return_reg, fct_def_id, pos);
                } else {
                    self.emit_invoke_direct(return_type, return_reg, fct_def_id, pos);
                }
            }
            CallType::Fct(_, _, _) => {
                self.emit_invoke_static(return_type, return_reg, fct_def_id, pos);
            }
            CallType::Expr(_, _) => {
                if fct.is_virtual() {
                    self.emit_invoke_virtual(return_type, return_reg, fct_def_id, pos);
                } else if arg_bytecode_types[0] != BytecodeType::Ptr {
                    self.emit_invoke_static(return_type, return_reg, fct_def_id, pos);
                } else {
                    self.emit_invoke_direct(return_type, return_reg, fct_def_id, pos);
                }
            }
            CallType::TraitObjectMethod(_, _) => unimplemented!(),
            CallType::GenericMethod(_, _, _) => {
                if self.generic_mode {
                    self.emit_invoke_generic_direct(return_type, return_reg, fct_def_id, pos);
                } else if arg_bytecode_types[0] != BytecodeType::Ptr {
                    self.emit_invoke_static(return_type, return_reg, fct_def_id, pos);
                } else {
                    self.emit_invoke_direct(return_type, return_reg, fct_def_id, pos);
                }
            }
            CallType::GenericStaticMethod(_, _, _) => {
                if self.generic_mode {
                    self.emit_invoke_generic_static(return_type, return_reg, fct_def_id, pos);
                } else {
                    self.emit_invoke_static(return_type, return_reg, fct_def_id, pos);
                }
            }
            CallType::Intrinsic(_) => unreachable!(),
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
                    self.gen.emit_mov_ptr(dest_reg, obj_reg);
                    self.free_if_temp(obj_reg);
                    dest_reg
                }
            }
        } else {
            return_reg
        }
    }

    fn emit_mov(&mut self, ty: BytecodeType, dest: Register, src: Register) {
        if dest == src {
            return;
        }

        match ty {
            BytecodeType::Bool => self.gen.emit_mov_bool(dest, src),
            BytecodeType::UInt8 => self.gen.emit_mov_uint8(dest, src),
            BytecodeType::Char => self.gen.emit_mov_char(dest, src),
            BytecodeType::Float64 => self.gen.emit_mov_float64(dest, src),
            BytecodeType::Float32 => self.gen.emit_mov_float32(dest, src),
            BytecodeType::Int32 => self.gen.emit_mov_int32(dest, src),
            BytecodeType::Int64 => self.gen.emit_mov_int64(dest, src),
            BytecodeType::Ptr => self.gen.emit_mov_ptr(dest, src),
            BytecodeType::Tuple(tuple_id) => self.gen.emit_mov_tuple(dest, src, tuple_id),
            BytecodeType::TypeParam(_) => {
                assert!(self.generic_mode);
                self.gen.emit_mov_generic(dest, src)
            }
        }
    }

    fn emit_load_array(
        &mut self,
        ty: BytecodeType,
        dest: Register,
        arr: Register,
        idx: Register,
        pos: Position,
    ) {
        match ty {
            BytecodeType::Bool => self.gen.emit_load_array_bool(dest, arr, idx, pos),
            BytecodeType::UInt8 => self.gen.emit_load_array_uint8(dest, arr, idx, pos),
            BytecodeType::Char => self.gen.emit_load_array_char(dest, arr, idx, pos),
            BytecodeType::Int32 => self.gen.emit_load_array_int32(dest, arr, idx, pos),
            BytecodeType::Int64 => self.gen.emit_load_array_int64(dest, arr, idx, pos),
            BytecodeType::Float32 => self.gen.emit_load_array_float32(dest, arr, idx, pos),
            BytecodeType::Float64 => self.gen.emit_load_array_float64(dest, arr, idx, pos),
            BytecodeType::Ptr => self.gen.emit_load_array_ptr(dest, arr, idx, pos),
            BytecodeType::Tuple(_) => self.gen.emit_load_array_tuple(dest, arr, idx, pos),
            BytecodeType::TypeParam(_) => unreachable!(),
        }
    }

    fn emit_invoke_virtual(
        &mut self,
        return_type: BuiltinType,
        return_reg: Register,
        callee_id: FctDefId,
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
        return_type: BuiltinType,
        return_reg: Register,
        callee_id: FctDefId,
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
        return_type: BuiltinType,
        return_reg: Register,
        callee_id: FctDefId,
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
        return_type: BuiltinType,
        return_reg: Register,
        callee_id: FctDefId,
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
        return_type: BuiltinType,
        return_reg: Register,
        callee_id: FctDefId,
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
        let callee = self.vm.fcts.idx(callee_id);
        let callee = callee.read();

        let fct_def_id = self.specialize_call(&callee, &call_type);

        assert!(callee.return_type.is_unit());
        let arg_types = callee
            .params_with_self()
            .iter()
            .map(|&arg| self.specialize_type_for_call(&call_type, arg).into())
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
                self.gen.emit_invoke_direct_void(fct_def_id, expr.pos);
            }

            _ => unreachable!(),
        }

        for arg in arg_regs {
            self.free_if_temp(arg);
        }

        Register::invalid()
    }

    fn visit_expr_nil(&mut self, _nil: &ExprNilType, dest: DataDest) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

        let dest = self.ensure_register(dest, BytecodeType::Ptr);

        self.gen.emit_const_nil(dest);

        dest
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
        let ty: BytecodeType = self.src.var_self().ty.into();

        self.emit_mov(ty, dest, var_reg);

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
            BuiltinType::UInt8 => BytecodeType::UInt8,
            BuiltinType::Int32 => BytecodeType::Int32,
            BuiltinType::Int64 => BytecodeType::Int64,
            _ => unreachable!(),
        };

        let dest = self.ensure_register(dest, ty);

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
            BuiltinType::Float32 => BytecodeType::Float32,
            BuiltinType::Float64 => BytecodeType::Float64,
            _ => unreachable!(),
        };

        let dest = self.ensure_register(dest, ty);

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

        self.gen.emit_new_tuple(result, tuple_id);

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

        let callee = self.vm.fcts.idx(callee_id);
        let callee = callee.read();

        // Create FctDefId for this callee
        let callee_def_id = self.specialize_call(&callee, &call_type);

        let function_return_type: BuiltinType =
            self.specialize_type_for_call(call_type, callee.return_type);

        let function_return_type_bc: BytecodeType = function_return_type.into();

        let dest = self.ensure_register(dest, function_return_type_bc);

        self.gen.emit_push_register(opnd);

        if function_return_type_bc == BytecodeType::Ptr {
            self.emit_invoke_direct(function_return_type, dest, callee_def_id, expr.pos);
        } else {
            self.emit_invoke_static(function_return_type, dest, callee_def_id, expr.pos);
        }

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
        let lhs_type: BytecodeType = self.ty(expr.lhs.id()).into();

        let lhs = self.visit_expr(&expr.lhs, DataDest::Alloc);
        let rhs = self.visit_expr(&expr.rhs, DataDest::Alloc);

        let call_type = self.src.map_calls.get(expr.id).unwrap();
        let callee_id = self.determine_callee(call_type);

        let callee = self.vm.fcts.idx(callee_id);
        let callee = callee.read();

        // Create FctDefId for this callee
        let callee_def_id = self.specialize_call(&callee, &call_type);

        let function_return_type: BuiltinType =
            self.specialize_type_for_call(call_type, callee.return_type);

        let function_return_type_bc: BytecodeType = function_return_type.into();

        let return_type = match expr.op {
            BinOp::Cmp(_) => BytecodeType::Bool,
            _ => function_return_type_bc,
        };

        let dest = self.ensure_register(dest, return_type);

        let result = if function_return_type_bc == return_type {
            dest
        } else {
            self.alloc_temp(function_return_type_bc)
        };

        self.gen.emit_push_register(lhs);
        self.gen.emit_push_register(rhs);

        if lhs_type == BytecodeType::Ptr {
            self.emit_invoke_direct(function_return_type, result, callee_def_id, expr.pos);
        } else {
            self.emit_invoke_static(function_return_type, result, callee_def_id, expr.pos);
        }

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
                    self.emit_array_with_variadic_arguments(expr, &[ty], 0, dest)
                }

                Intrinsic::DefaultValue => {
                    let ty = self.ty(expr.id);

                    if ty.is_unit() || dest.is_effect() {
                        assert!(dest.is_unit());
                        return Register::invalid();
                    }

                    let ty: BytecodeType = ty.into();
                    let dest = self.ensure_register(dest, ty);

                    match ty {
                        BytecodeType::Bool => self.gen.emit_const_false(dest),
                        BytecodeType::UInt8 => self.gen.emit_const_uint8(dest, 0),
                        BytecodeType::Int32 => self.gen.emit_const_int32(dest, 0),
                        BytecodeType::Int64 => self.gen.emit_const_int64(dest, 0),
                        BytecodeType::Char => self.gen.emit_const_char(dest, '\0'),
                        BytecodeType::Float32 => self.gen.emit_const_float32(dest, 0.0),
                        BytecodeType::Float64 => self.gen.emit_const_float64(dest, 0.0),
                        BytecodeType::Ptr => self.gen.emit_const_nil(dest),
                        BytecodeType::Tuple(_) => unimplemented!(),
                        BytecodeType::TypeParam(_) => unreachable!(),
                    }

                    dest
                }

                _ => panic!("unimplemented intrinsic {:?}", intrinsic),
            }
        }
    }

    fn emit_intrinsic_new_array(&mut self, expr: &ExprCallType, dest: DataDest) -> Register {
        // We need array of elements
        let element_ty = self.ty(expr.id);
        let cls_def_id = specialize_class_ty(self.vm, element_ty);

        let array_reg = self.ensure_register(dest, BytecodeType::Ptr);
        let length_reg = self.visit_expr(&expr.args[0], DataDest::Alloc);

        self.gen
            .emit_new_array(array_reg, cls_def_id, length_reg, expr.pos);

        self.free_if_temp(length_reg);

        array_reg
    }

    fn emit_bin_is(&mut self, expr: &ExprBinType, dest: DataDest) -> Register {
        if dest.is_effect() {
            self.emit_expr_for_effect(&expr.lhs);
            self.emit_expr_for_effect(&expr.rhs);
            return Register::invalid();
        }

        let builtin_type = self.ty(expr.lhs.id());
        let dest = self.ensure_register(dest, BytecodeType::Bool);

        let lhs_reg = self.visit_expr(&expr.lhs, DataDest::Alloc);
        let rhs_reg = self.visit_expr(&expr.rhs, DataDest::Alloc);

        let cmp_ty: BytecodeType = match builtin_type {
            BuiltinType::Nil => BytecodeType::Ptr,
            BuiltinType::Float32 => BytecodeType::Int32,
            BuiltinType::Float64 => BytecodeType::Int64,
            _ => builtin_type.into(),
        };

        let cmp_lhs_reg;
        let cmp_rhs_reg;

        if builtin_type.is_float() {
            cmp_lhs_reg = self.alloc_temp(cmp_ty);
            cmp_rhs_reg = self.alloc_temp(cmp_ty);

            if cmp_ty == BytecodeType::Int32 {
                self.gen
                    .emit_reinterpret_float32_as_int32(cmp_lhs_reg, lhs_reg);
                self.gen
                    .emit_reinterpret_float32_as_int32(cmp_rhs_reg, rhs_reg);
            } else {
                self.gen
                    .emit_reinterpret_float64_as_int64(cmp_lhs_reg, lhs_reg);
                self.gen
                    .emit_reinterpret_float64_as_int64(cmp_rhs_reg, rhs_reg);
            }

            self.free_if_temp(lhs_reg);
            self.free_if_temp(rhs_reg);
        } else {
            cmp_lhs_reg = lhs_reg;
            cmp_rhs_reg = rhs_reg;
        }

        if expr.op == BinOp::Cmp(CmpOp::Is) {
            match cmp_ty {
                BytecodeType::UInt8 => self.gen.emit_test_eq_uint8(dest, cmp_lhs_reg, cmp_rhs_reg),
                BytecodeType::Bool => self.gen.emit_test_eq_bool(dest, cmp_lhs_reg, cmp_rhs_reg),
                BytecodeType::Char => self.gen.emit_test_eq_char(dest, cmp_lhs_reg, cmp_rhs_reg),
                BytecodeType::Int32 => self.gen.emit_test_eq_int32(dest, cmp_lhs_reg, cmp_rhs_reg),
                BytecodeType::Int64 => self.gen.emit_test_eq_int64(dest, cmp_lhs_reg, cmp_rhs_reg),
                BytecodeType::Ptr => self.gen.emit_test_eq_ptr(dest, cmp_lhs_reg, cmp_rhs_reg),
                _ => unreachable!(),
            }
        } else {
            match cmp_ty {
                BytecodeType::UInt8 => self.gen.emit_test_ne_uint8(dest, cmp_lhs_reg, cmp_rhs_reg),
                BytecodeType::Bool => self.gen.emit_test_ne_bool(dest, cmp_lhs_reg, cmp_rhs_reg),
                BytecodeType::Char => self.gen.emit_test_ne_char(dest, cmp_lhs_reg, cmp_rhs_reg),
                BytecodeType::Int32 => self.gen.emit_test_ne_int32(dest, cmp_lhs_reg, cmp_rhs_reg),
                BytecodeType::Int64 => self.gen.emit_test_ne_int64(dest, cmp_lhs_reg, cmp_rhs_reg),
                BytecodeType::Ptr => self.gen.emit_test_ne_ptr(dest, cmp_lhs_reg, cmp_rhs_reg),
                _ => unreachable!(),
            }
        }

        self.free_if_temp(cmp_lhs_reg);
        self.free_if_temp(cmp_rhs_reg);

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
        let ty = ty.type_params(self.vm);
        let ty = ty[0];
        let ty: Option<BytecodeType> = if ty.is_unit() { None } else { Some(ty.into()) };

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

        let ty = ty.unwrap();
        self.emit_store_array(ty, arr, idx, src, pos);

        self.free_if_temp(arr);
        self.free_if_temp(idx);
        self.free_if_temp(src);

        Register::invalid()
    }

    fn emit_store_array(
        &mut self,
        ty: BytecodeType,
        arr: Register,
        idx: Register,
        src: Register,
        pos: Position,
    ) {
        match ty {
            BytecodeType::UInt8 => self.gen.emit_store_array_uint8(src, arr, idx, pos),
            BytecodeType::Bool => self.gen.emit_store_array_bool(src, arr, idx, pos),
            BytecodeType::Char => self.gen.emit_store_array_char(src, arr, idx, pos),
            BytecodeType::Int32 => self.gen.emit_store_array_int32(src, arr, idx, pos),
            BytecodeType::Int64 => self.gen.emit_store_array_int64(src, arr, idx, pos),
            BytecodeType::Float32 => self.gen.emit_store_array_float32(src, arr, idx, pos),
            BytecodeType::Float64 => self.gen.emit_store_array_float64(src, arr, idx, pos),
            BytecodeType::Ptr => self.gen.emit_store_array_ptr(src, arr, idx, pos),
            BytecodeType::Tuple(_) => self.gen.emit_store_array_tuple(src, arr, idx, pos),
            BytecodeType::TypeParam(_) => unreachable!(),
        }
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
            Intrinsic::ReinterpretFloat32AsInt32 => {
                self.gen.emit_reinterpret_float32_as_int32(dest, src)
            }
            Intrinsic::ReinterpretInt32AsFloat32 => {
                self.gen.emit_reinterpret_int32_as_float32(dest, src)
            }
            Intrinsic::ReinterpretFloat64AsInt64 => {
                self.gen.emit_reinterpret_float64_as_int64(dest, src)
            }
            Intrinsic::ReinterpretInt64AsFloat64 => {
                self.gen.emit_reinterpret_int64_as_float64(dest, src)
            }
            Intrinsic::ByteToChar => self.gen.emit_extend_byte_to_char(dest, src),
            Intrinsic::ByteToInt32 => self.gen.emit_extend_byte_to_int32(dest, src),
            Intrinsic::ByteToInt64 => self.gen.emit_extend_byte_to_int64(dest, src),
            Intrinsic::Int32ToInt64 => self.gen.emit_extend_int32_to_int64(dest, src),
            Intrinsic::CharToInt32 => self.gen.emit_cast_char_to_int32(dest, src),
            Intrinsic::CharToInt64 => self.gen.emit_extend_char_to_int64(dest, src),
            Intrinsic::Int32ToByte => self.gen.emit_cast_int32_to_uint8(dest, src),
            Intrinsic::Int32ToChar => self.gen.emit_cast_int32_to_char(dest, src),
            Intrinsic::Int32ToInt32 => self.gen.emit_mov_int32(dest, src),
            Intrinsic::Int64ToByte => self.gen.emit_cast_int64_to_uint8(dest, src),
            Intrinsic::Int64ToChar => self.gen.emit_cast_int64_to_char(dest, src),
            Intrinsic::Int64ToInt32 => self.gen.emit_cast_int64_to_int32(dest, src),
            Intrinsic::Float32IsNan => self.gen.emit_test_ne_float32(dest, src, src),
            Intrinsic::Float64IsNan => self.gen.emit_test_ne_float64(dest, src, src),
            Intrinsic::Int32ToFloat32 => self.gen.emit_convert_int32_to_float32(dest, src),
            Intrinsic::Int32ToFloat64 => self.gen.emit_convert_int32_to_float64(dest, src),
            Intrinsic::Int64ToFloat32 => self.gen.emit_convert_int64_to_float32(dest, src),
            Intrinsic::Int64ToFloat64 => self.gen.emit_convert_int64_to_float64(dest, src),
            Intrinsic::Float32ToInt32 => self.gen.emit_truncate_float32_to_int32(dest, src),
            Intrinsic::Float32ToInt64 => self.gen.emit_truncate_float32_to_int64(dest, src),
            Intrinsic::Float64ToInt32 => self.gen.emit_truncate_float64_to_int32(dest, src),
            Intrinsic::Float64ToInt64 => self.gen.emit_truncate_float64_to_int64(dest, src),
            Intrinsic::BoolToInt32 => {
                self.gen.emit_const_int32(dest, 1);
                let lbl_end = self.gen.create_label();
                self.gen.emit_jump_if_true(src, lbl_end);
                self.gen.emit_const_int32(dest, 0);
                self.gen.bind_label(lbl_end);
            }
            Intrinsic::Float32Sqrt => {
                self.gen.emit_push_register(src);
                self.gen.emit_invoke_static(
                    dest,
                    FctDef::fct_id(self.vm, info.fct_id.unwrap()),
                    opnd.pos(),
                );
            }
            Intrinsic::Float64Sqrt => {
                self.gen.emit_push_register(src);
                self.gen.emit_invoke_static(
                    dest,
                    FctDef::fct_id(self.vm, info.fct_id.unwrap()),
                    opnd.pos(),
                );
            }
            Intrinsic::Int32CountZeroBits
            | Intrinsic::Int32CountZeroBitsLeading
            | Intrinsic::Int32CountZeroBitsTrailing
            | Intrinsic::Int32CountOneBits
            | Intrinsic::Int32CountOneBitsLeading
            | Intrinsic::Int32CountOneBitsTrailing => {
                self.gen.emit_push_register(src);
                self.gen.emit_invoke_static(
                    dest,
                    FctDef::fct_id(self.vm, info.fct_id.unwrap()),
                    opnd.pos(),
                );
            }
            Intrinsic::Int64CountZeroBits
            | Intrinsic::Int64CountZeroBitsLeading
            | Intrinsic::Int64CountZeroBitsTrailing
            | Intrinsic::Int64CountOneBits
            | Intrinsic::Int64CountOneBitsLeading
            | Intrinsic::Int64CountOneBitsTrailing => {
                self.gen.emit_push_register(src);
                self.gen.emit_invoke_static(
                    dest,
                    FctDef::fct_id(self.vm, info.fct_id.unwrap()),
                    opnd.pos(),
                );
            }
            Intrinsic::PromoteFloat32ToFloat64 => {
                self.gen.emit_promote_float32_to_float64(dest, src);
            }
            Intrinsic::DemoteFloat64ToFloat32 => {
                self.gen.emit_demote_float64_to_float32(dest, src);
            }
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
                    if ty.cls_id(self.vm) == Some(self.vm.vips.string_class) {
                        Some(BytecodeType::UInt8)
                    } else {
                        let ty = ty.type_params(self.vm);
                        let ty = ty[0];

                        if ty.is_unit() {
                            assert!(dest.is_unit());
                            None
                        } else {
                            Some(ty.into())
                        }
                    };

                let dest = ty.map(|ty| self.ensure_register(dest, ty));

                let arr = self.visit_expr(lhs, DataDest::Alloc);
                let idx = self.visit_expr(rhs, DataDest::Alloc);

                if ty.is_none() {
                    self.gen.emit_array_bound_check(arr, idx, pos);

                    self.free_if_temp(arr);
                    self.free_if_temp(idx);

                    return Register::invalid();
                }

                let ty = ty.unwrap();
                let dest = dest.unwrap();

                match ty {
                    BytecodeType::UInt8 => self.gen.emit_load_array_uint8(dest, arr, idx, pos),
                    BytecodeType::Bool => self.gen.emit_load_array_bool(dest, arr, idx, pos),
                    BytecodeType::Char => self.gen.emit_load_array_char(dest, arr, idx, pos),
                    BytecodeType::Int32 => self.gen.emit_load_array_int32(dest, arr, idx, pos),
                    BytecodeType::Int64 => self.gen.emit_load_array_int64(dest, arr, idx, pos),
                    BytecodeType::Float32 => self.gen.emit_load_array_float32(dest, arr, idx, pos),
                    BytecodeType::Float64 => self.gen.emit_load_array_float64(dest, arr, idx, pos),
                    BytecodeType::Ptr => self.gen.emit_load_array_ptr(dest, arr, idx, pos),
                    BytecodeType::Tuple(_) => self.gen.emit_load_array_tuple(dest, arr, idx, pos),
                    BytecodeType::TypeParam(_) => unreachable!(),
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
                None => self.gen.emit_sub_int32(dest, lhs_reg, rhs_reg),
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
                    let result = self.alloc_temp(BytecodeType::Int64);
                    self.gen.emit_sub_int64(result, lhs_reg, rhs_reg);
                    self.gen.emit_cast_int64_to_int32(dest, result);
                    self.free_temp(result);
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
                _ => unreachable!(),
            },
            Intrinsic::Int32Add => self.gen.emit_add_int32(dest, lhs_reg, rhs_reg),
            Intrinsic::Int32Sub => self.gen.emit_sub_int32(dest, lhs_reg, rhs_reg),
            Intrinsic::Int32Mul => self.gen.emit_mul_int32(dest, lhs_reg, rhs_reg),
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
            Intrinsic::Int64Add => self.gen.emit_add_int64(dest, lhs_reg, rhs_reg),
            Intrinsic::Int64Sub => self.gen.emit_sub_int64(dest, lhs_reg, rhs_reg),
            Intrinsic::Int64Mul => self.gen.emit_mul_int64(dest, lhs_reg, rhs_reg),
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
                ExprDot(ref dot) => self.visit_expr_assign_dot(expr, dot),
                ExprCall(ref call) => self.visit_expr_assign_call(expr, call),
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

            let type_params = obj_ty.type_params(self.vm);

            let callee_id = FctDef::fct_id_types(self.vm, fct_id, type_params);
            self.gen.emit_invoke_direct_void(callee_id, expr.pos);

            self.free_if_temp(obj_reg);
            self.free_if_temp(idx_reg);
            self.free_if_temp(val_reg);
        }
    }

    fn visit_expr_assign_dot(&mut self, expr: &ExprBinType, dot: &ExprDotType) {
        let (class, field_id) = {
            let ident_type = self.src.map_idents.get(dot.id).unwrap();
            match ident_type {
                &IdentType::Field(class, field) => (class, field),
                _ => unreachable!(),
            }
        };
        let class = self.specialize_type(class);
        let cls_id = specialize_class_ty(self.vm, class);
        let cls = self.vm.class_defs.idx(cls_id);
        let cls = cls.read();
        let field = &cls.fields[field_id.idx()];
        let ty: Option<BytecodeType> = if field.ty.is_unit() {
            None
        } else {
            Some(field.ty.into())
        };

        let obj = self.visit_expr(&dot.lhs, DataDest::Alloc);
        let src = self.visit_expr(&expr.rhs, DataDest::Alloc);

        if ty.is_none() {
            self.gen.emit_nil_check(obj, expr.pos);
            return;
        }

        self.gen
            .emit_store_field(src, obj, cls_id, field_id, expr.pos);

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

    fn visit_expr_assign_global(&mut self, expr: &ExprBinType, gid: GlobalId) {
        let glob = self.vm.globals.idx(gid);
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

            &IdentType::Field(_, _) => unimplemented!(),
            &IdentType::Struct(_) => unimplemented!(),
            &IdentType::Const(cid) => self.visit_expr_ident_const(cid, dest),

            &IdentType::Enum(_) | &IdentType::EnumValue(_, _) => unreachable!(),
            &IdentType::Fct(_) | &IdentType::FctType(_, _) => unreachable!(),
            &IdentType::Class(_) | &IdentType::ClassType(_, _) => unreachable!(),
            &IdentType::Module(_) => unreachable!(),
            &IdentType::ClassAndModule(_, _) | &IdentType::StructAndModule(_, _) => unreachable!(),
            &IdentType::TypeParam(_) | &IdentType::TypeParamStaticMethod(_, _) => unreachable!(),
            &IdentType::Method(_, _) | &IdentType::MethodType(_, _, _) => unreachable!(),
            &IdentType::StaticMethod(_, _) | &IdentType::StaticMethodType(_, _, _) => {
                unreachable!()
            }
        }
    }

    fn visit_expr_ident_const(&mut self, const_id: ConstId, dest: DataDest) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

        let xconst = self.vm.consts.idx(const_id);
        let xconst = xconst.lock();
        let ty = xconst.ty;

        let dest = self.ensure_register(dest, ty.into());

        match ty {
            BuiltinType::Bool => {
                if xconst.value.to_bool() {
                    self.gen.emit_const_true(dest);
                } else {
                    self.gen.emit_const_false(dest);
                }
            }

            BuiltinType::Char => {
                self.gen.emit_const_char(dest, xconst.value.to_char());
            }

            BuiltinType::UInt8 => {
                self.gen.emit_const_uint8(dest, xconst.value.to_int() as u8);
            }

            BuiltinType::Int32 => {
                self.gen
                    .emit_const_int32(dest, xconst.value.to_int() as i32);
            }

            BuiltinType::Int64 => {
                self.gen.emit_const_int64(dest, xconst.value.to_int());
            }

            BuiltinType::Float32 => {
                self.gen
                    .emit_const_float32(dest, xconst.value.to_float() as f32);
            }

            BuiltinType::Float64 => {
                self.gen.emit_const_float64(dest, xconst.value.to_float());
            }

            _ => unimplemented!(),
        }

        dest
    }

    fn visit_expr_ident_global(&mut self, gid: GlobalId, dest: DataDest) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

        let glob = self.vm.globals.idx(gid);
        let glob = glob.read();

        if glob.ty.is_unit() {
            assert!(dest.is_alloc());
            return Register::invalid();
        }

        let ty: BytecodeType = glob.ty.into();
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
        let ty: BytecodeType = self.specialize_type(ty).into();

        if dest.is_alloc() {
            return var_reg;
        }

        let dest = dest.reg();
        self.emit_mov(ty, dest, var_reg);

        dest
    }

    fn find_trait_impl(&self, fct_id: FctId, trait_id: TraitId, object_type: BuiltinType) -> FctId {
        let cls_id = object_type.cls_id(self.vm).unwrap();
        let cls = self.vm.classes.idx(cls_id);
        let cls = cls.read();

        for &impl_id in &cls.impls {
            let ximpl = self.vm.impls[impl_id].read();

            if ximpl.trait_id() != trait_id {
                continue;
            }

            for &mtd_id in &ximpl.methods {
                let mtd = self.vm.fcts.idx(mtd_id);
                let mtd = mtd.read();

                if mtd.impl_for == Some(fct_id) {
                    return mtd_id;
                }
            }
        }

        panic!("no impl found for generic trait call")
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

    fn determine_call_type_params(&self, call_type: &CallType) -> TypeList {
        match *call_type {
            CallType::CtorParent(ty, _) | CallType::Ctor(ty, _) => ty.type_params(self.vm),

            CallType::Method(ty, _, ref fct_type_params) => {
                let ty = self.specialize_type(ty);

                let cls_type_params = ty.type_params(self.vm);
                cls_type_params.append(fct_type_params)
            }

            CallType::ModuleMethod(ty, _, ref fct_type_params) => {
                let ty = self.specialize_type(ty);

                let cls_type_params = ty.type_params(self.vm);
                cls_type_params.append(fct_type_params)
            }

            CallType::Fct(_, ref cls_tps, ref fct_tps) => {
                let cls_type_params = cls_tps.clone();
                let fct_type_params = fct_tps.clone();
                cls_type_params.append(&fct_type_params)
            }

            CallType::Expr(ty, _) => {
                let ty = self.specialize_type(ty);

                ty.type_params(self.vm)
            }

            CallType::TraitObjectMethod(_, _) => TypeList::empty(),
            CallType::GenericMethod(_, _, _) => TypeList::empty(),
            CallType::GenericStaticMethod(_, _, _) => TypeList::empty(),

            CallType::Intrinsic(_) => unreachable!(),
        }
    }

    fn specialize_call(&self, fct: &Fct, call_type: &CallType) -> FctDefId {
        let type_params = self.determine_call_type_params(call_type);

        let type_params = TypeList::with(
            type_params
                .iter()
                .map(|ty| self.specialize_type(ty))
                .collect::<Vec<_>>(),
        );

        FctDef::with(self.vm, fct, type_params)
    }

    fn specialize_type_for_call(&self, call_type: &CallType, ty: BuiltinType) -> BuiltinType {
        let ty = match *call_type {
            CallType::Fct(_, ref cls_type_params, ref fct_type_params) => {
                let type_params = cls_type_params.append(fct_type_params);
                specialize_type(self.vm, ty, &type_params)
            }

            CallType::Method(cls_ty, _, ref fct_type_params) => {
                let cls_type_params = cls_ty.type_params(self.vm);
                let type_params = cls_type_params.append(fct_type_params);
                specialize_type(self.vm, ty, &type_params)
            }

            CallType::ModuleMethod(cls_ty, _, ref fct_type_params) => {
                let cls_type_params = cls_ty.type_params(self.vm);
                let type_params = cls_type_params.append(fct_type_params);
                specialize_type(self.vm, ty, &type_params)
            }

            CallType::CtorParent(cls_ty, _) | CallType::Ctor(cls_ty, _) => {
                let cls_type_params = cls_ty.type_params(self.vm);
                specialize_type(self.vm, ty, &cls_type_params)
            }

            CallType::Expr(object_ty, _) => {
                let type_params = object_ty.type_params(self.vm);
                specialize_type(self.vm, ty, &type_params)
            }

            CallType::TraitObjectMethod(_, _) => unimplemented!(),
            CallType::GenericMethod(id, _, _) => {
                debug_assert!(ty.is_concrete_type(self.vm) || ty.is_self());
                if ty.is_self() {
                    BuiltinType::TypeParam(id)
                } else {
                    ty
                }
            }
            CallType::GenericStaticMethod(_, _, _) => {
                specialize_type(self.vm, ty, &TypeList::empty())
            }
            CallType::Intrinsic(_) => unreachable!(),
        };

        self.specialize_type(ty)
    }

    fn specialize_type(&self, ty: BuiltinType) -> BuiltinType {
        if self.generic_mode {
            ty
        } else {
            specialize_type(self.vm, ty, self.type_params)
        }
    }

    fn ty(&self, id: NodeId) -> BuiltinType {
        let ty = self.src.ty(id);
        self.specialize_type(ty)
    }

    fn var_ty(&self, id: VarId) -> BuiltinType {
        let ty = self.src.vars[id].ty;
        self.specialize_type(ty)
    }

    fn get_intrinsic(&self, id: NodeId) -> Option<IntrinsicInfo> {
        let call_type = self.src.map_calls.get(id).unwrap();

        if let Some(intrinsic) = call_type.to_intrinsic() {
            return Some(intrinsic.into());
        }

        let fid = call_type.fct_id().unwrap();

        // the function we compile right now is never an intrinsic
        if self.fct.id == fid {
            return None;
        }

        let fct = self.vm.fcts.idx(fid);
        let fct = fct.read();

        match fct.kind {
            FctKind::Builtin(intr) => Some(IntrinsicInfo::with_fct(intr, fid)),
            _ => None,
        }
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
    fct_id: Option<FctId>,
}

impl IntrinsicInfo {
    fn with_fct(intrinsic: Intrinsic, fct_id: FctId) -> IntrinsicInfo {
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
