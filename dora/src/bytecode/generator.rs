use dora_parser::lexer::position::Position;
use std::collections::HashMap;

use dora_parser::ast::Expr::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::*;

use crate::bytecode::{BytecodeFunction, BytecodeType, BytecodeWriter, Label, Register};
use crate::semck::specialize::{specialize_class_ty, specialize_type};
use crate::semck::{expr_always_returns, expr_block_always_returns};
use crate::size::InstanceSize;
use crate::ty::{BuiltinType, TypeList, TypeParamId};
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

pub fn generate_fct<'ast>(
    vm: &VM<'ast>,
    id: FctId,
    cls_type_params: &TypeList,
    fct_type_params: &TypeList,
) -> BytecodeFunction {
    let fct = vm.fcts.idx(id);
    let fct = fct.read();
    let src = fct.src();
    let mut src = src.write();

    generate(vm, &fct, &mut src, cls_type_params, fct_type_params)
}

pub fn generate<'ast>(
    vm: &VM<'ast>,
    fct: &Fct<'ast>,
    src: &FctSrc,
    cls_type_params: &TypeList,
    fct_type_params: &TypeList,
) -> BytecodeFunction {
    let ast_bytecode_generator = AstBytecodeGen {
        vm,
        fct,
        ast: fct.ast,
        src,

        cls_type_params,
        fct_type_params,

        gen: BytecodeWriter::new(),
        loops: Vec::new(),
        var_registers: HashMap::new(),
    };
    ast_bytecode_generator.generate()
}

struct AstBytecodeGen<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    fct: &'a Fct<'ast>,
    ast: &'ast Function,
    src: &'a FctSrc,

    cls_type_params: &'a TypeList,
    fct_type_params: &'a TypeList,

    gen: BytecodeWriter,
    loops: Vec<LoopLabels>,
    var_registers: HashMap<VarId, Register>,
}

impl<'a, 'ast> AstBytecodeGen<'a, 'ast> {
    fn generate(mut self) -> BytecodeFunction {
        let mut arguments = 0;

        if self.fct.has_self() {
            let var_self = self.src.var_self();
            let var_ty = self.specialize_type(var_self.ty);
            let var_id = var_self.id;
            let reg = self.gen.add_register(var_ty.into());
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
                let reg = self.gen.add_register(ty);
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
            }
        } else {
            unreachable!();
        }

        if self.fct.return_type.is_unit() {
            self.gen.emit_ret_void();
        }

        self.gen.generate()
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        match *stmt {
            StmtReturn(ref ret) => self.visit_stmt_return(ret),
            StmtBreak(ref stmt) => self.visit_stmt_break(stmt),
            StmtContinue(ref stmt) => self.visit_stmt_continue(stmt),
            StmtExpr(ref expr) => self.visit_stmt_expr(expr),
            StmtVar(ref stmt) => self.visit_stmt_var(stmt),
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
        let array_reg = self.gen.add_register(BytecodeType::Ptr);
        let index_reg = self.gen.add_register(BytecodeType::Int64);
        let length_reg = self.gen.add_register(BytecodeType::Int64);

        let for_var_id = *self.src.map_vars.get(stmt.id).unwrap();
        let var_ty = self.var_ty(for_var_id);
        let var_ty: BytecodeType = var_ty.into();
        let var_reg = self.gen.add_register(var_ty);
        self.var_registers.insert(for_var_id, var_reg);

        // evaluate and store array
        self.visit_expr(&stmt.expr, DataDest::Reg(array_reg));

        // calculate array length
        self.gen.set_position(stmt.expr.pos());
        self.gen.emit_array_length(length_reg, array_reg);

        // initialized index to 0
        self.gen.emit_const_int64(index_reg, 0);

        let lbl_cond = self.gen.define_label();
        let lbl_end = self.gen.create_label();

        // if idx >= length then goto end
        let tmp_reg = self.gen.add_register(BytecodeType::Bool);
        self.gen.emit_test_lt_int64(tmp_reg, index_reg, length_reg);
        self.gen.emit_jump_if_false(tmp_reg, lbl_end);

        // load current array element
        self.emit_load_array(var_ty, var_reg, array_reg, index_reg);

        self.loops.push(LoopLabels::new(lbl_cond, lbl_end));
        self.visit_stmt(&stmt.block);
        self.loops.pop().unwrap();

        // increment index
        let tmp_reg = self.gen.add_register(BytecodeType::Int64);
        self.gen.emit_const_int64(tmp_reg, 1);
        self.gen.emit_add_int64(index_reg, index_reg, tmp_reg);

        // jump to loop header
        self.gen.emit_jump_loop(lbl_cond);
        self.gen.bind_label(lbl_end);
    }

    fn visit_stmt_for_iterator(&mut self, stmt: &StmtForType) {
        let for_type_info = self.src.map_fors.get(stmt.id).unwrap().clone();

        // Emit: <obj> = <expr> (for <var> in <expr> { ... })
        let object_reg = self.visit_expr(&stmt.expr, DataDest::Alloc);
        self.gen.emit_push_register(object_reg);

        // Emit: <iterator> = <obj>.makeIterator();
        let iterator_reg = self.gen.add_register(BytecodeType::Ptr);
        self.gen.emit_invoke_direct_ptr(
            iterator_reg,
            FctDef::fct_id(self.vm, for_type_info.make_iterator),
        );

        self.gen.emit_push_register(iterator_reg);

        let lbl_cond = self.gen.define_label();
        let lbl_end = self.gen.create_label();

        // Emit: <cond> = <iterator>.hasNext() & jump to lbl_end if false
        let cond_reg = self.gen.add_register(BytecodeType::Bool);
        self.gen
            .emit_invoke_direct_ptr(cond_reg, FctDef::fct_id(self.vm, for_type_info.has_next));
        self.gen.emit_jump_if_false(cond_reg, lbl_end);

        // Emit: <var> = <iterator>.next()
        let var_id = *self.src.map_vars.get(stmt.id).unwrap();
        let var_ty = self.var_ty(var_id);

        let ty: BytecodeType = var_ty.into();
        let var_reg = self.gen.add_register(ty);
        self.var_registers.insert(var_id, var_reg);

        self.gen.emit_push_register(iterator_reg);

        self.emit_invoke_direct(var_ty, var_reg, FctDef::fct_id(self.vm, for_type_info.next));

        self.loops.push(LoopLabels::new(lbl_cond, lbl_end));
        self.visit_stmt(&stmt.block);
        self.loops.pop().unwrap();

        self.gen.emit_jump_loop(lbl_cond);
        self.gen.bind_label(lbl_end);
    }

    fn visit_stmt_var(&mut self, stmt: &StmtVarType) {
        let var_id = *self.src.map_vars.get(stmt.id).unwrap();
        let ty = self.var_ty(var_id);

        let dest = if ty.is_unit() {
            DataDest::Effect
        } else {
            let ty: BytecodeType = ty.into();
            let var_reg = self.gen.add_register(ty);

            self.var_registers.insert(var_id, var_reg);

            DataDest::Reg(var_reg)
        };

        if let Some(ref expr) = stmt.expr {
            self.visit_expr(expr, dest);
        }
    }

    fn visit_stmt_while(&mut self, stmt: &StmtWhileType) {
        let cond_lbl = self.gen.define_label();
        let end_lbl = self.gen.create_label();
        let cond_reg = self.visit_expr(&stmt.cond, DataDest::Alloc);
        self.gen.emit_jump_if_false(cond_reg, end_lbl);
        self.loops.push(LoopLabels::new(cond_lbl, end_lbl));
        self.visit_stmt(&stmt.block);
        self.loops.pop().unwrap();
        self.gen.emit_jump_loop(cond_lbl);
        self.gen.bind_label(end_lbl);
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExprType) {
        self.visit_expr(&stmt.expr, DataDest::Effect);
    }

    fn visit_stmt_return(&mut self, ret: &StmtReturnType) {
        if let Some(ref expr) = ret.expr {
            let result_reg = self.visit_expr(expr, DataDest::Alloc);
            self.emit_ret_value(result_reg);
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

        let return_type: BytecodeType = ret_ty.into();

        match return_type {
            BytecodeType::Bool => self.gen.emit_ret_bool(result_reg),
            BytecodeType::UInt8 => self.gen.emit_ret_uint8(result_reg),
            BytecodeType::Char => self.gen.emit_ret_char(result_reg),
            BytecodeType::Int32 => self.gen.emit_ret_int32(result_reg),
            BytecodeType::Int64 => self.gen.emit_ret_int64(result_reg),
            BytecodeType::Float32 => self.gen.emit_ret_float32(result_reg),
            BytecodeType::Float64 => self.gen.emit_ret_float64(result_reg),
            BytecodeType::Ptr => self.gen.emit_ret_ptr(result_reg),
            BytecodeType::Tuple(_) => self.gen.emit_ret_tuple(result_reg),
        }
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
            ExprLambda(_) => unimplemented!(),
        }
    }

    fn visit_expr_template(&mut self, expr: &ExprTemplateType, dest: DataDest) -> Register {
        let buffer_register = self.ensure_register(dest, BytecodeType::Ptr);
        self.gen.set_position(expr.pos);

        // build StringBuffer::empty() call
        let fct_id = self.vm.vips.fct.string_buffer_empty;
        self.gen
            .emit_invoke_static_ptr(buffer_register, FctDef::fct_id(self.vm, fct_id));

        let part_register = self.gen.add_register(BytecodeType::Ptr);

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
                        .find_trait_method(self.vm, self.vm.vips.stringable_trait, name)
                        .expect("toString() method not found");

                    if ty.reference_type() {
                        self.gen.emit_invoke_direct_ptr(
                            part_register,
                            FctDef::fct_id(self.vm, to_string_id),
                        );
                    } else {
                        self.gen.emit_invoke_static_ptr(
                            part_register,
                            FctDef::fct_id(self.vm, to_string_id),
                        );
                    }
                }
            }

            // build StringBuffer::append() call
            let fct_id = self.vm.vips.fct.string_buffer_append;
            self.gen.emit_push_register(buffer_register);
            self.gen.emit_push_register(part_register);
            self.gen
                .emit_invoke_direct_void(FctDef::fct_id(self.vm, fct_id));
        }

        // build StringBuffer::toString() call
        let fct_id = self.vm.vips.fct.string_buffer_to_string;
        self.gen.emit_push_register(buffer_register);
        self.gen
            .emit_invoke_static_ptr(buffer_register, FctDef::fct_id(self.vm, fct_id));

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

        self.gen.set_position(expr.pos);

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
            self.gen.emit_checked_cast(object, cls_def_id);
            object
        }
    }

    fn visit_expr_if(&mut self, expr: &ExprIfType, dest: DataDest) -> Register {
        let ty = self.ty(expr.id);
        let dest = if ty.is_unit() {
            Register::invalid()
        } else {
            self.ensure_register(dest, ty.into())
        };

        if let Some(ref else_block) = expr.else_block {
            let else_lbl = self.gen.create_label();
            let end_lbl = self.gen.create_label();

            let cond_reg = self.visit_expr(&expr.cond, DataDest::Alloc);
            self.gen.emit_jump_if_false(cond_reg, else_lbl);

            self.visit_expr(&expr.then_block, DataDest::Reg(dest));

            if !expr_always_returns(&expr.then_block) {
                self.gen.emit_jump(end_lbl);
            }

            self.gen.bind_label(else_lbl);
            self.visit_expr(else_block, DataDest::Reg(dest));
            self.gen.bind_label(end_lbl);
        } else {
            let end_lbl = self.gen.create_label();
            let cond_reg = self.visit_expr(&expr.cond, DataDest::Alloc);
            self.gen.emit_jump_if_false(cond_reg, end_lbl);
            self.visit_expr(&expr.then_block, DataDest::Reg(dest));
            self.gen.bind_label(end_lbl);
        }

        dest
    }

    fn visit_expr_block(&mut self, block: &ExprBlockType, dest: DataDest) -> Register {
        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }

        if let Some(ref expr) = block.expr {
            self.visit_expr(expr, dest)
        } else {
            Register::invalid()
        }
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
            self.gen.set_position(expr.pos);
            self.gen.emit_nil_check(obj);
            return Register::invalid();
        }

        let field_bc_ty: BytecodeType = field_ty.into();

        let dest = self.ensure_register(dest, field_bc_ty);
        let obj = self.visit_expr(&expr.lhs, DataDest::Alloc);

        self.gen.set_position(expr.pos);

        match field_bc_ty {
            BytecodeType::UInt8 => self
                .gen
                .emit_load_field_uint8(dest, obj, cls_def_id, field_id),
            BytecodeType::Bool => self
                .gen
                .emit_load_field_bool(dest, obj, cls_def_id, field_id),
            BytecodeType::Char => self
                .gen
                .emit_load_field_char(dest, obj, cls_def_id, field_id),
            BytecodeType::Int32 => self
                .gen
                .emit_load_field_int32(dest, obj, cls_def_id, field_id),
            BytecodeType::Int64 => self
                .gen
                .emit_load_field_int64(dest, obj, cls_def_id, field_id),
            BytecodeType::Float32 => self
                .gen
                .emit_load_field_float32(dest, obj, cls_def_id, field_id),
            BytecodeType::Float64 => self
                .gen
                .emit_load_field_float64(dest, obj, cls_def_id, field_id),
            BytecodeType::Ptr => self
                .gen
                .emit_load_field_ptr(dest, obj, cls_def_id, field_id),
            BytecodeType::Tuple(_) => self
                .gen
                .emit_load_field_tuple(dest, obj, cls_def_id, field_id),
        }

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
            return Register::invalid();
        }

        let ty: BytecodeType = ty.into();
        let dest = self.ensure_register(dest, ty);
        self.gen.emit_load_tuple_element(dest, tuple, tuple_id, idx);

        dest
    }

    fn visit_expr_assert(&mut self, expr: &ExprCallType, dest: DataDest) {
        assert!(dest.is_unit());
        let assert_reg = self.visit_expr(&*expr.args[0], DataDest::Alloc);
        self.gen.set_position(expr.pos);
        self.gen.emit_assert(assert_reg);
    }

    fn visit_expr_call(&mut self, expr: &ExprCallType, dest: DataDest) -> Register {
        if let Some(info) = self.get_intrinsic(expr.id) {
            return self.emit_intrinsic_call(expr, info, dest);
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

        // Allocte object or array for constructor calls
        self.emit_call_allocate(
            expr.pos,
            &call_type,
            &arg_types,
            object_argument,
            &arguments,
        );

        if let Some(obj_reg) = object_argument {
            self.gen.emit_push_register(obj_reg);
        }
        for reg in arguments {
            self.gen.emit_push_register(reg);
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
        self.emit_call_result(&call_type, dest, return_reg, object_argument)
    }

    fn determine_callee(&mut self, call_type: &CallType) -> FctId {
        match *call_type {
            CallType::Method(_, fct_id, _) => {
                let fct = self.vm.fcts.idx(fct_id);
                let fct = fct.read();

                if fct.parent.is_trait() {
                    // This happens for calls like (T: SomeTrait).method()
                    // Find the exact method that is called
                    let trait_id = fct.trait_id();
                    let object_type = match *call_type {
                        CallType::Method(ty, _, _) => ty,
                        _ => unreachable!(),
                    };
                    let object_type = self.specialize_type(object_type);
                    self.find_trait_impl(fct_id, trait_id, object_type)
                } else {
                    fct_id
                }
            }
            CallType::TraitStatic(tp_id, trait_id, trait_fct_id) => {
                let list_id = match tp_id {
                    TypeParamId::Fct(list_id) => list_id,
                    TypeParamId::Class(_) => unimplemented!(),
                };

                let ty = self.fct_type_params[list_id.idx()];
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
            CallType::Method(_, _, _) => {
                let obj_expr = expr.object().expect("method target required");
                let reg = self.visit_expr(obj_expr, DataDest::Alloc);

                Some(reg)
            }
            CallType::Expr(_, _) => Some(self.visit_expr(&expr.callee, DataDest::Alloc)),
            CallType::CtorNew(_, _) => {
                // Need to use new register for allocated object.
                // Otherwise code like `x = SomeClass(x)` would break.
                Some(self.gen.add_register(BytecodeType::Ptr))
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
            CallType::CtorNew(_, _) | CallType::Expr(_, _) | CallType::Method(_, _, _) => 1,
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
                self.visit_expr(arg, DataDest::Effect);
            } else {
                let reg = self.visit_expr(arg, DataDest::Alloc);
                registers.push(reg);
            };
        }

        if callee.variadic_arguments {
            let array_reg =
                self.emit_array_with_variadic_arguments(expr, arg_types, non_variadic_arguments);
            registers.push(array_reg);
        }

        registers
    }

    fn emit_array_with_variadic_arguments(
        &mut self,
        expr: &ExprCallType,
        arg_types: &[BuiltinType],
        non_variadic_arguments: usize,
    ) -> Register {
        let variadic_arguments = expr.args.len() - non_variadic_arguments;

        // We need array of elements
        let element_ty = arg_types.last().cloned().unwrap();
        let ty = self.vm.vips.array_ty(self.vm, element_ty);
        let cls_def_id = specialize_class_ty(self.vm, ty);

        // Store length in a register
        let length_reg = self.gen.add_register(BytecodeType::Int64);
        self.gen
            .emit_const_int64(length_reg, variadic_arguments as i64);

        // Allocate array of given length
        let array_reg = self.gen.add_register(BytecodeType::Ptr);
        self.gen.set_position(expr.pos);
        self.gen.emit_new_array(array_reg, cls_def_id, length_reg);

        let bytecode_ty: BytecodeType = element_ty.into();
        let index_reg = self.gen.add_register(BytecodeType::Int64);

        // Evaluate rest arguments and store them in array
        for (idx, arg) in expr.args.iter().skip(non_variadic_arguments).enumerate() {
            let arg_reg = self.visit_expr(arg, DataDest::Alloc);
            self.gen.emit_const_int64(index_reg, idx as i64);
            self.gen.set_position(expr.pos);
            self.emit_store_array(bytecode_ty, array_reg, index_reg, arg_reg);
        }

        array_reg
    }

    fn emit_call_allocate(
        &mut self,
        pos: Position,
        call_type: &CallType,
        arg_types: &[BuiltinType],
        object_reg: Option<Register>,
        arg_regs: &[Register],
    ) {
        match *call_type {
            CallType::CtorNew(_, _) => {
                let ty = arg_types.first().cloned().unwrap();
                let object_reg = object_reg.unwrap();
                let cls_def_id = specialize_class_ty(self.vm, ty);

                let cls = self.vm.class_defs.idx(cls_def_id);
                let cls = cls.read();

                self.gen.set_position(pos);

                match cls.size {
                    InstanceSize::Fixed(_) => {
                        self.gen.emit_new_object(object_reg, cls_def_id);
                    }
                    InstanceSize::ObjArray
                    | InstanceSize::PrimitiveArray(_)
                    | InstanceSize::UnitArray
                    | InstanceSize::TupleArray(_) => {
                        let length_arg = arg_regs[0];
                        self.gen.emit_new_array(object_reg, cls_def_id, length_arg);
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
        self.gen.set_position(pos);

        match *call_type {
            CallType::Ctor(_, _) | CallType::CtorNew(_, _) => {
                self.gen.emit_invoke_direct_void(fct_def_id);
            }

            CallType::Method(_, _, _) => {
                let is_super_call = expr
                    .object()
                    .map(|object| object.is_super())
                    .unwrap_or(false);

                if is_super_call {
                    self.emit_invoke_direct(return_type, return_reg, fct_def_id);
                } else if fct.is_virtual() {
                    self.emit_invoke_virtual(return_type, return_reg, fct_def_id);
                } else if arg_bytecode_types[0] != BytecodeType::Ptr {
                    self.emit_invoke_static(return_type, return_reg, fct_def_id);
                } else {
                    self.emit_invoke_direct(return_type, return_reg, fct_def_id);
                }
            }
            CallType::ModuleMethod(_, _, _) => {
                if arg_bytecode_types.is_empty() || arg_bytecode_types[0] != BytecodeType::Ptr {
                    self.emit_invoke_static(return_type, return_reg, fct_def_id);
                } else {
                    self.emit_invoke_direct(return_type, return_reg, fct_def_id);
                }
            }
            CallType::Fct(_, _, _) => {
                self.emit_invoke_static(return_type, return_reg, fct_def_id);
            }
            CallType::Expr(_, _) => {
                if fct.is_virtual() {
                    self.emit_invoke_virtual(return_type, return_reg, fct_def_id);
                } else if arg_bytecode_types[0] != BytecodeType::Ptr {
                    self.emit_invoke_static(return_type, return_reg, fct_def_id);
                } else {
                    self.emit_invoke_direct(return_type, return_reg, fct_def_id);
                }
            }
            CallType::Trait(_, _) => unimplemented!(),
            CallType::TraitStatic(_, _, _) => {
                self.emit_invoke_static(return_type, return_reg, fct_def_id);
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
        if call_type.is_ctor_new() || call_type.is_ctor() {
            match dest {
                DataDest::Effect => Register::invalid(),
                DataDest::Alloc => obj_reg.unwrap(),
                DataDest::Reg(dest_reg) => {
                    self.gen.emit_mov_ptr(dest_reg, obj_reg.unwrap());
                    dest_reg
                }
            }
        } else {
            return_reg
        }
    }

    fn emit_mov(&mut self, ty: BytecodeType, dest: Register, src: Register) {
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
        }
    }

    fn emit_load_array(&mut self, ty: BytecodeType, dest: Register, arr: Register, idx: Register) {
        match ty {
            BytecodeType::Bool => self.gen.emit_load_array_bool(dest, arr, idx),
            BytecodeType::UInt8 => self.gen.emit_load_array_uint8(dest, arr, idx),
            BytecodeType::Char => self.gen.emit_load_array_char(dest, arr, idx),
            BytecodeType::Int32 => self.gen.emit_load_array_int32(dest, arr, idx),
            BytecodeType::Int64 => self.gen.emit_load_array_int64(dest, arr, idx),
            BytecodeType::Float32 => self.gen.emit_load_array_float32(dest, arr, idx),
            BytecodeType::Float64 => self.gen.emit_load_array_float64(dest, arr, idx),
            BytecodeType::Ptr => self.gen.emit_load_array_ptr(dest, arr, idx),
            BytecodeType::Tuple(_) => self.gen.emit_load_array_tuple(dest, arr, idx),
        }
    }

    fn emit_invoke_virtual(
        &mut self,
        return_type: BuiltinType,
        return_reg: Register,
        callee_id: FctDefId,
    ) {
        if return_type.is_unit() {
            self.gen.emit_invoke_virtual_void(callee_id);
        } else {
            let return_type: BytecodeType = return_type.into();

            match return_type.into() {
                BytecodeType::Bool => self.gen.emit_invoke_virtual_bool(return_reg, callee_id),
                BytecodeType::UInt8 => self.gen.emit_invoke_virtual_uint8(return_reg, callee_id),
                BytecodeType::Char => self.gen.emit_invoke_virtual_char(return_reg, callee_id),
                BytecodeType::Int32 => self.gen.emit_invoke_virtual_int32(return_reg, callee_id),
                BytecodeType::Int64 => self.gen.emit_invoke_virtual_int64(return_reg, callee_id),
                BytecodeType::Float32 => {
                    self.gen.emit_invoke_virtual_float32(return_reg, callee_id)
                }
                BytecodeType::Float64 => {
                    self.gen.emit_invoke_virtual_float64(return_reg, callee_id)
                }
                BytecodeType::Ptr => self.gen.emit_invoke_virtual_ptr(return_reg, callee_id),
                BytecodeType::Tuple(_) => self.gen.emit_invoke_virtual_tuple(return_reg, callee_id),
            }
        }
    }

    fn emit_invoke_direct(
        &mut self,
        return_type: BuiltinType,
        return_reg: Register,
        callee_id: FctDefId,
    ) {
        if return_type.is_unit() {
            self.gen.emit_invoke_direct_void(callee_id);
        } else {
            let return_type: BytecodeType = return_type.into();

            match return_type {
                BytecodeType::Bool => self.gen.emit_invoke_direct_bool(return_reg, callee_id),
                BytecodeType::UInt8 => self.gen.emit_invoke_direct_uint8(return_reg, callee_id),
                BytecodeType::Char => self.gen.emit_invoke_direct_char(return_reg, callee_id),
                BytecodeType::Int32 => self.gen.emit_invoke_direct_int32(return_reg, callee_id),
                BytecodeType::Int64 => self.gen.emit_invoke_direct_int64(return_reg, callee_id),
                BytecodeType::Float32 => self.gen.emit_invoke_direct_float32(return_reg, callee_id),
                BytecodeType::Float64 => self.gen.emit_invoke_direct_float64(return_reg, callee_id),
                BytecodeType::Ptr => self.gen.emit_invoke_direct_ptr(return_reg, callee_id),
                BytecodeType::Tuple(_) => self.gen.emit_invoke_direct_tuple(return_reg, callee_id),
            }
        }
    }

    fn emit_invoke_static(
        &mut self,
        return_type: BuiltinType,
        return_reg: Register,
        callee_id: FctDefId,
    ) {
        if return_type.is_unit() {
            self.gen.emit_invoke_static_void(callee_id);
        } else {
            let return_type: BytecodeType = return_type.into();

            match return_type.into() {
                BytecodeType::Bool => self.gen.emit_invoke_static_bool(return_reg, callee_id),
                BytecodeType::UInt8 => self.gen.emit_invoke_static_uint8(return_reg, callee_id),
                BytecodeType::Char => self.gen.emit_invoke_static_char(return_reg, callee_id),
                BytecodeType::Int32 => self.gen.emit_invoke_static_int32(return_reg, callee_id),
                BytecodeType::Int64 => self.gen.emit_invoke_static_int64(return_reg, callee_id),
                BytecodeType::Float32 => self.gen.emit_invoke_static_float32(return_reg, callee_id),
                BytecodeType::Float64 => self.gen.emit_invoke_static_float64(return_reg, callee_id),
                BytecodeType::Ptr => self.gen.emit_invoke_static_ptr(return_reg, callee_id),
                BytecodeType::Tuple(_) => self.gen.emit_invoke_static_tuple(return_reg, callee_id),
            }
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
        for reg in arg_regs {
            self.gen.emit_push_register(reg);
        }

        self.gen.set_position(expr.pos);
        match *call_type {
            CallType::Ctor(_, _) => {
                self.gen.emit_invoke_direct_void(fct_def_id);
            }

            _ => unreachable!(),
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

        match ty {
            BytecodeType::Bool => self.gen.emit_mov_bool(dest, var_reg),
            BytecodeType::UInt8 => self.gen.emit_mov_uint8(dest, var_reg),
            BytecodeType::Char => self.gen.emit_mov_char(dest, var_reg),
            BytecodeType::Float64 => self.gen.emit_mov_float64(dest, var_reg),
            BytecodeType::Float32 => self.gen.emit_mov_float32(dest, var_reg),
            BytecodeType::Int32 => self.gen.emit_mov_int32(dest, var_reg),
            BytecodeType::Int64 => self.gen.emit_mov_int64(dest, var_reg),
            BytecodeType::Ptr => self.gen.emit_mov_ptr(dest, var_reg),
            BytecodeType::Tuple(tuple_id) => self.gen.emit_mov_tuple(dest, var_reg, tuple_id),
        }
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
            let reg = self.visit_expr(value, DataDest::Alloc);
            values.push(reg);
        }

        for value in values {
            self.gen.emit_push_register(value);
        }

        self.gen.emit_new_tuple(result, tuple_id);

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
        self.gen.set_position(expr.pos);

        if function_return_type_bc == BytecodeType::Ptr {
            self.emit_invoke_direct(function_return_type, dest, callee_def_id);
        } else {
            self.emit_invoke_static(function_return_type, dest, callee_def_id);
        }

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
            self.gen.add_register(function_return_type_bc)
        };

        self.gen.emit_push_register(lhs);
        self.gen.emit_push_register(rhs);

        self.gen.set_position(expr.pos);

        if lhs_type == BytecodeType::Ptr {
            self.emit_invoke_direct(function_return_type, result, callee_def_id);
        } else {
            self.emit_invoke_static(function_return_type, result, callee_def_id);
        }

        match expr.op {
            BinOp::Cmp(CmpOp::Eq) => assert_eq!(result, dest),
            BinOp::Cmp(CmpOp::Ne) => {
                assert_eq!(result, dest);
                self.gen.emit_not_bool(dest, dest);
            }

            BinOp::Cmp(op) => {
                assert_ne!(result, dest);
                let zero = self.gen.add_register(BytecodeType::Int32);
                self.gen.emit_const_int32(zero, 0);

                match op {
                    CmpOp::Lt => self.gen.emit_test_lt_int32(dest, result, zero),
                    CmpOp::Le => self.gen.emit_test_le_int32(dest, result, zero),
                    CmpOp::Gt => self.gen.emit_test_gt_int32(dest, result, zero),
                    CmpOp::Ge => self.gen.emit_test_ge_int32(dest, result, zero),
                    CmpOp::Eq | CmpOp::Ne | CmpOp::Is | CmpOp::IsNot => unreachable!(),
                }
            }
            _ => assert_eq!(result, dest),
        }

        dest
    }

    fn emit_intrinsic_call(
        &mut self,
        expr: &ExprCallType,
        info: IntrinsicInfo,
        dest: DataDest,
    ) -> Register {
        let intrinsic = info.intrinsic;

        if let Some(object) = expr.object() {
            match expr.args.len() {
                0 => self.emit_intrinsic_un(object, info, expr.pos, dest),
                1 => self.emit_intrinsic_bin(object, &expr.args[0], info, None, expr.pos, dest),
                2 => {
                    assert_eq!(intrinsic, Intrinsic::GenericArraySet);
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

                Intrinsic::GenericArrayGet => {
                    self.emit_intrinsic_bin(&expr.callee, &expr.args[0], info, None, expr.pos, dest)
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
                    }

                    dest
                }

                _ => panic!("unimplemented intrinsic {:?}", intrinsic),
            }
        }
    }

    fn emit_bin_intrinsic<F>(
        &mut self,
        lhs: &Expr,
        rhs: &Expr,
        dest: DataDest,
        return_type: BytecodeType,
        fct: F,
    ) -> Register
    where
        F: FnOnce(&mut AstBytecodeGen, Register, Register, Register),
    {
        if dest.is_effect() {
            self.visit_expr(lhs, DataDest::Effect);
            self.visit_expr(rhs, DataDest::Effect);
            return Register::invalid();
        }

        let dest = self.ensure_register(dest, return_type);

        let lhs_reg = self.visit_expr(lhs, DataDest::Alloc);
        let rhs_reg = self.visit_expr(rhs, DataDest::Alloc);

        fct(self, dest, lhs_reg, rhs_reg);

        dest
    }

    fn emit_un_intrinsic<F>(
        &mut self,
        opnd: &Expr,
        dest: DataDest,
        return_type: BytecodeType,
        fct: F,
    ) -> Register
    where
        F: FnOnce(&mut AstBytecodeGen, Register, Register),
    {
        if dest.is_effect() {
            self.visit_expr(opnd, DataDest::Effect);
            return Register::invalid();
        }

        let dest = self.ensure_register(dest, return_type);

        let opnd = self.visit_expr(opnd, DataDest::Alloc);

        fct(self, dest, opnd);

        dest
    }

    fn emit_bin_is(&mut self, expr: &ExprBinType, dest: DataDest) -> Register {
        if dest.is_effect() {
            self.visit_expr(&expr.lhs, dest);
            self.visit_expr(&expr.rhs, dest);
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
            cmp_lhs_reg = self.gen.add_register(cmp_ty);
            cmp_rhs_reg = self.gen.add_register(cmp_ty);

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

        dest
    }

    fn emit_bin_or(&mut self, expr: &ExprBinType, dest: DataDest) -> Register {
        if dest.is_effect() {
            let end_lbl = self.gen.create_label();
            let dest = self.gen.add_register(BytecodeType::Bool);

            self.visit_expr(&expr.lhs, DataDest::Reg(dest));
            self.gen.emit_jump_if_true(dest, end_lbl);
            self.visit_expr(&expr.rhs, DataDest::Effect);
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
            let dest = self.gen.add_register(BytecodeType::Bool);

            self.visit_expr(&expr.lhs, DataDest::Reg(dest));
            self.gen.emit_jump_if_false(dest, end_lbl);
            self.visit_expr(&expr.rhs, DataDest::Effect);
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

        let ty = self.src.ty(arr.id());
        let ty = self.specialize_type(ty);
        let ty = ty.type_params(self.vm);
        let ty = ty[0];
        let ty: Option<BytecodeType> = if ty.is_unit() { None } else { Some(ty.into()) };

        let arr = self.visit_expr(arr, DataDest::Alloc);
        let idx = self.visit_expr(idx, DataDest::Alloc);
        let src = self.visit_expr(src, DataDest::Alloc);

        self.gen.set_position(pos);

        if ty.is_none() {
            self.gen.emit_array_bound_check(arr, idx);
            return Register::invalid();
        }

        let ty = ty.unwrap();
        self.emit_store_array(ty, arr, idx, src);

        Register::invalid()
    }

    fn emit_store_array(&mut self, ty: BytecodeType, arr: Register, idx: Register, src: Register) {
        match ty {
            BytecodeType::UInt8 => self.gen.emit_store_array_uint8(src, arr, idx),
            BytecodeType::Bool => self.gen.emit_store_array_bool(src, arr, idx),
            BytecodeType::Char => self.gen.emit_store_array_char(src, arr, idx),
            BytecodeType::Int32 => self.gen.emit_store_array_int32(src, arr, idx),
            BytecodeType::Int64 => self.gen.emit_store_array_int64(src, arr, idx),
            BytecodeType::Float32 => self.gen.emit_store_array_float32(src, arr, idx),
            BytecodeType::Float64 => self.gen.emit_store_array_float64(src, arr, idx),
            BytecodeType::Ptr => self.gen.emit_store_array_ptr(src, arr, idx),
            BytecodeType::Tuple(_) => self.gen.emit_store_array_tuple(src, arr, idx),
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
                Intrinsic::GenericArrayLen | Intrinsic::StrLen => {
                    let src = self.visit_expr(opnd, DataDest::Alloc);
                    self.gen.set_position(pos);
                    self.gen.emit_nil_check(src);
                    return Register::invalid();
                }

                _ => {}
            }

            self.visit_expr(opnd, DataDest::Effect);
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
            Intrinsic::GenericArrayLen | Intrinsic::StrLen => {
                self.gen.set_position(pos);
                self.gen.emit_array_length(dest, src);
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
                self.gen.emit_invoke_static_float32(
                    dest,
                    FctDef::fct_id(self.vm, info.fct_id.unwrap()),
                );
            }
            Intrinsic::Float64Sqrt => {
                self.gen.emit_push_register(src);
                self.gen.emit_invoke_static_float64(
                    dest,
                    FctDef::fct_id(self.vm, info.fct_id.unwrap()),
                );
            }
            Intrinsic::Int32CountZeroBits
            | Intrinsic::Int32CountZeroBitsLeading
            | Intrinsic::Int32CountZeroBitsTrailing
            | Intrinsic::Int32CountOneBits
            | Intrinsic::Int32CountOneBitsLeading
            | Intrinsic::Int32CountOneBitsTrailing => {
                self.gen.emit_push_register(src);
                self.gen
                    .emit_invoke_static_int32(dest, FctDef::fct_id(self.vm, info.fct_id.unwrap()));
            }
            Intrinsic::Int64CountZeroBits
            | Intrinsic::Int64CountZeroBitsLeading
            | Intrinsic::Int64CountZeroBitsTrailing
            | Intrinsic::Int64CountOneBits
            | Intrinsic::Int64CountOneBitsLeading
            | Intrinsic::Int64CountOneBitsTrailing => {
                self.gen.emit_push_register(src);
                self.gen
                    .emit_invoke_static_int64(dest, FctDef::fct_id(self.vm, info.fct_id.unwrap()));
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
            Intrinsic::GenericArrayGet | Intrinsic::StrGet => {
                let ty = self.src.ty(lhs.id());
                let ty = self.specialize_type(ty);
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

                self.gen.set_position(pos);

                if ty.is_none() {
                    self.gen.emit_array_bound_check(arr, idx);
                    return Register::invalid();
                }

                let ty = ty.unwrap();
                let dest = dest.unwrap();

                match ty {
                    BytecodeType::UInt8 => self.gen.emit_load_array_uint8(dest, arr, idx),
                    BytecodeType::Bool => self.gen.emit_load_array_bool(dest, arr, idx),
                    BytecodeType::Char => self.gen.emit_load_array_char(dest, arr, idx),
                    BytecodeType::Int32 => self.gen.emit_load_array_int32(dest, arr, idx),
                    BytecodeType::Int64 => self.gen.emit_load_array_int64(dest, arr, idx),
                    BytecodeType::Float32 => self.gen.emit_load_array_float32(dest, arr, idx),
                    BytecodeType::Float64 => self.gen.emit_load_array_float64(dest, arr, idx),
                    BytecodeType::Ptr => self.gen.emit_load_array_ptr(dest, arr, idx),
                    BytecodeType::Tuple(_) => self.gen.emit_load_array_tuple(dest, arr, idx),
                }
                return dest;
            }

            _ => {}
        }

        if dest.is_effect() {
            self.visit_expr(lhs, dest);
            self.visit_expr(rhs, dest);
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
                    let result = self.gen.add_register(BytecodeType::Int64);
                    self.gen.emit_sub_int64(result, lhs_reg, rhs_reg);
                    self.gen.emit_cast_int64_to_int32(dest, result);
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
            Intrinsic::Int32Div => {
                self.gen.set_position(pos);
                self.gen.emit_div_int32(dest, lhs_reg, rhs_reg)
            }
            Intrinsic::Int32Mod => {
                self.gen.set_position(pos);
                self.gen.emit_mod_int32(dest, lhs_reg, rhs_reg)
            }
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
            Intrinsic::Int64Div => {
                self.gen.set_position(pos);
                self.gen.emit_div_int64(dest, lhs_reg, rhs_reg)
            }
            Intrinsic::Int64Mod => {
                self.gen.set_position(pos);
                self.gen.emit_mod_int64(dest, lhs_reg, rhs_reg)
            }
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
                Intrinsic::GenericArraySet => {
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

            let callee_id = FctDef::fct_id_types(
                self.vm,
                fct_id,
                obj_ty.type_params(self.vm),
                TypeList::empty(),
            );
            self.gen.emit_invoke_direct_void(callee_id);
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

        self.gen.set_position(expr.pos);

        if ty.is_none() {
            self.gen.emit_nil_check(obj);
            return;
        }

        match ty.unwrap() {
            BytecodeType::UInt8 => self.gen.emit_store_field_uint8(src, obj, cls_id, field_id),
            BytecodeType::Bool => self.gen.emit_store_field_bool(src, obj, cls_id, field_id),
            BytecodeType::Char => self.gen.emit_store_field_char(src, obj, cls_id, field_id),
            BytecodeType::Int32 => self.gen.emit_store_field_int32(src, obj, cls_id, field_id),
            BytecodeType::Int64 => self.gen.emit_store_field_int64(src, obj, cls_id, field_id),
            BytecodeType::Float32 => self
                .gen
                .emit_store_field_float32(src, obj, cls_id, field_id),
            BytecodeType::Float64 => self
                .gen
                .emit_store_field_float64(src, obj, cls_id, field_id),
            BytecodeType::Ptr => self.gen.emit_store_field_ptr(src, obj, cls_id, field_id),
            BytecodeType::Tuple(_) => self.gen.emit_store_field_tuple(src, obj, cls_id, field_id),
        }
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
            let ty: BytecodeType = glob.ty.into();
            match ty {
                BytecodeType::Bool => self.gen.emit_store_global_bool(src, gid),
                BytecodeType::UInt8 => self.gen.emit_store_global_uint8(src, gid),
                BytecodeType::Char => self.gen.emit_store_global_char(src, gid),
                BytecodeType::Int32 => self.gen.emit_store_global_int32(src, gid),
                BytecodeType::Int64 => self.gen.emit_store_global_int64(src, gid),
                BytecodeType::Float32 => self.gen.emit_store_global_float32(src, gid),
                BytecodeType::Float64 => self.gen.emit_store_global_float64(src, gid),
                BytecodeType::Ptr => self.gen.emit_store_global_ptr(src, gid),
                BytecodeType::Tuple(_) => self.gen.emit_store_global_tuple(src, gid),
            }
        }
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
            &IdentType::ModuleMethod(_, _) | &IdentType::ModuleMethodType(_, _, _) => {
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

        match ty {
            BytecodeType::Bool => self.gen.emit_load_global_bool(dest, gid),
            BytecodeType::UInt8 => self.gen.emit_load_global_uint8(dest, gid),
            BytecodeType::Char => self.gen.emit_load_global_char(dest, gid),
            BytecodeType::Int32 => self.gen.emit_load_global_int32(dest, gid),
            BytecodeType::Int64 => self.gen.emit_load_global_int64(dest, gid),
            BytecodeType::Float32 => self.gen.emit_load_global_float32(dest, gid),
            BytecodeType::Float64 => self.gen.emit_load_global_float64(dest, gid),
            BytecodeType::Ptr => self.gen.emit_load_global_ptr(dest, gid),
            BytecodeType::Tuple(_) => self.gen.emit_load_global_tuple(dest, gid),
        }

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

        if dest != var_reg {
            match ty {
                BytecodeType::Bool => self.gen.emit_mov_bool(dest, var_reg),
                BytecodeType::UInt8 => self.gen.emit_mov_uint8(dest, var_reg),
                BytecodeType::Char => self.gen.emit_mov_char(dest, var_reg),
                BytecodeType::Int32 => self.gen.emit_mov_int32(dest, var_reg),
                BytecodeType::Int64 => self.gen.emit_mov_int64(dest, var_reg),
                BytecodeType::Float32 => self.gen.emit_mov_float32(dest, var_reg),
                BytecodeType::Float64 => self.gen.emit_mov_float64(dest, var_reg),
                BytecodeType::Ptr => self.gen.emit_mov_ptr(dest, var_reg),
                BytecodeType::Tuple(tuple_id) => self.gen.emit_mov_tuple(dest, var_reg, tuple_id),
            }
        }

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
            DataDest::Effect | DataDest::Alloc => self.gen.add_register(ty),
            DataDest::Reg(reg) => reg,
        }
    }

    fn determine_call_type_params(&self, call_type: &CallType) -> (TypeList, TypeList) {
        let cls_type_params;
        let fct_type_params;

        match *call_type {
            CallType::Ctor(ty, _) | CallType::CtorNew(ty, _) => {
                cls_type_params = ty.type_params(self.vm);
                fct_type_params = TypeList::empty();
            }

            CallType::Method(ty, _, ref type_params) => {
                let ty = self.specialize_type(ty);

                cls_type_params = ty.type_params(self.vm);
                fct_type_params = type_params.clone();
            }

            CallType::ModuleMethod(ty, _, ref type_params) => {
                let ty = self.specialize_type(ty);

                cls_type_params = ty.type_params(self.vm);
                fct_type_params = type_params.clone();
            }

            CallType::Fct(_, ref cls_tps, ref fct_tps) => {
                cls_type_params = cls_tps.clone();
                fct_type_params = fct_tps.clone();
            }

            CallType::Expr(ty, _) => {
                let ty = self.specialize_type(ty);

                cls_type_params = ty.type_params(self.vm);
                fct_type_params = TypeList::empty();
            }

            CallType::Trait(_, _) => unimplemented!(),

            CallType::TraitStatic(_, _, _) => {
                cls_type_params = TypeList::empty();
                fct_type_params = TypeList::empty();
            }

            CallType::Intrinsic(_) => unreachable!(),
        }

        (cls_type_params, fct_type_params)
    }

    fn specialize_call(&self, fct: &Fct, call_type: &CallType) -> FctDefId {
        let (cls_type_params, fct_type_params) = self.determine_call_type_params(call_type);

        let cls_type_params = TypeList::with(
            cls_type_params
                .iter()
                .map(|ty| self.specialize_type(ty))
                .collect::<Vec<_>>(),
        );

        let fct_type_params = TypeList::with(
            fct_type_params
                .iter()
                .map(|ty| self.specialize_type(ty))
                .collect::<Vec<_>>(),
        );

        FctDef::with(self.vm, fct, cls_type_params, fct_type_params)
    }

    fn specialize_type_for_call(&self, call_type: &CallType, ty: BuiltinType) -> BuiltinType {
        let ty = match *call_type {
            CallType::Fct(_, ref cls_type_params, ref fct_type_params) => {
                specialize_type(self.vm, ty, cls_type_params, fct_type_params)
            }

            CallType::Method(cls_ty, _, ref type_params) => {
                let cls_type_params = cls_ty.type_params(self.vm);
                specialize_type(self.vm, ty, &cls_type_params, type_params)
            }

            CallType::ModuleMethod(cls_ty, _, ref type_params) => {
                let cls_type_params = cls_ty.type_params(self.vm);
                specialize_type(self.vm, ty, &cls_type_params, type_params)
            }

            CallType::Ctor(cls_ty, _) | CallType::CtorNew(cls_ty, _) => {
                let cls_type_params = cls_ty.type_params(self.vm);
                specialize_type(self.vm, ty, &cls_type_params, &TypeList::empty())
            }

            CallType::Expr(object_ty, _) => {
                let type_params = object_ty.type_params(self.vm);
                specialize_type(self.vm, ty, &type_params, &TypeList::empty())
            }

            CallType::Trait(_, _) => unimplemented!(),
            CallType::TraitStatic(_, _, _) => {
                specialize_type(self.vm, ty, &TypeList::empty(), &TypeList::empty())
            }
            CallType::Intrinsic(_) => unreachable!(),
        };

        self.specialize_type(ty)
    }

    fn specialize_type(&self, ty: BuiltinType) -> BuiltinType {
        specialize_type(self.vm, ty, self.cls_type_params, self.fct_type_params)
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
    // Do not store result. Only interested in side-effects of
    // expression.
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
