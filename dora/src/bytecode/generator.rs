use dora_parser::lexer::position::Position;
use std::collections::HashMap;

use dora_parser::ast::Expr::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::*;
use dora_parser::lexer::token::{FloatSuffix, IntSuffix};

use crate::bytecode::{BytecodeFunction, BytecodeType, BytecodeWriter, Label, Register};
use crate::semck::expr_block_always_returns;
use crate::semck::specialize::{specialize_class_id_params, specialize_class_ty, specialize_type};
use crate::ty::{BuiltinType, TypeList};
use crate::vm::{CallType, Fct, FctId, FctKind, FctSrc, IdentType, Intrinsic, VarId, VM};

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
            let var_id = self.src.var_self().id;
            let reg = self.gen.add_register(BytecodeType::Ptr);
            self.var_registers.insert(var_id, reg);
            arguments += 1;
        }

        for param in &self.ast.params {
            let var_id = *self.src.map_vars.get(param.id).unwrap();
            let var = &self.src.vars[var_id];
            let ty: BytecodeType = self.specialize_type(var.ty).into();
            let reg = self.gen.add_register(ty);
            self.var_registers.insert(var_id, reg);
            arguments += 1;
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

    // TODO - implement other statements
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match *stmt {
            StmtReturn(ref ret) => self.visit_stmt_return(ret),
            StmtBreak(ref stmt) => self.visit_stmt_break(stmt),
            StmtContinue(ref stmt) => self.visit_stmt_continue(stmt),
            StmtExpr(ref expr) => self.visit_stmt_expr(expr),
            StmtVar(ref stmt) => self.visit_stmt_var(stmt),
            StmtWhile(ref stmt) => self.visit_stmt_while(stmt),
            StmtLoop(ref stmt) => self.visit_stmt_loop(stmt),
            StmtThrow(ref stmt) => self.visit_stmt_throw(stmt),
            // StmtDefer(ref stmt) => {},
            // StmtDo(ref stmt) => {},
            // StmtSpawn(ref stmt) => {},
            // StmtFor(ref stmt) => {},
            _ => unimplemented!(),
        }
    }

    fn visit_stmt_var(&mut self, stmt: &StmtVarType) {
        let var_id = *self.src.map_vars.get(stmt.id).unwrap();
        let ty: BytecodeType = self.specialize_type(self.src.vars[var_id].ty).into();
        let var_reg = self.gen.add_register(ty);

        self.var_registers.insert(var_id, var_reg);

        if let Some(ref expr) = stmt.expr {
            self.visit_expr(expr, DataDest::Reg(var_reg));
        }
    }

    fn visit_stmt_while(&mut self, stmt: &StmtWhileType) {
        let cond_lbl = self.gen.define_label();
        let end_lbl = self.gen.create_label();
        let cond_reg = self.visit_expr(&stmt.cond, DataDest::Alloc);
        self.gen.emit_jump_if_false(cond_reg, end_lbl);
        self.loops.push(LoopLabels::new(cond_lbl, end_lbl));
        self.visit_stmt(&stmt.block);
        self.loops.pop();
        self.gen.emit_jump_loop(cond_lbl);
        self.gen.bind_label(end_lbl);
    }

    fn visit_stmt_loop(&mut self, stmt: &StmtLoopType) {
        let start_lbl = self.gen.define_label();
        let end_lbl = self.gen.create_label();
        self.loops.push(LoopLabels::new(start_lbl, end_lbl));
        self.visit_stmt(&stmt.block);
        self.loops.pop();
        self.gen.emit_jump(start_lbl);
        self.gen.bind_label(end_lbl);
    }

    fn visit_stmt_throw(&mut self, stmt: &StmtThrowType) {
        let exception_reg = self.visit_expr(&stmt.expr, DataDest::Alloc);
        self.gen.set_position(stmt.pos);
        self.gen.emit_throw(exception_reg);
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
        if BuiltinType::Unit == self.fct.return_type {
            self.gen.emit_ret_void();
            return;
        }

        let return_type: BytecodeType = self.specialize_type(self.fct.return_type).into();

        match return_type {
            BytecodeType::Bool => self.gen.emit_ret_bool(result_reg),
            BytecodeType::Byte => self.gen.emit_ret_byte(result_reg),
            BytecodeType::Char => self.gen.emit_ret_char(result_reg),
            BytecodeType::Int => self.gen.emit_ret_int(result_reg),
            BytecodeType::Long => self.gen.emit_ret_long(result_reg),
            BytecodeType::Float => self.gen.emit_ret_float(result_reg),
            BytecodeType::Double => self.gen.emit_ret_double(result_reg),
            BytecodeType::Ptr => self.gen.emit_ret_ptr(result_reg),
        }
    }

    fn visit_stmt_break(&mut self, _stmt: &StmtBreakType) {
        let end = self.loops.pop().unwrap().end;
        self.gen.emit_jump(end);
    }

    fn visit_stmt_continue(&mut self, _stmt: &StmtContinueType) {
        let cond = self.loops.last().unwrap().cond;
        self.gen.emit_jump_loop(cond);
    }

    // TODO - implement other expressions
    fn visit_expr(&mut self, expr: &Expr, dest: DataDest) -> Register {
        match *expr {
            ExprUn(ref un) => self.visit_expr_un(un, dest),
            ExprBin(ref bin) => self.visit_expr_bin(bin, dest),
            ExprDot(ref field) => self.visit_expr_dot(field, dest),
            ExprBlock(ref block) => self.visit_expr_block(block, dest),
            ExprIf(ref expr) => self.visit_expr_if(expr, dest),
            // ExprArray(ref array) => {},
            ExprLitChar(ref lit) => self.visit_expr_lit_char(lit, dest),
            ExprLitInt(ref lit) => self.visit_expr_lit_int(lit, dest),
            ExprLitFloat(ref lit) => self.visit_expr_lit_float(lit, dest),
            ExprLitStr(ref lit) => self.visit_expr_lit_string(lit, dest),
            // ExprLitStruct(ref lit) => {},
            ExprLitBool(ref lit) => self.visit_expr_lit_bool(lit, dest),
            ExprIdent(ref ident) => self.visit_expr_ident(ident, dest),
            ExprCall(ref call) => self.visit_expr_call(call, dest),
            ExprDelegation(ref call) => self.visit_expr_delegation(call, dest),
            ExprSelf(ref selfie) => self.visit_expr_self(selfie, dest),
            // ExprSuper(ref expr) => {},
            ExprNil(ref nil) => self.visit_expr_nil(nil, dest),
            // ExprConv(ref expr) => {},
            // ExprTry(ref expr) => {},
            // ExprLambda(ref expr) => {},
            _ => unimplemented!(),
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
            self.gen.emit_jump(end_lbl);

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
        let (class, field_id) = {
            let ident_type = self.src.map_idents.get(expr.id).unwrap();

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
        let ty: BytecodeType = field.ty.into();

        let dest = self.ensure_register(dest, ty);
        let obj = self.visit_expr(&expr.lhs, DataDest::Alloc);

        self.gen.set_position(expr.pos);

        match ty {
            BytecodeType::Byte => self.gen.emit_load_field_byte(dest, obj, cls_id, field_id),
            BytecodeType::Bool => self.gen.emit_load_field_bool(dest, obj, cls_id, field_id),
            BytecodeType::Char => self.gen.emit_load_field_char(dest, obj, cls_id, field_id),
            BytecodeType::Int => self.gen.emit_load_field_int(dest, obj, cls_id, field_id),
            BytecodeType::Long => self.gen.emit_load_field_long(dest, obj, cls_id, field_id),
            BytecodeType::Float => self.gen.emit_load_field_float(dest, obj, cls_id, field_id),
            BytecodeType::Double => self.gen.emit_load_field_double(dest, obj, cls_id, field_id),
            BytecodeType::Ptr => self.gen.emit_load_field_ptr(dest, obj, cls_id, field_id),
        }

        dest
    }

    fn visit_expr_assert(&mut self, expr: &ExprCallType, _dest: DataDest) {
        let lbl_assert = self.gen.create_label();

        let assert_reg = self.visit_expr(&*expr.args[0], DataDest::Alloc);
        self.gen.emit_jump_if_true(assert_reg, lbl_assert);

        let cls_id = self.vm.vips.error_class;
        let cls_ty = self.vm.cls(cls_id);
        let cls = self.vm.classes.idx(cls_id);
        let cls = cls.read();

        let fct_id = cls.constructor.expect("constructor is missing");
        let fct = self.vm.fcts.idx(fct_id);
        let fct = fct.read();

        let arg_types = fct
            .params_with_self()
            .iter()
            .map(|&arg| {
                self.specialize_type_for_call(&CallType::CtorNew(cls_ty, fct_id), arg)
                    .into()
            })
            .collect::<Vec<BytecodeType>>();
        let num_args = arg_types.len();

        let start_reg = self.gen.add_register_chain(&arg_types);
        let cls_id = specialize_class_id_params(self.vm, cls_id, &TypeList::Empty);

        self.gen.set_position(expr.pos);
        self.gen.emit_new_object(start_reg, cls_id);

        let error_string_reg = start_reg.offset(1);
        self.gen
            .emit_const_string(error_string_reg, "assert failed".to_string());

        self.gen
            .emit_invoke_direct_void(fct_id, start_reg, num_args);

        self.gen.emit_throw(start_reg);

        self.gen.bind_label(lbl_assert);
    }

    fn visit_expr_call(&mut self, expr: &ExprCallType, dest: DataDest) -> Register {
        if let Some(intrinsic) = self.get_intrinsic(expr.id) {
            match intrinsic {
                Intrinsic::Assert => self.visit_expr_assert(expr, dest),
                _ => unimplemented!(),
            }
            return Register::invalid();
        }

        let call_type = self.src.map_calls.get(expr.id).unwrap().clone();
        let fct_id = call_type.fct_id().unwrap();

        let fct = self.vm.fcts.idx(fct_id);
        let fct = fct.read();

        let callee_id = if fct.kind.is_definition() {
            unimplemented!()
        } else {
            fct_id
        };

        let callee = self.vm.fcts.idx(callee_id);
        let callee = callee.read();

        if let FctKind::Builtin(_intrinsic) = callee.kind {
            unimplemented!()
        }

        let return_type = if dest.is_effect() {
            BuiltinType::Unit
        } else {
            self.specialize_type_for_call(&call_type, callee.return_type)
        };
        let arg_types = callee
            .params_with_self()
            .iter()
            .map(|&arg| self.specialize_type_for_call(&call_type, arg).into())
            .collect::<Vec<BytecodeType>>();
        let num_args = arg_types.len();

        let return_reg = if return_type.is_unit() {
            Register::invalid()
        } else {
            self.ensure_register(dest, return_type.into())
        };

        let start_reg = if num_args > 0 {
            self.gen.add_register_chain(&arg_types)
        } else {
            Register::zero()
        };
        self.gen.set_position(expr.pos);
        let arg_start_reg = if let CallType::CtorNew(ty, _) = &*call_type {
            let cls_id = specialize_class_ty(self.vm, *ty);
            self.gen.emit_new_object(start_reg, cls_id);
            start_reg.offset(1)
        } else if callee.has_self() {
            let self_id = self.src.var_self().id;
            let self_reg = self.var_reg(self_id);

            self.gen.emit_mov_ptr(start_reg, self_reg);
            start_reg.offset(1)
        } else {
            start_reg
        };

        for (idx, arg) in expr.args.iter().enumerate() {
            let arg_reg = arg_start_reg.offset(idx);
            self.visit_expr(arg, DataDest::Reg(arg_reg));
        }
        self.gen.set_position(expr.pos);

        match *call_type {
            CallType::Ctor(_, _) | CallType::CtorNew(_, _) => {
                self.gen
                    .emit_invoke_direct_void(callee_id, start_reg, num_args);
            }

            CallType::Method(_, _, _) => {
                if return_type.is_unit() {
                    self.gen
                        .emit_invoke_direct_void(callee_id, start_reg, num_args);
                } else {
                    let return_type: BytecodeType = return_type.into();

                    match return_type.into() {
                        BytecodeType::Bool => self
                            .gen
                            .emit_invoke_direct_bool(return_reg, callee_id, start_reg, num_args),
                        BytecodeType::Byte => self
                            .gen
                            .emit_invoke_direct_byte(return_reg, callee_id, start_reg, num_args),
                        BytecodeType::Char => self
                            .gen
                            .emit_invoke_direct_char(return_reg, callee_id, start_reg, num_args),
                        BytecodeType::Int => self
                            .gen
                            .emit_invoke_direct_int(return_reg, callee_id, start_reg, num_args),
                        BytecodeType::Long => self
                            .gen
                            .emit_invoke_direct_long(return_reg, callee_id, start_reg, num_args),
                        BytecodeType::Float => self
                            .gen
                            .emit_invoke_direct_float(return_reg, callee_id, start_reg, num_args),
                        BytecodeType::Double => self
                            .gen
                            .emit_invoke_direct_double(return_reg, callee_id, start_reg, num_args),
                        BytecodeType::Ptr => self
                            .gen
                            .emit_invoke_direct_ptr(return_reg, callee_id, start_reg, num_args),
                    }
                }
            }
            CallType::Expr(_, _) => unimplemented!(),

            CallType::Fct(_, _, _) => {
                if return_type.is_unit() {
                    self.gen
                        .emit_invoke_static_void(callee_id, start_reg, num_args);
                } else {
                    let return_type: BytecodeType = return_type.into();

                    match return_type.into() {
                        BytecodeType::Bool => self
                            .gen
                            .emit_invoke_static_bool(return_reg, callee_id, start_reg, num_args),
                        BytecodeType::Byte => self
                            .gen
                            .emit_invoke_static_byte(return_reg, callee_id, start_reg, num_args),
                        BytecodeType::Char => self
                            .gen
                            .emit_invoke_static_char(return_reg, callee_id, start_reg, num_args),
                        BytecodeType::Int => self
                            .gen
                            .emit_invoke_static_int(return_reg, callee_id, start_reg, num_args),
                        BytecodeType::Long => self
                            .gen
                            .emit_invoke_static_long(return_reg, callee_id, start_reg, num_args),
                        BytecodeType::Float => self
                            .gen
                            .emit_invoke_static_float(return_reg, callee_id, start_reg, num_args),
                        BytecodeType::Double => self
                            .gen
                            .emit_invoke_static_double(return_reg, callee_id, start_reg, num_args),
                        BytecodeType::Ptr => self
                            .gen
                            .emit_invoke_static_ptr(return_reg, callee_id, start_reg, num_args),
                    }
                }
            }

            CallType::Trait(_, _) => unimplemented!(),
            CallType::TraitStatic(_, _, _) => unimplemented!(),
            CallType::Intrinsic(_) => unreachable!(),
        }

        if call_type.is_ctor_new() || call_type.is_ctor() {
            if dest.is_reg() {
                let return_reg = self.ensure_register(dest, BytecodeType::Ptr);
                self.gen.emit_mov_ptr(return_reg, start_reg);
                return_reg
            } else {
                start_reg
            }
        } else {
            return_reg
        }
    }

    fn visit_expr_delegation(&mut self, expr: &ExprDelegationType, dest: DataDest) -> Register {
        assert!(dest.is_effect());
        let call_type = self.src.map_calls.get(expr.id).unwrap().clone();
        let fct_id = call_type.fct_id().unwrap();

        let callee_id = fct_id;
        let callee = self.vm.fcts.idx(callee_id);
        let callee = callee.read();

        assert!(callee.return_type.is_unit());
        let arg_types = callee
            .params_with_self()
            .iter()
            .map(|&arg| self.specialize_type_for_call(&call_type, arg).into())
            .collect::<Vec<BytecodeType>>();
        let num_args = arg_types.len();

        assert!(num_args > 0);
        let start_reg = self.gen.add_register_chain(&arg_types);

        assert!(callee.has_self());
        let self_id = self.src.var_self().id;
        let self_reg = self.var_reg(self_id);
        self.gen.emit_mov_ptr(start_reg, self_reg);
        let arg_start_reg = start_reg.offset(1);

        for (idx, arg) in expr.args.iter().enumerate() {
            let arg_reg = arg_start_reg.offset(idx);
            self.visit_expr(arg, DataDest::Reg(arg_reg));
        }

        self.gen.set_position(expr.pos);
        match *call_type {
            CallType::Ctor(_, _) => {
                self.gen
                    .emit_invoke_direct_void(callee_id, start_reg, num_args);
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

    fn visit_expr_self(&mut self, _selfie: &ExprSelfType, dest: DataDest) -> Register {
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
            BytecodeType::Byte => self.gen.emit_mov_byte(dest, var_reg),
            BytecodeType::Char => self.gen.emit_mov_char(dest, var_reg),
            BytecodeType::Double => self.gen.emit_mov_double(dest, var_reg),
            BytecodeType::Float => self.gen.emit_mov_float(dest, var_reg),
            BytecodeType::Int => self.gen.emit_mov_int(dest, var_reg),
            BytecodeType::Long => self.gen.emit_mov_long(dest, var_reg),
            BytecodeType::Ptr => self.gen.emit_mov_ptr(dest, var_reg),
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

    fn visit_expr_lit_int(&mut self, lit: &ExprLitIntType, dest: DataDest) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

        let ty = match lit.suffix {
            IntSuffix::Byte => BytecodeType::Byte,
            IntSuffix::Int => BytecodeType::Int,
            IntSuffix::Long => BytecodeType::Long,
        };

        let dest = self.ensure_register(dest, ty);

        if lit.value == 0 {
            match ty {
                BytecodeType::Byte => self.gen.emit_const_zero_byte(dest),
                BytecodeType::Int => self.gen.emit_const_zero_int(dest),
                BytecodeType::Long => self.gen.emit_const_zero_long(dest),
                _ => unreachable!(),
            }
        } else {
            match ty {
                BytecodeType::Byte => self.gen.emit_const_byte(dest, lit.value as u8),
                BytecodeType::Int => self.gen.emit_const_int(dest, lit.value as i32),
                BytecodeType::Long => self.gen.emit_const_long(dest, lit.value as i64),
                _ => unreachable!(),
            }
        }

        dest
    }

    fn visit_expr_lit_float(&mut self, lit: &ExprLitFloatType, dest: DataDest) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

        let ty = match lit.suffix {
            FloatSuffix::Float => BytecodeType::Float,
            FloatSuffix::Double => BytecodeType::Double,
        };

        let dest = self.ensure_register(dest, ty);

        if lit.value == 0_f64 {
            match ty {
                BytecodeType::Float => self.gen.emit_const_zero_float(dest),
                BytecodeType::Double => self.gen.emit_const_zero_double(dest),
                _ => unreachable!(),
            }
        } else {
            match ty {
                BytecodeType::Float => self.gen.emit_const_float(dest, lit.value as f32),
                BytecodeType::Double => self.gen.emit_const_double(dest, lit.value),
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

    fn visit_expr_un(&mut self, expr: &ExprUnType, dest: DataDest) -> Register {
        if let Some(intrinsic) = self.get_intrinsic(expr.id) {
            if dest.is_effect() {
                self.visit_expr(&expr.opnd, dest);
                return Register::invalid();
            }

            match intrinsic {
                Intrinsic::IntPlus
                | Intrinsic::LongPlus
                | Intrinsic::FloatPlus
                | Intrinsic::DoublePlus => self.visit_expr(&expr.opnd, dest),

                Intrinsic::IntNeg => {
                    let dest = self.ensure_register(dest, BytecodeType::Int);
                    let src = self.visit_expr(&expr.opnd, DataDest::Alloc);
                    self.gen.emit_neg_int(dest, src);

                    dest
                }

                Intrinsic::LongNeg => {
                    let dest = self.ensure_register(dest, BytecodeType::Long);
                    let src = self.visit_expr(&expr.opnd, DataDest::Alloc);
                    self.gen.emit_neg_long(dest, src);

                    dest
                }

                Intrinsic::BoolNot => {
                    let dest = self.ensure_register(dest, BytecodeType::Bool);
                    let src = self.visit_expr(&expr.opnd, DataDest::Alloc);
                    self.gen.emit_not_bool(dest, src);

                    dest
                }

                _ => unimplemented!(),
            }
        } else {
            unimplemented!()
        }
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
        } else if let Some(intrinsic) = self.get_intrinsic(expr.id) {
            self.emit_intrinsic_bin(&expr.lhs, &expr.rhs, intrinsic, expr.op, expr.pos, dest)
        } else {
            unimplemented!();
        }
    }

    fn emit_bin_is(&mut self, expr: &ExprBinType, dest: DataDest) -> Register {
        if dest.is_effect() {
            self.visit_expr(&expr.lhs, dest);
            self.visit_expr(&expr.rhs, dest);
            return Register::invalid();
        }

        let dest = self.ensure_register(dest, BytecodeType::Bool);

        let lhs_reg = self.visit_expr(&expr.lhs, DataDest::Alloc);
        let rhs_reg = self.visit_expr(&expr.rhs, DataDest::Alloc);
        if expr.op == BinOp::Cmp(CmpOp::Is) {
            self.gen.emit_test_eq_ptr(dest, lhs_reg, rhs_reg);
        } else {
            self.gen.emit_test_ne_ptr(dest, lhs_reg, rhs_reg);
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

    fn emit_intrinsic_bin(
        &mut self,
        lhs: &Expr,
        rhs: &Expr,
        intrinsic: Intrinsic,
        op: BinOp,
        pos: Position,
        dest: DataDest,
    ) -> Register {
        let result_type = match intrinsic {
            Intrinsic::IntAdd
            | Intrinsic::IntSub
            | Intrinsic::IntMul
            | Intrinsic::IntDiv
            | Intrinsic::IntMod
            | Intrinsic::IntOr
            | Intrinsic::IntAnd
            | Intrinsic::IntXor
            | Intrinsic::IntShl
            | Intrinsic::IntShr
            | Intrinsic::IntSar => BytecodeType::Int,
            Intrinsic::FloatAdd
            | Intrinsic::FloatSub
            | Intrinsic::FloatDiv
            | Intrinsic::FloatMul => BytecodeType::Float,
            Intrinsic::BoolEq
            | Intrinsic::IntEq
            | Intrinsic::IntCmp
            | Intrinsic::FloatEq
            | Intrinsic::FloatCmp => BytecodeType::Bool,
            _ => unimplemented!(),
        };

        if dest.is_effect() {
            self.visit_expr(lhs, dest);
            self.visit_expr(rhs, dest);
            return Register::invalid();
        }

        let dest = self.ensure_register(dest, result_type);

        let lhs_reg = self.visit_expr(lhs, DataDest::Alloc);
        let rhs_reg = self.visit_expr(rhs, DataDest::Alloc);
        self.gen.set_position(pos);
        match intrinsic {
            Intrinsic::BoolEq => match op {
                BinOp::Cmp(CmpOp::Eq) => self.gen.emit_test_eq_bool(dest, lhs_reg, rhs_reg),
                BinOp::Cmp(CmpOp::Ne) => self.gen.emit_test_ne_bool(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::IntAdd => self.gen.emit_add_int(dest, lhs_reg, rhs_reg),
            Intrinsic::IntSub => self.gen.emit_sub_int(dest, lhs_reg, rhs_reg),
            Intrinsic::IntMul => self.gen.emit_mul_int(dest, lhs_reg, rhs_reg),
            Intrinsic::IntDiv => self.gen.emit_div_int(dest, lhs_reg, rhs_reg),
            Intrinsic::IntMod => self.gen.emit_mod_int(dest, lhs_reg, rhs_reg),
            Intrinsic::IntOr => self.gen.emit_or_int(dest, lhs_reg, rhs_reg),
            Intrinsic::IntAnd => self.gen.emit_and_int(dest, lhs_reg, rhs_reg),
            Intrinsic::IntXor => self.gen.emit_xor_int(dest, lhs_reg, rhs_reg),
            Intrinsic::IntShl => self.gen.emit_shl_int(dest, lhs_reg, rhs_reg),
            Intrinsic::IntShr => self.gen.emit_shr_int(dest, lhs_reg, rhs_reg),
            Intrinsic::IntSar => self.gen.emit_sar_int(dest, lhs_reg, rhs_reg),
            Intrinsic::IntEq => match op {
                BinOp::Cmp(CmpOp::Eq) => self.gen.emit_test_eq_int(dest, lhs_reg, rhs_reg),
                BinOp::Cmp(CmpOp::Ne) => self.gen.emit_test_ne_int(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::IntCmp => match op {
                BinOp::Cmp(CmpOp::Lt) => self.gen.emit_test_lt_int(dest, lhs_reg, rhs_reg),
                BinOp::Cmp(CmpOp::Le) => self.gen.emit_test_le_int(dest, lhs_reg, rhs_reg),
                BinOp::Cmp(CmpOp::Gt) => self.gen.emit_test_gt_int(dest, lhs_reg, rhs_reg),
                BinOp::Cmp(CmpOp::Ge) => self.gen.emit_test_ge_int(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::FloatAdd => self.gen.emit_add_float(dest, lhs_reg, rhs_reg),
            Intrinsic::FloatSub => self.gen.emit_sub_float(dest, lhs_reg, rhs_reg),
            Intrinsic::FloatMul => self.gen.emit_mul_float(dest, lhs_reg, rhs_reg),
            Intrinsic::FloatDiv => self.gen.emit_div_float(dest, lhs_reg, rhs_reg),
            Intrinsic::FloatEq => match op {
                BinOp::Cmp(CmpOp::Eq) => self.gen.emit_test_eq_float(dest, lhs_reg, rhs_reg),
                BinOp::Cmp(CmpOp::Ne) => self.gen.emit_test_ne_float(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::FloatCmp => match op {
                BinOp::Cmp(CmpOp::Lt) => self.gen.emit_test_lt_float(dest, lhs_reg, rhs_reg),
                BinOp::Cmp(CmpOp::Le) => self.gen.emit_test_le_float(dest, lhs_reg, rhs_reg),
                BinOp::Cmp(CmpOp::Gt) => self.gen.emit_test_gt_float(dest, lhs_reg, rhs_reg),
                BinOp::Cmp(CmpOp::Ge) => self.gen.emit_test_ge_float(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            _ => unimplemented!(),
        }

        dest
    }

    fn visit_expr_assign(&mut self, expr: &ExprBinType, dest: DataDest) -> Register {
        assert!(dest.is_effect());

        if expr.lhs.is_ident() {
            let ident_type = self.src.map_idents.get(expr.lhs.id()).unwrap();
            match ident_type {
                &IdentType::Var(var_id) => {
                    let var_reg = self.var_reg(var_id);
                    self.visit_expr(&expr.rhs, DataDest::Reg(var_reg));
                }

                &IdentType::Global(_) => unimplemented!(),
                &IdentType::Field(_, _) => unimplemented!(),

                &IdentType::Struct(_) => unimplemented!(),
                &IdentType::Enum(_) | &IdentType::EnumValue(_, _) => unreachable!(),
                &IdentType::Const(_) => unreachable!(),
                &IdentType::Fct(_) | &IdentType::FctType(_, _) => unreachable!(),
                &IdentType::Class(_) | &IdentType::ClassType(_, _) => unimplemented!(),
                &IdentType::Method(_, _) | &IdentType::MethodType(_, _, _) => unimplemented!(),
                &IdentType::TypeParam(_) | &IdentType::TypeParamStaticMethod(_, _) => {
                    unreachable!()
                }
                &IdentType::StaticMethod(_, _) | &IdentType::StaticMethodType(_, _, _) => {
                    unreachable!()
                }
            }
        } else {
            match *expr.lhs {
                ExprDot(ref dot) => {
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
                    let ty: BytecodeType = field.ty.into();

                    let src = self.visit_expr(&expr.rhs, DataDest::Alloc);
                    let obj = self.visit_expr(&dot.lhs, DataDest::Alloc);

                    self.gen.set_position(expr.pos);

                    match ty {
                        BytecodeType::Byte => {
                            self.gen.emit_store_field_byte(src, obj, cls_id, field_id)
                        }
                        BytecodeType::Bool => {
                            self.gen.emit_store_field_bool(src, obj, cls_id, field_id)
                        }
                        BytecodeType::Char => {
                            self.gen.emit_store_field_char(src, obj, cls_id, field_id)
                        }
                        BytecodeType::Int => {
                            self.gen.emit_store_field_int(src, obj, cls_id, field_id)
                        }
                        BytecodeType::Long => {
                            self.gen.emit_store_field_long(src, obj, cls_id, field_id)
                        }
                        BytecodeType::Float => {
                            self.gen.emit_store_field_float(src, obj, cls_id, field_id)
                        }
                        BytecodeType::Double => {
                            self.gen.emit_store_field_double(src, obj, cls_id, field_id)
                        }
                        BytecodeType::Ptr => {
                            self.gen.emit_store_field_ptr(src, obj, cls_id, field_id)
                        }
                    }
                }
                _ => unreachable!(),
            };
        }

        Register::invalid()
    }

    fn visit_expr_ident(&mut self, ident: &ExprIdentType, dest: DataDest) -> Register {
        let ident_type = self.src.map_idents.get(ident.id).unwrap();

        match ident_type {
            &IdentType::Var(var_id) => {
                if dest.is_effect() {
                    return Register::invalid();
                }

                let var_reg = self.var_reg(var_id);
                let ty: BytecodeType = self.specialize_type(self.src.vars[var_id].ty).into();

                if dest.is_alloc() {
                    return var_reg;
                }

                let dest = dest.reg();

                if dest != var_reg {
                    match ty {
                        BytecodeType::Bool => self.gen.emit_mov_bool(dest, var_reg),
                        BytecodeType::Byte => self.gen.emit_mov_byte(dest, var_reg),
                        BytecodeType::Char => self.gen.emit_mov_char(dest, var_reg),
                        BytecodeType::Int => self.gen.emit_mov_int(dest, var_reg),
                        BytecodeType::Long => self.gen.emit_mov_long(dest, var_reg),
                        BytecodeType::Float => self.gen.emit_mov_float(dest, var_reg),
                        BytecodeType::Double => self.gen.emit_mov_double(dest, var_reg),
                        BytecodeType::Ptr => self.gen.emit_mov_ptr(dest, var_reg),
                    }
                }

                dest
            }

            &IdentType::Global(gid) => {
                if dest.is_effect() {
                    return Register::invalid();
                }

                let glob = self.vm.globals.idx(gid);
                let glob = glob.lock();

                let ty: BytecodeType = glob.ty.into();
                let dest = self.ensure_register(dest, ty);

                match ty {
                    BytecodeType::Bool => self.gen.emit_load_global_bool(dest, gid),
                    BytecodeType::Byte => self.gen.emit_load_global_byte(dest, gid),
                    BytecodeType::Char => self.gen.emit_load_global_char(dest, gid),
                    BytecodeType::Int => self.gen.emit_load_global_int(dest, gid),
                    BytecodeType::Long => self.gen.emit_load_global_long(dest, gid),
                    BytecodeType::Float => self.gen.emit_load_global_float(dest, gid),
                    BytecodeType::Double => self.gen.emit_load_global_double(dest, gid),
                    BytecodeType::Ptr => self.gen.emit_load_global_ptr(dest, gid),
                }

                dest
            }

            &IdentType::Field(_, _) => unimplemented!(),
            &IdentType::Struct(_) => unimplemented!(),
            &IdentType::Const(_) => unimplemented!(),

            &IdentType::Enum(_) | &IdentType::EnumValue(_, _) => unreachable!(),
            &IdentType::Fct(_) | &IdentType::FctType(_, _) => unreachable!(),
            &IdentType::Class(_) | &IdentType::ClassType(_, _) => unreachable!(),
            &IdentType::TypeParam(_) | &IdentType::TypeParamStaticMethod(_, _) => unreachable!(),
            &IdentType::Method(_, _) | &IdentType::MethodType(_, _, _) => unreachable!(),
            &IdentType::StaticMethod(_, _) | &IdentType::StaticMethodType(_, _, _) => {
                unreachable!()
            }
        }
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

    fn specialize_type_for_call(&self, call_type: &CallType, ty: BuiltinType) -> BuiltinType {
        let ty = match *call_type {
            CallType::Fct(_, ref cls_type_params, ref fct_type_params) => {
                specialize_type(self.vm, ty, cls_type_params, fct_type_params)
            }

            CallType::Method(cls_ty, _, ref type_params) => {
                let cls_type_params = cls_ty.type_params(self.vm);
                specialize_type(self.vm, ty, &cls_type_params, type_params)
            }

            CallType::Ctor(cls_ty, _) | CallType::CtorNew(cls_ty, _) => {
                let cls_type_params = cls_ty.type_params(self.vm);
                specialize_type(self.vm, ty, &cls_type_params, &TypeList::empty())
            }

            CallType::Expr(ty, _) => {
                let type_params = ty.type_params(self.vm);
                specialize_type(self.vm, ty, &type_params, &TypeList::empty())
            }

            CallType::Trait(_, _) | CallType::TraitStatic(_, _, _) => unimplemented!(),
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

    fn get_intrinsic(&self, id: NodeId) -> Option<Intrinsic> {
        let call_type = self.src.map_calls.get(id).unwrap();

        if let Some(intrinsic) = call_type.to_intrinsic() {
            return Some(intrinsic);
        }

        let fid = call_type.fct_id().unwrap();

        // the function we compile right now is never an intrinsic
        if self.fct.id == fid {
            return None;
        }

        let fct = self.vm.fcts.idx(fid);
        let fct = fct.read();

        match fct.kind {
            FctKind::Builtin(intr) => Some(intr),
            _ => None,
        }
    }
}

#[derive(Copy, Clone)]
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

    fn is_reg(&self) -> bool {
        match self {
            DataDest::Effect => false,
            DataDest::Reg(_) => true,
            DataDest::Alloc => false,
        }
    }

    fn reg(&self) -> Register {
        match self {
            DataDest::Effect | DataDest::Alloc => panic!("not a register"),
            DataDest::Reg(reg) => *reg,
        }
    }
}
