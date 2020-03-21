use dora_parser::lexer::position::Position;
use std::collections::HashMap;

use dora_parser::ast::Expr::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::*;
use dora_parser::lexer::token::{FloatSuffix, IntSuffix};

use crate::bytecode::{BytecodeFunction, BytecodeType, BytecodeWriter, Label, Register};
use crate::semck::specialize::{specialize_class_ty, specialize_type};
use crate::semck::{expr_always_returns, expr_block_always_returns};
use crate::size::InstanceSize;
use crate::ty::{BuiltinType, TypeList};
use crate::vm::{
    CallType, Fct, FctDef, FctDefId, FctId, FctKind, FctSrc, IdentType, Intrinsic, TraitId, VarId,
    VM,
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
            let var_id = self.src.var_self().id;
            let reg = self.gen.add_register(BytecodeType::Ptr);
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

    // TODO - implement other statements
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match *stmt {
            StmtReturn(ref ret) => self.visit_stmt_return(ret),
            StmtBreak(ref stmt) => self.visit_stmt_break(stmt),
            StmtContinue(ref stmt) => self.visit_stmt_continue(stmt),
            StmtExpr(ref expr) => self.visit_stmt_expr(expr),
            StmtVar(ref stmt) => self.visit_stmt_var(stmt),
            StmtWhile(ref stmt) => self.visit_stmt_while(stmt),
            // StmtDefer(ref stmt) => {},
            // StmtSpawn(ref stmt) => {},
            // StmtFor(ref stmt) => {},
            _ => unimplemented!(),
        }
    }

    fn visit_stmt_var(&mut self, stmt: &StmtVarType) {
        let var_id = *self.src.map_vars.get(stmt.id).unwrap();
        let ty = self.var_ty(var_id);

        let dest = if ty.is_unit() {
            DataDest::Effect
        } else {
            let ty: BytecodeType = self.specialize_type(ty).into();
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
        self.loops.pop();
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

    fn visit_expr(&mut self, expr: &Expr, dest: DataDest) -> Register {
        match *expr {
            ExprUn(ref un) => self.visit_expr_un(un, dest),
            ExprBin(ref bin) => self.visit_expr_bin(bin, dest),
            ExprDot(ref field) => self.visit_expr_dot(field, dest),
            ExprBlock(ref block) => self.visit_expr_block(block, dest),
            ExprIf(ref expr) => self.visit_expr_if(expr, dest),
            ExprTemplate(_) => unimplemented!(),
            ExprTypeParam(_) => unreachable!(),
            ExprPath(_) => unimplemented!(),
            ExprLitChar(ref lit) => self.visit_expr_lit_char(lit, dest),
            ExprLitInt(ref lit) => self.visit_expr_lit_int(lit, dest),
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

        let field_bc_ty: BytecodeType = field_ty.into();

        let dest = self.ensure_register(dest, field_bc_ty);
        let obj = self.visit_expr(&expr.lhs, DataDest::Alloc);

        self.gen.set_position(expr.pos);

        match field_bc_ty {
            BytecodeType::Byte => self
                .gen
                .emit_load_field_byte(dest, obj, cls_def_id, field_id),
            BytecodeType::Bool => self
                .gen
                .emit_load_field_bool(dest, obj, cls_def_id, field_id),
            BytecodeType::Char => self
                .gen
                .emit_load_field_char(dest, obj, cls_def_id, field_id),
            BytecodeType::Int => self
                .gen
                .emit_load_field_int(dest, obj, cls_def_id, field_id),
            BytecodeType::Long => self
                .gen
                .emit_load_field_long(dest, obj, cls_def_id, field_id),
            BytecodeType::Float => self
                .gen
                .emit_load_field_float(dest, obj, cls_def_id, field_id),
            BytecodeType::Double => self
                .gen
                .emit_load_field_double(dest, obj, cls_def_id, field_id),
            BytecodeType::Ptr => self
                .gen
                .emit_load_field_ptr(dest, obj, cls_def_id, field_id),
        }

        dest
    }

    fn visit_expr_assert(&mut self, expr: &ExprCallType, dest: DataDest) {
        assert!(dest.is_effect());
        let assert_reg = self.visit_expr(&*expr.args[0], DataDest::Alloc);
        self.gen.set_position(expr.pos);
        self.gen.emit_assert(assert_reg);
    }

    fn visit_expr_call(&mut self, expr: &ExprCallType, dest: DataDest) -> Register {
        if let Some(intrinsic) = self.get_intrinsic(expr.id) {
            return self.emit_intrinsic_call(expr, intrinsic, dest);
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
            self.determine_callee_types(&call_type, &*callee, dest);

        let num_args = arg_bytecode_types.len();

        // Allocate register for result
        let return_reg = if return_type.is_unit() {
            Register::invalid()
        } else {
            self.ensure_register(dest, return_type.into())
        };

        // Allocate registers for arguments
        let start_reg = if num_args > 0 {
            self.gen.add_register_chain(&arg_bytecode_types)
        } else {
            Register::zero()
        };

        // Evaluate object/self argument
        self.emit_call_object_argument(expr, &call_type, start_reg);

        // Evaluate function arguments
        self.emit_call_arguments(expr, &call_type, &arg_types, start_reg);

        // Allocte object or array for constructor calls
        self.emit_call_allocate(expr.pos, &call_type, &arg_types, start_reg);

        // Emit the actual Invoke(Direct|Static|Virtual)XXX instruction
        self.emit_call_inst(
            expr,
            &*callee,
            &call_type,
            &arg_bytecode_types,
            return_type,
            expr.pos,
            callee_def_id,
            start_reg,
            num_args,
            return_reg,
        );

        // Store result
        self.emit_call_result(&call_type, dest, return_reg, start_reg)
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
            _ => call_type.fct_id().unwrap(),
        }
    }

    fn determine_callee_types(
        &mut self,
        call_type: &CallType,
        fct: &Fct,
        dest: DataDest,
    ) -> (Vec<BuiltinType>, Vec<BytecodeType>, BuiltinType) {
        let return_type = if dest.is_effect() {
            BuiltinType::Unit
        } else {
            self.specialize_type_for_call(&call_type, fct.return_type)
        };

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
        start_reg: Register,
    ) {
        match *call_type {
            CallType::Method(_, _, _) => {
                let obj_expr = expr.object().expect("method target required");
                self.visit_expr(obj_expr, DataDest::Reg(start_reg));
            }
            _ => {}
        };
    }

    fn emit_call_arguments(
        &mut self,
        expr: &ExprCallType,
        call_type: &CallType,
        arg_types: &[BuiltinType],
        start_reg: Register,
    ) {
        let arg_start_offset = match *call_type {
            CallType::CtorNew(_, _) | CallType::Method(_, _, _) => 1,
            _ => 0,
        };

        for (idx, arg) in expr.args.iter().enumerate() {
            let ty = arg_types[idx + arg_start_offset];

            let dest = if ty.is_unit() {
                DataDest::Effect
            } else {
                let arg_reg = start_reg.offset(idx + arg_start_offset);
                DataDest::Reg(arg_reg)
            };

            self.visit_expr(arg, dest);
        }
    }

    fn emit_call_allocate(
        &mut self,
        pos: Position,
        call_type: &CallType,
        arg_types: &[BuiltinType],
        start_reg: Register,
    ) {
        match *call_type {
            CallType::CtorNew(_, _) => {
                let ty = arg_types.first().cloned().unwrap();
                let cls_def_id = specialize_class_ty(self.vm, ty);

                let cls = self.vm.class_defs.idx(cls_def_id);
                let cls = cls.read();

                self.gen.set_position(pos);

                match cls.size {
                    InstanceSize::Fixed(_) => {
                        self.gen.emit_new_object(start_reg, cls_def_id);
                    }
                    InstanceSize::Array(_) | InstanceSize::UnitArray => {
                        let length_arg = start_reg.offset(1);
                        self.gen.emit_new_array(start_reg, cls_def_id, length_arg);
                    }
                    _ => {
                        unimplemented!();
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
        start_reg: Register,
        num_args: usize,
        return_reg: Register,
    ) {
        self.gen.set_position(pos);

        match *call_type {
            CallType::Ctor(_, _) | CallType::CtorNew(_, _) => {
                self.gen
                    .emit_invoke_direct_void(fct_def_id, start_reg, num_args);
            }

            CallType::Method(_, _, _) => {
                let is_super_call = expr
                    .object()
                    .map(|object| object.is_super())
                    .unwrap_or(false);

                if is_super_call {
                    self.visit_call_direct(
                        return_type,
                        fct_def_id,
                        start_reg,
                        num_args,
                        return_reg,
                    );
                } else if fct.is_virtual() {
                    self.visit_call_virtual(
                        return_type,
                        fct_def_id,
                        start_reg,
                        num_args,
                        return_reg,
                    );
                } else if arg_bytecode_types[0] != BytecodeType::Ptr {
                    self.visit_call_static(
                        return_type,
                        fct_def_id,
                        start_reg,
                        num_args,
                        return_reg,
                    );
                } else {
                    self.visit_call_direct(
                        return_type,
                        fct_def_id,
                        start_reg,
                        num_args,
                        return_reg,
                    );
                }
            }
            CallType::Expr(_, _) => unimplemented!(),
            CallType::Fct(_, _, _) => {
                self.visit_call_static(return_type, fct_def_id, start_reg, num_args, return_reg);
            }

            CallType::Trait(_, _) => unimplemented!(),
            CallType::TraitStatic(_, _, _) => unimplemented!(),
            CallType::Intrinsic(_) => unreachable!(),
        }
    }

    fn emit_call_result(
        &mut self,
        call_type: &CallType,
        dest: DataDest,
        return_reg: Register,
        start_reg: Register,
    ) -> Register {
        if call_type.is_ctor_new() || call_type.is_ctor() {
            match dest {
                DataDest::Effect => Register::invalid(),
                DataDest::Reg(reg) => {
                    self.gen.emit_mov_ptr(reg, start_reg);
                    reg
                }
                DataDest::Alloc => start_reg,
            }
        } else {
            return_reg
        }
    }

    fn visit_call_virtual(
        &mut self,
        return_type: BuiltinType,
        callee_id: FctDefId,
        start_reg: Register,
        num_args: usize,
        return_reg: Register,
    ) {
        if return_type.is_unit() {
            self.gen
                .emit_invoke_virtual_void(callee_id, start_reg, num_args);
        } else {
            let return_type: BytecodeType = return_type.into();

            match return_type.into() {
                BytecodeType::Bool => self
                    .gen
                    .emit_invoke_virtual_bool(return_reg, callee_id, start_reg, num_args),
                BytecodeType::Byte => self
                    .gen
                    .emit_invoke_virtual_byte(return_reg, callee_id, start_reg, num_args),
                BytecodeType::Char => self
                    .gen
                    .emit_invoke_virtual_char(return_reg, callee_id, start_reg, num_args),
                BytecodeType::Int => self
                    .gen
                    .emit_invoke_virtual_int(return_reg, callee_id, start_reg, num_args),
                BytecodeType::Long => self
                    .gen
                    .emit_invoke_virtual_long(return_reg, callee_id, start_reg, num_args),
                BytecodeType::Float => self
                    .gen
                    .emit_invoke_virtual_float(return_reg, callee_id, start_reg, num_args),
                BytecodeType::Double => self
                    .gen
                    .emit_invoke_virtual_double(return_reg, callee_id, start_reg, num_args),
                BytecodeType::Ptr => self
                    .gen
                    .emit_invoke_virtual_ptr(return_reg, callee_id, start_reg, num_args),
            }
        }
    }

    fn visit_call_direct(
        &mut self,
        return_type: BuiltinType,
        callee_id: FctDefId,
        start_reg: Register,
        num_args: usize,
        return_reg: Register,
    ) {
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

    fn visit_call_static(
        &mut self,
        return_type: BuiltinType,
        callee_id: FctDefId,
        start_reg: Register,
        num_args: usize,
        return_reg: Register,
    ) {
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

    fn visit_expr_delegation(&mut self, expr: &ExprDelegationType, dest: DataDest) -> Register {
        assert!(dest.is_effect());
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
                    .emit_invoke_direct_void(fct_def_id, start_reg, num_args);
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

    fn visit_expr_tuple(&mut self, e: &ExprTupleType, dest: DataDest) -> Register {
        if e.values.is_empty() {
            assert!(dest.is_effect());
            return Register::invalid();
        }

        unimplemented!();
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
                Intrinsic::FloatNeg => {
                    let dest = self.ensure_register(dest, BytecodeType::Float);
                    let src = self.visit_expr(&expr.opnd, DataDest::Alloc);
                    self.gen.emit_neg_float(dest, src);

                    dest
                }
                Intrinsic::DoubleNeg => {
                    let dest = self.ensure_register(dest, BytecodeType::Double);
                    let src = self.visit_expr(&expr.opnd, DataDest::Alloc);
                    self.gen.emit_neg_double(dest, src);

                    dest
                }
                Intrinsic::BoolNot => {
                    let dest = self.ensure_register(dest, BytecodeType::Bool);
                    let src = self.visit_expr(&expr.opnd, DataDest::Alloc);
                    self.gen.emit_not_bool(dest, src);

                    dest
                }
                Intrinsic::IntNot => {
                    let dest = self.ensure_register(dest, BytecodeType::Int);
                    let src = self.visit_expr(&expr.opnd, DataDest::Alloc);
                    self.gen.emit_not_int(dest, src);

                    dest
                }
                Intrinsic::LongNot => {
                    let dest = self.ensure_register(dest, BytecodeType::Long);
                    let src = self.visit_expr(&expr.opnd, DataDest::Alloc);
                    self.gen.emit_not_long(dest, src);

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
            self.emit_intrinsic_bin(
                &expr.lhs,
                &expr.rhs,
                intrinsic,
                Some(expr.op),
                expr.pos,
                dest,
            )
        } else {
            unimplemented!();
        }
    }

    fn emit_intrinsic_call(
        &mut self,
        expr: &ExprCallType,
        intrinsic: Intrinsic,
        dest: DataDest,
    ) -> Register {
        match intrinsic {
            Intrinsic::Assert => {
                self.visit_expr_assert(expr, dest);
                Register::invalid()
            }
            Intrinsic::IntRotateLeft
            | Intrinsic::IntRotateRight
            | Intrinsic::LongRotateLeft
            | Intrinsic::LongRotateRight => self.emit_intrinsic_bin(
                expr.object().expect("value needed"),
                &*expr.args[0],
                intrinsic,
                None,
                expr.pos,
                dest,
            ),
            Intrinsic::FloatPlus | Intrinsic::DoublePlus => {
                self.visit_expr(expr.object().unwrap(), dest)
            }
            Intrinsic::FloatAdd | Intrinsic::DoubleAdd => {
                let ty = match intrinsic {
                    Intrinsic::FloatAdd => BytecodeType::Float,
                    Intrinsic::DoubleAdd => BytecodeType::Double,
                    _ => unreachable!(),
                };

                let dest = self.ensure_register(dest, ty);
                let lhs_reg = self.visit_expr(expr.object().unwrap(), DataDest::Alloc);
                let rhs_reg = self.visit_expr(&expr.args[0], DataDest::Alloc);

                match intrinsic {
                    Intrinsic::FloatAdd => self.gen.emit_add_float(dest, lhs_reg, rhs_reg),
                    Intrinsic::DoubleAdd => self.gen.emit_add_double(dest, lhs_reg, rhs_reg),
                    _ => unreachable!(),
                }

                dest
            }
            Intrinsic::FloatSub | Intrinsic::DoubleSub => {
                let ty = match intrinsic {
                    Intrinsic::FloatSub => BytecodeType::Float,
                    Intrinsic::DoubleSub => BytecodeType::Double,
                    _ => unreachable!(),
                };

                let dest = self.ensure_register(dest, ty);
                let lhs_reg = self.visit_expr(expr.object().unwrap(), DataDest::Alloc);
                let rhs_reg = self.visit_expr(&expr.args[0], DataDest::Alloc);

                match intrinsic {
                    Intrinsic::FloatSub => self.gen.emit_sub_float(dest, lhs_reg, rhs_reg),
                    Intrinsic::DoubleSub => self.gen.emit_sub_double(dest, lhs_reg, rhs_reg),
                    _ => unreachable!(),
                }

                dest
            }
            Intrinsic::FloatMul | Intrinsic::DoubleMul => {
                let ty = match intrinsic {
                    Intrinsic::FloatMul => BytecodeType::Float,
                    Intrinsic::DoubleMul => BytecodeType::Double,
                    _ => unreachable!(),
                };

                let dest = self.ensure_register(dest, ty);
                let lhs_reg = self.visit_expr(expr.object().unwrap(), DataDest::Alloc);
                let rhs_reg = self.visit_expr(&expr.args[0], DataDest::Alloc);

                match intrinsic {
                    Intrinsic::FloatMul => self.gen.emit_mul_float(dest, lhs_reg, rhs_reg),
                    Intrinsic::DoubleMul => self.gen.emit_mul_double(dest, lhs_reg, rhs_reg),
                    _ => unreachable!(),
                }

                dest
            }
            Intrinsic::FloatDiv | Intrinsic::DoubleDiv => {
                let ty = match intrinsic {
                    Intrinsic::FloatDiv => BytecodeType::Float,
                    Intrinsic::DoubleDiv => BytecodeType::Double,
                    _ => unreachable!(),
                };

                let dest = self.ensure_register(dest, ty);
                let lhs_reg = self.visit_expr(expr.object().unwrap(), DataDest::Alloc);
                let rhs_reg = self.visit_expr(&expr.args[0], DataDest::Alloc);

                match intrinsic {
                    Intrinsic::FloatDiv => self.gen.emit_div_float(dest, lhs_reg, rhs_reg),
                    Intrinsic::DoubleDiv => self.gen.emit_div_double(dest, lhs_reg, rhs_reg),
                    _ => unreachable!(),
                }

                dest
            }
            Intrinsic::ReinterpretFloatAsInt => {
                let dest = self.ensure_register(dest, BytecodeType::Int);
                let src = self.visit_expr(expr.object().unwrap(), DataDest::Alloc);
                self.gen.emit_reinterpret_float_as_int(dest, src);

                dest
            }
            Intrinsic::ReinterpretIntAsFloat => {
                let dest = self.ensure_register(dest, BytecodeType::Float);
                let src = self.visit_expr(expr.object().unwrap(), DataDest::Alloc);
                self.gen.emit_reinterpret_int_as_float(dest, src);

                dest
            }
            Intrinsic::ReinterpretDoubleAsLong => {
                let dest = self.ensure_register(dest, BytecodeType::Long);
                let src = self.visit_expr(expr.object().unwrap(), DataDest::Alloc);
                self.gen.emit_reinterpret_double_as_long(dest, src);

                dest
            }
            Intrinsic::ReinterpretLongAsDouble => {
                let dest = self.ensure_register(dest, BytecodeType::Double);
                let src = self.visit_expr(expr.object().unwrap(), DataDest::Alloc);
                self.gen.emit_reinterpret_long_as_double(dest, src);

                dest
            }
            Intrinsic::FloatNeg | Intrinsic::DoubleNeg => {
                let ty = match intrinsic {
                    Intrinsic::FloatNeg => BytecodeType::Float,
                    Intrinsic::DoubleNeg => BytecodeType::Double,
                    _ => unreachable!(),
                };

                let dest = self.ensure_register(dest, ty);
                let src = self.visit_expr(expr.object().unwrap(), DataDest::Alloc);

                match intrinsic {
                    Intrinsic::FloatNeg => self.gen.emit_neg_float(dest, src),
                    Intrinsic::DoubleNeg => self.gen.emit_neg_double(dest, src),
                    _ => unreachable!(),
                }

                dest
            }
            Intrinsic::GenericArrayLen => self.emit_intrinsic_array_len(
                expr.object().expect("array required"),
                expr.pos,
                dest,
            ),
            Intrinsic::GenericArrayGet => {
                self.emit_intrinsic_array_get(&*expr.callee, &*expr.args[0], expr.pos, dest)
            }
            Intrinsic::FloatIsNan => {
                let dest = self.ensure_register(dest, BytecodeType::Bool);
                let src = self.visit_expr(expr.object().unwrap(), DataDest::Alloc);
                self.gen.emit_test_ne_float(dest, src, src);

                dest
            }
            Intrinsic::DoubleIsNan => {
                let dest = self.ensure_register(dest, BytecodeType::Bool);
                let src = self.visit_expr(expr.object().unwrap(), DataDest::Alloc);
                self.gen.emit_test_ne_double(dest, src, src);

                dest
            }
            Intrinsic::IntToLong => {
                let dest = self.ensure_register(dest, BytecodeType::Long);
                let src = self.visit_expr(expr.object().unwrap(), DataDest::Alloc);
                self.gen.emit_extend_int_to_long(dest, src);

                dest
            }
            Intrinsic::LongToInt => {
                let dest = self.ensure_register(dest, BytecodeType::Int);
                let src = self.visit_expr(expr.object().unwrap(), DataDest::Alloc);
                self.gen.emit_cast_long_to_int(dest, src);

                dest
            }
            Intrinsic::IntToFloat => {
                let dest = self.ensure_register(dest, BytecodeType::Float);
                let src = self.visit_expr(expr.object().unwrap(), DataDest::Alloc);
                self.gen.emit_convert_int_to_float(dest, src);

                dest
            }
            Intrinsic::IntToDouble => {
                let dest = self.ensure_register(dest, BytecodeType::Double);
                let src = self.visit_expr(expr.object().unwrap(), DataDest::Alloc);
                self.gen.emit_convert_int_to_double(dest, src);

                dest
            }
            Intrinsic::LongToFloat => {
                let dest = self.ensure_register(dest, BytecodeType::Float);
                let src = self.visit_expr(expr.object().unwrap(), DataDest::Alloc);
                self.gen.emit_convert_long_to_float(dest, src);

                dest
            }
            Intrinsic::LongToDouble => {
                let dest = self.ensure_register(dest, BytecodeType::Double);
                let src = self.visit_expr(expr.object().unwrap(), DataDest::Alloc);
                self.gen.emit_convert_long_to_double(dest, src);

                dest
            }
            Intrinsic::FloatToInt => {
                let dest = self.ensure_register(dest, BytecodeType::Int);
                let src = self.visit_expr(expr.object().unwrap(), DataDest::Alloc);
                self.gen.emit_truncate_float_to_int(dest, src);

                dest
            }
            Intrinsic::FloatToLong => {
                let dest = self.ensure_register(dest, BytecodeType::Long);
                let src = self.visit_expr(expr.object().unwrap(), DataDest::Alloc);
                self.gen.emit_truncate_float_to_long(dest, src);

                dest
            }
            Intrinsic::DoubleToInt => {
                let dest = self.ensure_register(dest, BytecodeType::Int);
                let src = self.visit_expr(expr.object().unwrap(), DataDest::Alloc);
                self.gen.emit_truncate_double_to_int(dest, src);

                dest
            }
            Intrinsic::DoubleToLong => {
                let dest = self.ensure_register(dest, BytecodeType::Long);
                let src = self.visit_expr(expr.object().unwrap(), DataDest::Alloc);
                self.gen.emit_truncate_double_to_long(dest, src);

                dest
            }
            _ => {
                panic!("unimplemented intrinsic {:?} at {}", intrinsic, expr.pos);
            }
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

    fn emit_intrinsic_array_len(&mut self, arr: &Expr, pos: Position, dest: DataDest) -> Register {
        let arr_reg = self.visit_expr(arr, DataDest::Alloc);
        self.gen.set_position(pos);

        if dest.is_effect() {
            self.gen.emit_nil_check(arr_reg);
            return Register::invalid();
        } else {
            let dest = self.ensure_register(dest, BytecodeType::Int);
            self.gen.emit_array_length(dest, arr_reg);

            return dest;
        }
    }

    fn emit_intrinsic_array_get(
        &mut self,
        arr: &Expr,
        idx: &Expr,
        pos: Position,
        dest: DataDest,
    ) -> Register {
        let ty = self.src.ty(arr.id());
        let ty = self.specialize_type(ty);
        let ty = ty.type_params(self.vm);
        let ty = ty[0];
        let ty: BytecodeType = ty.into();

        let dest = self.ensure_register(dest, ty);

        let arr = self.visit_expr(arr, DataDest::Alloc);
        let idx = self.visit_expr(idx, DataDest::Alloc);

        self.gen.set_position(pos);

        match ty {
            BytecodeType::Byte => self.gen.emit_load_array_byte(dest, arr, idx),
            BytecodeType::Bool => self.gen.emit_load_array_bool(dest, arr, idx),
            BytecodeType::Char => self.gen.emit_load_array_char(dest, arr, idx),
            BytecodeType::Int => self.gen.emit_load_array_int(dest, arr, idx),
            BytecodeType::Long => self.gen.emit_load_array_long(dest, arr, idx),
            BytecodeType::Float => self.gen.emit_load_array_float(dest, arr, idx),
            BytecodeType::Double => self.gen.emit_load_array_double(dest, arr, idx),
            BytecodeType::Ptr => self.gen.emit_load_array_ptr(dest, arr, idx),
        }

        dest
    }

    fn emit_intrinsic_bin(
        &mut self,
        lhs: &Expr,
        rhs: &Expr,
        intrinsic: Intrinsic,
        op: Option<BinOp>,
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
            | Intrinsic::IntSar
            | Intrinsic::IntRotateLeft
            | Intrinsic::IntRotateRight => BytecodeType::Int,
            Intrinsic::LongAdd
            | Intrinsic::LongSub
            | Intrinsic::LongMul
            | Intrinsic::LongDiv
            | Intrinsic::LongMod
            | Intrinsic::LongOr
            | Intrinsic::LongAnd
            | Intrinsic::LongXor
            | Intrinsic::LongShl
            | Intrinsic::LongShr
            | Intrinsic::LongSar
            | Intrinsic::LongRotateLeft
            | Intrinsic::LongRotateRight => BytecodeType::Long,
            Intrinsic::FloatAdd
            | Intrinsic::FloatSub
            | Intrinsic::FloatDiv
            | Intrinsic::FloatMul => BytecodeType::Float,
            Intrinsic::DoubleAdd
            | Intrinsic::DoubleSub
            | Intrinsic::DoubleDiv
            | Intrinsic::DoubleMul => BytecodeType::Double,
            Intrinsic::BoolEq
            | Intrinsic::ByteEq
            | Intrinsic::ByteCmp
            | Intrinsic::CharEq
            | Intrinsic::CharCmp
            | Intrinsic::EnumEq
            | Intrinsic::EnumNe
            | Intrinsic::IntEq
            | Intrinsic::IntCmp
            | Intrinsic::LongEq
            | Intrinsic::LongCmp
            | Intrinsic::FloatEq
            | Intrinsic::FloatCmp
            | Intrinsic::DoubleEq
            | Intrinsic::DoubleCmp => BytecodeType::Bool,
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
                Some(BinOp::Cmp(CmpOp::Eq)) => self.gen.emit_test_eq_bool(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Ne)) => self.gen.emit_test_ne_bool(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::ByteEq => match op {
                Some(BinOp::Cmp(CmpOp::Eq)) => self.gen.emit_test_eq_byte(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Ne)) => self.gen.emit_test_ne_byte(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::ByteCmp => match op {
                Some(BinOp::Cmp(CmpOp::Lt)) => self.gen.emit_test_lt_byte(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Le)) => self.gen.emit_test_le_byte(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Gt)) => self.gen.emit_test_gt_byte(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Ge)) => self.gen.emit_test_ge_byte(dest, lhs_reg, rhs_reg),
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
            Intrinsic::IntEq => match op {
                Some(BinOp::Cmp(CmpOp::Eq)) => self.gen.emit_test_eq_int(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Ne)) => self.gen.emit_test_ne_int(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::IntCmp => match op {
                Some(BinOp::Cmp(CmpOp::Lt)) => self.gen.emit_test_lt_int(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Le)) => self.gen.emit_test_le_int(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Gt)) => self.gen.emit_test_gt_int(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Ge)) => self.gen.emit_test_ge_int(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::LongEq => match op {
                Some(BinOp::Cmp(CmpOp::Eq)) => self.gen.emit_test_eq_long(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Ne)) => self.gen.emit_test_ne_long(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::LongCmp => match op {
                Some(BinOp::Cmp(CmpOp::Lt)) => self.gen.emit_test_lt_long(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Le)) => self.gen.emit_test_le_long(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Gt)) => self.gen.emit_test_gt_long(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Ge)) => self.gen.emit_test_ge_long(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::FloatEq => match op {
                Some(BinOp::Cmp(CmpOp::Eq)) => self.gen.emit_test_eq_float(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Ne)) => self.gen.emit_test_ne_float(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::FloatCmp => match op {
                Some(BinOp::Cmp(CmpOp::Lt)) => self.gen.emit_test_lt_float(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Le)) => self.gen.emit_test_le_float(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Gt)) => self.gen.emit_test_gt_float(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Ge)) => self.gen.emit_test_ge_float(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::DoubleEq => match op {
                Some(BinOp::Cmp(CmpOp::Eq)) => self.gen.emit_test_eq_double(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Ne)) => self.gen.emit_test_ne_double(dest, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::DoubleCmp => match op {
                Some(BinOp::Cmp(CmpOp::Lt)) => self.gen.emit_test_lt_double(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Le)) => self.gen.emit_test_le_double(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Gt)) => self.gen.emit_test_gt_double(dest, lhs_reg, rhs_reg),
                Some(BinOp::Cmp(CmpOp::Ge)) => self.gen.emit_test_ge_double(dest, lhs_reg, rhs_reg),
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
            Intrinsic::IntRotateLeft => self.gen.emit_rol_int(dest, lhs_reg, rhs_reg),
            Intrinsic::IntRotateRight => self.gen.emit_ror_int(dest, lhs_reg, rhs_reg),
            Intrinsic::LongAdd => self.gen.emit_add_long(dest, lhs_reg, rhs_reg),
            Intrinsic::LongSub => self.gen.emit_sub_long(dest, lhs_reg, rhs_reg),
            Intrinsic::LongMul => self.gen.emit_mul_long(dest, lhs_reg, rhs_reg),
            Intrinsic::LongDiv => self.gen.emit_div_long(dest, lhs_reg, rhs_reg),
            Intrinsic::LongMod => self.gen.emit_mod_long(dest, lhs_reg, rhs_reg),
            Intrinsic::LongOr => self.gen.emit_or_long(dest, lhs_reg, rhs_reg),
            Intrinsic::LongAnd => self.gen.emit_and_long(dest, lhs_reg, rhs_reg),
            Intrinsic::LongXor => self.gen.emit_xor_long(dest, lhs_reg, rhs_reg),
            Intrinsic::LongShl => self.gen.emit_shl_long(dest, lhs_reg, rhs_reg),
            Intrinsic::LongShr => self.gen.emit_shr_long(dest, lhs_reg, rhs_reg),
            Intrinsic::LongSar => self.gen.emit_sar_long(dest, lhs_reg, rhs_reg),
            Intrinsic::LongRotateLeft => self.gen.emit_rol_long(dest, lhs_reg, rhs_reg),
            Intrinsic::LongRotateRight => self.gen.emit_ror_long(dest, lhs_reg, rhs_reg),
            Intrinsic::FloatAdd => self.gen.emit_add_float(dest, lhs_reg, rhs_reg),
            Intrinsic::FloatSub => self.gen.emit_sub_float(dest, lhs_reg, rhs_reg),
            Intrinsic::FloatMul => self.gen.emit_mul_float(dest, lhs_reg, rhs_reg),
            Intrinsic::FloatDiv => self.gen.emit_div_float(dest, lhs_reg, rhs_reg),
            Intrinsic::DoubleAdd => self.gen.emit_add_double(dest, lhs_reg, rhs_reg),
            Intrinsic::DoubleSub => self.gen.emit_sub_double(dest, lhs_reg, rhs_reg),
            Intrinsic::DoubleMul => self.gen.emit_mul_double(dest, lhs_reg, rhs_reg),
            Intrinsic::DoubleDiv => self.gen.emit_div_double(dest, lhs_reg, rhs_reg),
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
                    let ty = self.var_ty(var_id);

                    let dest = if ty.is_unit() {
                        DataDest::Effect
                    } else {
                        let var_reg = self.var_reg(var_id);
                        DataDest::Reg(var_reg)
                    };

                    self.visit_expr(&expr.rhs, dest);
                }

                &IdentType::Global(gid) => {
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
                            BytecodeType::Byte => self.gen.emit_store_global_byte(src, gid),
                            BytecodeType::Char => self.gen.emit_store_global_char(src, gid),
                            BytecodeType::Int => self.gen.emit_store_global_int(src, gid),
                            BytecodeType::Long => self.gen.emit_store_global_long(src, gid),
                            BytecodeType::Float => self.gen.emit_store_global_float(src, gid),
                            BytecodeType::Double => self.gen.emit_store_global_double(src, gid),
                            BytecodeType::Ptr => self.gen.emit_store_global_ptr(src, gid),
                        }
                    }
                }
                &IdentType::Field(_, _) => unimplemented!(),

                &IdentType::Struct(_) => unimplemented!(),
                &IdentType::Enum(_) | &IdentType::EnumValue(_, _) => unreachable!(),
                &IdentType::Const(_) => unreachable!(),
                &IdentType::Fct(_) | &IdentType::FctType(_, _) => unreachable!(),
                &IdentType::Class(_) | &IdentType::ClassType(_, _) => unimplemented!(),
                &IdentType::Module(_) => unimplemented!(),
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
                ExprCall(ref call) => {
                    if let Some(intrinsic) = self.get_intrinsic(expr.id) {
                        match intrinsic {
                            Intrinsic::GenericArraySet => {
                                let object = &call.callee;

                                let ty = self.src.ty(object.id());
                                let ty = self.specialize_type(ty);
                                let ty = ty.type_params(self.vm);
                                let ty = ty[0];
                                let ty: BytecodeType = ty.into();

                                let arr = self.visit_expr(object, DataDest::Alloc);
                                let idx = self.visit_expr(&call.args[0], DataDest::Alloc);
                                let src = self.visit_expr(&expr.rhs, DataDest::Alloc);

                                self.gen.set_position(expr.pos);

                                match ty {
                                    BytecodeType::Byte => {
                                        self.gen.emit_store_array_byte(src, arr, idx)
                                    }
                                    BytecodeType::Bool => {
                                        self.gen.emit_store_array_bool(src, arr, idx)
                                    }
                                    BytecodeType::Char => {
                                        self.gen.emit_store_array_char(src, arr, idx)
                                    }
                                    BytecodeType::Int => {
                                        self.gen.emit_store_array_int(src, arr, idx)
                                    }
                                    BytecodeType::Long => {
                                        self.gen.emit_store_array_long(src, arr, idx)
                                    }
                                    BytecodeType::Float => {
                                        self.gen.emit_store_array_float(src, arr, idx)
                                    }
                                    BytecodeType::Double => {
                                        self.gen.emit_store_array_double(src, arr, idx)
                                    }
                                    BytecodeType::Ptr => {
                                        self.gen.emit_store_array_ptr(src, arr, idx)
                                    }
                                }
                            }
                            _ => panic!("unexpected intrinsic {:?}", intrinsic),
                        }
                    } else {
                        unimplemented!();
                    }
                }
                _ => unreachable!("{:?}", expr),
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
                let glob = glob.read();

                if glob.ty.is_unit() {
                    assert!(dest.is_alloc());
                    return Register::invalid();
                }

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
            &IdentType::Module(_) => unreachable!(),
            &IdentType::TypeParam(_) | &IdentType::TypeParamStaticMethod(_, _) => unreachable!(),
            &IdentType::Method(_, _) | &IdentType::MethodType(_, _, _) => unreachable!(),
            &IdentType::StaticMethod(_, _) | &IdentType::StaticMethodType(_, _, _) => {
                unreachable!()
            }
        }
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

        if let Some(&id) = fct
            .specializations
            .read()
            .get(&(cls_type_params.clone(), fct_type_params.clone()))
        {
            return id;
        }

        self.create_specialized_call(fct, &cls_type_params, &fct_type_params)
    }

    fn create_specialized_call(
        &self,
        fct: &Fct,
        cls_type_params: &TypeList,
        fct_type_params: &TypeList,
    ) -> FctDefId {
        debug_assert!(cls_type_params
            .iter()
            .all(|ty| ty.is_concrete_type(self.vm)));
        debug_assert!(fct_type_params
            .iter()
            .all(|ty| ty.is_concrete_type(self.vm)));

        let fct_def_id = self.vm.add_fct_def(FctDef {
            id: FctDefId(0),
            fct_id: fct.id,
            cls_type_params: cls_type_params.clone(),
            fct_type_params: fct_type_params.clone(),
        });

        let old = fct.specializations.write().insert(
            (cls_type_params.clone(), fct_type_params.clone()),
            fct_def_id,
        );
        assert!(old.is_none());

        fct_def_id
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

    fn var_ty(&self, id: VarId) -> BuiltinType {
        let ty = self.src.vars[id].ty;
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
