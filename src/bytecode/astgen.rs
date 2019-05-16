use std::collections::HashMap;

use dora_parser::ast::Expr::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::*;
use dora_parser::lexer::token::{FloatSuffix, IntSuffix};

use bytecode::generate::{BytecodeFunction, BytecodeGenerator, BytecodeType, Label, Register};
use class::TypeParams;
use ctxt::{Fct, FctId, FctKind, FctParent, FctSrc, IdentType, Intrinsic, VarId, VM};
use semck::specialize::specialize_class_ty;
use ty::BuiltinType;

pub struct LoopLabels {
    cond: Label,
    end: Label,
}

impl LoopLabels {
    fn new(cond: Label, end: Label) -> LoopLabels {
        LoopLabels {
            cond: cond,
            end: end,
        }
    }
}

pub fn generate<'ast>(
    vm: &VM<'ast>,
    id: FctId,
    cls_type_params: &TypeParams,
    fct_type_params: &TypeParams,
) -> BytecodeFunction {
    let fct = vm.fcts.idx(id);
    let fct = fct.read();
    let src = fct.src();
    let mut src = src.write();

    generate_fct(vm, &fct, &mut src, cls_type_params, fct_type_params)
}

pub fn generate_fct<'ast>(
    vm: &VM<'ast>,
    fct: &Fct<'ast>,
    src: &mut FctSrc,
    cls_type_params: &TypeParams,
    fct_type_params: &TypeParams,
) -> BytecodeFunction {
    let ast_bytecode_generator = AstBytecodeGen {
        vm: vm,
        fct: fct,
        ast: fct.ast,
        src: src,

        cls_type_params: cls_type_params,
        fct_type_params: fct_type_params,

        gen: BytecodeGenerator::new(),
        loops: Vec::new(),
        var_registers: HashMap::new(),
    };
    ast_bytecode_generator.generate()
}

pub struct AstBytecodeGen<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    fct: &'a Fct<'ast>,
    ast: &'ast Function,
    src: &'a mut FctSrc,

    cls_type_params: &'a TypeParams,
    fct_type_params: &'a TypeParams,

    gen: BytecodeGenerator,
    loops: Vec<LoopLabels>,
    var_registers: HashMap<VarId, Register>,
}

impl<'a, 'ast> AstBytecodeGen<'a, 'ast> {
    pub fn generate(mut self) -> BytecodeFunction {
        if self.fct.has_self() {
            let var_id = self.src.var_self().id;
            let reg = self.gen.add_register(BytecodeType::Ptr);
            self.var_registers.insert(var_id, reg);
        }

        for param in &self.ast.params {
            let var_id = *self.src.map_vars.get(param.id).unwrap();
            let var = &self.src.vars[var_id];
            let ty: BytecodeType = self.specialize_type(var.ty).into();
            let reg = self.gen.add_register(ty);
            self.var_registers.insert(var_id, reg);
        }

        if let Some(ref block) = self.ast.block {
            self.visit_stmt(block);
        }

        if self.fct.return_type.is_unit() {
            self.gen.emit_ret_void();
        }

        self.gen.generate()
    }

    // TODO - implement other statements
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match *stmt {
            StmtBlock(ref block) => self.visit_block(block),
            StmtReturn(ref ret) => self.visit_stmt_return(ret),
            StmtBreak(ref stmt) => self.visit_stmt_break(stmt),
            StmtContinue(ref stmt) => self.visit_stmt_continue(stmt),
            StmtExpr(ref expr) => self.visit_stmt_expr(expr),
            StmtIf(ref stmt) => self.visit_stmt_if(stmt),
            StmtVar(ref stmt) => self.visit_stmt_var(stmt),
            StmtWhile(ref stmt) => self.visit_stmt_while(stmt),
            StmtLoop(ref stmt) => self.visit_stmt_loop(stmt),
            // StmtThrow(ref stmt) => {},
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
        self.gen.emit_jump(cond_lbl);
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

    fn visit_stmt_if(&mut self, stmt: &StmtIfType) {
        if let Some(ref else_block) = stmt.else_block {
            let else_lbl = self.gen.create_label();
            let end_lbl = self.gen.create_label();

            let cond_reg = self.visit_expr(&stmt.cond, DataDest::Alloc);
            self.gen.emit_jump_if_false(cond_reg, else_lbl);

            self.visit_stmt(&stmt.then_block);
            self.gen.emit_jump(end_lbl);

            self.gen.bind_label(else_lbl);
            self.visit_stmt(&else_block);
            self.gen.bind_label(end_lbl);
        } else {
            let end_lbl = self.gen.create_label();
            let cond_reg = self.visit_expr(&stmt.cond, DataDest::Alloc);
            self.gen.emit_jump_if_false(cond_reg, end_lbl);
            self.visit_stmt(&stmt.then_block);
            self.gen.bind_label(end_lbl);
        }
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExprType) {
        self.visit_expr(&stmt.expr, DataDest::Effect);
    }

    fn visit_block(&mut self, block: &StmtBlockType) {
        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }
    }

    fn visit_stmt_return(&mut self, ret: &StmtReturnType) {
        if let Some(ref expr) = ret.expr {
            let return_type: BytecodeType = self.specialize_type(self.fct.return_type).into();
            let result_reg = self.visit_expr(expr, DataDest::Alloc);

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
        } else {
            self.gen.emit_ret_void();
        }
    }

    fn visit_stmt_break(&mut self, _stmt: &StmtBreakType) {
        let end = self.loops.pop().unwrap().end;
        self.gen.emit_jump(end);
    }

    fn visit_stmt_continue(&mut self, _stmt: &StmtContinueType) {
        let cond = self.loops.last().unwrap().cond;
        self.gen.emit_jump(cond);
    }

    // TODO - implement other expressions
    fn visit_expr(&mut self, expr: &Expr, dest: DataDest) -> Register {
        match *expr {
            ExprUn(ref un) => self.visit_expr_un(un, dest),
            ExprBin(ref bin) => self.visit_expr_bin(bin, dest),
            ExprField(ref field) => self.visit_expr_field(field, dest),
            // ExprArray(ref array) => {},
            // ExprLitChar(ref lit) => {},
            ExprLitInt(ref lit) => self.visit_expr_lit_int(lit, dest),
            ExprLitFloat(ref lit) => self.visit_expr_lit_float(lit, dest),
            ExprLitStr(ref lit) => self.visit_expr_lit_string(lit, dest),
            // ExprLitStruct(ref lit) => {},
            ExprLitBool(ref lit) => self.visit_expr_lit_bool(lit, dest),
            ExprIdent(ref ident) => self.visit_expr_ident(ident, dest),
            ExprAssign(ref assign) => self.visit_expr_assign(assign, dest),
            // ExprCall(ref call) => {},
            // ExprDelegation(ref call) => {},
            // ExprSelf(ref selfie) => {},
            // ExprSuper(ref expr) => {},
            // ExprNil(ref nil) => {},
            // ExprConv(ref expr) => {},
            // ExprTry(ref expr) => {},
            // ExprLambda(ref expr) => {},
            _ => unimplemented!(),
        }
    }

    fn visit_expr_field(&mut self, e: &ExprFieldType, dest: DataDest) -> Register {
        let (class, field_id) = {
            let ident_type = self.src.map_idents.get(e.id).unwrap();

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
        let obj = self.visit_expr(&e.object, DataDest::Alloc);

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
                BytecodeType::Int => self.gen.emit_const_int(dest, lit.value as u32),
                BytecodeType::Long => self.gen.emit_const_long(dest, lit.value),
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

    fn visit_expr_bin(&mut self, e: &ExprBinType, dest: DataDest) -> Register {
        if e.op == BinOp::Cmp(CmpOp::Is) || e.op == BinOp::Cmp(CmpOp::IsNot) {
            self.emit_bin_is(e, dest)
        } else if e.op == BinOp::Or {
            self.emit_bin_or(e, dest)
        } else if e.op == BinOp::And {
            self.emit_bin_and(e, dest)
        } else if let Some(intrinsic) = self.get_intrinsic(e.id) {
            self.emit_intrinsic_bin(&e.lhs, &e.rhs, intrinsic, e.op, dest)
        } else {
            unimplemented!();
        }
    }

    fn emit_bin_is(&mut self, e: &ExprBinType, dest: DataDest) -> Register {
        if dest.is_effect() {
            self.visit_expr(&e.lhs, dest);
            self.visit_expr(&e.rhs, dest);
            return Register::invalid();
        }

        let dest = self.ensure_register(dest, BytecodeType::Bool);

        let lhs_reg = self.visit_expr(&e.lhs, DataDest::Alloc);
        let rhs_reg = self.visit_expr(&e.rhs, DataDest::Alloc);

        if e.op == BinOp::Cmp(CmpOp::Is) {
            self.gen.emit_test_eq_ptr(dest, lhs_reg, rhs_reg);
        } else {
            self.gen.emit_test_ne_ptr(dest, lhs_reg, rhs_reg);
        }

        dest
    }

    fn emit_bin_or(&mut self, e: &ExprBinType, dest: DataDest) -> Register {
        if dest.is_effect() {
            let end_lbl = self.gen.create_label();
            let dest = self.gen.add_register(BytecodeType::Bool);

            self.visit_expr(&e.lhs, DataDest::Reg(dest));
            self.gen.emit_jump_if_true(dest, end_lbl);
            self.visit_expr(&e.rhs, DataDest::Effect);
            self.gen.bind_label(end_lbl);

            Register::invalid()
        } else {
            let end_lbl = self.gen.create_label();
            let dest = self.ensure_register(dest, BytecodeType::Bool);

            self.visit_expr(&e.lhs, DataDest::Reg(dest));
            self.gen.emit_jump_if_true(dest, end_lbl);
            self.visit_expr(&e.rhs, DataDest::Reg(dest));
            self.gen.bind_label(end_lbl);

            dest
        }
    }

    fn emit_bin_and(&mut self, e: &ExprBinType, dest: DataDest) -> Register {
        if dest.is_effect() {
            let end_lbl = self.gen.create_label();
            let dest = self.gen.add_register(BytecodeType::Bool);

            self.visit_expr(&e.lhs, DataDest::Reg(dest));
            self.gen.emit_jump_if_false(dest, end_lbl);
            self.visit_expr(&e.rhs, DataDest::Effect);
            self.gen.bind_label(end_lbl);

            Register::invalid()
        } else {
            let end_lbl = self.gen.create_label();
            let dest = self.ensure_register(dest, BytecodeType::Bool);

            self.visit_expr(&e.lhs, DataDest::Reg(dest));
            self.gen.emit_jump_if_false(dest, end_lbl);
            self.visit_expr(&e.rhs, DataDest::Reg(dest));
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
            Intrinsic::IntEq | Intrinsic::IntCmp => BytecodeType::Bool,
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

        match intrinsic {
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
            _ => unimplemented!(),
        }

        dest
    }

    fn visit_expr_assign(&mut self, e: &ExprAssignType, dest: DataDest) -> Register {
        assert!(dest.is_effect());

        if e.lhs.is_ident() {
            let ident_type = *self.src.map_idents.get(e.lhs.id()).unwrap();
            match ident_type {
                IdentType::Var(var_id) => {
                    let var_reg = self.var_reg(var_id);
                    self.visit_expr(&e.rhs, DataDest::Reg(var_reg));
                }

                IdentType::Global(_) => unimplemented!(),
                IdentType::Field(_, _) => unimplemented!(),

                IdentType::Struct(_) => unimplemented!(),
                IdentType::Const(_) => unreachable!(),
                IdentType::Fct(_) => unreachable!(),
            }
        } else {
            unimplemented!();
        }

        Register::invalid()
    }

    fn visit_expr_ident(&mut self, ident: &ExprIdentType, dest: DataDest) -> Register {
        let ident_type = *self.src.map_idents.get(ident.id).unwrap();

        match ident_type {
            IdentType::Var(var_id) => {
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

            IdentType::Global(gid) => {
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

            IdentType::Field(_, _) => unimplemented!(),
            IdentType::Struct(_) => unimplemented!(),
            IdentType::Const(_) => unimplemented!(),

            IdentType::Fct(_) => unreachable!(),
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

    fn specialize_type(&self, ty: BuiltinType) -> BuiltinType {
        match ty {
            BuiltinType::ClassTypeParam(cls_id, id) => {
                assert!(self.fct.parent == FctParent::Class(cls_id));
                self.cls_type_params[id.idx()]
            }

            BuiltinType::FctTypeParam(fct_id, id) => {
                assert!(self.fct.id == fct_id);
                self.fct_type_params[id.idx()]
            }

            BuiltinType::Class(cls_id, list_id) => {
                let params = self.vm.lists.lock().get(list_id);
                let params: Vec<_> = params.iter().map(|t| self.specialize_type(t)).collect();
                let list_id = self.vm.lists.lock().insert(params.into());
                BuiltinType::Class(cls_id, list_id)
            }

            BuiltinType::Lambda(_) => unimplemented!(),

            _ => ty,
        }
    }

    fn get_intrinsic(&self, id: NodeId) -> Option<Intrinsic> {
        let fid = self.src.map_calls.get(id).unwrap().fct_id();

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

    fn reg(&self) -> Register {
        match self {
            DataDest::Effect | DataDest::Alloc => panic!("not a register"),
            DataDest::Reg(reg) => *reg,
        }
    }
}

#[cfg(test)]
mod tests {
    use bytecode::astgen;
    use bytecode::generate::{BytecodeFunction, BytecodeIdx, Register};
    use bytecode::opcode::Bytecode::*;
    use class::TypeParams;
    use ctxt::VM;
    use test;

    fn code(code: &'static str) -> BytecodeFunction {
        test::parse(code, |vm| {
            let fct_id = vm.fct_by_name("f").expect("no function `f`.");
            let tp = TypeParams::empty();
            astgen::generate(vm, fct_id, &tp, &tp)
        })
    }

    fn gen<F>(code: &'static str, testfct: F)
    where
        F: FnOnce(&VM, BytecodeFunction),
    {
        test::parse(code, |vm| {
            let fct_id = vm.fct_by_name("f").expect("no function `f`.");
            let tp = TypeParams::empty();
            let fct = astgen::generate(vm, fct_id, &tp, &tp);

            testfct(vm, fct);
        })
    }

    #[test]
    fn gen_load_field_byte() {
        gen(
            "class Foo(let bar: Byte) fun f(a: Foo) -> Byte { return a.bar; }",
            |vm, fct| {
                let (cls, field) = vm.field_by_name("Foo", "bar");
                let expected = vec![LoadFieldByte(r(1), r(0), cls, field), RetByte(r(1))];
                assert_eq!(expected, fct.code());
            },
        );
    }

    #[test]
    fn gen_load_field_bool() {
        gen(
            "class Foo(let bar: Bool) fun f(a: Foo) -> Bool { return a.bar; }",
            |vm, fct| {
                let (cls, field) = vm.field_by_name("Foo", "bar");
                let expected = vec![LoadFieldBool(r(1), r(0), cls, field), RetBool(r(1))];
                assert_eq!(expected, fct.code());
            },
        );
    }

    #[test]
    fn gen_load_field_char() {
        gen(
            "class Foo(let bar: Char) fun f(a: Foo) -> Char { return a.bar; }",
            |vm, fct| {
                let (cls, field) = vm.field_by_name("Foo", "bar");
                let expected = vec![LoadFieldChar(r(1), r(0), cls, field), RetChar(r(1))];
                assert_eq!(expected, fct.code());
            },
        );
    }

    #[test]
    fn gen_load_field_int() {
        gen(
            "class Foo(let bar: Int) fun f(a: Foo) -> Int { return a.bar; }",
            |vm, fct| {
                let (cls, field) = vm.field_by_name("Foo", "bar");
                let expected = vec![LoadFieldInt(r(1), r(0), cls, field), RetInt(r(1))];
                assert_eq!(expected, fct.code());
            },
        );
    }

    #[test]
    fn gen_load_field_long() {
        gen(
            "class Foo(let bar: Long) fun f(a: Foo) -> Long { return a.bar; }",
            |vm, fct| {
                let (cls, field) = vm.field_by_name("Foo", "bar");
                let expected = vec![LoadFieldLong(r(1), r(0), cls, field), RetLong(r(1))];
                assert_eq!(expected, fct.code());
            },
        );
    }

    #[test]
    fn gen_load_field_float() {
        gen(
            "class Foo(let bar: Float) fun f(a: Foo) -> Float { return a.bar; }",
            |vm, fct| {
                let (cls, field) = vm.field_by_name("Foo", "bar");
                let expected = vec![LoadFieldFloat(r(1), r(0), cls, field), RetFloat(r(1))];
                assert_eq!(expected, fct.code());
            },
        );
    }

    #[test]
    fn gen_load_field_double() {
        gen(
            "class Foo(let bar: Double) fun f(a: Foo) -> Double { return a.bar; }",
            |vm, fct| {
                let (cls, field) = vm.field_by_name("Foo", "bar");
                let expected = vec![LoadFieldDouble(r(1), r(0), cls, field), RetDouble(r(1))];
                assert_eq!(expected, fct.code());
            },
        );
    }

    #[test]
    fn gen_load_field_ptr() {
        gen(
            "class Foo(let bar: Object) fun f(a: Foo) -> Object { return a.bar; }",
            |vm, fct| {
                let (cls, field) = vm.field_by_name("Foo", "bar");
                let expected = vec![LoadFieldPtr(r(1), r(0), cls, field), RetPtr(r(1))];
                assert_eq!(expected, fct.code());
            },
        );
    }

    #[test]
    fn gen_add_int() {
        let fct = code("fun f() -> Int { return 1 + 2; }");
        let expected = vec![
            ConstInt(r(1), 1),
            ConstInt(r(2), 2),
            AddInt(r(0), r(1), r(2)),
            RetInt(r(0)),
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_id_int() {
        let fct = code("fun f(a: Int) -> Int { return a; }");
        let expected = vec![RetInt(r(0))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_id_ptr() {
        let fct = code("fun f(a: Object) -> Object { return a; }");
        let expected = vec![RetPtr(r(0))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_ptr_is() {
        let fct = code("fun f(a: Object, b: Object) -> Bool { return a === b; }");
        let expected = vec![TestEqPtr(r(2), r(0), r(1)), RetBool(r(2))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_ptr_is_not() {
        let fct = code("fun f(a: Object, b: Object) -> Bool { return a !== b; }");
        let expected = vec![TestNePtr(r(2), r(0), r(1)), RetBool(r(2))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_sub() {
        let fct = code("fun f(a: Int, b: Int) -> Int { return a - b; }");
        let expected = vec![SubInt(r(2), r(0), r(1)), RetInt(r(2))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_div() {
        let fct = code("fun f(a: Int, b: Int) -> Int { return a / b; }");
        let expected = vec![DivInt(r(2), r(0), r(1)), RetInt(r(2))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_mul() {
        let fct = code("fun f(a: Int, b: Int) -> Int { return a * b; }");
        let expected = vec![MulInt(r(2), r(0), r(1)), RetInt(r(2))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_stmt_var_init() {
        let fct = code("fun f() { let x = 1; }");
        let expected = vec![ConstInt(r(0), 1), RetVoid];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_stmt_while() {
        let fct = code("fun f() { while true { 0; } }");
        let code = vec![
            ConstTrue(r(0)),
            JumpIfFalse(r(0), bc(3)),
            Jump(bc(0)),
            RetVoid,
        ];
        assert_eq!(code, fct.code());
    }

    #[test]
    fn gen_stmt_if() {
        let fct = code("fun f(a: Bool) -> Int { if a { return 1; } return 0; }");
        let expected = vec![
            JumpIfFalse(r(0), bc(3)),
            ConstInt(r(1), 1),
            RetInt(r(1)),
            ConstZeroInt(r(2)),
            RetInt(r(2)),
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_stmt_if_else() {
        let fct = code("fun f(a: Bool) -> Int { if a { return 1; } else { return 2; } }");
        let expected = vec![
            JumpIfFalse(r(0), bc(4)),
            ConstInt(r(1), 1),
            RetInt(r(1)),
            Jump(bc(6)),
            ConstInt(r(2), 2),
            RetInt(r(2)),
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_stmt_break() {
        let fct = code("fun f() { while true { break; } }");
        let expected = vec![
            ConstTrue(r(0)),
            JumpIfFalse(r(0), bc(4)),
            Jump(bc(4)),
            Jump(bc(0)),
            RetVoid,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_stmt_continue() {
        let fct = code("fun f() { while true { continue; } }");
        let expected = vec![
            ConstTrue(r(0)),
            JumpIfFalse(r(0), bc(4)),
            Jump(bc(0)),
            Jump(bc(0)),
            RetVoid,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_lit_int() {
        let fct = code("fun f() -> Int { return 1; }");
        let expected = vec![ConstInt(r(0), 1), RetInt(r(0))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_lit_byte() {
        let fct = code("fun f() -> Byte { return 1Y; }");
        let expected = vec![ConstByte(r(0), 1), RetByte(r(0))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_lit_long() {
        let fct = code("fun f() -> Long { return 1L; }");
        let expected = vec![ConstLong(r(0), 1), RetLong(r(0))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_lit_float() {
        let fct = code("fun f() -> Float { return 1F; }");
        let expected = vec![ConstFloat(r(0), 1_f32), RetFloat(r(0))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_lit_double() {
        let fct = code("fun f() -> Double { return 1D; }");
        let expected = vec![ConstDouble(r(0), 1_f64), RetDouble(r(0))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_lit_string() {
        let fct = code("fun f() -> String { return \"z\"; }");
        let expected = vec![ConstString(r(0), String::from("z")), RetPtr(r(0))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_lit_byte_zero() {
        let fct = code("fun f() -> Byte { return 0Y; }");
        let expected = vec![ConstZeroByte(r(0)), RetByte(r(0))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_lit_int_zero() {
        let fct = code("fun f() -> Int { return 0; }");
        let expected = vec![ConstZeroInt(r(0)), RetInt(r(0))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_lit_long_zero() {
        let fct = code("fun f() -> Long { return 0L; }");
        let expected = vec![ConstZeroLong(r(0)), RetLong(r(0))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_lit_float_zero() {
        let fct = code("fun f() -> Float { return 0F; }");
        let expected = vec![ConstZeroFloat(r(0)), RetFloat(r(0))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_lit_double_zero() {
        let fct = code("fun f() -> Double { return 0D; }");
        let expected = vec![ConstZeroDouble(r(0)), RetDouble(r(0))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_or() {
        let fct = code("fun f(a: Bool, b: Bool) -> Bool { return a || b; }");
        let expected = vec![
            MovBool(r(2), r(0)),
            JumpIfTrue(r(2), bc(3)),
            MovBool(r(2), r(1)),
            RetBool(r(2)),
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_and() {
        let fct = code("fun f(a: Bool, b: Bool) -> Bool { return a && b; }");
        let expected = vec![
            MovBool(r(2), r(0)),
            JumpIfFalse(r(2), bc(3)),
            MovBool(r(2), r(1)),
            RetBool(r(2)),
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_plus() {
        let fct = code("fun f(a: Int) -> Int { return +a; }");
        let expected = vec![RetInt(r(0))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_neg() {
        let fct = code("fun f(a: Int) -> Int { return -a; }");
        let expected = vec![NegInt(r(1), r(0)), RetInt(r(1))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_not() {
        let fct = code("fun f(a: Bool) -> Bool { return !a; }");
        let expected = vec![NotBool(r(1), r(0)), RetBool(r(1))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_mod() {
        let fct = code("fun f(a: Int, b: Int) -> Int { return a % b; }");
        let expected = vec![ModInt(r(2), r(0), r(1)), RetInt(r(2))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_bit_or() {
        let fct = code("fun f(a: Int, b: Int) -> Int { return a | b; }");
        let expected = vec![OrInt(r(2), r(0), r(1)), RetInt(r(2))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_bit_and() {
        let fct = code("fun f(a: Int, b: Int) -> Int { return a & b; }");
        let expected = vec![AndInt(r(2), r(0), r(1)), RetInt(r(2))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_bit_xor() {
        let fct = code("fun f(a: Int, b: Int) -> Int { return a ^ b; }");
        let expected = vec![XorInt(r(2), r(0), r(1)), RetInt(r(2))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_bit_shiftl() {
        let fct = code("fun f(a: Int, b: Int) -> Int { return a << b; }");
        let expected = vec![ShlInt(r(2), r(0), r(1)), RetInt(r(2))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_bit_shiftr() {
        let fct = code("fun f(a: Int, b: Int) -> Int { return a >>> b; }");
        let expected = vec![ShrInt(r(2), r(0), r(1)), RetInt(r(2))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_bit_ashiftr() {
        let fct = code("fun f(a: Int, b: Int) -> Int { return a >> b; }");
        let expected = vec![SarInt(r(2), r(0), r(1)), RetInt(r(2))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_test_equal() {
        let fct = code("fun f(a: Int, b: Int) -> Bool { return a == b; }");
        let expected = vec![TestEqInt(r(2), r(0), r(1)), RetBool(r(2))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_test_notequal() {
        let fct = code("fun f(a: Int, b: Int) -> Bool { return a != b; }");
        let expected = vec![TestNeInt(r(2), r(0), r(1)), RetBool(r(2))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_test_lessthan() {
        let fct = code("fun f(a: Int, b: Int) -> Bool { return a < b; }");
        let expected = vec![TestLtInt(r(2), r(0), r(1)), RetBool(r(2))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_test_lessthanequal() {
        let fct = code("fun f(a: Int, b: Int) -> Bool { return a <= b; }");
        let expected = vec![TestLeInt(r(2), r(0), r(1)), RetBool(r(2))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_test_greaterthan() {
        let fct = code("fun f(a: Int, b: Int) -> Bool { return a > b; }");
        let expected = vec![TestGtInt(r(2), r(0), r(1)), RetBool(r(2))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_test_greaterthanequal() {
        let fct = code("fun f(a: Int, b: Int) -> Bool { return a >= b; }");
        let expected = vec![TestGeInt(r(2), r(0), r(1)), RetBool(r(2))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_ident() {
        let fct = code("fun f() -> Int { let x = 1; return x; }");
        let expected = vec![ConstInt(r(0), 1), RetInt(r(0))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_assign() {
        let fct = code("fun f() { var x = 1; x = 2; }");
        let expected = vec![ConstInt(r(0), 1), ConstInt(r(0), 2), RetVoid];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_return() {
        let fct = code("fun f() -> Int { return 1; }");
        let expected = vec![ConstInt(r(0), 1), RetInt(r(0))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_returnvoid() {
        let fct = code("fun f() { }");
        let expected = vec![RetVoid];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_load_global_bool() {
        gen("var a: Bool; fun f() -> Bool { return a; }", |vm, fct| {
            let gid = vm.global_by_name("a");
            let expected = vec![LoadGlobalBool(r(0), gid), RetBool(r(0))];
            assert_eq!(expected, fct.code());
        });
    }

    #[test]
    fn gen_load_global_byte() {
        gen("var a: Byte; fun f() -> Byte { return a; }", |vm, fct| {
            let gid = vm.global_by_name("a");
            let expected = vec![LoadGlobalByte(r(0), gid), RetByte(r(0))];
            assert_eq!(expected, fct.code());
        });
    }

    #[test]
    fn gen_load_global_char() {
        gen("var a: Char; fun f() -> Char { return a; }", |vm, fct| {
            let gid = vm.global_by_name("a");
            let expected = vec![LoadGlobalChar(r(0), gid), RetChar(r(0))];
            assert_eq!(expected, fct.code());
        });
    }

    #[test]
    fn gen_load_global_int() {
        gen("var a: Int; fun f() -> Int { return a; }", |vm, fct| {
            let gid = vm.global_by_name("a");
            let expected = vec![LoadGlobalInt(r(0), gid), RetInt(r(0))];
            assert_eq!(expected, fct.code());
        });
    }

    #[test]
    fn gen_load_global_long() {
        gen("var a: Long; fun f() -> Long { return a; }", |vm, fct| {
            let gid = vm.global_by_name("a");
            let expected = vec![LoadGlobalLong(r(0), gid), RetLong(r(0))];
            assert_eq!(expected, fct.code());
        });
    }

    #[test]
    fn gen_load_global_float() {
        gen("var a: Float; fun f() -> Float { return a; }", |vm, fct| {
            let gid = vm.global_by_name("a");
            let expected = vec![LoadGlobalFloat(r(0), gid), RetFloat(r(0))];
            assert_eq!(expected, fct.code());
        });
    }

    #[test]
    fn gen_load_global_double() {
        gen(
            "var a: Double; fun f() -> Double { return a; }",
            |vm, fct| {
                let gid = vm.global_by_name("a");
                let expected = vec![LoadGlobalDouble(r(0), gid), RetDouble(r(0))];
                assert_eq!(expected, fct.code());
            },
        );
    }

    #[test]
    fn gen_load_global_ptr() {
        gen(
            "var a: Object; fun f() -> Object { return a; }",
            |vm, fct| {
                let gid = vm.global_by_name("a");
                let expected = vec![LoadGlobalPtr(r(0), gid), RetPtr(r(0))];
                assert_eq!(expected, fct.code());
            },
        );
    }

    // #[test]
    // fn gen_fct_call_void_with_0_args() {
    //     gen("fun f() { g(); } fun g() { }", |vm, fct| {
    //         let fct_id = vm.fct_by_name("g").expect("g not found");
    //         let expected = vec![InvokeFctVoid(fct_id, r(0), 0), RetVoid];
    //         assert_eq!(expected, fct.code());
    //     });
    // }

    // #[test]
    // fn gen_fct_call_int_with_0_args() {
    //     gen(
    //         "fun f() -> Int { return g(); } fun g() -> Int { return 1; }",
    //         |vm, fct| {
    //             let fct_id = vm.fct_by_name("g").expect("g not found");
    //             let expected = vec![InvokeFctInt(r(0), fct_id, r(0), 0), RetInt(r(0))];
    //             assert_eq!(expected, fct.code());
    //         },
    //     );
    // }

    fn r(val: usize) -> Register {
        Register(val)
    }

    fn bc(val: usize) -> BytecodeIdx {
        BytecodeIdx(val)
    }
}
