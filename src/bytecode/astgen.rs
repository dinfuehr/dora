use std::collections::HashMap;

use dora_parser::ast::Expr::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::*;
use dora_parser::lexer::token::IntSuffix;

use bytecode::generate::{BytecodeFunction, BytecodeGenerator, BytecodeType, Label, Register};
use class::TypeParams;
use ctxt::{Fct, FctId, FctKind, FctParent, FctSrc, IdentType, Intrinsic, VarId, VM};
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
            let expr_reg = self.visit_expr(expr);

            match ty {
                BytecodeType::Bool => self.gen.emit_mov_bool(var_reg, expr_reg),
                BytecodeType::Byte => self.gen.emit_mov_byte(var_reg, expr_reg),
                BytecodeType::Char => self.gen.emit_mov_char(var_reg, expr_reg),
                BytecodeType::Int => self.gen.emit_mov_int(var_reg, expr_reg),
                BytecodeType::Long => self.gen.emit_mov_long(var_reg, expr_reg),
                BytecodeType::Float => self.gen.emit_mov_float(var_reg, expr_reg),
                BytecodeType::Double => self.gen.emit_mov_double(var_reg, expr_reg),
                BytecodeType::Ptr => self.gen.emit_mov_ptr(var_reg, expr_reg),
            }
        }
    }

    fn visit_stmt_while(&mut self, stmt: &StmtWhileType) {
        let cond_lbl = self.gen.define_label();
        let end_lbl = self.gen.create_label();
        let cond_reg = self.visit_expr(&stmt.cond);
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

            let cond_reg = self.visit_expr(&stmt.cond);
            self.gen.emit_jump_if_false(cond_reg, else_lbl);

            self.visit_stmt(&stmt.then_block);
            self.gen.emit_jump(end_lbl);

            self.gen.bind_label(else_lbl);
            self.visit_stmt(&else_block);
            self.gen.bind_label(end_lbl);
        } else {
            let end_lbl = self.gen.create_label();
            let cond_reg = self.visit_expr(&stmt.cond);
            self.gen.emit_jump_if_false(cond_reg, end_lbl);
            self.visit_stmt(&stmt.then_block);
            self.gen.bind_label(end_lbl);
        }
    }

    fn visit_stmt_expr(&mut self, stmt: &StmtExprType) {
        self.visit_expr(&stmt.expr);
    }

    fn visit_block(&mut self, block: &StmtBlockType) {
        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }
    }

    fn visit_stmt_return(&mut self, ret: &StmtReturnType) {
        if let Some(ref expr) = ret.expr {
            let return_type: BytecodeType = self.fct.return_type.into();
            let result_reg = self.visit_expr(expr);

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
    fn visit_expr(&mut self, expr: &Expr) -> Register {
        match *expr {
            ExprUn(ref un) => self.visit_expr_un(un),
            ExprBin(ref bin) => self.visit_expr_bin(bin),
            // ExprField(ref field) => {},
            // ExprArray(ref array) => {},
            // ExprLitChar(ref lit) => {},
            ExprLitInt(ref lit) => self.visit_expr_lit_int(lit),
            // ExprLitFloat(ref lit) => {},
            // ExprLitStr(ref lit) => {},
            // ExprLitStruct(ref lit) => {},
            ExprLitBool(ref lit) => self.visit_expr_lit_bool(lit),
            ExprIdent(ref ident) => self.visit_expr_ident(ident),
            ExprAssign(ref assign) => self.visit_expr_assign(assign),
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

    fn visit_expr_lit_int(&mut self, lit: &ExprLitIntType) -> Register {
        let ty = match lit.suffix {
            IntSuffix::Byte => BytecodeType::Byte,
            IntSuffix::Int => BytecodeType::Int,
            IntSuffix::Long => BytecodeType::Long,
        };

        let dest = self.gen.add_register(ty);

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

    fn visit_expr_lit_bool(&mut self, lit: &ExprLitBoolType) -> Register {
        let dest = self.gen.add_register(BytecodeType::Bool);

        if lit.value {
            self.gen.emit_const_true(dest);
        } else {
            self.gen.emit_const_false(dest);
        }

        dest
    }

    fn visit_expr_un(&mut self, expr: &ExprUnType) -> Register {
        let opnd_reg = self.visit_expr(&expr.opnd);

        if let Some(intrinsic) = self.get_intrinsic(expr.id) {
            match intrinsic {
                Intrinsic::IntPlus
                | Intrinsic::LongPlus
                | Intrinsic::FloatPlus
                | Intrinsic::DoublePlus => opnd_reg,

                Intrinsic::IntNeg => {
                    let dest_reg = self.gen.add_register(BytecodeType::Int);
                    self.gen.emit_neg_int(dest_reg, opnd_reg);
                    dest_reg
                }

                Intrinsic::LongNeg => {
                    let dest_reg = self.gen.add_register(BytecodeType::Long);
                    self.gen.emit_neg_long(dest_reg, opnd_reg);
                    dest_reg
                }

                Intrinsic::BoolNot => {
                    let dest_reg = self.gen.add_register(BytecodeType::Bool);
                    self.gen.emit_not_bool(dest_reg, opnd_reg);
                    dest_reg
                }

                _ => unimplemented!(),
            }
        } else {
            unimplemented!()
        }
    }

    fn visit_expr_bin(&mut self, e: &ExprBinType) -> Register {
        if e.op == BinOp::Cmp(CmpOp::Is) || e.op == BinOp::Cmp(CmpOp::IsNot) {
            self.emit_bin_is(e)
        } else if e.op == BinOp::Or {
            self.emit_bin_or(e)
        } else if e.op == BinOp::And {
            self.emit_bin_and(e)
        } else if let Some(intrinsic) = self.get_intrinsic(e.id) {
            self.emit_intrinsic_bin(&e.lhs, &e.rhs, intrinsic, e.op)
        } else {
            unimplemented!();
        }
    }

    fn emit_bin_is(&mut self, e: &ExprBinType) -> Register {
        let dest_reg = self.gen.add_register(BytecodeType::Bool);
        let lhs_reg = self.visit_expr(&e.lhs);
        let rhs_reg = self.visit_expr(&e.rhs);

        if e.op == BinOp::Cmp(CmpOp::Is) {
            self.gen.emit_test_eq_ptr(dest_reg, lhs_reg, rhs_reg);
        } else {
            self.gen.emit_test_ne_ptr(dest_reg, lhs_reg, rhs_reg);
        }

        dest_reg
    }

    fn emit_bin_or(&mut self, e: &ExprBinType) -> Register {
        let dest_reg = self.gen.add_register(BytecodeType::Bool);
        let end_lbl = self.gen.create_label();
        let lhs_reg = self.visit_expr(&e.lhs);
        self.gen.emit_mov_bool(dest_reg, lhs_reg);
        self.gen.emit_jump_if_true(dest_reg, end_lbl);
        let rhs_reg = self.visit_expr(&e.rhs);
        self.gen.emit_mov_bool(dest_reg, rhs_reg);
        self.gen.bind_label(end_lbl);

        dest_reg
    }

    fn emit_bin_and(&mut self, e: &ExprBinType) -> Register {
        let dest_reg = self.gen.add_register(BytecodeType::Bool);
        let end_lbl = self.gen.create_label();
        let lhs_reg = self.visit_expr(&e.lhs);
        self.gen.emit_mov_bool(dest_reg, lhs_reg);
        self.gen.emit_jump_if_false(dest_reg, end_lbl);
        let rhs_reg = self.visit_expr(&e.rhs);
        self.gen.emit_mov_bool(dest_reg, rhs_reg);
        self.gen.bind_label(end_lbl);

        dest_reg
    }

    fn emit_intrinsic_bin(
        &mut self,
        lhs: &Expr,
        rhs: &Expr,
        intrinsic: Intrinsic,
        op: BinOp,
    ) -> Register {
        let ty = match intrinsic {
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
            | Intrinsic::IntEq
            | Intrinsic::IntCmp => BytecodeType::Int,
            _ => unimplemented!(),
        };

        let dest_reg = self.gen.add_register(ty);
        let lhs_reg = self.visit_expr(lhs);
        let rhs_reg = self.visit_expr(rhs);

        match intrinsic {
            Intrinsic::IntAdd => self.gen.emit_add_int(dest_reg, lhs_reg, rhs_reg),
            Intrinsic::IntSub => self.gen.emit_sub_int(dest_reg, lhs_reg, rhs_reg),
            Intrinsic::IntMul => self.gen.emit_mul_int(dest_reg, lhs_reg, rhs_reg),
            Intrinsic::IntDiv => self.gen.emit_div_int(dest_reg, lhs_reg, rhs_reg),
            Intrinsic::IntMod => self.gen.emit_mod_int(dest_reg, lhs_reg, rhs_reg),
            Intrinsic::IntOr => self.gen.emit_or_int(dest_reg, lhs_reg, rhs_reg),
            Intrinsic::IntAnd => self.gen.emit_and_int(dest_reg, lhs_reg, rhs_reg),
            Intrinsic::IntXor => self.gen.emit_xor_int(dest_reg, lhs_reg, rhs_reg),
            Intrinsic::IntShl => self.gen.emit_shl_int(dest_reg, lhs_reg, rhs_reg),
            Intrinsic::IntShr => self.gen.emit_shr_int(dest_reg, lhs_reg, rhs_reg),
            Intrinsic::IntSar => self.gen.emit_sar_int(dest_reg, lhs_reg, rhs_reg),
            Intrinsic::IntEq => match op {
                BinOp::Cmp(CmpOp::Eq) => self.gen.emit_test_eq_int(dest_reg, lhs_reg, rhs_reg),
                BinOp::Cmp(CmpOp::Ne) => self.gen.emit_test_ne_int(dest_reg, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            Intrinsic::IntCmp => match op {
                BinOp::Cmp(CmpOp::Lt) => self.gen.emit_test_lt_int(dest_reg, lhs_reg, rhs_reg),
                BinOp::Cmp(CmpOp::Le) => self.gen.emit_test_le_int(dest_reg, lhs_reg, rhs_reg),
                BinOp::Cmp(CmpOp::Gt) => self.gen.emit_test_gt_int(dest_reg, lhs_reg, rhs_reg),
                BinOp::Cmp(CmpOp::Ge) => self.gen.emit_test_ge_int(dest_reg, lhs_reg, rhs_reg),
                _ => unreachable!(),
            },
            _ => unimplemented!(),
        }

        dest_reg
    }

    fn visit_expr_assign(&mut self, e: &ExprAssignType) -> Register {
        if e.lhs.is_ident() {
            let ident_type = *self.src.map_idents.get(e.lhs.id()).unwrap();
            match ident_type {
                IdentType::Var(var_id) => {
                    let var_reg = self.var_reg(var_id);
                    let rhs_reg = self.visit_expr(&e.rhs);
                    let ty: BytecodeType = self.specialize_type(self.src.vars[var_id].ty).into();

                    match ty {
                        BytecodeType::Bool => self.gen.emit_mov_bool(var_reg, rhs_reg),
                        BytecodeType::Byte => self.gen.emit_mov_byte(var_reg, rhs_reg),
                        BytecodeType::Char => self.gen.emit_mov_char(var_reg, rhs_reg),
                        BytecodeType::Int => self.gen.emit_mov_int(var_reg, rhs_reg),
                        BytecodeType::Long => self.gen.emit_mov_long(var_reg, rhs_reg),
                        BytecodeType::Float => self.gen.emit_mov_float(var_reg, rhs_reg),
                        BytecodeType::Double => self.gen.emit_mov_double(var_reg, rhs_reg),
                        BytecodeType::Ptr => self.gen.emit_mov_ptr(var_reg, rhs_reg),
                    }
                }

                _ => unimplemented!(),
            }
        } else {
            unimplemented!();
        }

        Register::invalid()
    }

    fn visit_expr_ident(&mut self, ident: &ExprIdentType) -> Register {
        let ident_type = *self.src.map_idents.get(ident.id).unwrap();

        match ident_type {
            IdentType::Var(var_id) => self.var_reg(var_id),

            _ => unimplemented!(),
        }
    }

    fn var_reg(&self, var_id: VarId) -> Register {
        *self
            .var_registers
            .get(&var_id)
            .expect("no register for var found")
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

#[cfg(test)]
mod tests {
    use bytecode::astgen;
    use bytecode::generate::{BytecodeFunction, BytecodeIdx, Register};
    use bytecode::opcode::Bytecode::*;
    use class::TypeParams;
    use test;

    fn code(code: &'static str) -> BytecodeFunction {
        test::parse(code, |vm| {
            let fct_id = vm.fct_by_name("f").expect("no function `f`.");
            let tp = TypeParams::empty();
            astgen::generate(vm, fct_id, &tp, &tp)
        })
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
        let expected = vec![ConstInt(r(1), 1), MovInt(r(0), r(1)), RetVoid];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_stmt_while() {
        let fct = code("fun f() { while true { 0; } }");
        let code = vec![
            ConstTrue(r(0)),
            JumpIfFalse(r(0), bc(4)),
            ConstZeroInt(r(1)),
            Jump(bc(0)),
            RetVoid,
        ];
        assert_eq!(code, fct.code());
    }

    #[test]
    fn gen_stmt_if() {
        let fct = code("fun f() { if true { 1; } }");
        let expected = vec![
            ConstTrue(r(0)),
            JumpIfFalse(r(0), bc(3)),
            ConstInt(r(1), 1),
            RetVoid,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_stmt_if_else() {
        let fct = code("fun f() { if true { 1; } else { 2; } }");
        let expected = vec![
            ConstTrue(r(0)),
            JumpIfFalse(r(0), bc(4)),
            ConstInt(r(1), 1),
            Jump(bc(5)),
            ConstInt(r(2), 2),
            RetVoid,
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
        let fct = code("fun f() { +1; }");
        let expected = vec![ConstInt(r(0), 1), RetVoid];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_neg() {
        let fct = code("fun f() -> Int { return -1; }");
        let expected = vec![ConstInt(r(0), 1), NegInt(r(1), r(0)), RetInt(r(1))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_not() {
        let fct = code("fun f() -> Bool { return !true; }");
        let expected = vec![ConstTrue(r(0)), NotBool(r(1), r(0)), RetBool(r(1))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_mod() {
        let fct = code("fun f() -> Int { return 1 % 2; }");
        let expected = vec![
            ConstInt(r(1), 1),
            ConstInt(r(2), 2),
            ModInt(r(0), r(1), r(2)),
            RetInt(r(0)),
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_bit_or() {
        let fct = code("fun f() -> Int { return 1 | 2; }");
        let expected = vec![
            ConstInt(r(1), 1),
            ConstInt(r(2), 2),
            OrInt(r(0), r(1), r(2)),
            RetInt(r(0)),
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_bit_and() {
        let fct = code("fun f() -> Int { return 1 & 2; }");
        let expected = vec![
            ConstInt(r(1), 1),
            ConstInt(r(2), 2),
            AndInt(r(0), r(1), r(2)),
            RetInt(r(0)),
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_bit_xor() {
        let fct = code("fun f() -> Int { return 1 ^ 2; }");
        let expected = vec![
            ConstInt(r(1), 1),
            ConstInt(r(2), 2),
            XorInt(r(0), r(1), r(2)),
            RetInt(r(0)),
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_bit_shiftl() {
        let fct = code("fun f() -> Int { return 1 << 2; }");
        let expected = vec![
            ConstInt(r(1), 1),
            ConstInt(r(2), 2),
            ShlInt(r(0), r(1), r(2)),
            RetInt(r(0)),
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_bit_shiftr() {
        let fct = code("fun f() -> Int { return 1 >>> 2; }");
        let expected = vec![
            ConstInt(r(1), 1),
            ConstInt(r(2), 2),
            ShrInt(r(0), r(1), r(2)),
            RetInt(r(0)),
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_bit_ashiftr() {
        let fct = code("fun f() -> Int { return 1 >> 2; }");
        let expected = vec![
            ConstInt(r(1), 1),
            ConstInt(r(2), 2),
            SarInt(r(0), r(1), r(2)),
            RetInt(r(0)),
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_test_equal() {
        let fct = code("fun f() -> Bool { return 1 == 2; }");
        let expected = vec![
            ConstInt(r(1), 1),
            ConstInt(r(2), 2),
            TestEqInt(r(0), r(1), r(2)),
            RetBool(r(0)),
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_test_notequal() {
        let fct = code("fun f() -> Bool { return 1 != 2; }");
        let expected = vec![
            ConstInt(r(1), 1),
            ConstInt(r(2), 2),
            TestNeInt(r(0), r(1), r(2)),
            RetBool(r(0)),
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_test_lessthan() {
        let fct = code("fun f() -> Bool { return 1 < 2; }");
        let expected = vec![
            ConstInt(r(1), 1),
            ConstInt(r(2), 2),
            TestLtInt(r(0), r(1), r(2)),
            RetBool(r(0)),
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_test_lessthanequal() {
        let fct = code("fun f() -> Bool { return 1 <= 2; }");
        let expected = vec![
            ConstInt(r(1), 1),
            ConstInt(r(2), 2),
            TestLeInt(r(0), r(1), r(2)),
            RetBool(r(0)),
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_test_greaterthan() {
        let fct = code("fun f() -> Bool { return 1 > 2; }");
        let expected = vec![
            ConstInt(r(1), 1),
            ConstInt(r(2), 2),
            TestGtInt(r(0), r(1), r(2)),
            RetBool(r(0)),
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_test_greaterthanequal() {
        let fct = code("fun f() -> Bool { return 1 >= 2; }");
        let expected = vec![
            ConstInt(r(1), 1),
            ConstInt(r(2), 2),
            TestGeInt(r(0), r(1), r(2)),
            RetBool(r(0)),
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_ident() {
        let fct = code("fun f() -> Int { let x = 1; return x; }");
        let expected = vec![ConstInt(r(1), 1), MovInt(r(0), r(1)), RetInt(r(0))];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_assign() {
        let fct = code("fun f() { var x = 1; x = 2; }");
        let expected = vec![
            ConstInt(r(1), 1),
            MovInt(r(0), r(1)),
            ConstInt(r(2), 2),
            MovInt(r(0), r(2)),
            RetVoid,
        ];
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

    fn r(val: usize) -> Register {
        Register(val)
    }

    fn bc(val: usize) -> BytecodeIdx {
        BytecodeIdx(val)
    }
}
