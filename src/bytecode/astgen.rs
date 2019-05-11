use std::collections::HashMap;

use dora_parser::ast::Expr::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::*;

use bytecode::generate::{BytecodeFunction, BytecodeGenerator, BytecodeType, Label, Register};
use class::TypeParams;
use ctxt::{Fct, FctId, FctKind, FctSrc, IdentType, Intrinsic, VarId, VM};

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
            let ty: BytecodeType = var.ty.into();
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
        let ty = self.src.vars[var_id].ty;
        let ty: BytecodeType = ty.into();
        let var_reg = self.gen.add_register(ty);

        self.var_registers.insert(var_id, var_reg);

        if let Some(ref expr) = stmt.expr {
            self.visit_expr(expr);
            self.gen.emit_star(var_reg);
        }
    }

    fn visit_stmt_while(&mut self, stmt: &StmtWhileType) {
        let cond_lbl = self.gen.define_label();
        let end_lbl = self.gen.create_label();
        self.visit_expr(&stmt.cond);
        self.gen.emit_jump_if_false(end_lbl);
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

            self.visit_expr(&stmt.cond);
            self.gen.emit_jump_if_false(else_lbl);

            self.visit_stmt(&stmt.then_block);
            self.gen.emit_jump(end_lbl);

            self.gen.bind_label(else_lbl);
            self.visit_stmt(&else_block);
            self.gen.bind_label(end_lbl);
        } else {
            let end_lbl = self.gen.create_label();
            self.visit_expr(&stmt.cond);
            self.gen.emit_jump_if_false(end_lbl);
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
            self.visit_expr(expr);
            self.gen.emit_ret();
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
    fn visit_expr(&mut self, expr: &Expr) {
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

    fn visit_expr_lit_int(&mut self, lit: &ExprLitIntType) {
        if lit.value == 0 {
            self.gen.emit_lda_zero();
        } else {
            self.gen.emit_lda_int(lit.value);
        }
    }

    fn visit_expr_lit_bool(&mut self, lit: &ExprLitBoolType) {
        if lit.value {
            self.gen.emit_lda_true();
        } else {
            self.gen.emit_lda_false();
        }
    }

    fn visit_expr_un(&mut self, expr: &ExprUnType) {
        self.visit_expr(&expr.opnd);
        match expr.op {
            UnOp::Plus => {}
            UnOp::Neg => self.gen.emit_neg_int(),
            UnOp::Not => self.gen.emit_not_bool(),
        }
    }

    fn visit_expr_bin(&mut self, e: &ExprBinType) {
        if e.op == BinOp::Cmp(CmpOp::Is) || e.op == BinOp::Cmp(CmpOp::IsNot) {
            self.emit_bin_is(e);
        } else if e.op == BinOp::Or {
            self.emit_bin_or(e);
        } else if e.op == BinOp::And {
            self.emit_bin_and(e);
        } else if let Some(intrinsic) = self.get_intrinsic(e.id) {
            self.emit_intrinsic_bin(&e.lhs, &e.rhs, intrinsic, e.op);
        } else {
            unimplemented!();
        }
    }

    fn emit_bin_is(&mut self, e: &ExprBinType) {
        self.visit_expr(&e.lhs);
        let reg = self.gen.add_register(BytecodeType::Ptr);
        self.gen.emit_star(reg);
        self.visit_expr(&e.rhs);

        if e.op == BinOp::Cmp(CmpOp::Is) {
            self.gen.emit_test_eq_ptr(reg);
        } else {
            self.gen.emit_test_ne_ptr(reg);
        }
    }

    fn emit_bin_or(&mut self, e: &ExprBinType) {
        let end_lbl = self.gen.create_label();
        self.visit_expr(&e.lhs);
        self.gen.emit_jump_if_true(end_lbl);
        self.visit_expr(&e.rhs);
        self.gen.bind_label(end_lbl);
    }

    fn emit_bin_and(&mut self, e: &ExprBinType) {
        let end_lbl = self.gen.create_label();
        self.visit_expr(&e.lhs);
        self.gen.emit_jump_if_false(end_lbl);
        self.visit_expr(&e.rhs);
        self.gen.bind_label(end_lbl);
    }

    fn emit_intrinsic_bin(&mut self, lhs: &Expr, rhs: &Expr, intrinsic: Intrinsic, op: BinOp) {
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

        self.visit_expr(rhs);
        let reg = self.gen.add_register(ty);
        self.gen.emit_star(reg);
        self.visit_expr(lhs);

        match intrinsic {
            Intrinsic::IntAdd => self.gen.emit_add_int(reg),
            Intrinsic::IntSub => self.gen.emit_sub_int(reg),
            Intrinsic::IntMul => self.gen.emit_mul_int(reg),
            Intrinsic::IntDiv => self.gen.emit_div_int(reg),
            Intrinsic::IntMod => self.gen.emit_mod_int(reg),
            Intrinsic::IntOr => self.gen.emit_or_int(reg),
            Intrinsic::IntAnd => self.gen.emit_and_int(reg),
            Intrinsic::IntXor => self.gen.emit_xor_int(reg),
            Intrinsic::IntShl => self.gen.emit_shl_int(reg),
            Intrinsic::IntShr => self.gen.emit_shr_int(reg),
            Intrinsic::IntSar => self.gen.emit_sar_int(reg),
            Intrinsic::IntEq => match op {
                BinOp::Cmp(CmpOp::Eq) => self.gen.emit_test_eq_int(reg),
                BinOp::Cmp(CmpOp::Ne) => self.gen.emit_test_ne_int(reg),
                _ => unreachable!(),
            },
            Intrinsic::IntCmp => match op {
                BinOp::Cmp(CmpOp::Lt) => self.gen.emit_test_lt_int(reg),
                BinOp::Cmp(CmpOp::Le) => self.gen.emit_test_le_int(reg),
                BinOp::Cmp(CmpOp::Gt) => self.gen.emit_test_gt_int(reg),
                BinOp::Cmp(CmpOp::Ge) => self.gen.emit_test_ge_int(reg),
                _ => unreachable!(),
            },
            _ => unimplemented!(),
        }
    }

    fn visit_expr_assign(&mut self, e: &ExprAssignType) {
        if e.lhs.is_ident() {
            let ident_type = *self.src.map_idents.get(e.lhs.id()).unwrap();
            match ident_type {
                IdentType::Var(var_id) => {
                    let var_reg = self.var_reg(var_id);
                    self.visit_expr(&e.rhs);
                    self.gen.emit_star(var_reg);
                }

                _ => unimplemented!(),
            }
        } else {
            unimplemented!();
        }
    }

    fn visit_expr_ident(&mut self, ident: &ExprIdentType) {
        let ident_type = *self.src.map_idents.get(ident.id).unwrap();

        match ident_type {
            IdentType::Var(var_id) => {
                let reg = self.var_reg(var_id);
                self.gen.emit_ldar(reg);
            }

            _ => unimplemented!(),
        }
    }

    fn var_reg(&self, var_id: VarId) -> Register {
        *self
            .var_registers
            .get(&var_id)
            .expect("no register for var found")
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
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            AddInt(Register(0)),
            Ret,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_id_int() {
        let fct = code("fun f(a: Int) -> Int { return a; }");
        let expected = vec![Ldar(Register(0)), Ret];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_id_ptr() {
        let fct = code("fun f(a: Object) -> Object { return a; }");
        let expected = vec![Ldar(Register(0)), Ret];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_ptr_is() {
        let fct = code("fun f(a: Object, b: Object) -> Bool { return a === b; }");
        let expected = vec![
            Ldar(Register(0)),
            Star(Register(2)),
            Ldar(Register(1)),
            TestEqPtr(Register(2)),
            Ret,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_ptr_is_not() {
        let fct = code("fun f(a: Object, b: Object) -> Bool { return a !== b; }");
        let expected = vec![
            Ldar(Register(0)),
            Star(Register(2)),
            Ldar(Register(1)),
            TestNePtr(Register(2)),
            Ret,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_sub() {
        let fct = code("fun f() -> Int { return 1 - 2; }");
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            SubInt(Register(0)),
            Ret,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_div() {
        let fct = code("fun f() -> Int { return 1 / 2; }");
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            DivInt(Register(0)),
            Ret,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_mul() {
        let fct = code("fun f() -> Int { return 1 * 2; }");
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            MulInt(Register(0)),
            Ret,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_stmt_var_init() {
        let fct = code("fun f() { let x = 1; }");
        let expected = vec![LdaInt(1), Star(Register(0)), RetVoid];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_stmt_while() {
        let fct = code("fun f() { while true { 0; } }");
        let code = vec![
            LdaTrue,
            JumpIfFalse(BytecodeIdx(4)),
            LdaZero,
            Jump(BytecodeIdx(0)),
            RetVoid,
        ];
        assert_eq!(code, fct.code());
    }

    #[test]
    fn gen_stmt_if() {
        let fct = code("fun f() { if true { 1; } }");
        let expected = vec![LdaTrue, JumpIfFalse(BytecodeIdx(3)), LdaInt(1), RetVoid];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_stmt_if_else() {
        let fct = code("fun f() { if true { 1; } else { 2; } }");
        let expected = vec![
            LdaTrue,
            JumpIfFalse(BytecodeIdx(4)),
            LdaInt(1),
            Jump(BytecodeIdx(5)),
            LdaInt(2),
            RetVoid,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_stmt_break() {
        let fct = code("fun f() { while true { break; } }");
        let expected = vec![
            LdaTrue,
            JumpIfFalse(BytecodeIdx(4)),
            Jump(BytecodeIdx(4)),
            Jump(BytecodeIdx(0)),
            RetVoid,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_stmt_continue() {
        let fct = code("fun f() { while true { continue; } }");
        let expected = vec![
            LdaTrue,
            JumpIfFalse(BytecodeIdx(4)),
            Jump(BytecodeIdx(0)),
            Jump(BytecodeIdx(0)),
            RetVoid,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_lit_int() {
        let fct = code("fun f() { 1; }");
        let expected = vec![LdaInt(1), RetVoid];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_lit_zero() {
        let fct = code("fun f() { 0; }");
        let expected = vec![LdaZero, RetVoid];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_or() {
        let fct = code("fun f(a: Bool, b: Bool) -> Bool { return a || b; }");
        let expected = vec![
            Ldar(Register(0)),
            JumpIfTrue(BytecodeIdx(3)),
            Ldar(Register(1)),
            Ret,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_and() {
        let fct = code("fun f(a: Bool, b: Bool) -> Bool { return a && b; }");
        let expected = vec![
            Ldar(Register(0)),
            JumpIfFalse(BytecodeIdx(3)),
            Ldar(Register(1)),
            Ret,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_plus() {
        let fct = code("fun f() { +1; }");
        let expected = vec![LdaInt(1), RetVoid];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_neg() {
        let fct = code("fun f() -> Int { return -1; }");
        let expected = vec![LdaInt(1), NegInt, Ret];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_not() {
        let fct = code("fun f() -> Bool { return !true; }");
        let expected = vec![LdaTrue, NotBool, Ret];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_mod() {
        let fct = code("fun f() -> Int { return 1 % 2; }");
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            ModInt(Register(0)),
            Ret,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_bit_or() {
        let fct = code("fun f() -> Int { return 1 | 2; }");
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            OrInt(Register(0)),
            Ret,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_bit_and() {
        let fct = code("fun f() -> Int { return 1 & 2; }");
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            AndInt(Register(0)),
            Ret,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_bit_xor() {
        let fct = code("fun f() -> Int { return 1 ^ 2; }");
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            XorInt(Register(0)),
            Ret,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_bit_shiftl() {
        let fct = code("fun f() -> Int { return 1 << 2; }");
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            ShlInt(Register(0)),
            Ret,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_bit_shiftr() {
        let fct = code("fun f() -> Int { return 1 >>> 2; }");
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            ShrInt(Register(0)),
            Ret,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_bit_ashiftr() {
        let fct = code("fun f() -> Int { return 1 >> 2; }");
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            SarInt(Register(0)),
            Ret,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_test_equal() {
        let fct = code("fun f() -> Bool { return 1 == 2; }");
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            TestEqInt(Register(0)),
            Ret,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_test_notequal() {
        let fct = code("fun f() -> Bool { return 1 != 2; }");
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            TestNeInt(Register(0)),
            Ret,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_test_lessthan() {
        let fct = code("fun f() -> Bool { return 1 < 2; }");
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            TestLtInt(Register(0)),
            Ret,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_test_lessthanequal() {
        let fct = code("fun f() -> Bool { return 1 <= 2; }");
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            TestLeInt(Register(0)),
            Ret,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_test_greaterthan() {
        let fct = code("fun f() -> Bool { return 1 > 2; }");
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            TestGtInt(Register(0)),
            Ret,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_test_greaterthanequal() {
        let fct = code("fun f() -> Bool { return 1 >= 2; }");
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            TestGeInt(Register(0)),
            Ret,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_ident() {
        let fct = code("fun f() -> Int { let x = 1; return x; }");
        let expected = vec![LdaInt(1), Star(Register(0)), Ldar(Register(0)), Ret];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_assign() {
        let fct = code("fun f() { var x = 1; x = 2; }");
        let expected = vec![
            LdaInt(1),
            Star(Register(0)),
            LdaInt(2),
            Star(Register(0)),
            RetVoid,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_return() {
        let fct = code("fun f() -> Int { return 1; }");
        let expected = vec![LdaInt(1), Ret];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_returnvoid() {
        let fct = code("fun f() { }");
        let expected = vec![RetVoid];
        assert_eq!(expected, fct.code());
    }
}
