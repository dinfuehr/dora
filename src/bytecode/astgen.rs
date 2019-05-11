use std::collections::HashMap;

use dora_parser::ast::Expr::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::*;

use bytecode::generate::{BytecodeFunction, BytecodeGenerator, BytecodeType, Label, Register};
use class::TypeParams;
use ctxt::{Fct, FctId, FctSrc, IdentType, VarId, VM};

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
            let reg = self.gen.add_register(BytecodeType::Ref);
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
            UnOp::Neg => self.gen.emit_neg(),
            UnOp::Not => self.gen.emit_logical_not(),
        }
    }

    fn visit_expr_bin(&mut self, expr: &ExprBinType) {
        self.visit_expr(&expr.rhs);
        let rhs_reg = self.gen.add_register(BytecodeType::Int);
        self.gen.emit_star(rhs_reg);
        self.visit_expr(&expr.lhs);
        match expr.op {
            BinOp::Add => self.gen.emit_add_int(rhs_reg),
            BinOp::Sub => self.gen.emit_sub(rhs_reg),
            BinOp::Mul => self.gen.emit_mul(rhs_reg),
            BinOp::Div => self.gen.emit_div(rhs_reg),
            BinOp::Mod => self.gen.emit_mod(rhs_reg),
            BinOp::BitOr => self.gen.emit_bitwise_or(rhs_reg),
            BinOp::BitAnd => self.gen.emit_bitwise_and(rhs_reg),
            BinOp::BitXor => self.gen.emit_bitwise_xor(rhs_reg),
            BinOp::ShiftL => self.gen.emit_shift_left(rhs_reg),
            BinOp::LogicalShiftR => self.gen.emit_shift_right(rhs_reg),
            BinOp::ArithShiftR => self.gen.emit_arith_shift_right(rhs_reg),
            // BinOp::Or => { },
            // BinOp::And => { },
            BinOp::Cmp(op) => match op {
                CmpOp::Eq => self.gen.emit_test_eq(rhs_reg),
                CmpOp::Ne => self.gen.emit_test_ne(rhs_reg),
                CmpOp::Lt => self.gen.emit_test_lt(rhs_reg),
                CmpOp::Le => self.gen.emit_test_le(rhs_reg),
                CmpOp::Gt => self.gen.emit_test_gt(rhs_reg),
                CmpOp::Ge => self.gen.emit_test_ge(rhs_reg),
                _ => unimplemented!(),
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
        *self.var_registers.get(&var_id).expect("no register for var found")
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
            Return,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_id_int() {
        let fct = code("fun f(a: Int) -> Int { return a; }");
        let expected = vec![
            Ldar(Register(0)),
            Return,
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
            Sub(Register(0)),
            Return,
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
            Div(Register(0)),
            Return,
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
            Mul(Register(0)),
            Return,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_stmt_var_init() {
        let fct = code("fun f() { let x = 1; }");
        let expected = vec![LdaInt(1), Star(Register(0)), ReturnVoid];
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
            ReturnVoid,
        ];
        assert_eq!(code, fct.code());
    }

    #[test]
    fn gen_stmt_if() {
        let fct = code("fun f() { if true { 1; } }");
        let expected = vec![LdaTrue, JumpIfFalse(BytecodeIdx(3)), LdaInt(1), ReturnVoid];
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
            ReturnVoid,
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
            ReturnVoid,
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
            ReturnVoid,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_lit_int() {
        let fct = code("fun f() { 1; }");
        let expected = vec![LdaInt(1), ReturnVoid];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_lit_zero() {
        let fct = code("fun f() { 0; }");
        let expected = vec![LdaZero, ReturnVoid];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_plus() {
        let fct = code("fun f() { +1; }");
        let expected = vec![LdaInt(1), ReturnVoid];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_neg() {
        let fct = code("fun f() -> Int { return -1; }");
        let expected = vec![LdaInt(1), Neg, Return];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_not() {
        let fct = code("fun f() -> Bool { return !true; }");
        let expected = vec![LdaTrue, LogicalNot, Return];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_mod() {
        let fct = code("fun f() -> Int { return 1 % 2; }");
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            Mod(Register(0)),
            Return,
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
            BitwiseOr(Register(0)),
            Return,
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
            BitwiseAnd(Register(0)),
            Return,
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
            BitwiseXor(Register(0)),
            Return,
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
            ShiftLeft(Register(0)),
            Return,
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
            ShiftRight(Register(0)),
            Return,
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
            ArithShiftRight(Register(0)),
            Return,
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
            TestEqual(Register(0)),
            Return,
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
            TestNotEqual(Register(0)),
            Return,
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
            TestLessThan(Register(0)),
            Return,
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
            TestLessThanOrEqual(Register(0)),
            Return,
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
            TestGreatherThan(Register(0)),
            Return,
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
            TestGreatherThanOrEqual(Register(0)),
            Return,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_ident() {
        let fct = code("fun f() -> Int { let x = 1; return x; }");
        let expected = vec![LdaInt(1), Star(Register(0)), Ldar(Register(0)), Return];
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
            ReturnVoid,
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_return() {
        let fct = code("fun f() -> Int { return 1; }");
        let expected = vec![LdaInt(1), Return];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_returnvoid() {
        let fct = code("fun f() { }");
        let expected = vec![ReturnVoid];
        assert_eq!(expected, fct.code());
    }
}
