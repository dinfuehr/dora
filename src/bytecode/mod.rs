use std::collections::HashMap;

use dora_parser::ast::visit::Visitor;
use dora_parser::ast::Expr::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::*;
use dora_parser::interner::Name;

use bytecode::generate::{BytecodeFunction, BytecodeGenerator, BytecodeIdx, Label, Register};
use class::TypeParams;
use ctxt::{Fct, FctId, FctSrc, VM};

mod generate;
mod opcode;

pub struct Context {
    var_map: HashMap<Name, Register>,
}

impl Context {
    pub fn new() -> Context {
        Context {
            var_map: HashMap::new(),
        }
    }

    pub fn new_var(&mut self, var: Name, reg: Register) {
        self.var_map.insert(var, reg);
    }

    pub fn get_reg(&self, var: Name) -> Option<&Register> {
        let reg = self.var_map.get(&var);
        reg
    }
}

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
    _vm: &VM<'ast>,
    fct: &Fct<'ast>,
    _src: &mut FctSrc,
    _cls_type_params: &TypeParams,
    _fct_type_params: &TypeParams,
) -> BytecodeFunction {
    let ast_bytecode_generator = AstBytecodeGenerator::new();
    ast_bytecode_generator.generate(fct.ast)
}

pub struct AstBytecodeGenerator {
    ctxs: Vec<Context>,
    loops: Vec<LoopLabels>,
    labels: Vec<Option<BytecodeIdx>>,
    regs: usize,
    gen: BytecodeGenerator,
}

impl<'ast> visit::Visitor<'ast> for AstBytecodeGenerator {
    fn visit_fct(&mut self, f: &'ast Function) {
        for p in &f.params {
            self.visit_param(p);
        }

        if let Some(ref ty) = f.return_type {
            self.visit_type(ty);
        }

        if let Some(ref block) = f.block {
            self.visit_stmt(block);
        }

        if f.return_type.is_none() {
            self.gen.emit_ret_void();
        }
    }
}

impl AstBytecodeGenerator {
    pub fn new() -> AstBytecodeGenerator {
        AstBytecodeGenerator {
            ctxs: Vec::new(),
            labels: Vec::new(),
            loops: Vec::new(),
            regs: 0,
            gen: BytecodeGenerator::new(),
        }
    }

    pub fn generate(mut self, ast: &Function) -> BytecodeFunction {
        self.visit_fct(ast);
        self.gen.generate()
    }

    pub fn get_reg(&self, var: Name) -> Option<&Register> {
        for ctx in self.ctxs.iter() {
            let opt = ctx.get_reg(var);
            if opt.is_some() {
                return opt;
            }
        }
        None
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
        let reg = self.regs;
        let varid = stmt.name;
        self.regs += 1;
        self.ctxs
            .last_mut()
            .unwrap()
            .new_var(varid as Name, Register(reg));

        if let Some(ref expr) = stmt.expr {
            self.visit_expr(expr);
        } else {
            self.gen.emit_lda_zero();
        };
        self.gen.emit_star(Register(reg));
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
        let regs = self.regs;
        self.ctxs.push(Context::new());
        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }
        self.ctxs.pop();
        self.regs = regs;
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
        let rhs_reg = self.regs;
        self.regs += 1;
        self.gen.emit_star(Register(rhs_reg));
        self.visit_expr(&expr.lhs);
        match expr.op {
            BinOp::Add => self.gen.emit_add(Register(rhs_reg)),
            BinOp::Sub => self.gen.emit_sub(Register(rhs_reg)),
            BinOp::Mul => self.gen.emit_mul(Register(rhs_reg)),
            BinOp::Div => self.gen.emit_div(Register(rhs_reg)),
            BinOp::Mod => self.gen.emit_mod(Register(rhs_reg)),
            BinOp::BitOr => self.gen.emit_bitwise_or(Register(rhs_reg)),
            BinOp::BitAnd => self.gen.emit_bitwise_and(Register(rhs_reg)),
            BinOp::BitXor => self.gen.emit_bitwise_xor(Register(rhs_reg)),
            BinOp::ShiftL => self.gen.emit_shift_left(Register(rhs_reg)),
            BinOp::LogicalShiftR => self.gen.emit_shift_right(Register(rhs_reg)),
            BinOp::ArithShiftR => self.gen.emit_arith_shift_right(Register(rhs_reg)),
            // BinOp::Or => { },
            // BinOp::And => { },
            BinOp::Cmp(op) => {
                match op {
                    CmpOp::Eq => self.gen.emit_test_eq(Register(rhs_reg)),
                    CmpOp::Ne => self.gen.emit_test_ne(Register(rhs_reg)),
                    CmpOp::Lt => self.gen.emit_test_lt(Register(rhs_reg)),
                    CmpOp::Le => self
                        .gen.emit_test_le(Register(rhs_reg)),
                    CmpOp::Gt => self
                        .gen.emit_test_gt(Register(rhs_reg)),
                    CmpOp::Ge => self
                        .gen.emit_test_ge(Register(rhs_reg)),
                    _ => unimplemented!(),
                }
            }
            _ => unimplemented!(),
        }
        self.regs -= 1;
    }

    fn visit_expr_assign(&mut self, expr: &ExprAssignType) {
        self.visit_expr(&expr.rhs);
        match *expr.lhs {
            ExprIdent(ref assign) => {
                let Register(reg) = *self.get_reg(assign.name).unwrap();
                self.gen.emit_star(Register(reg));
            }
            _ => unimplemented!(),
        }
    }

    fn visit_expr_ident(&mut self, ident: &ExprIdentType) {
        let Register(reg) = *self.get_reg(ident.name).unwrap();
        self.gen.emit_ldar(Register(reg));
    }
}

#[cfg(test)]
mod tests {
    use dora_parser::interner::Interner;
    use dora_parser::lexer::reader::Reader;
    use dora_parser::parser::{NodeIdGenerator, Parser};

    use bytecode::opcode::Bytecode::*;
    use bytecode::*;
    use test;

    fn parse(code: &'static str) -> (Ast, Interner) {
        let id_generator = NodeIdGenerator::new();
        let mut interner = Interner::new();
        let mut ast = Ast::new();

        let reader = Reader::from_string(code);
        Parser::new(reader, &id_generator, &mut ast, &mut interner)
            .parse()
            .unwrap();

        (ast, interner)
    }

    fn code(code: &'static str) -> BytecodeFunction {
        test::parse(code, |vm| {
            let fct_id = vm.fct_by_name("f").expect("no function `f`.");
            let tp = TypeParams::empty();
            generate(vm, fct_id, &tp, &tp)
        })
    }

    #[test]
    fn gen_add() {
        let fct = code("fun f() -> Int { return 1 + 2; }");
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            Add(Register(0)),
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
        let expected = vec![
            LdaTrue,
            JumpIfFalse(BytecodeIdx(3)),
            LdaInt(1),
            ReturnVoid
        ];
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
        let expected = vec![
            LdaTrue,
            LogicalNot,
            Return
        ];
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
        let expected = vec![
            LdaInt(1),
            Star(Register(0)),
            Ldar(Register(0)),
            Return
        ];
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
        let expected = vec![
            LdaInt(1),
            Return
        ];
        assert_eq!(expected, fct.code());
    }

    #[test]
    fn gen_expr_returnvoid() {
        let fct = code("fun f() { }");
        let expected = vec![
            ReturnVoid
        ];
        assert_eq!(expected, fct.code());
    }
}
