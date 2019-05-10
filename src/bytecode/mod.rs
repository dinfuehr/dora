use std::collections::HashMap;

use dora_parser::ast::visit::Visitor;
use dora_parser::ast::Expr::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::*;
use dora_parser::interner::Name;

use bytecode::generate::{Bytecode, BytecodeIdx, Label, Register};

mod generate;

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

pub struct BytecodeGen {
    code: Vec<Bytecode>,
    ctxs: Vec<Context>,
    loops: Vec<LoopLabels>,
    labels: Vec<Option<BytecodeIdx>>,
    regs: usize,
}

impl<'ast> visit::Visitor<'ast> for BytecodeGen {
    fn visit_fct(&mut self, f: &'ast Function) {
        if !f.has_optimize {
            return;
        }

        for p in &f.params {
            self.visit_param(p);
        }

        if let Some(ref ty) = f.return_type {
            self.visit_type(ty);
        }

        if let Some(ref block) = f.block {
            self.visit_stmt(block);
        }

        if self.code.len() == 0 || self.code.last().unwrap() != &Bytecode::Return {
            self.code.push(Bytecode::ReturnVoid);
        }
    }
}

impl BytecodeGen {
    pub fn new() -> BytecodeGen {
        BytecodeGen {
            code: Vec::new(),
            ctxs: Vec::new(),
            labels: Vec::new(),
            loops: Vec::new(),
            regs: 0,
        }
    }

    pub fn gen(&mut self, ast: &Ast) {
        self.visit_ast(ast);
    }

    fn create_label(&mut self) -> Label {
        self.labels.push(None);
        Label(self.labels.len() - 1)
    }

    fn define_label(&mut self) -> Label {
        let dest = BytecodeIdx(self.code.len());
        self.labels.push(Some(dest));
        Label(self.labels.len() - 1)
    }

    fn bind_label(&mut self, lbl: Label) {
        assert!(self.labels[lbl.0].is_none(), "bind label twice");
        let dest = BytecodeIdx(self.code.len());
        self.labels[lbl.0] = Some(dest);
    }

    fn dest_label(&self, lbl: Label) -> BytecodeIdx {
        self.labels[lbl.0].expect("label wasn't bound")
    }

    pub fn dump(&self) {
        let mut btidx = 0;
        for btcode in self.code.iter() {
            match btcode {
                Bytecode::Add(Register(register)) => println!("{}: Add {}", btidx, register),
                Bytecode::BitwiseAnd(Register(register)) => {
                    println!("{}: BitwiseAnd {}", btidx, register)
                }
                Bytecode::BitwiseOr(Register(register)) => {
                    println!("{}: BitwiseOr {}", btidx, register)
                }
                Bytecode::BitwiseXor(Register(register)) => {
                    println!("{}: BitwiseXor {}", btidx, register)
                }
                Bytecode::Div(Register(register)) => println!("{}: Div {}", btidx, register),
                Bytecode::Ldar(Register(register)) => println!("{}: Ldar {}", btidx, register),
                Bytecode::LdaInt(value) => println!("{}: LdaInt {}", btidx, value),
                Bytecode::LdaZero => println!("{}: LdaZero", btidx),
                Bytecode::LogicalNot => println!("{}: LogicalNot", btidx),
                Bytecode::Star(Register(register)) => println!("{}: Star {}", btidx, register),
                Bytecode::JumpIfFalse(label) => {
                    let dest = self.dest_label(*label);
                    println!("{}: JumpIfFalse {}", btidx, dest)
                }
                Bytecode::Jump(label) => {
                    let dest = self.dest_label(*label);
                    println!("{}: Jump {}", btidx, dest)
                }
                Bytecode::JumpIfFalseBytecodeIdx(dest) => {
                    println!("{}: JumpIfFalse bc#{}", btidx, dest)
                }
                Bytecode::JumpBytecodeIdx(dest) => {
                    println!("{}: Jump bc#{}", btidx, dest)
                }
                Bytecode::Mod(Register(register)) => println!("{}: Mod {}", btidx, register),
                Bytecode::Mul(Register(register)) => println!("{}: Mul {}", btidx, register),
                Bytecode::Neg => println!("{}: Neg", btidx),
                Bytecode::ShiftLeft(Register(register)) => {
                    println!("{}: ShiftLeft {}", btidx, register)
                }
                Bytecode::ShiftRight(Register(register)) => {
                    println!("{}: ShiftRight {}", btidx, register)
                }
                Bytecode::ArithShiftRight(Register(register)) => {
                    println!("{}: ArithShiftRight {}", btidx, register)
                }
                Bytecode::Sub(Register(register)) => println!("{}: Sub {}", btidx, register),
                Bytecode::Return => println!("{}: Return", btidx),
                Bytecode::ReturnVoid => println!("{}: ReturnVoid", btidx),
                Bytecode::TestEqual(Register(register)) => {
                    println!("{}: TestEqual {}", btidx, register)
                }
                Bytecode::TestGreatherThan(Register(register)) => {
                    println!("{}: TestGreaterThan {}", btidx, register)
                }
                Bytecode::TestGreatherThanOrEqual(Register(register)) => {
                    println!("{}: TestGreatherThanOrEqual {}", btidx, register)
                }
                Bytecode::TestLessThan(Register(register)) => {
                    println!("{}: TestLessThan {}", btidx, register)
                }
                Bytecode::TestLessThanOrEqual(Register(register)) => {
                    println!("{}: TestLessThanOrEqual {}", btidx, register)
                }
                Bytecode::TestNotEqual(Register(register)) => {
                    println!("{}: TestNotEqual {}", btidx, register)
                }
            }
            btidx = btidx + 1;
        }
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
            self.code.push(Bytecode::LdaZero);
        };
        self.code.push(Bytecode::Star(Register(reg)));
    }

    fn visit_stmt_while(&mut self, stmt: &StmtWhileType) {
        let cond_lbl = self.define_label();
        let end_lbl = self.create_label();
        self.visit_expr(&stmt.cond);
        self.code.push(Bytecode::JumpIfFalse(end_lbl));
        self.loops.push(LoopLabels::new(cond_lbl, end_lbl));
        self.visit_stmt(&stmt.block);
        self.loops.pop();
        self.code.push(Bytecode::Jump(cond_lbl));
        self.bind_label(end_lbl);
    }

    fn visit_stmt_loop(&mut self, stmt: &StmtLoopType) {
        let start_lbl = self.define_label();
        let end_lbl = self.create_label();
        self.loops.push(LoopLabels::new(start_lbl, end_lbl));
        self.visit_stmt(&stmt.block);
        self.loops.pop();
        self.code.push(Bytecode::Jump(start_lbl));
        self.bind_label(end_lbl);
    }

    fn visit_stmt_if(&mut self, stmt: &StmtIfType) {
        if let Some(ref else_block) = stmt.else_block {
            let else_lbl = self.create_label();
            let end_lbl = self.create_label();

            self.visit_expr(&stmt.cond);
            self.code.push(Bytecode::JumpIfFalse(else_lbl));

            self.visit_stmt(&stmt.then_block);
            self.code.push(Bytecode::Jump(end_lbl));

            self.bind_label(else_lbl);
            self.visit_stmt(&else_block);
            self.bind_label(end_lbl);
        } else {
            let end_lbl = self.create_label();
            self.visit_expr(&stmt.cond);
            self.code.push(Bytecode::JumpIfFalse(end_lbl));
            self.visit_stmt(&stmt.then_block);
            self.bind_label(end_lbl);
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
        }
        self.code.push(Bytecode::Return);
    }

    fn visit_stmt_break(&mut self, _stmt: &StmtBreakType) {
        let end = self.loops.pop().unwrap().end;
        self.code.push(Bytecode::Jump(end));
    }

    fn visit_stmt_continue(&mut self, _stmt: &StmtContinueType) {
        let cond = self.loops.last().unwrap().cond;
        self.code.push(Bytecode::Jump(cond));
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
            // ExprLitBool(ref lit) => {},
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
            self.code.push(Bytecode::LdaZero);
        } else {
            self.code.push(Bytecode::LdaInt(lit.value));
        }
    }

    fn visit_expr_un(&mut self, expr: &ExprUnType) {
        self.visit_expr(&expr.opnd);
        match expr.op {
            UnOp::Plus => {}
            UnOp::Neg => self.code.push(Bytecode::Neg),
            UnOp::Not => self.code.push(Bytecode::LogicalNot),
        }
    }

    fn visit_expr_bin(&mut self, expr: &ExprBinType) {
        self.visit_expr(&expr.rhs);
        let rhs_reg = self.regs;
        self.regs += 1;
        self.code.push(Bytecode::Star(Register(rhs_reg)));
        self.visit_expr(&expr.lhs);
        match expr.op {
            BinOp::Add => self.code.push(Bytecode::Add(Register(rhs_reg))),
            BinOp::Sub => self.code.push(Bytecode::Sub(Register(rhs_reg))),
            BinOp::Mul => self.code.push(Bytecode::Mul(Register(rhs_reg))),
            BinOp::Div => self.code.push(Bytecode::Div(Register(rhs_reg))),
            BinOp::Mod => self.code.push(Bytecode::Mod(Register(rhs_reg))),
            BinOp::BitOr => self.code.push(Bytecode::BitwiseOr(Register(rhs_reg))),
            BinOp::BitAnd => self.code.push(Bytecode::BitwiseAnd(Register(rhs_reg))),
            BinOp::BitXor => self.code.push(Bytecode::BitwiseXor(Register(rhs_reg))),
            BinOp::ShiftL => self.code.push(Bytecode::ShiftLeft(Register(rhs_reg))),
            BinOp::ShiftR => self.code.push(Bytecode::ShiftRight(Register(rhs_reg))),
            // BinOp::Or => { },
            // BinOp::And => { },
            // BinOp::UnShiftR => { },
            BinOp::Cmp(op) => {
                match op {
                    CmpOp::Eq => self.code.push(Bytecode::TestEqual(Register(rhs_reg))),
                    CmpOp::Ne => self.code.push(Bytecode::TestNotEqual(Register(rhs_reg))),
                    CmpOp::Lt => self.code.push(Bytecode::TestLessThan(Register(rhs_reg))),
                    CmpOp::Le => self
                        .code
                        .push(Bytecode::TestLessThanOrEqual(Register(rhs_reg))),
                    CmpOp::Gt => self
                        .code
                        .push(Bytecode::TestGreatherThan(Register(rhs_reg))),
                    CmpOp::Ge => self
                        .code
                        .push(Bytecode::TestGreatherThanOrEqual(Register(rhs_reg))),
                    // CmpOp::Is => { },
                    // CmpOp::IsNot => { },
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
                self.code.push(Bytecode::Star(Register(reg)));
            }
            _ => unimplemented!(),
        }
    }

    fn visit_expr_ident(&mut self, ident: &ExprIdentType) {
        let Register(reg) = *self.get_reg(ident.name).unwrap();
        self.code.push(Bytecode::Ldar(Register(reg)));
    }
}

#[cfg(test)]
mod tests {
    use dora_parser::interner::Interner;
    use dora_parser::lexer::reader::Reader;
    use dora_parser::parser::{NodeIdGenerator, Parser};

    use bytecode::Bytecode::*;
    use bytecode::*;

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

    #[test]
    fn gen_nooptimize() {
        let (ast, _) = parse("fun f() {1 + 2;}");
        let mut bytecodegen = BytecodeGen::new();
        bytecodegen.gen(&ast);
        assert!(bytecodegen.code.is_empty());
    }

    #[test]
    fn gen_add() {
        let (ast, _) = parse(
            "
            optimize fun f() {1 + 2;}",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            Add(Register(0)),
            ReturnVoid,
        ];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_sub() {
        let (ast, _) = parse(
            "
            optimize fun f() {1 - 2;}",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            Sub(Register(0)),
            ReturnVoid,
        ];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_div() {
        let (ast, _) = parse(
            "
            optimize fun f() {1 / 2;}",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            Div(Register(0)),
            ReturnVoid,
        ];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_mul() {
        let (ast, _) = parse(
            "
            optimize fun f() {1 * 2;}",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            Mul(Register(0)),
            ReturnVoid,
        ];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_stmt_var_noinit() {
        let (ast, _) = parse(
            "
            optimize fun f() { let x; }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let mut expected = Vec::new();
        expected.push(Bytecode::LdaZero);
        expected.push(Bytecode::Star(Register(0)));
        expected.push(Bytecode::ReturnVoid);
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_stmt_var_init() {
        let (ast, _) = parse(
            "
            optimize fun f() { let x = 1; }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![LdaInt(1), Star(Register(0)), ReturnVoid];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_stmt_while() {
        let (ast, _) = parse(
            "
            optimize fun f() { while 1 { 0; } }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let code = vec![
            LdaInt(1),
            JumpIfFalse(Label(1)),
            LdaZero,
            Jump(Label(0)),
            ReturnVoid,
        ];
        let labels = vec![Some(BytecodeIdx(0)), Some(BytecodeIdx(4))];
        bytecodegen.gen(&ast);
        assert_eq!(code, bytecodegen.code);
        assert_eq!(labels, bytecodegen.labels);
    }

    #[test]
    fn gen_stmt_if() {
        let (ast, _) = parse(
            "
            optimize fun f() { if 0 { 1; } }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![LdaZero, JumpIfFalse(Label(0)), LdaInt(1), ReturnVoid];
        let labels = vec![Some(BytecodeIdx(3))];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
        assert_eq!(labels, bytecodegen.labels);
    }

    #[test]
    fn gen_stmt_if_else() {
        let (ast, _) = parse(
            "
            optimize fun f() { if 0 { 1; } else { 2; } }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![
            LdaZero,
            JumpIfFalse(Label(0)),
            LdaInt(1),
            Jump(Label(1)),
            LdaInt(2),
            ReturnVoid,
        ];
        let labels = vec![Some(BytecodeIdx(4)), Some(BytecodeIdx(5))];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
        assert_eq!(labels, bytecodegen.labels);
    }

    #[test]
    fn gen_stmt_break() {
        let (ast, _) = parse(
            "
            optimize fun f() { while 1 { break; } }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![
            LdaInt(1),
            JumpIfFalse(Label(1)),
            Jump(Label(1)),
            Jump(Label(0)),
            ReturnVoid,
        ];
        let labels = vec![Some(BytecodeIdx(0)), Some(BytecodeIdx(4))];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
        assert_eq!(labels, bytecodegen.labels);
    }

    #[test]
    fn gen_stmt_continue() {
        let (ast, _) = parse(
            "
            optimize fun f() { while 1 { continue; } }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![
            LdaInt(1),
            JumpIfFalse(Label(1)),
            Jump(Label(0)),
            Jump(Label(0)),
            ReturnVoid,
        ];
        let labels = vec![Some(BytecodeIdx(0)), Some(BytecodeIdx(4))];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
        assert_eq!(labels, bytecodegen.labels);
    }

    #[test]
    fn gen_expr_lit_int() {
        let (ast, _) = parse(
            "
            optimize fun f() { 1; }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![LdaInt(1), ReturnVoid];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_expr_lit_zero() {
        let (ast, _) = parse(
            "
            optimize fun f() { 0; }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![LdaZero, ReturnVoid];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_expr_puls() {
        let (ast, _) = parse(
            "
            optimize fun f() { +1; }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![LdaInt(1), ReturnVoid];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_expr_neg() {
        let (ast, _) = parse(
            "
            optimize fun f() { -1; }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![LdaInt(1), Neg, ReturnVoid];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_expr_not() {
        let (ast, _) = parse(
            "
            optimize fun f() { !1; }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![LdaInt(1), LogicalNot, ReturnVoid];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_expr_mod() {
        let (ast, _) = parse(
            "
            optimize fun f() { 1 % 2; }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            Mod(Register(0)),
            ReturnVoid,
        ];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_expr_bit_or() {
        let (ast, _) = parse(
            "
            optimize fun f() { 1 | 2; }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            BitwiseOr(Register(0)),
            ReturnVoid,
        ];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_expr_bit_and() {
        let (ast, _) = parse(
            "
            optimize fun f() { 1 & 2; }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            BitwiseAnd(Register(0)),
            ReturnVoid,
        ];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_expr_bit_xor() {
        let (ast, _) = parse(
            "
            optimize fun f() { 1 ^ 2; }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            BitwiseXor(Register(0)),
            ReturnVoid,
        ];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_expr_bit_shiftl() {
        let (ast, _) = parse(
            "
            optimize fun f() { 1 << 2; }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            ShiftLeft(Register(0)),
            ReturnVoid,
        ];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_expr_bit_shiftr() {
        let (ast, _) = parse(
            "
            optimize fun f() { 1 >> 2; }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            ShiftRight(Register(0)),
            ReturnVoid,
        ];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_expr_test_equal() {
        let (ast, _) = parse(
            "
            optimize fun f() { 1 == 2; }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            TestEqual(Register(0)),
            ReturnVoid,
        ];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_expr_test_notequal() {
        let (ast, _) = parse(
            "
            optimize fun f() { 1 != 2; }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            TestNotEqual(Register(0)),
            ReturnVoid,
        ];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_expr_test_lessthan() {
        let (ast, _) = parse(
            "
            optimize fun f() { 1 < 2; }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            TestLessThan(Register(0)),
            ReturnVoid,
        ];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_expr_test_lessthanequal() {
        let (ast, _) = parse(
            "
            optimize fun f() { 1 <= 2; }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            TestLessThanOrEqual(Register(0)),
            ReturnVoid,
        ];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_expr_test_greaterthan() {
        let (ast, _) = parse(
            "
            optimize fun f() { 1 > 2; }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            TestGreatherThan(Register(0)),
            ReturnVoid,
        ];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_expr_test_greaterthanequall() {
        let (ast, _) = parse(
            "
            optimize fun f() { 1 >= 2; }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![
            LdaInt(2),
            Star(Register(0)),
            LdaInt(1),
            TestGreatherThanOrEqual(Register(0)),
            ReturnVoid,
        ];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_expr_ident() {
        let (ast, _) = parse(
            "
            optimize fun f() { let x = 1; x; }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![LdaInt(1), Star(Register(0)), Ldar(Register(0)), ReturnVoid];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_expr_assign() {
        let (ast, _) = parse(
            "
            optimize fun f() { var x = 1; x = 2; }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![
            LdaInt(1),
            Star(Register(0)),
            LdaInt(2),
            Star(Register(0)),
            ReturnVoid,
        ];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_expr_return() {
        let (ast, _) = parse(
            "
            optimize fun f() { return 1; }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![LdaInt(1), Return];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }

    #[test]
    fn gen_expr_returnvoid() {
        let (ast, _) = parse(
            "
            optimize fun f() { }",
        );
        let mut bytecodegen = BytecodeGen::new();
        let expected = vec![ReturnVoid];
        bytecodegen.gen(&ast);
        assert_eq!(expected, bytecodegen.code);
    }
}
