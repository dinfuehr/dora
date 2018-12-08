use std::collections::HashMap;

use dora_parser::ast::*;
use dora_parser::ast::Expr::*;
use dora_parser::ast::Stmt::*;
use dora_parser::interner::Name;
use dora_parser::ast::visit::Visitor;

pub struct Register(usize);
pub struct Label(usize);

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

    pub fn get_reg(&self,var: Name) -> Option<&Register> {
        let reg = self.var_map.get(&var);
        reg
    }
}

pub struct LoopLabels {
    cond: Label,
    end: Label
}

pub enum Bytecode {
    Add(Register),
    BitwiseAnd(Register),
    BitwiseOr(Register),
    BitwiseXor(Register),
    Div(Register),
    Ldar(Register),
    LdaInt(u64),
    LdaZero,
    LogicalNot,
    Star(Register),
    JumpIfFalse(Label),
    Jump(Label),
    Mod(Register),
    Mul(Register),
    Neg,
    ShiftLeft(Register),
    ShiftRight(Register),
    Sub(Register),
    Return,
    ReturnVoid,
    TestEqual(Register),
    TestGreatherThan(Register),
    TestGreatherThanOrEqual(Register),
    TestLessThan(Register),
    TestLessThanOrEqual(Register),
    TestNotEqual(Register),
}

pub struct BytecodeGen {
    code: Vec<Bytecode>,
    ctxs: Vec<Context>,
    loops: Vec<LoopLabels>,
    labels: HashMap<usize, usize>,
    regs: usize,
}

impl<'ast> visit::Visitor<'ast> for BytecodeGen {
    fn visit_fct(&mut self, f: &'ast Function) {
        if  !f.has_optimize {
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

        if f.return_type.is_none() {
            self.code.push(Bytecode::ReturnVoid);
        }
    }
}

impl BytecodeGen {
    pub fn new() -> BytecodeGen {
        BytecodeGen {
            code: Vec::new(),
            ctxs: Vec::new(),
            loops: Vec::new(),
            labels: HashMap::new(),
            regs: 0,
        }
    }

    pub fn gen(&mut self, ast: &Ast) {
        self.visit_ast(ast);
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
            // StmtLoop(ref stmt) => {},
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
        self.ctxs.last_mut().unwrap().new_var(varid as Name, Register(reg));

        if let Some(ref expr) = stmt.expr {
            self.visit_expr(expr);
        } else {
            self.code.push(Bytecode::LdaZero);
        };
        self.code.push(Bytecode::Star(Register(reg)));
    }

    fn visit_stmt_while(&mut self, stmt: &StmtWhileType) {
        let cond_lbl = self.labels.len();
        let end_lbl = cond_lbl + 1;
        self.loops.push(LoopLabels{ cond: Label(cond_lbl), end: Label(end_lbl)});

        self.labels.insert(cond_lbl, self.code.len());
        self.labels.insert(end_lbl, 0); // Just a place holder

        self.visit_expr(&stmt.cond);
        self.code.push(Bytecode::JumpIfFalse(Label(end_lbl)));
        self.visit_stmt(&stmt.block);
        self.code.push(Bytecode::Jump(Label(cond_lbl)));
        self.labels.insert(end_lbl, self.code.len());
        self.loops.pop();
    }

    fn visit_stmt_if(&mut self, stmt: &StmtIfType) {
        let else_lbl = self.labels.len();
        let end_lbl = else_lbl + 1;

        self.labels.insert(else_lbl, 0); // Just a place holder
        self.labels.insert(end_lbl, 0);  // Just a place holder

        self.visit_expr(&stmt.cond);
        self.code.push(Bytecode::JumpIfFalse(Label(else_lbl)));
        self.visit_stmt(&stmt.then_block);
        self.code.push(Bytecode::Jump(Label(end_lbl)));
        self.labels.insert(else_lbl, self.code.len());
        self.visit_stmt(&stmt.then_block);
        self.labels.insert(end_lbl, self.code.len());
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

    fn visit_stmt_break(&mut self, stmt: &StmtBreakType) {
        let Label(end) = self.loops.pop().unwrap().end;
        self.code.push(Bytecode::Jump(Label(end)));
    }

    fn visit_stmt_continue(&mut self, stmt: &StmtContinueType) {
        let Label(cond) = self.loops.last().unwrap().cond;
        self.code.push(Bytecode::Jump(Label(cond)));
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
        self.code.push(Bytecode::LdaInt(lit.value));
    }

    fn visit_expr_un(&mut self, expr: &ExprUnType) {
        self.visit_expr(&expr.opnd);
        match expr.op {
            UnOp::Plus => { },
            UnOp::Neg => { self.code.push(Bytecode::Neg)},
            UnOp::Not => { self.code.push(Bytecode::LogicalNot) },
        }
    }

    fn visit_expr_bin(&mut self, expr: &ExprBinType) {
        self.visit_expr(&expr.rhs);
        let rhs_reg = self.regs;
        self.regs += 1;
        self.code.push(Bytecode::Star(Register(rhs_reg)));
        self.visit_expr(&expr.lhs);
        match expr.op {
            BinOp::Add => { self.code.push(Bytecode::Add(Register(rhs_reg))) },
            BinOp::Sub => { self.code.push(Bytecode::Sub(Register(rhs_reg))) },
            BinOp::Mul => { self.code.push(Bytecode::Mul(Register(rhs_reg))) },
            BinOp::Div => { self.code.push(Bytecode::Div(Register(rhs_reg))) },
            BinOp::Mod => { self.code.push(Bytecode::Mod(Register(rhs_reg))) },
            BinOp::BitOr => { self.code.push(Bytecode::BitwiseOr(Register(rhs_reg))) },
            BinOp::BitAnd => { self.code.push(Bytecode::BitwiseAnd(Register(rhs_reg))) },
            BinOp::BitXor => { self.code.push(Bytecode::BitwiseXor(Register(rhs_reg))) },
            BinOp::ShiftL => { self.code.push(Bytecode::ShiftLeft(Register(rhs_reg))) },
            BinOp::ShiftR => { self.code.push(Bytecode::ShiftRight(Register(rhs_reg))) },
            // BinOp::Or => { },
            // BinOp::And => { },
            // BinOp::UnShiftR => { },
            BinOp::Cmp(op) => {
                match op {
                    CmpOp::Eq => {
                        self.code.push(Bytecode::TestEqual(Register(rhs_reg))) },
                    CmpOp::Ne => {
                        self.code.push(Bytecode::TestNotEqual(Register(rhs_reg))) },
                    CmpOp::Lt => {
                        self.code.push(Bytecode::TestLessThan(Register(rhs_reg))) },
                    CmpOp::Le => {
                        self.code.push(
                            Bytecode::TestLessThanOrEqual(Register(rhs_reg))) },
                    CmpOp::Ge => {
                        self.code.push(
                            Bytecode::TestGreatherThan(Register(rhs_reg))) },
                    CmpOp::Gt => {
                        self.code.push(
                            Bytecode::TestGreatherThanOrEqual (Register(rhs_reg))) },
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
            },
            _ => unimplemented!(),
        }
    }

    fn visit_expr_ident(&mut self, ident: &ExprIdentType) {
        let Register(reg) = *self.get_reg(ident.name).unwrap();
        self.code.push(Bytecode::Ldar(Register(reg)));
    }
}
