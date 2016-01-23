use ast::*;
use ast::Stmt::*;
use ast::visit::*;

use cpu::{Reg, REG_PARAMS};
use cpu::emit;
use ctxt::*;
use dseg::DSeg;

use jit::buffer::*;
use jit::expr::*;
use jit::fct::JitFct;
use jit::info;
use jit::info::Info;

pub struct CodeGen<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    fct: &'ast Function,
    buf: Buffer,
    dseg: DSeg,

    info: Info,

    lbl_break: Option<Label>,
    lbl_continue: Option<Label>,
}

impl<'a, 'ast> CodeGen<'a, 'ast> where 'ast: 'a {
    pub fn new(ctxt: &'a Context<'a, 'ast>, fct: &'ast Function) -> CodeGen<'a, 'ast> {
        CodeGen {
            ctxt: ctxt,
            fct: fct,
            buf: Buffer::new(),
            dseg: DSeg::new(),

            info: Default::default(),

            lbl_break: None,
            lbl_continue: None
        }
    }

    pub fn generate(mut self) -> JitFct {
        self.info = info::generate(self.ctxt, self.fct);

        if self.ctxt.args.flag_emit_debug {
            emit::debug(&mut self.buf);
        }

        self.emit_prolog();
        self.store_register_params_on_stack();
        self.visit_fct(self.fct);

        let always_returns = self.ctxt.fct(self.fct.id, |fct| fct.always_returns);

        if !always_returns {
            self.emit_epilog();
        }

        let mc = self.buf.finish();
        JitFct::new(&self.dseg, &mc)
    }

    fn store_register_params_on_stack(&mut self) {
        let params_len = self.fct.params.len();
        if params_len == 0 { return; }

        for (reg, p) in REG_PARAMS.iter().zip(&self.fct.params) {
            let varid = self.ctxt.fct(self.fct.id, |fct| *fct.defs.get(&p.id).unwrap());
            var_store(&mut self.buf, self.ctxt, self.fct.id, *reg, varid);
        }
    }

    fn emit_prolog(&mut self) {
        emit::prolog(&mut self.buf, self.info.stacksize());
    }

    fn emit_epilog(&mut self) {
        emit::epilog(&mut self.buf, self.info.stacksize());
    }

    fn emit_stmt_return(&mut self, s: &'ast StmtReturnType) {
        if let Some(ref expr) = s.expr {
            self.emit_expr(expr);
        }

        self.emit_epilog();
    }

    fn emit_stmt_while(&mut self, s: &'ast StmtWhileType) {
        let lbl_start = self.buf.create_label();
        let lbl_end = self.buf.create_label();

        self.buf.define_label(lbl_start);

        // execute condition, when condition is false jump to
        // end of while
        let reg = self.emit_expr(&s.cond);
        emit::jump_if(&mut self.buf, JumpCond::Zero, reg, lbl_end);

        self.save_label_state(lbl_end, lbl_start, |this| {
            // execute while body, then jump back to condition
            this.visit_stmt(&s.block);
            emit::jump(&mut this.buf, lbl_start);
        });

        self.buf.define_label(lbl_end);
    }

    fn emit_stmt_loop(&mut self, s: &'ast StmtLoopType) {
        let lbl_start = self.buf.create_label();
        let lbl_end = self.buf.create_label();
        self.buf.define_label(lbl_start);

        self.save_label_state(lbl_end, lbl_start, |this| {
            this.visit_stmt(&s.block);
        });

        self.buf.define_label(lbl_end);

        emit::jump(&mut self.buf, lbl_start);
    }

    fn save_label_state<F>(&mut self, lbl_break: Label, lbl_continue: Label, f: F)
            where F: FnOnce(&mut CodeGen<'a, 'ast>) {
        let old_lbl_break = self.lbl_break;
        let old_lbl_continue = self.lbl_continue;

        self.lbl_break = Some(lbl_break);
        self.lbl_continue = Some(lbl_continue);

        f(self);

        self.lbl_break = old_lbl_break;
        self.lbl_continue = old_lbl_continue;
    }

    fn emit_stmt_if(&mut self, s: &'ast StmtIfType) {
        let lbl_end = self.buf.create_label();
        let lbl_else = if let Some(_) = s.else_block {
            self.buf.create_label()
        } else {
            lbl_end
        };

        let reg = self.emit_expr(&s.cond);
        emit::jump_if(&mut self.buf, JumpCond::Zero, reg, lbl_else);

        self.visit_stmt(&s.then_block);

        if let Some(ref else_block) = s.else_block {
            emit::jump(&mut self.buf, lbl_end);
            self.buf.define_label(lbl_else);

            self.visit_stmt(else_block);
        }

        self.buf.define_label(lbl_end);
    }

    fn emit_stmt_break(&mut self, _: &'ast StmtBreakType)  {
        emit::jump(&mut self.buf, self.lbl_break.unwrap());
    }

    fn emit_stmt_continue(&mut self, _: &'ast StmtContinueType) {
        emit::jump(&mut self.buf, self.lbl_continue.unwrap());
    }

    fn emit_stmt_expr(&mut self, s: &'ast StmtExprType) {
        self.emit_expr(&s.expr);
    }

    fn emit_stmt_block(&mut self, s: &'ast StmtBlockType) {
        for stmt in &s.stmts {
            self.visit_stmt(stmt);
        }
    }

    fn emit_stmt_var(&mut self, s: &'ast StmtVarType) {
        if let Some(ref expr) = s.expr {
            let reg = self.emit_expr(expr);
            let varid = self.ctxt.fct(self.fct.id, |fct| *fct.defs.get(&s.id).unwrap());

            var_store(&mut self.buf, self.ctxt, self.fct.id, reg, varid);
        }
    }

    fn emit_expr(&mut self, e: &'ast Expr) -> Reg {
        let expr_gen = ExprGen::new(self.ctxt, self.fct, &mut self.buf,
            &mut self.dseg, self.info.localsize);

        expr_gen.generate(e)
    }
}

impl<'a, 'ast> visit::Visitor<'ast> for CodeGen<'a, 'ast> {
    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtExpr(ref stmt) => self.emit_stmt_expr(stmt),
            StmtIf(ref stmt) => self.emit_stmt_if(stmt),
            StmtLoop(ref stmt) => self.emit_stmt_loop(stmt),
            StmtWhile(ref stmt) => self.emit_stmt_while(stmt),
            StmtReturn(ref stmt) => self.emit_stmt_return(stmt),
            StmtBreak(ref stmt) => self.emit_stmt_break(stmt),
            StmtContinue(ref stmt) => self.emit_stmt_continue(stmt),
            StmtBlock(ref stmt) => self.emit_stmt_block(stmt),
            StmtVar(ref stmt) => self.emit_stmt_var(stmt),
        }
    }

    fn visit_expr(&mut self, _: &'ast Expr) {
        unreachable!("should not be invoked");
    }
}

pub enum JumpCond {
    Zero,
    NonZero
}

pub fn var_store(buf: &mut Buffer, ctxt: &Context, fctid: NodeId, src: Reg, var: VarContextId) {
    ctxt.fct(fctid, |fct| {
        let var = &fct.vars[var.0];

        emit::mov_reg_local(buf, var.data_type, src, var.offset);
    })
}

pub fn var_load(buf: &mut Buffer, ctxt: &Context, fctid: NodeId, var: VarContextId, dest: Reg) {
    ctxt.fct(fctid, |fct| {
        let var = &fct.vars[var.0];

        emit::mov_local_reg(buf, var.data_type, var.offset, dest);
    });
}

#[cfg(test)]
mod tests {
    use std::mem;

    use jit;
    use jit::fct::JitFct;
    use driver;
    use driver::cmd::AsmSyntax;
    use test;

    fn compile(code: &'static str) -> JitFct {
        test::parse(code, |ctxt| {
            // generate code for first function
            let fct = ctxt.ast.elements[0].to_function().unwrap();
            let jit_fct = jit::generate(ctxt, fct);

            driver::dump_asm(&jit_fct, &ctxt.interner.str(fct.name), AsmSyntax::Att);

            jit_fct
        })
    }

    fn run<T>(code: &'static str) -> T {
        let mem = compile(code);
        let compiled_fct : extern "C" fn() -> T = unsafe { mem::transmute(mem.fct_ptr()) };

        compiled_fct()
    }

    fn fct1<T>(code: &'static str) -> (JitFct, extern "C" fn(T) -> T) {
        let m = compile(code);
        let fct = m.fct_ptr();

        (m, unsafe { mem::transmute(fct) })
    }

    fn fct2<T>(code: &'static str) -> (JitFct, extern "C" fn(T, T) -> T) {
        let m = compile(code);
        let fct = m.fct_ptr();

        (m, unsafe { mem::transmute(fct) })
    }

    #[test]
    fn test_lit_int() {
        assert_eq!(1i32, run("fn f() -> int { return 1; }"));
        assert_eq!(2i32, run("fn f() -> int { return 2; }"));
        assert_eq!(3i32, run("fn f() -> int { return 3; }"));
    }

    #[test]
    fn test_expr_un_plus() {
        let (mem, f) = fct1("fn f(a: int) -> int { return +a; }");

        assert_eq!(-1i32, f(-1));
        assert_eq!(0, f(0));
        assert_eq!(1, f(1));
    }

    #[test]
    fn test_expr_un_neg() {
        let (mem, f) = fct1("fn f(a: int) -> int { return -a; }");

        assert_eq!(1i32, f(-1));
        assert_eq!(0, f(0));
        assert_eq!(-1, f(1));
    }

    #[test]
    fn test_expr_un_bit_not() {
        let (mem, f) = fct1("fn f(a: int) -> int { return ~a; }");

        assert_eq!(0i32, f(-1));
        assert_eq!(-1, f(0));
        assert_eq!(-2, f(1));
    }

    #[test]
    fn test_expr_un_not() {
        let (mem, f) = fct1("fn f(a: bool) -> bool { return !a; }");

        assert_eq!(true, f(false));
        assert_eq!(false, f(true));
    }

    #[test]
    fn test_param() {
        let (mem, f) = fct1("fn f(a: int) -> int { return a; }");

        assert_eq!(0i32, f(0));
        assert_eq!(1, f(1));
        assert_eq!(2, f(2));
    }

    #[test]
    fn test_param_bool() {
        let (mem, f) = fct1("fn f(a: bool) -> bool { return a; }");

        assert_eq!(true, f(true));
        assert_eq!(false, f(false));
    }

    #[test]
    fn test_lit_bool() {
        assert_eq!(true, run("fn f() -> bool { return true; }"));
        assert_eq!(false, run("fn f() -> bool { return false; }"));
    }

    #[test]
    fn test_if() {
        assert_eq!(1i32, run("fn f() -> int { if true { return 1; } else { return 2; } }"));
        assert_eq!(1i32, run("fn f() -> int { if true { return 1; } return 2; }"));
        assert_eq!(2i32, run("fn f() -> int { if false { return 1; } else { return 2; } }"));
        assert_eq!(2i32, run("fn f() -> int { if false { return 1; } return 2; }"));
    }

    #[test]
    fn test_ident_load_and_store() {
        assert_eq!(4711, run("fn f() -> int { var a = 4711; return a; }"));
        assert_eq!(true, run("fn f() -> bool { var a = true; return a; }"));
        assert_eq!(false, run("fn f() -> bool { var a = false; return a; }"));
    }

    #[test]
    fn test_assign() {
        assert_eq!(4711, run("fn f() -> int { var a: int; a = 4711; return a; }"));
        assert_eq!(true, run("fn f() -> bool { var a: bool; a = true; return a; }"));
        assert_eq!(false, run("fn f() -> bool { var a: bool; a = false; return a; }"));
    }

    #[test]
    fn test_add() {
        assert_eq!(3, run("fn f() -> int { var a = 1; var b = 2; return a + b; }"));
    }

    #[test]
    fn test_add_with_complex_rhs() {
        assert_eq!(10, run("fn f() -> int { return 1+(2+(3+4)); }"));
    }

    #[test]
    fn test_sub() {
        assert_eq!(-1, run("fn f() -> int { var a = 1; var b = 2; return a - b; }"));
    }

    #[test]
    fn test_sub_with_complex_rhs() {
        assert_eq!(-1, run("fn f() -> int { return 3-4; }"));
        assert_eq!(3, run("fn f() -> int { return 2-(3-4); }"));
        assert_eq!(-2, run("fn f() -> int { return 1-(2-(3-4)); }"));
        assert_eq!(42, run("fn f() -> int {
            var a = 7;
            var b = 1+(2+3);

            return a*b;
        }"))
    }

    #[test]
    fn test_bit_or() {
        assert_eq!(3, run("fn f() -> int { var a = 1; var b = 2; return a | b; }"));
    }

    #[test]
    fn test_bit_and() {
        assert_eq!(1, run("fn f() -> int { var a = 1; var b = 3; return a & b; }"));
    }

    #[test]
    fn test_bit_xor() {
        assert_eq!(1, run("fn f() -> int { var a = 3; var b = 2; return a ^ b; }"));
    }

    #[test]
    fn test_mul() {
        assert_eq!(6, run("fn f() -> int { var a = 3; var b = 2; return a * b; }"));
    }

    #[test]
    fn test_div() {
        let (mem, f) = fct1("fn f(a: int) -> int { var b = 3; return a / b; }");
        let b = 3;

        for a in 0..8 {
            assert_eq!(a/b, f(a));
        }
    }

    #[test]
    fn test_mod() {
        let (mem, f) = fct1("fn f(a: int) -> int { var b = 3; return a % b; }");
        let b = 3;

        for a in 0..8 {
            assert_eq!(a%b, f(a));
        }
    }

    #[test]
    fn test_cmp_lt() {
        let (mem, f) = fct1("fn f(a: int) -> int { if a < 3 { return 1; } else { return 0; } }");

        assert_eq!(1, f(-4));
        assert_eq!(1, f(-3));
        assert_eq!(1, f(-1));
        assert_eq!(1, f(1));
        assert_eq!(0, f(3));
        assert_eq!(0, f(4));
    }

    #[test]
    fn test_cmp_le() {
        let (mem, f) = fct1("fn f(a: int) -> int { if a <= 3 { return 1; } else { return 0; } }");

        assert_eq!(1, f(-4));
        assert_eq!(1, f(-3));
        assert_eq!(1, f(-1));
        assert_eq!(1, f(1));
        assert_eq!(1, f(3));
        assert_eq!(0, f(4));
    }

    #[test]
    fn test_cmp_gt() {
        let (mem, f) = fct1("fn f(a: int) -> int { if a > 3 { return 1; } else { return 0; } }");

        assert_eq!(0, f(-4));
        assert_eq!(0, f(-3));
        assert_eq!(0, f(-1));
        assert_eq!(0, f(1));
        assert_eq!(0, f(3));
        assert_eq!(1, f(4));
    }

    #[test]
    fn test_cmp_ge() {
        let (mem, f) = fct1("fn f(a: int) -> int { if a >= 3 { return 1; } else { return 0; } }");

        assert_eq!(0, f(-4));
        assert_eq!(0, f(-3));
        assert_eq!(0, f(-1));
        assert_eq!(0, f(1));
        assert_eq!(1, f(3));
        assert_eq!(1, f(4));
    }

    #[test]
    fn test_cmp_eq() {
        let (mem, f) = fct1("fn f(a: int) -> int { if a == 3 { return 1; } else { return 0; } }");

        assert_eq!(0, f(-4));
        assert_eq!(0, f(-3));
        assert_eq!(0, f(-1));
        assert_eq!(0, f(1));
        assert_eq!(1, f(3));
        assert_eq!(0, f(4));
    }

    #[test]
    fn test_cmp_ne() {
        let (mem, f) = fct1("fn f(a: int) -> int { if a != 3 { return 1; } else { return 0; } }");

        assert_eq!(1, f(-4));
        assert_eq!(1, f(-3));
        assert_eq!(1, f(-1));
        assert_eq!(1, f(1));
        assert_eq!(0, f(3));
        assert_eq!(1, f(4));
    }

    #[test]
    fn test_or() {
        let (mem, f) = fct2("fn f(a: bool, b: bool) -> bool { return a || b; }");

        assert_eq!(true, f(true, true));
        assert_eq!(true, f(true, false));
        assert_eq!(true, f(false, true));
        assert_eq!(false, f(false, false));
    }

    #[test]
    fn test_and() {
        let (mem, f) = fct2("fn f(a: bool, b: bool) -> bool { return a && b; }");

        assert_eq!(true, f(true, true));
        assert_eq!(false, f(true, false));
        assert_eq!(false, f(false, true));
        assert_eq!(false, f(false, false));
    }
}
