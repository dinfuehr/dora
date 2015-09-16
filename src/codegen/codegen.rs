use codegen::buffer::*;
use codegen::info;
use codegen::x64::reg::*;
use codegen::x64::reg::Reg::*;
use codegen::x64::emit::*;

use parser::ast::ctxt::*;
use parser::ast::*;
use parser::ast::Expr::*;
use parser::ast::Stmt::*;
use parser::ast::visit::*;

use sym::BuiltinType;

pub struct CodeGen<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    fct: &'ast Function,
    buf: Buffer,

    stacksize: u32,
    lbl_break: Option<Label>,
    lbl_continue: Option<Label>,
}

impl<'a, 'ast> CodeGen<'a, 'ast> where 'ast: 'a {
    pub fn new(ctxt: &'a Context<'a, 'ast>, fct: &'ast Function) -> CodeGen<'a, 'ast> {
        CodeGen {
            ctxt: ctxt,
            fct: fct,
            buf: Buffer::new(),

            stacksize: ctxt.function(fct.id, |fct| fct.stacksize),
            lbl_break: None,
            lbl_continue: None
        }
    }

    pub fn generate(mut self) -> Buffer {
        info::generate(self.ctxt, self.fct);

        self.emit_prolog();
        self.store_params();
        self.visit_fct(self.fct);
        self.emit_epilog();

        self.buf
    }

    fn store_params(&mut self) {
        let params_len = self.fct.params.len();
        if params_len == 0 { return; }

        let defs = self.ctxt.defs.borrow();

        for (reg, p) in PARAM_REGS.iter().zip(&self.fct.params) {
            let varid = *defs.get(&p.id).unwrap();
            self.emit_var_store(*reg, varid);
        }
    }

    fn emit_prolog(&mut self) {
        emit_pushq_reg(&mut self.buf, Reg::RBP);
        emit_movq_reg_reg(&mut self.buf, Reg::RSP, Reg::RBP);

        if self.stacksize > 0 {
            emit_subq_imm_reg(&mut self.buf, self.stacksize as i32, RSP);
        }
    }

    fn emit_epilog(&mut self) {
        if self.stacksize > 0 {
            emit_addq_imm_reg(&mut self.buf, self.stacksize as i32, RSP);
        }

        emit_popq_reg(&mut self.buf, Reg::RBP);
        emit_retq(&mut self.buf);
    }

    fn emit_stmt_return(&mut self, s: &'ast StmtReturnType) {
        if let Some(ref expr) = s.expr {
            self.visit_expr(expr);
        }

        self.emit_epilog();
    }

    fn emit_stmt_while(&mut self, s: &'ast StmtWhileType) {
        let lbl_start = self.buf.create_label();
        let lbl_end = self.buf.create_label();

        self.buf.define_label(lbl_start);

        // execute condition, when condition is false jump to
        // end of while
        self.visit_expr(&s.cond);
        emit_testl_reg_reg(&mut self.buf, RAX, RAX);
        emit_jz(&mut self.buf, lbl_end);

        self.save_label_state(lbl_end, lbl_start, |this| {
            // execute while body, then jump back to condition
            this.visit_stmt(&s.block);
            emit_jmp(&mut this.buf, lbl_start);
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

        emit_jmp(&mut self.buf, lbl_start);
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

        self.visit_expr(&s.cond);
        emit_testl_reg_reg(&mut self.buf, RAX, RAX);
        emit_jz(&mut self.buf, lbl_else);

        self.visit_stmt(&s.then_block);

        if let Some(ref else_block) = s.else_block {
            emit_jmp(&mut self.buf, lbl_end);
            self.buf.define_label(lbl_else);

            self.visit_stmt(else_block);
        }

        self.buf.define_label(lbl_end);
    }

    fn emit_stmt_break(&mut self, s: &'ast StmtBreakType)  {
        emit_jmp(&mut self.buf, self.lbl_break.unwrap());
    }

    fn emit_stmt_continue(&mut self, s: &'ast StmtContinueType) {
        emit_jmp(&mut self.buf, self.lbl_continue.unwrap());
    }

    fn emit_stmt_expr(&mut self, s: &'ast StmtExprType) {
        self.visit_expr(&s.expr);
    }

    fn emit_stmt_block(&mut self, s: &'ast StmtBlockType) {
        for stmt in &s.stmts {
            self.visit_stmt(stmt);
        }
    }

    fn emit_stmt_var(&mut self, s: &'ast StmtVarType) {
        if let Some(ref expr) = s.expr {
            self.visit_expr(expr);

            let defs = self.ctxt.defs.borrow();
            let varid = *defs.get(&s.id).unwrap();

            self.emit_var_store(RAX, varid);
        }
    }

    fn emit_expr_lit_int(&mut self, lit: &'ast ExprLitIntType) {
        emit_movl_imm_reg(&mut self.buf, lit.value as u32, Reg::RAX);
    }

    fn emit_expr_lit_bool(&mut self, lit: &'ast ExprLitBoolType) {
        let value : u32 = if lit.value { 1 } else { 0 };
        emit_movl_imm_reg(&mut self.buf, value, Reg::RAX);
    }

    fn emit_expr_ident(&mut self, e: &'ast ExprIdentType) {
        let defs = self.ctxt.defs.borrow();
        let varid = *defs.get(&e.id).unwrap();

        self.emit_var_load(varid, RAX);
    }

    fn emit_var_store(&mut self, src: Reg, var: VarInfoId) {
        let var_infos = self.ctxt.var_infos.borrow();
        let var = &var_infos[var.0];

        match var.data_type {
            BuiltinType::Bool => emit_movb_reg_memq(&mut self.buf, src, RBP, var.offset),
            BuiltinType::Int => emit_movl_reg_memq(&mut self.buf, src, RBP, var.offset),
            BuiltinType::Str => emit_movq_reg_memq(&mut self.buf, src, RBP, var.offset),
            BuiltinType::Unit => {},
        }
    }

    fn emit_var_load(&mut self, var: VarInfoId, dest: Reg) {
        let var_infos = self.ctxt.var_infos.borrow();
        let var = &var_infos[var.0];

        match var.data_type {
            BuiltinType::Bool => emit_movb_memq_reg(&mut self.buf, RBP, var.offset, dest),
            BuiltinType::Int => emit_movl_memq_reg(&mut self.buf, RBP, var.offset, dest),
            BuiltinType::Str => emit_movq_memq_reg(&mut self.buf, RBP, var.offset, dest),
            BuiltinType::Unit => {},
        }
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

    fn visit_expr(&mut self, e: &'ast Expr) {
        match *e {
            ExprLitInt(ref expr) => self.emit_expr_lit_int(expr),
            ExprLitBool(ref expr) => self.emit_expr_lit_bool(expr),
            ExprIdent(ref expr) => self.emit_expr_ident(expr),
            _ => unreachable!()
        }
    }
}

#[cfg(test)]
mod tests {
    use std::mem;

    use codegen;
    use driver;
    use mem::CodeMemory;
    use semck;
    use test;

    use super::*;

    fn compile(code: &'static str) -> CodeMemory {
        test::parse(code, |ctxt| {
            // generate code for first function
            let fct = ctxt.ast.elements[0].to_function().unwrap();
            let buffer = codegen::generate(ctxt, fct);

            driver::dump_asm(&buffer, &ctxt.interner.str(fct.name));

            CodeMemory::new(&buffer)
        })
    }

    fn run<T>(code: &'static str) -> T {
        let mem = compile(code);
        let compiled_fct : extern "C" fn() -> T = unsafe { mem::transmute(mem.ptr()) };

        compiled_fct()
    }

    fn fct1<T>(code: &'static str) -> (CodeMemory, extern "C" fn(T) -> T) {
        let m = compile(code);
        let ptr = m.ptr();

        (m, unsafe { mem::transmute(ptr) })
    }

    #[test]
    fn test_lit_int() {
        assert_eq!(1i32, run("fn f() -> int { return 1; }"));
        assert_eq!(2i32, run("fn f() -> int { return 2; }"));
        assert_eq!(3i32, run("fn f() -> int { return 3; }"));
    }

    #[test]
    fn test_param() {
        let (mem, f) = fct1("fn f(a: int) -> int { return a; }");

        assert_eq!(0i32, f(0));
        assert_eq!(1, f(1));
        assert_eq!(2, f(2));
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
}
