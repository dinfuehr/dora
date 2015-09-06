use codegen::buffer::Buffer;
use codegen::x64::reg::*;
use codegen::x64::emit::*;

use driver::ctxt::Context;
use parser::ast::*;
use parser::ast::Expr::*;
use parser::ast::Stmt::*;
use parser::ast::visit::*;

pub struct CodeGen<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    fct: &'ast Function,
    buf: Buffer,
}

impl<'a, 'ast> CodeGen<'a, 'ast> {
    pub fn new(ctxt: &'a Context<'a, 'ast>, fct: &'ast Function) -> CodeGen<'a, 'ast> {
        CodeGen {
            ctxt: ctxt,
            fct: fct,
            buf: Buffer::new()
        }
    }

    pub fn generate(&mut self) {
        self.emit_prolog();
        self.visit_fct(self.fct);
        self.emit_epilog();
    }

    fn emit_prolog(&mut self) {
        emit_pushq_reg(&mut self.buf, Reg::RBP);
        emit_movq_reg_reg(&mut self.buf, Reg::RSP, Reg::RBP);
    }

    fn emit_epilog(&mut self) {
        emit_popq_reg(&mut self.buf, Reg::RBP);
        emit_retq(&mut self.buf);
    }

    fn emit_stmt_return(&mut self, s: &'ast StmtReturnType) {
        if let Some(ref expr) = s.expr {
            self.visit_expr(expr);
        }
    }

    fn emit_expr_lit_int(&mut self, lit: &'ast ExprLitIntType) {
        emit_movl_imm_reg(&mut self.buf, lit.value as u32, Reg::RAX);
    }

    fn emit_expr_lit_bool(&mut self, lit: &'ast ExprLitBoolType) {
        let value : u32 = if lit.value { 1 } else { 0 };
        emit_movl_imm_reg(&mut self.buf, value, Reg::RAX);
    }
}

impl<'a, 'ast> visit::Visitor<'ast> for CodeGen<'a, 'ast> {
    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtReturn(ref stmt) => self.emit_stmt_return(stmt),
            _ => unreachable!()
        }
    }

    fn visit_expr(&mut self, e: &'ast Expr) {
        match *e {
            ExprLitInt(ref expr) => self.emit_expr_lit_int(expr),
            ExprLitBool(ref expr) => self.emit_expr_lit_bool(expr),
            _ => unreachable!()
        }
    }
}
