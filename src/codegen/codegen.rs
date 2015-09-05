use codegen::buffer::Buffer;
use codegen::x64::*;

use driver::ctxt::Context;
use parser::ast::*;
use parser::ast::Expr::*;
use parser::ast::Stmt::*;
use parser::ast::visit::*;

pub struct CodeGen<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    fct: &'ast Function,
    buffer: Buffer,
}

impl<'a, 'ast> CodeGen<'a, 'ast> {
    pub fn new(ctxt: &'a Context<'a, 'ast>, fct: &'ast Function) -> CodeGen<'a, 'ast> {
        CodeGen {
            ctxt: ctxt,
            fct: fct,
            buffer: Buffer::new()
        }
    }

    pub fn generate(&mut self) {
        self.visit_fct(self.fct);
    }

    fn emit_stmt_return(&mut self, s: &'ast StmtReturnType) {
        if let Some(ref expr) = s.expr {
            self.visit_expr(expr);
        }

        self.buffer.emit_u8(INST_RET);
    }

    fn emit_expr_lit_int(&mut self, lit: &'ast ExprLitIntType) {
        self.emit_movd_imm_reg(lit.value as u32, Reg32::EAX);
    }

    fn emit_movd_imm_reg(&mut self, value: u32, reg: Reg32) {
        self.emit_op(0xB8 as u8 + reg.and7());
        self.buffer.emit_u32(value);
    }

    fn emit_op(&mut self, opcode: u8) {
        self.buffer.emit_u8(opcode);
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
            _ => unreachable!()
        }
    }
}
