use codegen::buffer::*;
use codegen::emit;
use codegen::x64::emit::*;
use codegen::x64::reg::*;
use codegen::x64::reg::Reg::*;

use parser::ast::ctxt::*;
use parser::ast::*;
use parser::ast::Expr::*;

use sym::BuiltinType;

pub enum ExprLoc {
    LocConst(i32),
    LocReg(Reg),
    LocStack(i32),
}

pub struct ExprGen<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    fct: &'ast Function,
    buf: &'a mut Buffer,
}

impl<'a, 'ast> ExprGen<'a, 'ast> where 'ast: 'a {
    pub fn new(
        ctxt: &'a Context<'a, 'ast>,
        fct: &'ast Function,
        buf: &'a mut Buffer
    ) -> ExprGen<'a, 'ast> {
        ExprGen {
            ctxt: ctxt,
            fct: fct,
            buf: buf,
        }
    }

    pub fn generate(&mut self, e: &'ast Expr) {
        match *e {
            ExprLitInt(ref expr) => self.emit_lit_int(expr),
            ExprLitBool(ref expr) => self.emit_lit_bool(expr),
            ExprUn(ref expr) => self.emit_un(expr),
            ExprIdent(ref expr) => self.emit_ident(expr),
            _ => unreachable!(),
        }
    }

    fn emit_lit_int(&mut self, lit: &'ast ExprLitIntType) {
        emit_movl_imm_reg(self.buf, lit.value as u32, REG_RESULT);
    }

    fn emit_lit_bool(&mut self, lit: &'ast ExprLitBoolType) {
        let value : u32 = if lit.value { 1 } else { 0 };
        emit_movl_imm_reg(self.buf, value, REG_RESULT);
    }

    fn emit_ident(&mut self, e: &'ast ExprIdentType) {
        let defs = self.ctxt.defs.borrow();
        let varid = *defs.get(&e.id).unwrap();

        emit::var_load(self.buf, self.ctxt, varid, REG_RESULT);
    }

    fn emit_un(&mut self, e: &'ast ExprUnType) {
        self.generate(&e.opnd);

        match e.op {
            UnOp::Plus => {},
            UnOp::Neg => emit_negl_reg(self.buf, REG_RESULT),
            UnOp::BitNot => emit_notl_reg(self.buf, REG_RESULT),
            UnOp::Not => {
                emit_xorb_imm_reg(self.buf, 1, REG_RESULT);
                emit_andb_imm_reg(self.buf, 1, REG_RESULT);
            },
        }
    }
}
