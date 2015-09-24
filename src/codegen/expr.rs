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

    pub fn generate(mut self, e: &'ast Expr) -> Reg {
        self.emit_expr(e, REG_RESULT)
    }

    fn emit_expr(&mut self, e: &'ast Expr, dest: Reg) -> Reg {
        match *e {
            ExprLitInt(ref expr) => self.emit_lit_int(expr, dest),
            ExprLitBool(ref expr) => self.emit_lit_bool(expr, dest),
            ExprUn(ref expr) => self.emit_un(expr, dest),
            ExprIdent(ref expr) => self.emit_ident(expr, dest),
            ExprAssign(ref expr) => self.emit_assign(expr, dest),
            _ => unreachable!(),
        }

        dest
    }

    fn emit_lit_int(&mut self, lit: &'ast ExprLitIntType, dest: Reg) {
        emit_movl_imm_reg(self.buf, lit.value as u32, dest);
    }

    fn emit_lit_bool(&mut self, lit: &'ast ExprLitBoolType, dest: Reg) {
        let value : u32 = if lit.value { 1 } else { 0 };
        emit_movl_imm_reg(self.buf, value, dest);
    }

    fn emit_ident(&mut self, e: &'ast ExprIdentType, dest: Reg) {
        let defs = self.ctxt.defs.borrow();
        let varid = *defs.get(&e.id).unwrap();

        emit::var_load(self.buf, self.ctxt, varid, dest);
    }

    fn emit_un(&mut self, e: &'ast ExprUnType, dest: Reg) {
        self.emit_expr(&e.opnd, dest);

        match e.op {
            UnOp::Plus => {},
            UnOp::Neg => emit_negl_reg(self.buf, dest),
            UnOp::BitNot => emit_notl_reg(self.buf, dest),
            UnOp::Not => {
                emit_xorb_imm_reg(self.buf, 1, dest);
                emit_andb_imm_reg(self.buf, 1, dest);
            },
        }
    }

    fn emit_assign(&mut self, e: &'ast ExprAssignType, dest: Reg) {
        self.emit_expr(&e.rhs, dest);

        let defs = self.ctxt.defs.borrow();
        let varid = *defs.get(&e.lhs.id()).unwrap();

        emit::var_store(&mut self.buf, self.ctxt, dest, varid);
    }
}
