use codegen::buffer::*;
use codegen::emit;
use codegen::x64::emit::*;
use codegen::x64::reg::*;
use codegen::x64::reg::Reg::*;

use dseg::DSeg;

use ctxt::*;
use ast::*;
use ast::Expr::*;

pub struct ExprGen<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    fct: &'ast Function,
    buf: &'a mut Buffer,
    dseg: &'a mut DSeg,
    tempsize: u32,
}

impl<'a, 'ast> ExprGen<'a, 'ast> where 'ast: 'a {
    pub fn new(
        ctxt: &'a Context<'a, 'ast>,
        fct: &'ast Function,
        buf: &'a mut Buffer,
        dseg: &'a mut DSeg
    ) -> ExprGen<'a, 'ast> {
        ExprGen {
            ctxt: ctxt,
            fct: fct,
            buf: buf,
            dseg: dseg,
            tempsize: 0,
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
            ExprBin(ref expr) => self.emit_bin(expr, dest),
            ExprCall(ref expr) => self.emit_call(expr, dest),
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

    fn emit_bin(&mut self, e: &'ast ExprBinType, dest: Reg) {
        // assert!(e.rhs.is_leaf());

        match e.op {
            BinOp::Add => self.emit_bin_add(e, dest),
            BinOp::Sub => self.emit_bin_sub(e, dest),
            BinOp::Mul => self.emit_bin_mul(e, dest),
            BinOp::Div => self.emit_bin_div(e, dest),
            BinOp::Mod => self.emit_bin_mod(e, dest),
            BinOp::Cmp(op) => self.emit_bin_cmp(e, dest, op),
            BinOp::BitOr => self.emit_bin_bit_or(e, dest),
            BinOp::BitAnd => self.emit_bin_bit_and(e, dest),
            BinOp::BitXor => self.emit_bin_bit_xor(e, dest),
            BinOp::Or => self.emit_bin_or(e, dest),
            BinOp::And => self.emit_bin_and(e, dest),
        }
    }

    fn emit_bin_or(&mut self, e: &'ast ExprBinType, dest: Reg) {
        let lbl_true = self.buf.create_label();
        let lbl_false = self.buf.create_label();
        let lbl_end = self.buf.create_label();

        self.emit_expr(&e.lhs, REG_RESULT);
        emit_cmpb_imm_reg(self.buf, 0, REG_RESULT);
        emit_jnz(self.buf, lbl_true);

        self.emit_expr(&e.rhs, REG_RESULT);
        emit_cmpb_imm_reg(self.buf, 0, REG_RESULT);
        emit_jz(self.buf, lbl_false);

        self.buf.define_label(lbl_true);
        emit_movl_imm_reg(self.buf, 1, dest);
        emit_jmp(self.buf, lbl_end);

        self.buf.define_label(lbl_false);
        emit_movl_imm_reg(self.buf, 0, dest);

        self.buf.define_label(lbl_end);
    }

    fn emit_bin_and(&mut self, e: &'ast ExprBinType, dest: Reg) {
        let lbl_true = self.buf.create_label();
        let lbl_false = self.buf.create_label();
        let lbl_end = self.buf.create_label();

        self.emit_expr(&e.lhs, REG_RESULT);
        emit_cmpb_imm_reg(self.buf, 0, REG_RESULT);
        emit_jz(self.buf, lbl_false);

        self.emit_expr(&e.rhs, REG_RESULT);
        emit_cmpb_imm_reg(self.buf, 0, REG_RESULT);
        emit_jz(self.buf, lbl_false);

        self.buf.define_label(lbl_true);
        emit_movl_imm_reg(self.buf, 1, dest);
        emit_jmp(self.buf, lbl_end);

        self.buf.define_label(lbl_false);
        emit_movl_imm_reg(self.buf, 0, dest);

        self.buf.define_label(lbl_end);
    }

    fn emit_bin_cmp(&mut self, e: &'ast ExprBinType, dest: Reg, op: CmpOp) {
        self.emit_expr(&e.lhs, REG_RESULT);
        self.emit_expr(&e.rhs, REG_TMP1);

        emit_cmpl_reg_reg(self.buf, REG_TMP1, REG_RESULT);
        emit_setb_reg(self.buf, op, REG_RESULT);
        emit_movzbl_reg_reg(self.buf, REG_RESULT, REG_RESULT);

        if dest != REG_RESULT {
            emit_movb_reg_reg(self.buf, REG_RESULT, dest);
        }
    }

    fn emit_bin_div(&mut self, e: &'ast ExprBinType, dest: Reg) {
        self.emit_expr(&e.lhs, RAX);
        self.emit_expr(&e.rhs, REG_TMP1);

        emit_cltd(self.buf);
        emit_idivl_reg_reg(self.buf, REG_TMP1);

        if dest != REG_RESULT {
            emit_movl_reg_reg(self.buf, REG_RESULT, dest);
        }
    }

    fn emit_bin_mod(&mut self, e: &'ast ExprBinType, dest: Reg) {
        self.emit_expr(&e.lhs, RAX);
        self.emit_expr(&e.rhs, REG_TMP1);

        emit_cltd(self.buf);
        emit_idivl_reg_reg(self.buf, REG_TMP1);

        if dest != RDX {
            emit_movl_reg_reg(self.buf, RDX, dest);
        }
    }

    fn emit_bin_mul(&mut self, e: &'ast ExprBinType, dest: Reg) {
        self.emit_expr(&e.lhs, REG_RESULT);
        self.emit_expr(&e.rhs, REG_TMP1);

        emit_imull_reg_reg(self.buf, REG_TMP1, REG_RESULT);

        if dest != REG_RESULT {
            emit_movl_reg_reg(self.buf, REG_RESULT, dest);
        }
    }

    fn emit_bin_add(&mut self, e: &'ast ExprBinType, dest: Reg) {
        self.emit_binop(e, dest, |eg, src, dest| {
            emit_addl_reg_reg(eg.buf, src, dest);
        });
    }

    fn emit_bin_sub(&mut self, e: &'ast ExprBinType, dest: Reg) {
        self.emit_binop(e, dest, |eg, src, dest| {
            emit_subl_reg_reg(eg.buf, src, dest);
        });
    }

    fn emit_binop<F>(&mut self, e: &'ast ExprBinType, dest: Reg, emit_action: F)
            where F: FnOnce(&mut ExprGen, Reg, Reg) {
        self.emit_expr(&e.lhs, REG_RESULT);
        let mut tempoffset : i32 = 0;

        if !e.rhs.is_leaf() {
            self.tempsize += 4;
            tempoffset = -(self.tempsize as i32);

            emit_movl_reg_memq(self.buf, REG_RESULT, RBP, tempoffset);
        }

        self.emit_expr(&e.rhs, REG_TMP1);

        if !e.rhs.is_leaf() {
            emit_movl_memq_reg(self.buf, RBP, tempoffset, REG_RESULT);
        }

        emit_action(self, REG_TMP1, REG_RESULT);

        if dest != REG_RESULT {
            emit_movl_reg_reg(self.buf, REG_RESULT, dest);
        }
    }

    fn emit_bin_bit_or(&mut self, e: &'ast ExprBinType, dest: Reg) {
        self.emit_expr(&e.lhs, REG_RESULT);
        self.emit_expr(&e.rhs, REG_TMP1);

        emit_orl_reg_reg(self.buf, REG_TMP1, REG_RESULT);

        if dest != REG_RESULT {
            emit_movl_reg_reg(self.buf, REG_RESULT, dest);
        }
    }

    fn emit_bin_bit_and(&mut self, e: &'ast ExprBinType, dest: Reg) {
        self.emit_expr(&e.lhs, REG_RESULT);
        self.emit_expr(&e.rhs, REG_TMP1);

        emit_andl_reg_reg(self.buf, REG_TMP1, REG_RESULT);

        if dest != REG_RESULT {
            emit_movl_reg_reg(self.buf, REG_RESULT, dest);
        }
    }

    fn emit_bin_bit_xor(&mut self, e: &'ast ExprBinType, dest: Reg) {
        self.emit_expr(&e.lhs, REG_RESULT);
        self.emit_expr(&e.rhs, REG_TMP1);

        emit_xorl_reg_reg(self.buf, REG_TMP1, REG_RESULT);

        if dest != REG_RESULT {
            emit_movl_reg_reg(self.buf, REG_RESULT, dest);
        }
    }

    fn emit_call(&mut self, e: &'ast ExprCallType, dest: Reg) {
        let calls = self.ctxt.calls.borrow();
        let fid = *calls.get(&e.id).unwrap();

        self.ctxt.fct_info_for_id(fid, |fct_info| {
            assert!(!fct_info.compiled_fct.is_null());

            for (ind, arg) in e.args.iter().enumerate().rev() {
                if REG_PARAMS.len() > ind {
                    let dest = REG_PARAMS[ind];
                    self.emit_expr(arg, dest);
                } else {
                    self.emit_expr(arg, REG_RESULT);
                    emit_pushq_reg(self.buf, REG_RESULT);
                }
            }

            let disp = self.dseg.add_addr(fct_info.compiled_fct);
            let pos = self.buf.pos() as i32;

            // next instruction has 7 bytes
            let disp = disp - pos - 7;

            emit_movq_memq_reg(self.buf, RIP, disp, REG_RESULT); // 7 bytes
            emit_callq_reg(self.buf, REG_RESULT);

            // TODO: move REG_RESULT into dest
        })
    }
}
