use ast::*;
use ast::Expr::*;
use cpu::{Reg, REG_RESULT, REG_TMP1, REG_PARAMS};
use cpu::emit;
use cpu::trap;
use ctxt::*;
use dseg::DSeg;
use jit::buffer::*;
use jit::codegen::{self, JumpCond};
use sym::BuiltinType;

pub struct ExprGen<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    fct: &'ast Function,
    buf: &'a mut Buffer,
    dseg: &'a mut DSeg,
    tempsize: i32,
    localsize: i32,
}

impl<'a, 'ast> ExprGen<'a, 'ast> where 'ast: 'a {
    pub fn new(
        ctxt: &'a Context<'a, 'ast>,
        fct: &'ast Function,
        buf: &'a mut Buffer,
        dseg: &'a mut DSeg,
        localsize: i32,
    ) -> ExprGen<'a, 'ast> {
        ExprGen {
            ctxt: ctxt,
            fct: fct,
            buf: buf,
            dseg: dseg,
            tempsize: 0,
            localsize: localsize,
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
        emit::movl_imm_reg(self.buf, lit.value as u32, dest);
    }

    fn emit_lit_bool(&mut self, lit: &'ast ExprLitBoolType, dest: Reg) {
        let value : u32 = if lit.value { 1 } else { 0 };
        emit::movl_imm_reg(self.buf, value, dest);
    }

    fn emit_ident(&mut self, e: &'ast ExprIdentType, dest: Reg) {
        let defs = self.ctxt.defs.borrow();
        let varid = *defs.get(&e.id).unwrap();

        codegen::var_load(self.buf, self.ctxt, varid, dest);
    }

    fn emit_un(&mut self, e: &'ast ExprUnType, dest: Reg) {
        self.emit_expr(&e.opnd, dest);

        match e.op {
            UnOp::Plus => {},
            UnOp::Neg => emit::negl_reg(self.buf, dest),
            UnOp::BitNot => emit::notl_reg(self.buf, dest),
            UnOp::Not => emit::bool_not_reg(self.buf, dest)
        }
    }

    fn emit_assign(&mut self, e: &'ast ExprAssignType, dest: Reg) {
        self.emit_expr(&e.rhs, dest);

        let defs = self.ctxt.defs.borrow();
        let varid = *defs.get(&e.lhs.id()).unwrap();

        codegen::var_store(&mut self.buf, self.ctxt, dest, varid);
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
        emit::jump_if(self.buf, JumpCond::NonZero, REG_RESULT, lbl_true);

        self.emit_expr(&e.rhs, REG_RESULT);
        emit::jump_if(self.buf, JumpCond::Zero, REG_RESULT, lbl_false);

        self.buf.define_label(lbl_true);
        emit::movl_imm_reg(self.buf, 1, dest);
        emit::jump(self.buf, lbl_end);

        self.buf.define_label(lbl_false);
        emit::movl_imm_reg(self.buf, 0, dest);

        self.buf.define_label(lbl_end);
    }

    fn emit_bin_and(&mut self, e: &'ast ExprBinType, dest: Reg) {
        let lbl_true = self.buf.create_label();
        let lbl_false = self.buf.create_label();
        let lbl_end = self.buf.create_label();

        self.emit_expr(&e.lhs, REG_RESULT);
        emit::jump_if(self.buf, JumpCond::Zero, REG_RESULT, lbl_false);

        self.emit_expr(&e.rhs, REG_RESULT);
        emit::jump_if(self.buf, JumpCond::Zero, REG_RESULT, lbl_false);

        self.buf.define_label(lbl_true);
        emit::movl_imm_reg(self.buf, 1, dest);
        emit::jump(self.buf, lbl_end);

        self.buf.define_label(lbl_false);
        emit::movl_imm_reg(self.buf, 0, dest);

        self.buf.define_label(lbl_end);
    }

    fn emit_bin_cmp(&mut self, e: &'ast ExprBinType, dest: Reg, op: CmpOp) {
        self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
            emit::cmpl_setl(eg.buf, lhs, op, rhs, dest);

            dest
        });
    }

    fn emit_bin_div(&mut self, e: &'ast ExprBinType, dest: Reg) {
        self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
            let lbl_div = eg.buf.create_label();
            emit::jump_if(eg.buf, JumpCond::NonZero, rhs, lbl_div);
            emit::trap(eg.buf, trap::DIV0);

            eg.buf.define_label(lbl_div);
            emit::divl(eg.buf, lhs, rhs, dest)
        });
    }

    fn emit_bin_mod(&mut self, e: &'ast ExprBinType, dest: Reg) {
        self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
            emit::modl(eg.buf, lhs, rhs, dest)
        });
    }

    fn emit_bin_mul(&mut self, e: &'ast ExprBinType, dest: Reg) {
        self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
            emit::mull(eg.buf, lhs, rhs, dest)
        });
    }

    fn emit_bin_add(&mut self, e: &'ast ExprBinType, dest: Reg) {
        self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
            emit::addl(eg.buf, lhs, rhs, dest)
        });
    }

    fn emit_bin_sub(&mut self, e: &'ast ExprBinType, dest: Reg) {
        self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
            emit::subl(eg.buf, lhs, rhs, dest)
        });
    }

    fn emit_bin_bit_or(&mut self, e: &'ast ExprBinType, dest: Reg) {
        self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
            emit::orl(eg.buf, lhs, rhs, dest)
        });
    }

    fn emit_bin_bit_and(&mut self, e: &'ast ExprBinType, dest: Reg) {
        self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
            emit::andl(eg.buf, lhs, rhs, dest)
        });
    }

    fn emit_bin_bit_xor(&mut self, e: &'ast ExprBinType, dest: Reg) {
        self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
            emit::xorl(eg.buf, lhs, rhs, dest)
        });
    }

    fn emit_binop<F>(&mut self, e: &'ast ExprBinType, dest_reg: Reg, emit_action: F)
            where F: FnOnce(&mut ExprGen, Reg, Reg, Reg) -> Reg {
        let lhs_reg = REG_RESULT;
        let rhs_reg = REG_TMP1;

        let not_leaf = !is_leaf(&e.rhs);
        let mut temp_offset : i32 = 0;

        if not_leaf {
            temp_offset = self.add_temp_var(BuiltinType::Int);
        }

        self.emit_expr(&e.lhs, lhs_reg);
        if not_leaf { emit::mov_reg_local(self.buf, BuiltinType::Int, lhs_reg, temp_offset); }

        self.emit_expr(&e.rhs, rhs_reg);
        if not_leaf { emit::mov_local_reg(self.buf, BuiltinType::Int, temp_offset, lhs_reg); }

        let reg = emit_action(self, lhs_reg, rhs_reg, dest_reg);
        if reg != dest_reg { emit::movl_reg_reg(self.buf, reg, dest_reg); }
    }

    fn add_temp_var(&mut self, ty: BuiltinType) -> i32 {
        self.tempsize += ty.size();

        -(self.tempsize + self.localsize)
    }

    fn emit_call(&mut self, e: &'ast ExprCallType, dest: Reg) {
        let calls = self.ctxt.calls.borrow();
        let fid = *calls.get(&e.id).unwrap();

        self.ctxt.fct_info_for_id(fid, |fct_info| {
            assert!(fct_info.compiled_fct.is_some());

            for (ind, arg) in e.args.iter().enumerate().rev() {
                if REG_PARAMS.len() > ind {
                    let dest = REG_PARAMS[ind];
                    self.emit_expr(arg, dest);
                } else {
                    self.emit_expr(arg, REG_RESULT);
                    emit::push_param(self.buf, REG_RESULT);
                }
            }

            let disp = self.dseg.add_addr(fct_info.compiled_fct.unwrap());
            let pos = self.buf.pos() as i32;

            emit::call(self.buf, disp + pos);

            // TODO: move REG_RESULT into dest
            assert_eq!(REG_RESULT, dest);
        })
    }
}

/// Returns `true` if the given expression `expr` is either literal or
/// variable usage.
pub fn is_leaf(expr: &Expr) -> bool {
    match *expr {
        ExprUn(_) => false,
        ExprBin(ref val) => false,
        ExprLitInt(ref val) => true,
        ExprLitStr(ref val) => true,
        ExprLitBool(ref val) => true,
        ExprIdent(ref val) => true,
        ExprAssign(ref val) => false,
        ExprCall(ref val) => false,
    }
}
