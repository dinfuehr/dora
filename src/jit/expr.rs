use ast::*;
use ast::Expr::*;
use cpu::{Reg, REG_RESULT, REG_TMP1, REG_PARAMS};
use cpu::emit;
use cpu::trap;
use ctxt::*;
use jit::buffer::*;
use jit::codegen::{self, JumpCond};
use jit::stub::Stub;
use object::Str;
use mem::ptr::Ptr;
use ty::BuiltinType;

pub struct ExprGen<'a, 'ast: 'a> {
    ctxt: &'a Context<'a, 'ast>,
    fct: &'a mut Fct<'ast>,
    ast: &'ast Function,
    buf: &'a mut Buffer,
    tempsize: i32,
}

impl<'a, 'ast> ExprGen<'a, 'ast> where 'ast: 'a {
    pub fn new(
        ctxt: &'a Context<'a, 'ast>,
        fct: &'a mut Fct<'ast>,
        ast: &'ast Function,
        buf: &'a mut Buffer,
    ) -> ExprGen<'a, 'ast> {
        ExprGen {
            ctxt: ctxt,
            fct: fct,
            ast: ast,
            buf: buf,
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
            ExprLitStr(ref expr) => self.emit_lit_str(expr, dest),
            ExprUn(ref expr) => self.emit_un(expr, dest),
            ExprIdent(ref expr) => self.emit_ident(expr, dest),
            ExprAssign(ref expr) => self.emit_assign(expr, dest),
            ExprBin(ref expr) => self.emit_bin(expr, dest),
            ExprCall(ref expr) => self.emit_call(expr, dest),
            ExprProp(_) => unreachable!(),
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

    fn emit_lit_str(&mut self, lit: &'ast ExprLitStrType, dest: Reg) {
        let string = {
            let mut gc = self.ctxt.gc.lock().unwrap();
            Str::from_buffer(&mut gc, lit.value.as_bytes())
        };

        let disp = self.buf.add_addr(string.ptr());
        let pos = self.buf.pos() as i32;

        emit::movq_addr_reg(self.buf, disp + pos, dest);
    }

    fn emit_ident(&mut self, e: &'ast ExprIdentType, dest: Reg) {
        codegen::var_load(self.buf, self.fct, e.id, dest);
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

        codegen::var_store(&mut self.buf, self.fct, dest, e.lhs.id());
    }

    fn emit_bin(&mut self, e: &'ast ExprBinType, dest: Reg) {
        match e.op {
            BinOp::Add => self.emit_bin_add(e, dest),
            BinOp::Sub => self.emit_bin_sub(e, dest),
            BinOp::Mul => self.emit_bin_mul(e, dest),
            BinOp::Div |
            BinOp::Mod => self.emit_bin_divmod(e, dest),
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
        let cmp_type = *self.fct.src().types.get(&e.lhs.id()).unwrap();

        if cmp_type == BuiltinType::Str {
            if op == CmpOp::Is {
                self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
                    emit::cmp_setl(eg.buf, BuiltinType::Str, lhs, CmpOp::Eq, rhs, dest);

                    dest
                });

            } else if op == CmpOp::IsNot {
                self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
                    emit::cmp_setl(eg.buf, BuiltinType::Str, lhs, CmpOp::Ne, rhs, dest);

                    dest
                });

            } else {
                use libc::c_void;
                use stdlib;

                let fct = Ptr::new(stdlib::strcmp as *mut c_void);
                self.emit_builtin_call(fct, e, REG_RESULT);
                emit::movl_imm_reg(self.buf, 0, REG_TMP1);
                emit::cmp_setl(self.buf, BuiltinType::Int, REG_RESULT, op, REG_TMP1, dest);
            }

        } else {
            self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
                emit::cmp_setl(eg.buf, BuiltinType::Int, lhs, op, rhs, dest);

                dest
            });
        }
    }

    fn emit_bin_divmod(&mut self, e: &'ast ExprBinType, dest: Reg) {
        self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
            let lbl_div = eg.buf.create_label();
            emit::jump_if(eg.buf, JumpCond::NonZero, rhs, lbl_div);
            trap::emit(eg.buf, trap::DIV0);

            eg.buf.define_label(lbl_div);

            if e.op == BinOp::Div {
                emit::divl(eg.buf, lhs, rhs, dest)
            } else {
                emit::modl(eg.buf, lhs, rhs, dest)
            }
        });
    }

    fn emit_bin_mul(&mut self, e: &'ast ExprBinType, dest: Reg) {
        self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
            emit::mull(eg.buf, lhs, rhs, dest)
        });
    }

    fn emit_bin_add(&mut self, e: &'ast ExprBinType, dest: Reg) {
        let add_type = *self.fct.src().types.get(&e.id).unwrap();

        if add_type == BuiltinType::Str {
            use libc::c_void;
            use stdlib;

            let fct = Ptr::new(stdlib::strcat as *mut c_void);
            self.emit_builtin_call(fct, e, dest);

        } else {
            self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
                emit::addl(eg.buf, lhs, rhs, dest)
            });
        }
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

        let ty = *self.fct.src().types.get(&e.id).unwrap();

        if not_leaf {
            temp_offset = self.add_temp_var(ty);
        }

        self.emit_expr(&e.lhs, lhs_reg);
        if not_leaf { emit::mov_reg_local(self.buf, ty, lhs_reg, temp_offset); }

        self.emit_expr(&e.rhs, rhs_reg);
        if not_leaf { emit::mov_local_reg(self.buf, ty, temp_offset, lhs_reg); }

        let reg = emit_action(self, lhs_reg, rhs_reg, dest_reg);
        if reg != dest_reg { emit::mov_reg_reg(self.buf, ty, reg, dest_reg); }
    }

    fn add_temp_var(&mut self, ty: BuiltinType) -> i32 {
        self.tempsize += ty.size();

        -(self.tempsize + self.fct.src().localsize)
    }

    fn emit_call(&mut self, e: &'ast ExprCallType, dest: Reg) {
        let fid = *self.fct.src().calls.get(&e.id).unwrap();

        let ptr = if self.fct.id == fid {
            // we want to recursively invoke the function we are compiling right now
            ensure_jit_or_stub_ptr(self.fct, self.ctxt)

        } else {
            self.ctxt.fct_by_id_mut(fid, |fct| {
                match fct.kind {
                    FctKind::Source(_) | FctKind::Gen(_) => ensure_jit_or_stub_ptr(fct, self.ctxt),
                    FctKind::Builtin(ptr) => ptr,
                    FctKind::Intrinsic => unreachable!("intrinsic fct call"),
                }
            })
        };

        for (ind, arg) in e.args.iter().enumerate().rev() {
            assert!(!contains_fct_call(arg));

            if REG_PARAMS.len() > ind {
                let dest = REG_PARAMS[ind];
                self.emit_expr(arg, dest);
            } else {
                self.emit_expr(arg, REG_RESULT);
                emit::push_param(self.buf, REG_RESULT);
            }
        }

        let disp = self.buf.add_addr(ptr);
        let pos = self.buf.pos() as i32;

        emit::movq_addr_reg(self.buf, disp + pos, REG_RESULT);
        emit::call(self.buf, REG_RESULT);

        if REG_RESULT != dest {
            let return_type = *self.fct.src().types.get(&e.id).unwrap();
            emit::mov_reg_reg(self.buf, return_type, REG_RESULT, dest);
        }
    }

    fn emit_builtin_call(&mut self, fct: Ptr, expr: &'ast ExprBinType, dest: Reg) {
        assert!(!contains_fct_call(&expr.lhs));
        self.emit_expr(&expr.lhs, REG_PARAMS[0]);

        assert!(!contains_fct_call(&expr.rhs));
        self.emit_expr(&expr.rhs, REG_PARAMS[1]);

        let disp = self.buf.add_addr(fct);
        let pos = self.buf.pos() as i32;

        emit::movq_addr_reg(self.buf, disp + pos, REG_RESULT);
        emit::call(self.buf, REG_RESULT);

        if REG_RESULT != dest {
            let return_type = *self.fct.src().types.get(&expr.id).unwrap();
            emit::mov_reg_reg(self.buf, return_type, REG_RESULT, dest);
        }
    }
}

fn ensure_jit_or_stub_ptr<'ast>(fct: &mut Fct<'ast>, ctxt: &Context) -> Ptr {
    if fct.is_src() {
        let src = fct.src();

        if let Some(ref jit) = src.jit_fct { return jit.fct_ptr(); }
        if let Some(ref stub) = src.stub { return stub.ptr_start(); }
    } else {
        let gen = fct.gen();

        if let Some(ref jit) = gen.jit_fct { return jit.fct_ptr(); }
        if let Some(ref stub) = gen.stub { return stub.ptr_start(); }
    }

    let stub = Stub::new();

    {
        let mut code_map = ctxt.code_map.lock().unwrap();
        code_map.insert(stub.ptr_start(), stub.ptr_end(), fct.id);
    }

    if ctxt.args.flag_emit_stubs {
        println!("create stub at {:?}", stub.ptr_start());
    }

    let ptr = stub.ptr_start();

    if fct.is_src() {
        fct.src_mut().stub = Some(stub);
    } else {
        fct.gen_mut().stub = Some(stub);
    }

    ptr
}

/// Returns `true` if the given expression `expr` is either literal or
/// variable usage.
pub fn is_leaf(expr: &Expr) -> bool {
    match *expr {
        ExprUn(_) => false,
        ExprBin(_) => false,
        ExprLitInt(_) => true,
        ExprLitStr(_) => true,
        ExprLitBool(_) => true,
        ExprIdent(_) => true,
        ExprAssign(_) => false,
        ExprCall(_) => false,
        ExprProp(_) => false,
    }
}

/// Returns `true` if the given expression `expr` contains a function call
pub fn contains_fct_call(expr: &Expr) -> bool {
    match *expr {
        ExprUn(ref e) => contains_fct_call(&e.opnd),
        ExprBin(ref e) => contains_fct_call(&e.lhs) || contains_fct_call(&e.rhs),
        ExprLitInt(_) => false,
        ExprLitStr(_) => false,
        ExprLitBool(_) => false,
        ExprIdent(_) => false,
        ExprAssign(ref e) => contains_fct_call(&e.lhs) || contains_fct_call(&e.rhs),
        ExprCall(ref val) => true,
        ExprProp(ref e) => contains_fct_call(&e.object),
    }
}
