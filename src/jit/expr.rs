use ast::*;
use ast::Expr::*;
use class::{ClassId, FieldId};
use cpu::{self, Reg, REG_RESULT, REG_TMP1, REG_TMP2, REG_PARAMS};
use cpu::emit;
use cpu::trap;
use ctxt::*;
use jit::buffer::*;
use jit::codegen::{self, JumpCond, Scopes, TempOffsets};
use jit::stub::Stub;
use lexer::position::Position;
use mem;
use mem::ptr::Ptr;
use object::{Header, IntArray, Str};
use stdlib;
use ty::{BuiltinType, MachineMode};
use vtable::VTable;

pub struct ExprGen<'a, 'ast: 'a> {
    ctxt: &'a Context<'ast>,
    fct: &'a Fct<'ast>,
    src: &'a mut FctSrc<'ast>,
    ast: &'ast Function,
    buf: &'a mut Buffer,
    scopes: &'a mut Scopes,
    tempsize: i32,
    temps: TempOffsets,
}

impl<'a, 'ast> ExprGen<'a, 'ast> where 'ast: 'a {
    pub fn new(
        ctxt: &'a Context<'ast>,
        fct: &'a Fct<'ast>,
        src: &'a mut FctSrc<'ast>,
        ast: &'ast Function,
        buf: &'a mut Buffer,
        scopes: &'a mut Scopes,
    ) -> ExprGen<'a, 'ast> {
        ExprGen {
            ctxt: ctxt,
            fct: fct,
            src: src,
            ast: ast,
            buf: buf,
            tempsize: 0,
            scopes: scopes,
            temps: TempOffsets::new()
        }
    }

    pub fn generate(mut self, e: &'ast Expr) -> Reg {
        let reg = self.emit_expr(e, REG_RESULT);

        if !self.temps.is_empty() {
            panic!("temporary variables are not fully freed!");
        }

        reg
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
            ExprDelegation(ref expr) => self.emit_delegation(expr, dest),
            ExprField(ref expr) => self.emit_field(expr, dest),
            ExprSelf(_) => self.emit_self(dest),
            ExprSuper(_) => self.emit_self(dest),
            ExprNil(_) => self.emit_nil(dest),
            ExprArray(ref expr) => self.emit_array(expr, dest),
            ExprConv(ref expr) => self.emit_conv(expr, dest),
        }

        dest
    }

    fn emit_conv(&mut self, e: &'ast ExprConvType, dest: Reg) {
        assert!(e.is);

        self.emit_expr(&e.object, dest);

        // return false if object is nil
        let lbl_nil = emit::nil_ptr_check(self.buf, dest);

        if e.valid() {
            emit::movl_imm_reg(self.buf, 1, dest);

        } else {
            let cls_id = e.cls_id();
            let cls = self.ctxt.cls_by_id(cls_id);
            let vtable: &VTable<'ast> = cls.vtable.as_ref().unwrap();

            emit::mov_mem_reg(self.buf, MachineMode::Ptr, dest, 0, REG_TMP1);

            let disp = self.buf.add_addr(vtable as *const VTable<'ast> as *mut u8);
            let pos = self.buf.pos() as i32;

            emit::movq_addr_reg(self.buf, disp + pos, REG_TMP2);

            emit::mov_mem_reg(self.buf, MachineMode::Ptr, REG_TMP1,
                              vtable.subtype_offset, REG_TMP1);

            emit::cmp_setl(self.buf, MachineMode::Ptr, REG_TMP1, CmpOp::Eq, REG_TMP2, dest);
        }

        self.buf.define_label(lbl_nil);
    }

    fn emit_array(&mut self, e: &'ast ExprArrayType, dest: Reg) {
        if self.intrinsic(e.id).is_some() {
            self.emit_expr(&e.object, REG_RESULT);
            let offset = self.reserve_temp_for_node(&e.object);
            emit::mov_reg_local(self.buf, MachineMode::Ptr, REG_RESULT, offset);

            self.emit_expr(&e.index, REG_TMP1);
            emit::mov_local_reg(self.buf, MachineMode::Ptr, offset, REG_RESULT);

            if !self.ctxt.args.flag_omit_bounds_check {
                emit::check_index_out_of_bounds(self.buf, e.pos, REG_RESULT, REG_TMP1, REG_TMP2);
            }

            cpu::instr::emit_addq_imm_reg(self.buf, IntArray::offset_of_data(), REG_RESULT);
            emit::mov_array_reg(self.buf, MachineMode::Int32, REG_RESULT, REG_TMP1, 4, REG_RESULT);

            self.free_temp_for_node(&e.object, offset);

            if dest != REG_RESULT {
                emit::mov_reg_reg(self.buf, MachineMode::Int32, REG_RESULT, dest);
            }

        } else {
            self.emit_universal_call(e.id, e.pos, dest);
        }
    }

    fn reserve_temp_for_node(&mut self, expr: &Expr) -> i32 {
        let id = expr.id();
        let ty = expr.ty();
        let offset = -(self.src.localsize + self.src.get_store(id).offset());

        if ty.reference_type() {
            self.temps.insert(offset);
        }

        offset
    }

    fn reserve_temp_for_arg(&mut self, arg: &Arg<'ast>) -> i32 {
        let offset = -(self.src.localsize + arg.offset());
        let ty = arg.ty();

        if ty.reference_type() {
            self.temps.insert(offset);
        }

        offset
    }

    fn free_temp_for_node(&mut self, expr: &Expr, offset: i32) {
        let ty = expr.ty();

        if ty.reference_type() {
            self.temps.remove(offset);
        }
    }

    fn free_temp_with_type(&mut self, ty: BuiltinType, offset: i32) {
        if ty.reference_type() {
            self.temps.remove(offset);
        }
    }

    fn intrinsic(&self, id: NodeId) -> Option<Intrinsic> {
        let fid = self.src.calls.get(&id).unwrap().fct_id();

        // the function we compile right now is never an intrinsic
        if self.fct.id == fid { return None; }

        let fct = self.ctxt.fct_by_id(fid);

        match fct.kind {
            FctKind::Builtin(intrinsic) => Some(intrinsic),
            _ => None,
        }
    }

    fn emit_self(&mut self, dest: Reg) {
        let var = self.src.var_self();

        emit::mov_local_reg(self.buf, var.ty.mode(), var.offset, dest);
    }

    fn emit_nil(&mut self, dest: Reg) {
        emit::nil(self.buf, dest);
    }

    fn emit_field(&mut self, expr: &'ast ExprFieldType, dest: Reg) {
        let (cls, field) = expr.cls_and_field();

        self.emit_expr(&expr.object, REG_RESULT);
        self.emit_field_access(cls, field, REG_RESULT, dest);
    }

    fn emit_field_access(&mut self, cls: ClassId, field: FieldId, src: Reg, dest: Reg) {
        let cls = self.ctxt.cls_by_id(cls);
        let field = &cls.fields[field];
        emit::mov_mem_reg(self.buf, field.ty.mode(), src, field.offset, dest);
    }

    fn emit_lit_int(&mut self, lit: &'ast ExprLitIntType, dest: Reg) {
        emit::movl_imm_reg(self.buf, lit.value as u32, dest);
    }

    fn emit_lit_bool(&mut self, lit: &'ast ExprLitBoolType, dest: Reg) {
        let value : u32 = if lit.value { 1 } else { 0 };
        emit::movl_imm_reg(self.buf, value, dest);
    }

    fn emit_lit_str(&mut self, lit: &'ast ExprLitStrType, dest: Reg) {
        let handle = Str::from(lit.value.as_bytes());
        self.ctxt.literals.lock().unwrap().push(handle);

        let disp = self.buf.add_addr(handle.raw() as *const u8);
        let pos = self.buf.pos() as i32;

        emit::movq_addr_reg(self.buf, disp + pos, dest);
    }

    fn emit_ident(&mut self, e: &'ast ExprIdentType, dest: Reg) {
        match e.ident_type() {
            IdentType::Var(_) => {
                codegen::var_load(self.buf, self.src, e.var(), dest)
            }

            IdentType::Field(cls, field) => {
                self.emit_self(REG_RESULT);
                self.emit_field_access(cls, field, REG_RESULT, dest);
            }
        }
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
        if e.lhs.is_array() {
            if self.intrinsic(e.id).is_some() {
                let array = e.lhs.to_array().unwrap();
                self.emit_expr(&array.object, REG_RESULT);
                let offset_object = self.reserve_temp_for_node(&array.object);
                emit::mov_reg_local(self.buf, MachineMode::Ptr, REG_RESULT, offset_object);

                self.emit_expr(&array.index, REG_RESULT);
                let offset_index = self.reserve_temp_for_node(&array.index);
                emit::mov_reg_local(self.buf, MachineMode::Int32, REG_RESULT, offset_index);

                self.emit_expr(&e.rhs, REG_RESULT);
                let offset_value = self.reserve_temp_for_node(&e.rhs);
                emit::mov_reg_local(self.buf, MachineMode::Int32, REG_RESULT, offset_value);

                emit::mov_local_reg(self.buf, MachineMode::Ptr, offset_object, REG_TMP1);
                emit::mov_local_reg(self.buf, MachineMode::Int32, offset_index, REG_TMP2);

                if !self.ctxt.args.flag_omit_bounds_check {
                    emit::check_index_out_of_bounds(self.buf, e.pos, REG_TMP1,
                                                    REG_TMP2, REG_RESULT);
                }

                emit::mov_local_reg(self.buf, MachineMode::Int32, offset_value, REG_RESULT);
                cpu::instr::emit_addq_imm_reg(self.buf, IntArray::offset_of_data(), REG_TMP1);
                emit::shiftlq_imm_reg(self.buf, 2, REG_TMP2);
                emit::addq_reg_reg(self.buf, REG_TMP2, REG_TMP1);
                emit::mov_reg_mem(self.buf, MachineMode::Int32, REG_RESULT, REG_TMP1, 0);

                self.free_temp_for_node(&array.object, offset_object);
                self.free_temp_for_node(&array.index, offset_index);
                self.free_temp_for_node(&e.rhs, offset_value);
            } else {
                self.emit_universal_call(e.id, e.pos, dest);
            }

            return;
        }

        let ident_type = if e.lhs.is_ident() {
            e.lhs.to_ident().unwrap().ident_type()

        } else if e.lhs.is_field() {
            let (cls, field) = e.lhs.to_field().unwrap().cls_and_field();

            IdentType::Field(cls, field)

        } else {
            unreachable!();
        };

        match ident_type {
            IdentType::Var(_) => {
                self.emit_expr(&e.rhs, dest);
                let lhs = e.lhs.to_ident().unwrap();
                codegen::var_store(&mut self.buf, self.src, dest, lhs.var());
            }

            IdentType::Field(clsid, fieldid) => {
                let cls = self.ctxt.cls_by_id(clsid);
                let field = &cls.fields[fieldid];

                let temp = if let Some(expr_field) = e.lhs.to_field() {
                    self.emit_expr(&expr_field.object, REG_RESULT);

                    &expr_field.object

                } else {
                    self.emit_self(REG_RESULT);

                    &e.lhs
                };

                let temp_offset = self.reserve_temp_for_node(temp);
                emit::mov_reg_local(self.buf, MachineMode::Ptr, REG_RESULT, temp_offset);

                self.emit_expr(&e.rhs, REG_RESULT);
                emit::mov_local_reg(self.buf, MachineMode::Ptr, temp_offset, REG_TMP1);

                emit::mov_reg_mem(self.buf, field.ty.mode(), REG_RESULT, REG_TMP1, field.offset);
                self.free_temp_for_node(temp, temp_offset);

                if REG_RESULT != dest {
                    emit::mov_reg_reg(self.buf, field.ty.mode(), REG_RESULT, dest);
                }
            }
        }
    }

    fn emit_bin(&mut self, e: &'ast ExprBinType, dest: Reg) {
        match e.op {
            BinOp::Add => self.emit_bin_add(e, dest),
            BinOp::Sub => self.emit_bin_sub(e, dest),
            BinOp::Mul => self.emit_bin_mul(e, dest),
            BinOp::Div
                | BinOp::Mod => self.emit_bin_divmod(e, dest),
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
        let lhs_type = e.lhs.ty();
        let rhs_type = e.rhs.ty();

        let cmp_type = lhs_type.if_nil(rhs_type);

        if op == CmpOp::Is || op == CmpOp::IsNot {
            let op = if op == CmpOp::Is { CmpOp::Eq } else { CmpOp::Ne };

            self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
                emit::cmp_setl(eg.buf, MachineMode::Ptr, lhs, op, rhs, dest);

                dest
            });

            return;
        }

        if cmp_type == BuiltinType::Str {
            self.emit_universal_call(e.id, e.pos, dest);
            emit::movl_imm_reg(self.buf, 0, REG_TMP1);
            emit::cmp_setl(self.buf, MachineMode::Int32, REG_RESULT, op, REG_TMP1, dest);

        } else {
            self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
                emit::cmp_setl(eg.buf, MachineMode::Int32, lhs, op, rhs, dest);

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
        if self.has_call_site(e.id) {
            self.emit_universal_call(e.id, e.pos, dest);

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

        if let Some(&Store::Temp(_, _)) = self.src.storage.get(&e.lhs.id()) {
            let offset = self.reserve_temp_for_node(&e.lhs);
            let ty = e.lhs.ty();

            self.emit_expr(&e.lhs, REG_RESULT);
            emit::mov_reg_local(self.buf, ty.mode(), REG_RESULT, offset);

            self.emit_expr(&e.rhs, rhs_reg);
            emit::mov_local_reg(self.buf, ty.mode(), offset, lhs_reg);

            self.free_temp_for_node(&e.lhs, offset);
        } else {
            self.emit_expr(&e.lhs, lhs_reg);
            self.emit_expr(&e.rhs, rhs_reg);
        }

        let ty = e.ty();
        let reg = emit_action(self, lhs_reg, rhs_reg, dest_reg);
        if reg != dest_reg { emit::mov_reg_reg(self.buf, ty.mode(), reg, dest_reg); }
    }

    fn ptr_for_fct_id(&mut self, fid: FctId) -> *const u8 {
        if self.fct.id == fid {
            // we want to recursively invoke the function we are compiling right now
            ensure_jit_or_stub_ptr(fid, self.src, self.ctxt).raw()

        } else {
            let fct = self.ctxt.fct_by_id(fid);

            match fct.kind {
                FctKind::Source(_) => {
                    let src = fct.src();
                    let mut src = src.lock().unwrap();

                    ensure_jit_or_stub_ptr(fid, &mut src, self.ctxt).raw()
                },
                FctKind::Native(ptr) => ptr.raw(),

                FctKind::Definition => unreachable!(),
                FctKind::Builtin(_) => panic!("intrinsic fct call"),
            }
        }
    }

    fn emit_call(&mut self, e: &'ast ExprCallType, dest: Reg) {
        if let Some(intrinsic) = self.intrinsic(e.id) {
            match intrinsic {
                Intrinsic::IntArrayLen => self.emit_intrinsic_len(e, dest),
                Intrinsic::Assert => self.emit_intrinsic_assert(e, dest),
                Intrinsic::Shl => self.emit_intrinsic_shl(e, dest),
                _ => panic!("unknown intrinsic {:?}", intrinsic),
            }

            return;
        }

        self.emit_universal_call(e.id, e.pos, dest);
    }

    fn emit_intrinsic_len(&mut self, e: &'ast ExprCallType, dest: Reg) {
        self.emit_expr(&e.object.as_ref().unwrap(), REG_RESULT);
        emit::mov_mem_reg(self.buf, MachineMode::Ptr, REG_RESULT, Header::size(), dest);
    }

    fn emit_intrinsic_assert(&mut self, e: &'ast ExprCallType, _: Reg) {
        let lbl_div = self.buf.create_label();
        self.emit_expr(&e.args[0], REG_RESULT);
        emit::jump_if(self.buf, JumpCond::Zero, REG_RESULT, lbl_div);
        self.buf.emit_bailout(lbl_div, trap::ASSERT, e.pos);
    }

    fn emit_intrinsic_shl(&mut self, e: &'ast ExprCallType, dest: Reg) {
        self.emit_expr(&e.args[0], REG_RESULT);
        let offset = self.reserve_temp_for_node(&e.args[0]);
        emit::mov_reg_local(self.buf, MachineMode::Int32, REG_RESULT, offset);

        self.emit_expr(&e.args[1], cpu::reg::RCX);
        emit::mov_local_reg(self.buf, MachineMode::Int32, offset, REG_RESULT);
        emit::shll_reg_cl(self.buf, REG_RESULT);

        if REG_RESULT != dest {
            emit::mov_reg_reg(self.buf, MachineMode::Int32, REG_RESULT, dest);
        }
    }

    fn emit_delegation(&mut self, e: &'ast ExprDelegationType, dest: Reg) {
        self.emit_universal_call(e.id, e.pos, dest);
    }

    fn has_call_site(&self, id: NodeId) -> bool {
        self.src.call_sites.get(&id).is_some()
    }

    fn emit_universal_call(&mut self, id: NodeId, pos: Position, dest: Reg) {
        let csite = self.src.call_sites.get(&id).unwrap().clone();
        let mut temps : Vec<(BuiltinType, i32)> = Vec::new();

        for arg in &csite.args {
            match *arg {
                Arg::Expr(ast, _, _) => {
                    self.emit_expr(ast, REG_RESULT);
                }

                Arg::Selfie(_, _) => {
                    self.emit_self(REG_RESULT);
                }

                Arg::SelfieNew(cls_id, _) => {
                    // allocate storage for object
                    let cls = self.ctxt.cls_by_id(cls_id);
                    emit::movl_imm_reg(self.buf, cls.size as u32, REG_PARAMS[0]);

                    let mptr = stdlib::gc_alloc as *mut u8;
                    self.emit_direct_call_insn(mptr, pos, BuiltinType::Ptr, REG_RESULT);

                    // store classptr in object
                    let cptr = (&**cls.vtable.as_ref().unwrap()) as *const VTable as *const u8;
                    let disp = self.buf.add_addr(cptr);
                    let pos = self.buf.pos() as i32;

                    emit::movq_addr_reg(self.buf, disp + pos, REG_TMP1);
                    emit::mov_reg_mem(self.buf, MachineMode::Ptr, REG_TMP1, REG_RESULT, 0);
                }
            }

            let offset = self.reserve_temp_for_arg(arg);
            emit::mov_reg_local(self.buf, arg.ty().mode(), REG_RESULT, offset);
            temps.push((arg.ty(), offset));
        }

        let mut arg_offset = -self.src.stacksize();

        for (ind, arg) in csite.args.iter().enumerate() {
            let ty = arg.ty();
            let offset = temps[ind].1;

            if ind < REG_PARAMS.len() {
                let reg = REG_PARAMS[ind];
                emit::mov_local_reg(self.buf, ty.mode(), offset, reg);

                if ind == 0 {
                    let call_type = self.src.calls.get(&id);

                    if call_type.is_some() && call_type.unwrap().is_method()
                        && check_for_nil(ty) {
                        emit::nil_ptr_check_bailout(self.buf, pos, reg);
                    }
                }

            } else {
                emit::mov_local_reg(self.buf, ty.mode(), offset, REG_RESULT);
                emit::mov_reg_local(self.buf, ty.mode(), REG_RESULT, arg_offset);

                arg_offset += 8;
            }
        }

        match csite.callee {
            Callee::Fct(fid) => {
                let fct = self.ctxt.fct_by_id(fid);

                if csite.super_call {
                    let ptr = self.ptr_for_fct_id(fid);
                    self.emit_direct_call_insn(ptr, pos, csite.return_type, dest);

                } else if fct.is_virtual() {
                    let vtable_index = fct.vtable_index.unwrap();
                    self.emit_indirect_call_insn(vtable_index, pos, csite.return_type, dest);

                } else {
                    let ptr = self.ptr_for_fct_id(fid);
                    self.emit_direct_call_insn(ptr, pos, csite.return_type, dest);
                }
            }

            Callee::Ptr(ptr) => {
                self.emit_direct_call_insn(ptr.raw(), pos, csite.return_type, dest);
            }
        }

        if csite.args.len() > 0 {
            if let Arg::SelfieNew(_, _) = csite.args[0] {
                let (ty, offset) = temps[0];
                emit::mov_local_reg(self.buf, ty.mode(), offset, dest);
            }
        }

        for temp in temps.into_iter() {
            self.free_temp_with_type(temp.0, temp.1);
        }
    }

    fn emit_direct_call_insn(&mut self, ptr: *const u8, pos: Position, ty: BuiltinType, dest: Reg) {
        self.insn_direct_call(ptr);
        self.emit_after_call_insns(pos, ty, dest);
    }

    fn emit_indirect_call_insn(&mut self, index: u32, pos: Position, ty: BuiltinType, dest: Reg) {
        self.insn_indirect_call(index);
        self.emit_after_call_insns(pos, ty, dest);
    }

    fn emit_after_call_insns(&mut self, pos: Position, ty: BuiltinType, dest: Reg) {
        self.buf.emit_lineno(pos.line as i32);

        let gcpoint = codegen::create_gcpoint(self.scopes, &self.temps);
        self.buf.emit_gcpoint(gcpoint);

        if REG_RESULT != dest {
            emit::mov_reg_reg(self.buf, ty.mode(), REG_RESULT, dest);
        }
    }

    fn insn_direct_call(&mut self, ptr: *const u8) {
        let disp = self.buf.add_addr(ptr);
        let pos = self.buf.pos() as i32;

        emit::movq_addr_reg(self.buf, disp + pos, REG_RESULT);
        emit::call(self.buf, REG_RESULT);
    }

    fn insn_indirect_call(&mut self, index: u32) {
        let obj = REG_PARAMS[0];

        // REG_RESULT = [obj]
        // REG_TMP1 = offset table in vtable
        // REG_RESULT = REG_RESULT + REG_TMP1
        emit::mov_mem_reg(self.buf, MachineMode::Ptr, obj, 0, REG_RESULT);
        emit::movl_imm_reg(self.buf, VTable::offset_of_table() as u32, REG_TMP1);
        emit::addq_reg_reg(self.buf, REG_TMP1, REG_RESULT);

        // REG_TMP1 = index
        // REG_RESULT = [REG_RESULT + 8 * REG_TMP1]
        emit::movl_imm_reg(self.buf, index, REG_TMP1);
        emit::mov_array_reg(self.buf, MachineMode::Ptr, REG_RESULT,
            REG_TMP1, mem::ptr_width() as u8, REG_RESULT);

        // call *REG_RESULT
        emit::call(self.buf, REG_RESULT);
    }
}

fn check_for_nil(ty: BuiltinType) -> bool {
    match ty {
        BuiltinType::Unit => false,
        BuiltinType::Str => true,
        BuiltinType::Int | BuiltinType::Bool => false,
        BuiltinType::Nil | BuiltinType::Ptr | BuiltinType::IntArray => true,
        BuiltinType::Class(_) => true
    }
}

fn ensure_jit_or_stub_ptr<'ast>(fid: FctId, src: &mut FctSrc<'ast>, ctxt: &Context) -> Ptr {
    if let Some(ref jit) = src.jit_fct { return jit.fct_ptr(); }
    if let Some(ref stub) = src.stub { return stub.ptr_start(); }

    let stub = Stub::new(fid);

    {
        let mut code_map = ctxt.code_map.lock().unwrap();
        code_map.insert(stub.ptr_start(), stub.ptr_end(), fid);
    }

    if ctxt.args.flag_emit_stubs {
        println!("create stub at {:x}", stub.ptr_start().raw() as usize);
    }

    let ptr = stub.ptr_start();

    src.stub = Some(stub);

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
        ExprDelegation(_) => false,
        ExprField(_) => false,
        ExprSelf(_) => true,
        ExprSuper(_) => true,
        ExprNil(_) => true,
        ExprArray(_) => false,
        ExprConv(_) => false,
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
        ExprCall(_) => true,
        ExprDelegation(_) => true,
        ExprField(ref e) => contains_fct_call(&e.object),
        ExprSelf(_) => false,
        ExprSuper(_) => false,
        ExprNil(_) => false,
        ExprArray(ref e) => contains_fct_call(&e.object) || contains_fct_call(&e.index),
        ExprConv(ref e) => contains_fct_call(&e.object),
    }
}
