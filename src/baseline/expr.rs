use ast::*;
use ast::Expr::*;
use baseline::codegen::{self, dump_asm, CondCode, Scopes, TempOffsets};
use baseline::fct::{CatchType, Comment};
use baseline::native;
use baseline::stub::Stub;
use class::{ClassId, FieldId};
use cpu::{Mem, Reg, REG_RESULT, REG_TMP1, REG_TMP2, REG_PARAMS};
use cpu::emit;
use cpu::trap;
use ctxt::*;
use driver::cmd::AsmSyntax;
use lexer::position::Position;
use masm::*;
use mem;
use mem::ptr::Ptr;
use object::{Header, Str};
use stdlib;
use ty::{BuiltinType, MachineMode};
use vtable::{DISPLAY_SIZE, VTable};

pub struct ExprGen<'a, 'ast: 'a> {
    ctxt: &'a Context<'ast>,
    fct: &'a Fct<'ast>,
    src: &'a mut FctSrc<'ast>,
    ast: &'ast Function,
    masm: &'a mut MacroAssembler,
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
        masm: &'a mut MacroAssembler,
        scopes: &'a mut Scopes,
    ) -> ExprGen<'a, 'ast> {
        ExprGen {
            ctxt: ctxt,
            fct: fct,
            src: src,
            ast: ast,
            masm: masm,
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
            ExprTry(ref expr) => self.emit_try(expr, dest),
        }

        dest
    }

    fn emit_try(&mut self, e: &'ast ExprTryType, dest: Reg) {
        match e.mode {
            TryMode::Normal => {
                self.emit_expr(&e.expr, dest);
            },

            TryMode::Else(ref alt_expr) => {
                let lbl_after = self.masm.create_label();

                let try_span = {
                    let start = self.masm.pos();
                    self.emit_expr(&e.expr, dest);
                    let end = self.masm.pos();

                    emit::jump(&mut self.masm, lbl_after);

                    (start, end)
                };

                let catch_span = {
                    let start = self.masm.pos();
                    self.emit_expr(alt_expr, dest);
                    let end = self.masm.pos();

                    (start, end)
                };

                self.masm.emit_exception_handler(try_span, catch_span.0, None, CatchType::Any);
                self.masm.bind_label(lbl_after);
            }

            TryMode::Force => {
                let lbl_after = self.masm.create_label();

                let try_span = {
                    let start = self.masm.pos();
                    self.emit_expr(&e.expr, dest);
                    let end = self.masm.pos();

                    emit::jump(&mut self.masm, lbl_after);

                    (start, end)
                };

                let catch_span = {
                    let start = self.masm.pos();
                    self.masm.emit_bailout_inplace(trap::UNEXPECTED, e.pos);
                    let end = self.masm.pos();

                    (start, end)
                };

                self.masm.emit_exception_handler(try_span, catch_span.0, None, CatchType::Any);
                self.masm.bind_label(lbl_after);
            }

            TryMode::Opt => panic!("unsupported"),
        }
    }

    fn emit_conv(&mut self, e: &'ast ExprConvType, dest: Reg) {
        self.emit_expr(&e.object, dest);

        // return false if object is nil
        let lbl_nil = emit::test_if_nil(self.masm, dest);

        if e.valid() {
            if e.is {
                // return true for object is T
                emit::load_true(self.masm, dest);

            } else {
                // do nothing for object as T
            }

        } else {
            let cls_id = e.cls_id();
            let cls = self.ctxt.cls_by_id(cls_id);
            let vtable: &VTable<'ast> = cls.vtable.as_ref().unwrap();

            let offset = if e.is {
                0
            } else {
                // reserve temp variable for object
                let offset = self.reserve_temp_for_node(&e.object);
                emit::store_mem(self.masm, MachineMode::Ptr, Mem::Local(offset), dest);

                offset
            };

            // object instanceof T

            // tmp1 = <vtable of object>
            emit::load_mem(self.masm, MachineMode::Ptr, REG_TMP1, Mem::Base(dest, 0));

            let disp = self.masm.add_addr(vtable as *const _ as *mut u8);
            let pos = self.masm.pos() as i32;

            // tmp2 = <vtable of T>
            emit::load_constpool(self.masm, REG_TMP2, disp + pos);

            if vtable.subtype_depth >= DISPLAY_SIZE as i32 {
                // cmp [tmp1 + offset T.vtable.subtype_depth], tmp3
                emit::cmp_mem_imm(self.masm, MachineMode::Int32,
                                  Mem::Base(REG_TMP1, VTable::offset_of_depth()),
                                  vtable.subtype_depth);

                // jnz lbl_false
                let lbl_false = self.masm.create_label();
                emit::jump_if(self.masm, CondCode::Less, lbl_false);

                // tmp1 = tmp1.subtype_overflow
                emit::load_mem(self.masm, MachineMode::Ptr, REG_TMP1,
                               Mem::Base(REG_TMP1, VTable::offset_of_overflow()));

                let overflow_offset = mem::ptr_width() *
                                        (vtable.subtype_depth - DISPLAY_SIZE as i32);

                // cmp [tmp1 + 8*(vtable.subtype_depth - DISPLAY_SIZE) ], tmp2
                emit::cmp_mem(self.masm, MachineMode::Ptr,
                              Mem::Base(REG_TMP1, overflow_offset),
                              REG_TMP2);

                if e.is {
                    // dest = if zero then true else false
                    emit::set(self.masm, dest, CondCode::Equal);

                } else {
                    // jump to lbl_false if cmp did not succeed
                    emit::jump_if(self.masm, CondCode::NonZero, lbl_false);

                    // otherwise load temp variable again
                    emit::load_mem(self.masm, MachineMode::Ptr, dest, Mem::Local(offset));
                }

                // jmp lbl_finished
                let lbl_finished = self.masm.create_label();
                emit::jump(self.masm, lbl_finished);

                // lbl_false:
                self.masm.bind_label(lbl_false);

                if e.is {
                    // dest = false
                    emit::load_false(self.masm, dest);
                } else {
                    // bailout
                    self.masm.emit_bailout_inplace(trap::CAST, e.pos);
                }

                // lbl_finished:
                self.masm.bind_label(lbl_finished);
            } else {
                let display_entry = VTable::offset_of_display()
                                    + vtable.subtype_depth * mem::ptr_width();

                // tmp1 = vtable of object
                // tmp2 = vtable of T
                // cmp [tmp1 + offset], tmp2
                emit::cmp_mem(self.masm, MachineMode::Ptr, Mem::Base(REG_TMP1, display_entry),
                               REG_TMP2);

                if e.is {
                    emit::set(self.masm, dest, CondCode::Equal);

                } else {
                    let lbl_bailout = self.masm.create_label();
                    emit::jump_if(self.masm, CondCode::NotEqual, lbl_bailout);
                    self.masm.emit_bailout(lbl_bailout, trap::CAST, e.pos);

                    emit::load_mem(self.masm, MachineMode::Ptr, dest, Mem::Local(offset));
                }
            }

            if !e.is {
                self.free_temp_for_node(&e.object, offset);
            }
        }

        // lbl_nil:
        self.masm.bind_label(lbl_nil);

        // for is we are finished: dest is null which is boolean false
        // also for as we are finished: dest is null and stays null
    }

    fn emit_array(&mut self, e: &'ast ExprArrayType, dest: Reg) {
        if self.intrinsic(e.id).is_some() {
            self.emit_expr(&e.object, REG_RESULT);
            let offset = self.reserve_temp_for_node(&e.object);
            emit::store_mem(self.masm, MachineMode::Ptr, Mem::Local(offset), REG_RESULT);

            self.emit_expr(&e.index, REG_TMP1);
            emit::load_mem(self.masm, MachineMode::Ptr, REG_RESULT, Mem::Local(offset));

            if !self.ctxt.args.flag_omit_bounds_check {
                emit::check_index_out_of_bounds(self.masm, e.pos, REG_RESULT, REG_TMP1, REG_TMP2);
            }

            emit::load_array_elem(self.masm, MachineMode::Int32, REG_RESULT, REG_RESULT, REG_TMP1);

            self.free_temp_for_node(&e.object, offset);

            if dest != REG_RESULT {
                emit::copy_reg(self.masm, MachineMode::Int32, dest, REG_RESULT);
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

        emit::load_mem(self.masm, var.ty.mode(), dest, Mem::Local(var.offset));
    }

    fn emit_nil(&mut self, dest: Reg) {
        emit::load_nil(self.masm, dest);
    }

    fn emit_field(&mut self, expr: &'ast ExprFieldType, dest: Reg) {
        let (cls, field) = expr.cls_and_field();

        self.emit_expr(&expr.object, REG_RESULT);
        self.emit_field_access(cls, field, REG_RESULT, dest);
    }

    fn emit_field_access(&mut self, cls: ClassId, field: FieldId, src: Reg, dest: Reg) {
        let cls = self.ctxt.cls_by_id(cls);
        let field = &cls.fields[field];
        emit::load_mem(self.masm, field.ty.mode(), dest, Mem::Base(src, field.offset));
    }

    fn emit_lit_int(&mut self, lit: &'ast ExprLitIntType, dest: Reg) {
        emit::load_int_const(self.masm, MachineMode::Int32, dest, lit.value);
    }

    fn emit_lit_bool(&mut self, lit: &'ast ExprLitBoolType, dest: Reg) {
        if lit.value {
            emit::load_true(self.masm, dest);
        } else {
            emit::load_false(self.masm, dest);
        };
    }

    fn emit_lit_str(&mut self, lit: &'ast ExprLitStrType, dest: Reg) {
        let handle = Str::from(lit.value.as_bytes());
        self.ctxt.literals.lock().unwrap().push(handle);

        let disp = self.masm.add_addr(handle.raw() as *const u8);
        let pos = self.masm.pos() as i32;

        self.masm.emit_comment(Comment::LoadString(handle));
        emit::load_constpool(self.masm, dest, disp + pos);
    }

    fn emit_ident(&mut self, e: &'ast ExprIdentType, dest: Reg) {
        match e.ident_type() {
            IdentType::Var(_) => {
                codegen::var_load(self.masm, self.src, e.var(), dest)
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
            UnOp::Neg => emit::int_neg(self.masm, dest, dest),
            UnOp::BitNot => emit::int_not(self.masm, dest, dest),
            UnOp::Not => emit::bool_not(self.masm, dest, dest)
        }
    }

    fn emit_assign(&mut self, e: &'ast ExprAssignType, dest: Reg) {
        if e.lhs.is_array() {
            if self.intrinsic(e.id).is_some() {
                let array = e.lhs.to_array().unwrap();
                self.emit_expr(&array.object, REG_RESULT);
                let offset_object = self.reserve_temp_for_node(&array.object);
                emit::store_mem(self.masm, MachineMode::Ptr, Mem::Local(offset_object),
                                REG_RESULT);

                self.emit_expr(&array.index, REG_RESULT);
                let offset_index = self.reserve_temp_for_node(&array.index);
                emit::store_mem(self.masm, MachineMode::Int32, Mem::Local(offset_index),
                                REG_RESULT);

                self.emit_expr(&e.rhs, REG_RESULT);
                let offset_value = self.reserve_temp_for_node(&e.rhs);
                emit::store_mem(self.masm, MachineMode::Int32, Mem::Local(offset_value),
                                REG_RESULT);

                emit::load_mem(self.masm, MachineMode::Ptr, REG_TMP1, Mem::Local(offset_object));
                emit::load_mem(self.masm, MachineMode::Int32, REG_TMP2, Mem::Local(offset_index));

                if !self.ctxt.args.flag_omit_bounds_check {
                    emit::check_index_out_of_bounds(self.masm, e.pos, REG_TMP1,
                                                    REG_TMP2, REG_RESULT);
                }

                emit::load_mem(self.masm, MachineMode::Int32, REG_RESULT, Mem::Local(offset_value));
                emit::store_array_elem(self.masm, MachineMode::Int32, REG_TMP1, REG_TMP2,
                                       REG_RESULT);


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
                codegen::var_store(&mut self.masm, self.src, dest, lhs.var());
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
                emit::store_mem(self.masm, MachineMode::Ptr, Mem::Local(temp_offset), REG_RESULT);

                self.emit_expr(&e.rhs, REG_RESULT);
                emit::load_mem(self.masm, MachineMode::Ptr, REG_TMP1, Mem::Local(temp_offset));

                emit::store_mem(self.masm, field.ty.mode(),
                                Mem::Base(REG_TMP1, field.offset), REG_RESULT);
                self.free_temp_for_node(temp, temp_offset);

                if REG_RESULT != dest {
                    emit::copy_reg(self.masm, field.ty.mode(), dest, REG_RESULT);
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
        let lbl_true = self.masm.create_label();
        let lbl_false = self.masm.create_label();
        let lbl_end = self.masm.create_label();

        self.emit_expr(&e.lhs, REG_RESULT);
        emit::test_and_jump_if(self.masm, CondCode::NonZero, REG_RESULT, lbl_true);

        self.emit_expr(&e.rhs, REG_RESULT);
        emit::test_and_jump_if(self.masm, CondCode::Zero, REG_RESULT, lbl_false);

        self.masm.bind_label(lbl_true);
        emit::load_true(self.masm, dest);
        emit::jump(self.masm, lbl_end);

        self.masm.bind_label(lbl_false);
        emit::load_false(self.masm, dest);

        self.masm.bind_label(lbl_end);
    }

    fn emit_bin_and(&mut self, e: &'ast ExprBinType, dest: Reg) {
        let lbl_true = self.masm.create_label();
        let lbl_false = self.masm.create_label();
        let lbl_end = self.masm.create_label();

        self.emit_expr(&e.lhs, REG_RESULT);
        emit::test_and_jump_if(self.masm, CondCode::Zero, REG_RESULT, lbl_false);

        self.emit_expr(&e.rhs, REG_RESULT);
        emit::test_and_jump_if(self.masm, CondCode::Zero, REG_RESULT, lbl_false);

        self.masm.bind_label(lbl_true);
        emit::load_true(self.masm, dest);
        emit::jump(self.masm, lbl_end);

        self.masm.bind_label(lbl_false);
        emit::load_false(self.masm, dest);

        self.masm.bind_label(lbl_end);
    }

    fn emit_bin_cmp(&mut self, e: &'ast ExprBinType, dest: Reg, op: CmpOp) {
        let lhs_type = e.lhs.ty();
        let rhs_type = e.rhs.ty();

        let cmp_type = lhs_type.if_nil(rhs_type);

        if op == CmpOp::Is || op == CmpOp::IsNot {
            let op = if op == CmpOp::Is { CondCode::Equal } else { CondCode::NotEqual };

            self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
                emit::cmp_reg(eg.masm, MachineMode::Ptr, lhs, rhs);
                emit::set(eg.masm, dest, op);

                dest
            });

            return;
        }

        if cmp_type == BuiltinType::Str {
            self.emit_universal_call(e.id, e.pos, dest);
            emit::load_int_const(self.masm, MachineMode::Ptr, REG_TMP1, 0);
            emit::cmp_reg(self.masm, MachineMode::Int32, REG_RESULT, REG_TMP1);
            emit::set(self.masm, dest, to_cond_code(op));

        } else {
            self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
                emit::cmp_reg(eg.masm, MachineMode::Int32, lhs, rhs);
                emit::set(eg.masm, dest, to_cond_code(op));

                dest
            });
        }
    }

    fn emit_bin_divmod(&mut self, e: &'ast ExprBinType, dest: Reg) {
        self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
            let lbl_div = eg.masm.create_label();
            emit::test_and_jump_if(eg.masm, CondCode::NonZero, rhs, lbl_div);
            trap::emit(eg.masm, trap::DIV0);

            eg.masm.bind_label(lbl_div);

            if e.op == BinOp::Div {
                emit::int_div(eg.masm, dest, lhs, rhs)
            } else {
                emit::int_mod(eg.masm, dest, lhs, rhs)
            }

            dest
        });
    }

    fn emit_bin_mul(&mut self, e: &'ast ExprBinType, dest: Reg) {
        self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
            emit::int_mul(eg.masm, dest, lhs, rhs);

            dest
        });
    }

    fn emit_bin_add(&mut self, e: &'ast ExprBinType, dest: Reg) {
        if self.has_call_site(e.id) {
            self.emit_universal_call(e.id, e.pos, dest);

        } else {
            self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
                emit::int_add(eg.masm, dest, lhs, rhs);

                dest
            });
        }
    }

    fn emit_bin_sub(&mut self, e: &'ast ExprBinType, dest: Reg) {
        self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
            emit::int_sub(eg.masm, dest, lhs, rhs);

            dest
        });
    }

    fn emit_bin_bit_or(&mut self, e: &'ast ExprBinType, dest: Reg) {
        self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
            emit::int_or(eg.masm, dest, lhs, rhs);

            dest
        });
    }

    fn emit_bin_bit_and(&mut self, e: &'ast ExprBinType, dest: Reg) {
        self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
            emit::int_and(eg.masm, dest, lhs, rhs);

            dest
        });
    }

    fn emit_bin_bit_xor(&mut self, e: &'ast ExprBinType, dest: Reg) {
        self.emit_binop(e, dest, |eg, lhs, rhs, dest| {
            emit::int_xor(eg.masm, dest, lhs, rhs);

            dest
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
            emit::store_mem(self.masm, ty.mode(), Mem::Local(offset), REG_RESULT);

            self.emit_expr(&e.rhs, rhs_reg);
            emit::load_mem(self.masm, ty.mode(), lhs_reg, Mem::Local(offset));

            self.free_temp_for_node(&e.lhs, offset);
        } else {
            self.emit_expr(&e.lhs, lhs_reg);
            self.emit_expr(&e.rhs, rhs_reg);
        }

        let ty = e.ty();
        let reg = emit_action(self, lhs_reg, rhs_reg, dest_reg);
        if reg != dest_reg { emit::copy_reg(self.masm, ty.mode(), dest_reg, reg); }
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
                }

                FctKind::Native(ptr) => {
                    ensure_native_stub(self.ctxt, fid, ptr.raw(), fct.return_type, fct.real_args())
                }

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
        emit::load_mem(self.masm, MachineMode::Ptr, dest, Mem::Base(REG_RESULT, Header::size()));
    }

    fn emit_intrinsic_assert(&mut self, e: &'ast ExprCallType, _: Reg) {
        let lbl_div = self.masm.create_label();
        self.emit_expr(&e.args[0], REG_RESULT);

        self.masm.emit_comment(Comment::Lit("check assert"));
        emit::test_and_jump_if(self.masm, CondCode::Zero, REG_RESULT, lbl_div);
        self.masm.emit_bailout(lbl_div, trap::ASSERT, e.pos);
    }

    fn emit_intrinsic_shl(&mut self, e: &'ast ExprCallType, dest: Reg) {
        self.emit_expr(&e.args[0], REG_RESULT);
        let offset = self.reserve_temp_for_node(&e.args[0]);
        emit::store_mem(self.masm, MachineMode::Int32, Mem::Local(offset), REG_RESULT);

        self.emit_expr(&e.args[1], REG_TMP1);
        emit::load_mem(self.masm, MachineMode::Int32, REG_RESULT, Mem::Local(offset));

        emit::int_shl(self.masm, dest, REG_RESULT, REG_TMP1);

        if REG_RESULT != dest {
            emit::copy_reg(self.masm, MachineMode::Int32, dest, REG_RESULT);
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
                    self.masm.emit_comment(Comment::Alloc(cls_id));

                    let cls = self.ctxt.cls_by_id(cls_id);
                    emit::load_int_const(self.masm, MachineMode::Int32, REG_PARAMS[0], cls.size);

                    let mptr = stdlib::gc_alloc as *mut u8;
                    self.emit_native_call_insn(mptr, pos, BuiltinType::Ptr, 1, dest);

                    // store classptr in object
                    let cptr = (&**cls.vtable.as_ref().unwrap()) as *const VTable as *const u8;
                    let disp = self.masm.add_addr(cptr);
                    let pos = self.masm.pos() as i32;

                    self.masm.emit_comment(Comment::StoreVTable(cls_id));
                    emit::load_constpool(self.masm, REG_TMP1, disp + pos);
                    emit::store_mem(self.masm, MachineMode::Ptr, Mem::Base(REG_RESULT, 0), REG_TMP1);
                }
            }

            let offset = self.reserve_temp_for_arg(arg);
            emit::store_mem(self.masm, arg.ty().mode(), Mem::Local(offset), REG_RESULT);
            temps.push((arg.ty(), offset));
        }

        let mut arg_offset = -self.src.stacksize();

        for (ind, arg) in csite.args.iter().enumerate() {
            let ty = arg.ty();
            let offset = temps[ind].1;

            if ind < REG_PARAMS.len() {
                let reg = REG_PARAMS[ind];
                emit::load_mem(self.masm, ty.mode(), reg, Mem::Local(offset));

                if ind == 0 {
                    let call_type = self.src.calls.get(&id);

                    if call_type.is_some() && call_type.unwrap().is_method()
                        && check_for_nil(ty) {
                        emit::test_if_nil_bailout(self.masm, pos, reg);
                    }
                }

            } else {
                emit::load_mem(self.masm, ty.mode(), REG_RESULT, Mem::Local(offset));
                emit::store_mem(self.masm, ty.mode(), Mem::Local(arg_offset), REG_RESULT);

                arg_offset += 8;
            }
        }

        match csite.callee {
            Callee::Fct(fid) => {
                let fct = self.ctxt.fct_by_id(fid);

                if csite.super_call {
                    let ptr = self.ptr_for_fct_id(fid);
                    self.masm.emit_comment(Comment::CallSuper(fid));
                    self.emit_direct_call_insn(ptr, pos, csite.return_type, dest);

                } else if fct.is_virtual() {
                    let vtable_index = fct.vtable_index.unwrap();
                    self.masm.emit_comment(Comment::CallVirtual(fid));
                    self.emit_indirect_call_insn(vtable_index, pos, csite.return_type, dest);

                } else {
                    let ptr = self.ptr_for_fct_id(fid);
                    self.masm.emit_comment(Comment::CallDirect(fid));
                    self.emit_direct_call_insn(ptr, pos, csite.return_type, dest);
                }
            }

            Callee::Ptr(ptr) => {
                self.emit_native_call_insn(ptr.raw(), pos, csite.return_type,
                                           csite.args.len() as i32, dest);
            }
        }

        if csite.args.len() > 0 {
            if let Arg::SelfieNew(_, _) = csite.args[0] {
                let (ty, offset) = temps[0];
                emit::load_mem(self.masm, ty.mode(), dest, Mem::Local(offset));
            }
        }

        for temp in temps.into_iter() {
            self.free_temp_with_type(temp.0, temp.1);
        }
    }

    fn emit_native_call_insn(&mut self, ptr: *const u8, pos: Position,
                             ty: BuiltinType, args: i32, dest: Reg) {
        let ptr = ensure_native_stub(self.ctxt, FctId(0), ptr, ty, args);
        self.emit_direct_call_insn(ptr, pos, ty, dest);
    }

    fn emit_direct_call_insn(&mut self, ptr: *const u8, pos: Position, ty: BuiltinType, dest: Reg) {
        emit::direct_call(self.masm, ptr);
        self.emit_after_call_insns(pos, ty, dest);
    }

    fn emit_indirect_call_insn(&mut self, index: u32, pos: Position, ty: BuiltinType, dest: Reg) {
        emit::indirect_call(&mut self.masm, index);
        self.emit_after_call_insns(pos, ty, dest);
    }

    fn emit_after_call_insns(&mut self, pos: Position, ty: BuiltinType, dest: Reg) {
        self.masm.emit_lineno(pos.line as i32);

        let gcpoint = codegen::create_gcpoint(self.scopes, &self.temps);
        self.masm.emit_gcpoint(gcpoint);

        if REG_RESULT != dest {
            emit::copy_reg(self.masm, ty.mode(), dest, REG_RESULT);
        }
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

fn ensure_native_stub(ctxt: &Context, fct_id: FctId, ptr: *const u8,
                      ty: BuiltinType, args: i32) -> *const u8 {
    let mut native_fcts = ctxt.native_fcts.lock().unwrap();

    if let Some(ptr) = native_fcts.find_fct(ptr) {
        ptr

    } else {
        let jit_fct = native::generate(ctxt, fct_id, ptr, ty, args);

        if ctxt.args.flag_emit_asm {
            dump_asm(ctxt, &jit_fct,
                ctxt.args.flag_asm_syntax.unwrap_or(AsmSyntax::Att));
        }

        native_fcts.insert_fct(ptr, jit_fct)
    }
}

fn ensure_jit_or_stub_ptr<'ast>(fid: FctId, src: &mut FctSrc<'ast>, ctxt: &Context) -> Ptr {
    if let Some(ref jit) = src.jit_fct { return jit.fct_ptr(); }
    if let Some(ref stub) = src.stub { return stub.ptr_start(); }

    let stub = Stub::new(fid);

    {
        let mut code_map = ctxt.code_map.lock().unwrap();
        code_map.insert(stub.ptr_start().raw(), stub.ptr_end().raw(), fid);
    }

    if ctxt.args.flag_emit_stubs {
        println!("create stub at {:x}", stub.ptr_start().raw() as usize);
    }

    let ptr = stub.ptr_start();

    src.stub = Some(stub);

    ptr
}

fn to_cond_code(cmp: CmpOp) -> CondCode {
    match cmp {
        CmpOp::Eq => CondCode::Equal,
        CmpOp::Ne => CondCode::NotEqual,
        CmpOp::Gt => CondCode::Greater,
        CmpOp::Ge => CondCode::GreaterEq,
        CmpOp::Lt => CondCode::Less,
        CmpOp::Le => CondCode::LessEq,
        CmpOp::Is => CondCode::Equal,
        CmpOp::IsNot => CondCode::NotEqual,
    }
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
        ExprTry(_) => false,
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
        ExprTry(ref e) => contains_fct_call(&e.expr),
    }
}
