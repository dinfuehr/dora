use libc::c_void;

use ast::*;
use ast::Expr::*;
use class::{self, ClassId, PropId};
use cpu::{self, Reg, REG_RESULT, REG_TMP1, REG_TMP2, REG_PARAMS};
use cpu::emit;
use cpu::trap;
use ctxt::*;
use jit::buffer::*;
use jit::codegen::{self, JumpCond, Scopes, TempOffsets};
use jit::stub::Stub;
use lexer::position::Position;
use mem::ptr::Ptr;
use object::{IntArray, Str};
use stdlib;
use ty::{BuiltinType, MachineMode};

pub struct ExprGen<'a, 'ast: 'a> {
    ctxt: &'a Context<'ast>,
    fct: &'a mut Fct<'ast>,
    ast: &'ast Function,
    buf: &'a mut Buffer,
    scopes: &'a mut Scopes,
    tempsize: i32,
    temps: TempOffsets,
}

impl<'a, 'ast> ExprGen<'a, 'ast> where 'ast: 'a {
    pub fn new(
        ctxt: &'a Context<'ast>,
        fct: &'a mut Fct<'ast>,
        ast: &'ast Function,
        buf: &'a mut Buffer,
        scopes: &'a mut Scopes,
    ) -> ExprGen<'a, 'ast> {
        ExprGen {
            ctxt: ctxt,
            fct: fct,
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
            ExprProp(ref expr) => self.emit_prop(expr, dest),
            ExprSelf(_) => self.emit_self(dest),
            ExprNil(_) => self.emit_nil(dest),
            ExprArray(ref expr) => self.emit_array(expr, dest),
        }

        dest
    }

    fn emit_array(&mut self, e: &'ast ExprArrayType, dest: Reg) {
        if self.is_intrinsic(e.id) {
            self.emit_expr(&e.object, REG_RESULT);
            let offset = self.reserve_temp_for_node(e.object.id());
            emit::mov_reg_local(self.buf, MachineMode::Ptr, REG_RESULT, offset);

            self.emit_expr(&e.index, REG_TMP1);
            emit::mov_local_reg(self.buf, MachineMode::Ptr, offset, REG_RESULT);
            emit::check_index_out_of_bounds(self.buf, e.pos, REG_RESULT, REG_TMP1, REG_TMP2);

            cpu::instr::emit_addq_imm_reg(self.buf, IntArray::offset_of_data(), REG_RESULT);
            emit::mov_array_reg(self.buf, MachineMode::Int32, REG_RESULT, REG_TMP1, 4, REG_RESULT);

            self.free_temp_for_node(e.object.id(), offset);

            if dest != REG_RESULT {
                emit::mov_reg_reg(self.buf, MachineMode::Int32, REG_RESULT, dest);
            }

        } else {
            self.emit_universal_call(e.id, e.pos, dest);
        }
    }

    fn reserve_temp_for_node(&mut self, id: NodeId) -> i32 {
        let offset = self.fct.src().get_store(id).offset();
        let ty = self.fct.src().get_type(id);

        let offset = -(self.fct.src().localsize + offset);

        if ty.reference_type() {
            self.temps.insert(offset);
        }

        offset
    }

    fn reserve_temp_for_arg(&mut self, arg: &Arg<'ast>) -> i32 {
        let offset = -(self.fct.src().localsize + arg.offset());
        let ty = arg.ty();

        if ty.reference_type() {
            self.temps.insert(offset);
        }

        offset
    }

    fn free_temp_for_node(&mut self, id: NodeId, offset: i32) {
        let ty = self.fct.src().get_type(id);

        if ty.reference_type() {
            self.temps.remove(offset);
        }
    }

    fn free_temp_with_type(&mut self, ty: BuiltinType, offset: i32) {
        if ty.reference_type() {
            self.temps.remove(offset);
        }
    }

    fn is_intrinsic(&self, id: NodeId) -> bool {
        let fid = self.fct.src().calls.get(&id).unwrap().fct_id();

        // the function we compile right now is never an intrinsic
        if self.fct.id == fid { return false; }

        self.ctxt.fct_by_id(fid, |fct| fct.kind.is_intrinsic())
    }

    fn emit_self(&mut self, dest: Reg) {
        let var = self.fct.var_self();

        emit::mov_local_reg(self.buf, var.ty.mode(), var.offset, dest);
    }

    fn emit_nil(&mut self, dest: Reg) {
        emit::nil(self.buf, dest);
    }

    fn emit_prop(&mut self, expr: &'ast ExprPropType, dest: Reg) {
        let (cls, field) = expr.cls_and_field();

        self.emit_expr(&expr.object, REG_RESULT);
        self.emit_prop_access(cls, field, REG_RESULT, dest);
    }

    fn emit_prop_access(&mut self, cls: ClassId, field: PropId, src: Reg, dest: Reg) {
        let cls = self.ctxt.cls_by_id(cls);
        let prop = &cls.props[field];
        emit::mov_mem_reg(self.buf, prop.ty.mode(), src, prop.offset, dest);
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

        let disp = self.buf.add_addr(handle.into());
        let pos = self.buf.pos() as i32;

        emit::movq_addr_reg(self.buf, disp + pos, dest);
    }

    fn emit_ident(&mut self, e: &'ast ExprIdentType, dest: Reg) {
        match e.ident_type() {
            IdentType::Var(_) => {
                codegen::var_load(self.buf, self.fct, e.var(), dest)
            }

            IdentType::Prop(cls, field) => {
                self.emit_self(REG_RESULT);
                self.emit_prop_access(cls, field, REG_RESULT, dest);
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
            if self.is_intrinsic(e.id) {
                let array = e.lhs.to_array().unwrap();
                self.emit_expr(&array.object, REG_RESULT);
                let offset_object = self.reserve_temp_for_node(array.object.id());
                emit::mov_reg_local(self.buf, MachineMode::Ptr, REG_RESULT, offset_object);

                self.emit_expr(&array.index, REG_RESULT);
                let offset_index = self.reserve_temp_for_node(array.index.id());
                emit::mov_reg_local(self.buf, MachineMode::Int32, REG_RESULT, offset_index);

                self.emit_expr(&e.rhs, REG_RESULT);
                let offset_value = self.reserve_temp_for_node(e.rhs.id());
                emit::mov_reg_local(self.buf, MachineMode::Int32, REG_RESULT, offset_value);

                emit::mov_local_reg(self.buf, MachineMode::Ptr, offset_object, REG_TMP1);
                emit::mov_local_reg(self.buf, MachineMode::Int32, offset_index, REG_TMP2);
                emit::check_index_out_of_bounds(self.buf, e.pos, REG_TMP1, REG_TMP2, REG_RESULT);

                emit::mov_local_reg(self.buf, MachineMode::Int32, offset_value, REG_RESULT);
                cpu::instr::emit_addq_imm_reg(self.buf, IntArray::offset_of_data(), REG_TMP1);
                emit::shiftlq_imm_reg(self.buf, 2, REG_TMP2);
                emit::addq_reg_reg(self.buf, REG_TMP2, REG_TMP1);
                emit::mov_reg_mem(self.buf, MachineMode::Int32, REG_RESULT, REG_TMP1, 0);

                self.free_temp_for_node(array.object.id(), offset_object);
                self.free_temp_for_node(array.index.id(), offset_index);
                self.free_temp_for_node(e.rhs.id(), offset_value);
            } else {
                self.emit_universal_call(e.id, e.pos, dest);
            }

            return;
        }

        let ident_type = if e.lhs.is_ident() {
            e.lhs.to_ident().unwrap().ident_type()

        } else if e.lhs.is_prop() {
            let (cls, field) = e.lhs.to_prop().unwrap().cls_and_field();

            IdentType::Prop(cls, field)

        } else {
            unreachable!();
        };

        match ident_type {
            IdentType::Var(_) => {
                self.emit_expr(&e.rhs, dest);
                let lhs = e.lhs.to_ident().unwrap();
                codegen::var_store(&mut self.buf, self.fct, dest, lhs.var());
            }

            IdentType::Prop(clsid, propid) => {
                let cls = self.ctxt.cls_by_id(clsid);
                let prop = &cls.props[propid.0];

                let temp_id = if let Some(expr_prop) = e.lhs.to_prop() {
                    self.emit_expr(&expr_prop.object, REG_RESULT);

                    expr_prop.object.id()

                } else {
                    self.emit_self(REG_RESULT);

                    e.lhs.id()
                };

                let temp_offset = self.reserve_temp_for_node(temp_id);
                emit::mov_reg_local(self.buf, MachineMode::Ptr, REG_RESULT, temp_offset);

                self.emit_expr(&e.rhs, REG_RESULT);
                emit::mov_local_reg(self.buf, MachineMode::Ptr, temp_offset, REG_TMP1);

                emit::mov_reg_mem(self.buf, prop.ty.mode(), REG_RESULT, REG_TMP1, prop.offset);
                self.free_temp_for_node(temp_id, temp_offset);

                if REG_RESULT != dest {
                    emit::mov_reg_reg(self.buf, prop.ty.mode(), REG_RESULT, dest);
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
        let lhs_type = *self.fct.src().types.get(&e.lhs.id()).unwrap();
        let rhs_type = *self.fct.src().types.get(&e.rhs.id()).unwrap();

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

        if let Some(&Store::Temp(_, _)) = self.fct.src().storage.get(&e.lhs.id()) {
            let offset = self.reserve_temp_for_node(e.lhs.id());
            let ty = self.fct.src().get_type(e.lhs.id());

            self.emit_expr(&e.lhs, REG_RESULT);
            emit::mov_reg_local(self.buf, ty.mode(), REG_RESULT, offset);

            self.emit_expr(&e.rhs, rhs_reg);
            emit::mov_local_reg(self.buf, ty.mode(), offset, lhs_reg);

            self.free_temp_for_node(e.lhs.id(), offset);
        } else {
            self.emit_expr(&e.lhs, lhs_reg);
            self.emit_expr(&e.rhs, rhs_reg);
        }

        let ty = self.fct.src().get_type(e.id);
        let reg = emit_action(self, lhs_reg, rhs_reg, dest_reg);
        if reg != dest_reg { emit::mov_reg_reg(self.buf, ty.mode(), reg, dest_reg); }
    }

    fn ptr_for_fct_id(&mut self, fid: FctId) -> Ptr {
        if self.fct.id == fid {
            // we want to recursively invoke the function we are compiling right now
            ensure_jit_or_stub_ptr(self.fct, self.ctxt)

        } else {
            self.ctxt.fct_by_id_mut(fid, |fct| {
                match fct.kind {
                    FctKind::Source(_) => ensure_jit_or_stub_ptr(fct, self.ctxt),
                    FctKind::Builtin(ptr) => ptr,
                    FctKind::Intrinsic => panic!("intrinsic fct call"),
                }
            })
        }
    }

    fn emit_call(&mut self, e: &'ast ExprCallType, dest: Reg) {
        if self.is_intrinsic(e.id) {
            // only intrinsic: IntArray.len()
            self.emit_expr(&e.args[0], REG_RESULT);
            emit::mov_mem_reg(self.buf, MachineMode::Ptr, REG_RESULT, 8, dest);

            return;
        }

        self.emit_universal_call(e.id, e.pos, dest);
    }

    fn has_call_site(&self, id: NodeId) -> bool {
        self.fct.src().call_sites.get(&id).is_some()
    }

    fn emit_universal_call(&mut self, id: NodeId, pos: Position, dest: Reg) {
        let csite = self.fct.src().call_sites.get(&id).unwrap().clone();
        let ptr = match csite.callee {
            Callee::Fct(fid) => self.ptr_for_fct_id(fid),
            Callee::Ptr(ptr) => ptr
        };
        let mut temps : Vec<(BuiltinType, i32)> = Vec::new();

        for (ind, arg) in csite.args.iter().enumerate() {
            match *arg {
                Arg::Expr(ast, _, _) => {
                    self.emit_expr(ast, REG_RESULT);
                }

                Arg::Selfie(cls_id, _) => {
                    // allocate storage for object
                    let cls = self.ctxt.cls_by_id(cls_id);
                    emit::movl_imm_reg(self.buf, cls.size as u32, REG_PARAMS[0]);

                    let mptr = Ptr::new(stdlib::gc_alloc as *mut c_void);
                    self.emit_call_insn(pos, mptr, BuiltinType::Ptr, REG_RESULT);

                    // store classptr in object
                    let cptr = (&*cls) as *const class::Class as usize;
                    let disp = self.buf.add_addr(cptr.into());
                    let pos = self.buf.pos() as i32;

                    emit::movq_addr_reg(self.buf, disp + pos, REG_TMP1);
                    emit::mov_reg_mem(self.buf, MachineMode::Ptr, REG_TMP1, REG_RESULT, 0);
                }
            }

            let offset = self.reserve_temp_for_arg(arg);
            emit::mov_reg_local(self.buf, arg.ty().mode(), REG_RESULT, offset);
            temps.push((arg.ty(), offset));
        }

        let mut arg_offset = -self.fct.src().stacksize();

        for (ind, arg) in csite.args.iter().enumerate() {
            let ty = arg.ty();
            let offset = temps[ind].1;

            if ind < REG_PARAMS.len() {
                let reg = REG_PARAMS[ind];
                emit::mov_local_reg(self.buf, ty.mode(), offset, reg);

                if ind == 0 {
                    let call_type = self.fct.src().calls.get(&id);

                    if call_type.is_some() && call_type.unwrap().is_method()
                        && check_for_nil(ty) {
                        emit::nil_ptr_check(self.buf, pos, reg);
                    }
                }

            } else {
                emit::mov_local_reg(self.buf, ty.mode(), offset, REG_RESULT);
                emit::mov_reg_local(self.buf, ty.mode(), REG_RESULT, arg_offset);

                arg_offset += 8;
            }
        }

        self.emit_call_insn(pos, ptr, csite.return_type, dest);

        for temp in temps.into_iter() {
            self.free_temp_with_type(temp.0, temp.1);
        }
    }

    fn emit_call_insn(&mut self, pos: Position, ptr: Ptr, ty: BuiltinType, dest: Reg) {
        let lineno = pos.line as i32;
        let disp = self.buf.add_addr(ptr);
        let pos = self.buf.pos() as i32;

        emit::movq_addr_reg(self.buf, disp + pos, REG_RESULT);
        emit::call(self.buf, REG_RESULT);
        self.buf.emit_lineno(lineno);

        let gcpoint = codegen::create_gcpoint(self.scopes, &self.temps);
        self.buf.emit_gcpoint(gcpoint);

        if REG_RESULT != dest {
            emit::mov_reg_reg(self.buf, ty.mode(), REG_RESULT, dest);
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

fn ensure_jit_or_stub_ptr<'ast>(fct: &mut Fct<'ast>, ctxt: &Context) -> Ptr {
    {
        let src = fct.src();

        if let Some(ref jit) = src.jit_fct { return jit.fct_ptr(); }
        if let Some(ref stub) = src.stub { return stub.ptr_start(); }
    }

    let stub = Stub::new(fct.id);

    {
        let mut code_map = ctxt.code_map.lock().unwrap();
        code_map.insert(stub.ptr_start(), stub.ptr_end(), fct.id);
    }

    if ctxt.args.flag_emit_stubs {
        println!("create stub at {:?}", stub.ptr_start());
    }

    let ptr = stub.ptr_start();

    fct.src_mut().stub = Some(stub);

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
        ExprSelf(_) => true,
        ExprNil(_) => true,
        ExprArray(_) => false,
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
        ExprSelf(_) => false,
        ExprNil(_) => false,
        ExprArray(ref e) => contains_fct_call(&e.object) || contains_fct_call(&e.index),
    }
}
