use baseline::asm::BaselineAssembler;
use baseline::codegen::{
    self, dump_asm, register_for_mode, should_emit_asm, should_emit_debug, CondCode, Scopes,
    TempOffsets,
};
use baseline::dora_native::{self, InternalFct, InternalFctDescriptor};
use baseline::fct::{CatchType, Comment, GcPoint};
use baseline::info::JitInfo;
use class::{ClassDefId, ClassSize, FieldId, TypeParams};
use cpu::{
    FReg, Mem, Reg, FREG_PARAMS, FREG_RESULT, FREG_TMP1, REG_PARAMS, REG_RESULT, REG_TMP1, REG_TMP2,
};
use ctxt::VM;
use ctxt::*;
use dora_parser::ast::Expr::*;
use dora_parser::ast::*;
use dora_parser::lexer::position::Position;
use dora_parser::lexer::token::{FloatSuffix, IntSuffix};
use driver::cmd::AsmSyntax;
use gc::Address;
use mem;
use object::{Header, Str};
use os::signal::Trap;
use semck::specialize::{specialize_class_id, specialize_class_ty};
use ty::{BuiltinType, MachineMode};
use vtable::{VTable, DISPLAY_SIZE};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ExprStore {
    Reg(Reg),
    FReg(FReg),
}

impl ExprStore {
    pub fn is_reg(&self) -> bool {
        match self {
            &ExprStore::Reg(_) => true,
            _ => false,
        }
    }

    pub fn is_freg(&self) -> bool {
        match self {
            &ExprStore::FReg(_) => true,
            _ => false,
        }
    }

    pub fn reg(&self) -> Reg {
        match self {
            &ExprStore::Reg(reg) => reg,
            _ => panic!("fp-register accessed as gp-register."),
        }
    }

    pub fn freg(&self) -> FReg {
        match self {
            &ExprStore::FReg(reg) => reg,
            _ => panic!("gp-register accessed as fp-register."),
        }
    }
}

impl From<Reg> for ExprStore {
    fn from(reg: Reg) -> ExprStore {
        ExprStore::Reg(reg)
    }
}

impl From<FReg> for ExprStore {
    fn from(reg: FReg) -> ExprStore {
        ExprStore::FReg(reg)
    }
}

pub struct ExprGen<'a, 'b, 'ast>
where
    'ast: 'a,
    'ast: 'b,
{
    vm: &'a VM<'ast>,
    fct: &'a Fct<'ast>,
    src: &'a mut FctSrc,
    ast: &'ast Function,
    asm: &'a mut BaselineAssembler<'b, 'ast>,
    scopes: &'a mut Scopes,
    tempsize: i32,
    temps: TempOffsets,
    jit_info: &'a JitInfo<'ast>,
    cls_type_params: &'a TypeParams,
    fct_type_params: &'a TypeParams,
}

impl<'a, 'b, 'ast> ExprGen<'a, 'b, 'ast>
where
    'ast: 'a,
    'ast: 'b,
{
    pub fn new(
        vm: &'a VM<'ast>,
        fct: &'a Fct<'ast>,
        src: &'a mut FctSrc,
        ast: &'ast Function,
        asm: &'a mut BaselineAssembler<'b, 'ast>,
        scopes: &'a mut Scopes,
        jit_info: &'a JitInfo<'ast>,
        cls_type_params: &'a TypeParams,
        fct_type_params: &'a TypeParams,
    ) -> ExprGen<'a, 'b, 'ast> {
        ExprGen {
            vm: vm,
            fct: fct,
            src: src,
            ast: ast,
            asm: asm,
            tempsize: 0,
            scopes: scopes,
            temps: TempOffsets::new(),
            jit_info: jit_info,
            cls_type_params: cls_type_params,
            fct_type_params: fct_type_params,
        }
    }

    pub fn generate(mut self, e: &'ast Expr, dest: ExprStore) {
        self.emit_expr(e, dest);

        if !self.temps.is_empty() {
            panic!("temporary variables are not fully freed!");
        }
    }

    fn emit_expr(&mut self, e: &'ast Expr, dest: ExprStore) {
        match *e {
            ExprLitChar(ref expr) => self.emit_lit_char(expr, dest.reg()),
            ExprLitInt(ref expr) => self.emit_lit_int(expr, dest.reg()),
            ExprLitFloat(ref expr) => self.emit_lit_float(expr, dest.freg()),
            ExprLitBool(ref expr) => self.emit_lit_bool(expr, dest.reg()),
            ExprLitStr(ref expr) => self.emit_lit_str(expr, dest.reg()),
            ExprLitStruct(ref expr) => self.emit_lit_struct(expr, dest),
            ExprUn(ref expr) => self.emit_un(expr, dest),
            ExprIdent(ref expr) => self.emit_ident(expr, dest),
            ExprAssign(ref expr) => self.emit_assign(expr),
            ExprBin(ref expr) => self.emit_bin(expr, dest),
            ExprCall(ref expr) => self.emit_call(expr, dest),
            ExprDelegation(ref expr) => self.emit_delegation(expr, dest),
            ExprField(ref expr) => self.emit_field(expr, dest),
            ExprSelf(_) => self.emit_self(dest.reg()),
            ExprSuper(_) => self.emit_self(dest.reg()),
            ExprNil(_) => self.emit_nil(dest.reg()),
            ExprArray(ref expr) => self.emit_array(expr, dest),
            ExprConv(ref expr) => self.emit_conv(expr, dest.reg()),
            ExprTry(ref expr) => self.emit_try(expr, dest),
            ExprLambda(_) => unimplemented!(),
        }
    }

    fn emit_try(&mut self, e: &'ast ExprTryType, dest: ExprStore) {
        match e.mode {
            TryMode::Normal => {
                self.emit_expr(&e.expr, dest);
            }

            TryMode::Else(ref alt_expr) => {
                let lbl_after = self.asm.create_label();

                let try_span = {
                    let start = self.asm.pos();
                    self.emit_expr(&e.expr, dest);
                    let end = self.asm.pos();

                    self.asm.jump(lbl_after);

                    (start, end)
                };

                let catch_span = {
                    let start = self.asm.pos();
                    self.emit_expr(alt_expr, dest);
                    let end = self.asm.pos();

                    (start, end)
                };

                self.asm
                    .emit_exception_handler(try_span, catch_span.0, None, CatchType::Any);
                self.asm.bind_label(lbl_after);
            }

            TryMode::Force => {
                let lbl_after = self.asm.create_label();

                let try_span = {
                    let start = self.asm.pos();
                    self.emit_expr(&e.expr, dest);
                    let end = self.asm.pos();

                    self.asm.jump(lbl_after);

                    (start, end)
                };

                let catch_span = {
                    let start = self.asm.pos();
                    self.asm.emit_bailout_inplace(Trap::UNEXPECTED, e.pos);
                    let end = self.asm.pos();

                    (start, end)
                };

                self.asm
                    .emit_exception_handler(try_span, catch_span.0, None, CatchType::Any);
                self.asm.bind_label(lbl_after);
            }

            TryMode::Opt => panic!("unsupported"),
        }
    }

    fn emit_conv(&mut self, e: &'ast ExprConvType, dest: Reg) {
        self.emit_expr(&e.object, dest.into());

        // return false if object is nil
        let lbl_nil = self.asm.test_if_nil(dest);
        let conv = *self.src.map_convs.get(e.id).unwrap();

        if conv.valid {
            if e.is {
                // return true for object is T
                self.asm.load_true(dest);
            } else {
                // do nothing for object as T
            }
        } else {
            let cls_id = conv.cls_id;
            let cls_id = specialize_class_id(self.vm, cls_id);
            let cls = self.vm.class_defs.idx(cls_id);
            let cls = cls.read();

            let vtable: &VTable = cls.vtable.as_ref().unwrap();

            let offset = if e.is {
                0
            } else {
                // reserve temp variable for object
                let offset = self.reserve_temp_for_node(&e.object);
                self.asm
                    .store_mem(MachineMode::Ptr, Mem::Local(offset), dest.into());

                offset
            };

            // object instanceof T

            // tmp1 = <vtable of object>
            self.asm
                .load_mem(MachineMode::Ptr, REG_TMP1.into(), Mem::Base(dest, 0));

            let disp = self.asm.add_addr(vtable as *const _ as *mut u8);
            let pos = self.asm.pos() as i32;

            // tmp2 = <vtable of T>
            self.asm.load_constpool(REG_TMP2, disp + pos);

            if vtable.subtype_depth >= DISPLAY_SIZE as i32 {
                // cmp [tmp1 + offset T.vtable.subtype_depth], tmp3
                self.asm.cmp_mem_imm(
                    MachineMode::Int32,
                    Mem::Base(REG_TMP1, VTable::offset_of_depth()),
                    vtable.subtype_depth,
                );

                // jnz lbl_false
                let lbl_false = self.asm.create_label();
                self.asm.jump_if(CondCode::Less, lbl_false);

                // tmp1 = tmp1.subtype_overflow
                self.asm.load_mem(
                    MachineMode::Ptr,
                    REG_TMP1.into(),
                    Mem::Base(REG_TMP1, VTable::offset_of_overflow()),
                );

                let overflow_offset =
                    mem::ptr_width() * (vtable.subtype_depth - DISPLAY_SIZE as i32);

                // cmp [tmp1 + 8*(vtable.subtype_depth - DISPLAY_SIZE) ], tmp2
                self.asm.cmp_mem(
                    MachineMode::Ptr,
                    Mem::Base(REG_TMP1, overflow_offset),
                    REG_TMP2,
                );

                if e.is {
                    // dest = if zero then true else false
                    self.asm.set(dest, CondCode::Equal);
                } else {
                    // jump to lbl_false if cmp did not succeed
                    self.asm.jump_if(CondCode::NonZero, lbl_false);

                    // otherwise load temp variable again
                    self.asm
                        .load_mem(MachineMode::Ptr, dest.into(), Mem::Local(offset));
                }

                // jmp lbl_finished
                let lbl_finished = self.asm.create_label();
                self.asm.jump(lbl_finished);

                // lbl_false:
                self.asm.bind_label(lbl_false);

                if e.is {
                    // dest = false
                    self.asm.load_false(dest);
                } else {
                    // bailout
                    self.asm.emit_bailout_inplace(Trap::CAST, e.pos);
                }

                // lbl_finished:
                self.asm.bind_label(lbl_finished);
            } else {
                let display_entry =
                    VTable::offset_of_display() + vtable.subtype_depth * mem::ptr_width();

                // tmp1 = vtable of object
                // tmp2 = vtable of T
                // cmp [tmp1 + offset], tmp2
                self.asm.cmp_mem(
                    MachineMode::Ptr,
                    Mem::Base(REG_TMP1, display_entry),
                    REG_TMP2,
                );

                if e.is {
                    self.asm.set(dest, CondCode::Equal);
                } else {
                    let lbl_bailout = self.asm.create_label();
                    self.asm.jump_if(CondCode::NotEqual, lbl_bailout);
                    self.asm.emit_bailout(lbl_bailout, Trap::CAST, e.pos);

                    self.asm
                        .load_mem(MachineMode::Ptr, dest.into(), Mem::Local(offset));
                }
            }

            if !e.is {
                self.free_temp_for_node(&e.object, offset);
            }
        }

        // lbl_nil:
        self.asm.bind_label(lbl_nil);

        // for is we are finished: dest is null which is boolean false
        // also for as we are finished: dest is null and stays null
    }

    fn emit_array(&mut self, e: &'ast ExprArrayType, dest: ExprStore) {
        if let Some(intrinsic) = self.intrinsic(e.id) {
            match intrinsic {
                Intrinsic::GenericArrayGet => {
                    let ty = self.ty(e.id);
                    self.emit_array_get(e.pos, ty.mode(), &e.object, &e.index, dest);
                }

                Intrinsic::StrGet => {
                    self.emit_array_get(e.pos, MachineMode::Int8, &e.object, &e.index, dest)
                }

                _ => panic!("unexpected intrinsic {:?}", intrinsic),
            }
        } else {
            self.emit_call_site_id(e.id, e.pos, dest);
        }
    }

    fn reserve_temp_for_node(&mut self, expr: &Expr) -> i32 {
        let id = expr.id();
        let ty = self.ty(id);
        let offset = -(self.jit_info.localsize + self.jit_info.get_store(id).offset());

        if ty.reference_type() {
            self.temps.insert(offset);
        }

        offset
    }

    fn reserve_temp_for_arg(&mut self, arg: &Arg<'ast>) -> i32 {
        let offset = -(self.jit_info.localsize + arg.offset());
        let ty = arg.ty();

        if ty.reference_type() {
            self.temps.insert(offset);
        }

        offset
    }

    fn reserve_temp_for_self(&mut self, arg: &Arg<'ast>) -> i32 {
        let offset = -(self.jit_info.localsize + arg.offset());

        offset
    }

    fn free_temp_for_node(&mut self, expr: &Expr, offset: i32) {
        let ty = self.ty(expr.id());

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
        self.jit_info.map_intrinsics.get(id).map(|&intr| intr)
    }

    fn emit_self(&mut self, dest: Reg) {
        let var = self.src.var_self();

        self.asm.emit_comment(Comment::LoadSelf(var.id));

        let offset = self.jit_info.offset(var.id);
        self.asm
            .load_mem(var.ty.mode(), dest.into(), Mem::Local(offset));
    }

    fn emit_nil(&mut self, dest: Reg) {
        self.asm.load_nil(dest);
    }

    fn emit_field(&mut self, expr: &'ast ExprFieldType, dest: ExprStore) {
        let (ty, field) = {
            let ident_type = self.src.map_idents.get(expr.id).unwrap();

            match ident_type {
                &IdentType::Field(ty, field) => (ty, field),
                _ => unreachable!(),
            }
        };

        let ty = self.specialize_type(ty);

        self.emit_expr(&expr.object, REG_RESULT.into());
        self.emit_field_access(expr.pos, ty, field, REG_RESULT, dest);
    }

    fn emit_field_access(
        &mut self,
        pos: Position,
        ty: BuiltinType,
        fieldid: FieldId,
        src: Reg,
        dest: ExprStore,
    ) {
        let cls_id = specialize_class_ty(self.vm, ty);
        let cls = self.vm.class_defs.idx(cls_id);
        let cls = cls.read();
        let field = &cls.fields[fieldid.idx()];

        self.asm.emit_comment(Comment::LoadField(cls_id, fieldid));
        self.asm
            .load_field(field.ty.mode(), dest, src, field.offset, pos.line as i32);
    }

    fn emit_lit_char(&mut self, lit: &'ast ExprLitCharType, dest: Reg) {
        self.asm
            .load_int_const(MachineMode::Int32, dest, lit.value as i64);
    }

    fn emit_lit_int(&mut self, lit: &'ast ExprLitIntType, dest: Reg) {
        let ty = match lit.suffix {
            IntSuffix::Byte => MachineMode::Int8,
            IntSuffix::Int => MachineMode::Int32,
            IntSuffix::Long => MachineMode::Int64,
        };

        self.asm.load_int_const(ty, dest, lit.value as i64);
    }

    fn emit_lit_float(&mut self, lit: &'ast ExprLitFloatType, dest: FReg) {
        let ty = match lit.suffix {
            FloatSuffix::Float => MachineMode::Float32,
            FloatSuffix::Double => MachineMode::Float64,
        };

        self.asm.load_float_const(ty, dest, lit.value);
    }

    fn emit_lit_bool(&mut self, lit: &'ast ExprLitBoolType, dest: Reg) {
        if lit.value {
            self.asm.load_true(dest);
        } else {
            self.asm.load_false(dest);
        };
    }

    fn emit_lit_str(&mut self, lit: &'ast ExprLitStrType, dest: Reg) {
        let handle = Str::from_buffer_in_perm(self.vm, lit.value.as_bytes());

        let disp = self.asm.add_addr(handle.raw() as *const u8);
        let pos = self.asm.pos() as i32;

        self.asm.emit_comment(Comment::LoadString(handle));
        self.asm.load_constpool(dest, disp + pos);
    }

    fn emit_lit_struct(&mut self, _: &'ast ExprLitStructType, _: ExprStore) {
        unimplemented!();
    }

    fn emit_ident(&mut self, e: &'ast ExprIdentType, dest: ExprStore) {
        let &ident = self.src.map_idents.get(e.id).unwrap();

        match ident {
            IdentType::Var(varid) => {
                self.asm.emit_comment(Comment::LoadVar(varid));
                self.asm.var_load(self.jit_info, varid, dest)
            }

            IdentType::Global(gid) => {
                let glob = self.vm.globals.idx(gid);
                let glob = glob.lock();

                let disp = self.asm.add_addr(glob.address_value.to_ptr());
                let pos = self.asm.pos() as i32;

                self.asm.emit_comment(Comment::LoadGlobal(gid));
                self.asm.load_constpool(REG_TMP1, disp + pos);

                self.asm
                    .load_mem(glob.ty.mode(), dest, Mem::Base(REG_TMP1, 0));
            }

            IdentType::Field(cls, field) => {
                self.emit_self(REG_RESULT);
                self.emit_field_access(e.pos, cls, field, REG_RESULT, dest);
            }

            IdentType::Struct(_) => {
                unimplemented!();
            }

            IdentType::Const(const_id) => {
                self.emit_const(const_id, dest);
            }
        }
    }

    fn emit_const(&mut self, const_id: ConstId, dest: ExprStore) {
        let xconst = self.vm.consts.idx(const_id);
        let xconst = xconst.lock();
        let ty = xconst.ty;

        match ty {
            BuiltinType::Bool => {
                if xconst.value.to_bool() {
                    self.asm.load_true(dest.reg());
                } else {
                    self.asm.load_false(dest.reg());
                }
            }

            BuiltinType::Char => {
                self.asm.load_int_const(
                    MachineMode::Int32,
                    dest.reg(),
                    xconst.value.to_char() as i64,
                );
            }

            BuiltinType::Byte | BuiltinType::Int | BuiltinType::Long => {
                self.asm
                    .load_int_const(ty.mode(), dest.reg(), xconst.value.to_int());
            }

            BuiltinType::Float | BuiltinType::Double => {
                self.asm
                    .load_float_const(ty.mode(), dest.freg(), xconst.value.to_float());
            }

            _ => unimplemented!(),
        }
    }

    fn emit_un(&mut self, e: &'ast ExprUnType, dest: ExprStore) {
        if let Some(intrinsic) = self.intrinsic(e.id) {
            self.emit_expr(&e.opnd, dest);

            match intrinsic {
                Intrinsic::IntPlus
                | Intrinsic::LongPlus
                | Intrinsic::FloatPlus
                | Intrinsic::DoublePlus => {}

                Intrinsic::IntNeg | Intrinsic::LongNeg => {
                    let dest = dest.reg();

                    let mode = if intrinsic == Intrinsic::IntNeg {
                        MachineMode::Int32
                    } else {
                        MachineMode::Int64
                    };

                    self.asm.int_neg(mode, dest, dest);
                }

                Intrinsic::FloatNeg | Intrinsic::DoubleNeg => {
                    let dest = dest.freg();

                    let mode = if intrinsic == Intrinsic::FloatNeg {
                        MachineMode::Float32
                    } else {
                        MachineMode::Float64
                    };

                    self.asm.float_neg(mode, dest, dest);
                }

                Intrinsic::ByteNot => {
                    let dest = dest.reg();
                    self.asm.int_not(MachineMode::Int8, dest, dest)
                }

                Intrinsic::IntNot | Intrinsic::LongNot => {
                    let dest = dest.reg();

                    let mode = if intrinsic == Intrinsic::IntNot {
                        MachineMode::Int32
                    } else {
                        MachineMode::Int64
                    };

                    self.asm.int_not(mode, dest, dest);
                }

                Intrinsic::BoolNot => {
                    let dest = dest.reg();
                    self.asm.bool_not(dest, dest)
                }

                _ => panic!("unexpected intrinsic {:?}", intrinsic),
            }
        } else {
            self.emit_call_site_id(e.id, e.pos, dest);
        }
    }

    fn emit_assign(&mut self, e: &'ast ExprAssignType) {
        if e.lhs.is_array() {
            let array = e.lhs.to_array().unwrap();

            if let Some(intrinsic) = self.intrinsic(e.id) {
                match intrinsic {
                    Intrinsic::GenericArraySet => {
                        let ty = self.ty(array.id);
                        self.emit_array_set(
                            e.pos,
                            ty,
                            ty.mode(),
                            &array.object,
                            &array.index,
                            &e.rhs,
                        )
                    }

                    Intrinsic::StrSet => self.emit_array_set(
                        e.pos,
                        BuiltinType::Byte,
                        MachineMode::Int8,
                        &array.object,
                        &array.index,
                        &e.rhs,
                    ),

                    _ => panic!("unexpected intrinsic {:?}", intrinsic),
                }
            } else {
                self.emit_call_site_id(e.id, e.pos, REG_RESULT.into());
            }

            return;
        }

        let &ident_type = self.src.map_idents.get(e.lhs.id()).unwrap();

        match ident_type {
            IdentType::Var(varid) => {
                let ty = self.jit_info.ty(varid);
                let dest = result_reg(ty.mode());
                self.emit_expr(&e.rhs, dest);

                self.asm.emit_comment(Comment::StoreVar(varid));
                self.asm.var_store(self.jit_info, dest, varid);
            }

            IdentType::Global(gid) => {
                let glob = self.vm.globals.idx(gid);
                let (address_value, ty) = {
                    let glob = glob.lock();
                    (glob.address_value, glob.ty)
                };

                let dest = result_reg(ty.mode());
                self.emit_expr(&e.rhs, dest);

                let disp = self.asm.add_addr(address_value.to_ptr());
                let pos = self.asm.pos() as i32;

                self.asm.emit_comment(Comment::StoreGlobal(gid));
                self.asm.load_constpool(REG_TMP1, disp + pos);

                self.asm.store_mem(ty.mode(), Mem::Base(REG_TMP1, 0), dest);
            }

            IdentType::Field(ty, fieldid) => {
                let ty = self.specialize_type(ty);
                let cls_id = specialize_class_ty(self.vm, ty);
                let cls = self.vm.class_defs.idx(cls_id);
                let cls = cls.read();
                let field = &cls.fields[fieldid.idx()];

                let temp = if let Some(expr_field) = e.lhs.to_field() {
                    self.emit_expr(&expr_field.object, REG_RESULT.into());

                    &expr_field.object
                } else {
                    self.emit_self(REG_RESULT);

                    &e.lhs
                };

                let temp_offset = self.reserve_temp_for_node(temp);
                self.asm
                    .store_mem(MachineMode::Ptr, Mem::Local(temp_offset), REG_RESULT.into());

                let reg = result_reg(field.ty.mode());
                self.emit_expr(&e.rhs, reg);
                self.asm
                    .load_mem(MachineMode::Ptr, REG_TMP1.into(), Mem::Local(temp_offset));

                self.asm.emit_comment(Comment::StoreField(cls_id, fieldid));

                let write_barrier = self.vm.gc.needs_write_barrier() && field.ty.reference_type();
                let card_table_offset = self.vm.gc.card_table_offset();

                self.asm.store_field(
                    field.ty.mode(),
                    REG_TMP1,
                    field.offset,
                    reg,
                    e.pos.line as i32,
                    write_barrier,
                    card_table_offset,
                );
                self.free_temp_for_node(temp, temp_offset);
            }

            IdentType::Struct(_) => {
                unimplemented!();
            }

            IdentType::Const(_) => {
                unreachable!();
            }
        }
    }

    fn emit_bin(&mut self, e: &'ast ExprBinType, dest: ExprStore) {
        if let Some(intrinsic) = self.intrinsic(e.id) {
            self.emit_intrinsic_bin(&e.lhs, &e.rhs, dest, intrinsic, Some(e.op));
        } else if e.op == BinOp::Cmp(CmpOp::Is) || e.op == BinOp::Cmp(CmpOp::IsNot) {
            self.emit_bin_is(e, dest.reg());
        } else if e.op == BinOp::Or {
            self.emit_bin_or(e, dest.reg());
        } else if e.op == BinOp::And {
            self.emit_bin_and(e, dest.reg());
        } else {
            self.emit_call_site_id(e.id, e.pos, dest);

            match e.op {
                BinOp::Cmp(CmpOp::Eq) => {}
                BinOp::Cmp(CmpOp::Ne) => {
                    let dest = dest.reg();
                    self.asm.bool_not(dest, dest);
                }

                BinOp::Cmp(op) => {
                    let dest = dest.reg();

                    let temp = if dest == REG_RESULT {
                        REG_TMP1
                    } else {
                        REG_RESULT
                    };

                    self.asm.load_int_const(MachineMode::Int32, temp, 0);
                    self.asm.cmp_reg(MachineMode::Int32, dest, temp);
                    self.asm.set(dest, to_cond_code(op));
                }
                _ => {}
            }
        }
    }

    fn emit_bin_is(&mut self, e: &'ast ExprBinType, dest: Reg) {
        let builtin_type = self.ty(e.lhs.id());
        let dest_mode = match builtin_type {
            BuiltinType::Nil=> MachineMode::Ptr,
            BuiltinType::Float => MachineMode::Int32,
            BuiltinType::Double => MachineMode::Int64,
            _ => builtin_type.mode()
        };
        let offset;
        if builtin_type.is_float() {
            let src_mode = builtin_type.mode();

            self.emit_expr(&e.lhs, FREG_RESULT.into());
            self.asm.float_as_int(dest_mode, REG_RESULT, src_mode, FREG_RESULT);
            offset = self.reserve_temp_for_node(&e.lhs);
            self.asm
                .store_mem(dest_mode, Mem::Local(offset), REG_RESULT.into());

            self.emit_expr(&e.rhs, FREG_RESULT.into());
            self.asm.float_as_int(dest_mode, REG_TMP1, src_mode, FREG_RESULT);
        } else {
            self.emit_expr(&e.lhs, REG_RESULT.into());
            offset = self.reserve_temp_for_node(&e.lhs);
            self.asm
                .store_mem(dest_mode, Mem::Local(offset), REG_RESULT.into());

            self.emit_expr(&e.rhs, REG_TMP1.into());
        }

        self.asm
            .load_mem(dest_mode, REG_RESULT.into(), Mem::Local(offset));
        self.asm.cmp_reg(dest_mode, REG_RESULT, REG_TMP1);

        let op = match e.op {
            BinOp::Cmp(CmpOp::Is) => CondCode::Equal,
            _ => CondCode::NotEqual,
        };

        self.asm.set(dest, op);
        self.free_temp_for_node(&e.lhs, offset);
    }

    fn emit_bin_or(&mut self, e: &'ast ExprBinType, dest: Reg) {
        let lbl_true = self.asm.create_label();
        let lbl_false = self.asm.create_label();
        let lbl_end = self.asm.create_label();

        self.emit_expr(&e.lhs, REG_RESULT.into());
        self.asm
            .test_and_jump_if(CondCode::NonZero, REG_RESULT, lbl_true);

        self.emit_expr(&e.rhs, REG_RESULT.into());
        self.asm
            .test_and_jump_if(CondCode::Zero, REG_RESULT, lbl_false);

        self.asm.bind_label(lbl_true);
        self.asm.load_true(dest);
        self.asm.jump(lbl_end);

        self.asm.bind_label(lbl_false);
        self.asm.load_false(dest);

        self.asm.bind_label(lbl_end);
    }

    fn emit_bin_and(&mut self, e: &'ast ExprBinType, dest: Reg) {
        let lbl_true = self.asm.create_label();
        let lbl_false = self.asm.create_label();
        let lbl_end = self.asm.create_label();

        self.emit_expr(&e.lhs, REG_RESULT.into());
        self.asm
            .test_and_jump_if(CondCode::Zero, REG_RESULT, lbl_false);

        self.emit_expr(&e.rhs, REG_RESULT.into());
        self.asm
            .test_and_jump_if(CondCode::Zero, REG_RESULT, lbl_false);

        self.asm.bind_label(lbl_true);
        self.asm.load_true(dest);
        self.asm.jump(lbl_end);

        self.asm.bind_label(lbl_false);
        self.asm.load_false(dest);

        self.asm.bind_label(lbl_end);
    }

    fn ptr_for_fct_id(
        &mut self,
        fid: FctId,
        cls_type_params: TypeParams,
        fct_type_params: TypeParams,
    ) -> Address {
        if self.fct.id == fid {
            // we want to recursively invoke the function we are compiling right now
            ensure_jit_or_stub_ptr(self.src, self.vm, cls_type_params, fct_type_params)
        } else {
            let fct = self.vm.fcts.idx(fid);
            let fct = fct.read();

            match fct.kind {
                FctKind::Source(_) => {
                    let src = fct.src();
                    let mut src = src.write();

                    ensure_jit_or_stub_ptr(&mut src, self.vm, cls_type_params, fct_type_params)
                }

                FctKind::Native(ptr) => {
                    let internal_fct = InternalFct {
                        ptr: ptr,
                        args: fct.params_with_self(),
                        return_type: fct.return_type,
                        throws: fct.ast.throws,
                        desc: InternalFctDescriptor::NativeThunk(fid),
                    };

                    ensure_native_stub(self.vm, fid, internal_fct)
                }

                FctKind::Definition => panic!("prototype for fct call"),
                FctKind::Builtin(_) => panic!("intrinsic fct call"),
            }
        }
    }

    fn emit_call(&mut self, e: &'ast ExprCallType, dest: ExprStore) {
        if let Some(intrinsic) = self.intrinsic(e.id) {
            match intrinsic {
                Intrinsic::GenericArrayLen => self.emit_intrinsic_len(e, dest.reg()),
                Intrinsic::Assert => self.emit_intrinsic_assert(e, dest.reg()),
                Intrinsic::Debug => self.emit_intrinsic_debug(),
                Intrinsic::Shl => self.emit_intrinsic_shl(e, dest.reg()),
                Intrinsic::SetUint8 => self.emit_set_uint8(e, dest.reg()),
                Intrinsic::StrLen => self.emit_intrinsic_len(e, dest.reg()),
                Intrinsic::StrGet => self.emit_array_get(
                    e.pos,
                    MachineMode::Int8,
                    e.object.as_ref().unwrap(),
                    &e.args[0],
                    dest,
                ),

                Intrinsic::BoolToInt | Intrinsic::ByteToInt => {
                    self.emit_intrinsic_byte_to_int(e, dest.reg())
                }
                Intrinsic::BoolToLong | Intrinsic::ByteToLong => {
                    self.emit_intrinsic_byte_to_long(e, dest.reg())
                }
                Intrinsic::LongToByte => self.emit_intrinsic_long_to_byte(e, dest.reg()),
                Intrinsic::LongToChar | Intrinsic::LongToInt => {
                    self.emit_intrinsic_long_to_int(e, dest.reg())
                }
                Intrinsic::LongToFloat => {
                    self.emit_intrinsic_int_to_float(e, dest.freg(), intrinsic)
                }
                Intrinsic::LongToDouble => {
                    self.emit_intrinsic_int_to_float(e, dest.freg(), intrinsic)
                }

                Intrinsic::LongAsDouble => {
                    self.emit_intrinsic_int_as_float(e, dest.freg(), intrinsic)
                }

                Intrinsic::FloatToInt => self.emit_intrinsic_float_to_int(e, dest.reg(), intrinsic),
                Intrinsic::FloatToLong => {
                    self.emit_intrinsic_float_to_int(e, dest.reg(), intrinsic)
                }
                Intrinsic::FloatToDouble => self.emit_intrinsic_float_to_double(e, dest.freg()),
                Intrinsic::FloatAsInt => self.emit_intrinsic_float_as_int(e, dest.reg(), intrinsic),

                Intrinsic::DoubleToInt => {
                    self.emit_intrinsic_float_to_int(e, dest.reg(), intrinsic)
                }
                Intrinsic::DoubleToLong => {
                    self.emit_intrinsic_float_to_int(e, dest.reg(), intrinsic)
                }
                Intrinsic::DoubleToFloat => self.emit_intrinsic_double_to_float(e, dest.freg()),
                Intrinsic::DoubleAsLong => self.emit_intrinsic_float_as_int(e, dest.reg(), intrinsic),

                Intrinsic::CharToInt | Intrinsic::IntToChar => {
                    self.emit_expr(e.object.as_ref().unwrap(), dest);
                }

                Intrinsic::CharToLong => self.emit_intrinsic_int_to_long(e, dest.reg()),
                Intrinsic::CharEq => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::CharCmp => self.emit_intrinsic_bin_call(e, dest, intrinsic),

                Intrinsic::IntToByte => self.emit_intrinsic_int_to_byte(e, dest.reg()),
                Intrinsic::IntToLong => self.emit_intrinsic_int_to_long(e, dest.reg()),
                Intrinsic::IntToFloat => {
                    self.emit_intrinsic_int_to_float(e, dest.freg(), intrinsic)
                }
                Intrinsic::IntToDouble => {
                    self.emit_intrinsic_int_to_float(e, dest.freg(), intrinsic)
                }

                Intrinsic::IntAsFloat => {
                    self.emit_intrinsic_int_as_float(e, dest.freg(), intrinsic)
                }

                Intrinsic::ByteEq => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::ByteCmp => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::ByteNot => self.emit_intrinsic_bin_call(e, dest, intrinsic),

                Intrinsic::BoolEq => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::BoolNot => self.emit_intrinsic_bin_call(e, dest, intrinsic),

                Intrinsic::IntEq => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::IntCmp => self.emit_intrinsic_bin_call(e, dest, intrinsic),

                Intrinsic::IntAdd => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::IntSub => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::IntMul => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::IntDiv => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::IntMod => self.emit_intrinsic_bin_call(e, dest, intrinsic),

                Intrinsic::IntOr => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::IntAnd => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::IntXor => self.emit_intrinsic_bin_call(e, dest, intrinsic),

                Intrinsic::IntShl => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::IntSar => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::IntShr => self.emit_intrinsic_bin_call(e, dest, intrinsic),

                Intrinsic::LongEq => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::LongCmp => self.emit_intrinsic_bin_call(e, dest, intrinsic),

                Intrinsic::LongAdd => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::LongSub => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::LongMul => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::LongDiv => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::LongMod => self.emit_intrinsic_bin_call(e, dest, intrinsic),

                Intrinsic::LongOr => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::LongAnd => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::LongXor => self.emit_intrinsic_bin_call(e, dest, intrinsic),

                Intrinsic::LongShl => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::LongSar => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::LongShr => self.emit_intrinsic_bin_call(e, dest, intrinsic),

                Intrinsic::FloatAdd => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::FloatSub => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::FloatMul => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::FloatDiv => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::FloatIsNan => self.emit_intrinsic_is_nan(e, dest.reg(), intrinsic),
                Intrinsic::FloatSqrt => self.emit_intrinsic_sqrt(e, dest.freg(), intrinsic),

                Intrinsic::DoubleAdd => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::DoubleSub => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::DoubleMul => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::DoubleDiv => self.emit_intrinsic_bin_call(e, dest, intrinsic),
                Intrinsic::DoubleIsNan => self.emit_intrinsic_is_nan(e, dest.reg(), intrinsic),
                Intrinsic::DoubleSqrt => self.emit_intrinsic_sqrt(e, dest.freg(), intrinsic),

                Intrinsic::DefaultValue => self.emit_intrinsic_default_value(e, dest),

                _ => panic!("unknown intrinsic {:?}", intrinsic),
            }
        } else {
            self.emit_call_site_id(e.id, e.pos, dest);
        }
    }

    fn emit_intrinsic_default_value(&mut self, e: &'ast ExprCallType, dest: ExprStore) {
        let ty = self.ty(e.id);

        match ty {
            BuiltinType::Bool
            | BuiltinType::Byte
            | BuiltinType::Int
            | BuiltinType::Long
            | BuiltinType::Char => self.asm.load_int_const(ty.mode(), dest.reg(), 0),
            BuiltinType::Float | BuiltinType::Double => {
                self.asm.load_float_const(ty.mode(), dest.freg(), 0.0)
            }
            _ => self.asm.load_nil(dest.reg()),
        }
    }

    fn emit_intrinsic_sqrt(&mut self, e: &'ast ExprCallType, dest: FReg, intrinsic: Intrinsic) {
        self.emit_expr(e.object.as_ref().unwrap(), dest.into());

        let mode = match intrinsic {
            Intrinsic::FloatSqrt => MachineMode::Float32,
            Intrinsic::DoubleSqrt => MachineMode::Float64,
            _ => unreachable!(),
        };

        self.asm.float_sqrt(mode, dest, dest);
    }

    fn emit_array_set(
        &mut self,
        pos: Position,
        element_type: BuiltinType,
        mode: MachineMode,
        object: &'ast Expr,
        index: &'ast Expr,
        rhs: &'ast Expr,
    ) {
        self.emit_expr(object, REG_RESULT.into());
        let offset_object = self.reserve_temp_for_node(object);
        self.asm.store_mem(
            MachineMode::Ptr,
            Mem::Local(offset_object),
            REG_RESULT.into(),
        );

        self.emit_expr(index, REG_RESULT.into());
        let offset_index = self.reserve_temp_for_node(index);
        self.asm.store_mem(
            MachineMode::Int32,
            Mem::Local(offset_index),
            REG_RESULT.into(),
        );

        let res = result_reg(mode);

        self.emit_expr(rhs, res);
        let offset_value = self.reserve_temp_for_node(rhs);
        self.asm.store_mem(mode, Mem::Local(offset_value), res);

        self.asm
            .load_mem(MachineMode::Ptr, REG_TMP1.into(), Mem::Local(offset_object));
        self.asm.load_mem(
            MachineMode::Int32,
            REG_TMP2.into(),
            Mem::Local(offset_index),
        );

        self.asm.test_if_nil_bailout(pos, REG_TMP1, Trap::NIL);

        if !self.vm.args.flag_omit_bounds_check {
            self.asm.check_index_out_of_bounds(pos, REG_TMP1, REG_TMP2);
        }

        self.asm.load_mem(mode, res, Mem::Local(offset_value));

        let write_barrier = self.vm.gc.needs_write_barrier() && element_type.reference_type();
        let card_table_offset = self.vm.gc.card_table_offset();

        self.asm.store_array_elem(
            mode,
            REG_TMP1,
            REG_TMP2,
            res,
            write_barrier,
            card_table_offset,
        );

        self.free_temp_for_node(object, offset_object);
        self.free_temp_for_node(index, offset_index);
        self.free_temp_for_node(rhs, offset_value);
    }

    fn emit_array_get(
        &mut self,
        pos: Position,
        mode: MachineMode,
        object: &'ast Expr,
        index: &'ast Expr,
        dest: ExprStore,
    ) {
        self.emit_expr(object, REG_RESULT.into());
        let offset = self.reserve_temp_for_node(object);
        self.asm
            .store_mem(MachineMode::Ptr, Mem::Local(offset), REG_RESULT.into());

        self.emit_expr(index, REG_TMP1.into());
        self.asm
            .load_mem(MachineMode::Ptr, REG_RESULT.into(), Mem::Local(offset));

        self.asm.test_if_nil_bailout(pos, REG_RESULT, Trap::NIL);

        if !self.vm.args.flag_omit_bounds_check {
            self.asm
                .check_index_out_of_bounds(pos, REG_RESULT, REG_TMP1);
        }

        let res = result_reg(mode);

        self.asm.load_array_elem(mode, res, REG_RESULT, REG_TMP1);

        self.free_temp_for_node(object, offset);

        if dest != res {
            self.asm.copy(mode, dest, res);
        }
    }

    fn emit_set_uint8(&mut self, e: &'ast ExprCallType, _: Reg) {
        self.emit_expr(&e.args[0], REG_RESULT.into());
        let offset = self.reserve_temp_for_node(&e.args[0]);
        self.asm
            .store_mem(MachineMode::Int64, Mem::Local(offset), REG_RESULT.into());

        self.emit_expr(&e.args[1], REG_TMP1.into());
        self.asm
            .load_mem(MachineMode::Int64, REG_RESULT.into(), Mem::Local(offset));

        self.asm
            .store_mem(MachineMode::Int8, Mem::Base(REG_RESULT, 0), REG_TMP1.into());
    }

    fn emit_intrinsic_is_nan(&mut self, e: &'ast ExprCallType, dest: Reg, intrinsic: Intrinsic) {
        self.emit_expr(&e.object.as_ref().unwrap(), FREG_RESULT.into());

        let mode = match intrinsic {
            Intrinsic::FloatIsNan => MachineMode::Float32,
            Intrinsic::DoubleIsNan => MachineMode::Float64,
            _ => unreachable!(),
        };

        self.asm.float_cmp_nan(mode, dest, FREG_RESULT);
    }

    fn emit_intrinsic_len(&mut self, e: &'ast ExprCallType, dest: Reg) {
        self.emit_expr(&e.object.as_ref().unwrap(), REG_RESULT.into());
        self.asm.test_if_nil_bailout(e.pos, REG_RESULT, Trap::NIL);
        self.asm.load_mem(
            MachineMode::Ptr,
            dest.into(),
            Mem::Base(REG_RESULT, Header::size()),
        );
    }

    fn emit_intrinsic_assert(&mut self, e: &'ast ExprCallType, _: Reg) {
        let lbl_div = self.asm.create_label();
        self.emit_expr(&e.args[0], REG_RESULT.into());

        self.asm.emit_comment(Comment::Lit("check assert"));
        self.asm
            .test_and_jump_if(CondCode::Zero, REG_RESULT, lbl_div);
        self.asm.emit_bailout(lbl_div, Trap::ASSERT, e.pos);
    }

    fn emit_intrinsic_debug(&mut self) {
        self.asm.debug();
    }

    fn emit_intrinsic_shl(&mut self, e: &'ast ExprCallType, dest: Reg) {
        self.emit_expr(&e.args[0], REG_RESULT.into());
        let offset = self.reserve_temp_for_node(&e.args[0]);
        self.asm
            .store_mem(MachineMode::Int32, Mem::Local(offset), REG_RESULT.into());

        self.emit_expr(&e.args[1], REG_TMP1.into());
        self.asm
            .load_mem(MachineMode::Int32, REG_RESULT.into(), Mem::Local(offset));

        self.asm
            .int_shl(MachineMode::Int32, dest, REG_RESULT, REG_TMP1);
    }

    fn emit_intrinsic_long_to_int(&mut self, e: &'ast ExprCallType, dest: Reg) {
        self.emit_expr(e.object.as_ref().unwrap(), dest.into());
    }

    fn emit_intrinsic_long_to_byte(&mut self, e: &'ast ExprCallType, dest: Reg) {
        self.emit_expr(e.object.as_ref().unwrap(), dest.into());
        self.asm.extend_byte(MachineMode::Int32, dest, dest);
    }

    fn emit_intrinsic_int_to_byte(&mut self, e: &'ast ExprCallType, dest: Reg) {
        self.emit_expr(e.object.as_ref().unwrap(), dest.into());
        self.asm.extend_byte(MachineMode::Int32, dest, dest);
    }

    fn emit_intrinsic_int_to_long(&mut self, e: &'ast ExprCallType, dest: Reg) {
        self.emit_expr(e.object.as_ref().unwrap(), REG_RESULT.into());
        self.asm.extend_int_long(dest, REG_RESULT);
    }

    fn emit_intrinsic_float_to_double(&mut self, e: &'ast ExprCallType, dest: FReg) {
        self.emit_expr(e.object.as_ref().unwrap(), FREG_RESULT.into());
        self.asm.float_to_double(dest, FREG_RESULT);
    }

    fn emit_intrinsic_double_to_float(&mut self, e: &'ast ExprCallType, dest: FReg) {
        self.emit_expr(e.object.as_ref().unwrap(), FREG_RESULT.into());
        self.asm.double_to_float(dest, FREG_RESULT);
    }

    fn emit_intrinsic_int_to_float(
        &mut self,
        e: &'ast ExprCallType,
        dest: FReg,
        intrinsic: Intrinsic,
    ) {
        self.emit_expr(e.object.as_ref().unwrap(), REG_RESULT.into());

        let (src_mode, dest_mode) = match intrinsic {
            Intrinsic::IntToFloat => (MachineMode::Int32, MachineMode::Float32),
            Intrinsic::IntToDouble => (MachineMode::Int32, MachineMode::Float64),
            Intrinsic::LongToFloat => (MachineMode::Int64, MachineMode::Float32),
            Intrinsic::LongToDouble => (MachineMode::Int64, MachineMode::Float64),
            _ => unreachable!(),
        };

        self.asm.int_to_float(dest_mode, dest, src_mode, REG_RESULT);
    }

    fn emit_intrinsic_float_to_int(
        &mut self,
        e: &'ast ExprCallType,
        dest: Reg,
        intrinsic: Intrinsic,
    ) {
        self.emit_expr(e.object.as_ref().unwrap(), FREG_RESULT.into());

        let (src_mode, dest_mode) = match intrinsic {
            Intrinsic::FloatToInt => (MachineMode::Float32, MachineMode::Int32),
            Intrinsic::FloatToLong => (MachineMode::Float32, MachineMode::Int64),
            Intrinsic::DoubleToInt => (MachineMode::Float64, MachineMode::Int32),
            Intrinsic::DoubleToLong => (MachineMode::Float64, MachineMode::Int64),
            _ => unreachable!(),
        };

        self.asm
            .float_to_int(dest_mode, dest, src_mode, FREG_RESULT);
    }

    fn emit_intrinsic_float_as_int(
        &mut self,
        e: &'ast ExprCallType,
        dest: Reg,
        intrinsic: Intrinsic,
    ) {
        self.emit_expr(e.object.as_ref().unwrap(), FREG_RESULT.into());

        let (src_mode, dest_mode) = match intrinsic {
            Intrinsic::FloatAsInt => (MachineMode::Float32, MachineMode::Int32),
            Intrinsic::DoubleAsLong => (MachineMode::Float64, MachineMode::Int64),
            _ => unreachable!(),
        };

        self.asm
            .float_as_int(dest_mode, dest, src_mode, FREG_RESULT);
    }

    fn emit_intrinsic_int_as_float(
        &mut self,
        e: &'ast ExprCallType,
        dest: FReg,
        intrinsic: Intrinsic,
    ) {
        self.emit_expr(e.object.as_ref().unwrap(), REG_RESULT.into());

        let (src_mode, dest_mode) = match intrinsic {
            Intrinsic::IntAsFloat => (MachineMode::Int32, MachineMode::Float32),
            Intrinsic::LongAsDouble => (MachineMode::Int64, MachineMode::Float64),
            _ => unreachable!(),
        };

        self.asm.int_as_float(dest_mode, dest, src_mode, REG_RESULT);
    }

    fn emit_intrinsic_byte_to_int(&mut self, e: &'ast ExprCallType, dest: Reg) {
        self.emit_expr(e.object.as_ref().unwrap(), dest.into());
        self.asm.extend_byte(MachineMode::Int32, dest, dest);
    }

    fn emit_intrinsic_byte_to_long(&mut self, e: &'ast ExprCallType, dest: Reg) {
        self.emit_expr(e.object.as_ref().unwrap(), dest.into());
        self.asm.extend_byte(MachineMode::Int64, dest, dest);
    }

    fn emit_intrinsic_bin_call(&mut self, e: &'ast ExprCallType, dest: ExprStore, intr: Intrinsic) {
        let lhs = e.object.as_ref().unwrap();
        let rhs = &e.args[0];

        self.emit_intrinsic_bin(lhs, rhs, dest, intr, None);
    }

    fn emit_intrinsic_bin(
        &mut self,
        lhs: &'ast Expr,
        rhs: &'ast Expr,
        dest: ExprStore,
        intr: Intrinsic,
        op: Option<BinOp>,
    ) {
        let mode = self.ty(lhs.id()).mode();

        let (lhs_reg, rhs_reg) = if mode.is_float() {
            (FREG_RESULT.into(), FREG_TMP1.into())
        } else {
            (REG_RESULT.into(), REG_TMP1.into())
        };

        self.emit_expr(lhs, lhs_reg);
        let offset = self.reserve_temp_for_node(lhs);

        self.asm.store_mem(mode, Mem::Local(offset), lhs_reg);

        self.emit_expr(rhs, rhs_reg);

        self.asm.load_mem(mode, lhs_reg, Mem::Local(offset));

        if mode.is_float() {
            let lhs_reg = lhs_reg.freg();
            let rhs_reg = rhs_reg.freg();

            self.emit_intrinsic_float(dest, lhs_reg, rhs_reg, intr, op);
        } else {
            let lhs_reg = lhs_reg.reg();
            let rhs_reg = rhs_reg.reg();

            self.emit_intrinsic_int(dest.reg(), lhs_reg, rhs_reg, intr, op);
        }

        self.free_temp_for_node(lhs, offset);
    }

    fn emit_intrinsic_int(
        &mut self,
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
        intr: Intrinsic,
        op: Option<BinOp>,
    ) {
        match intr {
            Intrinsic::ByteEq
            | Intrinsic::BoolEq
            | Intrinsic::CharEq
            | Intrinsic::IntEq
            | Intrinsic::LongEq => {
                let mode = if intr == Intrinsic::LongEq {
                    MachineMode::Int64
                } else {
                    MachineMode::Int32
                };

                let cond_code = match op {
                    Some(BinOp::Cmp(cmp)) => to_cond_code(cmp),
                    _ => CondCode::Equal,
                };

                self.asm.cmp_reg(mode, lhs, rhs);
                self.asm.set(dest, cond_code);
            }

            Intrinsic::ByteCmp | Intrinsic::CharCmp | Intrinsic::IntCmp | Intrinsic::LongCmp => {
                let mode = if intr == Intrinsic::LongCmp {
                    MachineMode::Int64
                } else {
                    MachineMode::Int32
                };

                if let Some(BinOp::Cmp(op)) = op {
                    let cond_code = to_cond_code(op);

                    self.asm.cmp_reg(mode, lhs, rhs);
                    self.asm.set(dest, cond_code);
                } else {
                    self.asm.int_sub(mode, dest, lhs, rhs);
                }
            }

            Intrinsic::IntAdd => self.asm.int_add(MachineMode::Int32, dest, lhs, rhs),
            Intrinsic::IntSub => self.asm.int_sub(MachineMode::Int32, dest, lhs, rhs),
            Intrinsic::IntMul => self.asm.int_mul(MachineMode::Int32, dest, lhs, rhs),
            Intrinsic::IntDiv => self.asm.int_div(MachineMode::Int32, dest, lhs, rhs),
            Intrinsic::IntMod => self.asm.int_mod(MachineMode::Int32, dest, lhs, rhs),

            Intrinsic::IntOr => self.asm.int_or(MachineMode::Int32, dest, lhs, rhs),
            Intrinsic::IntAnd => self.asm.int_and(MachineMode::Int32, dest, lhs, rhs),
            Intrinsic::IntXor => self.asm.int_xor(MachineMode::Int32, dest, lhs, rhs),

            Intrinsic::IntShl => self.asm.int_shl(MachineMode::Int32, dest, lhs, rhs),
            Intrinsic::IntSar => self.asm.int_sar(MachineMode::Int32, dest, lhs, rhs),
            Intrinsic::IntShr => self.asm.int_shr(MachineMode::Int32, dest, lhs, rhs),

            Intrinsic::LongAdd => self.asm.int_add(MachineMode::Int64, dest, lhs, rhs),
            Intrinsic::LongSub => self.asm.int_sub(MachineMode::Int64, dest, lhs, rhs),
            Intrinsic::LongMul => self.asm.int_mul(MachineMode::Int64, dest, lhs, rhs),
            Intrinsic::LongDiv => self.asm.int_div(MachineMode::Int64, dest, lhs, rhs),
            Intrinsic::LongMod => self.asm.int_mod(MachineMode::Int64, dest, lhs, rhs),

            Intrinsic::LongOr => self.asm.int_or(MachineMode::Int64, dest, lhs, rhs),
            Intrinsic::LongAnd => self.asm.int_and(MachineMode::Int64, dest, lhs, rhs),
            Intrinsic::LongXor => self.asm.int_xor(MachineMode::Int64, dest, lhs, rhs),

            Intrinsic::LongShl => self.asm.int_shl(MachineMode::Int64, dest, lhs, rhs),
            Intrinsic::LongSar => self.asm.int_sar(MachineMode::Int64, dest, lhs, rhs),
            Intrinsic::LongShr => self.asm.int_shr(MachineMode::Int64, dest, lhs, rhs),

            _ => panic!("unexpected intrinsic {:?}", intr),
        }
    }

    fn emit_intrinsic_float(
        &mut self,
        dest: ExprStore,
        lhs: FReg,
        rhs: FReg,
        intr: Intrinsic,
        op: Option<BinOp>,
    ) {
        use ty::MachineMode::{Float32, Float64};

        match intr {
            Intrinsic::FloatEq | Intrinsic::DoubleEq => {
                let mode = if intr == Intrinsic::DoubleEq {
                    Float64
                } else {
                    Float32
                };

                let cond_code = match op {
                    Some(BinOp::Cmp(cmp)) => to_cond_code(cmp),
                    _ => CondCode::Equal,
                };

                self.asm.float_cmp(mode, dest.reg(), lhs, rhs, cond_code);
            }

            Intrinsic::FloatCmp | Intrinsic::DoubleCmp => {
                let mode = if intr == Intrinsic::DoubleCmp {
                    Float64
                } else {
                    Float32
                };

                if let Some(BinOp::Cmp(op)) = op {
                    let cond_code = to_cond_code(op);

                    self.asm.float_cmp(mode, dest.reg(), lhs, rhs, cond_code);
                } else {
                    unimplemented!();
                }
            }

            Intrinsic::FloatAdd => self.asm.float_add(Float32, dest.freg(), lhs, rhs),
            Intrinsic::FloatSub => self.asm.float_sub(Float32, dest.freg(), lhs, rhs),
            Intrinsic::FloatMul => self.asm.float_mul(Float32, dest.freg(), lhs, rhs),
            Intrinsic::FloatDiv => self.asm.float_div(Float32, dest.freg(), lhs, rhs),

            Intrinsic::DoubleAdd => self.asm.float_add(Float64, dest.freg(), lhs, rhs),
            Intrinsic::DoubleSub => self.asm.float_sub(Float64, dest.freg(), lhs, rhs),
            Intrinsic::DoubleMul => self.asm.float_mul(Float64, dest.freg(), lhs, rhs),
            Intrinsic::DoubleDiv => self.asm.float_div(Float64, dest.freg(), lhs, rhs),

            _ => panic!("unexpected intrinsic {:?}", intr),
        }
    }

    fn emit_delegation(&mut self, e: &'ast ExprDelegationType, dest: ExprStore) {
        self.emit_call_site_id(e.id, e.pos, dest);
    }

    fn has_call_site(&self, id: NodeId) -> bool {
        self.jit_info.map_csites.get(id).is_some()
    }

    fn emit_call_site_id(&mut self, id: NodeId, pos: Position, dest: ExprStore) {
        let csite = self.jit_info.map_csites.get(id).unwrap().clone();
        self.emit_call_site(&csite, pos, dest);
    }

    pub fn emit_call_site(&mut self, csite: &CallSite<'ast>, pos: Position, dest: ExprStore) {
        let mut temps: Vec<(BuiltinType, i32, Option<ClassDefId>)> = Vec::new();

        let fid = csite.callee;
        let fct = self.vm.fcts.idx(fid);
        let fct = fct.read();

        for (idx, arg) in csite.args.iter().enumerate() {
            let mode = arg.ty().mode();
            let dest = register_for_mode(mode);

            match *arg {
                Arg::Expr(ast, ty, _) => {
                    self.emit_expr(ast, dest);

                    // check first argument for nil for method calls
                    //
                    // no check necessary for:
                    //   super calls (guaranteed to not be nil) and
                    //   dynamic dispatch (implicit check when loading fctptr from vtable)
                    if idx == 0
                        && fct.has_self()
                        && check_for_nil(ty)
                        && !csite.super_call
                        && !fct.is_virtual()
                    {
                        self.asm.test_if_nil_bailout(pos, dest.reg(), Trap::NIL);
                    }
                }

                Arg::Stack(soffset, ty, _) => {
                    self.asm.load_mem(ty.mode(), dest, Mem::Local(soffset));
                }

                Arg::Selfie(_, _) => {
                    self.emit_self(dest.reg());
                }

                Arg::SelfieNew(ty, _) => {
                    let cls_id = specialize_class_ty(self.vm, ty);
                    // do NOT invoke `reserve_temp_for_arg` here, since
                    // this would also add it to the set of temporaries, which
                    // leads to this address being part of the gc point for
                    // the collection of the object.
                    let offset = self.reserve_temp_for_self(arg);
                    temps.push((ty, offset, Some(cls_id)));
                    continue;
                }
            }

            let offset = self.reserve_temp_for_arg(arg);
            self.asm
                .store_mem(arg.ty().mode(), Mem::Local(offset), dest);
            temps.push((arg.ty(), offset, None));
        }

        let mut arg_offset = -self.jit_info.stacksize();
        let mut idx = 0;
        let mut reg_idx = 0;
        let mut freg_idx = 0;

        for arg in &csite.args {
            let ty = arg.ty();
            let mode = ty.mode();
            let is_float = mode.is_float();
            let offset = temps[idx].1;

            if idx == 0 {
                if let Some(cls_id) = temps[idx].2 {
                    let reg = REG_PARAMS[reg_idx];
                    self.emit_allocation(pos, &temps, cls_id, offset, reg);

                    // after the allocation `offset` is initialized,
                    // add it to the set of temporaries such that it is part
                    // of the gc point
                    self.temps.insert(offset);

                    reg_idx += 1;
                    idx += 1;
                    continue;
                }
            }

            if is_float {
                if freg_idx < FREG_PARAMS.len() {
                    let freg = FREG_PARAMS[freg_idx];
                    self.asm.load_mem(mode, freg.into(), Mem::Local(offset));

                    freg_idx += 1;
                } else {
                    self.asm
                        .load_mem(mode, FREG_TMP1.into(), Mem::Local(offset));
                    self.asm
                        .store_mem(mode, Mem::Local(arg_offset), FREG_TMP1.into());

                    arg_offset += 8;
                }
            } else {
                if reg_idx < REG_PARAMS.len() {
                    let reg = REG_PARAMS[reg_idx];
                    self.asm.load_mem(mode, reg.into(), Mem::Local(offset));

                    reg_idx += 1;
                } else {
                    self.asm.load_mem(mode, REG_TMP1.into(), Mem::Local(offset));
                    self.asm
                        .store_mem(mode, Mem::Local(arg_offset), REG_TMP1.into());

                    arg_offset += 8;
                }
            }

            idx += 1;
        }

        let return_type = self.specialize_type(csite.return_type);
        let cls_type_params: TypeParams = csite
            .cls_type_params
            .iter()
            .map(|ty| self.specialize_type(ty))
            .collect::<Vec<_>>()
            .into();
        let fct_type_params: TypeParams = csite
            .fct_type_params
            .iter()
            .map(|ty| self.specialize_type(ty))
            .collect::<Vec<_>>()
            .into();

        debug_assert!(cls_type_params
            .iter()
            .all(|ty| !ty.contains_type_param(self.vm)));
        debug_assert!(fct_type_params
            .iter()
            .all(|ty| !ty.contains_type_param(self.vm)));

        if csite.super_call {
            let ptr = self.ptr_for_fct_id(fid, cls_type_params.clone(), fct_type_params.clone());
            self.asm.emit_comment(Comment::CallSuper(fid));
            let gcpoint = codegen::create_gcpoint(self.scopes, &self.temps);
            self.asm.direct_call(
                fid,
                ptr.to_ptr(),
                cls_type_params,
                fct_type_params,
                pos,
                gcpoint,
                return_type,
                dest,
            );
        } else if fct.is_virtual() {
            let vtable_index = fct.vtable_index.unwrap();
            self.asm.emit_comment(Comment::CallVirtual(fid));
            let gcpoint = self.create_gcpoint();
            self.asm
                .indirect_call(vtable_index, pos, gcpoint, return_type, dest);
        } else {
            let ptr = self.ptr_for_fct_id(fid, cls_type_params.clone(), fct_type_params.clone());
            self.asm.emit_comment(Comment::CallDirect(fid));
            let gcpoint = codegen::create_gcpoint(self.scopes, &self.temps);
            self.asm.direct_call(
                fid,
                ptr.to_ptr(),
                cls_type_params,
                fct_type_params,
                pos,
                gcpoint,
                return_type,
                dest,
            );
        }

        if csite.args.len() > 0 {
            if let Arg::SelfieNew(_, _) = csite.args[0] {
                let (ty, offset, _) = temps[0];
                self.asm.load_mem(ty.mode(), dest, Mem::Local(offset));
            }
        }

        for temp in temps.into_iter() {
            self.free_temp_with_type(temp.0, temp.1);
        }
    }

    fn emit_allocation(
        &mut self,
        pos: Position,
        temps: &[(BuiltinType, i32, Option<ClassDefId>)],
        cls_id: ClassDefId,
        offset: i32,
        dest: Reg,
    ) {
        let cls = self.vm.class_defs.idx(cls_id);
        let cls = cls.read();
        let mut store_length = false;

        // allocate storage for object
        self.asm.emit_comment(Comment::Alloc(cls_id));

        let alloc_size;

        match cls.size {
            ClassSize::Fixed(size) => {
                self.asm
                    .load_int_const(MachineMode::Int32, REG_PARAMS[0], size as i64);
                alloc_size = AllocationSize::Fixed(size as usize);
            }

            ClassSize::Array(esize) if temps.len() > 1 => {
                self.asm
                    .load_mem(MachineMode::Int32, REG_TMP1.into(), Mem::Local(temps[1].1));

                self.asm
                    .determine_array_size(REG_PARAMS[0], REG_TMP1, esize, true);

                store_length = true;
                alloc_size = AllocationSize::Dynamic(REG_PARAMS[0]);
            }

            ClassSize::ObjArray if temps.len() > 1 => {
                self.asm
                    .load_mem(MachineMode::Int32, REG_TMP1.into(), Mem::Local(temps[1].1));

                self.asm
                    .determine_array_size(REG_PARAMS[0], REG_TMP1, mem::ptr_width(), true);

                store_length = true;
                alloc_size = AllocationSize::Dynamic(REG_PARAMS[0]);
            }

            ClassSize::Str if temps.len() > 1 => {
                self.asm
                    .load_mem(MachineMode::Int32, REG_TMP1.into(), Mem::Local(temps[1].1));

                self.asm
                    .determine_array_size(REG_PARAMS[0], REG_TMP1, 1, true);

                store_length = true;
                alloc_size = AllocationSize::Dynamic(REG_PARAMS[0]);
            }

            ClassSize::Array(_) | ClassSize::ObjArray | ClassSize::Str => {
                let size = Header::size() as usize + mem::ptr_width_usize();
                self.asm
                    .load_int_const(MachineMode::Int32, REG_PARAMS[0], size as i64);

                store_length = true;
                alloc_size = AllocationSize::Fixed(size);
            }
        }

        let array_ref = match cls.size {
            ClassSize::ObjArray => true,
            _ => false,
        };

        let gcpoint = self.create_gcpoint();
        self.asm.allocate(dest, alloc_size, pos, array_ref, gcpoint);

        // store gc object in temporary storage
        self.asm
            .store_mem(MachineMode::Ptr, Mem::Local(offset), dest.into());

        // store classptr in object
        let cptr = (&**cls.vtable.as_ref().unwrap()) as *const VTable as *const u8;
        let disp = self.asm.add_addr(cptr);
        let pos = self.asm.pos() as i32;

        let temp = if dest == REG_TMP1 { REG_TMP2 } else { REG_TMP1 };

        self.asm.emit_comment(Comment::StoreVTable(cls_id));
        self.asm.load_constpool(temp, disp + pos);
        self.asm
            .store_mem(MachineMode::Ptr, Mem::Base(dest, 0), temp.into());

        // clear mark/fwdptr word in header
        assert!(Header::size() == 2 * mem::ptr_width());
        self.asm.load_int_const(MachineMode::Ptr, temp, 0);
        self.asm.store_mem(
            MachineMode::Ptr,
            Mem::Base(dest, mem::ptr_width()),
            temp.into(),
        );

        // store length in object
        if store_length {
            if temps.len() > 1 {
                self.asm
                    .load_mem(MachineMode::Int32, temp.into(), Mem::Local(temps[1].1));
            } else {
                self.asm.load_int_const(MachineMode::Ptr, temp, 0);
            }

            self.asm.store_mem(
                MachineMode::Ptr,
                Mem::Base(dest, Header::size()),
                temp.into(),
            );
        }

        match cls.size {
            ClassSize::Fixed(size) => {
                self.asm.fill_zero(dest, size as usize);
            }

            _ if temps.len() > 1 => {
                self.asm.int_add_imm(
                    MachineMode::Ptr,
                    dest,
                    dest,
                    (Header::size() + mem::ptr_width()) as i64,
                );

                let element_size = match cls.size {
                    ClassSize::Array(esize) => esize,
                    ClassSize::ObjArray => mem::ptr_width(),
                    ClassSize::Str => 1,
                    ClassSize::Fixed(_) => unreachable!(),
                };

                self.asm
                    .determine_array_size(temp, temp, element_size, false);
                self.asm.int_add(MachineMode::Ptr, temp, temp, dest);
                self.asm.fill_zero_dynamic(dest, temp);
            }

            // arrays with length 0 do not need to clear any data
            _ => {}
        }

        self.asm
            .load_mem(MachineMode::Ptr, dest.into(), Mem::Local(offset));
    }

    fn specialize_type(&self, ty: BuiltinType) -> BuiltinType {
        match ty {
            BuiltinType::ClassTypeParam(cls_id, id) => {
                assert!(self.fct.parent == FctParent::Class(cls_id));
                self.cls_type_params[id.idx()]
            }

            BuiltinType::FctTypeParam(fct_id, id) => {
                assert!(self.fct.id == fct_id);
                self.fct_type_params[id.idx()]
            }

            BuiltinType::Class(cls_id, list_id) => {
                let params = self.vm.lists.lock().get(list_id);

                let params: Vec<_> = params.iter().map(|t| self.specialize_type(t)).collect();

                let list_id = self.vm.lists.lock().insert(params.into());

                BuiltinType::Class(cls_id, list_id)
            }

            BuiltinType::Lambda(_) => unimplemented!(),

            _ => ty,
        }
    }

    fn ty(&self, id: NodeId) -> BuiltinType {
        let ty = self.src.ty(id);
        self.specialize_type(ty)
    }

    fn create_gcpoint(&self) -> GcPoint {
        codegen::create_gcpoint(self.scopes, &self.temps)
    }
}

fn result_reg(mode: MachineMode) -> ExprStore {
    if mode.is_float() {
        FREG_RESULT.into()
    } else {
        REG_RESULT.into()
    }
}

fn check_for_nil(ty: BuiltinType) -> bool {
    match ty {
        BuiltinType::Error => panic!("error shouldn't occur in code generation."),
        BuiltinType::Unit => false,
        BuiltinType::Byte
        | BuiltinType::Char
        | BuiltinType::Int
        | BuiltinType::Long
        | BuiltinType::Float
        | BuiltinType::Double
        | BuiltinType::Bool => false,
        BuiltinType::Nil | BuiltinType::Ptr => true,
        BuiltinType::Class(_, _) => true,
        BuiltinType::Struct(_, _) => false,
        BuiltinType::Trait(_) => false,
        BuiltinType::This => unreachable!(),
        BuiltinType::ClassTypeParam(_, _) => unreachable!(),
        BuiltinType::FctTypeParam(_, _) => unreachable!(),
        BuiltinType::Lambda(_) => true,
    }
}

pub fn ensure_native_stub(vm: &VM, fct_id: FctId, internal_fct: InternalFct) -> Address {
    let mut native_thunks = vm.native_thunks.lock();
    let ptr = internal_fct.ptr;

    if let Some(jit_fct_id) = native_thunks.find_fct(ptr) {
        let jit_fct = vm.jit_fcts.idx(jit_fct_id);
        jit_fct.fct_ptr()
    } else {
        let fct = vm.fcts.idx(fct_id);
        let fct = fct.read();
        let dbg = should_emit_debug(vm, &*fct);

        let jit_fct_id = dora_native::generate(vm, internal_fct, dbg);
        let jit_fct = vm.jit_fcts.idx(jit_fct_id);
        let jit_fct = jit_fct.to_base().expect("baseline expected");

        let fct_start = jit_fct.fct_start;

        if should_emit_asm(vm, &*fct) {
            dump_asm(
                vm,
                &*fct,
                &jit_fct,
                None,
                vm.args.flag_asm_syntax.unwrap_or(AsmSyntax::Att),
            );
        }

        native_thunks.insert_fct(ptr, jit_fct_id);
        fct_start
    }
}

fn ensure_jit_or_stub_ptr<'ast>(
    src: &mut FctSrc,
    vm: &VM,
    cls_type_params: TypeParams,
    fct_type_params: TypeParams,
) -> Address {
    let specials = src.specializations.read();
    let key = (cls_type_params, fct_type_params);

    if let Some(&jit_fct_id) = specials.get(&key) {
        let jit_fct = vm.jit_fcts.idx(jit_fct_id);
        return jit_fct.fct_ptr();
    }

    vm.compiler_thunk()
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

pub enum AllocationSize {
    Fixed(usize),
    Dynamic(Reg),
}
