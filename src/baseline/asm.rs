use dora_parser::lexer::position::Position;

use baseline::codegen::CondCode;
use baseline::dora_native::{InternalFct, InternalFctDescriptor};
use baseline::expr::{AllocationSize, ensure_native_stub, ExprStore};
use baseline::fct::{CatchType, Comment, GcPoint, JitBaselineFct, JitDescriptor};
use baseline::info::JitInfo;
use class::TypeParams;
use cpu::{FReg, Mem, Reg, FREG_RESULT, REG_PARAMS, REG_RESULT, REG_THREAD, REG_TMP1};
use ctxt::{FctId, SemContext, VarId};
use gc::tlab::TLAB_OBJECT_SIZE;
use masm::{Label, MacroAssembler, ScratchReg};
use os::signal::Trap;
use stdlib;
use threads::ThreadLocalData;
use ty::{BuiltinType, MachineMode};

pub struct BaselineAssembler<'a, 'ast: 'a> {
    masm: MacroAssembler,
    ctxt: &'a SemContext<'ast>,
}

impl<'a, 'ast> BaselineAssembler<'a, 'ast>
where
    'ast: 'a,
{
    pub fn new(ctxt: &'a SemContext<'ast>) -> BaselineAssembler<'a, 'ast> {
        BaselineAssembler {
            masm: MacroAssembler::new(),
            ctxt: ctxt,
        }
    }

    pub fn debug(&mut self) {
        self.masm.debug();
    }

    pub fn prolog(&mut self, stacksize: i32) {
        self.masm.prolog(stacksize);
    }

    pub fn epilog_with_polling(&mut self, stacksize: i32, polling_page: *const u8) {
        self.masm.epilog_with_polling(stacksize, polling_page);
    }

    pub fn emit_comment(&mut self, comment: Comment) {
        self.masm.emit_comment(comment);
    }

    pub fn copy_reg(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        self.masm.copy_reg(mode, dest, src);
    }

    pub fn copy_freg(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        self.masm.copy_freg(mode, dest, src);
    }

    pub fn check_polling_page(&mut self, page: *const u8) {
        self.masm.check_polling_page(page);
    }

    pub fn emit_gcpoint(&mut self, gcpoint: GcPoint) {
        self.masm.emit_gcpoint(gcpoint);
    }

    pub fn bind_label(&mut self, label: Label) {
        self.masm.bind_label(label);
    }

    pub fn create_label(&mut self) -> Label {
        self.masm.create_label()
    }

    pub fn jump(&mut self, label: Label) {
        self.masm.jump(label);
    }

    pub fn jump_if(&mut self, cond: CondCode, label: Label) {
        self.masm.jump_if(cond, label);
    }

    pub fn pos(&self) -> usize {
        self.masm.pos()
    }

    pub fn throw(&mut self, receiver: Reg, pos: Position) {
        self.masm.throw(receiver, pos);
    }

    pub fn store_mem(&mut self, mode: MachineMode, mem: Mem, src: ExprStore) {
        self.masm.store_mem(mode, mem, src);
    }

    pub fn load_mem(&mut self, mode: MachineMode, dest: ExprStore, mem: Mem) {
        self.masm.load_mem(mode, dest, mem);
    }

    pub fn test_and_jump_if(&mut self, cond: CondCode, reg: Reg, lbl: Label) {
        self.masm.test_and_jump_if(cond, reg, lbl);
    }

    pub fn test_if_nil_bailout(&mut self, pos: Position, reg: Reg, trap: Trap) {
        self.masm.test_if_nil_bailout(pos, reg, trap);
    }

    pub fn test_if_nil(&mut self, reg: Reg) -> Label {
        self.masm.test_if_nil(reg)
    }

    pub fn load_nil(&mut self, dest: Reg) {
        self.masm.load_nil(dest);
    }

    pub fn load_int_const(&mut self, mode: MachineMode, dest: Reg, imm: i64) {
        self.masm.load_int_const(mode, dest, imm);
    }

    pub fn load_float_const(&mut self, mode: MachineMode, dest: FReg, imm: f64) {
        self.masm.load_float_const(mode, dest, imm);
    }

    pub fn load_constpool(&mut self, dest: Reg, disp: i32) {
        self.masm.load_constpool(dest, disp);
    }

    pub fn load_true(&mut self, dest: Reg) {
        self.masm.load_true(dest);
    }

    pub fn load_false(&mut self, dest: Reg) {
        self.masm.load_false(dest);
    }

    pub fn emit_bailout(&mut self, lbl: Label, trap: Trap, pos: Position) {
        self.masm.emit_bailout(lbl, trap, pos);
    }

    pub fn emit_bailout_inplace(&mut self, trap: Trap, pos: Position) {
        self.masm.emit_bailout_inplace(trap, pos)
    }

    pub fn emit_exception_handler(
        &mut self,
        span: (usize, usize),
        catch: usize,
        offset: Option<i32>,
        catch_type: CatchType,
    ) {
        self.masm
            .emit_exception_handler(span, catch, offset, catch_type);
    }

    pub fn get_scratch(&self) -> ScratchReg {
        self.masm.get_scratch()
    }

    pub fn cmp_reg(&mut self, mode: MachineMode, lhs: Reg, rhs: Reg) {
        self.masm.cmp_reg(mode, lhs, rhs);
    }

    pub fn cmp_reg_imm(&mut self, mode: MachineMode, lhs: Reg, imm: i32) {
        self.masm.cmp_reg_imm(mode, lhs, imm);
    }

    pub fn cmp_mem_imm(&mut self, mode: MachineMode, mem: Mem, imm: i32) {
        self.masm.cmp_mem_imm(mode, mem, imm);
    }

    pub fn cmp_mem(&mut self, mode: MachineMode, mem: Mem, rhs: Reg) {
        self.masm.cmp_mem(mode, mem, rhs);
    }

    pub fn add_addr(&mut self, ptr: *const u8) -> i32 {
        self.masm.add_addr(ptr)
    }

    pub fn set(&mut self, dest: Reg, op: CondCode) {
        self.masm.set(dest, op);
    }

    pub fn int_add(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        self.masm.int_add(mode, dest, lhs, rhs);
    }

    pub fn int_add_imm(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, value: i64) {
        self.masm.int_add_imm(mode, dest, lhs, value);
    }

    pub fn int_sub(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        self.masm.int_sub(mode, dest, lhs, rhs);
    }

    pub fn int_mul(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        self.masm.int_mul(mode, dest, lhs, rhs);
    }

    pub fn int_div(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        self.masm.int_div(mode, dest, lhs, rhs);
    }

    pub fn int_mod(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        self.masm.int_mod(mode, dest, lhs, rhs);
    }

    pub fn int_neg(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        self.masm.int_neg(mode, dest, src);
    }

    pub fn int_not(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        self.masm.int_not(mode, dest, src);
    }

    pub fn int_or(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        self.masm.int_or(mode, dest, lhs, rhs);
    }

    pub fn int_and(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        self.masm.int_and(mode, dest, lhs, rhs);
    }

    pub fn int_xor(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        self.masm.int_xor(mode, dest, lhs, rhs);
    }

    pub fn int_shl(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        self.masm.int_shl(mode, dest, lhs, rhs);
    }

    pub fn int_shr(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        self.masm.int_shr(mode, dest, lhs, rhs);
    }

    pub fn int_sar(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        self.masm.int_sar(mode, dest, lhs, rhs);
    }

    pub fn bool_not(&mut self, dest: Reg, src: Reg) {
        self.masm.bool_not(dest, src);
    }

    pub fn float_add(&mut self, mode: MachineMode, dest: FReg, lhs: FReg, rhs: FReg) {
        self.masm.float_add(mode, dest, lhs, rhs);
    }

    pub fn float_sub(&mut self, mode: MachineMode, dest: FReg, lhs: FReg, rhs: FReg) {
        self.masm.float_sub(mode, dest, lhs, rhs);
    }

    pub fn float_mul(&mut self, mode: MachineMode, dest: FReg, lhs: FReg, rhs: FReg) {
        self.masm.float_mul(mode, dest, lhs, rhs);
    }

    pub fn float_div(&mut self, mode: MachineMode, dest: FReg, lhs: FReg, rhs: FReg) {
        self.masm.float_div(mode, dest, lhs, rhs);
    }

    pub fn float_neg(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        self.masm.float_neg(mode, dest, src);
    }

    pub fn float_cmp_nan(&mut self, mode: MachineMode, dest: Reg, src: FReg) {
        self.masm.float_cmp_nan(mode, dest, src);
    }

    pub fn float_cmp(
        &mut self,
        mode: MachineMode,
        dest: Reg,
        lhs: FReg,
        rhs: FReg,
        cond: CondCode,
    ) {
        self.masm.float_cmp(mode, dest, lhs, rhs, cond);
    }

    pub fn determine_array_size(
        &mut self,
        dest: Reg,
        length: Reg,
        element_size: i32,
        with_header: bool,
    ) {
        self.masm
            .determine_array_size(dest, length, element_size, with_header);
    }

    pub fn fill_zero(&mut self, obj: Reg, size: usize) {
        self.masm.fill_zero(obj, size);
    }

    pub fn fill_zero_dynamic(&mut self, obj: Reg, obj_end: Reg) {
        self.masm.fill_zero_dynamic(obj, obj_end);
    }

    pub fn load_field(
        &mut self,
        mode: MachineMode,
        dest: ExprStore,
        base: Reg,
        offset: i32,
        line: i32,
    ) {
        self.masm.load_field(mode, dest, base, offset, line);
    }

    pub fn store_field(
        &mut self,
        mode: MachineMode,
        base: Reg,
        offset: i32,
        src: ExprStore,
        line: i32,
        write_barrier: bool,
        card_table_offset: usize,
    ) {
        self.masm.store_field(
            mode,
            base,
            offset,
            src,
            line,
            write_barrier,
            card_table_offset,
        );
    }

    pub fn load_array_elem(&mut self, mode: MachineMode, dest: ExprStore, array: Reg, index: Reg) {
        self.masm.load_array_elem(mode, dest, array, index);
    }

    pub fn store_array_elem(
        &mut self,
        mode: MachineMode,
        array: Reg,
        index: Reg,
        value: ExprStore,
        write_barrier: bool,
        card_table_offset: usize,
    ) {
        self.masm
            .store_array_elem(mode, array, index, value, write_barrier, card_table_offset);
    }

    pub fn float_sqrt(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        self.masm.float_sqrt(mode, dest, src);
    }

    pub fn copy(&mut self, mode: MachineMode, dest: ExprStore, src: ExprStore) {
        self.masm.copy(mode, dest, src);
    }

    pub fn check_index_out_of_bounds(&mut self, pos: Position, array: Reg, index: Reg) {
        self.masm.check_index_out_of_bounds(pos, array, index);
    }

    pub fn extend_byte(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        self.masm.extend_byte(mode, dest, src);
    }

    pub fn extend_int_long(&mut self, dest: Reg, src: Reg) {
        self.masm.extend_int_long(dest, src);
    }

    pub fn float_to_double(&mut self, dest: FReg, src: FReg) {
        self.masm.float_to_double(dest, src);
    }

    pub fn double_to_float(&mut self, dest: FReg, src: FReg) {
        self.masm.double_to_float(dest, src);
    }

    pub fn int_to_float(
        &mut self,
        dest_mode: MachineMode,
        dest: FReg,
        src_mode: MachineMode,
        src: Reg,
    ) {
        self.masm.int_to_float(dest_mode, dest, src_mode, src);
    }

    pub fn float_to_int(
        &mut self,
        dest_mode: MachineMode,
        dest: Reg,
        src_mode: MachineMode,
        src: FReg,
    ) {
        self.masm.float_to_int(dest_mode, dest, src_mode, src);
    }

    pub fn emit_lineno(&mut self, lineno: i32) {
        self.masm.emit_lineno(lineno);
    }

    pub fn var_store(&mut self, jit_info: &JitInfo, src: ExprStore, var_id: VarId) {
        let offset = jit_info.offset(var_id);
        let ty = jit_info.ty(var_id);
        self.masm.store_mem(ty.mode(), Mem::Local(offset), src);
    }

    pub fn var_load(&mut self, jit_info: &JitInfo, var_id: VarId, dest: ExprStore) {
        let offset = jit_info.offset(var_id);
        let ty = jit_info.ty(var_id);
        self.masm.load_mem(ty.mode(), dest, Mem::Local(offset));
    }

    pub fn jit(self, stacksize: i32, desc: JitDescriptor, throws: bool) -> JitBaselineFct {
        self.masm.jit(self.ctxt, stacksize, desc, throws)
    }

    pub fn native_call(
        &mut self,
        internal_fct: InternalFct,
        pos: Position,
        gcpoint: GcPoint,
        dest: ExprStore,
    ) {
        let ty = internal_fct.return_type;
        let ptr = ensure_native_stub(self.ctxt, FctId(0), internal_fct);

        self.masm.raw_call(ptr);
        self.call_epilog(pos, ty, dest, gcpoint);
    }

    pub fn direct_call(
        &mut self,
        fct_id: FctId,
        ptr: *const u8,
        cls_tps: TypeParams,
        fct_tps: TypeParams,
        pos: Position,
        gcpoint: GcPoint,
        ty: BuiltinType,
        dest: ExprStore,
    ) {
        self.masm.direct_call(fct_id, ptr, cls_tps, fct_tps);
        self.call_epilog(pos, ty, dest, gcpoint);
    }

    pub fn indirect_call(
        &mut self,
        index: u32,
        pos: Position,
        gcpoint: GcPoint,
        ty: BuiltinType,
        dest: ExprStore,
    ) {
        self.masm.indirect_call(pos.line as i32, index);
        self.call_epilog(pos, ty, dest, gcpoint);
    }

    fn call_epilog(&mut self, pos: Position, ty: BuiltinType, dest: ExprStore, gcpoint: GcPoint) {
        self.masm.emit_lineno(pos.line as i32);
        self.masm.emit_gcpoint(gcpoint);
        self.copy_result(ty, dest);
    }

    fn copy_result(&mut self, ty: BuiltinType, dest: ExprStore) {
        if ty.is_unit() {
            return;
        }

        match dest {
            ExprStore::Reg(dest) => {
                self.masm.copy_reg(ty.mode(), dest, REG_RESULT);
            }

            ExprStore::FReg(dest) => {
                self.masm.copy_freg(ty.mode(), dest, FREG_RESULT);
            }
        }
    }

    pub fn allocate(
        &mut self,
        dest: Reg,
        size: AllocationSize,
        pos: Position,
        array_ref: bool,
        gcpoint: GcPoint,
    ) {
        match size {
            AllocationSize::Fixed(size) => {
                self.masm
                    .load_int_const(MachineMode::Ptr, REG_PARAMS[0], size as i64);
            }

            AllocationSize::Dynamic(reg) => {
                self.masm.copy_reg(MachineMode::Ptr, REG_PARAMS[0], reg);
            }
        }

        self.masm.load_int_const(
            MachineMode::Int8,
            REG_PARAMS[1],
            if array_ref { 1 } else { 0 },
        );

        let internal_fct = InternalFct {
            ptr: stdlib::gc_alloc as *mut u8,
            args: &[BuiltinType::Ptr],
            return_type: BuiltinType::Ptr,
            throws: false,
            desc: InternalFctDescriptor::AllocThunk,
        };

        self.native_call(internal_fct, pos, gcpoint, dest.into());
        self.masm.test_if_nil_bailout(pos, dest, Trap::OOM);
    }

    pub fn tlab_allocate(
        &mut self,
        dest: Reg,
        size: AllocationSize,
        pos: Position,
        array_ref: bool,
        gcpoint: GcPoint,
    ) {
        let lbl_success = self.masm.create_label();
        let lbl_end = self.masm.create_label();
        let lbl_normal_alloc = self.masm.create_label();

        match size {
            AllocationSize::Dynamic(reg_size) => {
                self.masm
                    .cmp_reg_imm(MachineMode::Ptr, reg_size, TLAB_OBJECT_SIZE as i32);
                self.masm.jump_if(CondCode::GreaterEq, lbl_normal_alloc);
            }

            AllocationSize::Fixed(size) => {
                assert!(size < TLAB_OBJECT_SIZE);
            }
        }

        let tlab_next = self.masm.get_scratch();
        let tlab_end = self.masm.get_scratch();

        self.masm.load_mem(
            MachineMode::Ptr,
            REG_TMP1.into(),
            Mem::Base(REG_THREAD, ThreadLocalData::tlab_top_offset()),
        );

        self.masm.load_mem(
            MachineMode::Ptr,
            (*tlab_end).into(),
            Mem::Base(REG_THREAD, ThreadLocalData::tlab_end_offset()),
        );

        match size {
            AllocationSize::Fixed(size) => {
                self.masm.copy_reg(MachineMode::Ptr, *tlab_next, REG_TMP1);
                self.masm
                    .int_add_imm(MachineMode::Ptr, *tlab_next, *tlab_next, size as i64);
            }

            AllocationSize::Dynamic(reg_size) => {
                self.masm.copy_reg(MachineMode::Ptr, *tlab_next, REG_TMP1);
                self.masm
                    .int_add(MachineMode::Ptr, *tlab_next, *tlab_next, reg_size);
            }
        }

        self.masm.cmp_reg(MachineMode::Ptr, *tlab_next, *tlab_end);
        self.masm.jump_if(CondCode::LessEq, lbl_success);

        self.masm.bind_label(lbl_normal_alloc);
        self.allocate(dest, size, pos, array_ref, gcpoint);
        self.masm.jump(lbl_end);

        self.masm.bind_label(lbl_success);

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_THREAD, ThreadLocalData::tlab_top_offset()),
            (*tlab_next).into(),
        );

        self.masm.copy_reg(MachineMode::Ptr, dest, REG_TMP1);
        self.masm.bind_label(lbl_end);
    }
}
