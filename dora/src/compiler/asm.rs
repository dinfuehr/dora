use std::mem;

use dora_parser::lexer::position::Position;

use crate::compiler::codegen::{ensure_native_stub, AllocationSize, AnyReg};
use crate::compiler::fct::{Code, GcPoint, JitDescriptor};
use crate::compiler::native_stub::{NativeFct, NativeFctDescriptor};
use crate::cpu::{FReg, Reg, FREG_RESULT, REG_PARAMS, REG_RESULT, REG_THREAD, REG_TMP1, REG_TMP2};
use crate::gc::tlab::TLAB_OBJECT_SIZE;
use crate::gc::Address;
use crate::masm::{CondCode, Label, MacroAssembler, Mem, ScratchReg};
use crate::stdlib;
use crate::threads::ThreadLocalData;
use crate::ty::{MachineMode, SourceType, SourceTypeArray};
use crate::vm::FctDefinitionId;
use crate::vm::{GlobalDefinition, Trap, VM};

pub struct BaselineAssembler<'a> {
    masm: MacroAssembler,
    vm: &'a VM,
    slow_paths: Vec<SlowPathKind>,
}

impl<'a> BaselineAssembler<'a> {
    pub fn new(vm: &'a VM) -> BaselineAssembler<'a> {
        BaselineAssembler {
            masm: MacroAssembler::new(),
            vm,
            slow_paths: Vec::new(),
        }
    }

    pub fn debug(&mut self) {
        self.masm.debug();
    }

    pub fn prolog_size(&mut self, stacksize: i32) {
        self.masm.prolog_size(stacksize);
    }

    pub fn prolog(&mut self) -> usize {
        self.masm.prolog()
    }

    pub fn stack_guard(&mut self, pos: Position, gcpoint: GcPoint) {
        let lbl_stack_overflow = self.masm.create_label();
        self.masm.check_stack_pointer(lbl_stack_overflow);
        let lbl_return = self.masm.create_label();
        self.masm.bind_label(lbl_return);

        self.slow_paths.push(SlowPathKind::StackOverflow(
            lbl_stack_overflow,
            lbl_return,
            pos,
            gcpoint,
        ));
    }

    pub fn safepoint(&mut self, pos: Position, gcpoint: GcPoint) {
        let lbl_safepoint = self.masm.create_label();
        self.masm.safepoint(lbl_safepoint);
        let lbl_return = self.masm.create_label();
        self.masm.bind_label(lbl_return);

        self.slow_paths.push(SlowPathKind::Safepoint(
            lbl_safepoint,
            lbl_return,
            pos,
            gcpoint,
        ));
    }

    pub fn patch_stacksize(&mut self, patch_offset: usize, stacksize: i32) {
        self.masm.patch_stacksize(patch_offset, stacksize);
    }

    pub fn assert(&mut self, value: Reg, pos: Position) {
        let lbl_assert = self.masm.create_label();
        self.masm
            .test_and_jump_if(CondCode::Zero, value, lbl_assert);

        self.slow_paths.push(SlowPathKind::Assert(lbl_assert, pos));
    }

    pub fn epilog(&mut self) {
        self.masm.epilog();
    }

    pub fn increase_stack_frame(&mut self, size: i32) {
        self.masm.increase_stack_frame(size);
    }

    pub fn decrease_stack_frame(&mut self, size: i32) {
        self.masm.decrease_stack_frame(size);
    }

    pub fn emit_comment(&mut self, comment: String) {
        self.masm.emit_comment(comment);
    }

    pub fn copy_reg(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        self.masm.copy_reg(mode, dest, src);
    }

    pub fn copy_freg(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        self.masm.copy_freg(mode, dest, src);
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

    pub fn create_and_bind_label(&mut self) -> Label {
        self.masm.create_and_bind_label()
    }

    pub fn jump(&mut self, label: Label) {
        self.masm.jump(label);
    }

    pub fn jump_if(&mut self, cond: CondCode, label: Label) {
        self.masm.jump_if(cond, label);
    }

    pub fn bailout_if(&mut self, cond: CondCode, trap: Trap, pos: Position) {
        self.masm.bailout_if(cond, trap, pos);
    }

    pub fn pos(&self) -> usize {
        self.masm.pos()
    }

    pub fn store_mem(&mut self, mode: MachineMode, mem: Mem, src: AnyReg) {
        self.masm.store_mem(mode, mem, src);
    }

    pub fn store_zero(&mut self, mode: MachineMode, mem: Mem) {
        self.masm.store_zero(mode, mem);
    }

    pub fn lea(&mut self, dest: Reg, mem: Mem) {
        self.masm.lea(dest, mem);
    }

    pub fn load_mem(&mut self, mode: MachineMode, dest: AnyReg, mem: Mem) {
        self.masm.load_mem(mode, dest, mem);
    }

    pub fn load_int32_synchronized(&mut self, dest: Reg, addr: Reg) {
        self.masm.load_int32_synchronized(dest, addr);
    }

    pub fn load_int64_synchronized(&mut self, dest: Reg, addr: Reg) {
        self.masm.load_int64_synchronized(dest, addr);
    }

    pub fn store_int32_synchronized(&mut self, src: Reg, addr: Reg) {
        self.masm.store_int32_synchronized(src, addr);
    }

    pub fn store_int64_synchronized(&mut self, src: Reg, addr: Reg) {
        self.masm.store_int64_synchronized(src, addr);
    }

    pub fn exchange_int32_synchronized(&mut self, old: Reg, new: Reg, addr: Reg) {
        self.masm.exchange_int32_synchronized(old, new, addr);
    }

    pub fn exchange_int64_synchronized(&mut self, old: Reg, new: Reg, addr: Reg) {
        self.masm.exchange_int32_synchronized(old, new, addr);
    }

    pub fn compare_exchange_int32_synchronized(
        &mut self,
        expected: Reg,
        new: Reg,
        addr: Reg,
    ) -> Reg {
        self.masm
            .compare_exchange_int32_synchronized(expected, new, addr)
    }

    pub fn compare_exchange_int64_synchronized(
        &mut self,
        expected: Reg,
        new: Reg,
        addr: Reg,
    ) -> Reg {
        self.masm
            .compare_exchange_int64_synchronized(expected, new, addr)
    }

    pub fn fetch_add_int32_synchronized(&mut self, previous: Reg, value: Reg, addr: Reg) -> Reg {
        self.masm
            .fetch_add_int32_synchronized(previous, value, addr)
    }

    pub fn fetch_add_int64_synchronized(&mut self, previous: Reg, value: Reg, addr: Reg) -> Reg {
        self.masm
            .fetch_add_int64_synchronized(previous, value, addr)
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

    pub fn emit_barrier(&mut self, src: Reg, card_table_offset: usize) {
        self.masm.emit_barrier(src, card_table_offset);
    }

    pub fn emit_bailout(&mut self, lbl: Label, trap: Trap, pos: Position) {
        self.masm.emit_bailout(lbl, trap, pos);
    }

    pub fn emit_bailout_inplace(&mut self, trap: Trap, pos: Position) {
        self.masm.emit_bailout_inplace(trap, pos)
    }

    pub fn get_scratch(&self) -> ScratchReg {
        self.masm.get_scratch()
    }

    pub fn cmp_int(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        self.masm.cmp_int(mode, dest, lhs, rhs);
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

    pub fn int_add_checked(
        &mut self,
        mode: MachineMode,
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
        pos: Position,
    ) {
        self.masm.int_add_checked(mode, dest, lhs, rhs, pos);
    }

    pub fn int_add_imm(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, value: i64) {
        self.masm.int_add_imm(mode, dest, lhs, value);
    }

    pub fn int_sub(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        self.masm.int_sub(mode, dest, lhs, rhs);
    }

    pub fn int_sub_checked(
        &mut self,
        mode: MachineMode,
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
        pos: Position,
    ) {
        self.masm.int_sub_checked(mode, dest, lhs, rhs, pos);
    }

    pub fn int_mul(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        self.masm.int_mul(mode, dest, lhs, rhs);
    }

    pub fn int_mul_checked(
        &mut self,
        mode: MachineMode,
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
        pos: Position,
    ) {
        self.masm.int_mul_checked(mode, dest, lhs, rhs, pos);
    }

    pub fn int_div(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg, pos: Position) {
        self.masm.int_div(mode, dest, lhs, rhs, pos);
    }

    pub fn int_mod(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg, pos: Position) {
        self.masm.int_mod(mode, dest, lhs, rhs, pos);
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

    pub fn int_rol(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        self.masm.int_rol(mode, dest, lhs, rhs);
    }

    pub fn int_ror(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        self.masm.int_ror(mode, dest, lhs, rhs);
    }

    pub fn count_bits(&mut self, mode: MachineMode, dest: Reg, src: Reg, count_one_bits: bool) {
        self.masm.count_bits(mode, dest, src, count_one_bits);
    }

    pub fn count_bits_leading(
        &mut self,
        mode: MachineMode,
        dest: Reg,
        src: Reg,
        count_one_bits: bool,
    ) {
        self.masm
            .count_bits_leading(mode, dest, src, count_one_bits);
    }

    pub fn count_bits_trailing(
        &mut self,
        mode: MachineMode,
        dest: Reg,
        src: Reg,
        count_one_bits: bool,
    ) {
        self.masm
            .count_bits_trailing(mode, dest, src, count_one_bits);
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

    pub fn float_abs(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        self.masm.float_abs(mode, dest, src);
    }

    pub fn float_neg(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        self.masm.float_neg(mode, dest, src);
    }

    pub fn float_cmp_nan(&mut self, mode: MachineMode, dest: Reg, src: FReg) {
        self.masm.float_cmp_nan(mode, dest, src);
    }

    pub fn float_cmp_int(&mut self, mode: MachineMode, dest: Reg, lhs: FReg, rhs: FReg) {
        self.masm.float_cmp_int(mode, dest, lhs, rhs);
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

    pub fn fill_zero(&mut self, obj: Reg, array: bool, size: usize) {
        self.masm.fill_zero(obj, array, size);
    }

    pub fn fill_zero_dynamic(&mut self, obj: Reg, obj_end: Reg) {
        self.masm.fill_zero_dynamic(obj, obj_end);
    }

    pub fn load_array_elem(&mut self, mode: MachineMode, dest: AnyReg, array: Reg, index: Reg) {
        self.masm.load_array_elem(mode, dest, array, index);
    }

    pub fn array_address(&mut self, dest: Reg, obj: Reg, index: Reg, element_size: i32) {
        self.masm.array_address(dest, obj, index, element_size);
    }

    pub fn float_round_halfeven(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        self.masm.float_round_halfeven(mode, dest, src);
    }

    pub fn float_sqrt(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        self.masm.float_sqrt(mode, dest, src);
    }

    pub fn copy(&mut self, mode: MachineMode, dest: AnyReg, src: AnyReg) {
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

    pub fn float32_to_float64(&mut self, dest: FReg, src: FReg) {
        self.masm.float32_to_float64(dest, src);
    }

    pub fn float64_to_float32(&mut self, dest: FReg, src: FReg) {
        self.masm.float64_to_float32(dest, src);
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

    pub fn float_as_int(
        &mut self,
        dest_mode: MachineMode,
        dest: Reg,
        src_mode: MachineMode,
        src: FReg,
    ) {
        self.masm.float_as_int(dest_mode, dest, src_mode, src);
    }

    pub fn int_as_float(
        &mut self,
        dest_mode: MachineMode,
        dest: FReg,
        src_mode: MachineMode,
        src: Reg,
    ) {
        self.masm.int_as_float(dest_mode, dest, src_mode, src);
    }

    pub fn emit_positon(&mut self, position: Position) {
        self.masm.emit_position(position);
    }

    pub fn var_store(&mut self, offset: i32, ty: SourceType, src: AnyReg) {
        self.masm.store_mem(ty.mode(), Mem::Local(offset), src);
    }

    pub fn var_load(&mut self, offset: i32, ty: SourceType, dest: AnyReg) {
        self.masm.load_mem(ty.mode(), dest, Mem::Local(offset));
    }

    pub fn jit(mut self, stacksize: i32, desc: JitDescriptor) -> Code {
        self.masm.debug();
        self.slow_paths();
        self.masm.jit(self.vm, stacksize, desc)
    }

    pub fn native_call(
        &mut self,
        internal_fct: NativeFct,
        pos: Position,
        gcpoint: GcPoint,
        dest: AnyReg,
    ) {
        let ty = internal_fct.return_type.clone();
        let ptr = ensure_native_stub(self.vm, None, internal_fct);

        self.masm.raw_call(ptr.to_ptr());

        let mode = if ty.is_unit() { None } else { Some(ty.mode()) };
        self.call_epilog(pos, mode, dest, gcpoint);
    }

    pub fn direct_call(
        &mut self,
        fct_id: FctDefinitionId,
        ptr: *const u8,
        type_params: SourceTypeArray,
        pos: Position,
        gcpoint: GcPoint,
        return_mode: Option<MachineMode>,
        dest: AnyReg,
    ) {
        self.masm.direct_call(fct_id, ptr, type_params);
        self.call_epilog(pos, return_mode, dest, gcpoint);
    }

    pub fn indirect_call(
        &mut self,
        fct_id: FctDefinitionId,
        vtable_index: u32,
        self_index: u32,
        pos: Position,
        gcpoint: GcPoint,
        return_mode: Option<MachineMode>,
        type_params: SourceTypeArray,
        dest: AnyReg,
    ) {
        self.masm
            .indirect_call(pos, fct_id, vtable_index, self_index, type_params);
        self.call_epilog(pos, return_mode, dest, gcpoint);
    }

    fn call_epilog(
        &mut self,
        pos: Position,
        mode: Option<MachineMode>,
        dest: AnyReg,
        gcpoint: GcPoint,
    ) {
        self.masm.emit_position(pos);
        self.masm.emit_gcpoint(gcpoint);
        self.copy_result(mode, dest);
    }

    fn copy_result(&mut self, mode: Option<MachineMode>, dest: AnyReg) {
        if let Some(mode) = mode {
            match dest {
                AnyReg::Reg(dest) => {
                    if dest != REG_RESULT {
                        self.masm.copy_reg(mode, dest, REG_RESULT);
                    }
                }

                AnyReg::FReg(dest) => {
                    if dest != FREG_RESULT {
                        self.masm.copy_freg(mode, dest, FREG_RESULT);
                    }
                }
            }
        }
    }

    pub fn gc_allocate(
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

        let internal_fct = NativeFct {
            ptr: Address::from_ptr(stdlib::gc_alloc as *const u8),
            args: &[SourceType::Int64, SourceType::Bool],
            return_type: SourceType::Ptr,
            desc: NativeFctDescriptor::AllocStub,
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
        let lbl_slow_path = self.masm.create_label();

        match size {
            AllocationSize::Dynamic(reg_size) => {
                self.masm
                    .cmp_reg_imm(MachineMode::Ptr, reg_size, TLAB_OBJECT_SIZE as i32);
                self.masm.jump_if(CondCode::GreaterEq, lbl_slow_path);
            }

            AllocationSize::Fixed(size) => {
                assert!(size < TLAB_OBJECT_SIZE);
            }
        }

        let tlab_next = self.masm.get_scratch();
        let tlab_end = self.masm.get_scratch();

        self.masm.load_mem(
            MachineMode::Ptr,
            (*tlab_next).into(),
            Mem::Base(REG_THREAD, ThreadLocalData::tlab_top_offset()),
        );

        self.masm.load_mem(
            MachineMode::Ptr,
            (*tlab_end).into(),
            Mem::Base(REG_THREAD, ThreadLocalData::tlab_end_offset()),
        );

        let temp;

        match size {
            AllocationSize::Fixed(size) => {
                temp = REG_TMP1;
                self.masm.copy_reg(MachineMode::Ptr, temp, *tlab_next);
                self.masm
                    .int_add_imm(MachineMode::Ptr, *tlab_next, *tlab_next, size as i64);
            }

            AllocationSize::Dynamic(reg_size) => {
                temp = if reg_size == REG_TMP1 {
                    REG_TMP2
                } else {
                    REG_TMP1
                };
                self.masm.copy_reg(MachineMode::Ptr, temp, *tlab_next);
                self.masm
                    .int_add(MachineMode::Ptr, *tlab_next, *tlab_next, reg_size);
            }
        }

        self.masm.cmp_reg(MachineMode::Ptr, *tlab_next, *tlab_end);
        self.masm.jump_if(CondCode::Greater, lbl_slow_path);

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_THREAD, ThreadLocalData::tlab_top_offset()),
            (*tlab_next).into(),
        );

        if dest != temp {
            self.masm.copy_reg(MachineMode::Ptr, dest, temp);
        }

        let lbl_return = self.masm.create_label();
        self.masm.bind_label(lbl_return);

        self.slow_paths.push(SlowPathKind::TlabAllocationFailure(
            lbl_slow_path,
            lbl_return,
            dest,
            size,
            pos,
            array_ref,
            gcpoint,
        ));
    }

    pub fn allocate(
        &mut self,
        dest: Reg,
        size: AllocationSize,
        pos: Position,
        array_ref: bool,
        gcpoint: GcPoint,
    ) {
        if self.vm.args.flag_disable_tlab {
            self.gc_allocate(dest, size, pos, array_ref, gcpoint);
            return;
        }

        match size {
            AllocationSize::Fixed(fixed_size) => {
                if fixed_size < TLAB_OBJECT_SIZE {
                    self.tlab_allocate(dest, size, pos, array_ref, gcpoint);
                } else {
                    self.gc_allocate(dest, size, pos, array_ref, gcpoint);
                }
            }

            AllocationSize::Dynamic(_) => {
                self.tlab_allocate(dest, size, pos, array_ref, gcpoint);
            }
        }
    }

    pub fn ensure_global(
        &mut self,
        glob: &GlobalDefinition,
        fid: FctDefinitionId,
        ptr: Address,
        position: Position,
        gcpoint: GcPoint,
    ) {
        let lbl_global = self.masm.create_label();
        let lbl_return = self.masm.create_label();

        let disp = self.masm.add_addr(glob.address_init.to_ptr());
        let pos = self.masm.pos() as i32;
        self.masm.load_constpool(REG_RESULT, disp + pos);
        self.masm.load_mem(
            MachineMode::Int8,
            REG_RESULT.into(),
            Mem::Base(REG_RESULT, 0),
        );
        self.masm.cmp_reg_imm(MachineMode::Ptr, REG_RESULT, 0);
        self.masm.jump_if(CondCode::Zero, lbl_global);
        self.masm.bind_label(lbl_return);

        self.slow_paths.push(SlowPathKind::InitializeGlobal(
            lbl_global, lbl_return, fid, ptr, position, gcpoint,
        ));
    }

    fn slow_paths(&mut self) {
        let slow_paths = mem::replace(&mut self.slow_paths, Vec::new());

        for slow_path in slow_paths {
            match slow_path {
                SlowPathKind::TlabAllocationFailure(
                    lbl_start,
                    lbl_return,
                    dest,
                    size,
                    pos,
                    array_ref,
                    gcpoint,
                ) => {
                    self.slow_path_tlab_allocation_failure(
                        lbl_start, lbl_return, dest, size, pos, array_ref, gcpoint,
                    );
                }

                SlowPathKind::StackOverflow(lbl_start, lbl_return, pos, gcpoint) => {
                    self.slow_path_stack_overflow(lbl_start, lbl_return, pos, gcpoint);
                }

                SlowPathKind::InitializeGlobal(
                    lbl_start,
                    lbl_return,
                    fct_id,
                    ptr,
                    pos,
                    gcpoint,
                ) => {
                    self.slow_path_global(lbl_start, lbl_return, fct_id, ptr, pos, gcpoint);
                }

                SlowPathKind::Assert(lbl_start, pos) => {
                    self.slow_path_assert(lbl_start, pos);
                }

                SlowPathKind::Safepoint(lbl_start, lbl_return, pos, gcpoint) => {
                    self.slow_path_safepoint(lbl_start, lbl_return, pos, gcpoint);
                }
            }
        }

        self.masm.debug();
        assert!(self.slow_paths.is_empty());
    }

    fn slow_path_tlab_allocation_failure(
        &mut self,
        lbl_start: Label,
        lbl_return: Label,
        dest: Reg,
        size: AllocationSize,
        pos: Position,
        array_ref: bool,
        gcpoint: GcPoint,
    ) {
        self.masm.bind_label(lbl_start);
        self.masm.emit_comment("slow path tlab allocation".into());
        self.gc_allocate(dest, size, pos, array_ref, gcpoint);
        self.masm.jump(lbl_return);
    }

    fn slow_path_stack_overflow(
        &mut self,
        lbl_stack_overflow: Label,
        lbl_return: Label,
        pos: Position,
        gcpoint: GcPoint,
    ) {
        self.masm.bind_label(lbl_stack_overflow);
        self.masm.emit_comment("slow path stack overflow".into());
        self.masm.raw_call(self.vm.stack_overflow_stub().to_ptr());
        self.masm.emit_gcpoint(gcpoint);
        self.masm.emit_position(pos);
        self.masm.jump(lbl_return);
    }

    fn slow_path_safepoint(
        &mut self,
        lbl_start: Label,
        lbl_return: Label,
        pos: Position,
        gcpoint: GcPoint,
    ) {
        self.masm.bind_label(lbl_start);
        self.masm.emit_comment("slow path safepoint".into());
        self.masm.raw_call(self.vm.safepoint_stub().to_ptr());
        self.masm.emit_gcpoint(gcpoint);
        self.masm.emit_position(pos);
        self.masm.jump(lbl_return);
    }

    fn slow_path_global(
        &mut self,
        lbl_start: Label,
        lbl_return: Label,
        fct_id: FctDefinitionId,
        ptr: Address,
        pos: Position,
        gcpoint: GcPoint,
    ) {
        self.masm.bind_label(lbl_start);
        self.direct_call(
            fct_id,
            ptr.to_ptr(),
            SourceTypeArray::empty(),
            pos,
            gcpoint,
            None,
            REG_RESULT.into(),
        );
        self.masm.jump(lbl_return);
    }

    fn slow_path_assert(&mut self, lbl_assert: Label, pos: Position) {
        self.masm.bind_label(lbl_assert);
        self.masm.emit_comment("slow path assert".into());
        self.masm
            .load_int_const(MachineMode::Int32, REG_PARAMS[0], Trap::ASSERT.int() as i64);
        self.masm.raw_call(self.vm.trap_stub().to_ptr());
        self.masm.emit_gcpoint(GcPoint::new());
        self.masm.emit_position(pos);
    }
}

enum SlowPathKind {
    TlabAllocationFailure(Label, Label, Reg, AllocationSize, Position, bool, GcPoint),
    StackOverflow(Label, Label, Position, GcPoint),
    Safepoint(Label, Label, Position, GcPoint),
    Assert(Label, Position),
    InitializeGlobal(Label, Label, FctDefinitionId, Address, Position, GcPoint),
}
