use std::mem;

use crate::Shape;
use crate::cannon::codegen::{RegOrOffset, mode, result_passed_as_argument, result_reg_mode, size};
use crate::compiler::CompilationMode;
use crate::compiler::codegen::{AllocationSize, AnyReg};
use crate::cpu::{
    FREG_RESULT, FReg, REG_PARAMS, REG_RESULT, REG_SP, REG_THREAD, REG_TMP1, Reg,
    STACK_FRAME_ALIGNMENT,
};
use crate::gc::Address;
use crate::gc::tlab::MAX_TLAB_OBJECT_SIZE;
use crate::masm::{CondCode, Label, MacroAssembler, Mem, ScratchReg};
use crate::mirror::Header;
use crate::mode::MachineMode;
use crate::threads::ThreadLocalData;
use crate::vm::{
    CodeDescriptor, EnumLayout, GcPoint, INITIALIZED, LazyCompilationSite, Trap, VM,
    create_enum_instance, create_struct_instance, get_concrete_tuple_bty_array,
};
use dora_bytecode::{BytecodeType, BytecodeTypeArray, FunctionId, GlobalId, Location, StructId};

pub struct BaselineAssembler<'a> {
    masm: MacroAssembler,
    vm: &'a VM,
    slow_paths: Vec<SlowPathKind>,
    mode: CompilationMode,
}

impl<'a> BaselineAssembler<'a> {
    pub fn new(vm: &'a VM, mode: CompilationMode) -> BaselineAssembler<'a> {
        BaselineAssembler {
            masm: MacroAssembler::new(),
            vm,
            slow_paths: Vec::new(),
            mode,
        }
    }

    pub fn debug(&mut self) {
        self.masm.debug();
    }

    pub fn prolog(&mut self, stacksize: i32) {
        self.masm.prolog(stacksize);
    }

    pub fn check_stack_limit(&mut self, location: Location, gcpoint: GcPoint) {
        let lbl_stack_overflow = self.masm.create_label();
        self.masm.check_stack_limit(lbl_stack_overflow);
        let lbl_return = self.masm.create_label();
        self.masm.bind_label(lbl_return);

        self.slow_paths.push(SlowPathKind::StackOverflow(
            lbl_stack_overflow,
            lbl_return,
            location,
            gcpoint,
        ));
    }

    pub fn safepoint(&mut self, location: Location, gcpoint: GcPoint) {
        let lbl_safepoint = self.masm.create_label();
        self.masm.safepoint(lbl_safepoint);
        let lbl_return = self.masm.create_label();
        self.masm.bind_label(lbl_return);

        self.slow_paths.push(SlowPathKind::Safepoint(
            lbl_safepoint,
            lbl_return,
            location,
            gcpoint,
        ));
    }

    pub fn assert(&mut self, value: Reg, location: Location) {
        let lbl_assert = self.masm.create_label();
        self.masm
            .test_and_jump_if(MachineMode::Int8, CondCode::Zero, value, lbl_assert);

        self.slow_paths
            .push(SlowPathKind::Assert(lbl_assert, location));
    }

    pub fn epilog(&mut self) {
        self.masm.epilog();
    }

    pub fn increase_stack_frame(&mut self, size: i32) {
        debug_assert_eq!(size % STACK_FRAME_ALIGNMENT as i32, 0);
        self.masm.increase_stack_frame(size);
    }

    pub fn decrease_stack_frame(&mut self, size: i32) {
        debug_assert_eq!(size % STACK_FRAME_ALIGNMENT as i32, 0);
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

    pub fn jump_reg(&mut self, reg: Reg) {
        self.masm.jump_reg(reg);
    }

    pub fn jump_if(&mut self, cond: CondCode, label: Label) {
        self.masm.jump_if(cond, label);
    }

    pub fn bailout_if(&mut self, cond: CondCode, trap: Trap, location: Location) {
        self.masm.bailout_if(cond, trap, location);
    }

    pub fn pos(&self) -> usize {
        self.masm.pos()
    }

    pub fn copy_bytecode_ty(&mut self, ty: BytecodeType, dest: RegOrOffset, src: RegOrOffset) {
        match ty {
            BytecodeType::Unit => {
                // nothing to do
            }

            BytecodeType::Tuple(subtypes) => {
                self.copy_tuple(subtypes, dest, src);
            }

            BytecodeType::Enum(enum_id, type_params) => {
                let enum_instance_id = create_enum_instance(self.vm, enum_id, type_params);
                let enum_instance = self.vm.enum_instances.idx(enum_instance_id);

                let mode = match enum_instance.layout {
                    EnumLayout::Int => MachineMode::Int32,
                    EnumLayout::Ptr | EnumLayout::Tagged => MachineMode::Ptr,
                };

                let reg = self.get_scratch();
                self.load_mem(mode, (*reg).into(), src.mem());
                self.store_mem(mode, dest.mem(), (*reg).into());
            }

            BytecodeType::Struct(struct_id, type_params) => {
                self.copy_struct(struct_id, type_params, dest, src);
            }

            BytecodeType::TypeAlias(..)
            | BytecodeType::Assoc { .. }
            | BytecodeType::GenericAssoc { .. }
            | BytecodeType::TypeParam(_)
            | BytecodeType::This => {
                unreachable!()
            }

            BytecodeType::Ptr
            | BytecodeType::TraitObject(..)
            | BytecodeType::Class(..)
            | BytecodeType::Lambda(..) => {
                let mode: MachineMode = MachineMode::Ptr;
                let reg = self.get_scratch();
                self.load_mem(mode, (*reg).into(), src.mem());
                self.store_mem(mode, dest.mem(), (*reg).into());
            }

            BytecodeType::Char
            | BytecodeType::Int32
            | BytecodeType::Int64
            | BytecodeType::Bool
            | BytecodeType::UInt8 => {
                self.emit_comment(format!("broken copy bytecode {:?}", ty));
                let mode = mode(self.vm, ty);
                let reg = self.masm.get_scratch();
                self.load_mem(mode, (*reg).into(), src.mem());
                self.store_mem(mode, dest.mem(), (*reg).into());
            }

            BytecodeType::Float32 | BytecodeType::Float64 => {
                let value_reg = self.masm.get_scratch();
                let mode = if ty == BytecodeType::Float32 {
                    MachineMode::Int32
                } else {
                    MachineMode::Int64
                };

                self.load_mem(mode, (*value_reg).into(), src.mem());
                self.store_mem(mode, dest.mem(), (*value_reg).into());
            }
        }
    }

    pub fn copy_tuple(&mut self, subtypes: BytecodeTypeArray, dest: RegOrOffset, src: RegOrOffset) {
        let tuple = get_concrete_tuple_bty_array(self.vm, subtypes.clone());

        for (subtype, &subtype_offset) in subtypes.iter().zip(tuple.offsets()) {
            let src = src.offset(subtype_offset);
            let dest = dest.offset(subtype_offset);
            self.copy_bytecode_ty(subtype, dest, src);
        }
    }

    pub fn copy_struct(
        &mut self,
        struct_id: StructId,
        type_params: BytecodeTypeArray,
        dest: RegOrOffset,
        src: RegOrOffset,
    ) {
        let struct_instance_id = create_struct_instance(self.vm, struct_id, type_params.clone());
        let struct_instance = self.vm.struct_instances.idx(struct_instance_id);

        for field in &struct_instance.fields {
            let src = src.offset(field.offset);
            let dest = dest.offset(field.offset);
            self.copy_bytecode_ty(field.ty.clone(), dest, src);
        }
    }

    pub fn store_mem(&mut self, mode: MachineMode, mem: Mem, src: AnyReg) {
        self.masm.store_mem(mode, mem, src);
    }

    pub fn store_zero(&mut self, mode: MachineMode, mem: Mem) {
        self.masm.store_zero(mode, mem);
    }

    pub fn store_field(
        &mut self,
        host_reg: Reg,
        host_offset: i32,
        value: RegOrOffset,
        ty: BytecodeType,
    ) {
        match &ty {
            BytecodeType::Unit => {
                // nothing to do
            }

            BytecodeType::Tuple(subtypes) => {
                let tuple = get_concrete_tuple_bty_array(self.vm, subtypes.clone());

                for (subtype, &subtype_offset) in subtypes.iter().zip(tuple.offsets()) {
                    self.store_field(
                        host_reg,
                        host_offset + subtype_offset,
                        value.offset(subtype_offset),
                        subtype,
                    );
                }
            }

            BytecodeType::Struct(struct_id, type_params) => {
                let struct_instance_id =
                    create_struct_instance(self.vm, *struct_id, type_params.clone());
                let struct_instance = self.vm.struct_instances.idx(struct_instance_id);

                for field in &struct_instance.fields {
                    self.store_field(
                        host_reg,
                        host_offset + field.offset,
                        value.offset(field.offset),
                        field.ty.clone(),
                    );
                }
            }

            BytecodeType::Enum(enum_id, type_params) => {
                let value_reg = self.masm.get_scratch();

                let enum_instance_id = create_enum_instance(self.vm, *enum_id, type_params.clone());
                let enum_instance = self.vm.enum_instances.idx(enum_instance_id);

                let mode = match enum_instance.layout {
                    EnumLayout::Int => MachineMode::Int32,
                    EnumLayout::Ptr | EnumLayout::Tagged => MachineMode::Ptr,
                };

                self.load_mem(mode, (*value_reg).into(), value.mem());
                self.store_mem(mode, Mem::Base(host_reg, host_offset), (*value_reg).into());

                if self.vm.gc.needs_write_barrier() && mode == MachineMode::Ptr {
                    self.emit_write_barrier(host_reg, (*value_reg).into());
                }
            }

            BytecodeType::TypeAlias(..)
            | BytecodeType::Assoc { .. }
            | BytecodeType::GenericAssoc { .. }
            | BytecodeType::TypeParam(_)
            | BytecodeType::Lambda(_, _)
            | BytecodeType::This => {
                unreachable!()
            }

            BytecodeType::UInt8
            | BytecodeType::Bool
            | BytecodeType::Char
            | BytecodeType::Int32
            | BytecodeType::Int64 => {
                let value_reg = self.masm.get_scratch();
                let mode = mode(self.vm, ty.clone());

                self.load_mem(mode, (*value_reg).into(), value.mem());
                self.store_mem(mode, Mem::Base(host_reg, host_offset), (*value_reg).into());
            }

            BytecodeType::Float32 | BytecodeType::Float64 => {
                let value_reg = self.masm.get_scratch();
                let mode = if ty == BytecodeType::Float32 {
                    MachineMode::Int32
                } else {
                    assert_eq!(ty, BytecodeType::Float64);
                    MachineMode::Int64
                };

                self.load_mem(mode, (*value_reg).into(), value.mem());
                self.store_mem(mode, Mem::Base(host_reg, host_offset), (*value_reg).into());
            }

            BytecodeType::Ptr | BytecodeType::TraitObject(..) | BytecodeType::Class(..) => {
                let value_reg = self.masm.get_scratch();
                let mode = MachineMode::Ptr;

                self.load_mem(mode, (*value_reg).into(), value.mem());
                self.store_mem(mode, Mem::Base(host_reg, host_offset), (*value_reg).into());

                if self.vm.gc.needs_write_barrier() {
                    self.emit_write_barrier(host_reg, (*value_reg).into());
                }
            }
        }
    }

    pub fn store_array(
        &mut self,
        arr_reg: Reg,
        element_reg: Reg,
        offset: i32,
        value: RegOrOffset,
        ty: BytecodeType,
    ) {
        match ty {
            BytecodeType::Unit => {}

            BytecodeType::Tuple(ref subtypes) => {
                let tuple = get_concrete_tuple_bty_array(self.vm, subtypes.clone());

                for (subtype, &subtype_offset) in subtypes.iter().zip(tuple.offsets()) {
                    self.store_array(
                        arr_reg,
                        element_reg,
                        offset + subtype_offset,
                        value.offset(subtype_offset),
                        subtype,
                    );
                }
            }

            BytecodeType::Struct(struct_id, type_params) => {
                let struct_instance_id =
                    create_struct_instance(self.vm, struct_id, type_params.clone());
                let struct_instance = self.vm.struct_instances.idx(struct_instance_id);

                for field in &struct_instance.fields {
                    self.store_array(
                        arr_reg,
                        element_reg,
                        offset + field.offset,
                        value.offset(field.offset),
                        field.ty.clone(),
                    );
                }
            }

            BytecodeType::Enum(enum_id, type_params) => {
                let value_reg = self.get_scratch();

                let enum_instance_id = create_enum_instance(self.vm, enum_id, type_params);
                let enum_instance = self.vm.enum_instances.idx(enum_instance_id);

                let mode = match enum_instance.layout {
                    EnumLayout::Int => MachineMode::Int32,
                    EnumLayout::Ptr | EnumLayout::Tagged => MachineMode::Ptr,
                };

                self.load_mem(mode, (*value_reg).into(), value.mem());
                self.store_mem(mode, Mem::Base(element_reg, offset), (*value_reg).into());

                if self.vm.gc.needs_write_barrier() && mode == MachineMode::Ptr {
                    self.emit_write_barrier(arr_reg, *value_reg);
                }
            }

            BytecodeType::TypeAlias(..)
            | BytecodeType::Assoc { .. }
            | BytecodeType::GenericAssoc { .. }
            | BytecodeType::TypeParam(_)
            | BytecodeType::Lambda(_, _)
            | BytecodeType::This => {
                unreachable!()
            }

            BytecodeType::UInt8
            | BytecodeType::Int32
            | BytecodeType::Bool
            | BytecodeType::Char
            | BytecodeType::Int64 => {
                let mode = mode(self.vm, ty.clone());
                let value_reg = self.get_scratch();
                self.load_mem(mode, (*value_reg).into(), value.mem());
                self.store_mem(mode, Mem::Base(element_reg, offset), (*value_reg).into());
            }

            BytecodeType::Ptr | BytecodeType::TraitObject(..) | BytecodeType::Class(..) => {
                let mode = mode(self.vm, ty.clone());

                let value_reg = self.get_scratch();

                self.load_mem(mode, (*value_reg).into(), value.mem());
                self.store_mem(mode, Mem::Base(element_reg, offset), (*value_reg).into());

                if self.vm.gc.needs_write_barrier() {
                    self.emit_write_barrier(arr_reg, *value_reg);
                }
            }

            BytecodeType::Float32 | BytecodeType::Float64 => {
                let value_reg = self.masm.get_scratch();
                let mode = if ty == BytecodeType::Float32 {
                    MachineMode::Int32
                } else {
                    assert_eq!(ty, BytecodeType::Float64);
                    MachineMode::Int64
                };

                self.load_mem(mode, (*value_reg).into(), value.mem());
                self.store_mem(mode, Mem::Base(element_reg, offset), (*value_reg).into());
            }
        }
    }

    pub fn lea(&mut self, dest: Reg, mem: Mem) {
        self.masm.lea(dest, mem);
    }

    pub fn lea_label(&mut self, dest: Reg, label: Label) {
        self.masm.lea_label(dest, label);
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

    pub fn store_int8_synchronized(&mut self, src: Reg, addr: Reg) {
        self.masm.store_int8_synchronized(src, addr);
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

    pub fn test_and_jump_if(&mut self, mode: MachineMode, cond: CondCode, reg: Reg, lbl: Label) {
        self.masm.test_and_jump_if(mode, cond, reg, lbl);
    }

    pub fn test_if_nil_bailout(&mut self, location: Location, reg: Reg, trap: Trap) {
        self.masm.test_if_nil_bailout(location, reg, trap);
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

    pub fn emit_write_barrier(&mut self, host: Reg, value: Reg) {
        let lbl_marking_slow_path = self.masm.emit_marking_barrier_fast_path();

        let lbl_slow_path = self.masm.emit_object_write_barrier_fast_path(host);
        let lbl_return = self.masm.create_and_bind_label();

        self.slow_paths.push(SlowPathKind::MarkingWriteBarrier {
            lbl_start: lbl_marking_slow_path,
            lbl_return,
            host,
            value,
        });

        self.slow_paths.push(SlowPathKind::ObjectWriteBarrier {
            lbl_start: lbl_slow_path,
            lbl_return,
            host,
            value,
        });
    }

    pub fn emit_jump_table(&mut self, targets: Vec<Label>) -> Label {
        self.masm.emit_jump_table(targets)
    }

    pub fn emit_bailout(&mut self, lbl: Label, trap: Trap, location: Location) {
        self.masm.emit_bailout(lbl, trap, location);
    }

    pub fn get_scratch(&self) -> ScratchReg {
        self.masm.get_scratch()
    }

    pub fn cmp_ordering(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        self.masm.cmp_ordering(mode, dest, lhs, rhs);
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
        location: Location,
    ) {
        self.masm.int_add_checked(mode, dest, lhs, rhs, location);
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
        location: Location,
    ) {
        self.masm.int_sub_checked(mode, dest, lhs, rhs, location);
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
        location: Location,
    ) {
        self.masm.int_mul_checked(mode, dest, lhs, rhs, location);
    }

    pub fn int_div_checked(
        &mut self,
        mode: MachineMode,
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
        location: Location,
    ) {
        self.masm.int_div_checked(mode, dest, lhs, rhs, location);
    }

    pub fn int_mod_checked(
        &mut self,
        mode: MachineMode,
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
        location: Location,
    ) {
        self.masm.int_mod_checked(mode, dest, lhs, rhs, location);
    }

    pub fn int_neg(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        self.masm.int_neg(mode, dest, src);
    }

    pub fn int_neg_checked(&mut self, mode: MachineMode, dest: Reg, src: Reg, location: Location) {
        self.masm.int_neg_checked(mode, dest, src, location);
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

    pub fn float_cmp_ordering(&mut self, mode: MachineMode, dest: Reg, lhs: FReg, rhs: FReg) {
        self.masm.float_cmp_ordering(mode, dest, lhs, rhs);
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

    pub fn float_round_tozero(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        self.masm.float_round_tozero(mode, dest, src);
    }

    pub fn float_round_up(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        self.masm.float_round_up(mode, dest, src);
    }

    pub fn float_round_down(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        self.masm.float_round_down(mode, dest, src);
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

    pub fn check_index_out_of_bounds(&mut self, location: Location, array: Reg, index: Reg) {
        self.masm.check_index_out_of_bounds(location, array, index);
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

    pub fn emit_positon(&mut self, location: Location) {
        self.masm.emit_position(location);
    }

    pub fn code(mut self) -> CodeDescriptor {
        self.masm.debug();
        self.slow_paths();
        self.masm.code()
    }

    pub fn runtime_call(&mut self, address: Address, location: Location, gcpoint: GcPoint) {
        self.masm.raw_call(address);
        self.call_epilog(location, None, REG_RESULT.into(), gcpoint);
    }

    pub fn direct_call(
        &mut self,
        fct_id: FunctionId,
        type_params: BytecodeTypeArray,
        ptr: Address,
        location: Location,
        gcpoint: GcPoint,
        return_mode: Option<MachineMode>,
        dest: AnyReg,
    ) {
        self.masm.direct_call(fct_id, ptr, type_params);
        self.call_epilog(location, return_mode, dest, gcpoint);
    }

    pub fn virtual_call(
        &mut self,
        vtable_index: u32,
        self_index: u32,
        location: Location,
        gcpoint: GcPoint,
        return_mode: Option<MachineMode>,
        dest: AnyReg,
        lazy_compilation_site: LazyCompilationSite,
    ) {
        self.masm.virtual_call(
            location,
            vtable_index,
            self_index,
            lazy_compilation_site,
            self.vm.meta_space_start(),
        );
        self.call_epilog(location, return_mode, dest, gcpoint);
    }

    fn call_epilog(
        &mut self,
        location: Location,
        mode: Option<MachineMode>,
        dest: AnyReg,
        gcpoint: GcPoint,
    ) {
        self.masm.emit_position(location);
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

    pub fn thread_current(&mut self, dest: Reg) {
        self.masm.load_mem(
            MachineMode::Ptr,
            dest.into(),
            Mem::Base(REG_THREAD, ThreadLocalData::managed_thread_handle_offset()),
        );
        self.masm
            .load_mem(MachineMode::Ptr, dest.into(), Mem::Base(dest, 0));
    }

    pub fn gc_allocate(
        &mut self,
        dest: Reg,
        size: AllocationSize,
        location: Location,
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

        self.masm
            .raw_call(self.vm.native_methods.gc_allocation_trampoline());
        self.call_epilog(location, Some(MachineMode::Ptr), dest.into(), gcpoint);
        self.masm.test_if_nil_bailout(location, dest, Trap::OOM);
    }

    pub fn tlab_allocate(
        &mut self,
        object: Reg,
        size: AllocationSize,
        location: Location,
        gcpoint: GcPoint,
    ) {
        let lbl_slow_path = self.masm.create_label();

        match size {
            AllocationSize::Dynamic(reg_size) => {
                self.masm
                    .cmp_reg_imm(MachineMode::Ptr, reg_size, MAX_TLAB_OBJECT_SIZE as i32);
                self.masm.jump_if(CondCode::GreaterEq, lbl_slow_path);
            }

            AllocationSize::Fixed(size) => {
                assert!(size < MAX_TLAB_OBJECT_SIZE);
            }
        }

        let tlab_top = self.masm.get_scratch();
        let tlab_limit = self.masm.get_scratch();

        self.masm.load_mem(
            MachineMode::Ptr,
            object.into(),
            Mem::Base(REG_THREAD, ThreadLocalData::tlab_top_offset()),
        );

        self.masm.load_mem(
            MachineMode::Ptr,
            (*tlab_limit).into(),
            Mem::Base(REG_THREAD, ThreadLocalData::tlab_end_offset()),
        );

        self.masm.copy_reg(MachineMode::Ptr, *tlab_top, object);

        match size {
            AllocationSize::Fixed(size) => {
                self.masm
                    .int_add_imm(MachineMode::Ptr, *tlab_top, *tlab_top, size as i64);
            }

            AllocationSize::Dynamic(reg_size) => {
                self.masm
                    .int_add(MachineMode::Ptr, *tlab_top, *tlab_top, reg_size);
            }
        }

        self.masm.cmp_reg(MachineMode::Ptr, *tlab_top, *tlab_limit);
        self.masm.jump_if(CondCode::Greater, lbl_slow_path);

        self.masm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_THREAD, ThreadLocalData::tlab_top_offset()),
            (*tlab_top).into(),
        );

        let lbl_return = self.masm.create_label();
        self.masm.bind_label(lbl_return);

        self.slow_paths.push(SlowPathKind::TlabAllocationFailure(
            lbl_slow_path,
            lbl_return,
            object,
            size,
            location,
            gcpoint,
        ));
    }

    pub fn initialize_object(&mut self, obj: Reg, shape: &Shape) {
        let size = shape.instance_size();
        let tmp_reg = self.get_scratch();

        // Store shape pointer in object.
        let (is_marked, is_remembered) = self.vm.gc.initial_metadata_value(size as usize, false);
        let header_word_value = Header::compute_header_word(
            shape.address(),
            self.vm.meta_space_start(),
            is_marked,
            is_remembered,
        );

        self.masm.load_int_const(
            MachineMode::IntPtr,
            (*tmp_reg).into(),
            header_word_value as i64,
        );

        self.store_mem(
            MachineMode::Ptr,
            Mem::Base(obj, Header::offset_shape_word() as i32),
            (*tmp_reg).into(),
        );

        // Reset object body to zero.
        self.fill_zero(obj, false, size as usize);
    }

    pub fn initialize_array_header(
        &mut self,
        obj: Reg,
        shape: &Shape,
        length_reg: Reg,
        size_reg: Reg,
    ) {
        let tmp_reg = self.get_scratch();
        assert!(obj != length_reg && length_reg != *tmp_reg && obj != *tmp_reg);

        // store classptr in object
        let header_word_value =
            Header::compute_header_word(shape.address(), self.vm.meta_space_start(), false, false);

        self.masm.load_int_const(
            MachineMode::IntPtr,
            (*tmp_reg).into(),
            header_word_value as i64,
        );

        let remembered_reg = self.get_scratch();
        self.masm.compute_remembered_bit(*remembered_reg, size_reg);

        self.masm
            .int_or(MachineMode::Ptr, *tmp_reg, *tmp_reg, *remembered_reg);

        self.store_mem(
            MachineMode::Ptr,
            Mem::Base(obj, Header::offset_shape_word() as i32),
            (*tmp_reg).into(),
        );

        self.store_mem(
            MachineMode::Ptr,
            Mem::Base(obj, Header::size()),
            length_reg.into(),
        );
    }

    pub fn allocate(
        &mut self,
        dest: Reg,
        size: AllocationSize,
        location: Location,
        gcpoint: GcPoint,
    ) {
        if self.vm.flags.disable_tlab && self.mode.is_jit() {
            self.gc_allocate(dest, size, location, gcpoint);
            return;
        }

        match size {
            AllocationSize::Fixed(fixed_size) => {
                if fixed_size < MAX_TLAB_OBJECT_SIZE {
                    self.tlab_allocate(dest, size, location, gcpoint);
                } else {
                    self.gc_allocate(dest, size, location, gcpoint);
                }
            }

            AllocationSize::Dynamic(_) => {
                self.tlab_allocate(dest, size, location, gcpoint);
            }
        }
    }

    pub fn ensure_global(
        &mut self,
        global_id: GlobalId,
        fid: FunctionId,
        ptr: Address,
        location: Location,
        gcpoint: GcPoint,
    ) {
        let lbl_init_global = self.masm.create_label();
        let lbl_return = self.masm.create_label();

        let address_init = self
            .vm
            .global_variable_memory
            .as_ref()
            .unwrap()
            .address_init(global_id);

        self.masm.load_int_const(
            MachineMode::IntPtr,
            REG_RESULT,
            address_init.to_usize() as i64,
        );
        self.masm.load_int8_synchronized(REG_RESULT, REG_RESULT);
        self.masm
            .cmp_reg_imm(MachineMode::Ptr, REG_RESULT, INITIALIZED as i32);
        self.masm.jump_if(CondCode::NotEqual, lbl_init_global);
        self.masm.bind_label(lbl_return);

        self.slow_paths.push(SlowPathKind::InitializeGlobal(
            lbl_init_global,
            lbl_return,
            global_id,
            fid,
            ptr,
            location,
            gcpoint,
        ));
    }

    pub fn zero_ty(&mut self, ty: BytecodeType, dest: RegOrOffset) {
        match ty {
            BytecodeType::Tuple(_) => {
                let subtypes = ty.tuple_subtypes();
                let tuple = get_concrete_tuple_bty_array(self.vm, subtypes.clone());

                for (subtype, &subtype_offset) in subtypes.iter().zip(tuple.offsets()) {
                    self.zero_ty(subtype.clone(), dest.offset(subtype_offset));
                }
            }

            BytecodeType::Unit => {
                // do nothing
            }

            BytecodeType::Enum(enum_id, type_params) => {
                let enum_instance_id = create_enum_instance(self.vm, enum_id, type_params);
                let enum_instance = self.vm.enum_instances.idx(enum_instance_id);

                let mode = match enum_instance.layout {
                    EnumLayout::Int => MachineMode::Int32,
                    EnumLayout::Ptr | EnumLayout::Tagged => MachineMode::Ptr,
                };

                self.store_zero(mode, dest.mem());
            }

            BytecodeType::Struct(struct_id, type_params) => {
                let struct_instance_id =
                    create_struct_instance(self.vm, struct_id, type_params.clone());
                let struct_instance = self.vm.struct_instances.idx(struct_instance_id);

                for field in &struct_instance.fields {
                    let dest = dest.offset(field.offset);
                    self.zero_ty(field.ty.clone(), dest);
                }
            }

            BytecodeType::Ptr
            | BytecodeType::UInt8
            | BytecodeType::Bool
            | BytecodeType::Char
            | BytecodeType::Int32
            | BytecodeType::Int64
            | BytecodeType::Float32
            | BytecodeType::Float64
            | BytecodeType::Class(..)
            | BytecodeType::TraitObject(..)
            | BytecodeType::Lambda(..) => {
                let mode = mode(self.vm, ty);
                self.store_zero(mode, dest.mem());
            }

            BytecodeType::TypeAlias(..)
            | BytecodeType::Assoc { .. }
            | BytecodeType::GenericAssoc { .. }
            | BytecodeType::TypeParam(_)
            | BytecodeType::This => {
                unreachable!()
            }
        }
    }

    fn slow_paths(&mut self) {
        let slow_paths = mem::replace(&mut self.slow_paths, Vec::new());

        for slow_path in slow_paths {
            self.masm.emit_comment("slow path".into());

            match slow_path {
                SlowPathKind::TlabAllocationFailure(
                    lbl_start,
                    lbl_return,
                    dest,
                    size,
                    pos,
                    gcpoint,
                ) => {
                    self.slow_path_tlab_allocation_failure(
                        lbl_start, lbl_return, dest, size, pos, gcpoint,
                    );
                }

                SlowPathKind::StackOverflow(lbl_start, lbl_return, pos, gcpoint) => {
                    self.slow_path_stack_overflow(lbl_start, lbl_return, pos, gcpoint);
                }

                SlowPathKind::InitializeGlobal(
                    lbl_start,
                    lbl_return,
                    global_id,
                    fct_id,
                    ptr,
                    pos,
                    gcpoint,
                ) => {
                    self.slow_path_global(
                        lbl_start, lbl_return, global_id, fct_id, ptr, pos, gcpoint,
                    );
                }

                SlowPathKind::Assert(lbl_start, pos) => {
                    self.slow_path_assert(lbl_start, pos);
                }

                SlowPathKind::Safepoint(lbl_start, lbl_return, pos, gcpoint) => {
                    self.slow_path_safepoint(lbl_start, lbl_return, pos, gcpoint);
                }

                SlowPathKind::MarkingWriteBarrier {
                    lbl_start,
                    lbl_return,
                    host,
                    value,
                } => {
                    self.slow_path_marking_write_barrier(lbl_start, lbl_return, host, value);
                }

                SlowPathKind::ObjectWriteBarrier {
                    lbl_start,
                    lbl_return,
                    host,
                    value,
                } => {
                    self.slow_path_object_write_barrier(lbl_start, lbl_return, host, value);
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
        location: Location,
        gcpoint: GcPoint,
    ) {
        self.masm.bind_label(lbl_start);
        self.masm.emit_comment("slow path tlab allocation".into());
        self.gc_allocate(dest, size, location, gcpoint);
        self.masm.jump(lbl_return);
    }

    fn slow_path_stack_overflow(
        &mut self,
        lbl_stack_overflow: Label,
        lbl_return: Label,
        location: Location,
        gcpoint: GcPoint,
    ) {
        self.masm.bind_label(lbl_stack_overflow);
        self.masm.emit_comment("slow path stack overflow".into());
        self.masm
            .raw_call(self.vm.native_methods.stack_overflow_trampoline());
        self.masm.emit_gcpoint(gcpoint);
        self.masm.emit_position(location);
        self.masm.jump(lbl_return);
    }

    fn slow_path_safepoint(
        &mut self,
        lbl_start: Label,
        lbl_return: Label,
        location: Location,
        gcpoint: GcPoint,
    ) {
        self.masm.bind_label(lbl_start);
        self.masm.emit_comment("slow path safepoint".into());
        self.masm
            .raw_call(self.vm.native_methods.safepoint_trampoline());
        self.masm.emit_gcpoint(gcpoint);
        self.masm.emit_position(location);
        self.masm.jump(lbl_return);
    }

    fn slow_path_marking_write_barrier(
        &mut self,
        lbl_start: Label,
        lbl_return: Label,
        _host: Reg,
        _value: Reg,
    ) {
        self.masm.bind_label(lbl_start);
        self.masm
            .emit_comment("slow path marking write barrier".into());
        self.masm.debug();
        self.masm.jump(lbl_return);
    }

    fn slow_path_object_write_barrier(
        &mut self,
        lbl_start: Label,
        lbl_return: Label,
        obj: Reg,
        value: Reg,
    ) {
        self.masm
            .emit_comment("slow path object write barrier".into());
        self.masm.bind_label(lbl_start);
        self.masm
            .test_and_jump_if(MachineMode::Ptr, CondCode::Zero, value, lbl_return);
        self.masm.copy_reg(MachineMode::Ptr, REG_PARAMS[0], obj);
        self.masm.copy_reg(MachineMode::Ptr, REG_PARAMS[1], value);
        let ptr = Address::from_ptr(crate::gc::swiper::object_write_barrier_slow_path as *const u8);
        self.masm.raw_call(ptr);
        self.masm.jump(lbl_return);
    }

    fn slow_path_global(
        &mut self,
        lbl_start: Label,
        lbl_return: Label,
        global_id: GlobalId,
        fct_id: FunctionId,
        ptr: Address,
        location: Location,
        gcpoint: GcPoint,
    ) {
        self.masm.bind_label(lbl_start);
        let ty = self.vm.global(global_id).ty.clone();
        let ty_size =
            crate::mem::align_i32(size(self.vm, ty.clone()), STACK_FRAME_ALIGNMENT as i32);

        let store_result_on_stack = result_passed_as_argument(ty.clone());

        if store_result_on_stack {
            self.increase_stack_frame(ty_size);
            self.copy_reg(MachineMode::Ptr, REG_PARAMS[0], REG_SP);
        }

        self.direct_call(
            fct_id,
            BytecodeTypeArray::empty(),
            ptr,
            location,
            gcpoint,
            None,
            REG_RESULT.into(),
        );

        let address_value = self
            .vm
            .global_variable_memory
            .as_ref()
            .unwrap()
            .address_value(global_id);
        self.masm.load_int_const(
            MachineMode::IntPtr,
            REG_TMP1,
            address_value.to_usize() as i64,
        );

        if store_result_on_stack {
            self.copy_bytecode_ty(
                ty.clone(),
                RegOrOffset::Reg(REG_TMP1),
                RegOrOffset::Reg(REG_SP),
            );
            self.decrease_stack_frame(ty_size);
        } else if !ty.is_unit() {
            let ty_mode = mode(self.vm, ty.clone());
            self.masm
                .store_mem(ty_mode, Mem::Base(REG_TMP1, 0), result_reg_mode(ty_mode));
        }

        let address_init = self
            .vm
            .global_variable_memory
            .as_ref()
            .unwrap()
            .address_init(global_id);

        self.masm.load_int_const(
            MachineMode::IntPtr,
            REG_RESULT,
            address_init.to_usize() as i64,
        );
        self.masm
            .load_int_const(MachineMode::Int32, REG_TMP1, INITIALIZED as i64);
        self.masm.store_int8_synchronized(REG_TMP1, REG_RESULT);

        self.masm.jump(lbl_return);
    }

    fn slow_path_assert(&mut self, lbl_assert: Label, location: Location) {
        self.masm.bind_label(lbl_assert);
        self.masm.emit_comment("slow path assert".into());
        self.masm
            .load_int_const(MachineMode::Int32, REG_PARAMS[0], Trap::ASSERT as i64);
        self.masm.raw_call(self.vm.native_methods.trap_trampoline());
        self.masm.emit_gcpoint(GcPoint::new());
        self.masm.emit_position(location);
    }
}

fn result_reg(vm: &VM, bytecode_type: BytecodeType) -> AnyReg {
    if mode(vm, bytecode_type).is_float() {
        FREG_RESULT.into()
    } else {
        REG_RESULT.into()
    }
}

enum SlowPathKind {
    TlabAllocationFailure(Label, Label, Reg, AllocationSize, Location, GcPoint),
    StackOverflow(Label, Label, Location, GcPoint),
    Safepoint(Label, Label, Location, GcPoint),
    Assert(Label, Location),
    InitializeGlobal(
        Label,
        Label,
        GlobalId,
        FunctionId,
        Address,
        Location,
        GcPoint,
    ),
    ObjectWriteBarrier {
        lbl_start: Label,
        lbl_return: Label,
        host: Reg,
        value: Reg,
    },
    MarkingWriteBarrier {
        lbl_start: Label,
        lbl_return: Label,
        host: Reg,
        value: Reg,
    },
}
