use std::collections::HashMap;

use crate::compiler::asm::BaselineAssembler;
use crate::compiler::codegen::{
    ensure_runtime_entry_trampoline, AllocationSize, AnyReg, CompilationData,
};
use crate::compiler::runtime_entry_trampoline::{NativeFct, NativeFctKind};
use crate::cpu::{
    has_lzcnt, has_popcnt, has_tzcnt, Reg, CALLEE_SAVED_REGS, FREG_PARAMS, FREG_RESULT, FREG_TMP1,
    REG_PARAMS, REG_RESULT, REG_SP, REG_TMP1, REG_TMP2, STACK_FRAME_ALIGNMENT,
};
use crate::gc::Address;
use crate::masm::{CondCode, Label, Mem};
use crate::mem::{self, align_i32};
use crate::mode::MachineMode;
use crate::object::{offset_of_array_data, Header, Str};
use crate::size::InstanceSize;
use crate::stdlib;
use crate::vm::{
    create_class_instance, create_enum_instance, create_struct_instance, display_fct, display_ty,
    ensure_class_instance_for_enum_variant, ensure_class_instance_for_lambda,
    ensure_class_instance_for_trait_object, find_trait_impl, get_concrete_tuple_bty,
    get_concrete_tuple_bty_array, specialize_bty, specialize_bty_array, CodeDescriptor, EnumLayout,
    GcPoint, LazyCompilationSite, Trap, INITIALIZED, VM,
};
use dora_bytecode::{
    read, BytecodeFunction, BytecodeOffset, BytecodeType, BytecodeTypeArray, BytecodeVisitor,
    ConstPoolEntry, ConstPoolIdx, FunctionId, FunctionKind, GlobalId, Intrinsic, Location,
    Register, TraitId,
};

use super::CompilationFlags;

macro_rules! comment {
    (
        $cannon:expr,
        $out:expr
    ) => {{
        if $cannon.emit_code_comments {
            $cannon.asm.emit_comment($out);
        }
    }};
}

struct ForwardJump {
    label: Label,
    offset: BytecodeOffset,
}

pub struct CannonCodeGen<'a> {
    vm: &'a VM,
    asm: BaselineAssembler<'a>,
    bytecode: &'a BytecodeFunction,

    location: Location,
    params: BytecodeTypeArray,
    has_variadic_parameter: bool,
    return_type: BytecodeType,
    emit_debug: bool,
    emit_code_comments: bool,

    type_params: BytecodeTypeArray,

    offset_to_address: HashMap<BytecodeOffset, usize>,
    offset_to_label: HashMap<BytecodeOffset, Label>,

    current_offset: BytecodeOffset,
    argument_stack: Vec<Register>,

    references: Vec<i32>,

    offsets: Vec<Option<i32>>,
    framesize: i32,
    register_start_offset: i32,

    flags: CompilationFlags,

    slow_paths: Vec<(
        Label,
        Register,
        FunctionId,
        Vec<Register>,
        BytecodeTypeArray,
        Location,
    )>,
}

impl<'a> CannonCodeGen<'a> {
    pub(super) fn new(
        vm: &'a VM,
        compilation_data: CompilationData<'a>,
        flags: CompilationFlags,
    ) -> CannonCodeGen<'a> {
        CannonCodeGen {
            vm,
            params: compilation_data.params,
            has_variadic_parameter: compilation_data.has_variadic_parameter,
            return_type: compilation_data.return_type,
            location: compilation_data.loc,
            emit_debug: compilation_data.emit_debug,
            asm: BaselineAssembler::new(vm),
            bytecode: compilation_data.bytecode_fct,
            emit_code_comments: compilation_data.emit_code_comments,
            type_params: compilation_data.type_params,
            offset_to_address: HashMap::new(),
            offset_to_label: HashMap::new(),
            current_offset: BytecodeOffset(0),
            argument_stack: Vec::new(),
            references: Vec::new(),
            offsets: Vec::new(),
            framesize: 0,
            register_start_offset: 0,
            flags,
            slow_paths: Vec::new(),
        }
    }

    pub fn generate(mut self) -> CodeDescriptor {
        if self.emit_debug {
            self.asm.debug();
        }

        self.compute_register_offsets();
        self.compute_reference_objects();

        self.emit_prolog();
        self.emit_stack_limit_check();
        self.emit_clear_registers();
        self.store_params_in_registers();
        self.emit_safepoint();

        read(self.bytecode.code(), &mut self);

        // Bytecode execution should never fall off the end and reach this instruction.
        self.asm.debug();

        self.emit_slow_paths();

        self.asm.code()
    }

    fn emit_safepoint(&mut self) {
        let gcpoint = self.create_gcpoint();
        self.asm.safepoint(self.location, gcpoint);
    }

    fn emit_slow_paths(&mut self) {
        let slow_paths = std::mem::replace(&mut self.slow_paths, Vec::new());

        for (lbl, dest, fct_id, arguments, type_params, pos) in slow_paths {
            self.asm.bind_label(lbl);
            self.emit_invoke_direct(dest, fct_id, type_params, arguments, pos);
        }
    }

    fn compute_register_offsets(&mut self) {
        self.register_start_offset = if self.has_result_address() {
            mem::ptr_width()
        } else {
            0
        };

        let (offsets, stacksize) = self.determine_offsets(self.register_start_offset);
        self.offsets = offsets;
        self.framesize = stacksize;
    }

    fn determine_offsets(&self, start: i32) -> (Vec<Option<i32>>, i32) {
        let len = self.bytecode.registers().len();
        let mut offset: Vec<Option<i32>> = vec![None; len];
        let mut stacksize: i32 = start;

        for (index, ty) in self.bytecode.registers().iter().enumerate() {
            let ty = register_bty(self.specialize_bty(ty.clone()));
            let size = size(self.vm, ty.clone());
            let align = align(self.vm, ty);
            stacksize = align_i32(stacksize + size, align);
            offset[index] = Some(-stacksize);
        }

        stacksize = align_i32(stacksize, STACK_FRAME_ALIGNMENT as i32);

        (offset, stacksize)
    }

    fn emit_clear_registers(&mut self) {
        let start = self.register_start_offset + mem::ptr_width();
        let end = self.framesize + mem::ptr_width();
        assert!(start <= end);

        if start == end {
            return;
        }

        comment!(self, "clear registers".into());

        // TODO: provide method in MacroAssembler for zeroing memory
        for word_offset in (start..end).step_by(mem::ptr_width_usize()) {
            self.asm
                .store_zero(MachineMode::Ptr, Mem::Local(-word_offset));
        }
    }

    fn compute_reference_objects(&mut self) {
        assert!(self.references.is_empty());
        for (idx, ty) in self.bytecode.registers().iter().enumerate() {
            let ty = register_bty(self.specialize_bty(ty.clone()));
            match ty {
                BytecodeType::Ptr | BytecodeType::Trait(_, _) => {
                    let offset = self.register_offset(Register(idx));
                    self.references.push(offset);
                }

                BytecodeType::Tuple(_) => {
                    let offset = self.register_offset(Register(idx));
                    let tuple = get_concrete_tuple_bty(self.vm, &ty);
                    for &ref_offset in tuple.references() {
                        self.references.push(offset + ref_offset);
                    }
                }

                BytecodeType::Struct(struct_id, type_params) => {
                    let offset = self.register_offset(Register(idx));
                    let struct_instance_id =
                        create_struct_instance(self.vm, struct_id, type_params);
                    let struct_instance = self.vm.struct_instances.idx(struct_instance_id);

                    for &ref_offset in &struct_instance.ref_fields {
                        self.references.push(offset + ref_offset);
                    }
                }

                BytecodeType::Enum(enum_id, type_params) => {
                    let enum_instance_id = create_enum_instance(self.vm, enum_id, type_params);
                    let enum_instance = self.vm.enum_instances.idx(enum_instance_id);

                    match enum_instance.layout {
                        EnumLayout::Int => {
                            // type does not contain reference
                        }
                        EnumLayout::Ptr | EnumLayout::Tagged => {
                            let offset = self.register_offset(Register(idx));
                            self.references.push(offset);
                        }
                    }
                }

                BytecodeType::TypeAlias(..)
                | BytecodeType::TypeParam(_)
                | BytecodeType::Class(_, _)
                | BytecodeType::Lambda(_, _)
                | BytecodeType::This => {
                    unreachable!()
                }

                BytecodeType::UInt8
                | BytecodeType::Int32
                | BytecodeType::Bool
                | BytecodeType::Char
                | BytecodeType::Int64
                | BytecodeType::Float32
                | BytecodeType::Float64
                | BytecodeType::Unit => {
                    // type does not contain reference
                }
            }
        }
    }

    fn create_gcpoint(&self) -> GcPoint {
        GcPoint::from_offsets(self.references.clone())
    }

    fn has_result_address(&self) -> bool {
        let return_type = self.specialize_bty(self.return_type.clone());
        result_passed_as_argument(return_type)
    }

    fn store_params_in_registers(&mut self) {
        let mut reg_idx = 0;
        let mut freg_idx = 0;
        let mut fp_offset = 2 * mem::ptr_width();

        if self.has_result_address() {
            self.asm.store_mem(
                MachineMode::Ptr,
                Mem::Local(result_address_offset()),
                REG_PARAMS[reg_idx].into(),
            );
            reg_idx += 1;
        }

        let params = self.params.clone();

        for (idx, param_ty) in params.iter().enumerate() {
            let param_ty = self.specialize_bty(param_ty.clone());
            assert!(param_ty.is_concrete_type());

            let dest = Register(idx);

            let param_ty = if idx == self.params.len() - 1 && self.has_variadic_parameter {
                assert_eq!(self.bytecode.register_type(dest), BytecodeType::Ptr);
                BytecodeType::Ptr
            } else if param_ty.is_unit() {
                continue;
            } else {
                assert_eq!(
                    self.specialize_register_type(dest),
                    register_bty(param_ty.clone())
                );
                param_ty
            };

            match param_ty.clone() {
                BytecodeType::Tuple(..) => {
                    self.store_param_on_stack_tuple(
                        &mut reg_idx,
                        &mut freg_idx,
                        &mut fp_offset,
                        dest,
                        param_ty,
                    );
                }

                BytecodeType::Struct(..) => {
                    self.store_param_on_stack_struct(
                        &mut reg_idx,
                        &mut freg_idx,
                        &mut fp_offset,
                        dest,
                        param_ty,
                    );
                }

                BytecodeType::Enum(..) => {
                    self.store_param_on_stack_enum(
                        &mut reg_idx,
                        &mut freg_idx,
                        &mut fp_offset,
                        dest,
                        param_ty,
                    );
                }

                BytecodeType::Float32 | BytecodeType::Float64 => {
                    self.store_param_on_stack_float(
                        &mut reg_idx,
                        &mut freg_idx,
                        &mut fp_offset,
                        dest,
                        param_ty,
                    );
                }

                BytecodeType::Ptr
                | BytecodeType::UInt8
                | BytecodeType::Bool
                | BytecodeType::Char
                | BytecodeType::Int32
                | BytecodeType::Int64
                | BytecodeType::Class(_, _)
                | BytecodeType::Trait(_, _)
                | BytecodeType::Lambda(_, _) => {
                    self.store_param_on_stack_core(
                        &mut reg_idx,
                        &mut freg_idx,
                        &mut fp_offset,
                        dest,
                        param_ty,
                    );
                }

                BytecodeType::TypeAlias(..)
                | BytecodeType::TypeParam(_)
                | BytecodeType::Unit
                | BytecodeType::This => {
                    unreachable!()
                }
            }
        }
    }

    fn store_param_on_stack_tuple(
        &mut self,
        reg_idx: &mut usize,
        _freg_idx: &mut usize,
        sp_offset: &mut i32,
        dest: Register,
        ty: BytecodeType,
    ) {
        let dest_offset = self.register_offset(dest);

        if *reg_idx < REG_PARAMS.len() {
            self.asm.copy(
                MachineMode::Ptr,
                REG_TMP1.into(),
                REG_PARAMS[*reg_idx].into(),
            );
            self.asm.copy_bytecode_ty(
                ty,
                RegOrOffset::Offset(dest_offset),
                RegOrOffset::Reg(REG_TMP1),
            );
            *reg_idx += 1;
        } else {
            self.asm
                .load_mem(MachineMode::Ptr, REG_TMP1.into(), Mem::Local(*sp_offset));
            self.asm.copy_bytecode_ty(
                ty,
                RegOrOffset::Offset(dest_offset),
                RegOrOffset::Reg(REG_TMP1),
            );
            *sp_offset += 8;
        }
    }

    fn store_param_on_stack_struct(
        &mut self,
        reg_idx: &mut usize,
        _freg_idx: &mut usize,
        sp_offset: &mut i32,
        dest: Register,
        ty: BytecodeType,
    ) {
        let dest_offset = self.reg(dest);

        if *reg_idx < REG_PARAMS.len() {
            self.asm.copy(
                MachineMode::Ptr,
                REG_TMP1.into(),
                REG_PARAMS[*reg_idx].into(),
            );
            self.asm
                .copy_bytecode_ty(ty, dest_offset, RegOrOffset::Reg(REG_TMP1));
            *reg_idx += 1;
        } else {
            self.asm
                .load_mem(MachineMode::Ptr, REG_TMP1.into(), Mem::Local(*sp_offset));
            self.asm
                .copy_bytecode_ty(ty, dest_offset, RegOrOffset::Reg(REG_TMP1));
            *sp_offset += 8;
        }
    }

    fn store_param_on_stack_enum(
        &mut self,
        reg_idx: &mut usize,
        _freg_idx: &mut usize,
        sp_offset: &mut i32,
        dest: Register,
        ty: BytecodeType,
    ) {
        let (enum_id, type_params) = match ty {
            BytecodeType::Enum(enum_id, type_params) => (enum_id, type_params),
            _ => unreachable!(),
        };
        let enum_instance_id = create_enum_instance(self.vm, enum_id, type_params);
        let enum_instance = self.vm.enum_instances.idx(enum_instance_id);

        let mode = match enum_instance.layout {
            EnumLayout::Int => MachineMode::Int32,
            EnumLayout::Tagged | EnumLayout::Ptr => MachineMode::Ptr,
        };

        if *reg_idx < REG_PARAMS.len() {
            let reg = REG_PARAMS[*reg_idx].into();
            *reg_idx += 1;
            self.emit_store_register(reg, dest);
        } else {
            self.asm
                .load_mem(mode, REG_RESULT.into(), Mem::Local(*sp_offset));
            self.emit_store_register(REG_RESULT.into(), dest);
            *sp_offset += 8;
        }
    }

    fn store_param_on_stack_float(
        &mut self,
        _reg_idx: &mut usize,
        freg_idx: &mut usize,
        sp_offset: &mut i32,
        dest: Register,
        ty: BytecodeType,
    ) {
        let mode = mode(self.vm, ty);

        if *freg_idx < FREG_PARAMS.len() {
            let freg = FREG_PARAMS[*freg_idx].into();
            *freg_idx += 1;
            self.emit_store_register(freg, dest);
        } else {
            self.asm
                .load_mem(mode, FREG_RESULT.into(), Mem::Local(*sp_offset));
            self.emit_store_register(FREG_RESULT.into(), dest);
            *sp_offset += 8;
        }
    }

    fn store_param_on_stack_core(
        &mut self,
        reg_idx: &mut usize,
        _freg_idx: &mut usize,
        sp_offset: &mut i32,
        dest: Register,
        ty: BytecodeType,
    ) {
        let mode = mode(self.vm, ty);

        if *reg_idx < REG_PARAMS.len() {
            let reg = REG_PARAMS[*reg_idx].into();
            *reg_idx += 1;
            self.emit_store_register(reg, dest);
        } else {
            self.asm
                .load_mem(mode, REG_RESULT.into(), Mem::Local(*sp_offset));
            self.emit_store_register(REG_RESULT.into(), dest);
            *sp_offset += 8;
        }
    }

    fn emit_prolog(&mut self) {
        self.asm.prolog(self.framesize);
    }

    fn emit_stack_limit_check(&mut self) {
        let gcpoint = self.create_gcpoint();
        self.asm.check_stack_limit(self.location, gcpoint);
    }

    fn emit_epilog(&mut self) {
        self.asm.epilog();
    }

    fn emit_load_register(&mut self, src: Register, dest: AnyReg) {
        let bytecode_type = self.specialize_register_type(src);
        let offset = self.register_offset(src);
        self.asm
            .load_mem(mode(self.vm, bytecode_type), dest, Mem::Local(offset));
    }

    fn emit_load_register_as(&mut self, src: Register, dest: AnyReg, mode: MachineMode) {
        let offset = self.register_offset(src);
        self.asm.load_mem(mode, dest, Mem::Local(offset));
    }

    fn emit_store_register(&mut self, src: AnyReg, dest: Register) {
        let bytecode_type = self.specialize_register_type(dest);
        let offset = self.register_offset(dest);
        self.asm
            .store_mem(mode(self.vm, bytecode_type), Mem::Local(offset), src);
    }

    fn emit_store_register_as(&mut self, src: AnyReg, dest: Register, mode: MachineMode) {
        let offset = self.register_offset(dest);
        self.asm.store_mem(mode, Mem::Local(offset), src);
    }

    fn emit_add(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.specialize_register_type(dest);

        if bytecode_type.is_any_float() {
            self.emit_load_register(lhs, FREG_RESULT.into());
            self.emit_load_register(rhs, FREG_TMP1.into());

            self.asm.float_add(
                mode(self.vm, bytecode_type),
                FREG_RESULT,
                FREG_RESULT,
                FREG_TMP1,
            );

            self.emit_store_register(FREG_RESULT.into(), dest);
        } else {
            assert!(bytecode_type == BytecodeType::Int32 || bytecode_type == BytecodeType::Int64);
            self.emit_load_register(lhs, REG_RESULT.into());
            self.emit_load_register(rhs, REG_TMP1.into());

            let position = self.bytecode.offset_location(self.current_offset.to_u32());

            self.asm.int_add_checked(
                mode(self.vm, bytecode_type),
                REG_RESULT,
                REG_RESULT,
                REG_TMP1,
                position,
            );

            self.emit_store_register(REG_RESULT.into(), dest);
        }
    }

    fn emit_sub(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.specialize_register_type(dest);

        if bytecode_type.is_any_float() {
            self.emit_load_register(lhs, FREG_RESULT.into());
            self.emit_load_register(rhs, FREG_TMP1.into());

            self.asm.float_sub(
                mode(self.vm, bytecode_type),
                FREG_RESULT,
                FREG_RESULT,
                FREG_TMP1,
            );

            self.emit_store_register(FREG_RESULT.into(), dest);
        } else {
            assert!(bytecode_type == BytecodeType::Int32 || bytecode_type == BytecodeType::Int64);
            self.emit_load_register(lhs, REG_RESULT.into());
            self.emit_load_register(rhs, REG_TMP1.into());

            let position = self.bytecode.offset_location(self.current_offset.to_u32());
            self.asm.int_sub_checked(
                mode(self.vm, bytecode_type),
                REG_RESULT,
                REG_RESULT,
                REG_TMP1,
                position,
            );

            self.emit_store_register(REG_RESULT.into(), dest);
        }
    }

    fn emit_neg(&mut self, dest: Register, src: Register) {
        assert_eq!(
            self.bytecode.register_type(src),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.specialize_register_type(dest);

        if bytecode_type.is_any_float() {
            self.emit_load_register(src, FREG_RESULT.into());

            self.asm
                .float_neg(mode(self.vm, bytecode_type), FREG_RESULT, FREG_RESULT);

            self.emit_store_register(FREG_RESULT.into(), dest);
        } else {
            assert!(bytecode_type == BytecodeType::Int32 || bytecode_type == BytecodeType::Int64);
            self.emit_load_register(src, REG_RESULT.into());

            let loc = self.bytecode.offset_location(self.current_offset.to_u32());

            self.asm
                .int_neg_checked(mode(self.vm, bytecode_type), REG_RESULT, REG_RESULT, loc);

            self.emit_store_register(REG_RESULT.into(), dest);
        }
    }

    fn emit_intrinsic_abs_float(&mut self, dest: Register, src: Register) {
        assert_eq!(
            self.bytecode.register_type(src),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(src, FREG_RESULT.into());

        let bytecode_type = self.bytecode.register_type(dest);
        self.asm
            .float_abs(mode(self.vm, bytecode_type), FREG_RESULT, FREG_RESULT);

        self.emit_store_register(FREG_RESULT.into(), dest);
    }

    fn emit_mul(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.specialize_register_type(dest);

        if bytecode_type.is_any_float() {
            self.emit_load_register(lhs, FREG_RESULT.into());
            self.emit_load_register(rhs, FREG_TMP1.into());

            self.asm.float_mul(
                mode(self.vm, bytecode_type),
                FREG_RESULT,
                FREG_RESULT,
                FREG_TMP1,
            );

            self.emit_store_register(FREG_RESULT.into(), dest);
        } else {
            assert!(bytecode_type == BytecodeType::Int32 || bytecode_type == BytecodeType::Int64);
            self.emit_load_register(lhs, REG_RESULT.into());
            self.emit_load_register(rhs, REG_TMP1.into());

            let position = self.bytecode.offset_location(self.current_offset.to_u32());

            self.asm.int_mul_checked(
                mode(self.vm, bytecode_type),
                REG_RESULT,
                REG_RESULT,
                REG_TMP1,
                position,
            );

            self.emit_store_register(REG_RESULT.into(), dest);
        }
    }

    fn emit_div(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.specialize_register_type(dest);

        if bytecode_type.is_any_float() {
            self.emit_load_register(lhs, FREG_RESULT.into());
            self.emit_load_register(rhs, FREG_TMP1.into());

            self.asm.float_div(
                mode(self.vm, bytecode_type),
                FREG_RESULT,
                FREG_RESULT,
                FREG_TMP1,
            );

            self.emit_store_register(FREG_RESULT.into(), dest);
        } else {
            assert!(bytecode_type == BytecodeType::Int32 || bytecode_type == BytecodeType::Int64);
            self.emit_load_register(lhs, REG_RESULT.into());
            self.emit_load_register(rhs, REG_TMP1.into());

            let position = self.bytecode.offset_location(self.current_offset.to_u32());

            self.asm.int_div_checked(
                mode(self.vm, bytecode_type),
                REG_RESULT,
                REG_RESULT,
                REG_TMP1,
                position,
            );

            self.emit_store_register(REG_RESULT.into(), dest);
        }
    }

    fn emit_mod(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.specialize_register_type(dest);
        assert!(bytecode_type == BytecodeType::Int32 || bytecode_type == BytecodeType::Int64);

        self.emit_load_register(lhs, REG_RESULT.into());
        self.emit_load_register(rhs, REG_TMP1.into());

        let position = self.bytecode.offset_location(self.current_offset.to_u32());

        self.asm.int_mod_checked(
            mode(self.vm, bytecode_type),
            REG_RESULT,
            REG_RESULT,
            REG_TMP1,
            position,
        );

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_and(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.specialize_register_type(dest);
        assert!(bytecode_type == BytecodeType::Int32 || bytecode_type == BytecodeType::Int64);

        self.emit_load_register(lhs, REG_RESULT.into());
        self.emit_load_register(rhs, REG_TMP1.into());

        self.asm.int_and(
            mode(self.vm, bytecode_type),
            REG_RESULT,
            REG_RESULT,
            REG_TMP1,
        );

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_or(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.specialize_register_type(dest);
        assert!(bytecode_type == BytecodeType::Int32 || bytecode_type == BytecodeType::Int64);

        self.emit_load_register(lhs, REG_RESULT.into());
        self.emit_load_register(rhs, REG_TMP1.into());

        self.asm.int_or(
            mode(self.vm, bytecode_type),
            REG_RESULT,
            REG_RESULT,
            REG_TMP1,
        );

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_xor(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.specialize_register_type(dest);
        assert!(bytecode_type == BytecodeType::Int32 || bytecode_type == BytecodeType::Int64);

        self.emit_load_register(lhs, REG_RESULT.into());
        self.emit_load_register(rhs, REG_TMP1.into());

        self.asm.int_xor(
            mode(self.vm, bytecode_type),
            REG_RESULT,
            REG_RESULT,
            REG_TMP1,
        );

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_not(&mut self, dest: Register, src: Register) {
        assert_eq!(
            self.bytecode.register_type(src),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.specialize_register_type(dest);
        assert!(
            bytecode_type == BytecodeType::Int32
                || bytecode_type == BytecodeType::Int64
                || bytecode_type == BytecodeType::Bool
        );

        if bytecode_type == BytecodeType::Bool {
            self.emit_load_register(src, REG_RESULT.into());
            self.asm.bool_not(REG_RESULT, REG_RESULT);
            self.emit_store_register(REG_RESULT.into(), dest);
        } else {
            self.emit_load_register(src, REG_RESULT.into());

            self.asm
                .int_not(mode(self.vm, bytecode_type), REG_RESULT, REG_RESULT);

            self.emit_store_register(REG_RESULT.into(), dest);
        }
    }

    fn emit_shl(&mut self, dest: Register, lhs: Register, rhs: Register) {
        let bytecode_type = self.specialize_register_type(dest);
        assert!(bytecode_type == BytecodeType::Int32 || bytecode_type == BytecodeType::Int64);

        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        assert_eq!(BytecodeType::Int32, self.bytecode.register_type(rhs));

        self.emit_load_register(lhs, REG_RESULT.into());
        self.emit_load_register(rhs, REG_TMP1.into());

        self.asm.int_shl(
            mode(self.vm, bytecode_type),
            REG_RESULT,
            REG_RESULT,
            REG_TMP1,
        );

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_shr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        let bytecode_type = self.specialize_register_type(dest);
        assert!(bytecode_type == BytecodeType::Int32 || bytecode_type == BytecodeType::Int64);

        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        assert_eq!(BytecodeType::Int32, self.bytecode.register_type(rhs));

        self.emit_load_register(lhs, REG_RESULT.into());
        self.emit_load_register(rhs, REG_TMP1.into());

        self.asm.int_shr(
            mode(self.vm, bytecode_type),
            REG_RESULT,
            REG_RESULT,
            REG_TMP1,
        );

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_sar(&mut self, dest: Register, lhs: Register, rhs: Register) {
        let bytecode_type = self.specialize_register_type(dest);
        assert!(bytecode_type == BytecodeType::Int32 || bytecode_type == BytecodeType::Int64);

        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        assert_eq!(BytecodeType::Int32, self.bytecode.register_type(rhs));

        self.emit_load_register(lhs, REG_RESULT.into());
        self.emit_load_register(rhs, REG_TMP1.into());

        self.asm.int_sar(
            mode(self.vm, bytecode_type),
            REG_RESULT,
            REG_RESULT,
            REG_TMP1,
        );

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_rol_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(BytecodeType::Int32, self.bytecode.register_type(rhs));
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(lhs, REG_RESULT.into());

        self.emit_load_register(rhs, REG_TMP1.into());

        let bytecode_type = self.bytecode.register_type(dest);
        self.asm.int_rol(
            mode(self.vm, bytecode_type),
            REG_RESULT,
            REG_RESULT,
            REG_TMP1,
        );

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_ror_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(BytecodeType::Int32, self.bytecode.register_type(rhs));
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(lhs, REG_RESULT.into());

        self.emit_load_register(rhs, REG_TMP1.into());

        let bytecode_type = self.bytecode.register_type(dest);
        self.asm.int_ror(
            mode(self.vm, bytecode_type),
            REG_RESULT,
            REG_RESULT,
            REG_TMP1,
        );

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_reinterpret(&mut self, dest: Register, src: Register) {
        assert_ne!(
            self.bytecode.register_type(dest),
            self.bytecode.register_type(src)
        );

        let src_type = self.bytecode.register_type(src);
        let src_register = result_reg(self.vm, src_type.clone());
        self.emit_load_register(src, src_register.into());

        let dest_type = self.bytecode.register_type(dest);
        let dest_register = result_reg(self.vm, dest_type.clone());

        match dest_type {
            BytecodeType::Int32 => {
                assert_eq!(src_type, BytecodeType::Float32);
                self.asm.float_as_int(
                    MachineMode::Int32,
                    dest_register.reg(),
                    MachineMode::Float32,
                    src_register.freg(),
                );
            }
            BytecodeType::Float32 => {
                assert_eq!(src_type, BytecodeType::Int32);
                self.asm.int_as_float(
                    MachineMode::Float32,
                    dest_register.freg(),
                    MachineMode::Int32,
                    src_register.reg(),
                );
            }
            BytecodeType::Int64 => {
                assert_eq!(src_type, BytecodeType::Float64);
                self.asm.float_as_int(
                    MachineMode::Int64,
                    dest_register.reg(),
                    MachineMode::Float64,
                    src_register.freg(),
                );
            }
            BytecodeType::Float64 => {
                assert_eq!(src_type, BytecodeType::Int64);
                self.asm.int_as_float(
                    MachineMode::Float64,
                    dest_register.freg(),
                    MachineMode::Int64,
                    src_register.reg(),
                );
            }
            _ => unreachable!(),
        }

        self.emit_store_register(dest_register.into(), dest);
    }

    fn emit_extend_uint8(&mut self, dest: Register, src: Register, _mode: MachineMode) {
        assert_eq!(self.bytecode.register_type(src), BytecodeType::UInt8);

        self.emit_load_register(src, REG_RESULT.into());
        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_shrink(
        &mut self,
        dest: Register,
        _dest_mode: MachineMode,
        src: Register,
        _src_mode: MachineMode,
    ) {
        self.emit_load_register(src, REG_RESULT.into());
        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_int_to_int64(&mut self, dest: Register, src: Register) {
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Int64);
        assert_eq!(self.bytecode.register_type(src), BytecodeType::Int32);

        self.emit_load_register(src, REG_RESULT.into());
        self.asm.extend_int_long(REG_RESULT, REG_RESULT);

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_int64_to_int(&mut self, dest: Register, src: Register) {
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Int32);
        assert_eq!(self.bytecode.register_type(src), BytecodeType::Int64);

        self.emit_load_register(src, REG_RESULT.into());

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_promote_float(&mut self, dest: Register, src: Register) {
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Float64);
        assert_eq!(self.bytecode.register_type(src), BytecodeType::Float32);

        self.emit_load_register(src, FREG_RESULT.into());
        self.asm.float32_to_float64(FREG_RESULT, FREG_RESULT);
        self.emit_store_register(FREG_RESULT.into(), dest);
    }

    fn emit_demote_float64(&mut self, dest: Register, src: Register) {
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Float32);
        assert_eq!(self.bytecode.register_type(src), BytecodeType::Float64);

        self.emit_load_register(src, FREG_RESULT.into());
        self.asm.float64_to_float32(FREG_RESULT, FREG_RESULT);
        self.emit_store_register(FREG_RESULT.into(), dest);
    }

    fn emit_mov_generic(&mut self, dest: Register, src: Register) {
        assert_eq!(
            self.bytecode.register_type(src),
            self.bytecode.register_type(dest)
        );

        let bytecode_type = self.specialize_register_type(src);
        let src = self.reg(src);
        let dest = self.reg(dest);
        self.asm.copy_bytecode_ty(bytecode_type, dest, src);
    }

    fn emit_load_tuple_element(&mut self, dest: Register, src: Register, idx: ConstPoolIdx) {
        let (tuple_ty, subtype_idx) = match self.bytecode.const_pool(idx) {
            ConstPoolEntry::TupleElement(tuple_ty, subtype_idx) => (tuple_ty.clone(), *subtype_idx),
            _ => unreachable!(),
        };

        let tuple_ty = self.specialize_bty(tuple_ty);
        let tuple = get_concrete_tuple_bty(self.vm, &tuple_ty);
        let offset = tuple.offsets()[subtype_idx as usize];

        let dest_type = self.specialize_register_type(dest);
        let src_offset = self.register_offset(src);

        self.asm.copy_bytecode_ty(
            dest_type,
            self.reg(dest),
            RegOrOffset::Offset(src_offset + offset),
        );
    }

    fn emit_load_enum_element(&mut self, dest: Register, src: Register, idx: ConstPoolIdx) {
        let (enum_id, type_params, variant_idx, element_idx) = match self.bytecode.const_pool(idx) {
            ConstPoolEntry::EnumElement(enum_id, type_params, variant_idx, element_idx) => {
                (*enum_id, type_params.clone(), *variant_idx, *element_idx)
            }
            _ => unreachable!(),
        };

        let type_params = self.specialize_bty_array(&type_params);
        debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

        let enum_ = &self.vm.program.enums[enum_id.0 as usize];

        let enum_instance_id = create_enum_instance(self.vm, enum_id, type_params);
        let enum_instance = self.vm.enum_instances.idx(enum_instance_id);

        match enum_instance.layout {
            EnumLayout::Int => {
                unreachable!();
            }
            EnumLayout::Ptr => {
                assert_eq!(0, element_idx);
                let first_variant = enum_.variants.first().unwrap();
                let some_idx = if first_variant.arguments.is_empty() {
                    1
                } else {
                    0
                };
                assert_eq!(variant_idx, some_idx);
                assert_eq!(BytecodeType::Ptr, self.specialize_register_type(dest));

                self.emit_load_register_as(src, REG_RESULT.into(), MachineMode::Ptr);
                let pos = self.bytecode.offset_location(self.current_offset.to_u32());
                self.asm.test_if_nil_bailout(pos, REG_RESULT, Trap::ILLEGAL);
                self.emit_store_register_as(REG_RESULT.into(), dest, MachineMode::Ptr);
            }

            EnumLayout::Tagged => {
                let cls_def_id = ensure_class_instance_for_enum_variant(
                    self.vm,
                    &*enum_instance,
                    &*enum_,
                    variant_idx,
                );

                let cls = self.vm.class_instances.idx(cls_def_id);

                self.emit_load_register_as(src, REG_TMP1.into(), MachineMode::Ptr);
                self.asm.load_mem(
                    MachineMode::Int32,
                    REG_RESULT.into(),
                    Mem::Base(REG_TMP1, Header::size()),
                );
                let lbl_bailout = self.asm.create_label();
                self.asm
                    .cmp_reg_imm(MachineMode::Int32, REG_RESULT, variant_idx as i32);
                self.asm.jump_if(CondCode::NotEqual, lbl_bailout);
                let pos = self.bytecode.offset_location(self.current_offset.to_u32());
                self.asm.emit_bailout(lbl_bailout, Trap::ILLEGAL, pos);

                let field_id = enum_instance.field_id(&*enum_, variant_idx, element_idx);
                let field = &cls.fields[field_id as usize];

                let bty = register_ty(field.ty.clone());
                assert_eq!(bty, self.specialize_register_type(dest));

                let bytecode_type = self.specialize_register_type(dest);
                assert_eq!(bytecode_type, register_ty(field.ty.clone()));
                let dest = self.register_offset(dest);
                let dest = RegOrOffset::Offset(dest);
                let src = RegOrOffset::RegWithOffset(REG_TMP1, field.offset);
                self.asm.copy_bytecode_ty(bytecode_type, dest, src);
            }
        }
    }

    fn emit_load_enum_variant(&mut self, dest: Register, src: Register, idx: ConstPoolIdx) {
        let (enum_id, type_params) = match self.bytecode.const_pool(idx) {
            ConstPoolEntry::Enum(enum_id, type_params) => (*enum_id, type_params.clone()),
            _ => unreachable!(),
        };

        let type_params = self.specialize_bty_array(&type_params);
        debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

        let enum_ = &self.vm.program.enums[enum_id.0 as usize];

        let enum_instance_id = create_enum_instance(self.vm, enum_id, type_params);
        let enum_instance = self.vm.enum_instances.idx(enum_instance_id);

        match enum_instance.layout {
            EnumLayout::Int => {
                self.emit_load_register_as(src, REG_RESULT.into(), MachineMode::Int32);
                self.emit_store_register_as(REG_RESULT.into(), dest, MachineMode::Int32);
            }
            EnumLayout::Ptr => {
                let first_variant = enum_.variants.first().unwrap();
                let none_idx = if first_variant.arguments.is_empty() {
                    0
                } else {
                    1
                };
                let some_idx = if none_idx == 0 { 1 } else { 0 };

                self.emit_load_register_as(src, REG_TMP1.into(), MachineMode::Ptr);
                self.asm
                    .load_int_const(MachineMode::Int32, REG_RESULT, none_idx as i64);
                self.asm.cmp_reg_imm(MachineMode::Ptr, REG_TMP1, 0);

                let lbl_end = self.asm.create_label();
                self.asm.jump_if(CondCode::Equal, lbl_end);
                self.asm
                    .load_int_const(MachineMode::Int32, REG_RESULT, some_idx);
                self.asm.bind_label(lbl_end);
                self.emit_store_register(REG_RESULT.into(), dest);
            }

            EnumLayout::Tagged => {
                self.emit_load_register_as(src, REG_RESULT.into(), MachineMode::Ptr);
                self.asm.load_mem(
                    MachineMode::Int32,
                    REG_RESULT.into(),
                    Mem::Base(REG_RESULT, Header::size()),
                );
                self.emit_store_register_as(REG_RESULT.into(), dest.into(), MachineMode::Int32);
            }
        }
    }

    fn emit_load_struct_field(&mut self, dest: Register, obj: Register, field_idx: ConstPoolIdx) {
        let (struct_id, type_params, field_id) = match self.bytecode.const_pool(field_idx) {
            ConstPoolEntry::StructField(struct_id, type_params, field_id) => {
                (*struct_id, type_params.clone(), *field_id)
            }
            _ => unreachable!(),
        };

        debug_assert_eq!(
            self.bytecode.register_type(obj),
            BytecodeType::Struct(struct_id, type_params.clone())
        );

        let type_params = self.specialize_bty_array(&type_params);
        debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

        let struct_instance_id = create_struct_instance(self.vm, struct_id, type_params);
        let struct_instance = self.vm.struct_instances.idx(struct_instance_id);

        let field = &struct_instance.fields[field_id as usize];

        let bytecode_type = self.specialize_register_type(dest);
        assert_eq!(bytecode_type, register_ty(field.ty.clone()));
        let dest = self.reg(dest);
        let src = self.reg(obj).offset(field.offset);
        self.asm.copy_bytecode_ty(bytecode_type, dest, src);
    }

    fn emit_load_field(&mut self, dest: Register, obj: Register, field_idx: ConstPoolIdx) {
        assert!(self.bytecode.register_type(obj).is_ptr());

        let (class_instance_id, field_id) = match self.bytecode.const_pool(field_idx) {
            ConstPoolEntry::Field(cls_id, type_params, field_id) => {
                let type_params = self.specialize_bty_array(&type_params);
                debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

                let class_instance_id = create_class_instance(self.vm, *cls_id, &type_params);

                (class_instance_id, *field_id)
            }
            _ => unreachable!(),
        };

        let cls = self.vm.class_instances.idx(class_instance_id);

        let field = &cls.fields[field_id as usize];

        let obj_reg = REG_TMP1;
        self.emit_load_register(obj, obj_reg.into());

        let pos = self.bytecode.offset_location(self.current_offset.to_u32());
        self.asm.test_if_nil_bailout(pos, obj_reg, Trap::NIL);

        let bytecode_type = self.specialize_register_type(dest);
        assert_eq!(bytecode_type, register_ty(field.ty.clone()));
        let dest = self.reg(dest);
        let src = RegOrOffset::RegWithOffset(obj_reg, field.offset);
        self.asm.copy_bytecode_ty(bytecode_type, dest, src);
    }

    fn emit_store_field(&mut self, src: Register, obj: Register, field_idx: ConstPoolIdx) {
        assert_eq!(self.bytecode.register_type(obj), BytecodeType::Ptr);

        let (cls_id, type_params, field_id) = match self.bytecode.const_pool(field_idx) {
            ConstPoolEntry::Field(cls_id, type_params, field_id) => {
                (*cls_id, type_params.clone(), *field_id)
            }
            _ => unreachable!(),
        };

        let type_params = self.specialize_bty_array(&type_params);
        debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

        let class_instance_id = create_class_instance(self.vm, cls_id, &type_params);
        let cls = self.vm.class_instances.idx(class_instance_id);

        let field = &cls.fields[field_id as usize];

        assert!(self.bytecode.register_type(obj).is_ptr());
        let obj_reg = REG_TMP1;
        self.emit_load_register(obj, obj_reg.into());

        let pos = self.bytecode.offset_location(self.current_offset.to_u32());
        self.asm.test_if_nil_bailout(pos, obj_reg, Trap::NIL);

        let bytecode_type = self.specialize_register_type(src);
        assert_eq!(bytecode_type, register_ty(field.ty.clone()));

        self.emit_store_field_raw(obj, obj_reg, field.offset, src);
    }

    fn emit_store_field_raw(&mut self, obj: Register, obj_reg: Reg, offset: i32, value: Register) {
        let ty = self.specialize_register_type(value);
        let needs_write_barrier;

        match &ty {
            BytecodeType::Unit => {
                // nothing to do
                needs_write_barrier = false;
            }

            BytecodeType::Tuple(subtypes) => {
                let src_offset = self.register_offset(value);
                self.asm.copy_tuple(
                    subtypes.clone(),
                    RegOrOffset::RegWithOffset(obj_reg, offset),
                    RegOrOffset::Offset(src_offset),
                );

                needs_write_barrier = get_concrete_tuple_bty(self.vm, &ty).contains_references()
            }

            BytecodeType::Struct(struct_id, type_params) => {
                let src_offset = self.register_offset(value);
                self.asm.copy_struct(
                    *struct_id,
                    type_params.clone(),
                    RegOrOffset::RegWithOffset(obj_reg, offset),
                    RegOrOffset::Offset(src_offset),
                );

                let struct_instance_id =
                    create_struct_instance(self.vm, *struct_id, type_params.clone());
                let struct_instance = self.vm.struct_instances.idx(struct_instance_id);
                needs_write_barrier = struct_instance.contains_references();
            }

            BytecodeType::Enum(enum_id, type_params) => {
                let enum_instance_id = create_enum_instance(self.vm, *enum_id, type_params.clone());
                let enum_instance = self.vm.enum_instances.idx(enum_instance_id);

                let mode = match enum_instance.layout {
                    EnumLayout::Int => MachineMode::Int32,
                    EnumLayout::Ptr | EnumLayout::Tagged => MachineMode::Ptr,
                };

                self.emit_load_register_as(value, REG_RESULT.into(), mode);
                self.asm
                    .store_mem(mode, Mem::Base(obj_reg, offset), REG_RESULT.into());

                needs_write_barrier = mode == MachineMode::Ptr;
            }

            BytecodeType::TypeAlias(..)
            | BytecodeType::TypeParam(_)
            | BytecodeType::Class(_, _)
            | BytecodeType::Lambda(_, _)
            | BytecodeType::This => {
                unreachable!()
            }
            BytecodeType::UInt8
            | BytecodeType::Bool
            | BytecodeType::Char
            | BytecodeType::Int32
            | BytecodeType::Int64
            | BytecodeType::Float32
            | BytecodeType::Float64 => {
                let value_reg = result_reg(self.vm, ty.clone());
                let mode = mode(self.vm, ty.clone());

                self.emit_load_register(value, value_reg.into());
                self.asm
                    .store_mem(mode, Mem::Base(obj_reg, offset), value_reg);

                needs_write_barrier = false;
            }

            BytecodeType::Ptr | BytecodeType::Trait(_, _) => {
                let value_reg = REG_RESULT;
                let mode = MachineMode::Ptr;

                self.emit_load_register(value, value_reg.into());
                self.asm
                    .store_mem(mode, Mem::Base(obj_reg, offset), value_reg.into());

                needs_write_barrier = true;
            }
        }

        if self.vm.gc.needs_write_barrier() && needs_write_barrier {
            self.emit_load_register(obj, REG_RESULT.into());
            self.asm.emit_write_barrier(REG_RESULT);
        }
    }

    fn emit_load_global(&mut self, dest: Register, global_id: GlobalId) {
        let global_var = &self.vm.program.globals[global_id.0 as usize];

        assert_eq!(
            self.bytecode.register_type(dest),
            register_bty(global_var.ty.clone())
        );

        if let Some(initializer) = global_var.initial_value {
            let ptr = get_function_address(self.vm, initializer, BytecodeTypeArray::empty());
            let position = self.bytecode.offset_location(self.current_offset.to_u32());
            let gcpoint = self.create_gcpoint();
            self.asm
                .ensure_global(global_id, initializer, ptr, position, gcpoint);
        }

        let address_value = self
            .vm
            .global_variable_memory
            .as_ref()
            .unwrap()
            .address_value(global_id);

        let disp = self.asm.add_addr(address_value);
        let pos = self.asm.pos() as i32;
        self.asm.load_constpool(REG_TMP1, disp + pos);

        let bytecode_type = self.bytecode.register_type(dest);

        let dest = self.reg(dest);
        let src = RegOrOffset::Reg(REG_TMP1);
        self.asm.copy_bytecode_ty(bytecode_type, dest, src);
    }

    fn emit_store_global(&mut self, src: Register, global_id: GlobalId) {
        let global_var = &self.vm.program.globals[global_id.0 as usize];

        assert_eq!(
            self.bytecode.register_type(src),
            register_bty(global_var.ty.clone())
        );

        let address_value = self
            .vm
            .global_variable_memory
            .as_ref()
            .unwrap()
            .address_value(global_id);

        let disp = self.asm.add_addr(address_value);
        let pos = self.asm.pos() as i32;

        self.asm.load_constpool(REG_TMP1, disp + pos);

        let bytecode_type = self.bytecode.register_type(src);

        let dest = RegOrOffset::Reg(REG_TMP1);
        let src = self.reg(src);
        self.asm.copy_bytecode_ty(bytecode_type, dest, src);

        if global_var.initial_value.is_some() {
            let address_init = self
                .vm
                .global_variable_memory
                .as_ref()
                .unwrap()
                .address_init(global_id);

            let disp = self.asm.add_addr(address_init);
            let pos = self.asm.pos() as i32;
            self.asm.load_constpool(REG_RESULT, disp + pos);
            self.asm
                .load_int_const(MachineMode::Int8, REG_TMP1, INITIALIZED as i64);
            self.asm
                .store_int8_synchronized(REG_TMP1.into(), REG_RESULT.into())
        }
    }

    fn emit_const_bool(&mut self, dest: Register, bool_const: bool) {
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Bool);

        if bool_const {
            self.asm.load_true(REG_RESULT);
        } else {
            self.asm.load_false(REG_RESULT);
        }
        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_const_int(&mut self, dest: Register, int_const: i64) {
        let bytecode_type = self.specialize_register_type(dest);

        assert!(
            bytecode_type == BytecodeType::Char
                || bytecode_type == BytecodeType::UInt8
                || bytecode_type == BytecodeType::Int32
                || bytecode_type == BytecodeType::Int64
        );

        self.asm
            .load_int_const(mode(self.vm, bytecode_type), REG_RESULT, int_const);

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_const_float(&mut self, dest: Register, float_const: f64) {
        let bytecode_type = self.specialize_register_type(dest);
        assert!(bytecode_type == BytecodeType::Float32 || bytecode_type == BytecodeType::Float64);

        self.asm
            .load_float_const(mode(self.vm, bytecode_type), FREG_RESULT, float_const);

        self.emit_store_register(FREG_RESULT.into(), dest);
    }

    fn emit_const_string(&mut self, dest: Register, lit_value: &str) {
        let bytecode_type = self.specialize_register_type(dest);
        assert_eq!(bytecode_type, BytecodeType::Ptr);

        let handle = Str::from_buffer_in_perm(self.vm, lit_value.as_bytes());
        let disp = self.asm.add_addr(handle.address());
        let pos = self.asm.pos() as i32;

        self.asm.load_constpool(REG_RESULT, disp + pos);

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_test_generic(&mut self, dest: Register, lhs: Register, rhs: Register, op: CondCode) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );

        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Bool);
        let bytecode_type = self.specialize_register_type(lhs);

        assert!(
            bytecode_type == BytecodeType::Float32
                || bytecode_type == BytecodeType::Float64
                || bytecode_type == BytecodeType::Int32
                || bytecode_type == BytecodeType::Int64
                || bytecode_type == BytecodeType::Char
                || bytecode_type == BytecodeType::UInt8
                || bytecode_type == BytecodeType::Bool
                || bytecode_type.is_enum()
        );

        if bytecode_type.is_any_float() {
            self.emit_load_register(lhs, FREG_RESULT.into());
            self.emit_load_register(rhs, FREG_TMP1.into());

            self.asm.float_cmp(
                mode(self.vm, bytecode_type),
                REG_RESULT,
                FREG_RESULT,
                FREG_TMP1,
                op,
            );

            self.emit_store_register(REG_RESULT.into(), dest);
        } else {
            self.emit_load_register(lhs, REG_RESULT.into());
            self.emit_load_register(rhs, REG_TMP1.into());

            self.asm
                .cmp_reg(mode(self.vm, bytecode_type), REG_RESULT, REG_TMP1);
            self.asm.set(REG_RESULT, op);

            self.emit_store_register(REG_RESULT.into(), dest);
        }
    }

    fn emit_test_identity(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Bool);

        let op = CondCode::Equal;

        let bytecode_type = self.specialize_register_type(lhs);

        match bytecode_type {
            BytecodeType::Tuple(_) => unimplemented!(),

            BytecodeType::TypeAlias(..)
            | BytecodeType::Class(_, _)
            | BytecodeType::TypeParam(_)
            | BytecodeType::Struct(_, _)
            | BytecodeType::Lambda(_, _)
            | BytecodeType::This => {
                unreachable!()
            }
            BytecodeType::Float32 | BytecodeType::Float64 => {
                let mode = match bytecode_type {
                    BytecodeType::Float32 => MachineMode::Int32,
                    BytecodeType::Float64 => MachineMode::Int64,
                    _ => unreachable!(),
                };

                self.emit_load_register_as(lhs, REG_RESULT.into(), mode);
                self.emit_load_register_as(rhs, REG_TMP1.into(), mode);

                self.asm.cmp_reg(mode, REG_RESULT, REG_TMP1);
                self.asm.set(REG_RESULT, op);

                self.emit_store_register(REG_RESULT.into(), dest);
            }

            BytecodeType::Unit => {
                self.emit_const_bool(dest, true);
            }

            BytecodeType::Bool
            | BytecodeType::UInt8
            | BytecodeType::Char
            | BytecodeType::Int32
            | BytecodeType::Int64
            | BytecodeType::Ptr
            | BytecodeType::Trait(_, _)
            | BytecodeType::Enum(_, _) => {
                self.emit_load_register(lhs, REG_RESULT.into());
                self.emit_load_register(rhs, REG_TMP1.into());

                self.asm
                    .cmp_reg(mode(self.vm, bytecode_type), REG_RESULT, REG_TMP1);
                self.asm.set(REG_RESULT, op);

                self.emit_store_register(REG_RESULT.into(), dest);
            }
        }
    }

    fn emit_jump_if(&mut self, src: Register, target: BytecodeOffset, op: bool) {
        assert_eq!(self.bytecode.register_type(src), BytecodeType::Bool);

        self.emit_load_register(src, REG_RESULT.into());

        let op = if op {
            CondCode::NonZero
        } else {
            CondCode::Zero
        };

        let lbl = self.ensure_forward_label(target);
        self.asm.test_and_jump_if(op, REG_RESULT, lbl);
    }

    fn emit_jump(&mut self, target: BytecodeOffset) {
        let lbl = self.ensure_forward_label(target);
        self.asm.jump(lbl);
    }

    fn emit_jump_loop(&mut self, target: BytecodeOffset) {
        assert!(target < self.current_offset);

        let opcode = self.bytecode.read_opcode(target);
        assert!(opcode.is_loop_start());

        self.emit_safepoint();
        let loop_start = *self.offset_to_label.get(&target).expect("missing label");
        self.asm.jump(loop_start);
    }

    fn ensure_forward_label(&mut self, target: BytecodeOffset) -> Label {
        assert!(target > self.current_offset);

        if let Some(&label) = self.offset_to_label.get(&target) {
            label
        } else {
            let label = self.asm.create_label();
            let old = self.offset_to_label.insert(target, label);
            assert!(old.is_none());

            label
        }
    }

    fn emit_load_trait_object_value(&mut self, dest: Register, object: Register) {
        assert!(self.bytecode.register_type(object).is_trait());

        let value_ty = self.type_params[self.type_params.len() - 1].clone();

        let bytecode_type = self.specialize_register_type(dest);
        assert_eq!(bytecode_type, register_bty(value_ty.clone()));

        let obj_reg = REG_TMP1;
        self.emit_load_register(object, obj_reg.into());

        let dest = self.reg(dest);
        let src = RegOrOffset::RegWithOffset(obj_reg, Header::size());
        self.asm.copy_bytecode_ty(bytecode_type, dest, src);
    }

    fn emit_return_generic(&mut self, src: Register) {
        let bytecode_type = self.specialize_register_type(src);
        match bytecode_type {
            BytecodeType::Unit => {
                // nothing to do
            }

            BytecodeType::Tuple(subtypes) => {
                let src_offset = self.register_offset(src);

                self.asm.load_mem(
                    MachineMode::Ptr,
                    REG_TMP1.into(),
                    Mem::Local(result_address_offset()),
                );

                self.asm.copy_tuple(
                    subtypes.clone(),
                    RegOrOffset::Reg(REG_TMP1),
                    RegOrOffset::Offset(src_offset),
                );
            }

            BytecodeType::Struct(struct_id, type_params) => {
                let src_offset = self.register_offset(src);

                self.asm.load_mem(
                    MachineMode::Ptr,
                    REG_TMP1.into(),
                    Mem::Local(result_address_offset()),
                );

                self.asm.copy_struct(
                    struct_id,
                    type_params.clone(),
                    RegOrOffset::Reg(REG_TMP1),
                    RegOrOffset::Offset(src_offset),
                );
            }

            BytecodeType::Enum(enum_id, type_params) => {
                let enum_instance_id = create_enum_instance(self.vm, enum_id, type_params.clone());
                let enum_instance = self.vm.enum_instances.idx(enum_instance_id);

                let mode = match enum_instance.layout {
                    EnumLayout::Int => MachineMode::Int32,
                    EnumLayout::Ptr | EnumLayout::Tagged => MachineMode::Ptr,
                };

                self.emit_load_register_as(src, REG_RESULT.into(), mode);
            }

            BytecodeType::TypeAlias(..)
            | BytecodeType::TypeParam(_)
            | BytecodeType::Class(_, _)
            | BytecodeType::Lambda(_, _)
            | BytecodeType::This => {
                unreachable!()
            }

            BytecodeType::UInt8
            | BytecodeType::Int32
            | BytecodeType::Bool
            | BytecodeType::Char
            | BytecodeType::Int64
            | BytecodeType::Float32
            | BytecodeType::Float64 => {
                let reg = result_reg(self.vm, bytecode_type);
                self.emit_load_register(src, reg.into());
            }

            BytecodeType::Ptr | BytecodeType::Trait(_, _) => {
                let reg = REG_RESULT;
                self.emit_load_register(src, reg.into());
                self.asm
                    .test_if_nil_bailout(Location::new(1, 1), REG_RESULT, Trap::ILLEGAL);
            }
        }

        self.emit_epilog();
    }

    fn emit_new_object(&mut self, dest: Register, idx: ConstPoolIdx) {
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Ptr);

        let const_pool_entry = self.bytecode.const_pool(idx);

        let (cls_id, type_params) = match const_pool_entry {
            ConstPoolEntry::Class(cls_id, type_params) => (*cls_id, type_params.clone()),
            _ => unreachable!(),
        };

        let type_params = self.specialize_bty_array(&type_params);
        debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

        let class_instance_id = create_class_instance(self.vm, cls_id, &type_params);
        let class_instance = self.vm.class_instances.idx(class_instance_id);

        let alloc_size = match class_instance.size {
            InstanceSize::Fixed(size) => AllocationSize::Fixed(size as usize),
            _ => unreachable!(
                "class size type {:?} for new object not supported",
                class_instance.size
            ),
        };

        let gcpoint = self.create_gcpoint();
        let position = self.bytecode.offset_location(self.current_offset.to_u32());
        self.asm
            .allocate(REG_RESULT.into(), alloc_size, position, gcpoint);

        // store gc object in temporary storage
        self.emit_store_register(REG_RESULT.into(), dest);

        self.asm.initialize_object(REG_RESULT, &*class_instance);
    }

    fn emit_new_object_initialized(&mut self, dest: Register, idx: ConstPoolIdx) {
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Ptr);

        let const_pool_entry = self.bytecode.const_pool(idx);

        let (cls_id, type_params) = match const_pool_entry {
            ConstPoolEntry::Class(cls_id, type_params) => (*cls_id, type_params.clone()),
            _ => unreachable!(),
        };

        let arguments = self.argument_stack.drain(..).collect::<Vec<_>>();

        let type_params = self.specialize_bty_array(&type_params);
        debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

        let class_instance_id = create_class_instance(self.vm, cls_id, &type_params);
        let class_instance = self.vm.class_instances.idx(class_instance_id);

        let alloc_size = match class_instance.size {
            InstanceSize::Fixed(size) => AllocationSize::Fixed(size as usize),
            _ => unreachable!(
                "class size type {:?} for new object not supported",
                class_instance.size
            ),
        };

        let gcpoint = self.create_gcpoint();
        let position = self.bytecode.offset_location(self.current_offset.to_u32());
        self.asm
            .allocate(REG_RESULT.into(), alloc_size, position, gcpoint);

        // store gc object in temporary storage
        self.emit_store_register(REG_RESULT.into(), dest);

        self.asm.initialize_object(REG_RESULT, &*class_instance);

        let obj_reg = REG_TMP1;
        self.emit_load_register(dest, obj_reg.into());

        assert_eq!(arguments.len(), class_instance.fields.len());

        // Initialize all class fields.
        for (&argument, field) in arguments.iter().zip(class_instance.fields.iter()) {
            self.emit_store_field_raw(dest, obj_reg, field.offset, argument);
        }
    }

    fn emit_new_array(&mut self, dest: Register, idx: ConstPoolIdx, length: Register) {
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Ptr);
        assert_eq!(self.bytecode.register_type(length), BytecodeType::Int64);

        let const_pool_entry = self.bytecode.const_pool(idx);

        let (cls_id, type_params) = match const_pool_entry {
            ConstPoolEntry::Class(cls_id, type_params) => (*cls_id, type_params),
            _ => unreachable!(),
        };

        let type_params = self.specialize_bty_array(&type_params);

        let class_instance_id = create_class_instance(self.vm, cls_id, &type_params);
        let class_instance = self.vm.class_instances.idx(class_instance_id);

        let length_reg = REG_TMP1;
        let size_reg = REG_TMP2;
        self.emit_load_register(length, length_reg.into());

        let array_header_size = Header::size() as usize + mem::ptr_width_usize();

        let alloc_size = match class_instance.size {
            InstanceSize::PrimitiveArray(size) | InstanceSize::StructArray(size) => {
                assert_ne!(size, 0);
                self.asm
                    .determine_array_size(size_reg, length_reg, size, true);
                AllocationSize::Dynamic(size_reg)
            }
            InstanceSize::ObjArray => {
                self.asm
                    .determine_array_size(size_reg, length_reg, mem::ptr_width(), true);
                AllocationSize::Dynamic(size_reg)
            }
            InstanceSize::UnitArray => AllocationSize::Fixed(array_header_size),
            _ => unreachable!(
                "class size type {:?} for new array not supported",
                class_instance.size
            ),
        };

        // REG_TMP1 and REG_TMP2 should be restored in the slow path of the allocation.
        assert!(CALLEE_SAVED_REGS.contains(&length_reg));
        assert!(CALLEE_SAVED_REGS.contains(&size_reg));

        let gcpoint = self.create_gcpoint();
        let position = self.bytecode.offset_location(self.current_offset.to_u32());
        self.asm
            .allocate(REG_RESULT.into(), alloc_size, position, gcpoint);

        // Store object address in bytecode register right away.
        self.emit_store_register(REG_RESULT.into(), dest);

        self.asm
            .initialize_array_header(REG_RESULT, &*class_instance, length_reg, size_reg);

        match class_instance.size {
            InstanceSize::PrimitiveArray(size) | InstanceSize::StructArray(size) => {
                self.emit_array_initialization(REG_RESULT, length_reg, size);
            }
            InstanceSize::ObjArray => {
                self.emit_array_initialization(REG_RESULT, length_reg, mem::ptr_width());
            }
            InstanceSize::UnitArray => {}
            _ => unreachable!(),
        }
    }

    fn emit_array_initialization(&mut self, object_start: Reg, array_length: Reg, size: i32) {
        let array_data_start = object_start;
        self.asm.int_add_imm(
            MachineMode::Ptr,
            array_data_start,
            object_start,
            (Header::size() + mem::ptr_width()) as i64,
        );
        let size_without_header = array_length;
        self.asm
            .determine_array_size(size_without_header, array_length, size, false);
        let array_data_limit = array_length;
        self.asm.int_add(
            MachineMode::Ptr,
            array_data_limit,
            size_without_header,
            REG_RESULT,
        );
        self.asm
            .fill_zero_dynamic(array_data_start, array_data_limit);
    }

    fn emit_new_tuple(&mut self, dest: Register, idx: ConstPoolIdx) {
        let source_type_array = match self.bytecode.const_pool(idx) {
            ConstPoolEntry::Tuple(ref source_type_array) => source_type_array,
            _ => unreachable!(),
        };
        let subtypes = self.specialize_bty_array(source_type_array);
        let tuple = get_concrete_tuple_bty_array(self.vm, subtypes.clone());
        let dest_offset = self.register_offset(dest);
        let mut arg_idx = 0;
        let arguments = std::mem::replace(&mut self.argument_stack, Vec::new());

        for (subtype, &subtype_offset) in subtypes.iter().zip(tuple.offsets()) {
            if subtype.is_unit() {
                continue;
            }

            let src = arguments[arg_idx];
            let src = self.reg(src);
            let dest = RegOrOffset::Offset(dest_offset + subtype_offset);
            self.asm.copy_bytecode_ty(subtype, dest, src);
            arg_idx += 1;
        }
    }

    fn emit_new_enum(&mut self, dest: Register, idx: ConstPoolIdx) {
        let (enum_id, type_params, variant_idx) = match self.bytecode.const_pool(idx) {
            ConstPoolEntry::EnumVariant(enum_id, type_params, variant_idx) => {
                (*enum_id, type_params, *variant_idx)
            }
            _ => unreachable!(),
        };

        let type_params = self.specialize_bty_array(&type_params);
        debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

        let enum_ = &self.vm.program.enums[enum_id.0 as usize];

        let enum_instance_id = create_enum_instance(self.vm, enum_id, type_params.clone());
        let enum_instance = self.vm.enum_instances.idx(enum_instance_id);

        let arguments = self.argument_stack.drain(..).collect::<Vec<_>>();

        match enum_instance.layout {
            EnumLayout::Int => {
                assert_eq!(0, arguments.len());
                self.asm
                    .load_int_const(MachineMode::Int32, REG_RESULT, variant_idx as i64);
                self.emit_store_register_as(REG_RESULT.into(), dest, MachineMode::Int32);
            }
            EnumLayout::Ptr => {
                let variant = &enum_.variants[variant_idx as usize];

                if variant.arguments.is_empty() {
                    assert_eq!(0, arguments.len());
                    self.asm.load_nil(REG_RESULT);
                    self.emit_store_register_as(REG_RESULT.into(), dest, MachineMode::Ptr);
                } else {
                    assert_eq!(1, arguments.len());
                    let ty = self.specialize_register_type(arguments[0]);
                    assert!(ty.is_ptr() || ty.is_trait());
                    self.emit_load_register(arguments[0], REG_RESULT.into());
                    self.emit_store_register_as(REG_RESULT.into(), dest, MachineMode::Ptr);
                }
            }

            EnumLayout::Tagged => {
                let cls_def_id = ensure_class_instance_for_enum_variant(
                    self.vm,
                    &*enum_instance,
                    &*enum_,
                    variant_idx,
                );

                let cls = self.vm.class_instances.idx(cls_def_id);

                let alloc_size = match cls.size {
                    InstanceSize::Fixed(size) => size as usize,
                    _ => unreachable!(
                        "class size type {:?} for new object not supported",
                        cls.size
                    ),
                };

                let gcpoint = self.create_gcpoint();
                let position = self.bytecode.offset_location(self.current_offset.to_u32());
                self.asm.allocate(
                    REG_TMP1.into(),
                    AllocationSize::Fixed(alloc_size),
                    position,
                    gcpoint,
                );

                // store gc object in register
                comment!(
                    self,
                    format!("NewEnum: store object address in register {}", dest)
                );
                self.emit_store_register_as(REG_TMP1.into(), dest, MachineMode::Ptr);

                comment!(self, format!("NewEnum: initialize object"));
                self.asm.initialize_object(REG_TMP1, &*cls);

                // store variant_idx
                comment!(self, format!("NewEnum: store variant_idx {}", variant_idx));
                self.asm
                    .load_int_const(MachineMode::Int32, REG_RESULT, variant_idx as i64);
                self.asm.store_mem(
                    MachineMode::Int32,
                    Mem::Base(REG_TMP1, Header::size()),
                    REG_RESULT.into(),
                );

                let mut field_idx = 1; // first field is variant_idx

                assert_eq!(arguments.len(), cls.fields.len() - 1);

                for arg in arguments {
                    let ty = self.specialize_register_type(arg);
                    let field = &cls.fields[field_idx];
                    comment!(self, format!("NewEnum: store register {} in object", arg));

                    let dest = RegOrOffset::RegWithOffset(REG_TMP1, field.offset);
                    let src = self.reg(arg);

                    self.asm.copy_bytecode_ty(ty, dest, src);
                    field_idx += 1;
                }
            }
        }
    }

    fn emit_new_struct(&mut self, dest: Register, idx: ConstPoolIdx) {
        let (struct_id, type_params) = match self.bytecode.const_pool(idx) {
            ConstPoolEntry::Struct(struct_id, type_params) => (*struct_id, type_params),
            _ => unreachable!(),
        };

        let type_params = self.specialize_bty_array(&type_params);
        debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

        let struct_instance_id = create_struct_instance(self.vm, struct_id, type_params);
        let struct_instance = self.vm.struct_instances.idx(struct_instance_id);

        let arguments = self.argument_stack.drain(..).collect::<Vec<_>>();

        let dest_offset = self.register_offset(dest);
        self.asm.lea(REG_TMP1, Mem::Local(dest_offset));

        assert_eq!(arguments.len(), struct_instance.fields.len());

        for (field_idx, &arg) in arguments.iter().enumerate() {
            let ty = self.specialize_register_type(arg);
            let field = &struct_instance.fields[field_idx];
            comment!(self, format!("NewStruct: store register {} in struct", arg));

            let dest = RegOrOffset::RegWithOffset(REG_TMP1, field.offset);
            let src = self.reg(arg);

            self.asm.copy_bytecode_ty(ty, dest, src);
        }
    }

    fn emit_new_trait_object(&mut self, dest: Register, idx: ConstPoolIdx, src: Register) {
        let (trait_id, type_params, object_ty) = match self.bytecode.const_pool(idx) {
            ConstPoolEntry::Trait(trait_id, type_params, object_ty) => {
                (*trait_id, type_params, object_ty.clone())
            }
            _ => unreachable!(),
        };

        let trait_id = TraitId(trait_id.0);

        let type_params = self.specialize_bty_array(type_params);
        debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

        let object_ty = self.specialize_bty(object_ty);
        debug_assert!(object_ty.is_concrete_type());

        let class_instance_id = ensure_class_instance_for_trait_object(
            self.vm,
            trait_id,
            &type_params,
            object_ty.clone(),
        );

        let cls = self.vm.class_instances.idx(class_instance_id);

        let alloc_size = match cls.size {
            InstanceSize::Fixed(size) => size as usize,
            _ => unreachable!(
                "class size type {:?} for new object not supported",
                cls.size
            ),
        };

        let gcpoint = self.create_gcpoint();
        let position = self.bytecode.offset_location(self.current_offset.to_u32());
        self.asm.allocate(
            REG_TMP1.into(),
            AllocationSize::Fixed(alloc_size),
            position,
            gcpoint,
        );

        // store gc object in register
        comment!(
            self,
            format!("NewTraitObject: store object address in register {}", dest)
        );
        self.emit_store_register_as(REG_TMP1.into(), dest, MachineMode::Ptr);

        comment!(self, format!("NewTraitObject: initialize object"));
        self.asm.initialize_object(REG_TMP1, &*cls);

        assert_eq!(cls.fields.len(), 1);
        let field = &cls.fields[0];
        comment!(
            self,
            format!("NewTraitObject: store register {} in object", src)
        );

        let dest = RegOrOffset::RegWithOffset(REG_TMP1, field.offset);
        let src = self.reg(src);

        self.asm.copy_bytecode_ty(object_ty, dest, src);
    }

    fn emit_new_lambda(&mut self, dest: Register, idx: ConstPoolIdx) {
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Ptr);

        let (fct_id, type_params) = match self.bytecode.const_pool(idx) {
            ConstPoolEntry::Fct(fct_id, type_params) => (*fct_id, type_params),
            _ => unreachable!(),
        };

        let type_params = self.specialize_bty_array(&type_params);
        debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

        let cls_def_id = ensure_class_instance_for_lambda(self.vm, fct_id, type_params);

        let arguments = self.argument_stack.drain(..).collect::<Vec<_>>();

        let cls = self.vm.class_instances.idx(cls_def_id);

        let alloc_size = match cls.size {
            InstanceSize::Fixed(size) => size as usize,
            _ => unreachable!(
                "class size type {:?} for new object not supported",
                cls.size
            ),
        };

        let gcpoint = self.create_gcpoint();
        let position = self.bytecode.offset_location(self.current_offset.to_u32());
        let object_reg = REG_TMP1;

        self.asm.allocate(
            object_reg,
            AllocationSize::Fixed(alloc_size),
            position,
            gcpoint,
        );

        // store gc object in register
        comment!(
            self,
            format!("NewLambda: store object address in register {}", dest)
        );
        self.emit_store_register_as(REG_TMP1.into(), dest, MachineMode::Ptr);

        comment!(self, format!("NewLambda: initialize object"));
        self.asm.initialize_object(object_reg, &*cls);

        // Store context pointer.
        if arguments.is_empty() {
            self.asm.load_int_const(MachineMode::Ptr, REG_RESULT, 0);
        } else {
            assert_eq!(arguments.len(), 1);
            self.emit_load_register(arguments[0], REG_RESULT.into());
        }

        self.asm.store_mem(
            MachineMode::Ptr,
            Mem::Base(object_reg, Header::size()),
            REG_RESULT.into(),
        );
    }

    fn emit_array_length(&mut self, dest: Register, arr: Register) {
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Int64);
        assert_eq!(self.bytecode.register_type(arr), BytecodeType::Ptr);

        let position = self.bytecode.offset_location(self.current_offset.to_u32());

        self.emit_load_register(arr, REG_RESULT.into());
        self.asm
            .test_if_nil_bailout(position, REG_RESULT, Trap::NIL);

        self.asm.load_mem(
            MachineMode::Int64,
            REG_RESULT.into(),
            Mem::Base(REG_RESULT, Header::size()),
        );

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_store_array(&mut self, src: Register, arr: Register, idx: Register) {
        assert_eq!(self.bytecode.register_type(idx), BytecodeType::Int64);
        assert_eq!(self.bytecode.register_type(arr), BytecodeType::Ptr);

        let position = self.bytecode.offset_location(self.current_offset.to_u32());

        self.emit_load_register(arr, REG_RESULT.into());
        self.asm
            .test_if_nil_bailout(position, REG_RESULT, Trap::NIL);

        self.emit_load_register(idx, REG_TMP1.into());

        if !self.vm.flags.omit_bounds_check {
            self.asm
                .check_index_out_of_bounds(position, REG_RESULT, REG_TMP1);
        }

        let src_type = self.specialize_register_type(src);

        match src_type {
            BytecodeType::Unit => {}

            BytecodeType::Tuple(ref subtypes) => {
                let tuple = get_concrete_tuple_bty(self.vm, &src_type);
                let element_size = tuple.size();
                self.asm
                    .array_address(REG_TMP1, REG_RESULT, REG_TMP1, element_size);
                let src_offset = self.register_offset(src);

                self.asm.copy_tuple(
                    subtypes.clone(),
                    RegOrOffset::Reg(REG_TMP1),
                    RegOrOffset::Offset(src_offset),
                );

                let needs_write_barrier = tuple.contains_references();

                if self.vm.gc.needs_write_barrier() && needs_write_barrier {
                    self.emit_load_register(arr, REG_RESULT.into());
                    self.asm.emit_write_barrier(REG_RESULT);
                }
            }

            BytecodeType::Struct(struct_id, type_params) => {
                let struct_instance_id =
                    create_struct_instance(self.vm, struct_id, type_params.clone());
                let struct_instance = self.vm.struct_instances.idx(struct_instance_id);

                self.asm
                    .array_address(REG_TMP1, REG_RESULT, REG_TMP1, struct_instance.size);
                let src_offset = self.register_offset(src);

                self.asm.copy_struct(
                    struct_id,
                    type_params.clone(),
                    RegOrOffset::Reg(REG_TMP1),
                    RegOrOffset::Offset(src_offset),
                );

                let needs_write_barrier = struct_instance.contains_references();

                if self.vm.gc.needs_write_barrier() && needs_write_barrier {
                    self.emit_load_register(arr, REG_RESULT.into());
                    self.asm.emit_write_barrier(REG_RESULT);
                }
            }

            BytecodeType::Enum(enum_id, type_params) => {
                let enum_instance_id = create_enum_instance(self.vm, enum_id, type_params);
                let enum_instance = self.vm.enum_instances.idx(enum_instance_id);

                let mode = match enum_instance.layout {
                    EnumLayout::Int => MachineMode::Int32,
                    EnumLayout::Ptr | EnumLayout::Tagged => MachineMode::Ptr,
                };

                let value_reg = REG_TMP2.into();

                self.emit_load_register_as(src, value_reg, mode);

                self.asm.store_mem(
                    mode,
                    Mem::Index(REG_RESULT, REG_TMP1, mode.size(), offset_of_array_data()),
                    value_reg,
                );

                let needs_write_barrier = mode == MachineMode::Ptr;

                if self.vm.gc.needs_write_barrier() && needs_write_barrier {
                    self.emit_load_register(arr, REG_RESULT.into());
                    self.asm.emit_write_barrier(REG_RESULT);
                }
            }

            BytecodeType::TypeAlias(..)
            | BytecodeType::TypeParam(_)
            | BytecodeType::Class(_, _)
            | BytecodeType::Lambda(_, _)
            | BytecodeType::This => {
                unreachable!()
            }
            BytecodeType::UInt8
            | BytecodeType::Int32
            | BytecodeType::Bool
            | BytecodeType::Char
            | BytecodeType::Int64
            | BytecodeType::Float32
            | BytecodeType::Float64
            | BytecodeType::Ptr
            | BytecodeType::Trait(_, _) => {
                let src_mode = mode(self.vm, src_type.clone());

                let value_reg: AnyReg = if src_mode.is_float() {
                    FREG_RESULT.into()
                } else {
                    REG_TMP2.into()
                };

                self.emit_load_register(src, value_reg.into());

                self.asm.store_mem(
                    src_mode,
                    Mem::Index(
                        REG_RESULT,
                        REG_TMP1,
                        src_mode.size(),
                        offset_of_array_data(),
                    ),
                    value_reg,
                );

                let needs_write_barrier = src_type.is_ptr();

                if self.vm.gc.needs_write_barrier() && needs_write_barrier {
                    self.emit_load_register(arr, REG_RESULT.into());
                    self.asm.emit_write_barrier(REG_RESULT);
                }
            }
        }
    }

    fn emit_load_array(&mut self, dest: Register, arr: Register, idx: Register) {
        assert_eq!(self.bytecode.register_type(idx), BytecodeType::Int64);
        assert_eq!(self.bytecode.register_type(arr), BytecodeType::Ptr);

        let position = self.bytecode.offset_location(self.current_offset.to_u32());

        self.emit_load_register(arr, REG_RESULT.into());
        self.asm
            .test_if_nil_bailout(position, REG_RESULT, Trap::NIL);

        self.emit_load_register(idx, REG_TMP1.into());

        if !self.vm.flags.omit_bounds_check {
            self.asm
                .check_index_out_of_bounds(position, REG_RESULT, REG_TMP1);
        }

        let dest_type = self.specialize_register_type(dest);

        match dest_type {
            BytecodeType::Unit => {}

            BytecodeType::Tuple(ref subtypes) => {
                let element_size = get_concrete_tuple_bty(self.vm, &dest_type).size();
                self.asm
                    .array_address(REG_TMP1, REG_RESULT, REG_TMP1, element_size);
                let dest_offset = self.register_offset(dest);

                self.asm.copy_tuple(
                    subtypes.clone(),
                    RegOrOffset::Offset(dest_offset),
                    RegOrOffset::Reg(REG_TMP1),
                );
            }

            BytecodeType::Struct(struct_id, type_params) => {
                let struct_instance_id =
                    create_struct_instance(self.vm, struct_id, type_params.clone());
                let struct_instance = self.vm.struct_instances.idx(struct_instance_id);

                let element_size = struct_instance.size;
                self.asm
                    .array_address(REG_TMP1, REG_RESULT, REG_TMP1, element_size);
                let dest_offset = self.register_offset(dest);

                self.asm.copy_struct(
                    struct_id,
                    type_params.clone(),
                    RegOrOffset::Offset(dest_offset),
                    RegOrOffset::Reg(REG_TMP1),
                );
            }

            BytecodeType::Enum(enum_id, type_params) => {
                let enum_instance_id = create_enum_instance(self.vm, enum_id, type_params);
                let enum_instance = self.vm.enum_instances.idx(enum_instance_id);

                let mode = match enum_instance.layout {
                    EnumLayout::Int => MachineMode::Int32,
                    EnumLayout::Ptr | EnumLayout::Tagged => MachineMode::Ptr,
                };

                self.asm
                    .load_array_elem(mode, REG_RESULT.into(), REG_RESULT, REG_TMP1);
                self.emit_store_register_as(REG_RESULT.into(), dest, mode);
            }

            BytecodeType::TypeAlias(..)
            | BytecodeType::TypeParam(_)
            | BytecodeType::Class(_, _)
            | BytecodeType::Lambda(_, _)
            | BytecodeType::This => {
                unreachable!()
            }
            BytecodeType::UInt8
            | BytecodeType::Int32
            | BytecodeType::Bool
            | BytecodeType::Char
            | BytecodeType::Int64
            | BytecodeType::Float32
            | BytecodeType::Float64
            | BytecodeType::Ptr
            | BytecodeType::Trait(_, _) => {
                let register = result_reg(self.vm, dest_type.clone());
                self.asm
                    .load_array_elem(mode(self.vm, dest_type), register, REG_RESULT, REG_TMP1);
                self.emit_store_register(register, dest);
            }
        }
    }

    fn emit_invoke_virtual_from_bytecode(&mut self, dest: Register, fct_idx: ConstPoolIdx) {
        let (trait_object_ty, fct_id, type_params) = match self.bytecode.const_pool(fct_idx) {
            ConstPoolEntry::TraitObjectMethod(trait_object_ty, fct_id, type_params) => {
                (trait_object_ty.clone(), *fct_id, type_params)
            }
            _ => unreachable!(),
        };

        let type_params = self.specialize_bty_array(type_params);
        debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

        let location = self.bytecode.offset_location(self.current_offset.to_u32());
        let arguments = self.argument_stack.drain(..).collect::<Vec<_>>();

        self.emit_invoke_virtual(
            dest,
            trait_object_ty,
            fct_id,
            type_params,
            arguments,
            location,
        );
    }

    fn emit_invoke_lambda_from_bytecode(&mut self, dest: Register, idx: ConstPoolIdx) {
        let (params, return_type) = match self.bytecode.const_pool(idx) {
            ConstPoolEntry::Lambda(params, return_type) => (params.clone(), return_type.clone()),
            _ => unreachable!(),
        };

        let location = self.bytecode.offset_location(self.current_offset.to_u32());
        let arguments = self.argument_stack.drain(..).collect::<Vec<_>>();

        self.emit_invoke_lambda(dest, params, return_type, arguments, location);
    }

    fn emit_invoke_lambda(
        &mut self,
        dest: Register,
        params: BytecodeTypeArray,
        return_type: BytecodeType,
        arguments: Vec<Register>,
        location: Location,
    ) {
        let bytecode_type = self.specialize_register_type(dest);

        let self_register = arguments[0];

        let bytecode_type_self = self.bytecode.register_type(self_register);
        assert!(bytecode_type_self.is_ptr());

        let fct_return_type = self.specialize_bty(return_type);
        assert!(fct_return_type.is_concrete_type());

        let mut params_including_self = params.to_vec();
        params_including_self.insert(0, BytecodeType::Ptr);
        let params_including_self = BytecodeTypeArray::new(params_including_self);
        let params_including_self = self.specialize_bty_array(&params_including_self);

        let argsize = self.emit_invoke_arguments(dest, fct_return_type.clone(), arguments);

        let vtable_index = 0;
        let gcpoint = self.create_gcpoint();

        let (result_reg, result_mode) = self.call_result_reg_and_mode(bytecode_type);

        let self_index = if result_passed_as_argument(fct_return_type.clone()) {
            1
        } else {
            0
        };

        let lazy_compilation_site =
            LazyCompilationSite::Lambda(self_index == 0, params_including_self, fct_return_type);

        self.asm.virtual_call(
            vtable_index,
            self_index,
            location,
            gcpoint,
            result_mode,
            result_reg,
            lazy_compilation_site,
        );

        self.asm.decrease_stack_frame(argsize);

        self.store_call_result(dest, result_reg);
    }

    fn emit_invoke_virtual(
        &mut self,
        dest: Register,
        trait_object_ty: BytecodeType,
        fct_id: FunctionId,
        type_params: BytecodeTypeArray,
        arguments: Vec<Register>,
        location: Location,
    ) {
        let bytecode_type = self.specialize_register_type(dest);

        let self_register = arguments[0];

        let bytecode_type_self = self.bytecode.register_type(self_register);
        assert!(bytecode_type_self.is_ptr() || bytecode_type_self.is_trait());

        let fct = &self.vm.program.functions[fct_id.0 as usize];

        let fct_return_type =
            self.specialize_bty(specialize_bty(fct.return_type.clone(), &type_params));
        assert!(fct_return_type.is_concrete_type());

        let argsize = self.emit_invoke_arguments(dest, fct_return_type.clone(), arguments);

        let vtable_index = fct.vtable_index.unwrap();
        let gcpoint = self.create_gcpoint();

        let (result_reg, result_mode) = self.call_result_reg_and_mode(bytecode_type);

        let self_index = if result_passed_as_argument(fct_return_type.clone()) {
            1
        } else {
            0
        };

        let lazy_compilation_site = LazyCompilationSite::Virtual(
            self_index == 0,
            trait_object_ty,
            fct_id,
            vtable_index,
            type_params.clone(),
        );

        self.asm.virtual_call(
            vtable_index,
            self_index,
            location,
            gcpoint,
            result_mode,
            result_reg,
            lazy_compilation_site,
        );

        self.asm.decrease_stack_frame(argsize);

        self.store_call_result(dest, result_reg);
    }

    fn emit_invoke_direct_from_bytecode(&mut self, dest: Register, fct_idx: ConstPoolIdx) {
        let (fct_id, type_params) = match self.bytecode.const_pool(fct_idx) {
            ConstPoolEntry::Fct(fct_id, type_params) => (*fct_id, type_params.clone()),
            _ => unreachable!(),
        };

        let type_params = self.specialize_bty_array(&type_params);
        debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

        let location = self.bytecode.offset_location(self.current_offset.to_u32());
        let arguments = self.argument_stack.drain(..).collect::<Vec<_>>();

        self.emit_invoke_direct_or_intrinsic(dest, fct_id, type_params, arguments, location);
    }

    fn emit_invoke_direct_or_intrinsic(
        &mut self,
        dest: Register,
        fct_id: FunctionId,
        type_params: BytecodeTypeArray,
        arguments: Vec<Register>,
        location: Location,
    ) {
        let fct = &self.vm.program.functions[fct_id.0 as usize];

        if let Some(intrinsic) = fct.intrinsic {
            self.emit_invoke_intrinsic(dest, fct_id, intrinsic, type_params, arguments, location);
        } else {
            self.emit_invoke_direct(dest, fct_id, type_params, arguments, location);
        };
    }

    fn emit_invoke_direct(
        &mut self,
        dest: Register,
        fct_id: FunctionId,
        type_params: BytecodeTypeArray,
        arguments: Vec<Register>,
        location: Location,
    ) {
        let self_register = arguments[0];

        let dest_ty = self.specialize_register_type(dest);

        let fct = &self.vm.program.functions[fct_id.0 as usize];

        let fct_return_type =
            self.specialize_bty(specialize_bty(fct.return_type.clone(), &type_params));
        assert!(fct_return_type.is_concrete_type());

        let bytecode_type_self = self.bytecode.register_type(self_register);

        if bytecode_type_self.is_ptr() {
            self.emit_load_register(self_register, REG_RESULT.into());

            self.asm
                .test_if_nil_bailout(location, REG_RESULT.into(), Trap::NIL);
        }

        let argsize = self.emit_invoke_arguments(dest, fct_return_type.clone(), arguments);

        let ptr = get_function_address(self.vm, fct_id, type_params.clone());
        let gcpoint = self.create_gcpoint();

        let (result_reg, result_mode) = self.call_result_reg_and_mode(dest_ty);

        self.asm.direct_call(
            fct_id,
            ptr,
            type_params,
            location,
            gcpoint,
            result_mode,
            result_reg,
        );

        self.asm.decrease_stack_frame(argsize);

        self.store_call_result(dest, result_reg);
    }

    fn emit_invoke_static_from_bytecode(&mut self, dest: Register, fct_idx: ConstPoolIdx) {
        let (fct_id, type_params) = match self.bytecode.const_pool(fct_idx) {
            ConstPoolEntry::Fct(fct_id, type_params) => (*fct_id, type_params.clone()),
            _ => unreachable!(),
        };

        let type_params = self.specialize_bty_array(&type_params);
        debug_assert!(type_params.iter().all(|ty| ty.is_concrete_type()));

        let location = self.bytecode.offset_location(self.current_offset.to_u32());
        let arguments = self.argument_stack.drain(..).collect::<Vec<_>>();

        self.emit_invoke_static_or_intrinsic(dest, fct_id, type_params, arguments, location);
    }

    fn emit_invoke_static_or_intrinsic(
        &mut self,
        dest: Register,
        fct_id: FunctionId,
        type_params: BytecodeTypeArray,
        arguments: Vec<Register>,
        location: Location,
    ) {
        let fct = &self.vm.program.functions[fct_id.0 as usize];

        if let Some(intrinsic) = fct.intrinsic {
            self.emit_invoke_intrinsic(dest, fct_id, intrinsic, type_params, arguments, location);
        } else {
            self.emit_invoke_static(dest, fct_id, type_params, arguments, location);
        }
    }

    fn emit_invoke_static(
        &mut self,
        dest: Register,
        fct_id: FunctionId,
        type_params: BytecodeTypeArray,
        arguments: Vec<Register>,
        location: Location,
    ) {
        let bytecode_type = self.specialize_register_type(dest);

        let fct = &self.vm.program.functions[fct_id.0 as usize];

        let fct_return_type =
            self.specialize_bty(specialize_bty(fct.return_type.clone(), &type_params));
        assert!(fct_return_type.is_concrete_type());

        let argsize = self.emit_invoke_arguments(dest, fct_return_type.clone(), arguments);

        let ptr = get_function_address(self.vm, fct_id, type_params.clone());
        let gcpoint = self.create_gcpoint();

        let (result_reg, result_mode) = self.call_result_reg_and_mode(bytecode_type);

        self.asm.direct_call(
            fct_id,
            ptr,
            type_params,
            location,
            gcpoint,
            result_mode,
            result_reg,
        );

        self.asm.decrease_stack_frame(argsize);

        self.store_call_result(dest, result_reg);
    }

    fn store_call_result(&mut self, dest: Register, reg: AnyReg) {
        let bytecode_ty = self.specialize_register_type(dest);
        if !result_passed_as_argument(bytecode_ty.clone()) && !bytecode_ty.is_unit() {
            self.emit_store_register(reg, dest);
        }
    }

    fn call_result_reg_and_mode(
        &self,
        bytecode_type: BytecodeType,
    ) -> (AnyReg, Option<MachineMode>) {
        match bytecode_type {
            BytecodeType::Struct(_, _) => (REG_RESULT.into(), None),
            BytecodeType::Tuple(_) => (REG_RESULT.into(), None),
            BytecodeType::Unit => (REG_RESULT.into(), None),
            bytecode_type => (
                result_reg(self.vm, bytecode_type.clone()),
                Some(mode(self.vm, bytecode_type)),
            ),
        }
    }

    fn emit_invoke_generic(&mut self, dest: Register, fct_idx: ConstPoolIdx, is_static: bool) {
        let (id, trait_fct_id, type_params) = match self.bytecode.const_pool(fct_idx) {
            ConstPoolEntry::Generic(id, fct_id, type_params) => (*id, *fct_id, type_params.clone()),
            _ => unreachable!(),
        };

        let fct = &self.vm.program.functions[trait_fct_id.0 as usize];

        let trait_id = match fct.kind {
            FunctionKind::Trait(trait_id) => trait_id,
            _ => unreachable!(),
        };
        let trait_ty = BytecodeType::Trait(trait_id, type_params.clone());

        let ty = self.type_params[id as usize].clone();
        let callee_id = find_trait_impl(self.vm, trait_fct_id, trait_ty, ty);

        let pos = self.bytecode.offset_location(self.current_offset.to_u32());
        let arguments = self.argument_stack.drain(..).collect::<Vec<_>>();

        if is_static {
            self.emit_invoke_static_or_intrinsic(dest, callee_id, type_params, arguments, pos);
        } else {
            self.emit_invoke_direct_or_intrinsic(dest, callee_id, type_params, arguments, pos);
        }
    }

    fn emit_invoke_intrinsic(
        &mut self,
        dest: Register,
        fct_id: FunctionId,
        intrinsic: Intrinsic,
        type_params: BytecodeTypeArray,
        arguments: Vec<Register>,
        location: Location,
    ) {
        match intrinsic {
            Intrinsic::Float32Abs | Intrinsic::Float64Abs => {
                debug_assert_eq!(arguments.len(), 1);
                let src_reg = arguments[0];
                self.emit_intrinsic_abs_float(dest, src_reg);
            }

            Intrinsic::Float32RoundToZero | Intrinsic::Float64RoundToZero => {
                self.emit_intrinsic_float_round_tozero(
                    dest,
                    fct_id,
                    intrinsic,
                    arguments,
                    type_params,
                    location,
                );
            }
            Intrinsic::Float32RoundUp | Intrinsic::Float64RoundUp => {
                self.emit_intrinsic_float_round_up(
                    dest,
                    fct_id,
                    intrinsic,
                    arguments,
                    type_params,
                    location,
                );
            }
            Intrinsic::Float32RoundDown | Intrinsic::Float64RoundDown => {
                self.emit_intrinsic_float_round_down(
                    dest,
                    fct_id,
                    intrinsic,
                    arguments,
                    type_params,
                    location,
                );
            }
            Intrinsic::Float32RoundHalfEven | Intrinsic::Float64RoundHalfEven => {
                self.emit_intrinsic_float_round_halfeven(
                    dest,
                    fct_id,
                    intrinsic,
                    arguments,
                    type_params,
                    location,
                );
            }

            Intrinsic::Float32Sqrt | Intrinsic::Float64Sqrt => {
                self.emit_intrinsic_float_sqrt(
                    dest,
                    fct_id,
                    intrinsic,
                    arguments,
                    type_params,
                    location,
                );
            }

            Intrinsic::Int32CountZeroBits
            | Intrinsic::Int32CountZeroBitsLeading
            | Intrinsic::Int32CountZeroBitsTrailing
            | Intrinsic::Int32CountOneBits
            | Intrinsic::Int32CountOneBitsLeading
            | Intrinsic::Int32CountOneBitsTrailing
            | Intrinsic::Int64CountZeroBits
            | Intrinsic::Int64CountZeroBitsLeading
            | Intrinsic::Int64CountZeroBitsTrailing
            | Intrinsic::Int64CountOneBits
            | Intrinsic::Int64CountOneBitsLeading
            | Intrinsic::Int64CountOneBitsTrailing => {
                self.emit_intrinsic_count_bits(
                    dest,
                    fct_id,
                    intrinsic,
                    arguments,
                    type_params,
                    location,
                );
            }

            Intrinsic::ReinterpretFloat32AsInt32
            | Intrinsic::ReinterpretInt32AsFloat32
            | Intrinsic::ReinterpretFloat64AsInt64
            | Intrinsic::ReinterpretInt64AsFloat64 => {
                debug_assert_eq!(arguments.len(), 1);
                let src_reg = arguments[0];

                self.emit_reinterpret(dest, src_reg);
            }

            Intrinsic::Unreachable => {
                let native_fct = NativeFct {
                    fctptr: Address::from_ptr(stdlib::unreachable as *const u8),
                    args: BytecodeTypeArray::empty(),
                    return_type: BytecodeType::Unit,
                    desc: NativeFctKind::RuntimeEntryTrampoline(fct_id),
                };
                let gcpoint = self.create_gcpoint();
                let result = REG_RESULT.into();
                self.asm.runtime_call(native_fct, location, gcpoint, result);

                // Method should never return
                self.asm.debug();
            }

            Intrinsic::PromoteFloat32ToFloat64 => {
                assert_eq!(arguments.len(), 1);
                let src_reg = arguments[0];
                self.emit_promote_float(dest, src_reg);
            }

            Intrinsic::DemoteFloat64ToFloat32 => {
                assert_eq!(arguments.len(), 1);
                let src_reg = arguments[0];
                self.emit_demote_float64(dest, src_reg);
            }

            Intrinsic::BoolToInt32 | Intrinsic::BoolToInt64 => {
                assert_eq!(arguments.len(), 1);
                let src_reg = arguments[0];

                self.emit_load_register(src_reg, REG_RESULT.into());
                self.asm
                    .extend_byte(MachineMode::Int64, REG_RESULT, REG_RESULT);
                self.emit_store_register(REG_RESULT.into(), dest);
            }

            Intrinsic::Float32ToInt32
            | Intrinsic::Float32ToInt64
            | Intrinsic::Float64ToInt32
            | Intrinsic::Float64ToInt64 => {
                assert_eq!(arguments.len(), 1);
                let src_reg = arguments[0];

                self.emit_load_register(src_reg, FREG_RESULT.into());
                let (src_mode, dest_mode) = match intrinsic {
                    Intrinsic::Float32ToInt32 => (MachineMode::Float32, MachineMode::Int32),
                    Intrinsic::Float64ToInt32 => (MachineMode::Float64, MachineMode::Int32),
                    Intrinsic::Float32ToInt64 => (MachineMode::Float32, MachineMode::Int64),
                    Intrinsic::Float64ToInt64 => (MachineMode::Float64, MachineMode::Int64),
                    _ => unreachable!(),
                };
                self.asm
                    .float_to_int(dest_mode, REG_RESULT, src_mode, FREG_RESULT);
                self.emit_store_register(REG_RESULT.into(), dest);
            }

            Intrinsic::Int32Cmp
            | Intrinsic::Int64Cmp
            | Intrinsic::UInt8Cmp
            | Intrinsic::CharCmp => {
                assert_eq!(arguments.len(), 2);
                let lhs_reg = arguments[0];
                let rhs_reg = arguments[1];

                self.emit_load_register(lhs_reg, REG_TMP1.into());
                self.emit_load_register(rhs_reg, REG_TMP2.into());

                let mode = match intrinsic {
                    Intrinsic::Int64Cmp => MachineMode::Int64,
                    Intrinsic::Int32Cmp | Intrinsic::CharCmp => MachineMode::Int32,
                    Intrinsic::UInt8Cmp => MachineMode::Int8,
                    _ => unreachable!(),
                };

                self.asm.cmp_ordering(mode, REG_RESULT, REG_TMP1, REG_TMP2);

                self.emit_store_register(REG_RESULT.into(), dest);
            }

            Intrinsic::Float32Cmp | Intrinsic::Float64Cmp => {
                assert_eq!(arguments.len(), 2);
                let lhs_reg = arguments[0];
                let rhs_reg = arguments[1];

                self.emit_load_register(lhs_reg, FREG_RESULT.into());
                self.emit_load_register(rhs_reg, FREG_TMP1.into());

                let mode = match intrinsic {
                    Intrinsic::Float64Cmp => MachineMode::Float64,
                    Intrinsic::Float32Cmp => MachineMode::Float32,
                    _ => unreachable!(),
                };

                self.asm
                    .float_cmp_ordering(mode, REG_RESULT, FREG_RESULT, FREG_TMP1);
                self.emit_store_register(REG_RESULT.into(), dest);
            }

            Intrinsic::Int32ToFloat32
            | Intrinsic::Int32ToFloat64
            | Intrinsic::Int64ToFloat32
            | Intrinsic::Int64ToFloat64 => {
                assert_eq!(arguments.len(), 1);
                let src_reg = arguments[0];

                self.emit_load_register(src_reg, REG_RESULT.into());
                let (src_mode, dest_mode) = match intrinsic {
                    Intrinsic::Int32ToFloat32 => (MachineMode::Int32, MachineMode::Float32),
                    Intrinsic::Int32ToFloat64 => (MachineMode::Int32, MachineMode::Float64),
                    Intrinsic::Int64ToFloat32 => (MachineMode::Int64, MachineMode::Float32),
                    Intrinsic::Int64ToFloat64 => (MachineMode::Int64, MachineMode::Float64),
                    _ => unreachable!(),
                };
                self.asm
                    .int_to_float(dest_mode, FREG_RESULT, src_mode, REG_RESULT);
                self.emit_store_register(FREG_RESULT.into(), dest);
            }

            Intrinsic::UnsafeKillRefs => {
                self.emit_intrinsic_unsafe_kill_refs(
                    dest,
                    fct_id,
                    intrinsic,
                    arguments,
                    type_params,
                    location,
                );
            }

            Intrinsic::OptionIsNone | Intrinsic::OptionIsSome => {
                self.emit_intrinsic_option_is_none(
                    dest,
                    fct_id,
                    intrinsic,
                    arguments,
                    type_params,
                    location,
                );
            }

            Intrinsic::OptionGetOrPanic => {
                self.emit_intrinsic_option_get_or_panic(
                    dest,
                    fct_id,
                    intrinsic,
                    arguments,
                    type_params,
                    location,
                );
            }

            Intrinsic::Debug => {
                self.asm.debug();
            }

            Intrinsic::AtomicInt32Get => {
                assert_eq!(arguments.len(), 1);
                let obj_reg = arguments[0];

                self.emit_load_register(obj_reg, REG_RESULT.into());
                self.asm.int_add_imm(
                    MachineMode::Ptr,
                    REG_RESULT,
                    REG_RESULT,
                    Header::size() as i64,
                );
                self.asm.load_int32_synchronized(REG_RESULT, REG_RESULT);
                self.emit_store_register(REG_RESULT.into(), dest);
            }

            Intrinsic::AtomicInt64Get => {
                assert_eq!(arguments.len(), 1);
                let obj_reg = arguments[0];

                self.emit_load_register(obj_reg, REG_RESULT.into());
                self.asm.int_add_imm(
                    MachineMode::Ptr,
                    REG_RESULT,
                    REG_RESULT,
                    Header::size() as i64,
                );
                self.asm.load_int64_synchronized(REG_RESULT, REG_RESULT);
                self.emit_store_register(REG_RESULT.into(), dest);
            }

            Intrinsic::AtomicInt32Exchange => {
                assert_eq!(arguments.len(), 2);
                let obj_reg = arguments[0];
                let value_reg = arguments[1];

                self.emit_load_register(obj_reg, REG_RESULT.into());
                self.emit_load_register(value_reg, REG_TMP1.into());
                self.asm.int_add_imm(
                    MachineMode::Ptr,
                    REG_RESULT,
                    REG_RESULT,
                    Header::size() as i64,
                );
                self.asm
                    .exchange_int32_synchronized(REG_TMP2, REG_TMP1, REG_RESULT);
                self.emit_store_register(REG_TMP2.into(), dest);
            }

            Intrinsic::AtomicInt32CompareExchange => {
                assert_eq!(arguments.len(), 3);
                let obj_reg = arguments[0];
                let expected_reg = arguments[1];
                let value_reg = arguments[2];

                self.emit_load_register(obj_reg, REG_TMP1.into());
                self.emit_load_register(expected_reg, REG_RESULT.into());
                self.emit_load_register(value_reg, REG_TMP2.into());
                self.asm
                    .int_add_imm(MachineMode::Ptr, REG_TMP1, REG_TMP1, Header::size() as i64);
                let current = self
                    .asm
                    .compare_exchange_int32_synchronized(REG_RESULT, REG_TMP2, REG_TMP1);
                self.emit_store_register(current.into(), dest);
            }

            Intrinsic::AtomicInt64CompareExchange => {
                assert_eq!(arguments.len(), 3);
                let obj_reg = arguments[0];
                let expected_reg = arguments[1];
                let value_reg = arguments[2];

                self.emit_load_register(obj_reg, REG_TMP1.into());
                self.emit_load_register(expected_reg, REG_RESULT.into());
                self.emit_load_register(value_reg, REG_TMP2.into());
                self.asm
                    .int_add_imm(MachineMode::Ptr, REG_TMP1, REG_TMP1, Header::size() as i64);
                let current = self
                    .asm
                    .compare_exchange_int64_synchronized(REG_RESULT, REG_TMP2, REG_TMP1);
                self.emit_store_register(current.into(), dest);
            }

            Intrinsic::AtomicInt32FetchAdd => {
                assert_eq!(arguments.len(), 2);
                let obj_reg = arguments[0];
                let value_reg = arguments[1];

                self.emit_load_register(obj_reg, REG_RESULT.into());
                self.emit_load_register(value_reg, REG_TMP1.into());
                self.asm.int_add_imm(
                    MachineMode::Ptr,
                    REG_RESULT,
                    REG_RESULT,
                    Header::size() as i64,
                );
                let previous = self
                    .asm
                    .fetch_add_int32_synchronized(REG_TMP2, REG_TMP1, REG_RESULT);
                self.emit_store_register(previous.into(), dest);
            }

            Intrinsic::AtomicInt64FetchAdd => {
                assert_eq!(arguments.len(), 2);
                let obj_reg = arguments[0];
                let value_reg = arguments[1];

                self.emit_load_register(obj_reg, REG_RESULT.into());
                self.emit_load_register(value_reg, REG_TMP1.into());
                self.asm.int_add_imm(
                    MachineMode::Ptr,
                    REG_RESULT,
                    REG_RESULT,
                    Header::size() as i64,
                );
                let previous = self
                    .asm
                    .fetch_add_int64_synchronized(REG_TMP2, REG_TMP1, REG_RESULT);
                self.emit_store_register(previous.into(), dest);
            }

            Intrinsic::AtomicInt64Exchange => {
                assert_eq!(arguments.len(), 2);
                let obj_reg = arguments[0];
                let value_reg = arguments[1];

                self.emit_load_register(obj_reg, REG_RESULT.into());
                self.emit_load_register(value_reg, REG_TMP1.into());
                self.asm.int_add_imm(
                    MachineMode::Ptr,
                    REG_RESULT,
                    REG_RESULT,
                    Header::size() as i64,
                );
                self.asm
                    .exchange_int64_synchronized(REG_TMP2, REG_TMP1, REG_RESULT);
                self.emit_store_register(REG_TMP2.into(), dest);
            }

            Intrinsic::AtomicInt32Set => {
                assert_eq!(arguments.len(), 2);
                let obj_reg = arguments[0];
                let value_reg = arguments[1];

                self.emit_load_register(obj_reg, REG_RESULT.into());
                self.emit_load_register(value_reg, REG_TMP1.into());
                self.asm.int_add_imm(
                    MachineMode::Ptr,
                    REG_RESULT,
                    REG_RESULT,
                    Header::size() as i64,
                );
                self.asm.store_int32_synchronized(REG_TMP1, REG_RESULT);
            }

            Intrinsic::AtomicInt64Set => {
                assert_eq!(arguments.len(), 2);
                let obj_reg = arguments[0];
                let value_reg = arguments[1];

                self.emit_load_register(obj_reg, REG_RESULT.into());
                self.emit_load_register(value_reg, REG_TMP1.into());
                self.asm.int_add_imm(
                    MachineMode::Ptr,
                    REG_RESULT,
                    REG_RESULT,
                    Header::size() as i64,
                );
                self.asm.store_int64_synchronized(REG_TMP1, REG_RESULT);
            }

            Intrinsic::Int32MulUnchecked | Intrinsic::Int64MulUnchecked => {
                assert_eq!(arguments.len(), 2);

                let lhs_reg = arguments[0];
                let rhs_reg = arguments[1];

                let mode = match intrinsic {
                    Intrinsic::Int32MulUnchecked => MachineMode::Int32,
                    Intrinsic::Int64MulUnchecked => MachineMode::Int64,
                    _ => unreachable!(),
                };

                self.emit_load_register(lhs_reg, REG_RESULT.into());
                self.emit_load_register(rhs_reg, REG_TMP1.into());
                self.asm.int_mul(mode, REG_RESULT, REG_RESULT, REG_TMP1);
                self.emit_store_register(REG_RESULT.into(), dest);
            }

            Intrinsic::Int32NegUnchecked | Intrinsic::Int64NegUnchecked => {
                assert_eq!(arguments.len(), 1);

                let reg = arguments[0];

                let mode = match intrinsic {
                    Intrinsic::Int32NegUnchecked => MachineMode::Int32,
                    Intrinsic::Int64NegUnchecked => MachineMode::Int64,
                    _ => unreachable!(),
                };

                self.emit_load_register(reg, REG_RESULT.into());
                self.asm.int_neg(mode, REG_RESULT, REG_RESULT);
                self.emit_store_register(REG_RESULT.into(), dest);
            }

            Intrinsic::Int32SubUnchecked | Intrinsic::Int64SubUnchecked => {
                assert_eq!(arguments.len(), 2);

                let lhs_reg = arguments[0];
                let rhs_reg = arguments[1];

                let mode = match intrinsic {
                    Intrinsic::Int32SubUnchecked => MachineMode::Int32,
                    Intrinsic::Int64SubUnchecked => MachineMode::Int64,
                    _ => unreachable!(),
                };

                self.emit_load_register(lhs_reg, REG_RESULT.into());
                self.emit_load_register(rhs_reg, REG_TMP1.into());
                self.asm.int_sub(mode, REG_RESULT, REG_RESULT, REG_TMP1);
                self.emit_store_register(REG_RESULT.into(), dest);
            }

            Intrinsic::Int32AddUnchecked | Intrinsic::Int64AddUnchecked => {
                assert_eq!(arguments.len(), 2);

                let lhs_reg = arguments[0];
                let rhs_reg = arguments[1];

                let mode = match intrinsic {
                    Intrinsic::Int32AddUnchecked => MachineMode::Int32,
                    Intrinsic::Int64AddUnchecked => MachineMode::Int64,
                    _ => unreachable!(),
                };

                self.emit_load_register(lhs_reg, REG_RESULT.into());
                self.emit_load_register(rhs_reg, REG_TMP1.into());
                self.asm.int_add(mode, REG_RESULT, REG_RESULT, REG_TMP1);
                self.emit_store_register(REG_RESULT.into(), dest);
            }

            Intrinsic::UInt8ToChar | Intrinsic::UInt8ToInt32 => {
                assert_eq!(arguments.len(), 1);

                let src_reg = arguments[0];

                self.emit_extend_uint8(dest, src_reg, MachineMode::Int32);
            }

            Intrinsic::UInt8ToInt64 => {
                assert_eq!(arguments.len(), 1);

                let src_reg = arguments[0];

                self.emit_extend_uint8(dest, src_reg, MachineMode::Int64);
            }

            Intrinsic::Int32ToInt64 => {
                assert_eq!(arguments.len(), 1);

                let src_reg = arguments[0];

                self.emit_int_to_int64(dest, src_reg);
            }

            Intrinsic::CharToInt64 => {
                assert_eq!(arguments.len(), 1);

                let src_reg = arguments[0];

                self.emit_shrink(dest, MachineMode::Int64, src_reg, MachineMode::Int32);
            }

            Intrinsic::Assert => {
                assert_eq!(arguments.len(), 1);
                self.emit_load_register(arguments[0], REG_RESULT.into());
                self.asm.assert(REG_RESULT, location);
            }

            Intrinsic::Int64ToInt32 => {
                assert_eq!(arguments.len(), 1);
                let src_reg = arguments[0];
                self.emit_int64_to_int(dest, src_reg);
            }

            Intrinsic::Int64ToChar => {
                assert_eq!(arguments.len(), 1);
                let src_reg = arguments[0];
                self.emit_shrink(dest, MachineMode::Int32, src_reg, MachineMode::Int64);
            }

            Intrinsic::Int64ToUInt8 => {
                assert_eq!(arguments.len(), 1);
                let src_reg = arguments[0];
                self.emit_shrink(dest, MachineMode::Int8, src_reg, MachineMode::Int64);
            }

            Intrinsic::Int32ToChar => {
                assert_eq!(arguments.len(), 1);
                let src_reg = arguments[0];
                self.emit_shrink(dest, MachineMode::Int32, src_reg, MachineMode::Int32);
            }

            Intrinsic::Int32ToUInt8 => {
                assert_eq!(arguments.len(), 1);
                let src_reg = arguments[0];
                self.emit_shrink(dest, MachineMode::Int8, src_reg, MachineMode::Int32);
            }

            Intrinsic::CharToInt32 => {
                assert_eq!(arguments.len(), 1);
                let src_reg = arguments[0];
                self.emit_shrink(dest, MachineMode::Int32, src_reg, MachineMode::Int32);
            }

            Intrinsic::Int32RotateLeft | Intrinsic::Int64RotateLeft => {
                assert_eq!(arguments.len(), 2);
                let lhs_reg = arguments[0];
                let rhs_reg = arguments[1];
                self.emit_rol_int(dest, lhs_reg, rhs_reg);
            }

            Intrinsic::Int32RotateRight | Intrinsic::Int64RotateRight => {
                assert_eq!(arguments.len(), 2);
                let lhs_reg = arguments[0];
                let rhs_reg = arguments[1];
                self.emit_ror_int(dest, lhs_reg, rhs_reg);
            }

            Intrinsic::ThreadCurrent => {
                assert_eq!(arguments.len(), 0);
                self.asm.thread_current(REG_RESULT);
                self.emit_store_register(REG_RESULT.into(), dest);
            }

            Intrinsic::BoolEq
            | Intrinsic::Int32Eq
            | Intrinsic::Int64Eq
            | Intrinsic::Float32Eq
            | Intrinsic::Float64Eq
            | Intrinsic::CharEq
            | Intrinsic::UInt8Eq => {
                assert_eq!(arguments.len(), 2);
                let lhs_reg = arguments[0];
                let rhs_reg = arguments[1];
                self.emit_test_generic(dest, lhs_reg, rhs_reg, CondCode::Equal);
            }

            Intrinsic::Int32Add
            | Intrinsic::Int64Add
            | Intrinsic::Float32Add
            | Intrinsic::Float64Add => {
                assert_eq!(arguments.len(), 2);
                let lhs_reg = arguments[0];
                let rhs_reg = arguments[1];
                self.emit_add(dest, lhs_reg, rhs_reg);
            }

            Intrinsic::Int32Sub
            | Intrinsic::Int64Sub
            | Intrinsic::Float32Sub
            | Intrinsic::Float64Sub => {
                assert_eq!(arguments.len(), 2);
                let lhs_reg = arguments[0];
                let rhs_reg = arguments[1];
                self.emit_sub(dest, lhs_reg, rhs_reg);
            }

            Intrinsic::Int32Mul
            | Intrinsic::Int64Mul
            | Intrinsic::Float32Mul
            | Intrinsic::Float64Mul => {
                assert_eq!(arguments.len(), 2);
                let lhs_reg = arguments[0];
                let rhs_reg = arguments[1];
                self.emit_mul(dest, lhs_reg, rhs_reg);
            }

            Intrinsic::Int32Div
            | Intrinsic::Int64Div
            | Intrinsic::Float32Div
            | Intrinsic::Float64Div => {
                assert_eq!(arguments.len(), 2);
                let lhs_reg = arguments[0];
                let rhs_reg = arguments[1];
                self.emit_div(dest, lhs_reg, rhs_reg);
            }

            Intrinsic::Int32Mod | Intrinsic::Int64Mod => {
                assert_eq!(arguments.len(), 2);
                let lhs_reg = arguments[0];
                let rhs_reg = arguments[1];
                self.emit_mod(dest, lhs_reg, rhs_reg);
            }

            Intrinsic::Int32And | Intrinsic::Int64And => {
                assert_eq!(arguments.len(), 2);
                let lhs_reg = arguments[0];
                let rhs_reg = arguments[1];
                self.emit_and(dest, lhs_reg, rhs_reg);
            }

            Intrinsic::Int32Or | Intrinsic::Int64Or => {
                assert_eq!(arguments.len(), 2);
                let lhs_reg = arguments[0];
                let rhs_reg = arguments[1];
                self.emit_or(dest, lhs_reg, rhs_reg);
            }

            Intrinsic::Int32Xor | Intrinsic::Int64Xor => {
                assert_eq!(arguments.len(), 2);
                let lhs_reg = arguments[0];
                let rhs_reg = arguments[1];
                self.emit_xor(dest, lhs_reg, rhs_reg);
            }

            Intrinsic::BoolNot | Intrinsic::Int32Not | Intrinsic::Int64Not => {
                assert_eq!(arguments.len(), 1);
                let src_reg = arguments[0];
                self.emit_not(dest, src_reg);
            }

            Intrinsic::Int32Neg
            | Intrinsic::Int64Neg
            | Intrinsic::Float32Neg
            | Intrinsic::Float64Neg => {
                assert_eq!(arguments.len(), 1);
                let src_reg = arguments[0];
                self.emit_neg(dest, src_reg);
            }

            Intrinsic::Int32Shl | Intrinsic::Int64Shl => {
                assert_eq!(arguments.len(), 2);
                let lhs_reg = arguments[0];
                let rhs_reg = arguments[1];
                self.emit_shl(dest, lhs_reg, rhs_reg);
            }

            Intrinsic::Int32Shr | Intrinsic::Int64Shr => {
                assert_eq!(arguments.len(), 2);
                let lhs_reg = arguments[0];
                let rhs_reg = arguments[1];
                self.emit_shr(dest, lhs_reg, rhs_reg);
            }

            Intrinsic::Int32Sar | Intrinsic::Int64Sar => {
                assert_eq!(arguments.len(), 2);
                let lhs_reg = arguments[0];
                let rhs_reg = arguments[1];
                self.emit_sar(dest, lhs_reg, rhs_reg);
            }

            Intrinsic::ArrayNewOfSize
            | Intrinsic::ArrayWithValues
            | Intrinsic::ArrayLen
            | Intrinsic::ArrayGet
            | Intrinsic::ArraySet
            | Intrinsic::StrLen
            | Intrinsic::StrGet
            | Intrinsic::EnumEq
            | Intrinsic::EnumNe
            | Intrinsic::Float32IsNan
            | Intrinsic::Float64IsNan => {
                panic!("unimplemented intrinsic {:?}", intrinsic);
            }
        }
    }

    fn emit_intrinsic_count_bits(
        &mut self,
        dest: Register,
        fct_id: FunctionId,
        intrinsic: Intrinsic,
        arguments: Vec<Register>,
        type_params: BytecodeTypeArray,
        location: Location,
    ) {
        debug_assert_eq!(arguments.len(), 1);
        debug_assert!(type_params.is_empty());
        let reg = REG_RESULT;

        let mode = match intrinsic {
            Intrinsic::Int64CountZeroBits
            | Intrinsic::Int64CountOneBits
            | Intrinsic::Int64CountZeroBitsLeading
            | Intrinsic::Int64CountOneBitsLeading
            | Intrinsic::Int64CountZeroBitsTrailing
            | Intrinsic::Int64CountOneBitsTrailing => MachineMode::Int64,
            Intrinsic::Int32CountZeroBits
            | Intrinsic::Int32CountOneBits
            | Intrinsic::Int32CountZeroBitsLeading
            | Intrinsic::Int32CountOneBitsLeading
            | Intrinsic::Int32CountZeroBitsTrailing
            | Intrinsic::Int32CountOneBitsTrailing => MachineMode::Int32,
            _ => unreachable!(),
        };

        match intrinsic {
            Intrinsic::Int32CountZeroBits | Intrinsic::Int64CountZeroBits => {
                if has_popcnt() {
                    self.emit_load_register(arguments[0], reg.into());
                    self.asm.count_bits(mode, reg, reg, false);
                    self.emit_store_register(reg.into(), dest);
                } else {
                    self.emit_invoke_direct(dest, fct_id, type_params, arguments, location);
                }
            }
            Intrinsic::Int32CountOneBits | Intrinsic::Int64CountOneBits => {
                if has_popcnt() {
                    self.emit_load_register(arguments[0], reg.into());
                    self.asm.count_bits(mode, reg, reg, true);
                    self.emit_store_register(reg.into(), dest);
                } else {
                    self.emit_invoke_direct(dest, fct_id, type_params, arguments, location);
                }
            }
            Intrinsic::Int32CountZeroBitsLeading | Intrinsic::Int64CountZeroBitsLeading => {
                if has_lzcnt() {
                    self.emit_load_register(arguments[0], reg.into());
                    self.asm.count_bits_leading(mode, reg, reg, false);
                    self.emit_store_register(reg.into(), dest);
                } else {
                    self.emit_invoke_direct(dest, fct_id, type_params, arguments, location);
                }
            }
            Intrinsic::Int32CountOneBitsLeading | Intrinsic::Int64CountOneBitsLeading => {
                if has_lzcnt() {
                    self.emit_load_register(arguments[0], reg.into());
                    self.asm.count_bits_leading(mode, reg, reg, true);
                    self.emit_store_register(reg.into(), dest);
                } else {
                    self.emit_invoke_direct(dest, fct_id, type_params, arguments, location);
                }
            }

            Intrinsic::Int32CountZeroBitsTrailing | Intrinsic::Int64CountZeroBitsTrailing => {
                if has_tzcnt() {
                    self.emit_load_register(arguments[0], reg.into());
                    self.asm.count_bits_trailing(mode, reg, reg, false);
                    self.emit_store_register(reg.into(), dest);
                } else {
                    self.emit_invoke_direct(dest, fct_id, type_params, arguments, location);
                }
            }
            Intrinsic::Int32CountOneBitsTrailing | Intrinsic::Int64CountOneBitsTrailing => {
                if has_tzcnt() {
                    self.emit_load_register(arguments[0], reg.into());
                    self.asm.count_bits_trailing(mode, reg, reg, true);
                    self.emit_store_register(reg.into(), dest);
                } else {
                    self.emit_invoke_direct(dest, fct_id, type_params, arguments, location);
                }
            }
            _ => unreachable!(),
        }
    }

    fn emit_intrinsic_float_round_tozero(
        &mut self,
        dest: Register,
        _fct_id: FunctionId,
        intrinsic: Intrinsic,
        arguments: Vec<Register>,
        type_params: BytecodeTypeArray,
        _location: Location,
    ) {
        debug_assert_eq!(arguments.len(), 1);
        debug_assert!(type_params.is_empty());

        let mode = match intrinsic {
            Intrinsic::Float32RoundToZero => MachineMode::Float32,
            Intrinsic::Float64RoundToZero => MachineMode::Float64,
            _ => unreachable!(),
        };

        self.emit_load_register(arguments[0], FREG_RESULT.into());
        self.asm.float_round_tozero(mode, FREG_RESULT, FREG_RESULT);
        self.emit_store_register(FREG_RESULT.into(), dest);
    }

    fn emit_intrinsic_float_round_up(
        &mut self,
        dest: Register,
        _fct_id: FunctionId,
        intrinsic: Intrinsic,
        arguments: Vec<Register>,
        type_params: BytecodeTypeArray,
        _location: Location,
    ) {
        debug_assert_eq!(arguments.len(), 1);
        debug_assert!(type_params.is_empty());

        let mode = match intrinsic {
            Intrinsic::Float32RoundUp => MachineMode::Float32,
            Intrinsic::Float64RoundUp => MachineMode::Float64,
            _ => unreachable!(),
        };

        self.emit_load_register(arguments[0], FREG_RESULT.into());
        self.asm.float_round_up(mode, FREG_RESULT, FREG_RESULT);
        self.emit_store_register(FREG_RESULT.into(), dest);
    }

    fn emit_intrinsic_float_round_down(
        &mut self,
        dest: Register,
        _fct_id: FunctionId,
        intrinsic: Intrinsic,
        arguments: Vec<Register>,
        type_params: BytecodeTypeArray,
        _location: Location,
    ) {
        debug_assert_eq!(arguments.len(), 1);
        debug_assert!(type_params.is_empty());

        let mode = match intrinsic {
            Intrinsic::Float32RoundDown => MachineMode::Float32,
            Intrinsic::Float64RoundDown => MachineMode::Float64,
            _ => unreachable!(),
        };

        self.emit_load_register(arguments[0], FREG_RESULT.into());
        self.asm.float_round_down(mode, FREG_RESULT, FREG_RESULT);
        self.emit_store_register(FREG_RESULT.into(), dest);
    }

    fn emit_intrinsic_float_round_halfeven(
        &mut self,
        dest: Register,
        _fct_id: FunctionId,
        intrinsic: Intrinsic,
        arguments: Vec<Register>,
        type_params: BytecodeTypeArray,
        _location: Location,
    ) {
        debug_assert_eq!(arguments.len(), 1);
        debug_assert!(type_params.is_empty());

        let mode = match intrinsic {
            Intrinsic::Float32RoundHalfEven => MachineMode::Float32,
            Intrinsic::Float64RoundHalfEven => MachineMode::Float64,
            _ => unreachable!(),
        };

        self.emit_load_register(arguments[0], FREG_RESULT.into());
        self.asm
            .float_round_halfeven(mode, FREG_RESULT, FREG_RESULT);
        self.emit_store_register(FREG_RESULT.into(), dest);
    }

    fn emit_intrinsic_float_sqrt(
        &mut self,
        dest: Register,
        _fct_id: FunctionId,
        intrinsic: Intrinsic,
        arguments: Vec<Register>,
        type_params: BytecodeTypeArray,
        _location: Location,
    ) {
        debug_assert_eq!(arguments.len(), 1);
        debug_assert!(type_params.is_empty());

        let mode = match intrinsic {
            Intrinsic::Float32Sqrt => MachineMode::Float32,
            Intrinsic::Float64Sqrt => MachineMode::Float64,
            _ => unreachable!(),
        };

        self.emit_load_register(arguments[0], FREG_RESULT.into());
        self.asm.float_sqrt(mode, FREG_RESULT, FREG_RESULT);
        self.emit_store_register(FREG_RESULT.into(), dest);
    }

    fn emit_intrinsic_option_get_or_panic(
        &mut self,
        dest: Register,
        fct_id: FunctionId,
        _intrinsic: Intrinsic,
        arguments: Vec<Register>,
        type_params: BytecodeTypeArray,
        location: Location,
    ) {
        assert_eq!(1, arguments.len());
        assert_eq!(1, type_params.len());

        let (enum_id, type_params) = if let BytecodeType::Enum(enum_id, type_params) =
            self.specialize_register_type(arguments[0])
        {
            (enum_id, type_params)
        } else {
            unreachable!()
        };

        let enum_instance_id = create_enum_instance(self.vm, enum_id, type_params.clone());
        let enum_instance = self.vm.enum_instances.idx(enum_instance_id);

        match enum_instance.layout {
            EnumLayout::Int => unreachable!(),
            EnumLayout::Ptr => {
                self.emit_load_register_as(arguments[0], REG_RESULT.into(), MachineMode::Ptr);
                let lbl_slow_path = self.asm.test_if_nil(REG_RESULT);
                self.add_slow_path(
                    lbl_slow_path,
                    dest,
                    fct_id,
                    arguments,
                    type_params,
                    location,
                );
                self.emit_store_register_as(REG_RESULT.into(), dest, MachineMode::Ptr);
            }

            EnumLayout::Tagged => {
                self.emit_load_register_as(arguments[0], REG_TMP1.into(), MachineMode::Ptr);
                self.asm
                    .test_if_nil_bailout(location, REG_TMP1, Trap::ILLEGAL);

                let enum_ = &self.vm.program.enums[enum_id.0 as usize];
                let first_variant = enum_.variants.first().unwrap();

                let some_variant_id: u32 = if first_variant.arguments.is_empty() {
                    1
                } else {
                    0
                };

                self.asm.cmp_mem_imm(
                    MachineMode::Int32,
                    Mem::Base(REG_TMP1, Header::size()),
                    some_variant_id as i32,
                );
                let lbl_slow_path = self.asm.create_label();
                self.asm.jump_if(CondCode::NotEqual, lbl_slow_path);

                self.add_slow_path(
                    lbl_slow_path,
                    dest,
                    fct_id,
                    arguments,
                    type_params,
                    location,
                );

                let class_instance_id = ensure_class_instance_for_enum_variant(
                    self.vm,
                    &*enum_instance,
                    &*enum_,
                    some_variant_id,
                );

                let cls = self.vm.class_instances.idx(class_instance_id);

                let field = &cls.fields[1];
                let dest_offset = self.register_offset(dest);

                self.asm.copy_bytecode_ty(
                    field.ty.clone(),
                    RegOrOffset::Offset(dest_offset),
                    RegOrOffset::RegWithOffset(REG_TMP1, field.offset),
                );
            }
        }
    }

    fn add_slow_path(
        &mut self,
        lbl: Label,
        dest: Register,
        fct_id: FunctionId,
        arguments: Vec<Register>,
        type_params: BytecodeTypeArray,
        location: Location,
    ) {
        self.slow_paths
            .push((lbl, dest, fct_id, arguments, type_params, location));
    }

    fn emit_intrinsic_unsafe_kill_refs(
        &mut self,
        dest: Register,
        _fct_id: FunctionId,
        _intrinsic: Intrinsic,
        arguments: Vec<Register>,
        type_params: BytecodeTypeArray,
        _location: Location,
    ) {
        assert_eq!(1, type_params.len());
        assert_eq!(2, arguments.len());
        assert!(self.bytecode.register_type(dest).is_unit());

        let ty = type_params[0].clone();

        if ty.is_unit() {
            return;
        }

        let bytecode_type: BytecodeType = register_bty(ty.clone());

        self.emit_load_register(arguments[0], REG_RESULT.into());
        self.emit_load_register(arguments[1], REG_TMP1.into());

        self.asm
            .array_address(REG_TMP1, REG_RESULT, REG_TMP1, size(self.vm, bytecode_type));

        self.asm.zero_ty(ty, RegOrOffset::Reg(REG_TMP1));
    }

    fn emit_intrinsic_option_is_none(
        &mut self,
        dest: Register,
        _fct_id: FunctionId,
        intrinsic: Intrinsic,
        arguments: Vec<Register>,
        type_params: BytecodeTypeArray,
        location: Location,
    ) {
        assert_eq!(1, type_params.len());
        assert_eq!(1, arguments.len());
        let (enum_id, type_params) = if let BytecodeType::Enum(enum_id, type_params) =
            self.specialize_register_type(arguments[0])
        {
            (enum_id, type_params)
        } else {
            unreachable!()
        };

        let enum_instance_id = create_enum_instance(self.vm, enum_id, type_params.clone());
        let enum_instance = self.vm.enum_instances.idx(enum_instance_id);

        match enum_instance.layout {
            EnumLayout::Int => unreachable!(),
            EnumLayout::Ptr => {
                self.emit_load_register_as(arguments[0], REG_TMP1.into(), MachineMode::Ptr);
                self.asm.cmp_reg_imm(MachineMode::Ptr, REG_TMP1, 0);
                let cond = match intrinsic {
                    Intrinsic::OptionIsSome => CondCode::NotEqual,
                    Intrinsic::OptionIsNone => CondCode::Equal,
                    _ => unreachable!(),
                };
                self.asm.set(REG_RESULT, cond);
                self.emit_store_register_as(REG_RESULT.into(), dest, MachineMode::Int8);
            }

            EnumLayout::Tagged => {
                self.emit_load_register_as(arguments[0], REG_TMP1.into(), MachineMode::Ptr);
                self.asm
                    .test_if_nil_bailout(location, REG_TMP1, Trap::ILLEGAL);

                let enum_ = &self.vm.program.enums[enum_id.0 as usize];
                let first_variant = enum_.variants.first().unwrap();

                let none_variant_id = if first_variant.arguments.is_empty() {
                    0
                } else {
                    1
                };

                self.asm.cmp_mem_imm(
                    MachineMode::Int32,
                    Mem::Base(REG_TMP1, Header::size()),
                    none_variant_id,
                );
                let cond = match intrinsic {
                    Intrinsic::OptionIsSome => CondCode::NotEqual,
                    Intrinsic::OptionIsNone => CondCode::Equal,
                    _ => unreachable!(),
                };
                self.asm.set(REG_RESULT, cond);

                self.emit_store_register_as(REG_RESULT.into(), dest, MachineMode::Int8);
            }
        }
    }

    fn emit_invoke_arguments(
        &mut self,
        dest: Register,
        fct_return_type: BytecodeType,
        arguments: Vec<Register>,
    ) -> i32 {
        let argsize = self.determine_argsize(&arguments);

        self.asm.increase_stack_frame(argsize);

        let mut reg_idx = 0;
        let mut freg_idx = 0;
        let mut sp_offset = 0;

        if result_passed_as_argument(fct_return_type) {
            let offset = self.register_offset(dest);
            self.asm.lea(REG_PARAMS[0], Mem::Local(offset));
            reg_idx += 1;
        }

        for src in arguments {
            let bytecode_type = self.specialize_register_type(src);
            let offset = self.register_offset(src);

            match bytecode_type {
                BytecodeType::Unit => {}

                BytecodeType::Tuple(_) | BytecodeType::Struct(_, _) => {
                    if reg_idx < REG_PARAMS.len() {
                        let reg = REG_PARAMS[reg_idx];
                        self.asm.lea(reg, Mem::Local(offset));
                        reg_idx += 1;
                    } else {
                        self.asm.lea(REG_TMP1, Mem::Local(offset));
                        self.asm.store_mem(
                            MachineMode::Ptr,
                            Mem::Base(REG_SP, sp_offset),
                            REG_TMP1.into(),
                        );

                        sp_offset += 8;
                    }
                }
                BytecodeType::Float32 | BytecodeType::Float64 => {
                    let mode = mode(self.vm, bytecode_type);

                    if freg_idx < FREG_PARAMS.len() {
                        self.asm
                            .load_mem(mode, FREG_PARAMS[freg_idx].into(), Mem::Local(offset));
                        freg_idx += 1;
                    } else {
                        self.asm
                            .load_mem(mode, FREG_TMP1.into(), Mem::Local(offset));
                        self.asm
                            .store_mem(mode, Mem::Base(REG_SP, sp_offset), FREG_TMP1.into());

                        sp_offset += 8;
                    }
                }

                BytecodeType::Bool
                | BytecodeType::UInt8
                | BytecodeType::Char
                | BytecodeType::Int32
                | BytecodeType::Int64
                | BytecodeType::Ptr
                | BytecodeType::Enum(_, _)
                | BytecodeType::Trait(_, _) => {
                    let mode = mode(self.vm, bytecode_type);

                    if reg_idx < REG_PARAMS.len() {
                        self.asm
                            .load_mem(mode, REG_PARAMS[reg_idx].into(), Mem::Local(offset));
                        reg_idx += 1;
                    } else {
                        self.asm.load_mem(mode, REG_TMP1.into(), Mem::Local(offset));
                        self.asm
                            .store_mem(mode, Mem::Base(REG_SP, sp_offset), REG_TMP1.into());
                        sp_offset += 8;
                    }
                }

                BytecodeType::TypeAlias(..)
                | BytecodeType::TypeParam(_)
                | BytecodeType::Class(_, _)
                | BytecodeType::Lambda(_, _)
                | BytecodeType::This => {
                    unreachable!()
                }
            }
        }

        argsize
    }

    fn determine_argsize(&mut self, arguments: &Vec<Register>) -> i32 {
        let mut reg_idx = 0;
        let mut freg_idx = 0;
        let mut argsize = 0;

        for &src in arguments {
            let bytecode_type = self.bytecode.register_type(src);

            match bytecode_type {
                BytecodeType::Float32 | BytecodeType::Float64 => {
                    if freg_idx >= FREG_PARAMS.len() {
                        argsize += mem::ptr_width();
                    }

                    freg_idx += 1;
                }
                _ => {
                    if reg_idx >= REG_PARAMS.len() {
                        argsize += mem::ptr_width();
                    }

                    reg_idx += 1;
                }
            }
        }

        mem::align_i32(argsize, STACK_FRAME_ALIGNMENT as i32)
    }

    fn specialize_bty(&self, ty: BytecodeType) -> BytecodeType {
        specialize_bty(ty, &self.type_params)
    }

    fn specialize_bty_array(&self, types: &BytecodeTypeArray) -> BytecodeTypeArray {
        specialize_bty_array(types, &self.type_params)
    }

    fn register_offset(&self, reg: Register) -> i32 {
        let Register(idx) = reg;
        self.offsets[idx].expect("offset missing")
    }

    fn reg(&self, reg: Register) -> RegOrOffset {
        RegOrOffset::Offset(self.register_offset(reg))
    }

    fn specialize_register_type(&self, reg: Register) -> BytecodeType {
        let ty = self.bytecode.register_type(reg);
        register_bty(self.specialize_bty(ty))
    }
}

impl<'a> BytecodeVisitor for CannonCodeGen<'a> {
    fn visit_instruction(&mut self, offset: BytecodeOffset) {
        self.offset_to_address.insert(offset, self.asm.pos());
        self.current_offset = offset;

        if let Some(&label) = self.offset_to_label.get(&offset) {
            self.asm.bind_label(label);
        }

        // Ensure that PushRegister instructions are only followed by InvokeXXX,
        // NewTuple, NewEnum or NewStruct.
        if !self.argument_stack.is_empty() {
            let opcode = self.bytecode.read_opcode(offset);
            assert!(
                opcode.is_any_invoke()
                    || opcode.is_push_register()
                    || opcode.is_new_tuple()
                    || opcode.is_new_enum()
                    || opcode.is_new_struct()
                    || opcode.is_new_object_initialized()
                    || opcode.is_new_lambda()
            );
        }
    }

    fn visit_add(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("Add {}, {}, {}", dest, lhs, rhs));
        self.emit_add(dest, lhs, rhs);
    }

    fn visit_sub(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("Sub {}, {}, {}", dest, lhs, rhs));
        self.emit_sub(dest, lhs, rhs);
    }

    fn visit_neg(&mut self, dest: Register, src: Register) {
        comment!(self, format!("Neg {}, {}", dest, src));
        self.emit_neg(dest, src);
    }

    fn visit_mul(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("Mul {}, {}, {}", dest, lhs, rhs));
        self.emit_mul(dest, lhs, rhs);
    }

    fn visit_div(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("Div {}, {}, {}", dest, lhs, rhs));
        self.emit_div(dest, lhs, rhs);
    }

    fn visit_mod(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("Mod {}, {}, {}", dest, lhs, rhs));
        self.emit_mod(dest, lhs, rhs);
    }

    fn visit_and(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("And {}, {}, {}", dest, lhs, rhs));
        self.emit_and(dest, lhs, rhs);
    }

    fn visit_or(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("Or {}, {}, {}", dest, lhs, rhs));
        self.emit_or(dest, lhs, rhs)
    }

    fn visit_xor(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("Xor {}, {}, {}", dest, lhs, rhs));
        self.emit_xor(dest, lhs, rhs);
    }

    fn visit_not(&mut self, dest: Register, src: Register) {
        comment!(self, format!("Not {}, {}", dest, src));
        self.emit_not(dest, src);
    }

    fn visit_shl(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("Shl {}, {}, {}", dest, lhs, rhs));
        self.emit_shl(dest, lhs, rhs);
    }
    fn visit_shr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("Shr {}, {}, {}", dest, lhs, rhs));
        self.emit_shr(dest, lhs, rhs);
    }
    fn visit_sar(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("Sar {}, {}, {}", dest, lhs, rhs));
        self.emit_sar(dest, lhs, rhs);
    }

    fn visit_mov(&mut self, dest: Register, src: Register) {
        comment!(self, format!("Mov {}, {}", dest, src));
        self.emit_mov_generic(dest, src);
    }

    fn visit_load_tuple_element(&mut self, dest: Register, src: Register, idx: ConstPoolIdx) {
        comment!(self, {
            let (tuple_ty, subtype_idx) = match self.bytecode.const_pool(idx) {
                ConstPoolEntry::TupleElement(tuple_ty, subtype_idx) => (tuple_ty, *subtype_idx),
                _ => unreachable!(),
            };

            let tuple_name = display_ty(self.vm, tuple_ty);
            format!(
                "LoadTupleElement {}, {}, ConstPoolIdx({}) # {}.{}",
                dest, src, idx.0, tuple_name, subtype_idx,
            )
        });
        self.emit_load_tuple_element(dest, src, idx);
    }

    fn visit_load_enum_element(&mut self, dest: Register, src: Register, idx: ConstPoolIdx) {
        comment!(self, {
            let (enum_id, type_params, variant_idx, element_idx) =
                match self.bytecode.const_pool(idx) {
                    ConstPoolEntry::EnumElement(enum_id, type_params, variant_idx, element_idx) => {
                        (*enum_id, type_params, *variant_idx, *element_idx)
                    }
                    _ => unreachable!(),
                };
            let enum_ = &self.vm.program.enums[enum_id.0 as usize];
            let enum_name = display_ty(self.vm, &BytecodeType::Enum(enum_id, type_params.clone()));
            let variant = &enum_.variants[variant_idx as usize];
            format!(
                "LoadEnumElement {}, {}, ConstPoolIdx({}), {} # {}::{}.{}",
                dest, src, idx.0, element_idx, enum_name, variant.name, element_idx,
            )
        });
        self.emit_load_enum_element(dest, src, idx);
    }

    fn visit_load_enum_variant(&mut self, dest: Register, src: Register, idx: ConstPoolIdx) {
        comment!(self, {
            let (enum_id, type_params) = match self.bytecode.const_pool(idx) {
                ConstPoolEntry::Enum(enum_id, type_params) => (*enum_id, type_params),
                _ => unreachable!(),
            };
            let enum_name = display_ty(self.vm, &BytecodeType::Enum(enum_id, type_params.clone()));
            format!(
                "LoadEnumVariant {}, {}, ConstPoolIdx({}) # {}",
                dest, src, idx.0, enum_name,
            )
        });
        self.emit_load_enum_variant(dest, src, idx);
    }

    fn visit_load_struct_field(&mut self, dest: Register, obj: Register, field_idx: ConstPoolIdx) {
        comment!(self, {
            let (struct_id, type_params, field_id) = match self.bytecode.const_pool(field_idx) {
                ConstPoolEntry::StructField(struct_id, type_params, field_id) => {
                    (*struct_id, type_params, *field_id)
                }
                _ => unreachable!(),
            };
            let struct_ = &self.vm.program.structs[struct_id.0 as usize];
            let struct_name = display_ty(
                self.vm,
                &BytecodeType::Struct(struct_id, type_params.clone()),
            );

            let field = &struct_.fields[field_id as usize];

            format!(
                "LoadStructField {}, {}, ConstPoolIdx({}) # {}.{}",
                dest, obj, field_idx.0, struct_name, field.name
            )
        });
        self.emit_load_struct_field(dest, obj, field_idx);
    }

    fn visit_load_field(&mut self, dest: Register, obj: Register, field_idx: ConstPoolIdx) {
        comment!(self, {
            match self.bytecode.const_pool(field_idx) {
                ConstPoolEntry::Field(cls_id, type_params, field_id) => {
                    let cname =
                        display_ty(self.vm, &BytecodeType::Class(*cls_id, type_params.clone()));

                    let cls = &self.vm.program.classes[cls_id.0 as usize];
                    let field = &cls.fields[*field_id as usize];

                    format!(
                        "LoadField {}, {}, ConstPoolIdx({}) # {}.{}",
                        dest, obj, field_idx.0, cname, field.name
                    )
                }
                _ => unreachable!(),
            }
        });
        self.emit_load_field(dest, obj, field_idx);
    }

    fn visit_store_field(&mut self, src: Register, obj: Register, field_idx: ConstPoolIdx) {
        comment!(self, {
            let (cls_id, type_params, field_id) = match self.bytecode.const_pool(field_idx) {
                ConstPoolEntry::Field(cls_id, type_params, field_id) => {
                    (*cls_id, type_params, *field_id)
                }
                _ => unreachable!(),
            };
            let cname = display_ty(self.vm, &BytecodeType::Class(cls_id, type_params.clone()));

            let cls = &self.vm.program.classes[cls_id.0 as usize];
            let field = &cls.fields[field_id as usize];

            format!(
                "StoreField {}, {}, ConstPoolIdx({}) # {}.{}",
                src, obj, field_idx.0, cname, field.name
            )
        });
        self.emit_store_field(src, obj, field_idx);
    }

    fn visit_load_global(&mut self, dest: Register, glob_id: GlobalId) {
        comment!(self, {
            let global_var = &self.vm.program.globals[glob_id.0 as usize];
            format!(
                "LoadGlobal {}, GlobalId({}) # {}",
                dest, glob_id.0, global_var.name
            )
        });
        self.emit_load_global(dest, glob_id);
    }

    fn visit_store_global(&mut self, src: Register, global_id: GlobalId) {
        comment!(self, {
            let global_var = &self.vm.program.globals[global_id.0 as usize];
            format!(
                "StoreGlobal {}, GlobalId({}) # {}",
                src, global_id.0, global_var.name
            )
        });
        self.emit_store_global(src, global_id);
    }

    fn visit_push_register(&mut self, src: Register) {
        comment!(self, format!("PushRegister {}", src));
        self.argument_stack.push(src);
    }

    fn visit_const_true(&mut self, dest: Register) {
        comment!(self, format!("ConstTrue {}", dest));
        self.emit_const_bool(dest, true);
    }
    fn visit_const_false(&mut self, dest: Register) {
        comment!(self, format!("ConstFalse {}", dest));
        self.emit_const_bool(dest, false);
    }
    fn visit_const_zero_uint8(&mut self, dest: Register) {
        comment!(self, format!("ConstZeroUInt8 {}", dest));
        self.emit_const_int(dest, 0);
    }
    fn visit_const_zero_char(&mut self, dest: Register) {
        comment!(self, format!("ConstZeroChar {}", dest));
        self.emit_const_int(dest, 0);
    }
    fn visit_const_zero_int32(&mut self, dest: Register) {
        comment!(self, format!("ConstZeroInt32 {}", dest));
        self.emit_const_int(dest, 0);
    }
    fn visit_const_zero_int64(&mut self, dest: Register) {
        comment!(self, format!("ConstZeroInt64 {}", dest));
        self.emit_const_int(dest, 0);
    }
    fn visit_const_zero_float32(&mut self, dest: Register) {
        comment!(self, format!("ConstZeroFloat32 {}", dest));
        self.emit_const_float(dest, 0_f64);
    }
    fn visit_const_zero_float64(&mut self, dest: Register) {
        comment!(self, format!("ConstZeroFloat64 {}", dest));
        self.emit_const_float(dest, 0_f64);
    }
    fn visit_const_char(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bytecode
            .const_pool(idx)
            .to_char()
            .expect("unexpected const pool entry");
        comment!(self, format!("ConstChar {}, {} # {}", dest, idx.0, value));
        self.emit_const_int(dest, value as i64);
    }

    fn visit_const_uint8(&mut self, dest: Register, value: u8) {
        comment!(self, format!("ConstUInt8 {}, {}", dest, value));
        self.emit_const_int(dest, value as i64);
    }
    fn visit_const_int32(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bytecode
            .const_pool(idx)
            .to_int32()
            .expect("unexpected const pool entry");
        comment!(
            self,
            format!("ConstInt32 {}, ConstPoolId({}) # {}", dest, idx.0, value)
        );
        self.emit_const_int(dest, value as i64);
    }
    fn visit_const_int64(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bytecode
            .const_pool(idx)
            .to_int64()
            .expect("unexpected const pool entry");
        comment!(
            self,
            format!("ConstInt64 {}, ConstPoolId({}) # {}", dest, idx.0, value)
        );
        self.emit_const_int(dest, value);
    }
    fn visit_const_float32(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bytecode
            .const_pool(idx)
            .to_float32()
            .expect("unexpected const pool entry");
        comment!(
            self,
            format!("ConstFloat32 {}, ConstPoolId({}) # {}", dest, idx.0, value)
        );
        self.emit_const_float(dest, value as f64);
    }
    fn visit_const_float64(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bytecode
            .const_pool(idx)
            .to_float64()
            .expect("unexpected const pool entry");
        comment!(
            self,
            format!("ConstFloat64 {}, ConstPoolId({}) # {}", dest, idx.0, value)
        );
        self.emit_const_float(dest, value);
    }
    fn visit_const_string(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bytecode
            .const_pool(idx)
            .to_string()
            .expect("unexpected const pool entry");
        comment!(
            self,
            format!("ConstString {}, ConstPoolId({}) # {}", dest, idx.0, value)
        );
        self.emit_const_string(dest, value);
    }

    fn visit_test_identity(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestIdentity {}, {}, {}", dest, lhs, rhs));
        self.emit_test_identity(dest, lhs, rhs);
    }
    fn visit_test_eq(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestEq {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::Equal);
    }
    fn visit_test_ne(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestNe {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::NotEqual);
    }
    fn visit_test_gt(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestGt {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::Greater);
    }
    fn visit_test_ge(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestGe {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::GreaterEq);
    }
    fn visit_test_lt(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestLt {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::Less);
    }
    fn visit_test_le(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestLe {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::LessEq);
    }

    fn visit_jump_if_false(&mut self, opnd: Register, offset: u32) {
        let target = BytecodeOffset(self.current_offset.to_u32() + offset);
        comment!(
            self,
            format!(
                "JumpIfFalse {}, {} # target {}",
                opnd,
                offset,
                target.to_usize()
            )
        );
        self.emit_jump_if(opnd, target, false);
    }

    fn visit_jump_if_true(&mut self, opnd: Register, offset: u32) {
        let target = BytecodeOffset(self.current_offset.to_u32() + offset);
        comment!(
            self,
            format!(
                "JumpIfTrue {}, {} # target {}",
                opnd,
                offset,
                target.to_usize()
            )
        );
        self.emit_jump_if(opnd, target, true);
    }
    fn visit_jump_loop(&mut self, offset: u32) {
        let target = BytecodeOffset(self.current_offset.to_u32() - offset);
        comment!(
            self,
            format!("JumpLoop {} # target {}", offset, target.to_usize())
        );
        self.emit_jump_loop(target);
    }
    fn visit_jump(&mut self, offset: u32) {
        let target = BytecodeOffset(self.current_offset.to_u32() + offset);
        comment!(
            self,
            format!("Jump {} # target {}", offset, target.to_usize())
        );
        self.emit_jump(target);
    }
    fn visit_loop_start(&mut self) {
        comment!(self, format!("LoopStart"));
        let label = self.asm.create_and_bind_label();
        self.offset_to_label.insert(self.current_offset, label);
    }

    fn visit_invoke_direct(&mut self, dest: Register, idx: ConstPoolIdx) {
        comment!(self, format!("InvokeDirect {}, {}", dest, idx.0));
        self.emit_invoke_direct_from_bytecode(dest, idx);
    }

    fn visit_invoke_virtual(&mut self, dest: Register, idx: ConstPoolIdx) {
        comment!(self, format!("InvokeVirtual {}, {}", dest, idx.0));
        self.emit_invoke_virtual_from_bytecode(dest, idx);
    }

    fn visit_invoke_lambda(&mut self, dest: Register, idx: ConstPoolIdx) {
        comment!(self, format!("InvokeLambda {}", dest));
        self.emit_invoke_lambda_from_bytecode(dest, idx);
    }

    fn visit_invoke_static(&mut self, dest: Register, idx: ConstPoolIdx) {
        comment!(self, format!("InvokeStatic {}, {}", dest, idx.0));
        self.emit_invoke_static_from_bytecode(dest, idx);
    }

    fn visit_invoke_generic_direct(&mut self, dest: Register, idx: ConstPoolIdx) {
        comment!(self, format!("InvokeGenericDirect {}, {}", dest, idx.0));
        self.emit_invoke_generic(dest, idx, false);
    }

    fn visit_invoke_generic_static(&mut self, dest: Register, idx: ConstPoolIdx) {
        comment!(self, format!("InvokeGenericStatic {}, {}", dest, idx.0));
        self.emit_invoke_generic(dest, idx, true);
    }

    fn visit_new_object(&mut self, dest: Register, idx: ConstPoolIdx) {
        comment!(self, {
            let (cls_id, type_params) = match self.bytecode.const_pool(idx) {
                ConstPoolEntry::Class(cls_id, type_params) => (*cls_id, type_params),
                _ => unreachable!(),
            };
            let cname = display_ty(self.vm, &BytecodeType::Class(cls_id, type_params.clone()));
            format!("NewObject {}, ConstPoolIdx({}) # {}", dest, idx.0, cname)
        });
        self.emit_new_object(dest, idx)
    }

    fn visit_new_object_initialized(&mut self, dest: Register, idx: ConstPoolIdx) {
        comment!(self, {
            let (cls_id, type_params) = match self.bytecode.const_pool(idx) {
                ConstPoolEntry::Class(cls_id, type_params) => (*cls_id, type_params),
                _ => unreachable!(),
            };
            let cname = display_ty(self.vm, &BytecodeType::Class(cls_id, type_params.clone()));
            format!(
                "NewObjectInitialized {}, ConstPoolIdx({}) # {}",
                dest, idx.0, cname
            )
        });
        self.emit_new_object_initialized(dest, idx)
    }

    fn visit_new_array(&mut self, dest: Register, idx: ConstPoolIdx, length: Register) {
        comment!(self, {
            let (cls_id, type_params) = match self.bytecode.const_pool(idx) {
                ConstPoolEntry::Class(cls_id, type_params) => (*cls_id, type_params),
                _ => unreachable!(),
            };
            let cname = display_ty(self.vm, &BytecodeType::Class(cls_id, type_params.clone()));
            format!(
                "NewArray {}, ConstPoolIdx({}), {} # {}",
                dest, idx.0, length, cname
            )
        });
        self.emit_new_array(dest, idx, length);
    }

    fn visit_new_tuple(&mut self, dest: Register, idx: ConstPoolIdx) {
        comment!(self, {
            let subtypes = match self.bytecode.const_pool(idx) {
                ConstPoolEntry::Tuple(ref subtypes) => subtypes,
                _ => unreachable!(),
            };
            let tuple_name = display_ty(self.vm, &BytecodeType::Tuple(subtypes.clone()));
            format!(
                "NewTuple {}, ConstPoolIdx({}) # {}",
                dest, idx.0, tuple_name,
            )
        });
        self.emit_new_tuple(dest, idx);
    }

    fn visit_new_enum(&mut self, dest: Register, idx: ConstPoolIdx) {
        comment!(self, {
            let (enum_id, type_params, variant_idx) = match self.bytecode.const_pool(idx) {
                ConstPoolEntry::EnumVariant(enum_id, type_params, variant_idx) => {
                    (*enum_id, type_params, *variant_idx)
                }
                _ => unreachable!(),
            };
            let enum_ = &self.vm.program.enums[enum_id.0 as usize];
            let enum_name = display_ty(self.vm, &BytecodeType::Enum(enum_id, type_params.clone()));
            let variant = &enum_.variants[variant_idx as usize];
            format!(
                "NewEnum {}, ConstPoolIdx({}) # {}::{}",
                dest, idx.0, enum_name, variant.name,
            )
        });
        self.emit_new_enum(dest, idx);
    }

    fn visit_new_struct(&mut self, dest: Register, idx: ConstPoolIdx) {
        comment!(self, {
            let (struct_id, type_params) = match self.bytecode.const_pool(idx) {
                ConstPoolEntry::Struct(enum_id, type_params) => (*enum_id, type_params),
                _ => unreachable!(),
            };
            let struct_name = display_ty(
                self.vm,
                &BytecodeType::Struct(struct_id, type_params.clone()),
            );
            format!(
                "NewStruct {}, ConstPoolIdx({}) # {}",
                dest, idx.0, struct_name,
            )
        });
        self.emit_new_struct(dest, idx);
    }

    fn visit_new_trait_object(&mut self, dest: Register, idx: ConstPoolIdx, src: Register) {
        comment!(self, {
            let (trait_id, type_params, object_ty) = match self.bytecode.const_pool(idx) {
                ConstPoolEntry::Trait(trait_id, type_params, object_ty) => {
                    (*trait_id, type_params, object_ty)
                }
                _ => unreachable!(),
            };
            let trait_name =
                display_ty(self.vm, &BytecodeType::Trait(trait_id, type_params.clone()));
            let object_name = display_ty(self.vm, object_ty);
            format!(
                "NewTraitObject {}, ConstPoolIdx({}), {} # {} from object {}",
                dest, idx.0, src, trait_name, object_name,
            )
        });
        self.emit_new_trait_object(dest, idx, src);
    }

    fn visit_new_lambda(&mut self, dest: Register, idx: ConstPoolIdx) {
        comment!(self, {
            let (fct_id, _type_params) = match self.bytecode.const_pool(idx) {
                ConstPoolEntry::Fct(fct_id, type_params) => (*fct_id, type_params),
                _ => unreachable!(),
            };
            let fct_name = display_fct(self.vm, fct_id);
            format!("NewLambda {}, ConstPoolIdx({}) # {}", dest, idx.0, fct_name,)
        });
        self.emit_new_lambda(dest, idx);
    }

    fn visit_array_length(&mut self, dest: Register, arr: Register) {
        comment!(self, format!("ArrayLength {}, {}", dest, arr));
        self.emit_array_length(dest, arr);
    }

    fn visit_load_array(&mut self, dest: Register, arr: Register, idx: Register) {
        comment!(self, format!("LoadArray {}, {}, {}", dest, arr, idx));
        self.emit_load_array(dest, arr, idx);
    }

    fn visit_store_array(&mut self, src: Register, arr: Register, idx: Register) {
        comment!(self, format!("StoreArray {}, {}, {}", src, arr, idx));
        self.emit_store_array(src, arr, idx);
    }

    fn visit_load_trait_object_value(&mut self, dest: Register, object: Register) {
        comment!(self, format!("LoadTraitObjectValue {}, {}", dest, object));
        self.emit_load_trait_object_value(dest, object);
    }

    fn visit_ret(&mut self, opnd: Register) {
        comment!(self, format!("Ret {}", opnd));
        self.emit_return_generic(opnd);
    }
}

pub fn register_bty(ty: BytecodeType) -> BytecodeType {
    match ty {
        BytecodeType::Class(_, _) | BytecodeType::Lambda(_, _) => BytecodeType::Ptr,
        _ => ty,
    }
}

pub fn result_passed_as_argument(ty: BytecodeType) -> bool {
    match ty {
        BytecodeType::Unit
        | BytecodeType::Bool
        | BytecodeType::UInt8
        | BytecodeType::Char
        | BytecodeType::Int32
        | BytecodeType::Int64
        | BytecodeType::Float32
        | BytecodeType::Float64
        | BytecodeType::Enum(..)
        | BytecodeType::Class(..)
        | BytecodeType::Lambda(..)
        | BytecodeType::Ptr
        | BytecodeType::Trait(..) => false,
        BytecodeType::TypeAlias(..) | BytecodeType::TypeParam(..) | BytecodeType::This => {
            panic!("unexpected type param")
        }
        BytecodeType::Struct(..) | BytecodeType::Tuple(..) => true,
    }
}

fn result_reg(vm: &VM, bytecode_type: BytecodeType) -> AnyReg {
    if mode(vm, bytecode_type).is_float() {
        FREG_RESULT.into()
    } else {
        REG_RESULT.into()
    }
}

pub fn result_reg_mode(mode: MachineMode) -> AnyReg {
    if mode.is_float() {
        FREG_RESULT.into()
    } else {
        REG_RESULT.into()
    }
}

#[derive(Copy, Clone)]
pub enum RegOrOffset {
    Reg(Reg),
    RegWithOffset(Reg, i32),
    Offset(i32),
}

impl RegOrOffset {
    pub fn offset(self, offset: i32) -> RegOrOffset {
        match self {
            RegOrOffset::Reg(reg) => RegOrOffset::RegWithOffset(reg, offset),
            RegOrOffset::RegWithOffset(reg, cur_offset) => {
                RegOrOffset::RegWithOffset(reg, cur_offset + offset)
            }
            RegOrOffset::Offset(cur_offset) => RegOrOffset::Offset(cur_offset + offset),
        }
    }

    pub fn mem(self) -> Mem {
        match self {
            RegOrOffset::Reg(reg) => Mem::Base(reg, 0),
            RegOrOffset::RegWithOffset(reg, offset) => Mem::Base(reg, offset),
            RegOrOffset::Offset(offset) => Mem::Local(offset),
        }
    }
}

fn result_address_offset() -> i32 {
    -mem::ptr_width()
}

pub fn mode(vm: &VM, ty: BytecodeType) -> MachineMode {
    match ty {
        BytecodeType::Bool => MachineMode::Int8,
        BytecodeType::UInt8 => MachineMode::Int8,
        BytecodeType::Char => MachineMode::Int32,
        BytecodeType::Int32 => MachineMode::Int32,
        BytecodeType::Int64 => MachineMode::Int64,
        BytecodeType::Float32 => MachineMode::Float32,
        BytecodeType::Float64 => MachineMode::Float64,
        BytecodeType::Ptr
        | BytecodeType::Trait(_, _)
        | BytecodeType::Class(_, _)
        | BytecodeType::Lambda(_, _) => MachineMode::Ptr,
        BytecodeType::Enum(enum_id, type_params) => {
            let edef_id = create_enum_instance(vm, enum_id, type_params);
            let edef = vm.enum_instances.idx(edef_id);

            match edef.layout {
                EnumLayout::Int => MachineMode::Int32,
                EnumLayout::Ptr | EnumLayout::Tagged => MachineMode::Ptr,
            }
        }
        BytecodeType::TypeAlias(..)
        | BytecodeType::Tuple(_)
        | BytecodeType::TypeParam(_)
        | BytecodeType::This
        | BytecodeType::Struct(_, _)
        | BytecodeType::Unit => {
            panic!("unexpected type {:?}", ty)
        }
    }
}

pub fn size(vm: &VM, ty: BytecodeType) -> i32 {
    match ty {
        BytecodeType::Unit => 0,
        BytecodeType::Bool => 1,
        BytecodeType::UInt8 => 1,
        BytecodeType::Char => 4,
        BytecodeType::Int32 => 4,
        BytecodeType::Int64 => 8,
        BytecodeType::Float32 => 4,
        BytecodeType::Float64 => 8,
        BytecodeType::Ptr
        | BytecodeType::Trait(_, _)
        | BytecodeType::Class(_, _)
        | BytecodeType::Lambda(_, _) => mem::ptr_width(),
        BytecodeType::Tuple(_) => get_concrete_tuple_bty(vm, &ty).size(),
        BytecodeType::TypeAlias(..) | BytecodeType::TypeParam(_) | BytecodeType::This => {
            unreachable!()
        }
        BytecodeType::Enum(enum_id, type_params) => {
            let edef_id = create_enum_instance(vm, enum_id, type_params);
            let edef = vm.enum_instances.idx(edef_id);

            match edef.layout {
                EnumLayout::Int => 4,
                EnumLayout::Ptr | EnumLayout::Tagged => mem::ptr_width(),
            }
        }
        BytecodeType::Struct(struct_id, type_params) => {
            let sdef_id = create_struct_instance(vm, struct_id, type_params);
            let sdef = vm.struct_instances.idx(sdef_id);

            sdef.size
        }
    }
}

pub fn align(vm: &VM, ty: BytecodeType) -> i32 {
    match ty {
        BytecodeType::Unit => 0,
        BytecodeType::Bool => 1,
        BytecodeType::UInt8 => 1,
        BytecodeType::Char => 4,
        BytecodeType::Int32 => 4,
        BytecodeType::Int64 => 8,
        BytecodeType::Float32 => 4,
        BytecodeType::Float64 => 8,
        BytecodeType::Ptr
        | BytecodeType::Trait(_, _)
        | BytecodeType::Class(_, _)
        | BytecodeType::Lambda(_, _) => mem::ptr_width(),
        BytecodeType::Tuple(_) => get_concrete_tuple_bty(vm, &ty).align(),
        BytecodeType::TypeAlias(..) | BytecodeType::TypeParam(_) | BytecodeType::This => {
            unreachable!()
        }
        BytecodeType::Enum(enum_id, type_params) => {
            let edef_id = create_enum_instance(vm, enum_id, type_params);
            let edef = vm.enum_instances.idx(edef_id);

            match edef.layout {
                EnumLayout::Int => 4,
                EnumLayout::Ptr | EnumLayout::Tagged => mem::ptr_width(),
            }
        }
        BytecodeType::Struct(struct_id, type_params) => {
            let sdef_id = create_struct_instance(vm, struct_id, type_params);
            let sdef = vm.struct_instances.idx(sdef_id);

            sdef.align
        }
    }
}

pub fn register_ty(ty: BytecodeType) -> BytecodeType {
    match ty {
        BytecodeType::Class(_, _) | BytecodeType::Lambda(_, _) => BytecodeType::Ptr,
        _ => ty,
    }
}

pub fn get_function_address(vm: &VM, fid: FunctionId, type_params: BytecodeTypeArray) -> Address {
    let fct = &vm.program.functions[fid.0 as usize];

    if let Some(native_pointer) = vm.native_methods.get(fid) {
        assert!(type_params.is_empty());
        // Method is implemented in native code. Create trampoline for invoking it.
        let internal_fct = NativeFct {
            fctptr: native_pointer,
            args: BytecodeTypeArray::new(fct.params.clone()),
            return_type: fct.return_type.clone(),
            desc: NativeFctKind::RuntimeEntryTrampoline(fid),
        };

        ensure_runtime_entry_trampoline(vm, Some(fid), internal_fct)
    } else {
        // Method is implemented in Dora. Use the lazy compilation stub if the method
        // wasn't compiled yet.
        vm.compilation_database
            .is_compiled(vm, fid, type_params)
            .unwrap_or(vm.native_methods.lazy_compilation_stub())
    }
}
