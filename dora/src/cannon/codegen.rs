use dora_parser::ast::*;
use std::collections::hash_map::HashMap;

use crate::bytecode::{
    self, BytecodeFunction, BytecodeOffset, BytecodeType, BytecodeVisitor, ConstPoolEntry,
    ConstPoolIdx, Register,
};
use crate::compiler::asm::BaselineAssembler;
use crate::compiler::codegen::{
    ensure_native_stub, should_emit_asm, should_emit_debug, AllocationSize, AnyReg,
};
use crate::compiler::fct::{Code, GcPoint, JitDescriptor};
use crate::compiler::native_stub::{NativeFct, NativeFctDescriptor};
use crate::cpu::{
    Mem, Reg, FREG_PARAMS, FREG_RESULT, FREG_TMP1, REG_PARAMS, REG_RESULT, REG_SP, REG_TMP1,
    REG_TMP2, STACK_FRAME_ALIGNMENT,
};
use crate::gc::Address;
use crate::masm::*;
use crate::mem::{self, align_i32};
use crate::object::{offset_of_array_data, Header, Str};
use crate::semck::specialize::{
    specialize_class_id_params, specialize_enum_id_params, specialize_tuple, specialize_type,
    specialize_type_list,
};
use crate::size::InstanceSize;
use crate::stdlib;
use crate::ty::{BuiltinType, MachineMode, TypeList};
use crate::vm::{
    EnumLayout, Fct, FctId, FctKind, FctSrc, GlobalId, Intrinsic, TraitId, Trap, TupleId, VM,
};
use crate::vtable::{VTable, DISPLAY_SIZE};

macro_rules! comment {
    (
        $cannon:expr,
        $out:expr
    ) => {{
        if $cannon.should_emit_asm {
            $cannon.asm.emit_comment($out);
        }
    }};
}

struct ForwardJump {
    label: Label,
    offset: BytecodeOffset,
}

pub struct CannonCodeGen<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    fct: &'a Fct<'ast>,
    ast: &'ast Function,
    asm: BaselineAssembler<'a, 'ast>,
    src: &'a FctSrc,
    bytecode: &'a BytecodeFunction,
    temporary_stack: Vec<BytecodeType>,
    temporary_stack_size: i32,

    should_emit_asm: bool,

    lbl_break: Option<Label>,
    lbl_continue: Option<Label>,

    type_params: &'a TypeList,

    offset_to_address: HashMap<BytecodeOffset, usize>,

    forward_jumps: Vec<ForwardJump>,
    current_offset: BytecodeOffset,
    argument_stack: Vec<Register>,

    references: Vec<i32>,

    offsets: Vec<Option<i32>>,
    stacksize: i32,
    register_start_offset: i32,
}

impl<'a, 'ast> CannonCodeGen<'a, 'ast>
where
    'ast: 'a,
{
    pub fn new(
        vm: &'a VM<'ast>,
        fct: &'a Fct<'ast>,
        src: &'a FctSrc,
        bytecode: &'a BytecodeFunction,
        type_params: &'a TypeList,
    ) -> CannonCodeGen<'a, 'ast> {
        CannonCodeGen {
            vm,
            fct,
            ast: fct.ast,
            asm: BaselineAssembler::new(vm),
            src,
            bytecode,
            temporary_stack: Vec::new(),
            temporary_stack_size: 0,
            should_emit_asm: should_emit_asm(vm, fct),
            lbl_break: None,
            lbl_continue: None,
            type_params,
            offset_to_address: HashMap::new(),
            forward_jumps: Vec::new(),
            current_offset: BytecodeOffset(0),
            argument_stack: Vec::new(),
            references: Vec::new(),
            offsets: Vec::new(),
            stacksize: 0,
            register_start_offset: 0,
        }
    }

    pub fn generate(mut self) -> Code {
        if should_emit_debug(self.vm, self.fct) {
            self.asm.debug();
        }

        self.calculate_offsets();
        self.initialize_references();

        self.emit_prolog();
        self.clear_registers();
        self.store_params_on_stack();
        self.emit_stack_guard();

        bytecode::read(self.bytecode.code(), &mut self);

        self.resolve_forward_jumps();

        let jit_fct = self
            .asm
            .jit(self.stacksize, JitDescriptor::DoraFct(self.fct.id));

        jit_fct
    }

    fn calculate_offsets(&mut self) {
        self.register_start_offset = if self.has_result_address() {
            mem::ptr_width()
        } else {
            0
        };

        let (offsets, stacksize) = self.determine_offsets(self.register_start_offset);
        self.offsets = offsets;
        self.stacksize = stacksize;
    }

    fn determine_offsets(&self, start: i32) -> (Vec<Option<i32>>, i32) {
        let len = self.bytecode.registers().len();
        let mut offset: Vec<Option<i32>> = vec![None; len];
        let mut stacksize: i32 = start;

        for (index, ty) in self.bytecode.registers().iter().enumerate() {
            if let Some(ty) = self.specialize_bytecode_type_unit(ty.clone()) {
                let sz = ty.size(self.vm);
                stacksize = align_i32(stacksize + sz, sz);
                offset[index] = Some(-stacksize);
            }
        }

        stacksize = align_i32(stacksize, STACK_FRAME_ALIGNMENT as i32);

        (offset, stacksize)
    }

    fn clear_registers(&mut self) {
        let start = self.register_start_offset + mem::ptr_width();
        let end = self.stacksize + mem::ptr_width();
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

    fn initialize_references(&mut self) {
        assert!(self.references.is_empty());
        for (idx, ty) in self.bytecode.registers().iter().enumerate() {
            if let Some(ty) = self.specialize_bytecode_type_unit(ty.clone()) {
                assert!(!ty.is_type_param());
                if ty.is_ptr() {
                    let offset = self.register_offset(Register(idx));
                    self.references.push(offset);
                } else if let Some(tuple_id) = ty.tuple_id() {
                    let offset = self.register_offset(Register(idx));
                    let tuples = self.vm.tuples.lock();
                    for &ref_offset in tuples.get_tuple(tuple_id).references() {
                        self.references.push(offset + ref_offset);
                    }
                }
            }
        }
    }

    fn create_gcpoint(&self) -> GcPoint {
        GcPoint::from_offsets(self.references.clone())
    }

    fn has_result_address(&self) -> bool {
        let return_type = self.specialize_type(self.fct.return_type);
        return_type.is_tuple()
    }

    fn store_params_on_stack(&mut self) {
        let mut reg_idx = 0;
        let mut freg_idx = 0;
        let mut sp_offset = 16;

        if self.has_result_address() {
            self.asm.store_mem(
                MachineMode::Ptr,
                Mem::Local(result_address_offset()),
                REG_PARAMS[reg_idx].into(),
            );
            reg_idx += 1;
        }

        let params = self.fct.params_with_self();

        for (idx, &param_ty) in params.iter().enumerate() {
            let param_ty = self.specialize_type(param_ty);

            let dest = Register(idx);

            let param_ty = if idx == params.len() - 1 && self.fct.variadic_arguments {
                assert_eq!(self.bytecode.register_type(dest), BytecodeType::Ptr);
                BuiltinType::Ptr
            } else if param_ty.is_unit() {
                continue;
            } else {
                assert_eq!(
                    self.specialize_register_type(dest),
                    BytecodeType::from_ty(self.vm, param_ty)
                );
                param_ty
            };

            if let Some(tuple_id) = param_ty.tuple_id() {
                let dest_offset = self.register_offset(dest);

                if reg_idx < REG_PARAMS.len() {
                    self.asm.copy(
                        MachineMode::Ptr,
                        REG_TMP1.into(),
                        REG_PARAMS[reg_idx].into(),
                    );
                    self.copy_tuple(
                        tuple_id,
                        RegOrOffset::Offset(dest_offset),
                        RegOrOffset::Reg(REG_TMP1),
                    );
                } else {
                    self.asm
                        .load_mem(MachineMode::Ptr, REG_TMP1.into(), Mem::Local(sp_offset));
                    self.copy_tuple(
                        tuple_id,
                        RegOrOffset::Offset(dest_offset),
                        RegOrOffset::Reg(REG_TMP1),
                    );
                    sp_offset += 8;
                }

                continue;
            }

            let mode = param_ty.mode();

            let register = if mode.is_float() {
                if freg_idx < FREG_PARAMS.len() {
                    let freg = FREG_PARAMS[freg_idx].into();
                    freg_idx += 1;
                    Some(freg)
                } else {
                    None
                }
            } else {
                if reg_idx < REG_PARAMS.len() {
                    let reg = REG_PARAMS[reg_idx].into();
                    reg_idx += 1;
                    Some(reg)
                } else {
                    None
                }
            };

            match register {
                Some(src) => {
                    self.emit_store_register(src, dest);
                }
                None => {
                    let reg = if mode.is_float() {
                        FREG_RESULT.into()
                    } else {
                        REG_RESULT.into()
                    };
                    self.asm.load_mem(mode, reg, Mem::Local(sp_offset));
                    self.emit_store_register(reg, dest);
                    sp_offset += 8;
                }
            }
        }
    }

    fn emit_prolog(&mut self) {
        self.asm.prolog_size(self.stacksize);
    }

    fn emit_stack_guard(&mut self) {
        let gcpoint = self.create_gcpoint();
        self.asm.stack_guard(self.fct.ast.pos, gcpoint);
    }

    fn emit_epilog(&mut self) {
        self.asm.epilog();
    }

    fn emit_load_register(&mut self, src: Register, dest: AnyReg) {
        let bytecode_type = self.specialize_register_type(src);
        let offset = self.register_offset(src);
        self.asm
            .load_mem(bytecode_type.mode(self.vm), dest, Mem::Local(offset));
    }

    fn emit_load_register_as(&mut self, src: Register, dest: AnyReg, mode: MachineMode) {
        let offset = self.register_offset(src);
        self.asm.load_mem(mode, dest, Mem::Local(offset));
    }

    fn emit_store_register(&mut self, src: AnyReg, dest: Register) {
        let bytecode_type = self.specialize_register_type(dest);
        let offset = self.register_offset(dest);
        self.asm
            .store_mem(bytecode_type.mode(self.vm), Mem::Local(offset), src);
    }

    fn emit_store_register_as(&mut self, src: AnyReg, dest: Register, mode: MachineMode) {
        let offset = self.register_offset(dest);
        self.asm.store_mem(mode, Mem::Local(offset), src);
    }

    fn emit_push(&mut self, _opnd: Register) {
        unimplemented!();
    }

    fn emit_pop(&mut self, _dest: Register) {
        unimplemented!();
    }

    fn emit_add_int_stack(&mut self) {
        unimplemented!();
    }

    fn emit_add_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(lhs, REG_RESULT.into());
        self.emit_load_register(rhs, REG_TMP1.into());

        let bytecode_type = self.bytecode.register_type(dest);
        self.asm.int_add(
            bytecode_type.mode(self.vm),
            REG_RESULT,
            REG_RESULT,
            REG_TMP1,
        );

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_add_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(lhs, FREG_RESULT.into());
        self.emit_load_register(rhs, FREG_TMP1.into());

        let bytecode_type = self.bytecode.register_type(dest);
        self.asm.float_add(
            bytecode_type.mode(self.vm),
            FREG_RESULT,
            FREG_RESULT,
            FREG_TMP1,
        );

        self.emit_store_register(FREG_RESULT.into(), dest);
    }

    fn emit_sub_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(lhs, REG_RESULT.into());

        self.emit_load_register(rhs, REG_TMP1.into());

        let bytecode_type = self.bytecode.register_type(dest);
        self.asm.int_sub(
            bytecode_type.mode(self.vm),
            REG_RESULT,
            REG_RESULT,
            REG_TMP1,
        );

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_sub_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(lhs, FREG_RESULT.into());
        self.emit_load_register(rhs, FREG_TMP1.into());

        let bytecode_type = self.bytecode.register_type(dest);
        self.asm.float_sub(
            bytecode_type.mode(self.vm),
            FREG_RESULT,
            FREG_RESULT,
            FREG_TMP1,
        );

        self.emit_store_register(FREG_RESULT.into(), dest);
    }

    fn emit_neg_int(&mut self, dest: Register, src: Register) {
        assert_eq!(
            self.bytecode.register_type(src),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(src, REG_RESULT.into());

        let bytecode_type = self.bytecode.register_type(dest);
        self.asm
            .int_neg(bytecode_type.mode(self.vm), REG_RESULT, REG_RESULT);

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_neg_float(&mut self, dest: Register, src: Register) {
        assert_eq!(
            self.bytecode.register_type(src),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(src, FREG_RESULT.into());

        let bytecode_type = self.bytecode.register_type(dest);
        self.asm
            .float_neg(bytecode_type.mode(self.vm), FREG_RESULT, FREG_RESULT);

        self.emit_store_register(FREG_RESULT.into(), dest);
    }

    fn emit_mul_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(lhs, REG_RESULT.into());

        self.emit_load_register(rhs, REG_TMP1.into());

        let bytecode_type = self.bytecode.register_type(dest);
        self.asm.int_mul(
            bytecode_type.mode(self.vm),
            REG_RESULT,
            REG_RESULT,
            REG_TMP1,
        );

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_mul_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(lhs, FREG_RESULT.into());
        self.emit_load_register(rhs, FREG_TMP1.into());

        let bytecode_type = self.bytecode.register_type(dest);
        self.asm.float_mul(
            bytecode_type.mode(self.vm),
            FREG_RESULT,
            FREG_RESULT,
            FREG_TMP1,
        );

        self.emit_store_register(FREG_RESULT.into(), dest);
    }

    fn emit_div_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(lhs, REG_RESULT.into());

        self.emit_load_register(rhs, REG_TMP1.into());

        let bytecode_type = self.bytecode.register_type(dest);

        let position = self.bytecode.offset_position(self.current_offset.to_u32());

        self.asm.int_div(
            bytecode_type.mode(self.vm),
            REG_RESULT,
            REG_RESULT,
            REG_TMP1,
            position,
        );

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_div_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(lhs, FREG_RESULT.into());
        self.emit_load_register(rhs, FREG_TMP1.into());

        let bytecode_type = self.bytecode.register_type(dest);
        self.asm.float_div(
            bytecode_type.mode(self.vm),
            FREG_RESULT,
            FREG_RESULT,
            FREG_TMP1,
        );

        self.emit_store_register(FREG_RESULT.into(), dest);
    }

    fn emit_mod_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(lhs, REG_RESULT.into());

        self.emit_load_register(rhs, REG_TMP1.into());

        let bytecode_type = self.bytecode.register_type(dest);

        let position = self.bytecode.offset_position(self.current_offset.to_u32());

        self.asm.int_mod(
            bytecode_type.mode(self.vm),
            REG_RESULT,
            REG_RESULT,
            REG_TMP1,
            position,
        );

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_and_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(lhs, REG_RESULT.into());

        self.emit_load_register(rhs, REG_TMP1.into());

        let bytecode_type = self.bytecode.register_type(dest);
        self.asm.int_and(
            bytecode_type.mode(self.vm),
            REG_RESULT,
            REG_RESULT,
            REG_TMP1,
        );

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_or_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(lhs, REG_RESULT.into());

        self.emit_load_register(rhs, REG_TMP1.into());

        let bytecode_type = self.bytecode.register_type(dest);
        self.asm.int_or(
            bytecode_type.mode(self.vm),
            REG_RESULT,
            REG_RESULT,
            REG_TMP1,
        );

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_xor_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(lhs, REG_RESULT.into());

        self.emit_load_register(rhs, REG_TMP1.into());

        let bytecode_type = self.bytecode.register_type(dest);
        self.asm.int_xor(
            bytecode_type.mode(self.vm),
            REG_RESULT,
            REG_RESULT,
            REG_TMP1,
        );

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_not_bool(&mut self, dest: Register, src: Register) {
        assert_eq!(
            self.bytecode.register_type(src),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(src, REG_RESULT.into());

        self.asm.bool_not(REG_RESULT, REG_RESULT);

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_not_int(&mut self, dest: Register, src: Register) {
        assert_eq!(
            self.bytecode.register_type(src),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(src, REG_RESULT.into());

        let bytecode_type = self.bytecode.register_type(dest);
        self.asm
            .int_not(bytecode_type.mode(self.vm), REG_RESULT, REG_RESULT);

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_shl_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(BytecodeType::Int32, self.bytecode.register_type(rhs));
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(lhs, REG_RESULT.into());

        self.emit_load_register(rhs, REG_TMP1.into());

        let bytecode_type = self.bytecode.register_type(dest);
        self.asm.int_shl(
            bytecode_type.mode(self.vm),
            REG_RESULT,
            REG_RESULT,
            REG_TMP1,
        );

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_shr_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(BytecodeType::Int32, self.bytecode.register_type(rhs));
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(lhs, REG_RESULT.into());

        self.emit_load_register(rhs, REG_TMP1.into());

        let bytecode_type = self.bytecode.register_type(dest);
        self.asm.int_shr(
            bytecode_type.mode(self.vm),
            REG_RESULT,
            REG_RESULT,
            REG_TMP1,
        );

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_sar_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(BytecodeType::Int32, self.bytecode.register_type(rhs));
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(lhs, REG_RESULT.into());

        self.emit_load_register(rhs, REG_TMP1.into());

        let bytecode_type = self.bytecode.register_type(dest);
        self.asm.int_sar(
            bytecode_type.mode(self.vm),
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
            bytecode_type.mode(self.vm),
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
            bytecode_type.mode(self.vm),
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

    fn emit_extend_byte(&mut self, dest: Register, src: Register, _mode: MachineMode) {
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

    fn emit_int_to_float(&mut self, dest: Register, src: Register) {
        let src_type = self.bytecode.register_type(src);
        let dest_type = self.bytecode.register_type(dest);

        let (dest_mode, src_mode) = match (dest_type, src_type) {
            (BytecodeType::Float32, BytecodeType::Int32) => {
                (MachineMode::Float32, MachineMode::Int32)
            }
            (BytecodeType::Float32, BytecodeType::Int64) => {
                (MachineMode::Float32, MachineMode::Int64)
            }
            (BytecodeType::Float64, BytecodeType::Int32) => {
                (MachineMode::Float64, MachineMode::Int32)
            }
            (BytecodeType::Float64, BytecodeType::Int64) => {
                (MachineMode::Float64, MachineMode::Int64)
            }
            _ => unreachable!(),
        };

        self.emit_load_register(src, REG_RESULT.into());

        self.asm
            .int_to_float(dest_mode, FREG_RESULT, src_mode, REG_RESULT);

        self.emit_store_register(FREG_RESULT.into(), dest);
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

    fn emit_instanceof(
        &mut self,
        dest: Register,
        src: Register,
        cls_idx: ConstPoolIdx,
        instanceof: bool,
    ) {
        let const_pool_entry = self.bytecode.const_pool(cls_idx);

        let (cls_id, type_params) = match const_pool_entry {
            ConstPoolEntry::Class(cls_id, type_params) => (*cls_id, type_params),
            _ => unreachable!(),
        };

        let class_def_id = specialize_class_id_params(self.vm, cls_id, type_params);
        let cls = self.vm.class_defs.idx(class_def_id);
        let cls = cls.read();

        let vtable: &VTable = cls.vtable.as_ref().unwrap();
        let position = if instanceof {
            None
        } else {
            Some(self.bytecode.offset_position(self.current_offset.to_u32()))
        };

        // object instanceof T

        // tmp1 = <vtable of object>
        self.emit_load_register(src, REG_TMP1.into());
        let lbl_nil = self.asm.test_if_nil(REG_TMP1);
        self.asm
            .load_mem(MachineMode::Ptr, REG_TMP1.into(), Mem::Base(REG_TMP1, 0));

        let disp = self.asm.add_addr(vtable as *const _ as *mut u8);
        let pos = self.asm.pos() as i32;

        // tmp2 = <vtable of T>
        self.asm.load_constpool(REG_TMP2, disp + pos);

        if vtable.subtype_depth >= DISPLAY_SIZE {
            // cmp [tmp1 + offset T.vtable.subtype_depth], tmp3
            self.asm.cmp_mem_imm(
                MachineMode::Int32,
                Mem::Base(REG_TMP1, VTable::offset_of_depth()),
                vtable.subtype_depth as i32,
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

            let overflow_offset = mem::ptr_width() * (vtable.subtype_depth - DISPLAY_SIZE) as i32;

            // cmp [tmp1 + 8*(vtable.subtype_depth - DISPLAY_SIZE) ], tmp2
            self.asm.cmp_mem(
                MachineMode::Ptr,
                Mem::Base(REG_TMP1, overflow_offset),
                REG_TMP2,
            );

            if instanceof {
                // dest = if zero then true else false
                self.asm.set(REG_RESULT, CondCode::Equal);

                self.emit_store_register(REG_RESULT.into(), dest);
            } else {
                // jump to lbl_false if cmp did not succeed
                self.asm.jump_if(CondCode::NonZero, lbl_false);
            }

            // jmp lbl_finished
            let lbl_finished = self.asm.create_label();
            self.asm.jump(lbl_finished);

            // lbl_false:
            self.asm.bind_label(lbl_false);

            if instanceof {
                // dest = false
                self.asm.load_false(REG_RESULT);

                self.emit_store_register(REG_RESULT.into(), dest);
            } else {
                // bailout
                self.asm.emit_bailout_inplace(Trap::CAST, position.unwrap());
            }

            // lbl_finished:
            self.asm.bind_label(lbl_finished);
        } else {
            let display_entry =
                VTable::offset_of_display() + vtable.subtype_depth as i32 * mem::ptr_width();

            // tmp1 = vtable of object
            // tmp2 = vtable of T
            // cmp [tmp1 + offset], tmp2
            self.asm.cmp_mem(
                MachineMode::Ptr,
                Mem::Base(REG_TMP1, display_entry),
                REG_TMP2,
            );

            if instanceof {
                self.asm.set(REG_RESULT, CondCode::Equal);

                self.emit_store_register(REG_RESULT.into(), dest);
            } else {
                let lbl_bailout = self.asm.create_label();
                self.asm.jump_if(CondCode::NotEqual, lbl_bailout);
                self.asm
                    .emit_bailout(lbl_bailout, Trap::CAST, position.unwrap());
            }
        }

        if instanceof {
            let lbl_end = self.asm.create_label();
            self.asm.jump(lbl_end);

            self.asm.bind_label(lbl_nil);

            // dest = false
            self.asm.load_false(REG_RESULT);

            self.emit_store_register(REG_RESULT.into(), dest);

            self.asm.bind_label(lbl_end);
        } else {
            self.asm.bind_label(lbl_nil);
        }
    }

    fn emit_mov_generic(&mut self, dest: Register, src: Register) {
        assert_eq!(
            self.bytecode.register_type(src),
            self.bytecode.register_type(dest)
        );

        if let Some(bytecode_type) = self.specialize_register_type_unit(src) {
            match bytecode_type {
                BytecodeType::Tuple(tuple_id) => {
                    self.emit_mov_tuple(dest, src, tuple_id);
                }

                BytecodeType::Enum(enum_id, type_params) => {
                    let enum_def_id = specialize_enum_id_params(self.vm, enum_id, type_params);
                    let edef = self.vm.enum_defs.idx(enum_def_id);
                    let edef = edef.read();

                    match edef.layout {
                        EnumLayout::Int => {
                            let reg = REG_RESULT.into();
                            let mode = MachineMode::Int32;
                            self.emit_load_register_as(src, reg, mode);
                            self.emit_store_register_as(reg, dest, mode);
                        }
                        EnumLayout::Ptr => {
                            let reg = REG_RESULT.into();
                            let mode = MachineMode::Ptr;
                            self.emit_load_register_as(src, reg, mode);
                            self.emit_store_register_as(reg, dest, mode);
                        }
                        EnumLayout::Tagged => unimplemented!(),
                    }
                }

                _ => {
                    let reg = result_reg(self.vm, bytecode_type);
                    self.emit_load_register(src, reg.into());
                    self.emit_store_register(reg.into(), dest);
                }
            }
        }
    }

    fn emit_mov_tuple(&mut self, dest: Register, src: Register, tuple_id: TupleId) {
        assert_eq!(
            self.bytecode.register_type(src),
            self.bytecode.register_type(dest)
        );

        let src_offset = self.register_offset(src);
        let dest_offset = self.register_offset(dest);

        self.copy_tuple(
            tuple_id,
            RegOrOffset::Offset(dest_offset),
            RegOrOffset::Offset(src_offset),
        );
    }

    fn emit_load_tuple_element(
        &mut self,
        dest: Register,
        src: Register,
        tuple_id: TupleId,
        idx: u32,
    ) {
        let tuple_id = specialize_tuple(self.vm, tuple_id, self.type_params);
        let (_ty, offset) = self
            .vm
            .tuples
            .lock()
            .get_ty_and_offset(tuple_id, idx as usize);
        let src_offset = self.register_offset(src);

        if let Some(dest_type) = self.specialize_register_type_unit(dest) {
            if let Some(dest_tuple_id) = dest_type.tuple_id() {
                let dest_offset = self.register_offset(dest);

                self.copy_tuple(
                    dest_tuple_id,
                    RegOrOffset::Offset(dest_offset),
                    RegOrOffset::Offset(src_offset + offset),
                );
            } else {
                let reg = result_reg(self.vm, dest_type.clone());
                self.asm.load_mem(
                    dest_type.mode(self.vm),
                    reg,
                    Mem::Local(src_offset + offset),
                );

                self.emit_store_register(reg.into(), dest);
            }
        }
    }

    fn copy_tuple(&mut self, tuple_id: TupleId, dest: RegOrOffset, src: RegOrOffset) {
        let subtypes = self.vm.tuples.lock().get(tuple_id);
        let offsets = self
            .vm
            .tuples
            .lock()
            .get_tuple(tuple_id)
            .offsets()
            .to_owned();

        for (&subtype, &subtype_offset) in subtypes.iter().zip(&offsets) {
            if let Some(tuple_id) = subtype.tuple_id() {
                let src = match src {
                    RegOrOffset::Reg(reg) => RegOrOffset::RegWithOffset(reg, subtype_offset),
                    RegOrOffset::RegWithOffset(reg, tuple_offset) => {
                        RegOrOffset::RegWithOffset(reg, tuple_offset + subtype_offset)
                    }
                    RegOrOffset::Offset(tuple_offset) => {
                        RegOrOffset::Offset(tuple_offset + subtype_offset)
                    }
                };
                let dest = match dest {
                    RegOrOffset::Reg(reg) => RegOrOffset::RegWithOffset(reg, subtype_offset),
                    RegOrOffset::RegWithOffset(reg, tuple_offset) => {
                        RegOrOffset::RegWithOffset(reg, tuple_offset + subtype_offset)
                    }
                    RegOrOffset::Offset(tuple_offset) => {
                        RegOrOffset::Offset(tuple_offset + subtype_offset)
                    }
                };
                self.copy_tuple(tuple_id, dest, src);
            } else if subtype.is_unit() {
                // nothing
            } else {
                let mode = subtype.mode();
                let tmp = result_reg_mode(mode);
                let src = match src {
                    RegOrOffset::Reg(reg) => Mem::Base(reg, subtype_offset),
                    RegOrOffset::RegWithOffset(reg, tuple_offset) => {
                        Mem::Base(reg, tuple_offset + subtype_offset)
                    }
                    RegOrOffset::Offset(tuple_offset) => Mem::Local(tuple_offset + subtype_offset),
                };
                self.asm.load_mem(mode, tmp, src);
                let dest = match dest {
                    RegOrOffset::Reg(reg) => Mem::Base(reg, subtype_offset),
                    RegOrOffset::RegWithOffset(reg, tuple_offset) => {
                        Mem::Base(reg, tuple_offset + subtype_offset)
                    }
                    RegOrOffset::Offset(tuple_offset) => Mem::Local(tuple_offset + subtype_offset),
                };
                self.asm.store_mem(mode, dest, tmp);
            }
        }
    }

    fn zero_tuple(&mut self, tuple_id: TupleId, dest: RegOrOffset) {
        let subtypes = self.vm.tuples.lock().get(tuple_id);
        let offsets = self
            .vm
            .tuples
            .lock()
            .get_tuple(tuple_id)
            .offsets()
            .to_owned();

        for (&subtype, &subtype_offset) in subtypes.iter().zip(&offsets) {
            if let Some(tuple_id) = subtype.tuple_id() {
                let dest = match dest {
                    RegOrOffset::Reg(reg) => RegOrOffset::RegWithOffset(reg, subtype_offset),
                    RegOrOffset::RegWithOffset(reg, tuple_offset) => {
                        RegOrOffset::RegWithOffset(reg, tuple_offset + subtype_offset)
                    }
                    RegOrOffset::Offset(tuple_offset) => {
                        RegOrOffset::Offset(tuple_offset + subtype_offset)
                    }
                };
                self.zero_tuple(tuple_id, dest);
            } else if subtype.is_unit() {
                // nothing
            } else {
                let mode = subtype.mode();
                let dest = match dest {
                    RegOrOffset::Reg(reg) => Mem::Base(reg, subtype_offset),
                    RegOrOffset::RegWithOffset(reg, tuple_offset) => {
                        Mem::Base(reg, tuple_offset + subtype_offset)
                    }
                    RegOrOffset::Offset(tuple_offset) => Mem::Local(tuple_offset + subtype_offset),
                };
                self.asm.store_zero(mode, dest);
            }
        }
    }

    fn zero_refs_tuple(&mut self, tuple_id: TupleId, dest: RegOrOffset) {
        let subtypes = self.vm.tuples.lock().get(tuple_id);
        let offsets = self
            .vm
            .tuples
            .lock()
            .get_tuple(tuple_id)
            .offsets()
            .to_owned();

        for (&subtype, &subtype_offset) in subtypes.iter().zip(&offsets) {
            if let Some(tuple_id) = subtype.tuple_id() {
                let dest = match dest {
                    RegOrOffset::Reg(reg) => RegOrOffset::RegWithOffset(reg, subtype_offset),
                    RegOrOffset::RegWithOffset(reg, tuple_offset) => {
                        RegOrOffset::RegWithOffset(reg, tuple_offset + subtype_offset)
                    }
                    RegOrOffset::Offset(tuple_offset) => {
                        RegOrOffset::Offset(tuple_offset + subtype_offset)
                    }
                };
                self.zero_refs_tuple(tuple_id, dest);
            } else if subtype.reference_type() {
                let dest = match dest {
                    RegOrOffset::Reg(reg) => Mem::Base(reg, subtype_offset),
                    RegOrOffset::RegWithOffset(reg, tuple_offset) => {
                        Mem::Base(reg, tuple_offset + subtype_offset)
                    }
                    RegOrOffset::Offset(tuple_offset) => Mem::Local(tuple_offset + subtype_offset),
                };
                self.asm.store_zero(MachineMode::Ptr, dest);
            }
        }
    }

    fn emit_load_field(&mut self, dest: Register, obj: Register, field_idx: ConstPoolIdx) {
        assert_eq!(self.bytecode.register_type(obj), BytecodeType::Ptr);

        let (cls_id, type_params, field_id) = match self.bytecode.const_pool(field_idx) {
            ConstPoolEntry::Field(cls_id, type_params, field_id) => {
                (*cls_id, type_params, *field_id)
            }
            _ => unreachable!(),
        };

        let type_params = specialize_type_list(self.vm, type_params, self.type_params);
        debug_assert!(type_params
            .iter()
            .all(|ty| !ty.contains_type_param(self.vm)));

        let class_def_id = specialize_class_id_params(self.vm, cls_id, &type_params);
        let cls = self.vm.class_defs.idx(class_def_id);
        let cls = cls.read();

        let field = &cls.fields[field_id.idx()];

        assert!(self.bytecode.register_type(obj).is_ptr());
        let obj_reg = REG_TMP1;
        self.emit_load_register(obj, obj_reg.into());

        let pos = self.bytecode.offset_position(self.current_offset.to_u32());
        self.asm.test_if_nil_bailout(pos, obj_reg, Trap::NIL);

        if let Some(bytecode_type) = self.specialize_register_type_unit(dest) {
            assert_eq!(bytecode_type, BytecodeType::from_ty(self.vm, field.ty));

            if let Some(tuple_id) = bytecode_type.tuple_id() {
                let dest_offset = self.register_offset(dest);
                self.copy_tuple(
                    tuple_id,
                    RegOrOffset::Offset(dest_offset),
                    RegOrOffset::RegWithOffset(obj_reg, field.offset),
                );
            } else {
                let dest_reg = result_reg(self.vm, bytecode_type);
                self.asm
                    .load_mem(field.ty.mode(), dest_reg, Mem::Base(obj_reg, field.offset));

                self.emit_store_register(dest_reg.into(), dest);
            }
        }
    }

    fn emit_store_field(&mut self, src: Register, obj: Register, field_idx: ConstPoolIdx) {
        assert_eq!(self.bytecode.register_type(obj), BytecodeType::Ptr);

        let (cls_id, type_params, field_id) = match self.bytecode.const_pool(field_idx) {
            ConstPoolEntry::Field(cls_id, type_params, field_id) => {
                (*cls_id, type_params, *field_id)
            }
            _ => unreachable!(),
        };

        let type_params = specialize_type_list(self.vm, type_params, self.type_params);
        debug_assert!(type_params
            .iter()
            .all(|ty| !ty.contains_type_param(self.vm)));

        let class_def_id = specialize_class_id_params(self.vm, cls_id, &type_params);
        let cls = self.vm.class_defs.idx(class_def_id);
        let cls = cls.read();

        let field = &cls.fields[field_id.idx()];

        assert!(self.bytecode.register_type(obj).is_ptr());
        let obj_reg = REG_TMP1;
        self.emit_load_register(obj, obj_reg.into());

        let pos = self.bytecode.offset_position(self.current_offset.to_u32());
        self.asm.test_if_nil_bailout(pos, obj_reg, Trap::NIL);

        if let Some(bytecode_type) = self.specialize_register_type_unit(src) {
            assert_eq!(bytecode_type, BytecodeType::from_ty(self.vm, field.ty));

            let needs_write_barrier = if let Some(tuple_id) = bytecode_type.tuple_id() {
                let src_offset = self.register_offset(src);
                self.copy_tuple(
                    tuple_id,
                    RegOrOffset::RegWithOffset(obj_reg, field.offset),
                    RegOrOffset::Offset(src_offset),
                );

                self.vm
                    .tuples
                    .lock()
                    .get_tuple(tuple_id)
                    .contains_references()
            } else {
                let value = result_reg(self.vm, bytecode_type);

                self.emit_load_register(src, value.into());
                self.asm
                    .store_mem(field.ty.mode(), Mem::Base(obj_reg, field.offset), value);

                field.ty.reference_type()
            };

            if self.vm.gc.needs_write_barrier() && needs_write_barrier {
                let card_table_offset = self.vm.gc.card_table_offset();
                self.asm.emit_barrier(obj_reg, card_table_offset);
            }
        }
    }

    fn emit_load_global(&mut self, dest: Register, global_id: GlobalId) {
        let glob = self.vm.globals.idx(global_id);
        let glob = glob.read();

        assert_eq!(
            self.bytecode.register_type(dest),
            BytecodeType::from_ty(self.vm, glob.ty)
        );

        if glob.needs_initialization() {
            let fid = glob.initializer.unwrap();
            let ptr = self.ptr_for_fct_id(fid, TypeList::empty());
            let gcpoint = self.create_gcpoint();
            self.asm.ensure_global(&*glob, fid, ptr, glob.pos, gcpoint);
        }

        let disp = self.asm.add_addr(glob.address_value.to_ptr());
        let pos = self.asm.pos() as i32;
        self.asm.load_constpool(REG_TMP1, disp + pos);

        let bytecode_type = self.bytecode.register_type(dest);

        if let Some(tuple_id) = bytecode_type.tuple_id() {
            let dest_offset = self.register_offset(dest);
            self.copy_tuple(
                tuple_id,
                RegOrOffset::Offset(dest_offset),
                RegOrOffset::Reg(REG_TMP1),
            );
        } else {
            let reg = result_reg(self.vm, bytecode_type);

            self.asm
                .load_mem(glob.ty.mode(), reg, Mem::Base(REG_TMP1, 0));

            self.emit_store_register(reg, dest);
        }
    }

    fn emit_store_global(&mut self, src: Register, global_id: GlobalId) {
        let glob = self.vm.globals.idx(global_id);
        let glob = glob.read();

        assert_eq!(
            self.bytecode.register_type(src),
            BytecodeType::from_ty(self.vm, glob.ty)
        );

        let disp = self.asm.add_addr(glob.address_value.to_ptr());
        let pos = self.asm.pos() as i32;

        self.asm.load_constpool(REG_TMP1, disp + pos);

        let bytecode_type = self.bytecode.register_type(src);

        if let Some(tuple_id) = bytecode_type.tuple_id() {
            let src_offset = self.register_offset(src);
            self.copy_tuple(
                tuple_id,
                RegOrOffset::Reg(REG_TMP1),
                RegOrOffset::Offset(src_offset),
            );
        } else {
            let reg = result_reg(self.vm, bytecode_type);

            self.emit_load_register(src, reg);

            self.asm
                .store_mem(glob.ty.mode(), Mem::Base(REG_TMP1, 0), reg);
        }

        if glob.needs_initialization() {
            let disp = self.asm.add_addr(glob.address_init.to_ptr());
            let pos = self.asm.pos() as i32;
            self.asm.load_constpool(REG_RESULT, disp + pos);
            self.asm.load_int_const(MachineMode::Int8, REG_TMP1, 1);
            self.asm
                .store_mem(MachineMode::Int8, Mem::Base(REG_RESULT, 0), REG_TMP1.into());
        }
    }

    fn emit_const_nil(&mut self, dest: Register) {
        assert!(self.specialize_register_type(dest).is_ptr());
        self.asm.load_nil(REG_RESULT);
        self.emit_store_register(REG_RESULT.into(), dest);
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
            .load_int_const(bytecode_type.mode(self.vm), REG_RESULT, int_const);

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_const_float(&mut self, dest: Register, float_const: f64) {
        let bytecode_type = self.specialize_register_type(dest);
        assert!(bytecode_type == BytecodeType::Float32 || bytecode_type == BytecodeType::Float64);

        self.asm
            .load_float_const(bytecode_type.mode(self.vm), FREG_RESULT, float_const);

        self.emit_store_register(FREG_RESULT.into(), dest);
    }

    fn emit_const_string(&mut self, dest: Register, lit_value: &str) {
        let bytecode_type = self.specialize_register_type(dest);
        assert_eq!(bytecode_type, BytecodeType::Ptr);

        let handle = Str::from_buffer_in_perm(self.vm, lit_value.as_bytes());
        let disp = self.asm.add_addr(handle.raw() as *const u8);
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

        self.emit_load_register(lhs, REG_RESULT.into());
        self.emit_load_register(rhs, REG_TMP1.into());

        let bytecode_type = self.bytecode.register_type(lhs);

        self.asm
            .cmp_reg(bytecode_type.mode(self.vm), REG_RESULT, REG_TMP1);
        self.asm.set(REG_RESULT, op);

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_test_identity(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Bool);

        let op = CondCode::Equal;

        if let Some(bytecode_type) = self.specialize_register_type_unit(lhs) {
            if let BytecodeType::Tuple(_tuple_id) = bytecode_type {
                unimplemented!()
            } else if bytecode_type.is_any_float() {
                let mode = match bytecode_type {
                    BytecodeType::Float32 => MachineMode::Int32,
                    BytecodeType::Float64 => MachineMode::Int64,
                    _ => unreachable!(),
                };

                let offset = self.register_offset(lhs);
                self.asm
                    .load_mem(mode, REG_RESULT.into(), Mem::Local(offset));

                let offset = self.register_offset(rhs);
                self.asm.load_mem(mode, REG_TMP1.into(), Mem::Local(offset));

                self.asm.cmp_reg(mode, REG_RESULT, REG_TMP1);
                self.asm.set(REG_RESULT, op);

                self.emit_store_register(REG_RESULT.into(), dest);
            } else {
                self.emit_load_register(lhs, REG_RESULT.into());
                self.emit_load_register(rhs, REG_TMP1.into());

                self.asm
                    .cmp_reg(bytecode_type.mode(self.vm), REG_RESULT, REG_TMP1);
                self.asm.set(REG_RESULT, op);

                self.emit_store_register(REG_RESULT.into(), dest);
            }
        } else {
            self.emit_const_bool(dest, true);
        }
    }

    fn emit_test_float(&mut self, dest: Register, lhs: Register, rhs: Register, op: CondCode) {
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(rhs)
        );
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Bool);

        self.emit_load_register(lhs, FREG_RESULT.into());
        self.emit_load_register(rhs, FREG_TMP1.into());

        let bytecode_type = self.bytecode.register_type(lhs);

        self.asm.float_cmp(
            bytecode_type.mode(self.vm),
            REG_RESULT,
            FREG_RESULT,
            FREG_TMP1,
            op,
        );

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_jump_if(&mut self, src: Register, offset: BytecodeOffset, op: bool) {
        assert_eq!(self.bytecode.register_type(src), BytecodeType::Bool);

        self.emit_load_register(src, REG_RESULT.into());

        let op = if op {
            CondCode::NonZero
        } else {
            CondCode::Zero
        };
        let lbl = self.asm.create_label();
        self.asm.test_and_jump_if(op, REG_RESULT, lbl);

        self.resolve_label(offset, lbl);
    }

    fn emit_jump(&mut self, offset: BytecodeOffset) {
        let lbl = self.asm.create_label();
        self.resolve_label(offset, lbl);
        self.asm.jump(lbl);
    }

    fn resolve_label(&mut self, target: BytecodeOffset, lbl: Label) {
        if target < self.current_offset {
            self.asm.bind_label_to(
                lbl,
                *self
                    .offset_to_address
                    .get(&target)
                    .expect("jump with wrong offset"),
            );
        } else {
            self.forward_jumps.push(ForwardJump {
                label: lbl,
                offset: target,
            });
        }
    }

    fn emit_return_generic(&mut self, src: Register) {
        if let Some(bytecode_type) = self.specialize_register_type_unit(src) {
            if let Some(tuple_id) = bytecode_type.tuple_id() {
                let src_offset = self.register_offset(src);

                self.asm.load_mem(
                    MachineMode::Ptr,
                    REG_TMP1.into(),
                    Mem::Local(result_address_offset()),
                );

                self.copy_tuple(
                    tuple_id,
                    RegOrOffset::Reg(REG_TMP1),
                    RegOrOffset::Offset(src_offset),
                );
            } else {
                let reg = result_reg(self.vm, bytecode_type);
                self.emit_load_register(src, reg.into());
            }
        }

        self.emit_epilog();
    }

    fn resolve_forward_jumps(&mut self) {
        for jump in &self.forward_jumps {
            let offset = *self
                .offset_to_address
                .get(&jump.offset)
                .expect("offset for bytecode not found");
            self.asm.bind_label_to(jump.label, offset);
        }
    }

    fn emit_new_object(&mut self, dest: Register, idx: ConstPoolIdx) {
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Ptr);

        let const_pool_entry = self.bytecode.const_pool(idx);

        let (cls_id, type_params) = match const_pool_entry {
            ConstPoolEntry::Class(cls_id, type_params) => (*cls_id, type_params),
            _ => unreachable!(),
        };

        let type_params = specialize_type_list(self.vm, type_params, self.type_params);
        debug_assert!(type_params
            .iter()
            .all(|ty| !ty.contains_type_param(self.vm)));

        let class_def_id = specialize_class_id_params(self.vm, cls_id, &type_params);

        let cls = self.vm.class_defs.idx(class_def_id);
        let cls = cls.read();

        let alloc_size = match cls.size {
            InstanceSize::Fixed(size) => AllocationSize::Fixed(size as usize),
            _ => unreachable!(
                "class size type {:?} for new object not supported",
                cls.size
            ),
        };

        let gcpoint = self.create_gcpoint();
        let position = self.bytecode.offset_position(self.current_offset.to_u32());
        self.asm
            .allocate(REG_RESULT.into(), alloc_size, position, false, gcpoint);

        // store gc object in temporary storage
        self.emit_store_register(REG_RESULT.into(), dest);

        // store classptr in object
        let cptr = (&**cls.vtable.as_ref().unwrap()) as *const VTable as *const u8;
        let disp = self.asm.add_addr(cptr);
        let pos = self.asm.pos() as i32;

        self.asm.load_constpool(REG_TMP1.into(), disp + pos);
        self.asm
            .store_mem(MachineMode::Ptr, Mem::Base(REG_RESULT, 0), REG_TMP1.into());

        // clear mark/fwdptr word in header
        assert!(Header::size() == 2 * mem::ptr_width());
        self.asm.load_int_const(MachineMode::Ptr, REG_TMP1, 0);
        self.asm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_RESULT, mem::ptr_width()),
            REG_TMP1.into(),
        );

        match cls.size {
            InstanceSize::Fixed(size) => {
                self.asm.fill_zero(REG_RESULT, false, size as usize);
            }
            _ => unreachable!(),
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

        let type_params = specialize_type_list(self.vm, &type_params, self.type_params);

        let class_def_id = specialize_class_id_params(self.vm, cls_id, &type_params);

        let cls = self.vm.class_defs.idx(class_def_id);
        let cls = cls.read();

        self.emit_load_register(length, REG_TMP1.into());

        let array_header_size = Header::size() as usize + mem::ptr_width_usize();

        let alloc_size = match cls.size {
            InstanceSize::PrimitiveArray(size) | InstanceSize::TupleArray(size) => {
                assert_ne!(size, 0);
                self.asm
                    .determine_array_size(REG_TMP1, REG_TMP1, size, true);
                AllocationSize::Dynamic(REG_TMP1)
            }
            InstanceSize::ObjArray => {
                self.asm
                    .determine_array_size(REG_TMP1, REG_TMP1, mem::ptr_width(), true);
                AllocationSize::Dynamic(REG_TMP1)
            }
            InstanceSize::UnitArray => AllocationSize::Fixed(array_header_size),
            _ => unreachable!("class size type {:?} for new array not supported", cls.size),
        };

        let array_ref = match cls.size {
            InstanceSize::ObjArray => true,
            _ => false,
        };

        let gcpoint = self.create_gcpoint();
        let position = self.bytecode.offset_position(self.current_offset.to_u32());
        self.asm
            .allocate(REG_RESULT.into(), alloc_size, position, array_ref, gcpoint);

        // store gc object in temporary storage
        self.emit_store_register(REG_RESULT.into(), dest);

        // store classptr in object
        let cptr = (&**cls.vtable.as_ref().unwrap()) as *const VTable as *const u8;
        let disp = self.asm.add_addr(cptr);
        let pos = self.asm.pos() as i32;

        self.asm.load_constpool(REG_TMP1.into(), disp + pos);
        self.asm
            .store_mem(MachineMode::Ptr, Mem::Base(REG_RESULT, 0), REG_TMP1.into());

        // clear mark/fwdptr word in header
        assert!(Header::size() == 2 * mem::ptr_width());
        self.asm.load_int_const(MachineMode::Ptr, REG_TMP1, 0);
        self.asm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_RESULT, mem::ptr_width()),
            REG_TMP1.into(),
        );

        // store length in object
        self.emit_load_register(length, REG_TMP1.into());
        self.asm.store_mem(
            MachineMode::Ptr,
            Mem::Base(REG_RESULT, Header::size()),
            REG_TMP1.into(),
        );

        match cls.size {
            InstanceSize::PrimitiveArray(size) | InstanceSize::TupleArray(size) => {
                self.emit_array_initialization(REG_RESULT, REG_TMP1, size);
            }
            InstanceSize::ObjArray => {
                self.emit_array_initialization(REG_RESULT, REG_TMP1, mem::ptr_width());
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

    fn emit_new_tuple(&mut self, dest: Register, tuple_id: TupleId) {
        let tuple_id = specialize_tuple(self.vm, tuple_id, self.type_params);
        let subtypes = self.vm.tuples.lock().get(tuple_id);
        let offsets = self
            .vm
            .tuples
            .lock()
            .get_tuple(tuple_id)
            .offsets()
            .to_owned();
        let dest_offset = self.register_offset(dest);
        let mut arg_idx = 0;
        let arguments = std::mem::replace(&mut self.argument_stack, Vec::new());

        for (&subtype, &subtype_offset) in subtypes.iter().zip(&offsets) {
            if let Some(tuple_id) = subtype.tuple_id() {
                let src = arguments[arg_idx];
                let src_offset = self.register_offset(src);

                self.copy_tuple(
                    tuple_id,
                    RegOrOffset::Offset(dest_offset + subtype_offset),
                    RegOrOffset::Offset(src_offset),
                );
            } else if subtype.is_unit() {
                // nothing
            } else {
                let subtype_reg = arguments[arg_idx];
                let dest_type = self.specialize_register_type(subtype_reg);
                let tmp = result_reg(self.vm, dest_type.clone());

                self.emit_load_register(arguments[arg_idx], tmp);

                self.asm.store_mem(
                    dest_type.mode(self.vm),
                    Mem::Local(dest_offset + subtype_offset),
                    tmp,
                );

                arg_idx += 1;
            }
        }
    }

    fn emit_new_enum(&mut self, dest: Register, idx: ConstPoolIdx) {
        let (enum_id, type_params, variant_id) = match self.bytecode.const_pool(idx) {
            ConstPoolEntry::EnumVariant(enum_id, type_params, variant_id) => {
                (*enum_id, type_params.clone(), *variant_id)
            }
            _ => unreachable!(),
        };

        let xenum = &self.vm.enums[enum_id];
        let xenum = xenum.read();

        let edef_id = specialize_enum_id_params(self.vm, enum_id, type_params);
        let edef = self.vm.enum_defs.idx(edef_id);
        let edef = edef.read();

        let arguments = self.argument_stack.drain(..).collect::<Vec<_>>();

        match edef.layout {
            EnumLayout::Int => {
                assert_eq!(0, arguments.len());
                self.asm
                    .load_int_const(MachineMode::Int32, REG_RESULT, variant_id as i64);
                self.emit_store_register_as(REG_RESULT.into(), dest, MachineMode::Int32);
            }
            EnumLayout::Ptr => {
                let variant = &xenum.variants[variant_id];

                if variant.types.is_empty() {
                    assert_eq!(0, arguments.len());
                    self.asm.load_nil(REG_RESULT);
                    self.emit_store_register_as(REG_RESULT.into(), dest, MachineMode::Ptr);
                } else {
                    assert_eq!(1, arguments.len());
                    self.emit_load_register(arguments[0], REG_RESULT.into());
                    self.emit_store_register_as(REG_RESULT.into(), dest, MachineMode::Ptr);
                }
            }
            EnumLayout::Tagged => unimplemented!(),
        }
    }

    fn emit_nil_check(&mut self, obj: Register) {
        assert_eq!(self.bytecode.register_type(obj), BytecodeType::Ptr);

        let position = self.bytecode.offset_position(self.current_offset.to_u32());

        self.emit_load_register(obj, REG_RESULT.into());
        self.asm
            .test_if_nil_bailout(position, REG_RESULT, Trap::NIL);
    }

    fn emit_array_length(&mut self, dest: Register, arr: Register) {
        // assert_eq!(self.bytecode.register_type(dest), BytecodeType::Int32);
        assert_eq!(self.bytecode.register_type(arr), BytecodeType::Ptr);

        let position = self.bytecode.offset_position(self.current_offset.to_u32());

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

    fn emit_array_bound_check(&mut self, arr: Register, idx: Register) {
        assert_eq!(self.bytecode.register_type(arr), BytecodeType::Ptr);
        assert_eq!(self.bytecode.register_type(idx), BytecodeType::Int64);

        let position = self.bytecode.offset_position(self.current_offset.to_u32());

        self.emit_load_register(arr, REG_RESULT.into());
        self.asm
            .test_if_nil_bailout(position, REG_RESULT, Trap::NIL);

        self.emit_load_register(idx, REG_TMP1.into());

        if !self.vm.args.flag_omit_bounds_check {
            self.asm
                .check_index_out_of_bounds(position, REG_RESULT, REG_TMP1);
        }
    }

    fn emit_store_array(&mut self, src: Register, arr: Register, idx: Register) {
        assert_eq!(self.bytecode.register_type(idx), BytecodeType::Int64);
        assert_eq!(self.bytecode.register_type(arr), BytecodeType::Ptr);

        let position = self.bytecode.offset_position(self.current_offset.to_u32());

        self.emit_load_register(arr, REG_RESULT.into());
        self.asm
            .test_if_nil_bailout(position, REG_RESULT, Trap::NIL);

        self.emit_load_register(idx, REG_TMP1.into());

        if !self.vm.args.flag_omit_bounds_check {
            self.asm
                .check_index_out_of_bounds(position, REG_RESULT, REG_TMP1);
        }

        let src_type = self.specialize_register_type_unit(src);

        if src_type.is_none() {
            // nothing to do for the unit type
            return;
        }

        let src_type = src_type.unwrap();

        if let Some(tuple_id) = src_type.tuple_id() {
            let element_size = self.vm.tuples.lock().get_tuple(tuple_id).size();
            self.asm
                .array_address(REG_TMP1, REG_RESULT, REG_TMP1, element_size);
            let src_offset = self.register_offset(src);

            self.copy_tuple(
                tuple_id,
                RegOrOffset::Reg(REG_TMP1),
                RegOrOffset::Offset(src_offset),
            );

            let needs_write_barrier = self
                .vm
                .tuples
                .lock()
                .get_tuple(tuple_id)
                .contains_references();

            if self.vm.gc.needs_write_barrier() && needs_write_barrier {
                let card_table_offset = self.vm.gc.card_table_offset();
                self.asm.emit_barrier(REG_TMP1, card_table_offset);
            }
        } else {
            let value_reg: AnyReg = if src_type.mode(self.vm).is_float() {
                FREG_RESULT.into()
            } else {
                REG_TMP2.into()
            };

            self.emit_load_register(src, value_reg.into());

            self.asm.store_mem(
                src_type.mode(self.vm),
                Mem::Index(
                    REG_RESULT,
                    REG_TMP1,
                    src_type.mode(self.vm).size(),
                    offset_of_array_data(),
                ),
                value_reg,
            );

            let needs_write_barrier = src_type.is_ptr();

            if self.vm.gc.needs_write_barrier() && needs_write_barrier {
                let card_table_offset = self.vm.gc.card_table_offset();
                let scratch = self.asm.get_scratch();
                self.asm.lea(
                    *scratch,
                    Mem::Index(
                        REG_RESULT,
                        REG_TMP1,
                        src_type.mode(self.vm).size(),
                        offset_of_array_data(),
                    ),
                );
                self.asm.emit_barrier(*scratch, card_table_offset);
            }
        }
    }

    fn emit_load_array(&mut self, dest: Register, arr: Register, idx: Register) {
        assert_eq!(self.bytecode.register_type(idx), BytecodeType::Int64);
        assert_eq!(self.bytecode.register_type(arr), BytecodeType::Ptr);

        let position = self.bytecode.offset_position(self.current_offset.to_u32());

        self.emit_load_register(arr, REG_RESULT.into());
        self.asm
            .test_if_nil_bailout(position, REG_RESULT, Trap::NIL);

        self.emit_load_register(idx, REG_TMP1.into());

        if !self.vm.args.flag_omit_bounds_check {
            self.asm
                .check_index_out_of_bounds(position, REG_RESULT, REG_TMP1);
        }

        let dest_type = self.specialize_register_type_unit(dest);

        if dest_type.is_none() {
            // nothing to do for the unit type
            return;
        }

        let dest_type = dest_type.unwrap();

        if let Some(tuple_id) = dest_type.tuple_id() {
            let element_size = self.vm.tuples.lock().get_tuple(tuple_id).size();
            self.asm
                .array_address(REG_TMP1, REG_RESULT, REG_TMP1, element_size);
            let dest_offset = self.register_offset(dest);

            self.copy_tuple(
                tuple_id,
                RegOrOffset::Offset(dest_offset),
                RegOrOffset::Reg(REG_TMP1),
            );
        } else {
            let register = result_reg(self.vm, dest_type.clone());
            self.asm
                .load_array_elem(dest_type.mode(self.vm), register, REG_RESULT, REG_TMP1);
            self.emit_store_register(register, dest);
        }
    }

    fn emit_invoke_virtual(&mut self, dest: Option<Register>, fct_idx: ConstPoolIdx) {
        let (fct_id, type_params) = match self.bytecode.const_pool(fct_idx) {
            ConstPoolEntry::Fct(fct_id, type_params) => (*fct_id, type_params.clone()),
            _ => unreachable!(),
        };

        let bytecode_type = if let Some(dest) = dest {
            self.specialize_register_type_unit(dest)
        } else {
            None
        };

        let arguments = std::mem::replace(&mut self.argument_stack, Vec::new());
        let self_register = arguments[0];

        let bytecode_type_self = self.bytecode.register_type(self_register);
        let position = self.bytecode.offset_position(self.current_offset.to_u32());
        assert!(bytecode_type_self.is_ptr());

        let fct = self.vm.fcts.idx(fct_id);
        let fct = fct.read();

        let fct_return_type =
            self.specialize_type(specialize_type(self.vm, fct.return_type, &type_params));
        assert!(fct_return_type.is_concrete_type(self.vm));

        let result_register = match fct_return_type {
            BuiltinType::Tuple(_) => Some(dest.expect("need register for tuple result")),
            _ => None,
        };

        let argsize = self.emit_invoke_arguments(result_register, arguments);

        let vtable_index = fct.vtable_index.unwrap();
        let gcpoint = self.create_gcpoint();

        let (reg, ty) = match bytecode_type {
            Some(BytecodeType::Tuple(_)) => (REG_RESULT.into(), None),
            Some(bytecode_type) => (
                result_reg(self.vm, bytecode_type.clone()),
                Some(bytecode_type.mode(self.vm)),
            ),
            None => (REG_RESULT.into(), None),
        };

        let type_params = specialize_type_list(self.vm, &type_params, self.type_params);
        debug_assert!(type_params
            .iter()
            .all(|ty| !ty.contains_type_param(self.vm)));

        let self_index = if result_register.is_some() { 1 } else { 0 };
        self.asm.indirect_call(
            vtable_index,
            self_index,
            position,
            gcpoint,
            ty,
            type_params,
            reg,
        );

        self.asm.decrease_stack_frame(argsize);

        if let Some(dest) = dest {
            if !fct_return_type.is_tuple() && !fct_return_type.is_unit() {
                self.emit_store_register(reg, dest);
            }
        }
    }

    fn emit_invoke_direct(&mut self, dest: Option<Register>, fct_idx: ConstPoolIdx) {
        let (fct_id, type_params) = match self.bytecode.const_pool(fct_idx) {
            ConstPoolEntry::Fct(fct_id, type_params) => (*fct_id, type_params.clone()),
            _ => unreachable!(),
        };

        self.emit_invoke_direct_inner(dest, fct_id, type_params)
    }

    fn emit_invoke_direct_inner(
        &mut self,
        mut dest: Option<Register>,
        fct_id: FctId,
        type_params: TypeList,
    ) {
        let bytecode_type = if let Some(dest) = dest {
            self.specialize_register_type_unit(dest)
        } else {
            None
        };

        let fct = self.vm.fcts.idx(fct_id);
        let fct = fct.read();
        assert!(fct.has_self());

        let fct_return_type =
            self.specialize_type(specialize_type(self.vm, fct.return_type, &type_params));
        assert!(fct_return_type.is_concrete_type(self.vm));

        let type_params = specialize_type_list(self.vm, &type_params, self.type_params);
        debug_assert!(type_params
            .iter()
            .all(|ty| !ty.contains_type_param(self.vm)));

        let reg = if let FctKind::Builtin(intrinsic) = fct.kind {
            self.emit_invoke_intrinsic(&*fct, &mut dest, type_params, intrinsic)
        } else {
            let arguments = self.argument_stack.drain(..).collect::<Vec<_>>();
            let self_register = arguments[0];

            let bytecode_type_self = self.bytecode.register_type(self_register);
            let position = self.bytecode.offset_position(self.current_offset.to_u32());

            if bytecode_type_self.is_ptr() {
                self.emit_load_register(self_register, REG_RESULT.into());

                self.asm
                    .test_if_nil_bailout(position, REG_RESULT.into(), Trap::NIL);
            }

            let result_register = match fct_return_type {
                BuiltinType::Tuple(_) => Some(dest.expect("need register for tuple result")),
                _ => None,
            };

            let argsize = self.emit_invoke_arguments(result_register, arguments);

            let ptr = self.ptr_for_fct_id(fct_id, type_params.clone());
            let gcpoint = self.create_gcpoint();

            let (reg, mode) = match bytecode_type {
                Some(BytecodeType::Tuple(_)) => (REG_RESULT.into(), None),
                Some(bytecode_type) => (
                    result_reg(self.vm, bytecode_type.clone()),
                    Some(bytecode_type.mode(self.vm)),
                ),
                None => (REG_RESULT.into(), None),
            };

            self.asm.direct_call(
                fct_id,
                ptr.to_ptr(),
                type_params,
                position,
                gcpoint,
                mode,
                reg,
            );

            self.asm.decrease_stack_frame(argsize);

            reg
        };

        if let Some(dest) = dest {
            if !fct_return_type.is_tuple() && !fct_return_type.is_unit() {
                self.emit_store_register(reg, dest);
            }
        }
    }

    fn emit_invoke_static(&mut self, dest: Option<Register>, fct_idx: ConstPoolIdx) {
        let (fct_id, type_params) = match self.bytecode.const_pool(fct_idx) {
            ConstPoolEntry::Fct(fct_id, type_params) => (*fct_id, type_params.clone()),
            _ => unreachable!(),
        };
        self.emit_invoke_static_inner(dest, fct_id, type_params)
    }

    fn emit_invoke_static_inner(
        &mut self,
        mut dest: Option<Register>,
        fct_id: FctId,
        type_params: TypeList,
    ) {
        let fct = self.vm.fcts.idx(fct_id);
        let fct = fct.read();
        assert!(!fct.has_self());

        let fct_return_type =
            self.specialize_type(specialize_type(self.vm, fct.return_type, &type_params));
        assert!(fct_return_type.is_concrete_type(self.vm));

        let type_params = specialize_type_list(self.vm, &type_params, self.type_params);
        debug_assert!(type_params
            .iter()
            .all(|ty| !ty.contains_type_param(self.vm)));

        let reg = if let FctKind::Builtin(intrinsic) = fct.kind {
            self.emit_invoke_intrinsic(&*fct, &mut dest, type_params, intrinsic)
        } else {
            let arguments = self.argument_stack.drain(..).collect::<Vec<_>>();

            let bytecode_type = if let Some(dest) = dest {
                self.specialize_register_type_unit(dest)
            } else {
                None
            };

            let result_register = match fct_return_type {
                BuiltinType::Tuple(_) => Some(dest.expect("need register for tuple result")),
                _ => None,
            };

            let argsize = self.emit_invoke_arguments(result_register, arguments);

            let ptr = self.ptr_for_fct_id(fct_id, type_params.clone());
            let gcpoint = self.create_gcpoint();
            let position = self.bytecode.offset_position(self.current_offset.to_u32());

            let (reg, mode) = match bytecode_type {
                Some(BytecodeType::Tuple(_)) => (REG_RESULT.into(), None),
                Some(bytecode_type) => (
                    result_reg(self.vm, bytecode_type.clone()),
                    Some(bytecode_type.mode(self.vm)),
                ),
                None => (REG_RESULT.into(), None),
            };

            self.asm.direct_call(
                fct_id,
                ptr.to_ptr(),
                type_params,
                position,
                gcpoint,
                mode,
                reg,
            );

            self.asm.decrease_stack_frame(argsize);

            reg
        };

        if let Some(dest) = dest {
            if !fct_return_type.is_tuple() && !fct_return_type.is_unit() {
                self.emit_store_register(reg, dest);
            }
        }
    }

    fn emit_invoke_generic(
        &mut self,
        dest: Option<Register>,
        fct_idx: ConstPoolIdx,
        is_static: bool,
    ) {
        let (id, trait_fct_id, type_params) = match self.bytecode.const_pool(fct_idx) {
            ConstPoolEntry::Generic(id, fct_id, type_params) => (*id, *fct_id, type_params.clone()),
            _ => unreachable!(),
        };

        let fct = self.vm.fcts.idx(trait_fct_id);
        let fct = fct.read();

        let trait_id = fct.trait_id();

        assert!(self
            .fct
            .type_param_id(self.vm, id, |tp, _| tp.trait_bounds.contains(&trait_id)));

        let ty = self.type_params[id.to_usize()];
        let callee_id = self.find_trait_impl(trait_fct_id, trait_id, ty);

        if is_static {
            self.emit_invoke_static_inner(dest, callee_id, type_params);
        } else {
            self.emit_invoke_direct_inner(dest, callee_id, type_params);
        }
    }

    fn find_trait_impl(&self, fct_id: FctId, trait_id: TraitId, object_type: BuiltinType) -> FctId {
        let cls_id = object_type.cls_id(self.vm).unwrap();
        let cls = self.vm.classes.idx(cls_id);
        let cls = cls.read();

        for &impl_id in &cls.impls {
            let ximpl = self.vm.impls[impl_id].read();

            if ximpl.trait_id() != trait_id {
                continue;
            }

            for &mtd_id in &ximpl.methods {
                let mtd = self.vm.fcts.idx(mtd_id);
                let mtd = mtd.read();

                if mtd.impl_for == Some(fct_id) {
                    return mtd_id;
                }
            }
        }

        panic!("no impl found for generic trait call")
    }

    fn emit_invoke_intrinsic(
        &mut self,
        fct: &Fct,
        dest: &mut Option<Register>,
        type_params: TypeList,
        intrinsic: Intrinsic,
    ) -> AnyReg {
        let arguments = self.argument_stack.drain(..).collect::<Vec<_>>();

        match intrinsic {
            Intrinsic::Float32Sqrt | Intrinsic::Float64Sqrt => {
                debug_assert_eq!(arguments.len(), 1);

                let mode = match intrinsic {
                    Intrinsic::Float32Sqrt => MachineMode::Float32,
                    Intrinsic::Float64Sqrt => MachineMode::Float64,
                    _ => unreachable!(),
                };

                self.emit_load_register(arguments[0], FREG_RESULT.into());
                self.asm.float_sqrt(mode, FREG_RESULT, FREG_RESULT);

                FREG_RESULT.into()
            }

            Intrinsic::Int32CountZeroBits
            | Intrinsic::Int32CountZeroBitsLeading
            | Intrinsic::Int32CountZeroBitsTrailing
            | Intrinsic::Int32CountOneBits
            | Intrinsic::Int32CountOneBitsLeading
            | Intrinsic::Int32CountOneBitsTrailing => {
                debug_assert_eq!(arguments.len(), 1);
                let dest = REG_RESULT;
                self.emit_load_register(arguments[0], dest.into());

                match intrinsic {
                    Intrinsic::Int32CountZeroBits => {
                        self.asm.count_bits(MachineMode::Int32, dest, dest, false)
                    }
                    Intrinsic::Int32CountOneBits => {
                        self.asm.count_bits(MachineMode::Int32, dest, dest, true)
                    }
                    Intrinsic::Int32CountZeroBitsLeading => {
                        self.asm
                            .count_bits_leading(MachineMode::Int32, dest, dest, false)
                    }
                    Intrinsic::Int32CountOneBitsLeading => {
                        self.asm
                            .count_bits_leading(MachineMode::Int32, dest, dest, true)
                    }
                    Intrinsic::Int32CountZeroBitsTrailing => {
                        self.asm
                            .count_bits_trailing(MachineMode::Int32, dest, dest, false)
                    }
                    Intrinsic::Int32CountOneBitsTrailing => {
                        self.asm
                            .count_bits_trailing(MachineMode::Int32, dest, dest, true)
                    }
                    _ => unreachable!(),
                }

                dest.into()
            }

            Intrinsic::ReinterpretFloat32AsInt32
            | Intrinsic::ReinterpretInt32AsFloat32
            | Intrinsic::ReinterpretFloat64AsInt64
            | Intrinsic::ReinterpretInt64AsFloat64 => {
                debug_assert_eq!(arguments.len(), 1);
                let dest_reg = dest.expect("missing dest");
                let src_reg = arguments[0];

                self.emit_reinterpret(dest_reg, src_reg);

                *dest = None;
                REG_RESULT.into()
            }

            Intrinsic::Int64CountZeroBits
            | Intrinsic::Int64CountZeroBitsLeading
            | Intrinsic::Int64CountZeroBitsTrailing
            | Intrinsic::Int64CountOneBits
            | Intrinsic::Int64CountOneBitsLeading
            | Intrinsic::Int64CountOneBitsTrailing => {
                debug_assert_eq!(arguments.len(), 1);
                let dest = REG_RESULT;
                self.emit_load_register(arguments[0], dest.into());

                match intrinsic {
                    Intrinsic::Int64CountZeroBits => {
                        self.asm.count_bits(MachineMode::Int64, dest, dest, false)
                    }
                    Intrinsic::Int64CountOneBits => {
                        self.asm.count_bits(MachineMode::Int64, dest, dest, true)
                    }
                    Intrinsic::Int64CountZeroBitsLeading => {
                        self.asm
                            .count_bits_leading(MachineMode::Int64, dest, dest, false)
                    }
                    Intrinsic::Int64CountOneBitsLeading => {
                        self.asm
                            .count_bits_leading(MachineMode::Int64, dest, dest, true)
                    }
                    Intrinsic::Int64CountZeroBitsTrailing => {
                        self.asm
                            .count_bits_trailing(MachineMode::Int64, dest, dest, false)
                    }
                    Intrinsic::Int64CountOneBitsTrailing => {
                        self.asm
                            .count_bits_trailing(MachineMode::Int64, dest, dest, true)
                    }
                    _ => unreachable!(),
                }

                dest.into()
            }

            Intrinsic::Unreachable => {
                let native_fct = NativeFct {
                    ptr: Address::from_ptr(stdlib::unreachable as *const u8),
                    args: &[],
                    return_type: BuiltinType::Unit,
                    desc: NativeFctDescriptor::NativeStub(fct.id),
                };
                let position = self.bytecode.offset_position(self.current_offset.to_u32());
                let gcpoint = self.create_gcpoint();
                let result = REG_RESULT.into();
                self.asm.native_call(native_fct, position, gcpoint, result);

                // Method should never return
                self.asm.debug();

                result
            }

            Intrinsic::PromoteFloat32ToFloat64 => {
                assert_eq!(arguments.len(), 1);
                let dest_reg = dest.expect("missing dest");
                let src_reg = arguments[0];
                self.emit_promote_float(dest_reg, src_reg);

                *dest = None;
                REG_RESULT.into()
            }

            Intrinsic::DemoteFloat64ToFloat32 => {
                assert_eq!(arguments.len(), 1);
                let dest_reg = dest.expect("missing dest");
                let src_reg = arguments[0];
                self.emit_demote_float64(dest_reg, src_reg);

                *dest = None;
                REG_RESULT.into()
            }

            Intrinsic::BoolToInt32 | Intrinsic::BoolToInt64 => {
                assert_eq!(arguments.len(), 1);
                let dest_reg = dest.expect("missing dest");
                let src_reg = arguments[0];

                self.emit_load_register(src_reg, REG_RESULT.into());
                self.asm
                    .extend_byte(MachineMode::Int64, REG_RESULT, REG_RESULT);
                self.emit_store_register(REG_RESULT.into(), dest_reg);

                *dest = None;
                REG_RESULT.into()
            }

            Intrinsic::Float32ToInt32
            | Intrinsic::Float32ToInt64
            | Intrinsic::Float64ToInt32
            | Intrinsic::Float64ToInt64 => {
                assert_eq!(arguments.len(), 1);
                let dest_reg = dest.expect("missing dest");
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
                self.emit_store_register(REG_RESULT.into(), dest_reg);

                *dest = None;
                REG_RESULT.into()
            }

            Intrinsic::Int32Cmp | Intrinsic::Int64Cmp | Intrinsic::ByteCmp | Intrinsic::CharCmp => {
                assert_eq!(arguments.len(), 2);
                let dest_reg = dest.expect("missing dest");
                let lhs_reg = arguments[0];
                let rhs_reg = arguments[1];

                self.emit_load_register(lhs_reg, REG_TMP1.into());
                self.emit_load_register(rhs_reg, REG_TMP2.into());

                let mode = match intrinsic {
                    Intrinsic::Int64Cmp => MachineMode::Int64,
                    Intrinsic::Int32Cmp | Intrinsic::CharCmp => MachineMode::Int32,
                    Intrinsic::ByteCmp => MachineMode::Int8,
                    _ => unreachable!(),
                };

                self.asm.cmp_int(mode, REG_RESULT, REG_TMP1, REG_TMP2);
                self.emit_store_register(REG_RESULT.into(), dest_reg);

                *dest = None;
                REG_RESULT.into()
            }

            Intrinsic::Float32Cmp | Intrinsic::Float64Cmp => {
                assert_eq!(arguments.len(), 2);
                let dest_reg = dest.expect("missing dest");
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
                    .float_cmp_int(mode, REG_RESULT, FREG_RESULT, FREG_TMP1);
                self.emit_store_register(REG_RESULT.into(), dest_reg);

                *dest = None;
                REG_RESULT.into()
            }

            Intrinsic::Int32ToFloat32
            | Intrinsic::Int32ToFloat64
            | Intrinsic::Int64ToFloat32
            | Intrinsic::Int64ToFloat64 => {
                assert_eq!(arguments.len(), 1);
                let dest_reg = dest.expect("missing dest");
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
                self.emit_store_register(FREG_RESULT.into(), dest_reg);

                *dest = None;
                REG_RESULT.into()
            }

            Intrinsic::KillRefs => {
                assert_eq!(1, type_params.len());
                assert_eq!(2, arguments.len());
                let ty = type_params[0];
                let result = REG_RESULT.into();

                if ty.is_unit() {
                    return result;
                }

                let bytecode_type: BytecodeType = BytecodeType::from_ty(self.vm, ty);

                match bytecode_type {
                    BytecodeType::Bool
                    | BytecodeType::Char
                    | BytecodeType::UInt8
                    | BytecodeType::Int32
                    | BytecodeType::Int64
                    | BytecodeType::Float32
                    | BytecodeType::Float64 => {}

                    BytecodeType::Ptr => {
                        self.emit_load_register(arguments[0], REG_RESULT.into());
                        self.emit_load_register(arguments[1], REG_TMP1.into());

                        self.asm
                            .array_address(REG_TMP1, REG_RESULT, REG_TMP1, mem::ptr_width());
                        self.asm
                            .store_zero(MachineMode::Ptr, Mem::Base(REG_TMP1, 0));
                    }

                    BytecodeType::Tuple(tuple_id) => {
                        let tuple_id = specialize_tuple(self.vm, tuple_id, &type_params);
                        self.emit_load_register(arguments[0], REG_RESULT.into());
                        self.emit_load_register(arguments[1], REG_TMP1.into());

                        let tuple_size = self.vm.tuples.lock().get_tuple(tuple_id).size();
                        self.asm
                            .array_address(REG_TMP1, REG_RESULT, REG_TMP1, tuple_size);
                        self.zero_refs_tuple(tuple_id, RegOrOffset::Reg(REG_TMP1));
                    }

                    BytecodeType::Enum(_, _) => unimplemented!(),

                    BytecodeType::TypeParam(_) => unreachable!(),
                }

                result
            }

            _ => unreachable!(),
        }
    }

    fn emit_invoke_arguments(
        &mut self,
        result_register: Option<Register>,
        arguments: Vec<Register>,
    ) -> i32 {
        let argsize = self.determine_argsize(&arguments);

        self.asm.increase_stack_frame(argsize);

        let mut reg_idx = 0;
        let mut freg_idx = 0;
        let mut sp_offset = 0;

        match result_register {
            Some(dest) => {
                let offset = self.register_offset(dest);
                self.asm.lea(REG_PARAMS[0], Mem::Local(offset));
                reg_idx += 1;
            }

            _ => {}
        }

        for src in arguments {
            if let Some(bytecode_type) = self.specialize_register_type_unit(src) {
                let offset = self.register_offset(src);

                match bytecode_type {
                    BytecodeType::Tuple(_tuple_id) => {
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
                        let mode = bytecode_type.mode(self.vm);

                        if freg_idx < FREG_PARAMS.len() {
                            self.asm.load_mem(
                                mode,
                                FREG_PARAMS[freg_idx].into(),
                                Mem::Local(offset),
                            );
                            freg_idx += 1;
                        } else {
                            self.asm
                                .load_mem(mode, FREG_TMP1.into(), Mem::Local(offset));
                            self.asm.store_mem(
                                mode,
                                Mem::Base(REG_SP, sp_offset),
                                FREG_TMP1.into(),
                            );

                            sp_offset += 8;
                        }
                    }
                    _ => {
                        let mode = bytecode_type.mode(self.vm);

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
                        argsize += 8;
                    }

                    freg_idx += 1;
                }
                _ => {
                    if reg_idx >= REG_PARAMS.len() {
                        argsize += 8;
                    }

                    reg_idx += 1;
                }
            }
        }

        mem::align_i32(argsize, STACK_FRAME_ALIGNMENT as i32)
    }

    fn ptr_for_fct_id(&mut self, fid: FctId, type_params: TypeList) -> Address {
        if self.fct.id == fid {
            // we want to recursively invoke the function we are compiling right now
            ensure_jit_or_stub_ptr(self.src, self.vm, type_params)
        } else {
            let fct = self.vm.fcts.idx(fid);
            let fct = fct.read();

            match fct.kind {
                FctKind::Source(_) => {
                    let src = fct.src();
                    let src = src.read();

                    ensure_jit_or_stub_ptr(&src, self.vm, type_params)
                }

                FctKind::Native(ptr) => {
                    assert!(type_params.is_empty());
                    let internal_fct = NativeFct {
                        ptr,
                        args: fct.params_with_self(),
                        return_type: fct.return_type,
                        desc: NativeFctDescriptor::NativeStub(fid),
                    };

                    ensure_native_stub(self.vm, Some(fid), internal_fct)
                }

                FctKind::Definition => panic!("prototype for fct call"),
                FctKind::Builtin(_) => panic!("intrinsic fct call"),
            }
        }
    }

    fn specialize_type(&self, ty: BuiltinType) -> BuiltinType {
        specialize_type(self.vm, ty, self.type_params)
    }

    fn register_offset(&self, reg: Register) -> i32 {
        let Register(idx) = reg;
        self.offsets[idx].expect("offset missing")
    }

    fn specialize_register_type(&self, reg: Register) -> BytecodeType {
        let ty = self.bytecode.register_type(reg);
        self.specialize_bytecode_type(ty)
    }

    fn specialize_register_type_unit(&self, reg: Register) -> Option<BytecodeType> {
        let ty = self.bytecode.register_type(reg);
        self.specialize_bytecode_type_unit(ty)
    }

    fn specialize_bytecode_type(&self, ty: BytecodeType) -> BytecodeType {
        match ty {
            BytecodeType::TypeParam(id) => {
                let ty = self.type_params[id as usize];
                BytecodeType::from_ty(self.vm, ty)
            }
            _ => ty,
        }
    }

    fn specialize_bytecode_type_unit(&self, ty: BytecodeType) -> Option<BytecodeType> {
        match ty {
            BytecodeType::TypeParam(id) => {
                let ty = self.type_params[id as usize];

                if ty.is_unit() {
                    None
                } else {
                    Some(BytecodeType::from_ty(self.vm, ty))
                }
            }
            BytecodeType::Tuple(tuple_id) => Some(BytecodeType::Tuple(specialize_tuple(
                self.vm,
                tuple_id,
                self.type_params,
            ))),
            _ => Some(ty),
        }
    }
}

impl<'a, 'ast: 'a> BytecodeVisitor for CannonCodeGen<'a, 'ast> {
    fn visit_instruction(&mut self, offset: BytecodeOffset) {
        self.offset_to_address.insert(offset, self.asm.pos());
        self.current_offset = offset;
    }

    fn visit_push(&mut self, opnd: Register) {
        comment!(self, format!("Push {}", opnd));
        self.emit_push(opnd);
    }
    fn visit_pop(&mut self, dest: Register) {
        comment!(self, format!("Pop {}", dest));
        self.emit_pop(dest);
    }

    fn visit_add_int32_stack(&mut self) {
        comment!(self, format!("AddInt32"));
        self.emit_add_int_stack();
    }
    fn visit_add_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("AddInt32 {}, {}, {}", dest, lhs, rhs));
        self.emit_add_int(dest, lhs, rhs);
    }
    fn visit_add_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("AddInt64 {}, {}, {}", dest, lhs, rhs));
        self.emit_add_int(dest, lhs, rhs);
    }
    fn visit_add_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("AddFloat32 {}, {}, {}", dest, lhs, rhs));
        self.emit_add_float(dest, lhs, rhs);
    }
    fn visit_add_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("AddFloat64 {}, {}, {}", dest, lhs, rhs));
        self.emit_add_float(dest, lhs, rhs);
    }

    fn visit_sub_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("SubInt32 {}, {}, {}", dest, lhs, rhs));
        self.emit_sub_int(dest, lhs, rhs);
    }
    fn visit_sub_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("SubInt64 {}, {}, {}", dest, lhs, rhs));
        self.emit_sub_int(dest, lhs, rhs);
    }
    fn visit_sub_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("SubFloat32 {}, {}, {}", dest, lhs, rhs));
        self.emit_sub_float(dest, lhs, rhs);
    }
    fn visit_sub_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("SubFloat64 {}, {}, {}", dest, lhs, rhs));
        self.emit_sub_float(dest, lhs, rhs);
    }

    fn visit_neg_int32(&mut self, dest: Register, src: Register) {
        comment!(self, format!("NegInt32 {}, {}", dest, src));
        self.emit_neg_int(dest, src);
    }
    fn visit_neg_int64(&mut self, dest: Register, src: Register) {
        comment!(self, format!("NegInt64 {}, {}", dest, src));
        self.emit_neg_int(dest, src);
    }
    fn visit_neg_float32(&mut self, dest: Register, src: Register) {
        comment!(self, format!("NegFloat32 {}, {}", dest, src));
        self.emit_neg_float(dest, src);
    }
    fn visit_neg_float64(&mut self, dest: Register, src: Register) {
        comment!(self, format!("NegFloat64 {}, {}", dest, src));
        self.emit_neg_float(dest, src);
    }

    fn visit_mul_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("MulInt32 {}, {}, {}", dest, lhs, rhs));
        self.emit_mul_int(dest, lhs, rhs);
    }
    fn visit_mul_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("MulInt64 {}, {}, {}", dest, lhs, rhs));
        self.emit_mul_int(dest, lhs, rhs);
    }
    fn visit_mul_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("MulFloat32 {}, {}, {}", dest, lhs, rhs));
        self.emit_mul_float(dest, lhs, rhs);
    }
    fn visit_mul_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("MulFloat64 {}, {}, {}", dest, lhs, rhs));
        self.emit_mul_float(dest, lhs, rhs);
    }

    fn visit_div_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("DivInt32 {}, {}, {}", dest, lhs, rhs));
        self.emit_div_int(dest, lhs, rhs);
    }
    fn visit_div_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("DivInt64 {}, {}, {}", dest, lhs, rhs));
        self.emit_div_int(dest, lhs, rhs);
    }
    fn visit_div_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("DivFloat32 {}, {}, {}", dest, lhs, rhs));
        self.emit_div_float(dest, lhs, rhs);
    }
    fn visit_div_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("DivFloat64 {}, {}, {}", dest, lhs, rhs));
        self.emit_div_float(dest, lhs, rhs);
    }

    fn visit_mod_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("ModInt32 {}, {}, {}", dest, lhs, rhs));
        self.emit_mod_int(dest, lhs, rhs);
    }
    fn visit_mod_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("ModInt64 {}, {}, {}", dest, lhs, rhs));
        self.emit_mod_int(dest, lhs, rhs);
    }

    fn visit_and_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("AndInt32 {}, {}, {}", dest, lhs, rhs));
        self.emit_and_int(dest, lhs, rhs);
    }
    fn visit_and_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("AndInt64 {}, {}, {}", dest, lhs, rhs));
        self.emit_and_int(dest, lhs, rhs);
    }

    fn visit_or_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("OrInt32 {}, {}, {}", dest, lhs, rhs));
        self.emit_or_int(dest, lhs, rhs)
    }
    fn visit_or_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("OrInt64 {}, {}, {}", dest, lhs, rhs));
        self.emit_or_int(dest, lhs, rhs)
    }

    fn visit_xor_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("XorInt32 {}, {}, {}", dest, lhs, rhs));
        self.emit_xor_int(dest, lhs, rhs);
    }
    fn visit_xor_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("XorInt64 {}, {}, {}", dest, lhs, rhs));
        self.emit_xor_int(dest, lhs, rhs);
    }

    fn visit_not_bool(&mut self, dest: Register, src: Register) {
        comment!(self, format!("NotBool {}, {}", dest, src));
        self.emit_not_bool(dest, src);
    }
    fn visit_not_int32(&mut self, dest: Register, src: Register) {
        comment!(self, format!("NotInt32 {}, {}", dest, src));
        self.emit_not_int(dest, src);
    }
    fn visit_not_int64(&mut self, dest: Register, src: Register) {
        comment!(self, format!("NotInt64 {}, {}", dest, src));
        self.emit_not_int(dest, src);
    }

    fn visit_shl_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("ShlInt32 {}, {}, {}", dest, lhs, rhs));
        self.emit_shl_int(dest, lhs, rhs);
    }
    fn visit_shr_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("ShrInt32 {}, {}, {}", dest, lhs, rhs));
        self.emit_shr_int(dest, lhs, rhs);
    }
    fn visit_sar_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("SarInt32 {}, {}, {}", dest, lhs, rhs));
        self.emit_sar_int(dest, lhs, rhs);
    }

    fn visit_shl_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("ShlInt64 {}, {}, {}", dest, lhs, rhs));
        self.emit_shl_int(dest, lhs, rhs);
    }
    fn visit_shr_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("ShrInt64 {}, {}, {}", dest, lhs, rhs));
        self.emit_shr_int(dest, lhs, rhs);
    }
    fn visit_sar_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("SarInt64 {}, {}, {}", dest, lhs, rhs));
        self.emit_sar_int(dest, lhs, rhs);
    }

    fn visit_rol_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("RolInt32 {}, {}, {}", dest, lhs, rhs));
        self.emit_rol_int(dest, lhs, rhs);
    }
    fn visit_ror_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("RorInt32 {}, {}, {}", dest, lhs, rhs));
        self.emit_ror_int(dest, lhs, rhs);
    }

    fn visit_rol_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("RolInt64 {}, {}, {}", dest, lhs, rhs));
        self.emit_rol_int(dest, lhs, rhs);
    }
    fn visit_ror_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("RorInt64 {}, {}, {}", dest, lhs, rhs));
        self.emit_ror_int(dest, lhs, rhs);
    }

    fn visit_extend_byte_to_char(&mut self, dest: Register, src: Register) {
        comment!(self, format!("ExtendByteToChar {}, {}", dest, src));
        self.emit_extend_byte(dest, src, MachineMode::Int32);
    }
    fn visit_extend_byte_to_int32(&mut self, dest: Register, src: Register) {
        comment!(self, format!("ExtendByteToInt32 {}, {}", dest, src));
        self.emit_extend_byte(dest, src, MachineMode::Int32);
    }
    fn visit_extend_byte_to_int64(&mut self, dest: Register, src: Register) {
        comment!(self, format!("ExtendByteToInt64 {}, {}", dest, src));
        self.emit_extend_byte(dest, src, MachineMode::Int64);
    }
    fn visit_extend_int32_to_int64(&mut self, dest: Register, src: Register) {
        comment!(self, format!("ExtendInt32ToInt64 {}, {}", dest, src));
        self.emit_int_to_int64(dest, src);
    }
    fn visit_extend_char_to_int64(&mut self, dest: Register, src: Register) {
        comment!(self, format!("ExtendCharToInt64 {}, {}", dest, src));
        self.emit_shrink(dest, MachineMode::Int64, src, MachineMode::Int32);
    }
    fn visit_cast_char_to_int32(&mut self, dest: Register, src: Register) {
        comment!(self, format!("CastCharToInt32 {}, {}", dest, src));
        self.emit_shrink(dest, MachineMode::Int32, src, MachineMode::Int32);
    }
    fn visit_cast_int32_to_uint8(&mut self, dest: Register, src: Register) {
        comment!(self, format!("CastInt32ToUInt8 {}, {}", dest, src));
        self.emit_shrink(dest, MachineMode::Int8, src, MachineMode::Int32);
    }
    fn visit_cast_int32_to_char(&mut self, dest: Register, src: Register) {
        comment!(self, format!("CastInt32ToChar {}, {}", dest, src));
        self.emit_shrink(dest, MachineMode::Int32, src, MachineMode::Int32);
    }
    fn visit_cast_int64_to_uint8(&mut self, dest: Register, src: Register) {
        comment!(self, format!("CastInt64ToUInt8 {}, {}", dest, src));
        self.emit_shrink(dest, MachineMode::Int8, src, MachineMode::Int64);
    }
    fn visit_cast_int64_to_char(&mut self, dest: Register, src: Register) {
        comment!(self, format!("CastInt64ToChar {}, {}", dest, src));
        self.emit_shrink(dest, MachineMode::Int32, src, MachineMode::Int64);
    }
    fn visit_cast_int64_to_int32(&mut self, dest: Register, src: Register) {
        comment!(self, format!("CastInt64ToInt32 {}, {}", dest, src));
        self.emit_int64_to_int(dest, src);
    }

    fn visit_instance_of(&mut self, dest: Register, src: Register, cls_idx: ConstPoolIdx) {
        comment!(self, {
            let (cls_id, type_params) = match self.bytecode.const_pool(cls_idx) {
                ConstPoolEntry::Class(cls_id, type_params) => (*cls_id, type_params),
                _ => unreachable!(),
            };
            let cls = self.vm.classes.idx(cls_id);
            let cls = cls.read();
            let cname = cls.name_with_params(self.vm, type_params);
            format!(
                "InstanceOf {}, {}, ConstPoolIdx({}) # {}",
                dest,
                src,
                cls_idx.to_usize(),
                cname
            )
        });
        self.emit_instanceof(dest, src, cls_idx, true);
    }
    fn visit_checked_cast(&mut self, src: Register, cls_idx: ConstPoolIdx) {
        comment!(self, {
            let (cls_id, type_params) = match self.bytecode.const_pool(cls_idx) {
                ConstPoolEntry::Class(cls_id, type_params) => (*cls_id, type_params),
                _ => unreachable!(),
            };
            let cls = self.vm.classes.idx(cls_id);
            let cls = cls.read();
            let cname = cls.name_with_params(self.vm, type_params);
            format!(
                "CheckedCast {}, ConstPoolIdx({}) # {}",
                src,
                cls_idx.to_usize(),
                cname
            )
        });
        self.emit_instanceof(Register::invalid(), src, cls_idx, false);
    }

    fn visit_mov_bool(&mut self, dest: Register, src: Register) {
        comment!(self, format!("MovBool {}, {}", dest, src));
        self.emit_mov_generic(dest, src);
    }
    fn visit_mov_uint8(&mut self, dest: Register, src: Register) {
        comment!(self, format!("MovUInt8 {}, {}", dest, src));
        self.emit_mov_generic(dest, src);
    }
    fn visit_mov_char(&mut self, dest: Register, src: Register) {
        comment!(self, format!("MovChar {}, {}", dest, src));
        self.emit_mov_generic(dest, src);
    }
    fn visit_mov_int32(&mut self, dest: Register, src: Register) {
        comment!(self, format!("MovInt32 {}, {}", dest, src));
        self.emit_mov_generic(dest, src);
    }
    fn visit_mov_int64(&mut self, dest: Register, src: Register) {
        comment!(self, format!("MovInt64 {}, {}", dest, src));
        self.emit_mov_generic(dest, src);
    }
    fn visit_mov_float32(&mut self, dest: Register, src: Register) {
        comment!(self, format!("MovFloat32 {}, {}", dest, src));
        self.emit_mov_generic(dest, src);
    }
    fn visit_mov_float64(&mut self, dest: Register, src: Register) {
        comment!(self, format!("MovFloat64 {}, {}", dest, src));
        self.emit_mov_generic(dest, src);
    }
    fn visit_mov_ptr(&mut self, dest: Register, src: Register) {
        comment!(self, format!("MovPtr {}, {}", dest, src));
        self.emit_mov_generic(dest, src);
    }
    fn visit_mov_tuple(&mut self, dest: Register, src: Register, tuple_id: TupleId) {
        comment!(self, {
            let tuple_ty = BuiltinType::Tuple(tuple_id);
            let tuple_ty_name = tuple_ty.name(self.vm);
            format!(
                "MovTuple {}, {}, Tuple({})={}",
                dest,
                src,
                tuple_id.to_usize(),
                tuple_ty_name,
            )
        });

        self.emit_mov_tuple(dest, src, tuple_id);
    }
    fn visit_mov_generic(&mut self, dest: Register, src: Register) {
        comment!(self, format!("MovGeneric {}, {}", dest, src));
        self.emit_mov_generic(dest, src);
    }
    fn visit_mov_enum(&mut self, dest: Register, src: Register, idx: ConstPoolIdx) {
        comment!(self, {
            let (enum_id, type_params) = match self.bytecode.const_pool(idx) {
                ConstPoolEntry::Enum(enum_id, type_params) => (*enum_id, type_params),
                _ => unreachable!(),
            };
            let xenum = &self.vm.enums[enum_id];
            let xenum = xenum.read();
            let xenum_name = xenum.name_with_params(self.vm, type_params);
            format!(
                "MovEnum {}, {}, ConstPoolIdx({}) # {}",
                dest,
                src,
                idx.to_usize(),
                xenum_name
            )
        });
        self.emit_mov_generic(dest, src);
    }

    fn visit_load_tuple_element(
        &mut self,
        dest: Register,
        src: Register,
        tuple_id: TupleId,
        idx: u32,
    ) {
        comment!(self, {
            let tuple_ty = BuiltinType::Tuple(tuple_id);
            let tuple_ty_name = tuple_ty.name(self.vm);
            format!(
                "LoadTupleElement {}, {}, Tuple({})={}.{}",
                dest,
                src,
                tuple_id.to_usize(),
                tuple_ty_name,
                idx
            )
        });
        self.emit_load_tuple_element(dest, src, tuple_id, idx);
    }

    fn visit_load_field(&mut self, dest: Register, obj: Register, field_idx: ConstPoolIdx) {
        comment!(self, {
            let (cls_id, type_params, field_id) = match self.bytecode.const_pool(field_idx) {
                ConstPoolEntry::Field(cls_id, type_params, field_id) => {
                    (*cls_id, type_params, *field_id)
                }
                _ => unreachable!(),
            };
            let cls = self.vm.classes.idx(cls_id);
            let cls = cls.read();
            let cname = cls.name_with_params(self.vm, type_params);

            let field = &cls.fields[field_id.idx()];
            let fname = self.vm.interner.str(field.name);

            format!(
                "LoadField {}, {}, ConstPoolIdx({}) # {}.{}",
                dest,
                obj,
                field_idx.to_usize(),
                cname,
                fname
            )
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
            let cls = self.vm.classes.idx(cls_id);
            let cls = cls.read();
            let cname = cls.name_with_params(self.vm, type_params);

            let field = &cls.fields[field_id.idx()];
            let fname = self.vm.interner.str(field.name);

            format!(
                "StoreField {}, {}, ConstPoolIdx({}) # {}.{}",
                src,
                obj,
                field_idx.to_usize(),
                cname,
                fname
            )
        });
        self.emit_store_field(src, obj, field_idx);
    }

    fn visit_load_global(&mut self, dest: Register, glob_id: GlobalId) {
        comment!(self, {
            let glob = self.vm.globals.idx(glob_id);
            let glob = glob.read();
            let name = self.vm.interner.str(glob.name);
            format!(
                "LoadGlobal {}, GlobalId({}) # {}",
                dest,
                glob_id.to_usize(),
                name
            )
        });
        self.emit_load_global(dest, glob_id);
    }

    fn visit_store_global(&mut self, src: Register, glob_id: GlobalId) {
        comment!(self, {
            let glob = self.vm.globals.idx(glob_id);
            let glob = glob.read();
            let name = self.vm.interner.str(glob.name);
            format!(
                "StoreGlobal {}, GlobalId({}) # {}",
                src,
                glob_id.to_usize(),
                name
            )
        });
        self.emit_store_global(src, glob_id);
    }

    fn visit_push_register(&mut self, src: Register) {
        comment!(self, format!("PushRegister {}", src));
        self.argument_stack.push(src);
    }

    fn visit_const_nil(&mut self, dest: Register) {
        comment!(self, format!("ConstNil {}", dest));
        self.emit_const_nil(dest);
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
        comment!(
            self,
            format!("ConstChar {}, {} # {}", dest, idx.to_usize(), value)
        );
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
            format!(
                "ConstInt32 {}, ConstPoolId({}) # {}",
                dest,
                idx.to_usize(),
                value
            )
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
            format!(
                "ConstInt64 {}, ConstPoolId({}) # {}",
                dest,
                idx.to_usize(),
                value
            )
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
            format!(
                "ConstFloat32 {}, ConstPoolId({}) # {}",
                dest,
                idx.to_usize(),
                value
            )
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
            format!(
                "ConstFloat64 {}, ConstPoolId({}) # {}",
                dest,
                idx.to_usize(),
                value
            )
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
            format!(
                "ConstString {}, ConstPoolId({}) # {}",
                dest,
                idx.to_usize(),
                value
            )
        );
        self.emit_const_string(dest, value);
    }

    fn visit_test_identity(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestIdentity {}, {}, {}", dest, lhs, rhs));
        self.emit_test_identity(dest, lhs, rhs);
    }

    fn visit_test_eq_bool(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestEqBool {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::Equal);
    }
    fn visit_test_ne_bool(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestNeBool {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::NotEqual);
    }

    fn visit_test_eq_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestEqUInt8 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::Equal);
    }
    fn visit_test_ne_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestNeUInt8 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::NotEqual);
    }
    fn visit_test_gt_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestGtUInt8 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::Greater);
    }
    fn visit_test_ge_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestGeUInt8 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::GreaterEq);
    }
    fn visit_test_lt_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestLtUInt8 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::Less);
    }
    fn visit_test_le_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestLeUInt8 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::LessEq);
    }

    fn visit_test_eq_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestEqChar {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::Equal);
    }
    fn visit_test_ne_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestNeChar {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::NotEqual);
    }
    fn visit_test_gt_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestGtChar {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::Greater);
    }
    fn visit_test_ge_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestGeChar {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::GreaterEq);
    }
    fn visit_test_lt_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestLtChar {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::Less);
    }
    fn visit_test_le_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestLeChar {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::LessEq);
    }

    fn visit_test_eq_enum(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestEqEnum {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::Equal);
    }
    fn visit_test_ne_enum(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestNeEnum {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::NotEqual);
    }

    fn visit_test_eq_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestEqInt32 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::Equal);
    }
    fn visit_test_ne_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestNeInt32 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::NotEqual);
    }
    fn visit_test_gt_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestGtInt32 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::Greater);
    }
    fn visit_test_ge_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestGeInt32 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::GreaterEq);
    }
    fn visit_test_lt_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestLtInt32 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::Less);
    }
    fn visit_test_le_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestLeInt32 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::LessEq);
    }

    fn visit_test_eq_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestEqInt64 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::Equal);
    }
    fn visit_test_ne_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestNeInt64 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::NotEqual);
    }
    fn visit_test_gt_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestGtInt64 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::Greater);
    }
    fn visit_test_ge_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestGeInt64 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::GreaterEq);
    }
    fn visit_test_lt_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestLtInt64 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::Less);
    }
    fn visit_test_le_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestLeInt64 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_generic(dest, lhs, rhs, CondCode::LessEq);
    }

    fn visit_test_eq_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestEqFloat32 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_float(dest, lhs, rhs, CondCode::Equal);
    }
    fn visit_test_ne_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestNeFloat32 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_float(dest, lhs, rhs, CondCode::NotEqual);
    }
    fn visit_test_gt_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestGtFloat32 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_float(dest, lhs, rhs, CondCode::Greater);
    }
    fn visit_test_ge_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestGeFloat32 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_float(dest, lhs, rhs, CondCode::GreaterEq);
    }
    fn visit_test_lt_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestLtFloat32 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_float(dest, lhs, rhs, CondCode::Less);
    }
    fn visit_test_le_float32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestLeFloat32 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_float(dest, lhs, rhs, CondCode::LessEq);
    }

    fn visit_test_eq_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestEqFloat64 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_float(dest, lhs, rhs, CondCode::Equal);
    }
    fn visit_test_ne_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestNeFloat64 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_float(dest, lhs, rhs, CondCode::NotEqual);
    }
    fn visit_test_gt_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestGtFloat64 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_float(dest, lhs, rhs, CondCode::Greater);
    }
    fn visit_test_ge_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestGeFloat64 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_float(dest, lhs, rhs, CondCode::GreaterEq);
    }
    fn visit_test_lt_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestLtFloat64 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_float(dest, lhs, rhs, CondCode::Less);
    }
    fn visit_test_le_float64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        comment!(self, format!("TestLeFloat64 {}, {}, {}", dest, lhs, rhs));
        self.emit_test_float(dest, lhs, rhs, CondCode::LessEq);
    }

    fn visit_assert(&mut self, value: Register) {
        comment!(self, format!("Assert {}", value));
        assert_eq!(self.bytecode.register_type(value), BytecodeType::Bool);
        let position = self.bytecode.offset_position(self.current_offset.to_u32());
        self.emit_load_register(value, REG_RESULT.into());

        self.asm.assert(REG_RESULT, position);
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
    fn visit_jump_if_false_const(&mut self, opnd: Register, idx: ConstPoolIdx) {
        let offset = self
            .bytecode
            .const_pool(idx)
            .to_int32()
            .expect("int expected");
        let target = BytecodeOffset((self.current_offset.to_u32() as i32 + offset) as u32);
        comment!(
            self,
            format!(
                "JumpIfFalseConst {}, ConstPoolId({}) # target {}",
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
    fn visit_jump_if_true_const(&mut self, opnd: Register, idx: ConstPoolIdx) {
        let offset = self
            .bytecode
            .const_pool(idx)
            .to_int32()
            .expect("int expected");
        let target = BytecodeOffset((self.current_offset.to_u32() as i32 + offset) as u32);
        comment!(
            self,
            format!(
                "JumpIfTrueConst {}, ConstPoolId({}) # target {}",
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
        self.emit_stack_guard();
        self.emit_jump(target);
    }
    fn visit_jump(&mut self, offset: u32) {
        let target = BytecodeOffset(self.current_offset.to_u32() + offset);
        comment!(
            self,
            format!("Jump {} # target {}", offset, target.to_usize())
        );
        self.emit_jump(target);
    }
    fn visit_jump_const(&mut self, idx: ConstPoolIdx) {
        let offset = self
            .bytecode
            .const_pool(idx)
            .to_int32()
            .expect("int expected");
        comment!(self, {
            let target = BytecodeOffset((self.current_offset.to_u32() as i32 + offset) as u32);
            format!(
                "JumpConst ConstPoolId({}) # target {}",
                idx.to_usize(),
                target.to_usize()
            )
        });
        self.visit_jump(offset as u32);
    }

    fn visit_invoke_direct_void(&mut self, fctdef: ConstPoolIdx) {
        comment!(self, format!("InvokeDirectVoid {}", fctdef.to_usize()));
        self.emit_invoke_direct(None, fctdef)
    }
    fn visit_invoke_direct(&mut self, dest: Register, fctdef: ConstPoolIdx) {
        comment!(
            self,
            format!("InvokeDirect {}, {}", dest, fctdef.to_usize())
        );
        self.emit_invoke_direct(Some(dest), fctdef);
    }

    fn visit_invoke_virtual_void(&mut self, fctdef: ConstPoolIdx) {
        comment!(self, format!("InvokeVirtualVoid {}", fctdef.to_usize()));
        self.emit_invoke_virtual(None, fctdef);
    }
    fn visit_invoke_virtual(&mut self, dest: Register, fctdef: ConstPoolIdx) {
        comment!(
            self,
            format!("InvokeVirtual {}, {}", dest, fctdef.to_usize())
        );
        self.emit_invoke_virtual(Some(dest), fctdef);
    }

    fn visit_invoke_static_void(&mut self, fctdef: ConstPoolIdx) {
        comment!(self, format!("InvokeStaticVoid {}", fctdef.to_usize()));
        self.emit_invoke_static(None, fctdef)
    }
    fn visit_invoke_static(&mut self, dest: Register, fctdef: ConstPoolIdx) {
        comment!(
            self,
            format!("InvokeStatic {}, {}", dest, fctdef.to_usize())
        );
        self.emit_invoke_static(Some(dest), fctdef);
    }

    fn visit_invoke_generic_direct_void(&mut self, fctdef: ConstPoolIdx) {
        comment!(
            self,
            format!("InvokeGenericDirectVoid {}", fctdef.to_usize())
        );
        self.emit_invoke_generic(None, fctdef, false);
    }
    fn visit_invoke_generic_direct(&mut self, dest: Register, fctdef: ConstPoolIdx) {
        comment!(
            self,
            format!("InvokeGenericDirect {}, {}", dest, fctdef.to_usize())
        );
        self.emit_invoke_generic(Some(dest), fctdef, false);
    }

    fn visit_invoke_generic_static_void(&mut self, fctdef: ConstPoolIdx) {
        comment!(
            self,
            format!("InvokeGenericStaticVoid {}", fctdef.to_usize())
        );
        self.emit_invoke_generic(None, fctdef, true);
    }
    fn visit_invoke_generic_static(&mut self, dest: Register, fctdef: ConstPoolIdx) {
        comment!(
            self,
            format!("InvokeGenericStatic {}, {}", dest, fctdef.to_usize())
        );
        self.emit_invoke_generic(Some(dest), fctdef, true);
    }

    fn visit_new_object(&mut self, dest: Register, idx: ConstPoolIdx) {
        comment!(self, {
            let (cls_id, type_params) = match self.bytecode.const_pool(idx) {
                ConstPoolEntry::Class(cls_id, type_params) => (*cls_id, type_params),
                _ => unreachable!(),
            };
            let cls = self.vm.classes.idx(cls_id);
            let cls = cls.read();
            let cname = cls.name_with_params(self.vm, type_params);
            format!(
                "NewObject {}, ConstPoolIdx({}) # {}",
                dest,
                idx.to_usize(),
                cname
            )
        });
        self.emit_new_object(dest, idx)
    }
    fn visit_new_array(&mut self, dest: Register, idx: ConstPoolIdx, length: Register) {
        comment!(self, {
            let (cls_id, type_params) = match self.bytecode.const_pool(idx) {
                ConstPoolEntry::Class(cls_id, type_params) => (*cls_id, type_params),
                _ => unreachable!(),
            };
            let cls = self.vm.classes.idx(cls_id);
            let cls = cls.read();
            let cname = cls.name_with_params(self.vm, type_params);
            format!(
                "NewArray {}, ConstPoolIdx({}), {} # {}",
                dest,
                idx.to_usize(),
                length,
                cname
            )
        });
        self.emit_new_array(dest, idx, length);
    }
    fn visit_new_tuple(&mut self, dest: Register, tuple_id: TupleId) {
        comment!(self, {
            let tuple_name = BuiltinType::Tuple(tuple_id).name(self.vm);
            format!(
                "NewTuple {}, TupleId({}) # {}",
                dest,
                tuple_id.to_usize(),
                tuple_name
            )
        });
        self.emit_new_tuple(dest, tuple_id);
    }

    fn visit_new_enum(&mut self, dest: Register, idx: ConstPoolIdx) {
        comment!(self, {
            let (enum_id, type_params, _variant_id) = match self.bytecode.const_pool(idx) {
                ConstPoolEntry::EnumVariant(enum_id, type_params, variant_id) => {
                    (*enum_id, type_params, *variant_id)
                }
                _ => unreachable!(),
            };
            let xenum = &self.vm.enums[enum_id];
            let xenum = xenum.read();
            let xenum_name = xenum.name_with_params(self.vm, type_params);
            format!(
                "MovEnum {}, ConstPoolIdx({}) # {}",
                dest,
                idx.to_usize(),
                xenum_name
            )
        });
        self.emit_new_enum(dest, idx);
    }

    fn visit_nil_check(&mut self, obj: Register) {
        comment!(self, format!("NilCheck {}", obj));
        self.emit_nil_check(obj);
    }

    fn visit_array_length(&mut self, dest: Register, arr: Register) {
        comment!(self, format!("ArrayLength {}, {}", dest, arr));
        self.emit_array_length(dest, arr);
    }
    fn visit_array_bound_check(&mut self, arr: Register, idx: Register) {
        comment!(self, format!("ArrayBoundCheck {}, {}", arr, idx));
        self.emit_array_bound_check(arr, idx);
    }

    fn visit_load_array_bool(&mut self, dest: Register, arr: Register, idx: Register) {
        comment!(self, format!("LoadArrayBool {}, {}, {}", dest, arr, idx));
        self.emit_load_array(dest, arr, idx);
    }
    fn visit_load_array_uint8(&mut self, dest: Register, arr: Register, idx: Register) {
        comment!(self, format!("LoadArrayUInt8 {}, {}, {}", dest, arr, idx));
        self.emit_load_array(dest, arr, idx);
    }
    fn visit_load_array_char(&mut self, dest: Register, arr: Register, idx: Register) {
        comment!(self, format!("LoadArrayChar {}, {}, {}", dest, arr, idx));
        self.emit_load_array(dest, arr, idx);
    }
    fn visit_load_array_int32(&mut self, dest: Register, arr: Register, idx: Register) {
        comment!(self, format!("LoadArrayInt32 {}, {}, {}", dest, arr, idx));
        self.emit_load_array(dest, arr, idx);
    }
    fn visit_load_array_int64(&mut self, dest: Register, arr: Register, idx: Register) {
        comment!(self, format!("LoadArrayInt64 {}, {}, {}", dest, arr, idx));
        self.emit_load_array(dest, arr, idx);
    }
    fn visit_load_array_float32(&mut self, dest: Register, arr: Register, idx: Register) {
        comment!(self, format!("LoadArrayFloat32 {}, {}, {}", dest, arr, idx));
        self.emit_load_array(dest, arr, idx);
    }
    fn visit_load_array_float64(&mut self, dest: Register, arr: Register, idx: Register) {
        comment!(self, format!("LoadArrayFloat64 {}, {}, {}", dest, arr, idx));
        self.emit_load_array(dest, arr, idx);
    }
    fn visit_load_array_ptr(&mut self, dest: Register, arr: Register, idx: Register) {
        comment!(self, format!("LoadArrayPtr {}, {}, {}", dest, arr, idx));
        self.emit_load_array(dest, arr, idx);
    }
    fn visit_load_array_tuple(&mut self, dest: Register, arr: Register, idx: Register) {
        comment!(self, format!("LoadArrayTuple {}, {}, {}", dest, arr, idx));
        self.emit_load_array(dest, arr, idx);
    }
    fn visit_load_array_generic(&mut self, dest: Register, arr: Register, idx: Register) {
        comment!(self, format!("LoadArrayGeneric {}, {}, {}", dest, arr, idx));
        self.emit_load_array(dest, arr, idx);
    }

    fn visit_store_array_bool(&mut self, src: Register, arr: Register, idx: Register) {
        comment!(self, format!("StoreArrayBool {}, {}, {}", src, arr, idx));
        self.emit_store_array(src, arr, idx);
    }
    fn visit_store_array_uint8(&mut self, src: Register, arr: Register, idx: Register) {
        comment!(self, format!("StoreArrayUInt8 {}, {}, {}", src, arr, idx));
        self.emit_store_array(src, arr, idx);
    }
    fn visit_store_array_char(&mut self, src: Register, arr: Register, idx: Register) {
        comment!(self, format!("StoreArrayChar {}, {}, {}", src, arr, idx));
        self.emit_store_array(src, arr, idx);
    }
    fn visit_store_array_int32(&mut self, src: Register, arr: Register, idx: Register) {
        comment!(self, format!("StoreArrayInt32 {}, {}, {}", src, arr, idx));
        self.emit_store_array(src, arr, idx);
    }
    fn visit_store_array_int64(&mut self, src: Register, arr: Register, idx: Register) {
        comment!(self, format!("StoreArrayInt64 {}, {}, {}", src, arr, idx));
        self.emit_store_array(src, arr, idx);
    }
    fn visit_store_array_float32(&mut self, src: Register, arr: Register, idx: Register) {
        comment!(self, format!("StoreArrayFloat32 {}, {}, {}", src, arr, idx));
        self.emit_store_array(src, arr, idx);
    }
    fn visit_store_array_float64(&mut self, src: Register, arr: Register, idx: Register) {
        comment!(self, format!("StoreArrayFloat64 {}, {}, {}", src, arr, idx));
        self.emit_store_array(src, arr, idx);
    }
    fn visit_store_array_ptr(&mut self, src: Register, arr: Register, idx: Register) {
        comment!(self, format!("StoreArrayPtr {}, {}, {}", src, arr, idx));
        self.emit_store_array(src, arr, idx);
    }
    fn visit_store_array_tuple(&mut self, src: Register, arr: Register, idx: Register) {
        comment!(self, format!("StoreArrayTuple {}, {}, {}", src, arr, idx));
        self.emit_store_array(src, arr, idx);
    }
    fn visit_store_array_generic(&mut self, src: Register, arr: Register, idx: Register) {
        comment!(self, format!("StoreArrayGeneric {}, {}, {}", src, arr, idx));
        self.emit_store_array(src, arr, idx);
    }

    fn visit_ret_void(&mut self) {
        comment!(self, format!("RetVoid"));
        self.emit_epilog();
    }
    fn visit_ret(&mut self, opnd: Register) {
        comment!(self, format!("Ret {}", opnd));
        self.emit_return_generic(opnd);
    }
}

fn result_reg(vm: &VM, bytecode_type: BytecodeType) -> AnyReg {
    if bytecode_type.mode(vm).is_float() {
        FREG_RESULT.into()
    } else {
        REG_RESULT.into()
    }
}

fn result_reg_mode(mode: MachineMode) -> AnyReg {
    if mode.is_float() {
        FREG_RESULT.into()
    } else {
        REG_RESULT.into()
    }
}

fn ensure_jit_or_stub_ptr<'ast>(src: &FctSrc, vm: &VM, type_params: TypeList) -> Address {
    let specials = src.specializations.read();

    if let Some(&jit_fct_id) = specials.get(&type_params) {
        let jit_fct = vm.jit_fcts.idx(jit_fct_id);
        return jit_fct.instruction_start();
    }

    vm.compile_stub()
}

enum RegOrOffset {
    Reg(Reg),
    RegWithOffset(Reg, i32),
    Offset(i32),
}

fn result_address_offset() -> i32 {
    -mem::ptr_width()
}
