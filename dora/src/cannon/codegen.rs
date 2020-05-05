use dora_parser::ast::*;
use std::collections::hash_map::HashMap;

use crate::bytecode::{
    self, BytecodeFunction, BytecodeOffset, BytecodeType, BytecodeVisitor, ConstPoolIdx, Register,
};
use crate::compiler::asm::BaselineAssembler;
use crate::compiler::codegen::{ensure_native_stub, should_emit_debug, AllocationSize, AnyReg};
use crate::compiler::fct::{Code, GcPoint, JitDescriptor};
use crate::compiler::native_stub::{NativeFct, NativeFctDescriptor};
use crate::cpu::{
    Mem, Reg, FREG_PARAMS, FREG_RESULT, FREG_TMP1, REG_PARAMS, REG_RESULT, REG_SP, REG_TMP1,
    REG_TMP2, STACK_FRAME_ALIGNMENT,
};
use crate::gc::Address;
use crate::masm::*;
use crate::mem;
use crate::object::{offset_of_array_data, Header, Str};
use crate::semck::specialize::specialize_type;
use crate::size::InstanceSize;
use crate::ty::{BuiltinType, MachineMode, TypeList};
use crate::vm::{
    ClassDefId, Fct, FctDef, FctDefId, FctId, FctKind, FctSrc, FieldId, GlobalId, Intrinsic, Trap,
    VM,
};
use crate::vtable::{VTable, DISPLAY_SIZE};

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

    lbl_break: Option<Label>,
    lbl_continue: Option<Label>,

    // stores all active finally blocks
    active_finallys: Vec<&'ast Stmt>,

    // label to jump instead of emitting epilog for return
    // needed for return's in finally blocks
    // return in finally needs to execute to next finally block and not
    // leave the current function
    lbl_return: Option<Label>,

    // length of active_finallys in last loop
    // default: 0
    // break/continue need to emit finally blocks up to the last loop
    // see tests/finally/break-while.dora
    active_loop: Option<usize>,

    // upper length of active_finallys in emitting finally-blocks for break/continue
    // default: active_finallys.len()
    // break/continue needs to execute finally-blocks in loop, return in these blocks
    // would dump all active_finally-entries from the loop but we need an upper bound.
    // see emit_finallys_within_loop and tests/finally/continue-return.dora
    active_upper: Option<usize>,

    cls_type_params: &'a TypeList,
    fct_type_params: &'a TypeList,

    offset_to_address: HashMap<BytecodeOffset, usize>,

    forward_jumps: Vec<ForwardJump>,
    current_offset: BytecodeOffset,
    argument_stack: Vec<Register>,

    references: Vec<i32>,
}

impl<'a, 'ast> CannonCodeGen<'a, 'ast>
where
    'ast: 'a,
{
    pub fn new(
        vm: &'a VM<'ast>,
        fct: &'a Fct<'ast>,
        ast: &'ast Function,
        asm: BaselineAssembler<'a, 'ast>,
        src: &'a FctSrc,
        bytecode: &'a BytecodeFunction,
        lbl_break: Option<Label>,
        lbl_continue: Option<Label>,
        active_finallys: Vec<&'ast Stmt>,
        lbl_return: Option<Label>,
        active_loop: Option<usize>,
        active_upper: Option<usize>,
        cls_type_params: &'a TypeList,
        fct_type_params: &'a TypeList,
    ) -> CannonCodeGen<'a, 'ast> {
        CannonCodeGen {
            vm,
            fct,
            ast,
            asm,
            src,
            bytecode,
            lbl_break,
            lbl_continue,
            active_finallys,
            active_upper,
            active_loop,
            lbl_return,
            cls_type_params,
            fct_type_params,
            offset_to_address: HashMap::new(),
            forward_jumps: Vec::new(),
            current_offset: BytecodeOffset(0),
            argument_stack: Vec::new(),
            references: Vec::new(),
        }
    }

    pub fn generate(mut self) -> Code {
        if should_emit_debug(self.vm, self.fct) {
            self.asm.debug();
        }

        self.emit_prolog();
        self.store_params_on_stack();
        self.emit_stack_guard();

        bytecode::read(self.bytecode.code(), &mut self);

        self.resolve_forward_jumps();

        let jit_fct = self.asm.jit(
            self.bytecode.stacksize(),
            JitDescriptor::DoraFct(self.fct.id),
        );

        jit_fct
    }

    fn store_params_on_stack(&mut self) {
        let mut reg_idx = 0;
        let mut freg_idx = 0;
        let mut sp_offset = 16;
        let mut idx = 0;

        for &param_ty in self.fct.params_with_self() {
            let param_ty = self.specialize_type(param_ty);

            if param_ty.is_unit() {
                continue;
            }

            let dest = Register(idx);
            assert_eq!(self.bytecode.register_type(dest), param_ty.into());

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

            idx += 1;
        }
    }

    fn emit_prolog(&mut self) {
        self.asm.prolog_size(self.bytecode.stacksize());
    }

    fn emit_stack_guard(&mut self) {
        let gcpoint = GcPoint::from_offsets(self.references.clone());
        self.asm.stack_guard(self.fct.ast.pos, gcpoint);
    }

    fn emit_epilog(&mut self) {
        self.asm.emit_comment("epilog".into());
        self.asm.epilog();
    }

    fn emit_load_register(&mut self, src: Register, dest: AnyReg) {
        let bytecode_type = self.bytecode.register_type(src);
        let offset = self.determine_register_offset(src);
        self.asm
            .load_mem(bytecode_type.mode(), dest, Mem::Local(offset));
    }

    fn emit_store_register(&mut self, src: AnyReg, dest: Register) {
        let bytecode_type = self.bytecode.register_type(dest);
        let offset = self.determine_register_offset(dest);
        self.asm
            .store_mem(bytecode_type.mode(), Mem::Local(offset), src);
    }

    fn determine_register_offset(&mut self, reg: Register) -> i32 {
        self.bytecode.register_offset(reg)
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
        self.asm
            .int_add(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

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
        self.asm
            .float_add(bytecode_type.mode(), FREG_RESULT, FREG_RESULT, FREG_TMP1);

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
        self.asm
            .int_sub(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

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
        self.asm
            .float_sub(bytecode_type.mode(), FREG_RESULT, FREG_RESULT, FREG_TMP1);

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
            .int_neg(bytecode_type.mode(), REG_RESULT, REG_RESULT);

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
            .float_neg(bytecode_type.mode(), FREG_RESULT, FREG_RESULT);

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
        self.asm
            .int_mul(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

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
        self.asm
            .float_mul(bytecode_type.mode(), FREG_RESULT, FREG_RESULT, FREG_TMP1);

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
            bytecode_type.mode(),
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
        self.asm
            .float_div(bytecode_type.mode(), FREG_RESULT, FREG_RESULT, FREG_TMP1);

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
            bytecode_type.mode(),
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
        self.asm
            .int_and(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

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
        self.asm
            .int_or(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

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
        self.asm
            .int_xor(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

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
            .int_not(bytecode_type.mode(), REG_RESULT, REG_RESULT);

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_shl_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(BytecodeType::Int, self.bytecode.register_type(rhs));
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(lhs, REG_RESULT.into());

        self.emit_load_register(rhs, REG_TMP1.into());

        let bytecode_type = self.bytecode.register_type(dest);
        self.asm
            .int_shl(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_shr_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(BytecodeType::Int, self.bytecode.register_type(rhs));
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(lhs, REG_RESULT.into());

        self.emit_load_register(rhs, REG_TMP1.into());

        let bytecode_type = self.bytecode.register_type(dest);
        self.asm
            .int_shr(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_sar_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(BytecodeType::Int, self.bytecode.register_type(rhs));
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(lhs, REG_RESULT.into());

        self.emit_load_register(rhs, REG_TMP1.into());

        let bytecode_type = self.bytecode.register_type(dest);
        self.asm
            .int_sar(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_rol_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(BytecodeType::Int, self.bytecode.register_type(rhs));
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(lhs, REG_RESULT.into());

        self.emit_load_register(rhs, REG_TMP1.into());

        let bytecode_type = self.bytecode.register_type(dest);
        self.asm
            .int_rol(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_ror_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        assert_eq!(BytecodeType::Int, self.bytecode.register_type(rhs));
        assert_eq!(
            self.bytecode.register_type(lhs),
            self.bytecode.register_type(dest)
        );

        self.emit_load_register(lhs, REG_RESULT.into());

        self.emit_load_register(rhs, REG_TMP1.into());

        let bytecode_type = self.bytecode.register_type(dest);
        self.asm
            .int_ror(bytecode_type.mode(), REG_RESULT, REG_RESULT, REG_TMP1);

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_reinterpret(&mut self, dest: Register, src: Register) {
        assert_ne!(
            self.bytecode.register_type(dest),
            self.bytecode.register_type(src)
        );

        let src_type = self.bytecode.register_type(src);
        let src_register = result_reg(src_type);
        self.emit_load_register(src, src_register.into());

        let dest_type = self.bytecode.register_type(dest);
        let dest_register = result_reg(dest_type);

        match dest_type {
            BytecodeType::Int => {
                assert_eq!(src_type, BytecodeType::Float);
                self.asm.float_as_int(
                    MachineMode::Int32,
                    dest_register.reg(),
                    MachineMode::Float32,
                    src_register.freg(),
                );
            }
            BytecodeType::Float => {
                assert_eq!(src_type, BytecodeType::Int);
                self.asm.int_as_float(
                    MachineMode::Float32,
                    dest_register.freg(),
                    MachineMode::Int32,
                    src_register.reg(),
                );
            }
            BytecodeType::Int64 => {
                assert_eq!(src_type, BytecodeType::Double);
                self.asm.float_as_int(
                    MachineMode::Int64,
                    dest_register.reg(),
                    MachineMode::Float64,
                    src_register.freg(),
                );
            }
            BytecodeType::Double => {
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
        assert_eq!(self.bytecode.register_type(src), BytecodeType::Int);

        self.emit_load_register(src, REG_RESULT.into());
        self.asm.extend_int_long(REG_RESULT, REG_RESULT);

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_int64_to_int(&mut self, dest: Register, src: Register) {
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Int);
        assert_eq!(self.bytecode.register_type(src), BytecodeType::Int64);

        self.emit_load_register(src, REG_RESULT.into());

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_int_to_float(&mut self, dest: Register, src: Register) {
        let src_type = self.bytecode.register_type(src);
        let dest_type = self.bytecode.register_type(dest);

        let (dest_mode, src_mode) = match (dest_type, src_type) {
            (BytecodeType::Float, BytecodeType::Int) => (MachineMode::Float32, MachineMode::Int32),
            (BytecodeType::Float, BytecodeType::Int64) => {
                (MachineMode::Float32, MachineMode::Int64)
            }
            (BytecodeType::Double, BytecodeType::Int) => (MachineMode::Float64, MachineMode::Int32),
            (BytecodeType::Double, BytecodeType::Int64) => {
                (MachineMode::Float64, MachineMode::Int64)
            }
            _ => unreachable!(),
        };

        self.emit_load_register(src, REG_RESULT.into());

        self.asm
            .int_to_float(dest_mode, FREG_RESULT, src_mode, REG_RESULT);

        self.emit_store_register(FREG_RESULT.into(), dest);
    }

    fn emit_float_to_int(&mut self, dest: Register, src: Register) {
        let src_type = self.bytecode.register_type(src);
        let dest_type = self.bytecode.register_type(dest);

        let (dest_mode, src_mode) = match (dest_type, src_type) {
            (BytecodeType::Int, BytecodeType::Float) => (MachineMode::Int32, MachineMode::Float32),
            (BytecodeType::Int, BytecodeType::Double) => (MachineMode::Int32, MachineMode::Float64),
            (BytecodeType::Int64, BytecodeType::Float) => {
                (MachineMode::Int64, MachineMode::Float32)
            }
            (BytecodeType::Int64, BytecodeType::Double) => {
                (MachineMode::Int64, MachineMode::Float64)
            }
            _ => unreachable!(),
        };

        self.emit_load_register(src, FREG_RESULT.into());

        self.asm
            .float_to_int(dest_mode, REG_RESULT, src_mode, FREG_RESULT);

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_instanceof(
        &mut self,
        dest: Register,
        src: Register,
        cls_id: ClassDefId,
        instanceof: bool,
    ) {
        let cls = self.vm.class_defs.idx(cls_id);
        let cls = cls.read();

        let vtable: &VTable = cls.vtable.as_ref().unwrap();
        let position = self.bytecode.offset_position(self.current_offset.to_u32());

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
                self.asm.emit_bailout_inplace(Trap::CAST, position);
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
                self.asm.emit_bailout(lbl_bailout, Trap::CAST, position);
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

        let bytecode_type = self.bytecode.register_type(src);
        let reg = result_reg(bytecode_type);

        self.emit_load_register(src, reg.into());

        self.emit_store_register(reg.into(), dest);
    }

    fn emit_load_field(
        &mut self,
        dest: Register,
        obj: Register,
        class_def_id: ClassDefId,
        field_id: FieldId,
    ) {
        assert_eq!(self.bytecode.register_type(obj), BytecodeType::Ptr);

        let cls = self.vm.class_defs.idx(class_def_id);
        let cls = cls.read();
        let field = &cls.fields[field_id.idx()];

        assert_eq!(self.bytecode.register_type(dest), field.ty.into());

        {
            let cname = cls.name(self.vm);

            let cls_id = cls.cls_id.expect("no corresponding class");
            let class = self.vm.classes.idx(cls_id);
            let class = class.read();
            let field = &class.fields[field_id.idx()];
            let fname = self.vm.interner.str(field.name);

            self.asm
                .emit_comment(format!("load field {}.{}", cname, fname));
        }

        assert!(self.bytecode.register_type(obj).is_ptr());

        let obj_reg = REG_RESULT;
        self.emit_load_register(obj, obj_reg.into());

        let bytecode_type = self.bytecode.register_type(dest);

        let dest_reg = result_reg(bytecode_type);
        let pos = self.bytecode.offset_position(self.current_offset.to_u32());

        self.asm.test_if_nil_bailout(pos, obj_reg, Trap::NIL);
        self.asm
            .load_mem(field.ty.mode(), dest_reg, Mem::Base(obj_reg, field.offset));

        self.emit_store_register(dest_reg.into(), dest)
    }

    fn emit_store_field(
        &mut self,
        src: Register,
        obj: Register,
        class_def_id: ClassDefId,
        field_id: FieldId,
    ) {
        assert_eq!(self.bytecode.register_type(obj), BytecodeType::Ptr);

        let cls = self.vm.class_defs.idx(class_def_id);
        let cls = cls.read();
        let field = &cls.fields[field_id.idx()];

        assert_eq!(self.bytecode.register_type(src), field.ty.into());

        {
            let cname = cls.name(self.vm);

            let cls_id = cls.cls_id.expect("no corresponding class");
            let class = self.vm.classes.idx(cls_id);
            let class = class.read();
            let field = &class.fields[field_id.idx()];
            let fname = self.vm.interner.str(field.name);

            self.asm
                .emit_comment(format!("store field {}.{}", cname, fname));
        }

        let bytecode_type = self.bytecode.register_type(src);

        let value = result_reg(bytecode_type);

        self.emit_load_register(src, value.into());

        assert!(self.bytecode.register_type(obj).is_ptr());

        let obj_reg = REG_TMP1;
        self.emit_load_register(obj, obj_reg.into());

        let write_barrier = self.vm.gc.needs_write_barrier() && field.ty.reference_type();
        let card_table_offset = self.vm.gc.card_table_offset();
        let pos = self.bytecode.offset_position(self.current_offset.to_u32());

        self.asm.test_if_nil_bailout(pos, obj_reg, Trap::NIL);
        self.asm
            .store_mem(field.ty.mode(), Mem::Base(obj_reg, field.offset), value);

        if write_barrier {
            self.asm.emit_barrier(obj_reg, card_table_offset);
        }
    }

    fn emit_load_global(&mut self, dest: Register, global_id: GlobalId) {
        let glob = self.vm.globals.idx(global_id);
        let glob = glob.read();

        assert_eq!(self.bytecode.register_type(dest), glob.ty.into());

        let disp = self.asm.add_addr(glob.address_value.to_ptr());
        let pos = self.asm.pos() as i32;

        let name = self.vm.interner.str(glob.name);
        self.asm.emit_comment(format!("load global {}", name));
        self.asm.load_constpool(REG_TMP1, disp + pos);

        let bytecode_type = self.bytecode.register_type(dest);

        let reg = result_reg(bytecode_type);

        self.asm
            .load_mem(glob.ty.mode(), reg, Mem::Base(REG_TMP1, 0));

        self.emit_store_register(reg, dest);
    }

    fn emit_store_global(&mut self, src: Register, global_id: GlobalId) {
        let glob = self.vm.globals.idx(global_id);
        let glob = glob.read();

        assert_eq!(self.bytecode.register_type(src), glob.ty.into());

        let disp = self.asm.add_addr(glob.address_value.to_ptr());
        let pos = self.asm.pos() as i32;

        let name = self.vm.interner.str(glob.name);
        self.asm.emit_comment(format!("store global {}", name));
        self.asm.load_constpool(REG_TMP1, disp + pos);

        let bytecode_type = self.bytecode.register_type(src);

        let reg = result_reg(bytecode_type);

        self.emit_load_register(src, reg);

        self.asm
            .store_mem(glob.ty.mode(), Mem::Base(REG_TMP1, 0), reg);
    }

    fn emit_const_nil(&mut self, dest: Register) {
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Ptr);

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
        assert!(
            self.bytecode.register_type(dest) == BytecodeType::Char
                || self.bytecode.register_type(dest) == BytecodeType::UInt8
                || self.bytecode.register_type(dest) == BytecodeType::Int
                || self.bytecode.register_type(dest) == BytecodeType::Int64
        );

        let bytecode_type = self.bytecode.register_type(dest);

        self.asm
            .load_int_const(bytecode_type.mode(), REG_RESULT, int_const);

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_const_float(&mut self, dest: Register, float_const: f64) {
        assert!(
            self.bytecode.register_type(dest) == BytecodeType::Float
                || self.bytecode.register_type(dest) == BytecodeType::Double
        );

        let bytecode_type = self.bytecode.register_type(dest);

        self.asm
            .load_float_const(bytecode_type.mode(), FREG_RESULT, float_const);

        self.emit_store_register(FREG_RESULT.into(), dest);
    }

    fn emit_const_string(&mut self, dest: Register, lit_value: &str) {
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Ptr);

        let handle = Str::from_buffer_in_perm(self.vm, lit_value.as_bytes());
        let disp = self.asm.add_addr(handle.raw() as *const u8);
        let pos = self.asm.pos() as i32;

        self.asm
            .emit_comment(format!("load string '{}'", lit_value));

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

        self.asm.cmp_reg(bytecode_type.mode(), REG_RESULT, REG_TMP1);
        self.asm.set(REG_RESULT, op);

        self.emit_store_register(REG_RESULT.into(), dest);
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

        self.asm
            .float_cmp(bytecode_type.mode(), REG_RESULT, FREG_RESULT, FREG_TMP1, op);

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
        let bytecode_type = self.bytecode.register_type(src);

        let reg = result_reg(bytecode_type);

        self.emit_load_register(src, reg.into());

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

    fn emit_new_object(&mut self, dest: Register, class_def_id: ClassDefId) {
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Ptr);

        let cls = self.vm.class_defs.idx(class_def_id);
        let cls = cls.read();

        {
            let name = cls.name(self.vm);
            self.asm
                .emit_comment(format!("allocate object of class {}", name));
        }

        let alloc_size = match cls.size {
            InstanceSize::Fixed(size) => AllocationSize::Fixed(size as usize),
            _ => unreachable!(
                "class size type {:?} for new object not supported",
                cls.size
            ),
        };

        let gcpoint = GcPoint::from_offsets(self.references.clone());
        let position = self.bytecode.offset_position(self.current_offset.to_u32());
        self.asm
            .allocate(REG_RESULT.into(), alloc_size, position, false, gcpoint);

        // store gc object in temporary storage
        self.emit_store_register(REG_RESULT.into(), dest);

        // store classptr in object
        let cptr = (&**cls.vtable.as_ref().unwrap()) as *const VTable as *const u8;
        let disp = self.asm.add_addr(cptr);
        let pos = self.asm.pos() as i32;

        let name = cls.name(self.vm);
        self.asm
            .emit_comment(format!("store vtable ptr for class {} in object", name));
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

        let ref_offset = self.determine_register_offset(dest);
        self.references.push(ref_offset);
    }

    fn emit_new_array(&mut self, dest: Register, class_def_id: ClassDefId, length: Register) {
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Ptr);
        assert_eq!(self.bytecode.register_type(length), BytecodeType::Int);

        let cls = self.vm.class_defs.idx(class_def_id);
        let cls = cls.read();

        {
            let name = cls.name(self.vm);
            self.asm
                .emit_comment(format!("allocate array of type {}", name));
        }

        self.emit_load_register(length, REG_TMP1.into());

        let array_header_size = Header::size() as usize + mem::ptr_width_usize();

        let alloc_size = match cls.size {
            InstanceSize::PrimitiveArray(size) => {
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

        let gcpoint = GcPoint::from_offsets(self.references.clone());
        let position = self.bytecode.offset_position(self.current_offset.to_u32());
        self.asm
            .allocate(REG_RESULT.into(), alloc_size, position, array_ref, gcpoint);

        // store gc object in temporary storage
        self.emit_store_register(REG_RESULT.into(), dest);

        // store classptr in object
        let cptr = (&**cls.vtable.as_ref().unwrap()) as *const VTable as *const u8;
        let disp = self.asm.add_addr(cptr);
        let pos = self.asm.pos() as i32;

        let name = cls.name(self.vm);
        self.asm
            .emit_comment(format!("store vtable ptr for class {} in object", name));
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
            InstanceSize::PrimitiveArray(size) => {
                self.emit_array_initialization(REG_RESULT, REG_TMP1, size);
            }
            InstanceSize::ObjArray => {
                self.emit_array_initialization(REG_RESULT, REG_TMP1, mem::ptr_width());
            }
            InstanceSize::UnitArray => {}
            _ => unreachable!(),
        }

        let ref_offset = self.determine_register_offset(dest);
        self.references.push(ref_offset);
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

    fn emit_nil_check(&mut self, obj: Register) {
        assert_eq!(self.bytecode.register_type(obj), BytecodeType::Ptr);

        let position = self.bytecode.offset_position(self.current_offset.to_u32());

        self.emit_load_register(obj, REG_RESULT.into());
        self.asm
            .test_if_nil_bailout(position, REG_RESULT, Trap::NIL);
    }

    fn emit_array_length(&mut self, dest: Register, arr: Register) {
        assert_eq!(self.bytecode.register_type(dest), BytecodeType::Int);
        assert_eq!(self.bytecode.register_type(arr), BytecodeType::Ptr);

        let position = self.bytecode.offset_position(self.current_offset.to_u32());

        self.emit_load_register(arr, REG_RESULT.into());
        self.asm
            .test_if_nil_bailout(position, REG_RESULT, Trap::NIL);

        self.asm.load_mem(
            MachineMode::Ptr,
            REG_RESULT.into(),
            Mem::Base(REG_RESULT, Header::size()),
        );

        self.emit_store_register(REG_RESULT.into(), dest);
    }

    fn emit_array_bound_check(&mut self, arr: Register, idx: Register) {
        assert_eq!(self.bytecode.register_type(arr), BytecodeType::Ptr);
        assert_eq!(self.bytecode.register_type(idx), BytecodeType::Int);

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
        assert_eq!(self.bytecode.register_type(idx), BytecodeType::Int);
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

        let src_type = self.bytecode.register_type(src);

        let value_reg: AnyReg = if src_type.mode().is_float() {
            FREG_RESULT.into()
        } else {
            REG_TMP2.into()
        };

        self.emit_load_register(src, value_reg.into());

        self.asm.store_mem(
            src_type.mode(),
            Mem::Index(
                REG_RESULT,
                REG_TMP1,
                src_type.mode().size(),
                offset_of_array_data(),
            ),
            value_reg,
        );

        if self.vm.gc.needs_write_barrier() && src_type.is_ptr() {
            let card_table_offset = self.vm.gc.card_table_offset();
            let scratch = self.asm.get_scratch();
            self.asm.lea(
                *scratch,
                Mem::Index(
                    REG_RESULT,
                    REG_TMP1,
                    src_type.mode().size(),
                    offset_of_array_data(),
                ),
            );
            self.asm.emit_barrier(*scratch, card_table_offset);
        }
    }

    fn emit_load_array(&mut self, dest: Register, arr: Register, idx: Register) {
        assert_eq!(self.bytecode.register_type(idx), BytecodeType::Int);
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

        let dest_type = self.bytecode.register_type(dest);

        let register = result_reg(dest_type);
        self.asm
            .load_array_elem(dest_type.mode(), register, REG_RESULT, REG_TMP1);
        self.emit_store_register(register, dest);
    }

    fn emit_invoke_virtual_void(&mut self, fct_id: FctDefId, num: u32) {
        self.emit_invoke_virtual(fct_id, num, None);
    }

    fn emit_invoke_virtual_generic(&mut self, dest: Register, fct_id: FctDefId, num: u32) {
        let bytecode_type = self.bytecode.register_type(dest);

        let reg = self.emit_invoke_virtual(fct_id, num, Some(bytecode_type));

        self.emit_store_register(reg, dest);
    }

    fn emit_invoke_virtual(
        &mut self,
        fct_def_id: FctDefId,
        num: u32,
        bytecode_type: Option<BytecodeType>,
    ) -> AnyReg {
        assert!(num > 0);

        assert_eq!(self.argument_stack.len() as u32, num);
        let arguments = self.argument_stack.drain(..).collect::<Vec<_>>();
        let self_register = arguments[0];

        let bytecode_type_self = self.bytecode.register_type(self_register);
        let position = self.bytecode.offset_position(self.current_offset.to_u32());
        assert_eq!(bytecode_type_self, BytecodeType::Ptr);

        self.emit_load_register(self_register, REG_RESULT.into());

        let fct_def = self.vm.fct_defs.idx(fct_def_id);
        let fct_def = fct_def.read();

        let fct_id = fct_def.fct_id;
        let fct = self.vm.fcts.idx(fct_id);
        let fct = fct.read();

        let argsize = self.emit_invoke_arguments(arguments, num);

        // handling of class and function type parameters has to implemented
        let cls_type_params = fct_def.cls_type_params.clone();

        let name = fct.full_name(self.vm);
        self.asm.emit_comment(format!("call virtual {}", name));
        let vtable_index = fct.vtable_index.unwrap();
        let gcpoint = GcPoint::from_offsets(self.references.clone());

        let (reg, ty) = match bytecode_type {
            Some(bytecode_type) => (result_reg(bytecode_type), bytecode_type.into()),
            None => (REG_RESULT.into(), BuiltinType::Unit),
        };

        self.asm
            .indirect_call(vtable_index, position, gcpoint, ty, cls_type_params, reg);

        self.asm.decrease_stack_frame(argsize);

        reg
    }

    fn emit_invoke_direct_void(&mut self, fct_def_id: FctDefId, num: u32) {
        self.emit_invoke_direct(fct_def_id, num, None);
    }

    fn emit_invoke_direct_generic(&mut self, dest: Register, fct_id: FctDefId, num: u32) {
        let bytecode_type = self.bytecode.register_type(dest);

        let reg = self.emit_invoke_direct(fct_id, num, Some(bytecode_type));

        self.emit_store_register(reg, dest);
    }

    fn emit_invoke_direct(
        &mut self,
        fct_def_id: FctDefId,
        num: u32,
        bytecode_type: Option<BytecodeType>,
    ) -> AnyReg {
        assert!(num > 0);

        assert_eq!(self.argument_stack.len() as u32, num);
        let arguments = self.argument_stack.drain(..).collect::<Vec<_>>();
        let self_register = arguments[0];

        let bytecode_type_self = self.bytecode.register_type(self_register);
        let position = self.bytecode.offset_position(self.current_offset.to_u32());
        assert_eq!(bytecode_type_self, BytecodeType::Ptr);

        self.emit_load_register(self_register, REG_RESULT.into());

        self.asm
            .test_if_nil_bailout(position, REG_RESULT.into(), Trap::NIL);

        let fct_def = self.vm.fct_defs.idx(fct_def_id);
        let fct_def = fct_def.read();

        let fct_id = fct_def.fct_id;
        let fct = self.vm.fcts.idx(fct_id);
        let fct = fct.read();

        assert!(fct.type_params.is_empty());

        let argsize = self.emit_invoke_arguments(arguments, num);

        let cls_type_params = fct_def.cls_type_params.clone();
        let fct_type_params = fct_def.fct_type_params.clone();

        let name = fct.full_name(self.vm);
        self.asm.emit_comment(format!("call direct {}", name));
        let ptr = self.ptr_for_fct_id(fct_id, cls_type_params.clone(), fct_type_params.clone());
        let gcpoint = GcPoint::from_offsets(self.references.clone());

        let (reg, ty) = match bytecode_type {
            Some(bytecode_type) => (result_reg(bytecode_type), bytecode_type.into()),
            None => (REG_RESULT.into(), BuiltinType::Unit),
        };
        self.asm.direct_call(
            fct_id,
            ptr.to_ptr(),
            cls_type_params,
            fct_type_params,
            position,
            gcpoint,
            ty,
            reg,
        );

        self.asm.decrease_stack_frame(argsize);

        reg
    }

    fn emit_invoke_static_void(&mut self, fct_id: FctDefId, num: u32) {
        self.emit_invoke_static(fct_id, num, None);
    }

    fn emit_invoke_static_generic(&mut self, dest: Register, fct_id: FctDefId, num: u32) {
        let bytecode_type = self.bytecode.register_type(dest);

        let reg = self.emit_invoke_static(fct_id, num, Some(bytecode_type));

        self.emit_store_register(reg, dest);
    }

    fn emit_invoke_static(
        &mut self,
        fct_def_id: FctDefId,
        num: u32,
        bytecode_type: Option<BytecodeType>,
    ) -> AnyReg {
        let fct_def = self.vm.fct_defs.idx(fct_def_id);
        let fct_def = fct_def.read();

        let fct_id = fct_def.fct_id;
        let fct = self.vm.fcts.idx(fct_id);
        let fct = fct.read();

        if let FctKind::Builtin(intrinsic) = fct.kind {
            return self.emit_invoke_intrinsic(&*fct, &*fct_def, intrinsic, num);
        }

        assert_eq!(self.argument_stack.len() as u32, num);
        let arguments = self.argument_stack.drain(..).collect::<Vec<_>>();

        let argsize = self.emit_invoke_arguments(arguments, num);

        let cls_type_params = fct_def.cls_type_params.clone();
        let fct_type_params = fct_def.fct_type_params.clone();

        let name = fct.full_name(self.vm);
        self.asm.emit_comment(format!("call direct {}", name));

        let ptr = self.ptr_for_fct_id(fct_id, cls_type_params.clone(), fct_type_params.clone());
        let gcpoint = GcPoint::from_offsets(self.references.clone());
        let position = self.bytecode.offset_position(self.current_offset.to_u32());

        let (reg, ty) = match bytecode_type {
            Some(bytecode_type) => (result_reg(bytecode_type), bytecode_type.into()),
            None => (REG_RESULT.into(), BuiltinType::Unit),
        };
        self.asm.direct_call(
            fct_id,
            ptr.to_ptr(),
            cls_type_params,
            fct_type_params,
            position,
            gcpoint,
            ty,
            reg,
        );

        self.asm.decrease_stack_frame(argsize);

        reg
    }

    fn emit_invoke_intrinsic(
        &mut self,
        _fct: &Fct,
        _fct_def: &FctDef,
        intrinsic: Intrinsic,
        num: u32,
    ) -> AnyReg {
        assert_eq!(self.argument_stack.len() as u32, num);
        let arguments = self.argument_stack.drain(..).collect::<Vec<_>>();

        match intrinsic {
            Intrinsic::FloatSqrt | Intrinsic::DoubleSqrt => {
                assert_eq!(num, 1);

                let mode = match intrinsic {
                    Intrinsic::FloatSqrt => MachineMode::Float32,
                    Intrinsic::DoubleSqrt => MachineMode::Float64,
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

            Intrinsic::Int64CountZeroBits
            | Intrinsic::Int64CountZeroBitsLeading
            | Intrinsic::Int64CountZeroBitsTrailing
            | Intrinsic::Int64CountOneBits
            | Intrinsic::Int64CountOneBitsLeading
            | Intrinsic::Int64CountOneBitsTrailing => {
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

            _ => unreachable!(),
        }
    }

    fn emit_invoke_arguments(&mut self, arguments: Vec<Register>, num: u32) -> i32 {
        if num == 0 {
            return 0;
        }

        let argsize = self.determine_argsize(&arguments);

        self.asm.increase_stack_frame(argsize);

        let mut reg_idx = 0;
        let mut freg_idx = 0;
        let mut sp_offset = 0;

        for src in arguments {
            let bytecode_type = self.bytecode.register_type(src);
            let offset = self.bytecode.register_offset(src);
            let mode = bytecode_type.mode();

            match bytecode_type {
                BytecodeType::Float | BytecodeType::Double => {
                    if freg_idx < FREG_PARAMS.len() {
                        self.asm.load_mem(
                            bytecode_type.mode(),
                            FREG_PARAMS[freg_idx].into(),
                            Mem::Local(offset),
                        );
                        freg_idx += 1;
                    } else {
                        self.asm
                            .load_mem(mode, FREG_TMP1.into(), Mem::Local(offset));
                        self.asm
                            .store_mem(mode, Mem::Base(REG_SP, sp_offset), FREG_TMP1.into());

                        sp_offset += 8;
                    }
                }
                _ => {
                    if reg_idx < REG_PARAMS.len() {
                        self.asm.load_mem(
                            bytecode_type.mode(),
                            REG_PARAMS[reg_idx].into(),
                            Mem::Local(offset),
                        );
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

        argsize
    }

    fn determine_argsize(&mut self, arguments: &Vec<Register>) -> i32 {
        let mut reg_idx = 0;
        let mut freg_idx = 0;
        let mut argsize = 0;

        for &src in arguments {
            let bytecode_type = self.bytecode.register_type(src);

            match bytecode_type {
                BytecodeType::Float | BytecodeType::Double => {
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

    fn ptr_for_fct_id(
        &mut self,
        fid: FctId,
        cls_type_params: TypeList,
        fct_type_params: TypeList,
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
                    let src = src.read();

                    ensure_jit_or_stub_ptr(&src, self.vm, cls_type_params, fct_type_params)
                }

                FctKind::Native(ptr) => {
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
        specialize_type(self.vm, ty, self.cls_type_params, self.fct_type_params)
    }
}

impl<'a, 'ast: 'a> BytecodeVisitor for CannonCodeGen<'a, 'ast> {
    fn visit_instruction(&mut self, offset: BytecodeOffset) {
        self.offset_to_address.insert(offset, self.asm.pos());
        self.current_offset = offset;
    }

    fn visit_add_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_add_int(dest, lhs, rhs);
    }
    fn visit_add_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_add_int(dest, lhs, rhs);
    }
    fn visit_add_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_add_int(dest, lhs, rhs);
    }
    fn visit_add_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_add_float(dest, lhs, rhs);
    }
    fn visit_add_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_add_float(dest, lhs, rhs);
    }

    fn visit_sub_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_sub_int(dest, lhs, rhs);
    }
    fn visit_sub_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_sub_int(dest, lhs, rhs);
    }
    fn visit_sub_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_sub_int(dest, lhs, rhs);
    }
    fn visit_sub_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_sub_float(dest, lhs, rhs);
    }
    fn visit_sub_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_sub_float(dest, lhs, rhs);
    }

    fn visit_neg_int(&mut self, dest: Register, src: Register) {
        self.emit_neg_int(dest, src);
    }
    fn visit_neg_int32(&mut self, dest: Register, src: Register) {
        self.emit_neg_int(dest, src);
    }
    fn visit_neg_int64(&mut self, dest: Register, src: Register) {
        self.emit_neg_int(dest, src);
    }
    fn visit_neg_float(&mut self, dest: Register, src: Register) {
        self.emit_neg_float(dest, src);
    }
    fn visit_neg_double(&mut self, dest: Register, src: Register) {
        self.emit_neg_float(dest, src);
    }

    fn visit_mul_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_mul_int(dest, lhs, rhs);
    }
    fn visit_mul_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_mul_int(dest, lhs, rhs);
    }
    fn visit_mul_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_mul_int(dest, lhs, rhs);
    }
    fn visit_mul_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_mul_float(dest, lhs, rhs);
    }
    fn visit_mul_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_mul_float(dest, lhs, rhs);
    }

    fn visit_div_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_div_int(dest, lhs, rhs);
    }
    fn visit_div_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_div_int(dest, lhs, rhs);
    }
    fn visit_div_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_div_int(dest, lhs, rhs);
    }
    fn visit_div_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_div_float(dest, lhs, rhs);
    }
    fn visit_div_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_div_float(dest, lhs, rhs);
    }

    fn visit_mod_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_mod_int(dest, lhs, rhs);
    }
    fn visit_mod_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_mod_int(dest, lhs, rhs);
    }
    fn visit_mod_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_mod_int(dest, lhs, rhs);
    }

    fn visit_and_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_and_int(dest, lhs, rhs);
    }
    fn visit_and_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_and_int(dest, lhs, rhs);
    }
    fn visit_and_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_and_int(dest, lhs, rhs);
    }

    fn visit_or_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_or_int(dest, lhs, rhs)
    }
    fn visit_or_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_or_int(dest, lhs, rhs)
    }
    fn visit_or_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_or_int(dest, lhs, rhs)
    }

    fn visit_xor_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_xor_int(dest, lhs, rhs);
    }
    fn visit_xor_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_xor_int(dest, lhs, rhs);
    }
    fn visit_xor_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_xor_int(dest, lhs, rhs);
    }

    fn visit_not_bool(&mut self, dest: Register, src: Register) {
        self.emit_not_bool(dest, src);
    }
    fn visit_not_int(&mut self, dest: Register, src: Register) {
        self.emit_not_int(dest, src);
    }
    fn visit_not_int32(&mut self, dest: Register, src: Register) {
        self.emit_not_int(dest, src);
    }
    fn visit_not_int64(&mut self, dest: Register, src: Register) {
        self.emit_not_int(dest, src);
    }

    fn visit_shl_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_shl_int(dest, lhs, rhs);
    }
    fn visit_shr_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_shr_int(dest, lhs, rhs);
    }
    fn visit_sar_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_sar_int(dest, lhs, rhs);
    }

    fn visit_shl_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_shl_int(dest, lhs, rhs);
    }
    fn visit_shr_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_shr_int(dest, lhs, rhs);
    }
    fn visit_sar_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_sar_int(dest, lhs, rhs);
    }

    fn visit_shl_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_shl_int(dest, lhs, rhs);
    }
    fn visit_shr_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_shr_int(dest, lhs, rhs);
    }
    fn visit_sar_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_sar_int(dest, lhs, rhs);
    }

    fn visit_rol_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_rol_int(dest, lhs, rhs);
    }
    fn visit_ror_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_ror_int(dest, lhs, rhs);
    }

    fn visit_rol_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_rol_int(dest, lhs, rhs);
    }
    fn visit_ror_int32(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_ror_int(dest, lhs, rhs);
    }

    fn visit_rol_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_rol_int(dest, lhs, rhs);
    }
    fn visit_ror_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_ror_int(dest, lhs, rhs);
    }

    fn visit_reinterpret_float_as_int(&mut self, dest: Register, src: Register) {
        self.emit_reinterpret(dest, src);
    }
    fn visit_reinterpret_int_as_float(&mut self, dest: Register, src: Register) {
        self.emit_reinterpret(dest, src);
    }
    fn visit_reinterpret_double_as_int64(&mut self, dest: Register, src: Register) {
        self.emit_reinterpret(dest, src);
    }
    fn visit_reinterpret_int64_as_double(&mut self, dest: Register, src: Register) {
        self.emit_reinterpret(dest, src);
    }

    fn visit_extend_byte_to_char(&mut self, dest: Register, src: Register) {
        self.emit_extend_byte(dest, src, MachineMode::Int32);
    }
    fn visit_extend_byte_to_int(&mut self, dest: Register, src: Register) {
        self.emit_extend_byte(dest, src, MachineMode::Int32);
    }
    fn visit_extend_byte_to_int64(&mut self, dest: Register, src: Register) {
        self.emit_extend_byte(dest, src, MachineMode::Int64);
    }
    fn visit_extend_int_to_int64(&mut self, dest: Register, src: Register) {
        self.emit_int_to_int64(dest, src);
    }
    fn visit_extend_char_to_int64(&mut self, dest: Register, src: Register) {
        self.emit_shrink(dest, MachineMode::Int64, src, MachineMode::Int32);
    }
    fn visit_cast_char_to_int(&mut self, dest: Register, src: Register) {
        self.emit_shrink(dest, MachineMode::Int32, src, MachineMode::Int32);
    }
    fn visit_cast_int_to_uint8(&mut self, dest: Register, src: Register) {
        self.emit_shrink(dest, MachineMode::Int8, src, MachineMode::Int32);
    }
    fn visit_cast_int_to_char(&mut self, dest: Register, src: Register) {
        self.emit_shrink(dest, MachineMode::Int32, src, MachineMode::Int32);
    }
    fn visit_cast_int_to_int32(&mut self, dest: Register, src: Register) {
        self.emit_shrink(dest, MachineMode::Int32, src, MachineMode::Int32);
    }
    fn visit_cast_int64_to_uint8(&mut self, dest: Register, src: Register) {
        self.emit_shrink(dest, MachineMode::Int8, src, MachineMode::Int64);
    }
    fn visit_cast_int64_to_char(&mut self, dest: Register, src: Register) {
        self.emit_shrink(dest, MachineMode::Int32, src, MachineMode::Int64);
    }
    fn visit_cast_int64_to_int(&mut self, dest: Register, src: Register) {
        self.emit_int64_to_int(dest, src);
    }

    fn visit_convert_int_to_float(&mut self, dest: Register, src: Register) {
        self.emit_int_to_float(dest, src);
    }
    fn visit_convert_int_to_double(&mut self, dest: Register, src: Register) {
        self.emit_int_to_float(dest, src);
    }
    fn visit_convert_int64_to_float(&mut self, dest: Register, src: Register) {
        self.emit_int_to_float(dest, src);
    }
    fn visit_convert_int64_to_double(&mut self, dest: Register, src: Register) {
        self.emit_int_to_float(dest, src);
    }

    fn visit_truncate_float_to_int(&mut self, dest: Register, src: Register) {
        self.emit_float_to_int(dest, src);
    }
    fn visit_truncate_float_to_int64(&mut self, dest: Register, src: Register) {
        self.emit_float_to_int(dest, src);
    }
    fn visit_truncate_double_to_int(&mut self, dest: Register, src: Register) {
        self.emit_float_to_int(dest, src);
    }
    fn visit_truncate_double_to_int64(&mut self, dest: Register, src: Register) {
        self.emit_float_to_int(dest, src);
    }

    fn visit_instance_of(&mut self, dest: Register, src: Register, cls_id: ClassDefId) {
        self.emit_instanceof(dest, src, cls_id, true);
    }
    fn visit_checked_cast(&mut self, src: Register, cls_id: ClassDefId) {
        self.emit_instanceof(Register::invalid(), src, cls_id, false);
    }

    fn visit_mov_bool(&mut self, dest: Register, src: Register) {
        self.emit_mov_generic(dest, src);
    }
    fn visit_mov_uint8(&mut self, dest: Register, src: Register) {
        self.emit_mov_generic(dest, src);
    }
    fn visit_mov_char(&mut self, dest: Register, src: Register) {
        self.emit_mov_generic(dest, src);
    }
    fn visit_mov_int(&mut self, dest: Register, src: Register) {
        self.emit_mov_generic(dest, src);
    }
    fn visit_mov_int64(&mut self, dest: Register, src: Register) {
        self.emit_mov_generic(dest, src);
    }
    fn visit_mov_float(&mut self, dest: Register, src: Register) {
        self.emit_mov_generic(dest, src);
    }
    fn visit_mov_double(&mut self, dest: Register, src: Register) {
        self.emit_mov_generic(dest, src);
    }
    fn visit_mov_ptr(&mut self, dest: Register, src: Register) {
        self.emit_mov_generic(dest, src);
    }

    fn visit_load_field_bool(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_load_field(dest, obj, cls, field);
    }
    fn visit_load_field_uint8(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_load_field(dest, obj, cls, field);
    }
    fn visit_load_field_char(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_load_field(dest, obj, cls, field);
    }
    fn visit_load_field_int(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_load_field(dest, obj, cls, field);
    }
    fn visit_load_field_int64(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_load_field(dest, obj, cls, field);
    }
    fn visit_load_field_float(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_load_field(dest, obj, cls, field);
    }
    fn visit_load_field_double(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_load_field(dest, obj, cls, field);
    }
    fn visit_load_field_ptr(
        &mut self,
        dest: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_load_field(dest, obj, cls, field);
    }

    fn visit_store_field_bool(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_store_field(src, obj, cls, field);
    }
    fn visit_store_field_uint8(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_store_field(src, obj, cls, field);
    }
    fn visit_store_field_char(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_store_field(src, obj, cls, field);
    }
    fn visit_store_field_int(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_store_field(src, obj, cls, field);
    }
    fn visit_store_field_int64(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_store_field(src, obj, cls, field);
    }
    fn visit_store_field_float(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_store_field(src, obj, cls, field);
    }
    fn visit_store_field_double(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_store_field(src, obj, cls, field);
    }
    fn visit_store_field_ptr(
        &mut self,
        src: Register,
        obj: Register,
        cls: ClassDefId,
        field: FieldId,
    ) {
        self.emit_store_field(src, obj, cls, field);
    }

    fn visit_load_global_bool(&mut self, dest: Register, glob: GlobalId) {
        self.emit_load_global(dest, glob);
    }
    fn visit_load_global_uint8(&mut self, dest: Register, glob: GlobalId) {
        self.emit_load_global(dest, glob);
    }
    fn visit_load_global_char(&mut self, dest: Register, glob: GlobalId) {
        self.emit_load_global(dest, glob);
    }
    fn visit_load_global_int(&mut self, dest: Register, glob: GlobalId) {
        self.emit_load_global(dest, glob);
    }
    fn visit_load_global_int64(&mut self, dest: Register, glob: GlobalId) {
        self.emit_load_global(dest, glob);
    }
    fn visit_load_global_float(&mut self, dest: Register, glob: GlobalId) {
        self.emit_load_global(dest, glob);
    }
    fn visit_load_global_double(&mut self, dest: Register, glob: GlobalId) {
        self.emit_load_global(dest, glob);
    }
    fn visit_load_global_ptr(&mut self, dest: Register, glob: GlobalId) {
        self.emit_load_global(dest, glob);
    }

    fn visit_store_global_bool(&mut self, src: Register, glob: GlobalId) {
        self.emit_store_global(src, glob);
    }
    fn visit_store_global_uint8(&mut self, src: Register, glob: GlobalId) {
        self.emit_store_global(src, glob);
    }
    fn visit_store_global_char(&mut self, src: Register, glob: GlobalId) {
        self.emit_store_global(src, glob);
    }
    fn visit_store_global_int(&mut self, src: Register, glob: GlobalId) {
        self.emit_store_global(src, glob);
    }
    fn visit_store_global_int64(&mut self, src: Register, glob: GlobalId) {
        self.emit_store_global(src, glob);
    }
    fn visit_store_global_float(&mut self, src: Register, glob: GlobalId) {
        self.emit_store_global(src, glob);
    }
    fn visit_store_global_double(&mut self, src: Register, glob: GlobalId) {
        self.emit_store_global(src, glob);
    }
    fn visit_store_global_ptr(&mut self, src: Register, glob: GlobalId) {
        self.emit_store_global(src, glob);
    }

    fn visit_push_register(&mut self, src: Register) {
        self.argument_stack.push(src);
    }

    fn visit_const_nil(&mut self, dest: Register) {
        self.emit_const_nil(dest);
    }
    fn visit_const_true(&mut self, dest: Register) {
        self.emit_const_bool(dest, true);
    }
    fn visit_const_false(&mut self, dest: Register) {
        self.emit_const_bool(dest, false);
    }
    fn visit_const_zero_uint8(&mut self, dest: Register) {
        self.emit_const_int(dest, 0);
    }
    fn visit_const_zero_char(&mut self, dest: Register) {
        self.emit_const_int(dest, 0);
    }
    fn visit_const_zero_int(&mut self, dest: Register) {
        self.emit_const_int(dest, 0);
    }
    fn visit_const_zero_int64(&mut self, dest: Register) {
        self.emit_const_int(dest, 0);
    }
    fn visit_const_zero_float(&mut self, dest: Register) {
        self.emit_const_float(dest, 0_f64);
    }
    fn visit_const_zero_double(&mut self, dest: Register) {
        self.emit_const_float(dest, 0_f64);
    }
    fn visit_const_char(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bytecode
            .const_pool(idx)
            .to_char()
            .expect("unexpected const pool entry");
        self.emit_const_int(dest, value as i64);
    }

    fn visit_const_uint8(&mut self, dest: Register, value: u8) {
        self.emit_const_int(dest, value as i64);
    }
    fn visit_const_int(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bytecode
            .const_pool(idx)
            .to_int()
            .expect("unexpected const pool entry");
        self.emit_const_int(dest, value as i64);
    }
    fn visit_const_int64(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bytecode
            .const_pool(idx)
            .to_int64()
            .expect("unexpected const pool entry");
        self.emit_const_int(dest, value);
    }
    fn visit_const_float(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bytecode
            .const_pool(idx)
            .to_float()
            .expect("unexpected const pool entry");
        self.emit_const_float(dest, value as f64);
    }
    fn visit_const_double(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bytecode
            .const_pool(idx)
            .to_double()
            .expect("unexpected const pool entry");
        self.emit_const_float(dest, value);
    }
    fn visit_const_string(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bytecode
            .const_pool(idx)
            .to_string()
            .expect("unexpected const pool entry");
        self.emit_const_string(dest, value);
    }

    fn visit_test_eq_ptr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::Equal);
    }
    fn visit_test_ne_ptr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::NotEqual);
    }

    fn visit_test_eq_bool(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::Equal);
    }
    fn visit_test_ne_bool(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::NotEqual);
    }

    fn visit_test_eq_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::Equal);
    }
    fn visit_test_ne_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::NotEqual);
    }
    fn visit_test_gt_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::Greater);
    }
    fn visit_test_ge_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::GreaterEq);
    }
    fn visit_test_lt_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::Less);
    }
    fn visit_test_le_uint8(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::LessEq);
    }

    fn visit_test_eq_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::Equal);
    }
    fn visit_test_ne_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::NotEqual);
    }
    fn visit_test_gt_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::Greater);
    }
    fn visit_test_ge_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::GreaterEq);
    }
    fn visit_test_lt_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::Less);
    }
    fn visit_test_le_char(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::LessEq);
    }

    fn visit_test_eq_enum(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::Equal);
    }
    fn visit_test_ne_enum(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::NotEqual);
    }

    fn visit_test_eq_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::Equal);
    }
    fn visit_test_ne_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::NotEqual);
    }
    fn visit_test_gt_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::Greater);
    }
    fn visit_test_ge_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::GreaterEq);
    }
    fn visit_test_lt_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::Less);
    }
    fn visit_test_le_int(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::LessEq);
    }

    fn visit_test_eq_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::Equal);
    }
    fn visit_test_ne_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::NotEqual);
    }
    fn visit_test_gt_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::Greater);
    }
    fn visit_test_ge_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::GreaterEq);
    }
    fn visit_test_lt_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::Less);
    }
    fn visit_test_le_int64(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_generic(dest, lhs, rhs, CondCode::LessEq);
    }

    fn visit_test_eq_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_float(dest, lhs, rhs, CondCode::Equal);
    }
    fn visit_test_ne_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_float(dest, lhs, rhs, CondCode::NotEqual);
    }
    fn visit_test_gt_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_float(dest, lhs, rhs, CondCode::Greater);
    }
    fn visit_test_ge_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_float(dest, lhs, rhs, CondCode::GreaterEq);
    }
    fn visit_test_lt_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_float(dest, lhs, rhs, CondCode::Less);
    }
    fn visit_test_le_float(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_float(dest, lhs, rhs, CondCode::LessEq);
    }

    fn visit_test_eq_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_float(dest, lhs, rhs, CondCode::Equal);
    }
    fn visit_test_ne_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_float(dest, lhs, rhs, CondCode::NotEqual);
    }
    fn visit_test_gt_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_float(dest, lhs, rhs, CondCode::Greater);
    }
    fn visit_test_ge_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_float(dest, lhs, rhs, CondCode::GreaterEq);
    }
    fn visit_test_lt_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_float(dest, lhs, rhs, CondCode::Less);
    }
    fn visit_test_le_double(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit_test_float(dest, lhs, rhs, CondCode::LessEq);
    }

    fn visit_assert(&mut self, value: Register) {
        assert_eq!(self.bytecode.register_type(value), BytecodeType::Bool);
        let position = self.bytecode.offset_position(self.current_offset.to_u32());
        self.emit_load_register(value, REG_RESULT.into());

        self.asm.assert(REG_RESULT, position);
    }

    fn visit_jump_if_false(&mut self, opnd: Register, offset: u32) {
        let target = BytecodeOffset(self.current_offset.to_u32() + offset);
        self.emit_jump_if(opnd, target, false);
    }
    fn visit_jump_if_false_const(&mut self, opnd: Register, idx: ConstPoolIdx) {
        let offset = self
            .bytecode
            .const_pool(idx)
            .to_int()
            .expect("int expected");
        self.visit_jump_if_false(opnd, offset as u32);
    }
    fn visit_jump_if_true(&mut self, opnd: Register, offset: u32) {
        let target = BytecodeOffset(self.current_offset.to_u32() + offset);
        self.emit_jump_if(opnd, target, true);
    }
    fn visit_jump_if_true_const(&mut self, opnd: Register, idx: ConstPoolIdx) {
        let offset = self
            .bytecode
            .const_pool(idx)
            .to_int()
            .expect("int expected");
        self.visit_jump_if_true(opnd, offset as u32);
    }
    fn visit_jump_loop(&mut self, offset: u32) {
        let target = BytecodeOffset(self.current_offset.to_u32() - offset);
        self.emit_stack_guard();
        self.emit_jump(target);
    }
    fn visit_jump(&mut self, offset: u32) {
        let target = BytecodeOffset(self.current_offset.to_u32() + offset);
        self.emit_jump(target);
    }
    fn visit_jump_const(&mut self, idx: ConstPoolIdx) {
        let offset = self
            .bytecode
            .const_pool(idx)
            .to_int()
            .expect("int expected");
        self.visit_jump(offset as u32);
    }

    fn visit_invoke_direct_void(&mut self, fctdef: FctDefId, count: u32) {
        self.emit_invoke_direct_void(fctdef, count)
    }
    fn visit_invoke_direct_bool(&mut self, dest: Register, fctdef: FctDefId, count: u32) {
        self.emit_invoke_direct_generic(dest, fctdef, count);
    }
    fn visit_invoke_direct_uint8(&mut self, dest: Register, fctdef: FctDefId, count: u32) {
        self.emit_invoke_direct_generic(dest, fctdef, count);
    }
    fn visit_invoke_direct_char(&mut self, dest: Register, fctdef: FctDefId, count: u32) {
        self.emit_invoke_direct_generic(dest, fctdef, count);
    }
    fn visit_invoke_direct_int(&mut self, dest: Register, fctdef: FctDefId, count: u32) {
        self.emit_invoke_direct_generic(dest, fctdef, count);
    }
    fn visit_invoke_direct_int64(&mut self, dest: Register, fctdef: FctDefId, count: u32) {
        self.emit_invoke_direct_generic(dest, fctdef, count);
    }
    fn visit_invoke_direct_float(&mut self, dest: Register, fctdef: FctDefId, count: u32) {
        self.emit_invoke_direct_generic(dest, fctdef, count);
    }
    fn visit_invoke_direct_double(&mut self, dest: Register, fctdef: FctDefId, count: u32) {
        self.emit_invoke_direct_generic(dest, fctdef, count);
    }
    fn visit_invoke_direct_ptr(&mut self, dest: Register, fctdef: FctDefId, count: u32) {
        self.emit_invoke_direct_generic(dest, fctdef, count);
    }

    fn visit_invoke_virtual_void(&mut self, fctdef: FctDefId, count: u32) {
        self.emit_invoke_virtual_void(fctdef, count);
    }
    fn visit_invoke_virtual_bool(&mut self, dest: Register, fctdef: FctDefId, count: u32) {
        self.emit_invoke_virtual_generic(dest, fctdef, count);
    }
    fn visit_invoke_virtual_uint8(&mut self, dest: Register, fctdef: FctDefId, count: u32) {
        self.emit_invoke_virtual_generic(dest, fctdef, count);
    }
    fn visit_invoke_virtual_char(&mut self, dest: Register, fctdef: FctDefId, count: u32) {
        self.emit_invoke_virtual_generic(dest, fctdef, count);
    }
    fn visit_invoke_virtual_int(&mut self, dest: Register, fctdef: FctDefId, count: u32) {
        self.emit_invoke_virtual_generic(dest, fctdef, count);
    }
    fn visit_invoke_virtual_int64(&mut self, dest: Register, fctdef: FctDefId, count: u32) {
        self.emit_invoke_virtual_generic(dest, fctdef, count);
    }
    fn visit_invoke_virtual_float(&mut self, dest: Register, fctdef: FctDefId, count: u32) {
        self.emit_invoke_virtual_generic(dest, fctdef, count);
    }
    fn visit_invoke_virtual_double(&mut self, dest: Register, fctdef: FctDefId, count: u32) {
        self.emit_invoke_virtual_generic(dest, fctdef, count);
    }
    fn visit_invoke_virtual_ptr(&mut self, dest: Register, fctdef: FctDefId, count: u32) {
        self.emit_invoke_virtual_generic(dest, fctdef, count);
    }

    fn visit_invoke_static_void(&mut self, fctdef: FctDefId, count: u32) {
        self.emit_invoke_static_void(fctdef, count)
    }
    fn visit_invoke_static_bool(&mut self, dest: Register, fctdef: FctDefId, count: u32) {
        self.emit_invoke_static_generic(dest, fctdef, count);
    }
    fn visit_invoke_static_uint8(&mut self, dest: Register, fctdef: FctDefId, count: u32) {
        self.emit_invoke_static_generic(dest, fctdef, count);
    }
    fn visit_invoke_static_char(&mut self, dest: Register, fctdef: FctDefId, count: u32) {
        self.emit_invoke_static_generic(dest, fctdef, count);
    }
    fn visit_invoke_static_int(&mut self, dest: Register, fctdef: FctDefId, count: u32) {
        self.emit_invoke_static_generic(dest, fctdef, count);
    }
    fn visit_invoke_static_int64(&mut self, dest: Register, fctdef: FctDefId, count: u32) {
        self.emit_invoke_static_generic(dest, fctdef, count);
    }
    fn visit_invoke_static_float(&mut self, dest: Register, fctdef: FctDefId, count: u32) {
        self.emit_invoke_static_generic(dest, fctdef, count);
    }
    fn visit_invoke_static_double(&mut self, dest: Register, fctdef: FctDefId, count: u32) {
        self.emit_invoke_static_generic(dest, fctdef, count);
    }
    fn visit_invoke_static_ptr(&mut self, dest: Register, fctdef: FctDefId, count: u32) {
        self.emit_invoke_static_generic(dest, fctdef, count);
    }

    fn visit_new_object(&mut self, dest: Register, cls: ClassDefId) {
        self.emit_new_object(dest, cls)
    }
    fn visit_new_array(&mut self, dest: Register, cls: ClassDefId, length: Register) {
        self.emit_new_array(dest, cls, length);
    }

    fn visit_nil_check(&mut self, obj: Register) {
        self.emit_nil_check(obj);
    }

    fn visit_array_length(&mut self, dest: Register, arr: Register) {
        self.emit_array_length(dest, arr);
    }
    fn visit_array_bound_check(&mut self, arr: Register, idx: Register) {
        self.emit_array_bound_check(arr, idx);
    }

    fn visit_load_array_bool(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit_load_array(dest, arr, idx);
    }
    fn visit_load_array_uint8(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit_load_array(dest, arr, idx);
    }
    fn visit_load_array_char(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit_load_array(dest, arr, idx);
    }
    fn visit_load_array_int(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit_load_array(dest, arr, idx);
    }
    fn visit_load_array_int64(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit_load_array(dest, arr, idx);
    }
    fn visit_load_array_float(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit_load_array(dest, arr, idx);
    }
    fn visit_load_array_double(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit_load_array(dest, arr, idx);
    }
    fn visit_load_array_ptr(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit_load_array(dest, arr, idx);
    }

    fn visit_store_array_bool(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit_store_array(src, arr, idx);
    }
    fn visit_store_array_uint8(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit_store_array(src, arr, idx);
    }
    fn visit_store_array_char(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit_store_array(src, arr, idx);
    }
    fn visit_store_array_int(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit_store_array(src, arr, idx);
    }
    fn visit_store_array_int64(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit_store_array(src, arr, idx);
    }
    fn visit_store_array_float(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit_store_array(src, arr, idx);
    }
    fn visit_store_array_double(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit_store_array(src, arr, idx);
    }
    fn visit_store_array_ptr(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit_store_array(src, arr, idx);
    }

    fn visit_ret_void(&mut self) {
        self.emit_epilog();
    }
    fn visit_ret_bool(&mut self, opnd: Register) {
        self.emit_return_generic(opnd);
    }
    fn visit_ret_uint8(&mut self, opnd: Register) {
        self.emit_return_generic(opnd);
    }
    fn visit_ret_char(&mut self, opnd: Register) {
        self.emit_return_generic(opnd);
    }
    fn visit_ret_int(&mut self, opnd: Register) {
        self.emit_return_generic(opnd);
    }
    fn visit_ret_int64(&mut self, opnd: Register) {
        self.emit_return_generic(opnd);
    }
    fn visit_ret_float(&mut self, opnd: Register) {
        self.emit_return_generic(opnd);
    }
    fn visit_ret_double(&mut self, opnd: Register) {
        self.emit_return_generic(opnd);
    }
    fn visit_ret_ptr(&mut self, opnd: Register) {
        self.emit_return_generic(opnd);
    }
}

fn result_reg(bytecode_type: BytecodeType) -> AnyReg {
    if bytecode_type.mode().is_float() {
        FREG_RESULT.into()
    } else {
        REG_RESULT.into()
    }
}

fn ensure_jit_or_stub_ptr<'ast>(
    src: &FctSrc,
    vm: &VM,
    cls_type_params: TypeList,
    fct_type_params: TypeList,
) -> Address {
    let specials = src.specializations.read();
    let key = (cls_type_params, fct_type_params);

    if let Some(&jit_fct_id) = specials.get(&key) {
        let jit_fct = vm.jit_fcts.idx(jit_fct_id);
        return jit_fct.instruction_start();
    }

    vm.compile_stub()
}
