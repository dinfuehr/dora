use dora_parser::lexer::position::Position;

use crate::compiler::codegen::AnyReg;
use crate::compiler::fct::LazyCompilationSite;
use crate::cpu::*;
use crate::gc::swiper::CARD_SIZE_BITS;
use crate::masm::{CondCode, Label, MacroAssembler, Mem};
use crate::mem::{fits_i32, ptr_width};
use crate::object::{offset_of_array_data, offset_of_array_length, Header};
use crate::threads::ThreadLocalData;
use crate::ty::{MachineMode, SourceTypeArray};
use crate::vm::{get_vm, FctId, Trap};
use crate::vtable::VTable;
pub use dora_asm::x64::AssemblerX64 as Assembler;
use dora_asm::x64::Register as AsmRegister;
use dora_asm::x64::{Address, Condition, Immediate, ScaleFactor, XmmRegister};

impl MacroAssembler {
    pub fn prolog_size(&mut self, stacksize: i32) {
        self.asm.pushq_r(RBP.into());
        self.asm.movq_rr(RBP.into(), RSP.into());
        debug_assert!(stacksize as usize % STACK_FRAME_ALIGNMENT == 0);

        if stacksize > 0 {
            self.asm.subq_ri(RSP.into(), Immediate(stacksize as i64));
        }
    }

    pub fn prolog(&mut self) -> usize {
        self.asm.pushq_r(RBP.into());
        self.asm.movq_rr(RBP.into(), RSP.into());

        self.asm.subq_ri32(RSP.into(), Immediate(0));
        let patch_offset = self.pos() - 4;

        patch_offset
    }

    pub fn patch_stacksize(&mut self, patch_offset: usize, stacksize: i32) {
        self.asm.set_position(patch_offset);
        self.asm.emit_u32(stacksize as u32);
        self.asm.set_position_end();
    }

    pub fn check_stack_pointer(&mut self, lbl_overflow: Label) {
        self.asm.cmpq_ar(
            Address::offset(REG_THREAD.into(), ThreadLocalData::stack_limit_offset()),
            RSP.into(),
        );

        self.asm.jcc(Condition::Above, lbl_overflow);
    }

    pub fn safepoint(&mut self, lbl_slow: Label) {
        self.asm.cmpb_ai(
            Address::offset(
                REG_THREAD.into(),
                ThreadLocalData::safepoint_requested_offset(),
            ),
            Immediate(0),
        );

        self.asm.jcc(Condition::NotEqual, lbl_slow);
    }

    pub fn fix_result(&mut self, result: Reg, mode: MachineMode) {
        // Returning a boolean only sets the lower byte. However Dora
        // on x64 keeps booleans in 32-bit registers. Fix result of
        // native call up.
        if mode.is_int8() {
            self.asm.andq_ri(result.into(), Immediate(0xFF));
        }
    }

    pub fn epilog(&mut self) {
        self.asm.movq_rr(RSP.into(), RBP.into());
        self.asm.popq_r(RBP.into());
        self.asm.retq();
    }

    pub fn epilog_without_return(&mut self) {
        self.asm.movq_rr(RSP.into(), RBP.into());
        self.asm.popq_r(RBP.into());
    }

    pub fn increase_stack_frame(&mut self, size: i32) {
        debug_assert!(size as usize % STACK_FRAME_ALIGNMENT == 0);

        if size > 0 {
            self.asm.subq_ri(RSP.into(), Immediate(size as i64));
        }
    }

    pub fn decrease_stack_frame(&mut self, size: i32) {
        if size > 0 {
            self.asm.addq_ri(RSP.into(), Immediate(size as i64));
        }
    }

    pub fn direct_call(&mut self, fct_id: FctId, ptr: *const u8, type_params: SourceTypeArray) {
        let disp = self.add_addr(ptr);
        let pos = self.pos() as i32;

        self.load_constpool(REG_RESULT, disp + pos);
        self.call_reg(REG_RESULT);

        let pos = self.pos() as i32;
        self.emit_lazy_compilation_site(LazyCompilationSite::Direct(
            fct_id,
            disp + pos,
            type_params,
        ));
    }

    pub fn raw_call(&mut self, ptr: *const u8) {
        let disp = self.add_addr(ptr);
        let pos = self.pos() as i32;

        self.load_constpool(REG_RESULT, disp + pos);
        self.call_reg(REG_RESULT);
    }

    pub fn indirect_call(
        &mut self,
        pos: Position,
        fct_id: FctId,
        vtable_index: u32,
        self_index: u32,
        type_params: SourceTypeArray,
    ) {
        let obj = REG_PARAMS[self_index as usize];
        self.test_if_nil_bailout(pos, obj, Trap::NIL);

        // REG_RESULT = [obj] (load vtable)
        self.load_mem(MachineMode::Ptr, REG_RESULT.into(), Mem::Base(obj, 0));

        // calculate offset of VTable entry
        let disp = VTable::offset_of_method_table() + (vtable_index as i32) * ptr_width();

        // load vtable entry
        self.load_mem(
            MachineMode::Ptr,
            REG_RESULT.into(),
            Mem::Base(REG_RESULT, disp),
        );

        // call *REG_RESULT
        self.call_reg(REG_RESULT);
        self.emit_lazy_compilation_site(LazyCompilationSite::Virtual(
            self_index == 0,
            fct_id,
            vtable_index,
            type_params,
        ));
    }

    pub fn load_array_elem(&mut self, mode: MachineMode, dest: AnyReg, array: Reg, index: Reg) {
        self.load_mem(
            mode,
            dest,
            Mem::Index(array, index, mode.size(), offset_of_array_data()),
        );
    }

    pub fn set(&mut self, dest: Reg, cond: CondCode) {
        self.asm.setcc_r(convert_into_condition(cond), dest.into());
        self.asm.movzxb_rr(dest.into(), dest.into());
    }

    pub fn cmp_mem(&mut self, mode: MachineMode, mem: Mem, rhs: Reg) {
        match mode {
            MachineMode::Int8 => self.asm.cmpb_ar(address_from_mem(mem), rhs.into()),
            MachineMode::Int32 => self.asm.cmpl_ar(address_from_mem(mem), rhs.into()),
            MachineMode::Int64 | MachineMode::Ptr => {
                self.asm.cmpq_ar(address_from_mem(mem), rhs.into())
            }
            _ => unreachable!(),
        }
    }

    pub fn cmp_mem_imm(&mut self, mode: MachineMode, mem: Mem, imm: i32) {
        let imm = Immediate(imm as i64);

        match mode {
            MachineMode::Int8 => self.asm.cmpb_ai(address_from_mem(mem), imm),
            MachineMode::Int32 => self.asm.cmpl_ai(address_from_mem(mem), imm),
            MachineMode::Int64 | MachineMode::Ptr => self.asm.cmpq_ai(address_from_mem(mem), imm),
            _ => unreachable!(),
        }
    }

    pub fn cmp_reg(&mut self, mode: MachineMode, lhs: Reg, rhs: Reg) {
        if mode.is64() {
            self.asm.cmpq_rr(lhs.into(), rhs.into());
        } else {
            self.asm.cmpl_rr(lhs.into(), rhs.into());
        }
    }

    pub fn cmp_reg_imm(&mut self, mode: MachineMode, lhs: Reg, imm: i32) {
        if mode.is64() {
            self.asm.cmpq_ri(lhs.into(), Immediate(imm as i64))
        } else {
            self.asm.cmpl_ri(lhs.into(), Immediate(imm as i64))
        }
    }

    pub fn cmp_int(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        self.asm.xorl_rr(dest.into(), dest.into());
        match mode {
            MachineMode::Int64 => self.asm.cmpq_rr(lhs.into(), rhs.into()),
            MachineMode::Int8 | MachineMode::Int32 => self.asm.cmpl_rr(lhs.into(), rhs.into()),
            _ => unreachable!(),
        }
        self.asm.setcc_r(Condition::Above, dest.into());

        let scratch = self.get_scratch();
        self.asm.movl_ri((*scratch).into(), Immediate(-1));
        self.asm
            .cmovl(Condition::Below, dest.into(), (*scratch).into());
    }

    pub fn float_cmp_int(&mut self, mode: MachineMode, dest: Reg, lhs: FReg, rhs: FReg) {
        self.asm.xorl_rr(dest.into(), dest.into());
        match mode {
            MachineMode::Float64 => self.asm.ucomisd_rr(lhs.into(), rhs.into()),
            MachineMode::Float32 => self.asm.ucomiss_rr(lhs.into(), rhs.into()),
            _ => unreachable!(),
        }
        self.asm.setcc_r(Condition::Above, dest.into());

        let scratch = self.get_scratch();
        self.asm.movl_ri((*scratch).into(), Immediate(-1));
        self.asm
            .cmovl(Condition::Below, dest.into(), (*scratch).into());
    }

    pub fn float_cmp(
        &mut self,
        mode: MachineMode,
        dest: Reg,
        lhs: FReg,
        rhs: FReg,
        cond: CondCode,
    ) {
        let scratch = self.get_scratch();

        match cond {
            CondCode::Equal | CondCode::NotEqual => {
                let init = if cond == CondCode::Equal { 0 } else { 1 };

                self.load_int_const(MachineMode::Int32, *scratch, init);
                self.asm.xorl_rr(dest.into(), dest.into());

                match mode {
                    MachineMode::Float32 => self.asm.ucomiss_rr(lhs.into(), rhs.into()),
                    MachineMode::Float64 => self.asm.ucomisd_rr(lhs.into(), rhs.into()),
                    _ => unreachable!(),
                }

                let parity = if cond == CondCode::Equal {
                    Condition::NoParity
                } else {
                    Condition::Parity
                };

                self.asm.setcc_r(parity, dest.into());
                self.asm
                    .cmovl(Condition::NotEqual, dest.into(), (*scratch).into());
            }

            CondCode::Greater | CondCode::GreaterEq => {
                self.load_int_const(MachineMode::Int32, dest, 0);

                match mode {
                    MachineMode::Float32 => self.asm.ucomiss_rr(lhs.into(), rhs.into()),
                    MachineMode::Float64 => self.asm.ucomisd_rr(lhs.into(), rhs.into()),
                    _ => unreachable!(),
                }

                let cond = match cond {
                    CondCode::Greater => Condition::Above,
                    CondCode::GreaterEq => Condition::AboveOrEqual,
                    _ => unreachable!(),
                };

                self.asm.setcc_r(cond, dest.into());
            }

            CondCode::Less | CondCode::LessEq => {
                self.asm.xorl_rr(dest.into(), dest.into());

                match mode {
                    MachineMode::Float32 => self.asm.ucomiss_rr(rhs.into(), lhs.into()),
                    MachineMode::Float64 => self.asm.ucomisd_rr(rhs.into(), lhs.into()),
                    _ => unreachable!(),
                }

                let cond = match cond {
                    CondCode::Less => Condition::Above,
                    CondCode::LessEq => Condition::AboveOrEqual,
                    _ => unreachable!(),
                };

                self.asm.setcc_r(cond, dest.into());
            }

            _ => unreachable!(),
        }
    }

    pub fn float_cmp_nan(&mut self, mode: MachineMode, dest: Reg, src: FReg) {
        self.asm.xorl_rr(dest.into(), dest.into());

        match mode {
            MachineMode::Float32 => self.asm.ucomiss_rr(src.into(), src.into()),
            MachineMode::Float64 => self.asm.ucomisd_rr(src.into(), src.into()),
            _ => unreachable!(),
        }

        self.asm.setcc_r(Condition::Parity, dest.into());
    }

    pub fn cmp_zero(&mut self, mode: MachineMode, lhs: Reg) {
        if mode.is64() {
            self.asm.testq_rr(lhs.into(), lhs.into());
        } else {
            self.asm.testl_rr(lhs.into(), lhs.into());
        }
    }

    pub fn test_and_jump_if(&mut self, cond: CondCode, reg: Reg, lbl: Label) {
        assert!(cond == CondCode::Zero || cond == CondCode::NonZero);

        self.asm.testl_rr(reg.into(), reg.into());
        self.jump_if(cond, lbl);
    }

    pub fn jump_if(&mut self, cond: CondCode, target: Label) {
        self.asm.jcc(convert_into_condition(cond), target)
    }

    pub fn jump(&mut self, target: Label) {
        self.asm.jmp(target);
    }

    pub fn jump_reg(&mut self, reg: Reg) {
        self.asm.jmp_r(reg.into());
    }

    pub fn int_div(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg, pos: Position) {
        self.div_common(mode, dest, lhs, rhs, RAX, true, pos);
    }

    pub fn int_mod(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg, pos: Position) {
        self.div_common(mode, dest, lhs, rhs, RDX, false, pos);
    }

    fn div_common(
        &mut self,
        mode: MachineMode,
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
        result: Reg,
        is_div: bool,
        pos: Position,
    ) {
        if mode.is64() {
            self.asm.testq_rr(rhs.into(), rhs.into());
        } else {
            self.asm.testl_rr(rhs.into(), rhs.into());
        }
        let lbl_zero = self.create_label();
        let lbl_done = self.create_label();
        let lbl_div = self.create_label();

        self.jump_if(CondCode::Zero, lbl_zero);
        self.emit_bailout(lbl_zero, Trap::DIV0, pos);

        if mode.is64() {
            self.asm.cmpq_ri(rhs.into(), Immediate(-1));
        } else {
            self.asm.cmpl_ri(rhs.into(), Immediate(-1));
        }
        self.jump_if(CondCode::NotEqual, lbl_div);

        if is_div {
            self.int_neg(mode, dest, lhs);
        } else {
            self.asm.xorl_rr(dest.into(), dest.into());
        }
        self.jump(lbl_done);

        self.bind_label(lbl_div);

        if lhs != RAX {
            assert!(rhs != RAX);
            self.mov_rr(mode.is64(), RAX.into(), lhs.into());
        }

        if mode.is64() {
            self.asm.cqo();
        } else {
            self.asm.cdq();
        }

        if mode.is64() {
            self.asm.idivq_r(rhs.into());
        } else {
            self.asm.idivl_r(rhs.into());
        }

        if dest != result {
            self.mov_rr(mode.is64(), dest.into(), result.into());
        }

        self.bind_label(lbl_done);
    }

    pub fn int_mul(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        if mode.is64() {
            self.asm.imulq_rr(lhs.into(), rhs.into());
        } else {
            self.asm.imull_rr(lhs.into(), rhs.into());
        }

        if dest != lhs {
            self.mov_rr(mode.is64(), dest.into(), lhs.into());
        }
    }

    pub fn int_add(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        if mode.is64() {
            self.asm.addq_rr(lhs.into(), rhs.into());
        } else {
            self.asm.addl_rr(lhs.into(), rhs.into());
        }

        if dest != lhs {
            self.mov_rr(mode.is64(), dest.into(), lhs.into());
        }
    }

    pub fn int_add_checked(
        &mut self,
        _mode: MachineMode,
        _dest: Reg,
        _lhs: Reg,
        _rhs: Reg,
        _pos: Position,
    ) {
        unimplemented!();
    }

    pub fn int_add_imm(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, value: i64) {
        if !fits_i32(value) {
            assert!(mode == MachineMode::Int64 || mode == MachineMode::Ptr);
            let reg_size = self.get_scratch();
            self.load_int_const(MachineMode::Ptr, *reg_size, value);
            self.int_add(mode, dest, lhs, *reg_size);
            return;
        }

        if mode.is64() {
            self.asm.addq_ri(lhs.into(), Immediate(value));
        } else {
            self.asm.addl_ri(lhs.into(), Immediate(value));
        }

        if dest != lhs {
            self.mov_rr(mode.is64(), dest.into(), lhs.into());
        }
    }

    pub fn int_sub(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        if mode.is64() {
            self.asm.subq_rr(lhs.into(), rhs.into());
        } else {
            self.asm.subl_rr(lhs.into(), rhs.into());
        }

        if dest != lhs {
            self.mov_rr(mode.is64(), dest.into(), lhs.into());
        }
    }

    pub fn int_sub_checked(
        &mut self,
        _mode: MachineMode,
        _dest: Reg,
        _lhs: Reg,
        _rhs: Reg,
        _pos: Position,
    ) {
        unimplemented!();
    }

    pub fn int_shl(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        if rhs != RCX {
            assert!(lhs != RCX);
            self.mov_rr(mode.is64(), RCX.into(), rhs.into());
        }

        if mode.is64() {
            self.asm.shlq_r(lhs.into());
        } else {
            self.asm.shll_r(lhs.into());
        }

        if dest != lhs {
            self.mov_rr(mode.is64(), dest.into(), lhs.into());
        }
    }

    pub fn int_shr(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        if rhs != RCX {
            assert!(lhs != RCX);
            self.mov_rr(mode.is64(), RCX.into(), rhs.into());
        }

        if mode.is64() {
            self.asm.shrq_r(lhs.into());
        } else {
            self.asm.shrl_r(lhs.into());
        }

        if dest != lhs {
            self.mov_rr(mode.is64(), dest.into(), lhs.into());
        }
    }

    pub fn int_sar(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        if rhs != RCX {
            assert!(lhs != RCX);
            self.mov_rr(mode.is64(), RCX.into(), rhs.into());
        }

        if mode.is64() {
            self.asm.sarq_r(lhs.into());
        } else {
            self.asm.sarl_r(lhs.into());
        }

        if dest != lhs {
            self.mov_rr(mode.is64(), dest.into(), lhs.into());
        }
    }

    pub fn int_rol(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        if rhs != RCX {
            assert!(lhs != RCX);
            self.mov_rr(mode.is64(), RCX.into(), rhs.into());
        }

        if mode.is64() {
            self.asm.rolq_r(lhs.into());
        } else {
            self.asm.roll_r(lhs.into());
        }

        if dest != lhs {
            self.mov_rr(mode.is64(), dest.into(), lhs.into());
        }
    }

    // We don't use RORX optionally like for the shifts above,
    // because curiously RORX only supports encoding the count as an immediate,
    // not by passing the value in a register.
    pub fn int_ror(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        if rhs != RCX {
            assert!(lhs != RCX);
            self.mov_rr(mode.is64(), RCX.into(), rhs.into());
        }

        if mode.is64() {
            self.asm.rorq_r(lhs.into());
        } else {
            self.asm.rorl_r(lhs.into());
        }

        if dest != lhs {
            self.mov_rr(mode.is64(), dest.into(), lhs.into());
        }
    }

    pub fn int_or(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        if mode.is64() {
            self.asm.orq_rr(lhs.into(), rhs.into());
        } else {
            self.asm.orl_rr(lhs.into(), rhs.into());
        }

        if dest != lhs {
            self.mov_rr(mode.is64(), dest.into(), lhs.into());
        }
    }

    pub fn int_and(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        if mode.is64() {
            self.asm.andq_rr(lhs.into(), rhs.into());
        } else {
            self.asm.andl_rr(lhs.into(), rhs.into());
        }

        if dest != lhs {
            self.mov_rr(mode.is64(), dest.into(), lhs.into());
        }
    }

    pub fn int_xor(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        if mode.is64() {
            self.asm.xorq_rr(lhs.into(), rhs.into());
        } else {
            self.asm.xorl_rr(lhs.into(), rhs.into());
        }

        if dest != lhs {
            self.mov_rr(mode.is64(), dest.into(), lhs.into());
        }
    }

    pub fn count_bits(&mut self, mode: MachineMode, dest: Reg, src: Reg, count_one_bits: bool) {
        if count_one_bits {
            if mode.is64() {
                self.asm.popcntq_rr(dest.into(), src.into());
            } else {
                self.asm.popcntl_rr(dest.into(), src.into());
            }
        } else {
            if mode.is64() {
                self.asm.notq(src.into());
                self.asm.popcntq_rr(dest.into(), src.into());
            } else {
                self.asm.notl(src.into());
                self.asm.popcntl_rr(dest.into(), src.into());
            }
        }
    }

    pub fn count_bits_leading(
        &mut self,
        mode: MachineMode,
        dest: Reg,
        src: Reg,
        count_one_bits: bool,
    ) {
        if count_one_bits {
            if mode.is64() {
                self.asm.notq(src.into());
                self.asm.lzcntq_rr(dest.into(), src.into());
            } else {
                self.asm.notl(src.into());
                self.asm.lzcntl_rr(dest.into(), src.into());
            }
        } else {
            if mode.is64() {
                self.asm.lzcntq_rr(dest.into(), src.into());
            } else {
                self.asm.lzcntl_rr(dest.into(), src.into());
            }
        }
    }

    pub fn count_bits_trailing(
        &mut self,
        mode: MachineMode,
        dest: Reg,
        src: Reg,
        count_one_bits: bool,
    ) {
        if count_one_bits {
            if mode.is64() {
                self.asm.notq(src.into());
                self.asm.tzcntq_rr(dest.into(), src.into());
            } else {
                self.asm.notl(src.into());
                self.asm.tzcntl_rr(dest.into(), src.into());
            }
        } else {
            if mode.is64() {
                self.asm.tzcntq_rr(dest.into(), src.into());
            } else {
                self.asm.tzcntl_rr(dest.into(), src.into());
            }
        }
    }

    pub fn int_to_float(
        &mut self,
        dest_mode: MachineMode,
        dest: FReg,
        src_mode: MachineMode,
        src: Reg,
    ) {
        self.asm.pxor_rr(dest.into(), dest.into());

        match dest_mode {
            MachineMode::Float32 => {
                if src_mode.is64() {
                    self.asm.cvtsi2ssq_rr(dest.into(), src.into());
                } else {
                    self.asm.cvtsi2ssd_rr(dest.into(), src.into());
                }
            }
            MachineMode::Float64 => {
                if src_mode.is64() {
                    self.asm.cvtsi2sdq_rr(dest.into(), src.into());
                } else {
                    self.asm.cvtsi2sdd_rr(dest.into(), src.into());
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn float_to_int(
        &mut self,
        dest_mode: MachineMode,
        dest: Reg,
        src_mode: MachineMode,
        src: FReg,
    ) {
        match src_mode {
            MachineMode::Float32 => {
                if dest_mode.is64() {
                    self.asm.cvttss2siq_rr(dest.into(), src.into())
                } else {
                    self.asm.cvttss2sid_rr(dest.into(), src.into())
                }
            }
            MachineMode::Float64 => {
                if dest_mode.is64() {
                    self.asm.cvttsd2siq_rr(dest.into(), src.into())
                } else {
                    self.asm.cvttsd2sid_rr(dest.into(), src.into())
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn float32_to_float64(&mut self, dest: FReg, src: FReg) {
        self.asm.cvtss2sd_rr(dest.into(), src.into());
    }

    pub fn float64_to_float32(&mut self, dest: FReg, src: FReg) {
        self.asm.cvtsd2ss_rr(dest.into(), src.into());
    }

    pub fn int_as_float(
        &mut self,
        dest_mode: MachineMode,
        dest: FReg,
        src_mode: MachineMode,
        src: Reg,
    ) {
        assert!(src_mode.size() == dest_mode.size());

        match dest_mode {
            MachineMode::Float32 => self.asm.movd_xr(dest.into(), src.into()),
            MachineMode::Float64 => self.asm.movq_xr(dest.into(), src.into()),
            _ => unreachable!(),
        }
    }

    pub fn float_as_int(
        &mut self,
        dest_mode: MachineMode,
        dest: Reg,
        src_mode: MachineMode,
        src: FReg,
    ) {
        assert!(src_mode.size() == dest_mode.size());

        match src_mode {
            MachineMode::Float32 => self.asm.movd_rx(dest.into(), src.into()),
            MachineMode::Float64 => self.asm.movq_rx(dest.into(), src.into()),
            _ => unreachable!(),
        }
    }

    pub fn determine_array_size(
        &mut self,
        dest: Reg,
        length: Reg,
        element_size: i32,
        with_header: bool,
    ) {
        let header_size = if with_header {
            Header::size() + ptr_width()
        } else {
            0
        };

        let size = header_size
            + if element_size != ptr_width() {
                ptr_width() - 1
            } else {
                0
            };

        if element_size == 1 || element_size == 2 || element_size == 4 || element_size == 8 {
            let scale = match element_size {
                1 => ScaleFactor::One,
                2 => ScaleFactor::Two,
                4 => ScaleFactor::Four,
                8 => ScaleFactor::Eight,
                _ => unreachable!(),
            };
            self.asm
                .lea(dest.into(), Address::index(length.into(), scale, size));
        } else {
            let scratch = self.get_scratch();
            self.load_int_const(MachineMode::Ptr, *scratch, element_size as i64);
            self.asm.imulq_rr((*scratch).into(), length.into());
            self.asm.addq_ri((*scratch).into(), Immediate(size as i64));
            self.asm.movq_rr(dest.into(), (*scratch).into());
        }

        if element_size != ptr_width() {
            self.asm
                .andq_ri(dest.into(), Immediate(-ptr_width() as i64));
        }
    }

    pub fn array_address(&mut self, dest: Reg, obj: Reg, index: Reg, element_size: i32) {
        let offset = Header::size() + ptr_width();
        let scratch = self.get_scratch();

        self.load_int_const(MachineMode::Ptr, *scratch, element_size as i64);
        self.asm.imulq_rr((*scratch).into(), index.into());
        self.asm
            .addq_ri((*scratch).into(), Immediate(offset as i64));
        self.asm.addq_rr((*scratch).into(), obj.into());
        self.asm.movq_rr(dest.into(), (*scratch).into());
    }

    pub fn check_index_out_of_bounds(&mut self, pos: Position, array: Reg, index: Reg) {
        let scratch = self.get_scratch();
        self.load_mem(
            MachineMode::Int64,
            (*scratch).into(),
            Mem::Base(array, offset_of_array_length()),
        );
        self.asm.cmpq_rr(index.into(), (*scratch).into());

        let lbl = self.create_label();
        self.jump_if(CondCode::UnsignedGreaterEq, lbl);
        self.emit_bailout(lbl, Trap::INDEX_OUT_OF_BOUNDS, pos);
    }

    pub fn load_nil(&mut self, dest: Reg) {
        self.asm.xorl_rr(dest.into(), dest.into());
    }

    pub fn load_int32_synchronized(&mut self, dest: Reg, addr: Reg) {
        self.asm.movl_ra(dest.into(), Address::reg(addr.into()));
    }

    pub fn load_int64_synchronized(&mut self, dest: Reg, addr: Reg) {
        self.asm.movq_ra(dest.into(), Address::reg(addr.into()));
    }

    pub fn store_int32_synchronized(&mut self, dest: Reg, addr: Reg) {
        self.asm.xchgl_ar(Address::reg(addr.into()), dest.into());
    }

    pub fn store_int64_synchronized(&mut self, dest: Reg, addr: Reg) {
        self.asm.xchgq_ar(Address::reg(addr.into()), dest.into());
    }

    pub fn exchange_int32_synchronized(&mut self, old: Reg, new: Reg, address: Reg) {
        self.asm.xchgl_ar(Address::reg(address.into()), new.into());
        self.asm.movl_rr(old.into(), new.into());
    }

    pub fn exchange_int64_synchronized(&mut self, old: Reg, new: Reg, address: Reg) {
        self.asm.xchgq_ar(Address::reg(address.into()), new.into());
        self.asm.movq_rr(old.into(), new.into());
    }

    pub fn compare_exchange_int32_synchronized(
        &mut self,
        expected: Reg,
        new: Reg,
        address: Reg,
    ) -> Reg {
        assert_eq!(expected, RAX);
        self.asm
            .lock_cmpxchgl_ar(Address::reg(address.into()), new.into());
        RAX
    }

    pub fn compare_exchange_int64_synchronized(
        &mut self,
        expected: Reg,
        new: Reg,
        address: Reg,
    ) -> Reg {
        assert_eq!(expected, RAX);
        self.asm
            .lock_cmpxchgq_ar(Address::reg(address.into()), new.into());
        RAX
    }

    pub fn fetch_add_int32_synchronized(
        &mut self,
        _previous: Reg,
        value: Reg,
        address: Reg,
    ) -> Reg {
        self.asm
            .lock_xaddl_ar(Address::reg(address.into()), value.into());
        value
    }

    pub fn fetch_add_int64_synchronized(
        &mut self,
        _previous: Reg,
        value: Reg,
        address: Reg,
    ) -> Reg {
        self.asm
            .lock_xaddq_ar(Address::reg(address.into()), value.into());
        value
    }

    pub fn load_mem(&mut self, mode: MachineMode, dest: AnyReg, mem: Mem) {
        match mode {
            MachineMode::Int8 => self.asm.movzxb_ra(dest.reg().into(), address_from_mem(mem)),
            MachineMode::Int32 => self.asm.movl_ra(dest.reg().into(), address_from_mem(mem)),
            MachineMode::Int64 | MachineMode::Ptr | MachineMode::IntPtr => {
                self.asm.movq_ra(dest.reg().into(), address_from_mem(mem))
            }
            MachineMode::Float32 => self.asm.movss_ra(dest.freg().into(), address_from_mem(mem)),
            MachineMode::Float64 => self.asm.movsd_ra(dest.freg().into(), address_from_mem(mem)),
        }
    }

    pub fn lea(&mut self, dest: Reg, mem: Mem) {
        self.asm.lea(dest.into(), address_from_mem(mem));
    }

    pub fn emit_barrier(&mut self, src: Reg, card_table_offset: usize) {
        self.asm
            .shrq_ri(src.into(), Immediate(CARD_SIZE_BITS as i64));

        // test if card table offset fits into displacement of memory store
        if card_table_offset <= 0x7FFF_FFFF {
            // emit mov [card_table_offset + base], 0
            self.asm.movb_ai(
                Address::offset(src.into(), card_table_offset as i32),
                Immediate(0),
            );
        } else {
            let scratch = self.get_scratch();
            self.load_int_const(MachineMode::Ptr, *scratch, card_table_offset as i64);
            self.asm.movb_ai(
                Address::array(src.into(), (*scratch).into(), ScaleFactor::One, 0),
                Immediate(0),
            );
        }
    }

    pub fn store_mem(&mut self, mode: MachineMode, mem: Mem, src: AnyReg) {
        match mode {
            MachineMode::Int8 => self.asm.movb_ar(address_from_mem(mem), src.reg().into()),
            MachineMode::Int32 => self.asm.movl_ar(address_from_mem(mem), src.reg().into()),
            MachineMode::Int64 | MachineMode::Ptr | MachineMode::IntPtr => {
                self.asm.movq_ar(address_from_mem(mem), src.reg().into())
            }
            MachineMode::Float32 => self.asm.movss_ar(address_from_mem(mem), src.freg().into()),
            MachineMode::Float64 => self.asm.movsd_ar(address_from_mem(mem), src.freg().into()),
        }
    }

    pub fn store_zero(&mut self, mode: MachineMode, mem: Mem) {
        match mode {
            MachineMode::Int8 => self.asm.movb_ai(address_from_mem(mem), Immediate(0)),
            MachineMode::Float32 | MachineMode::Int32 => {
                self.asm.movl_ai(address_from_mem(mem), Immediate(0))
            }
            MachineMode::Float64 | MachineMode::Int64 | MachineMode::Ptr | MachineMode::IntPtr => {
                self.asm.movq_ai(address_from_mem(mem), Immediate(0))
            }
        }
    }

    pub fn copy_reg(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        self.mov_rr(mode.is64(), dest.into(), src.into());
    }

    pub fn copy_pc(&mut self, dest: Reg) {
        self.asm.lea(dest.into(), Address::rip(0));
    }

    pub fn copy_ra(&mut self, dest: Reg) {
        self.load_mem(MachineMode::Ptr, dest.into(), Mem::Base(REG_SP, 0));
    }

    pub fn copy_sp(&mut self, dest: Reg) {
        self.copy_reg(MachineMode::Ptr, dest, REG_SP);
    }

    pub fn set_sp(&mut self, src: Reg) {
        self.copy_reg(MachineMode::Ptr, REG_SP, src);
    }

    pub fn copy_freg(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        match mode {
            MachineMode::Float32 => self.asm.movss_rr(dest.into(), src.into()),
            MachineMode::Float64 => self.asm.movsd_rr(dest.into(), src.into()),
            _ => unreachable!(),
        }
    }

    pub fn extend_int_long(&mut self, dest: Reg, src: Reg) {
        self.asm.movsxlq_rr(dest.into(), src.into());
    }

    pub fn extend_byte(&mut self, _mode: MachineMode, dest: Reg, src: Reg) {
        self.asm.movzxb_rr(dest.into(), src.into());
    }

    pub fn load_constpool(&mut self, dest: Reg, disp: i32) {
        // next instruction has 7 bytes
        let disp = -(disp + 7);

        self.asm.movq_ra(dest.into(), Address::rip(disp)); // 7 bytes
    }

    pub fn call_reg(&mut self, reg: Reg) {
        self.asm.call_r(reg.into());
    }

    // emit debug instruction
    pub fn debug(&mut self) {
        self.asm.int3();
    }

    pub fn load_int_const(&mut self, mode: MachineMode, dest: Reg, imm: i64) {
        if imm == 0 {
            self.asm.xorl_rr(dest.into(), dest.into());
            return;
        }

        match mode {
            MachineMode::Int8 | MachineMode::Int32 => {
                self.asm.movl_ri(dest.into(), Immediate(imm));
            }
            MachineMode::Int64 | MachineMode::Ptr | MachineMode::IntPtr => {
                self.asm.movq_ri(dest.into(), Immediate(imm));
            }
            MachineMode::Float32 | MachineMode::Float64 => unreachable!(),
        }
    }

    pub fn load_float_const(&mut self, mode: MachineMode, dest: FReg, imm: f64) {
        if imm == 0.0 {
            self.asm.xorps_rr(dest.into(), dest.into());
            return;
        }

        let pos = self.pos() as i32;
        let inst_size = 8 + if dest.msb() != 0 { 1 } else { 0 };

        match mode {
            MachineMode::Float32 => {
                let off = self.dseg.add_f32(imm as f32);
                self.asm
                    .movss_ra(dest.into(), Address::rip(-(off + pos + inst_size)))
            }

            MachineMode::Float64 => {
                let off = self.dseg.add_f64(imm);
                self.asm
                    .movsd_ra(dest.into(), Address::rip(-(off + pos + inst_size)))
            }

            _ => unreachable!(),
        }
    }

    pub fn load_true(&mut self, dest: Reg) {
        self.asm.movl_ri(dest.into(), Immediate(1));
    }

    pub fn load_false(&mut self, dest: Reg) {
        self.asm.xorl_rr(dest.into(), dest.into());
    }

    pub fn int_neg(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        if mode.is64() {
            self.asm.negq(src.into());
        } else {
            self.asm.negl(src.into());
        }

        if dest != src {
            self.mov_rr(mode.is64(), dest.into(), src.into());
        }
    }

    pub fn int_not(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        if mode.is64() {
            self.asm.notq(src.into());
        } else {
            self.asm.notl(src.into());
        }

        if dest != src {
            self.mov_rr(mode.is64(), dest.into(), src.into());
        }
    }

    pub fn bool_not(&mut self, dest: Reg, src: Reg) {
        self.asm.xorl_ri(src.into(), Immediate(1));

        if dest != src {
            self.asm.movl_rr(dest.into(), src.into());
        }
    }

    pub fn float_add(&mut self, mode: MachineMode, dest: FReg, lhs: FReg, rhs: FReg) {
        match mode {
            MachineMode::Float32 => self.asm.addss_rr(lhs.into(), rhs.into()),
            MachineMode::Float64 => self.asm.addsd_rr(lhs.into(), rhs.into()),
            _ => unimplemented!(),
        }

        if dest != lhs {
            self.copy_freg(mode, dest, lhs);
        }
    }

    pub fn float_sub(&mut self, mode: MachineMode, dest: FReg, lhs: FReg, rhs: FReg) {
        match mode {
            MachineMode::Float32 => self.asm.subss_rr(lhs.into(), rhs.into()),
            MachineMode::Float64 => self.asm.subsd_rr(lhs.into(), rhs.into()),
            _ => unimplemented!(),
        }

        if dest != lhs {
            self.copy_freg(mode, dest, lhs);
        }
    }

    pub fn float_mul(&mut self, mode: MachineMode, dest: FReg, lhs: FReg, rhs: FReg) {
        match mode {
            MachineMode::Float32 => self.asm.mulss_rr(lhs.into(), rhs.into()),
            MachineMode::Float64 => self.asm.mulsd_rr(lhs.into(), rhs.into()),
            _ => unimplemented!(),
        }

        if dest != lhs {
            self.copy_freg(mode, dest, lhs);
        }
    }

    pub fn float_div(&mut self, mode: MachineMode, dest: FReg, lhs: FReg, rhs: FReg) {
        match mode {
            MachineMode::Float32 => self.asm.divss_rr(lhs.into(), rhs.into()),
            MachineMode::Float64 => self.asm.divsd_rr(lhs.into(), rhs.into()),
            _ => unimplemented!(),
        }

        if dest != lhs {
            self.copy_freg(mode, dest, lhs);
        }
    }

    pub fn float_neg(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        let (fst, snd) = if mode == MachineMode::Float32 {
            (1i32 << 31, 0)
        } else {
            (0, 1i32 << 31)
        };

        // align MMX data to 16 bytes
        self.dseg.align(16);
        self.dseg.add_i32(0);
        self.dseg.add_i32(0);
        self.dseg.add_i32(snd);
        let disp = self.dseg.add_i32(fst);

        let pos = self.pos() as i32;

        let xmm_reg: XmmRegister = src.into();

        let inst_size = 7
            + if mode == MachineMode::Float64 { 1 } else { 0 }
            + if xmm_reg.needs_rex() { 1 } else { 0 };

        let address = Address::rip(-(disp + pos + inst_size));

        match mode {
            MachineMode::Float32 => self.asm.xorps_ra(src.into(), address),
            MachineMode::Float64 => self.asm.xorpd_ra(src.into(), address),
            _ => unimplemented!(),
        }

        if dest != src {
            self.copy_freg(mode, dest, src);
        }
    }

    pub fn float_sqrt(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        match mode {
            MachineMode::Float32 => self.asm.sqrtss_rr(dest.into(), src.into()),
            MachineMode::Float64 => self.asm.sqrtsd_rr(dest.into(), src.into()),
            _ => unreachable!(),
        }
    }

    pub fn trap(&mut self, trap: Trap, pos: Position) {
        let vm = get_vm();
        self.load_int_const(MachineMode::Int32, REG_PARAMS[0], trap.int() as i64);
        self.raw_call(vm.trap_stub().to_ptr());
        self.emit_position(pos);
    }

    pub fn nop(&mut self) {
        self.asm.nop();
    }

    fn mov_rr(&mut self, x64: bool, lhs: AsmRegister, rhs: AsmRegister) {
        if x64 {
            self.asm.movq_rr(lhs, rhs);
        } else {
            self.asm.movl_rr(lhs, rhs);
        }
    }
}

fn convert_into_condition(cond: CondCode) -> Condition {
    match cond {
        CondCode::Zero => Condition::Zero,
        CondCode::NonZero => Condition::NotZero,
        CondCode::Equal => Condition::Equal,
        CondCode::NotEqual => Condition::NotEqual,
        CondCode::Less => Condition::Less,
        CondCode::LessEq => Condition::LessOrEqual,
        CondCode::Greater => Condition::Greater,
        CondCode::GreaterEq => Condition::GreaterOrEqual,
        CondCode::UnsignedGreater => Condition::Above, // above
        CondCode::UnsignedGreaterEq => Condition::AboveOrEqual, // above or equal
        CondCode::UnsignedLess => Condition::Below,    // below
        CondCode::UnsignedLessEq => Condition::BelowOrEqual, // below or equal
    }
}

impl From<FReg> for XmmRegister {
    fn from(reg: FReg) -> XmmRegister {
        XmmRegister::new(reg.0)
    }
}

impl MachineMode {
    pub fn is64(self) -> bool {
        match self {
            MachineMode::Int8 | MachineMode::Int32 => false,
            MachineMode::Int64 | MachineMode::Ptr => true,
            _ => unreachable!(),
        }
    }
}

fn address_from_mem(mem: Mem) -> Address {
    match mem {
        Mem::Local(offset) => Address::offset(REG_FP.into(), offset),
        Mem::Base(base, disp) => Address::offset(base.into(), disp),
        Mem::Index(base, index, scale, disp) => {
            let factor = match scale {
                1 => ScaleFactor::One,
                2 => ScaleFactor::Two,
                4 => ScaleFactor::Four,
                8 => ScaleFactor::Eight,
                _ => unreachable!(),
            };
            Address::array(base.into(), index.into(), factor, disp)
        }
        Mem::Offset(index, scale, disp) => {
            let factor = match scale {
                1 => ScaleFactor::One,
                2 => ScaleFactor::Two,
                4 => ScaleFactor::Four,
                8 => ScaleFactor::Eight,
                _ => unreachable!(),
            };
            Address::index(index.into(), factor, disp)
        }
    }
}
