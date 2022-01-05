use dora_parser::lexer::position::Position;

use crate::compiler::codegen::AnyReg;
use crate::compiler::fct::LazyCompilationSite;
use crate::cpu::*;
use crate::gc::swiper::CARD_SIZE_BITS;
use crate::masm::{CondCode, Label, MacroAssembler, Mem};
use crate::mem::ptr_width;
use crate::object::{offset_of_array_data, offset_of_array_length, Header};
use crate::threads::ThreadLocalData;
use crate::ty::{MachineMode, SourceTypeArray};
use crate::vm::{get_vm, FctDefinitionId, Trap};
use crate::vtable::VTable;
pub use dora_asm::arm64::AssemblerArm64 as Assembler;
use dora_asm::arm64::{self as asm, Cond, Extend, NeonRegister, Shift};

impl MacroAssembler {
    pub fn prolog(&mut self) -> usize {
        self.asm
            .stp_pre(REG_FP.into(), REG_LR.into(), REG_SP.into(), -2);
        self.asm.add(REG_FP.into(), REG_SP.into(), REG_ZERO.into());

        let patch_offset = self.pos();

        self.asm.movz(REG_TMP1.into(), 0, 0);
        self.asm.movk(REG_TMP1.into(), 0, 1);
        self.asm.sub(REG_SP.into(), REG_SP.into(), REG_TMP1.into());

        patch_offset
    }

    pub fn prolog_size(&mut self, stacksize: i32) {
        self.asm
            .stp_pre(REG_FP.into(), REG_LR.into(), REG_SP.into(), -2);
        self.asm.add(REG_FP.into(), REG_SP.into(), REG_ZERO.into());

        if stacksize > 0 {
            self.load_int_const(MachineMode::Ptr, REG_TMP1, stacksize as i64);
            self.asm.sub(REG_SP.into(), REG_SP.into(), REG_TMP1.into());
        }
    }

    pub fn patch_stacksize(&mut self, patch_offset: usize, stacksize: i32) {
        let stacksize = stacksize as u32;
        self.asm.set_position(patch_offset);
        self.asm.movz(REG_TMP1.into(), stacksize & 0xFFFF, 0);
        self.asm
            .movk(REG_TMP1.into(), (stacksize >> 16) & 0xFFFF, 1);
        self.asm.set_position_end();
    }

    pub fn check_stack_pointer(&mut self, lbl_overflow: Label) {
        let offset = ThreadLocalData::stack_limit_offset() as u32;
        assert!(offset % 8 == 0);
        self.asm
            .ldr_imm(REG_TMP1.into(), REG_THREAD.into(), offset / 8);
        self.asm
            .add(REG_TMP2.into(), REG_SP.into(), REG_ZERO.into());
        self.cmp_reg(MachineMode::Ptr, REG_TMP1, REG_TMP2);
        self.jump_if(CondCode::UnsignedGreater, lbl_overflow);
    }

    pub fn safepoint(&mut self, lbl_safepoint: Label) {
        let offset = ThreadLocalData::safepoint_requested_offset() as u32;
        self.asm
            .ldr_imm_b(REG_TMP1.into(), REG_THREAD.into(), offset);
        self.asm.cbnz(REG_TMP1.into(), lbl_safepoint);
    }

    pub fn fix_result(&mut self, _result: Reg, _mode: MachineMode) {
        // nothing to do on ARM64, see version for x64 for more info.
    }

    pub fn epilog(&mut self) {
        self.epilog_without_return();
        self.asm.ret(REG_LR.into());
    }

    pub fn epilog_without_return(&mut self) {
        self.asm.add(REG_SP.into(), REG_FP.into(), REG_ZERO.into());
        self.asm
            .ldp_post(REG_FP.into(), REG_LR.into(), REG_SP.into(), 2);
    }

    pub fn increase_stack_frame(&mut self, size: i32) {
        if size > 0 {
            self.load_int_const(MachineMode::Ptr, REG_TMP1, size as i64);
            self.asm.sub(REG_SP.into(), REG_SP.into(), REG_TMP1.into());
        }
    }

    pub fn decrease_stack_frame(&mut self, size: i32) {
        if size > 0 {
            self.load_int_const(MachineMode::Ptr, REG_TMP1, size as i64);
            self.asm.add(REG_SP.into(), REG_SP.into(), REG_TMP1.into());
        }
    }

    pub fn direct_call(
        &mut self,
        fct_id: FctDefinitionId,
        ptr: *const u8,
        type_params: SourceTypeArray,
    ) {
        let disp = self.add_addr(ptr);
        let pos = self.pos() as i32;

        let scratch = self.get_scratch();

        self.load_constpool(*scratch, disp + pos);
        self.asm.bl_r((*scratch).into());

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

        let scratch = self.get_scratch();

        self.load_constpool(*scratch, disp + pos);
        self.asm.bl_r((*scratch).into());
    }

    pub fn indirect_call(
        &mut self,
        pos: Position,
        fct_id: FctDefinitionId,
        vtable_index: u32,
        self_index: u32,
        type_params: SourceTypeArray,
    ) {
        let obj = REG_PARAMS[self_index as usize];
        self.test_if_nil_bailout(pos, obj, Trap::NIL);

        // need to use scratch register instead of REG_RESULT for calculations
        // since REG_RESULT (x0) is also the first parameter
        let scratch = self.get_scratch();

        // scratch = [obj] (load vtable)
        self.load_mem(MachineMode::Ptr, (*scratch).into(), Mem::Base(obj, 0));

        // calculate offset of VTable entry
        let disp = VTable::offset_of_method_table() + (vtable_index as i32) * ptr_width();

        // load vtable entry into scratch
        self.load_mem(
            MachineMode::Ptr,
            scratch.reg().into(),
            Mem::Base(*scratch, disp),
        );

        // call *scratch
        self.asm.bl_r((*scratch).into());
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

    pub fn set(&mut self, dest: Reg, op: CondCode) {
        self.asm.cset_w(dest.into(), op.into());
    }

    pub fn cmp_mem(&mut self, mode: MachineMode, mem: Mem, rhs: Reg) {
        let scratch = self.get_scratch();

        self.load_mem(mode, scratch.reg().into(), mem);
        self.cmp_reg(mode, *scratch, rhs);
    }

    pub fn cmp_mem_imm(&mut self, mode: MachineMode, mem: Mem, imm: i32) {
        let scratch1 = self.get_scratch();
        self.load_mem(mode, scratch1.reg().into(), mem);

        let scratch2 = self.get_scratch();
        self.load_int_const(mode, *scratch2, imm as i64);

        self.cmp_reg(mode, *scratch1, *scratch2);
    }

    pub fn cmp_reg(&mut self, mode: MachineMode, lhs: Reg, rhs: Reg) {
        match mode {
            MachineMode::Int8 | MachineMode::Int32 => self.asm.cmp_w(lhs.into(), rhs.into()),
            MachineMode::IntPtr | MachineMode::Ptr | MachineMode::Int64 => {
                self.asm.cmp(lhs.into(), rhs.into())
            }
            MachineMode::Float32 | MachineMode::Float64 => unimplemented!(),
        }
    }

    pub fn cmp_reg_imm(&mut self, mode: MachineMode, lhs: Reg, imm: i32) {
        let scratch = self.get_scratch();
        self.load_int_const(mode, *scratch, imm as i64);
        self.cmp_reg(mode, lhs, *scratch);
    }

    pub fn cmp_zero(&mut self, mode: MachineMode, lhs: Reg) {
        match mode {
            MachineMode::Int8 | MachineMode::Int32 => self.asm.cmp_imm_w(lhs.into(), 0, 0),
            MachineMode::Int64 | MachineMode::Ptr => self.asm.cmp_imm(lhs.into(), 0, 0),
            _ => unreachable!(),
        }
    }

    pub fn test_and_jump_if(&mut self, cond: CondCode, reg: Reg, lbl: Label) {
        assert!(cond == CondCode::Zero || cond == CondCode::NonZero);

        self.asm.cmp_imm(reg.into(), 0, 0);
        self.jump_if(cond, lbl);
    }

    pub fn jump_if(&mut self, cond: CondCode, target: Label) {
        self.asm.bc_l(cond.into(), target);
    }

    pub fn jump(&mut self, target: Label) {
        self.asm.b_l(target);
    }

    pub fn jump_reg(&mut self, reg: Reg) {
        self.asm.b_r(reg.into());
    }

    pub fn int_div(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg, pos: Position) {
        self.divmod_common(mode, dest, lhs, rhs, pos, true);
    }

    pub fn int_mod(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg, pos: Position) {
        self.divmod_common(mode, dest, lhs, rhs, pos, false);
    }

    fn divmod_common(
        &mut self,
        mode: MachineMode,
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
        pos: Position,
        is_div: bool,
    ) {
        let lbl_zero = self.create_label();
        let lbl_div = self.create_label();

        match mode {
            MachineMode::Int32 => self.asm.cbz(rhs.into(), lbl_zero),
            MachineMode::Int64 => self.asm.cbz_w(rhs.into(), lbl_zero),
            _ => unreachable!(),
        }

        self.emit_bailout(lbl_zero, Trap::DIV0, pos);

        let lbl_overflow = self.create_label();
        let scratch = self.get_scratch();
        match mode {
            MachineMode::Int32 => {
                self.asm.movz_w((*scratch).into(), 0x8000, 1);
                self.asm.cmp_w(lhs.into(), (*scratch).into());
                self.asm.bc_l(Cond::NE, lbl_div);
                self.asm.cmn_imm_w(rhs.into(), 1, 0);
                self.asm.bc_l(Cond::EQ, lbl_overflow);
            }

            MachineMode::Int64 => {
                self.asm.movz((*scratch).into(), 0x8000, 3);
                self.asm.cmp(lhs.into(), (*scratch).into());
                self.asm.bc_l(Cond::NE, lbl_div);
                self.asm.cmn_imm(rhs.into(), 1, 0);
                self.asm.bc_l(Cond::EQ, lbl_overflow);
            }

            _ => unreachable!(),
        }

        self.emit_bailout(lbl_overflow, Trap::OVERFLOW, pos);

        self.asm.bind_label(lbl_div);

        if is_div {
            match mode {
                MachineMode::Int32 => self.asm.sdiv_w(dest.into(), lhs.into(), rhs.into()),
                MachineMode::Int64 => self.asm.sdiv(dest.into(), lhs.into(), rhs.into()),
                _ => unreachable!(),
            }
        } else {
            let scratch = self.get_scratch();

            match mode {
                MachineMode::Int32 => {
                    self.asm.sdiv_w((*scratch).into(), lhs.into(), rhs.into());
                    self.asm
                        .msub(dest.into(), (*scratch).into(), rhs.into(), lhs.into());
                }
                MachineMode::Int64 => {
                    self.asm.sdiv((*scratch).into(), lhs.into(), rhs.into());
                    self.asm
                        .msub_w(dest.into(), (*scratch).into(), rhs.into(), lhs.into());
                }
                _ => unreachable!(),
            }
        }
    }

    pub fn int_mul(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        match mode {
            MachineMode::Int32 => self.asm.mul_w(dest.into(), lhs.into(), rhs.into()),
            MachineMode::Int64 => self.asm.mul(dest.into(), lhs.into(), rhs.into()),
            _ => panic!("unimplemented mode {:?}", mode),
        }
    }

    pub fn int_mul_checked(
        &mut self,
        mode: MachineMode,
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
        pos: Position,
    ) {
        match mode {
            MachineMode::Int32 => {
                self.asm.smull(dest.into(), lhs.into(), rhs.into());
                self.asm.cmp_ext(dest.into(), dest.into(), Extend::SXTW, 0);

                let lbl_overflow = self.create_label();
                self.asm.bc_l(Cond::NE, lbl_overflow);
                self.emit_bailout(lbl_overflow, Trap::OVERFLOW, pos);
            }
            MachineMode::Int64 => {
                let lbl_overflow = self.create_label();

                let tmp = self.get_scratch();
                let tmp_reg = *tmp;

                if lhs == dest {
                    self.asm.mov(tmp_reg.into(), lhs.into());
                } else if rhs == dest {
                    self.asm.mov(tmp_reg.into(), rhs.into());
                } else {
                    assert_ne!(dest, lhs);
                    assert_ne!(dest, rhs);
                }

                self.asm.mul(dest.into(), lhs.into(), rhs.into());

                if lhs == dest {
                    self.asm.smulh(tmp_reg.into(), tmp_reg.into(), rhs.into());
                } else if rhs == dest {
                    self.asm.smulh(tmp_reg.into(), lhs.into(), tmp_reg.into());
                } else {
                    self.asm.smulh(tmp_reg.into(), lhs.into(), rhs.into());
                }

                self.asm.cmp_sh(tmp_reg.into(), dest.into(), Shift::ASR, 63);
                self.asm.bc_l(Cond::NE, lbl_overflow);
                self.emit_bailout(lbl_overflow, Trap::OVERFLOW, pos);
            }
            _ => panic!("unimplemented mode {:?}", mode),
        }
    }

    pub fn int_add(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        match mode {
            MachineMode::Int32 => self.asm.add_w(dest.into(), lhs.into(), rhs.into()),
            MachineMode::Int64 | MachineMode::Ptr => {
                self.asm.add(dest.into(), lhs.into(), rhs.into())
            }
            _ => panic!("unimplemented mode {:?}", mode),
        }
    }

    pub fn int_add_checked(
        &mut self,
        mode: MachineMode,
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
        pos: Position,
    ) {
        match mode {
            MachineMode::Int32 => self.asm.adds_w(dest.into(), lhs.into(), rhs.into()),
            MachineMode::Int64 | MachineMode::Ptr => {
                self.asm.adds(dest.into(), lhs.into(), rhs.into())
            }
            _ => panic!("unimplemented mode {:?}", mode),
        }

        let lbl_overflow = self.create_label();
        self.asm.bc_l(Cond::VS, lbl_overflow);
        self.emit_bailout(lbl_overflow, Trap::OVERFLOW, pos);
    }

    pub fn int_add_imm(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, value: i64) {
        if (value as u32) as i64 == value && asm::fits_addsub_imm(value as u32) {
            match mode {
                MachineMode::Int32 => self.asm.add_imm_w(dest.into(), lhs.into(), value as u32, 0),
                MachineMode::Int64 | MachineMode::Ptr => {
                    self.asm.add_imm(dest.into(), lhs.into(), value as u32, 0)
                }
                _ => unreachable!(),
            }
        } else {
            let scratch = self.get_scratch();
            self.load_int_const(mode, *scratch, value);
            self.int_add(mode, dest, lhs, *scratch);
        }
    }

    pub fn int_sub(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        match mode {
            MachineMode::Int32 => self.asm.sub_w(dest.into(), lhs.into(), rhs.into()),
            MachineMode::Int64 => self.asm.sub(dest.into(), lhs.into(), rhs.into()),
            _ => panic!("unimplemented mode {:?}", mode),
        }
    }

    pub fn int_sub_checked(
        &mut self,
        mode: MachineMode,
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
        pos: Position,
    ) {
        match mode {
            MachineMode::Int32 => self.asm.subs_w(dest.into(), lhs.into(), rhs.into()),
            MachineMode::Int64 => self.asm.subs(dest.into(), lhs.into(), rhs.into()),
            _ => panic!("unimplemented mode {:?}", mode),
        }

        let lbl_overflow = self.create_label();
        self.asm.bc_l(Cond::VS, lbl_overflow);
        self.emit_bailout(lbl_overflow, Trap::OVERFLOW, pos);
    }

    pub fn int_shl(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        match mode {
            MachineMode::Int32 => self.asm.lslv_w(dest.into(), lhs.into(), rhs.into()),
            MachineMode::Int64 => self.asm.lslv(dest.into(), lhs.into(), rhs.into()),
            _ => panic!("unimplemented mode {:?}", mode),
        }
    }

    pub fn int_shr(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        match mode {
            MachineMode::Int32 => self.asm.lsrv_w(dest.into(), lhs.into(), rhs.into()),
            MachineMode::Int64 => self.asm.lsrv(dest.into(), lhs.into(), rhs.into()),
            _ => panic!("unimplemented mode {:?}", mode),
        }
    }

    pub fn int_sar(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        match mode {
            MachineMode::Int32 => self.asm.asrv_w(dest.into(), lhs.into(), rhs.into()),
            MachineMode::Int64 => self.asm.asrv(dest.into(), lhs.into(), rhs.into()),
            _ => panic!("unimplemented mode {:?}", mode),
        }
    }

    pub fn int_rol(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode),
        };

        let max = match mode {
            MachineMode::Int32 => 32,
            MachineMode::Int64 => 64,
            _ => panic!("unimplemented mode {:?}", mode),
        };

        let scratch = self.get_scratch();
        self.load_int_const(mode, *scratch, max);

        if x64 != 0 {
            self.asm
                .sub((*scratch).into(), (*scratch).into(), rhs.into());
        } else {
            self.asm
                .sub_w((*scratch).into(), (*scratch).into(), rhs.into());
        }

        match mode {
            MachineMode::Int32 => self.asm.rorv_w(dest.into(), lhs.into(), (*scratch).into()),
            MachineMode::Int64 => self.asm.rorv(dest.into(), lhs.into(), (*scratch).into()),
            _ => panic!("unimplemented mode {:?}", mode),
        }
    }

    pub fn int_ror(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        match mode {
            MachineMode::Int32 => self.asm.rorv_w(dest.into(), lhs.into(), rhs.into()),
            MachineMode::Int64 => self.asm.rorv(dest.into(), lhs.into(), rhs.into()),
            _ => panic!("unimplemented mode {:?}", mode),
        }
    }

    pub fn int_or(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        match mode {
            MachineMode::Int32 => {
                self.asm
                    .orr_sh_w(dest.into(), lhs.into(), rhs.into(), Shift::LSL, 0)
            }
            MachineMode::Int64 => {
                self.asm
                    .orr_sh(dest.into(), lhs.into(), rhs.into(), Shift::LSL, 0)
            }
            _ => unreachable!(),
        }
    }

    pub fn int_and(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        match mode {
            MachineMode::Int32 => {
                self.asm
                    .and_sh_w(dest.into(), lhs.into(), rhs.into(), Shift::LSL, 0)
            }
            MachineMode::Int64 => {
                self.asm
                    .and_sh(dest.into(), lhs.into(), rhs.into(), Shift::LSL, 0)
            }
            _ => panic!("unimplemented mode {:?}", mode),
        }
    }

    pub fn int_xor(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        match mode {
            MachineMode::Int32 => {
                self.asm
                    .eor_sh_w(dest.into(), lhs.into(), rhs.into(), Shift::LSL, 0)
            }
            MachineMode::Int64 => {
                self.asm
                    .eor_sh(dest.into(), lhs.into(), rhs.into(), Shift::LSL, 0)
            }
            _ => panic!("unimplemented mode {:?}", mode),
        }
    }

    pub fn count_bits(&mut self, mode: MachineMode, dest: Reg, src: Reg, count_one_bits: bool) {
        let scratch = FREG_TMP1;
        let scratch_src = self.get_scratch();

        let src = if count_one_bits {
            src
        } else {
            self.int_not(mode, *scratch_src, src);
            *scratch_src
        };

        match mode {
            MachineMode::Int32 => {
                self.asm.fmov_fs_ws(scratch.into(), src.into());
                self.asm.cnt(0, 0b00, scratch.into(), scratch.into());
                self.asm.addv(0, 0b00, scratch.into(), scratch.into());
                self.asm.fmov_sf_ws(dest.into(), scratch.into());
            }

            MachineMode::Int64 => {
                self.asm.fmov_fs_d(scratch.into(), src.into());
                self.asm.cnt(0, 0b00, scratch.into(), scratch.into());
                self.asm.addv(0, 0b00, scratch.into(), scratch.into());
                self.asm.fmov_sf_d(dest.into(), scratch.into());
            }
            _ => unimplemented!(),
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
            self.int_not(mode, dest, src);

            match mode {
                MachineMode::Int32 => self.asm.clz_w(dest.into(), dest.into()),
                MachineMode::Int64 => self.asm.clz(dest.into(), dest.into()),
                _ => panic!("unimplemented mode {:?}", mode),
            }
        } else {
            match mode {
                MachineMode::Int32 => self.asm.clz_w(dest.into(), src.into()),
                MachineMode::Int64 => self.asm.clz(dest.into(), src.into()),
                _ => panic!("unimplemented mode {:?}", mode),
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
        match mode {
            MachineMode::Int32 => {
                self.asm.rbit_w(dest.into(), src.into());

                if count_one_bits {
                    self.int_not(mode, dest, dest);
                }

                self.asm.clz_w(dest.into(), dest.into());
            }
            MachineMode::Int64 => {
                self.asm.rbit(dest.into(), src.into());

                if count_one_bits {
                    self.int_not(mode, dest, dest);
                }

                self.asm.clz(dest.into(), dest.into());
            }
            _ => panic!("unimplemented mode {:?}", mode),
        };
    }

    pub fn int_to_float(
        &mut self,
        dest_mode: MachineMode,
        dest: FReg,
        src_mode: MachineMode,
        src: Reg,
    ) {
        match (dest_mode, src_mode) {
            (MachineMode::Float32, MachineMode::Int32) => {
                self.asm.scvtf_ws(dest.into(), src.into())
            }
            (MachineMode::Float32, MachineMode::Int64) => {
                self.asm.scvtf_ws(dest.into(), src.into())
            }
            (MachineMode::Float64, MachineMode::Int32) => {
                self.asm.scvtf_wd(dest.into(), src.into())
            }
            (MachineMode::Float64, MachineMode::Int64) => {
                self.asm.scvtf_wd(dest.into(), src.into())
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
        match (dest_mode, src_mode) {
            (MachineMode::Int32, MachineMode::Float32) => {
                self.asm.fcvtzs_ws(dest.into(), src.into())
            }
            (MachineMode::Int32, MachineMode::Float64) => {
                self.asm.fcvtzs_wd(dest.into(), src.into())
            }
            (MachineMode::Int64, MachineMode::Float32) => {
                self.asm.fcvtzs_s(dest.into(), src.into())
            }
            (MachineMode::Int64, MachineMode::Float64) => {
                self.asm.fcvtzs_d(dest.into(), src.into())
            }
            _ => unreachable!(),
        }
    }

    pub fn float32_to_float64(&mut self, dest: FReg, src: FReg) {
        self.asm.fcvt_sd(dest.into(), src.into());
    }

    pub fn float64_to_float32(&mut self, dest: FReg, src: FReg) {
        self.asm.fcvt_ds(dest.into(), src.into());
    }

    pub fn int_as_float(
        &mut self,
        dest_mode: MachineMode,
        dest: FReg,
        src_mode: MachineMode,
        src: Reg,
    ) {
        match (dest_mode, src_mode) {
            (MachineMode::Float32, MachineMode::Int32) => {
                self.asm.fmov_fs_ws(dest.into(), src.into());
            }
            (MachineMode::Float64, MachineMode::Int64) => {
                self.asm.fmov_fs_d(dest.into(), src.into());
            }
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
        match (dest_mode, src_mode) {
            (MachineMode::Int32, MachineMode::Float32) => {
                self.asm.fmov_sf_ws(dest.into(), src.into())
            }
            (MachineMode::Int64, MachineMode::Float64) => {
                self.asm.fmov_sf_d(dest.into(), src.into())
            }
            _ => unreachable!(),
        }
    }

    pub fn float_add(&mut self, mode: MachineMode, dest: FReg, lhs: FReg, rhs: FReg) {
        match mode {
            MachineMode::Float32 => self.asm.fadd_s(dest.into(), lhs.into(), rhs.into()),
            MachineMode::Float64 => self.asm.fadd_d(dest.into(), lhs.into(), rhs.into()),
            _ => unreachable!(),
        };
    }

    pub fn float_sub(&mut self, mode: MachineMode, dest: FReg, lhs: FReg, rhs: FReg) {
        match mode {
            MachineMode::Float32 => self.asm.fsub_s(dest.into(), lhs.into(), rhs.into()),
            MachineMode::Float64 => self.asm.fsub_d(dest.into(), lhs.into(), rhs.into()),
            _ => unreachable!(),
        }
    }

    pub fn float_mul(&mut self, mode: MachineMode, dest: FReg, lhs: FReg, rhs: FReg) {
        match mode {
            MachineMode::Float32 => self.asm.fmul_s(dest.into(), lhs.into(), rhs.into()),
            MachineMode::Float64 => self.asm.fmul_d(dest.into(), lhs.into(), rhs.into()),
            _ => unimplemented!(),
        }
    }

    pub fn float_div(&mut self, mode: MachineMode, dest: FReg, lhs: FReg, rhs: FReg) {
        match mode {
            MachineMode::Float32 => self.asm.fdiv_s(dest.into(), lhs.into(), rhs.into()),
            MachineMode::Float64 => self.asm.fdiv_d(dest.into(), lhs.into(), rhs.into()),
            _ => unimplemented!(),
        }
    }

    pub fn float_abs(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        match mode {
            MachineMode::Float32 => self.asm.fabs_s(dest.into(), src.into()),
            MachineMode::Float64 => self.asm.fabs_d(dest.into(), src.into()),
            _ => unimplemented!(),
        }
    }

    pub fn float_neg(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        match mode {
            MachineMode::Float32 => self.asm.fneg_s(dest.into(), src.into()),
            MachineMode::Float64 => self.asm.fneg_d(dest.into(), src.into()),
            _ => unimplemented!(),
        }
    }

    pub fn float_round_tozero(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        match mode {
            MachineMode::Float32 => self.asm.frintz_s(dest.into(), src.into()),
            MachineMode::Float64 => self.asm.frintz_d(dest.into(), src.into()),
            _ => unimplemented!(),
        }
    }

    pub fn float_round_up(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        match mode {
            MachineMode::Float32 => self.asm.frintp_s(dest.into(), src.into()),
            MachineMode::Float64 => self.asm.frintp_d(dest.into(), src.into()),
            _ => unimplemented!(),
        }
    }

    pub fn float_round_down(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        match mode {
            MachineMode::Float32 => self.asm.frintm_s(dest.into(), src.into()),
            MachineMode::Float64 => self.asm.frintm_d(dest.into(), src.into()),
            _ => unimplemented!(),
        }
    }

    pub fn float_round_halfeven(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        match mode {
            MachineMode::Float32 => self.asm.frintn_s(dest.into(), src.into()),
            MachineMode::Float64 => self.asm.frintn_d(dest.into(), src.into()),
            _ => unimplemented!(),
        }
    }

    pub fn float_sqrt(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        match mode {
            MachineMode::Float32 => self.asm.fsqrt_s(dest.into(), src.into()),
            MachineMode::Float64 => self.asm.fsqrt_d(dest.into(), src.into()),
            _ => unimplemented!(),
        }
    }

    pub fn cmp_int(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        match mode {
            MachineMode::Int8 | MachineMode::Int32 => {
                self.asm.cmp_w(lhs.into(), rhs.into());
            }

            MachineMode::Int64 => {
                self.asm.cmp(lhs.into(), rhs.into());
            }

            _ => unreachable!(),
        }

        self.asm.cset_w(dest.into(), Cond::NE);
        self.asm
            .csinv_w(dest.into(), dest.into(), REG_ZERO.into(), Cond::GE);
    }

    pub fn float_cmp_int(&mut self, mode: MachineMode, dest: Reg, lhs: FReg, rhs: FReg) {
        match mode {
            MachineMode::Float32 => self.asm.fcmp_s(lhs.into(), rhs.into()),
            MachineMode::Float64 => self.asm.fcmp_d(lhs.into(), rhs.into()),
            _ => unimplemented!(),
        }

        self.asm.cset_w(dest.into(), Cond::GT);
        let scratch = self.get_scratch();
        self.asm.movn_w((*scratch).into(), 0, 0);
        self.asm
            .csel_w(dest.into(), (*scratch).into(), dest.into(), Cond::MI);
    }

    pub fn float_cmp(
        &mut self,
        mode: MachineMode,
        dest: Reg,
        lhs: FReg,
        rhs: FReg,
        cond: CondCode,
    ) {
        let cond = match cond {
            CondCode::Equal => Cond::EQ,
            CondCode::NotEqual => Cond::NE,
            CondCode::Greater => Cond::GT,
            CondCode::GreaterEq => Cond::GE,
            CondCode::Less => Cond::MI,
            CondCode::LessEq => Cond::LS,
            _ => unreachable!(),
        };

        match mode {
            MachineMode::Float32 => self.asm.fcmp_s(lhs.into(), rhs.into()),
            MachineMode::Float64 => self.asm.fcmp_d(lhs.into(), rhs.into()),
            _ => unimplemented!(),
        }

        self.asm.cset_w(dest.into(), cond);
    }

    pub fn float_cmp_nan(&mut self, mode: MachineMode, dest: Reg, src: FReg) {
        match mode {
            MachineMode::Float32 => self.asm.fcmp_s(src.into(), src.into()),
            MachineMode::Float64 => self.asm.fcmp_d(src.into(), src.into()),
            _ => unimplemented!(),
        }

        self.asm.cset_w(dest.into(), Cond::VS);
    }

    pub fn load_float_const(&mut self, mode: MachineMode, dest: FReg, imm: f64) {
        let off = match mode {
            MachineMode::Float32 => self.dseg.add_f32(imm as f32),
            MachineMode::Float64 => self.dseg.add_f64(imm),
            _ => unreachable!(),
        };

        let pos = self.pos() as i32;
        let disp = off + pos;

        let scratch = self.get_scratch();
        self.asm.adr_imm((*scratch).into(), -disp);

        self.load_mem(mode, dest.into(), Mem::Base(*scratch, 0));
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

        if element_size == 1 {
            self.asm.add_imm(dest.into(), length.into(), size as u32, 0);
        } else if element_size == 2 || element_size == 4 || element_size == 8 {
            let shift = match element_size {
                2 => 1,
                4 => 2,
                8 => 3,
                _ => unreachable!(),
            };

            self.asm.lsl_imm(dest.into(), length.into(), shift);
            self.asm.add_imm(dest.into(), dest.into(), size as u32, 0);
        } else {
            let scratch = self.get_scratch();
            self.load_int_const(MachineMode::Ptr, *scratch, element_size as i64);
            self.asm.mul(dest.into(), length.into(), (*scratch).into());
            self.asm.add_imm(dest.into(), dest.into(), size as u32, 0);
        }

        if element_size != ptr_width() {
            self.asm
                .and_imm(dest.into(), dest.into(), -ptr_width() as u64);
        }
    }

    pub fn array_address(&mut self, dest: Reg, obj: Reg, index: Reg, element_size: i32) {
        let offset = Header::size() + ptr_width();
        let scratch = self.get_scratch();

        self.load_int_const(MachineMode::Ptr, *scratch, element_size as i64);
        self.asm
            .mul((*scratch).into(), index.into(), (*scratch).into());
        self.asm
            .add_imm((*scratch).into(), (*scratch).into(), offset as u32, 0);
        self.asm.add(dest.into(), obj.into(), (*scratch).into());
    }

    pub fn check_index_out_of_bounds(&mut self, pos: Position, array: Reg, index: Reg) {
        let scratch = self.get_scratch();
        self.load_mem(
            MachineMode::Int64,
            (*scratch).into(),
            Mem::Base(array, offset_of_array_length()),
        );
        self.cmp_reg(MachineMode::Int64, index, *scratch);

        let lbl = self.create_label();
        self.jump_if(CondCode::UnsignedGreaterEq, lbl);
        self.emit_bailout(lbl, Trap::INDEX_OUT_OF_BOUNDS, pos);
    }

    pub fn load_nil(&mut self, dest: Reg) {
        self.asm.movz(dest.into(), 0, 0);
    }

    pub fn load_int32_synchronized(&mut self, dest: Reg, address: Reg) {
        self.asm.ldar_w(dest.into(), address.into());
    }

    pub fn load_int64_synchronized(&mut self, dest: Reg, address: Reg) {
        self.asm.ldar(dest.into(), address.into());
    }

    pub fn store_int32_synchronized(&mut self, dest: Reg, address: Reg) {
        self.asm.stlr_w(dest.into(), address.into());
    }

    pub fn store_int64_synchronized(&mut self, dest: Reg, address: Reg) {
        self.asm.stlr(dest.into(), address.into());
    }

    pub fn exchange_int32_synchronized(&mut self, old: Reg, new: Reg, address: Reg) {
        if has_lse_atomics() {
            self.asm.swpal_w(new.into(), old.into(), address.into());
        } else {
            let scratch = self.get_scratch();
            let loop_start = self.asm.create_and_bind_label();
            self.asm.ldaxr_w(old.into(), address.into());
            self.asm
                .stlxr_w((*scratch).into(), new.into(), address.into());
            self.asm.cbnz_w((*scratch).into(), loop_start);
        }
    }

    pub fn exchange_int64_synchronized(&mut self, old: Reg, new: Reg, address: Reg) {
        if has_lse_atomics() {
            self.asm.swpal(new.into(), old.into(), address.into());
        } else {
            let scratch = self.get_scratch();
            let loop_start = self.asm.create_and_bind_label();
            self.asm.ldaxr(old.into(), address.into());
            self.asm
                .stlxr((*scratch).into(), new.into(), address.into());
            self.asm.cbnz_w((*scratch).into(), loop_start);
        }
    }

    pub fn compare_exchange_int32_synchronized(
        &mut self,
        expected: Reg,
        new: Reg,
        address: Reg,
    ) -> Reg {
        if has_lse_atomics() {
            self.asm
                .casal_w(expected.into(), new.into(), address.into());
            expected
        } else {
            let current = self.get_scratch();
            let state = self.get_scratch();
            let loop_start = self.asm.create_and_bind_label();
            let loop_end = self.asm.create_label();
            self.asm.ldaxr_w((*current).into(), address.into());
            self.asm.cmp_w((*current).into(), expected.into());
            self.asm.bc_l(Cond::NE, loop_end);
            self.asm
                .stlxr_w((*state).into(), new.into(), address.into());
            self.asm.cbnz_w((*state).into(), loop_start);
            self.asm.bind_label(loop_end);

            *current
        }
    }

    pub fn compare_exchange_int64_synchronized(
        &mut self,
        expected: Reg,
        new: Reg,
        address: Reg,
    ) -> Reg {
        if has_lse_atomics() {
            self.asm.casal(expected.into(), new.into(), address.into());
            expected
        } else {
            let current = self.get_scratch();
            let state = self.get_scratch();
            let loop_start = self.asm.create_and_bind_label();
            let loop_end = self.asm.create_label();
            self.asm.ldaxr((*current).into(), address.into());
            self.asm.cmp((*current).into(), expected.into());
            self.asm.bc_l(Cond::NE, loop_end);
            self.asm.stlxr((*state).into(), new.into(), address.into());
            self.asm.cbnz((*state).into(), loop_start);
            self.asm.bind_label(loop_end);

            *current
        }
    }

    pub fn fetch_add_int32_synchronized(&mut self, previous: Reg, value: Reg, address: Reg) -> Reg {
        if has_lse_atomics() {
            self.asm
                .ldaddal_w(value.into(), previous.into(), address.into());
            previous
        } else {
            let new_value = self.get_scratch();
            let state = self.get_scratch();

            let loop_start = self.asm.create_and_bind_label();
            self.asm.ldaxr_w(previous.into(), address.into());
            self.asm
                .add_w((*new_value).into(), previous.into(), value.into());
            self.asm
                .stlxr_w((*state).into(), (*new_value).into(), address.into());
            self.asm.cbnz_w((*state).into(), loop_start);

            previous
        }
    }

    pub fn fetch_add_int64_synchronized(&mut self, previous: Reg, value: Reg, address: Reg) -> Reg {
        if has_lse_atomics() {
            self.asm
                .ldaddal(value.into(), previous.into(), address.into());
            previous
        } else {
            let new_value = self.get_scratch();
            let state = self.get_scratch();

            let loop_start = self.asm.create_and_bind_label();
            self.asm.ldaxr(previous.into(), address.into());
            self.asm
                .add((*new_value).into(), previous.into(), value.into());
            self.asm
                .stlxr((*state).into(), (*new_value).into(), address.into());
            self.asm.cbnz_w((*state).into(), loop_start);

            previous
        }
    }

    pub fn load_mem(&mut self, mode: MachineMode, dest: AnyReg, mem: Mem) {
        match mem {
            Mem::Local(offset) => {
                self.common_load_base_with_offset(mode, dest, REG_FP, offset);
            }

            Mem::Base(base, disp) => {
                self.common_load_base_with_offset(mode, dest, base, disp);
            }

            Mem::Index(base, index, scale, disp) => {
                assert!(mode.size() == scale);

                let scratch = self.get_scratch();
                self.load_int_const(MachineMode::Ptr, *scratch, disp as i64);
                self.asm
                    .add((*scratch).into(), (*scratch).into(), base.into());

                match mode {
                    MachineMode::Int8 => self.asm.ldr_ind_b(
                        dest.reg().into(),
                        (*scratch).into(),
                        index.into(),
                        Extend::LSL,
                        0,
                    ),
                    MachineMode::Int32 => self.asm.ldr_ind_w(
                        dest.reg().into(),
                        (*scratch).into(),
                        index.into(),
                        Extend::LSL,
                        1,
                    ),
                    MachineMode::IntPtr | MachineMode::Int64 | MachineMode::Ptr => {
                        self.asm.ldr_ind(
                            dest.reg().into(),
                            (*scratch).into(),
                            index.into(),
                            Extend::LSL,
                            1,
                        )
                    }
                    MachineMode::Float32 => self.asm.ldr_ind_s(
                        dest.freg().into(),
                        (*scratch).into(),
                        index.into(),
                        Extend::LSL,
                        1,
                    ),
                    MachineMode::Float64 => self.asm.ldr_ind_d(
                        dest.freg().into(),
                        (*scratch).into(),
                        index.into(),
                        Extend::LSL,
                        1,
                    ),
                }
            }

            Mem::Offset(_, _, _) => unimplemented!(),
        }
    }

    fn common_load_base_with_offset(
        &mut self,
        mode: MachineMode,
        dest: AnyReg,
        base: Reg,
        disp: i32,
    ) {
        if disp >= 0 && disp % mode.size() == 0 && asm::fits_addsub_imm((disp / mode.size()) as u32)
        {
            let disp = (disp / mode.size()) as u32;
            match mode {
                MachineMode::Int8 => self.asm.ldr_imm_b(dest.reg().into(), base.into(), disp),
                MachineMode::Int32 => self.asm.ldr_imm_w(dest.reg().into(), base.into(), disp),
                MachineMode::IntPtr | MachineMode::Int64 | MachineMode::Ptr => {
                    self.asm.ldr_imm(dest.reg().into(), base.into(), disp)
                }
                MachineMode::Float32 => self.asm.ldr_imm_s(dest.freg().into(), base.into(), disp),
                MachineMode::Float64 => self.asm.ldr_imm_d(dest.freg().into(), base.into(), disp),
            }
        } else if asm::fits_ldst_unscaled(disp) {
            match mode {
                MachineMode::Int8 => self
                    .asm
                    .ldr_unscaled_b(dest.reg().into(), base.into(), disp),
                MachineMode::Int32 => self
                    .asm
                    .ldr_unscaled_w(dest.reg().into(), base.into(), disp),
                MachineMode::IntPtr | MachineMode::Int64 | MachineMode::Ptr => {
                    self.asm.ldr_unscaled(dest.reg().into(), base.into(), disp)
                }
                MachineMode::Float32 => {
                    self.asm
                        .ldr_unscaled_s(dest.freg().into(), base.into(), disp)
                }
                MachineMode::Float64 => {
                    self.asm
                        .ldr_unscaled_d(dest.freg().into(), base.into(), disp)
                }
            }
        } else {
            let scratch = self.get_scratch();
            self.load_int_const(MachineMode::Ptr, *scratch, disp as i64);
            match mode {
                MachineMode::Int8 => self.asm.ldr_ind_b(
                    dest.reg().into(),
                    base.into(),
                    (*scratch).into(),
                    Extend::LSL,
                    0,
                ),
                MachineMode::Int32 => self.asm.ldr_ind_w(
                    dest.reg().into(),
                    base.into(),
                    (*scratch).into(),
                    Extend::LSL,
                    0,
                ),
                MachineMode::IntPtr | MachineMode::Int64 | MachineMode::Ptr => self.asm.ldr_ind(
                    dest.reg().into(),
                    base.into(),
                    (*scratch).into(),
                    Extend::LSL,
                    0,
                ),
                MachineMode::Float32 => self.asm.ldr_ind_s(
                    dest.freg().into(),
                    base.into(),
                    (*scratch).into(),
                    Extend::LSL,
                    0,
                ),
                MachineMode::Float64 => self.asm.ldr_ind_d(
                    dest.freg().into(),
                    base.into(),
                    (*scratch).into(),
                    Extend::LSL,
                    0,
                ),
            }
        }
    }

    pub fn lea(&mut self, dest: Reg, mem: Mem) {
        match mem {
            Mem::Local(offset) => {
                if asm::fits_addsub_imm(offset as u32) {
                    self.asm
                        .add_imm(dest.into(), REG_FP.into(), offset as u32, 0);
                } else {
                    let scratch = self.get_scratch();
                    self.load_int_const(MachineMode::Ptr, *scratch, offset as i64);
                    self.asm.add(dest.into(), REG_FP.into(), (*scratch).into());
                }
            }

            Mem::Base(base, disp) => {
                if asm::fits_addsub_imm(disp as u32) {
                    self.asm.add_imm(dest.into(), base.into(), disp as u32, 0);
                } else {
                    let scratch = self.get_scratch();
                    self.load_int_const(MachineMode::Ptr, *scratch, disp as i64);
                    self.asm.add(dest.into(), base.into(), (*scratch).into());
                }
            }

            Mem::Index(base, index, scale, disp) => {
                let scratch = self.get_scratch();

                if asm::fits_addsub_imm(disp as u32) {
                    self.asm
                        .add_imm((*scratch).into(), base.into(), disp as u32, 0);
                } else {
                    self.load_int_const(MachineMode::Ptr, *scratch, disp as i64);
                    self.asm
                        .add((*scratch).into(), base.into(), (*scratch).into());
                }

                let shift = match scale {
                    1 => 0,
                    2 => 1,
                    4 => 2,
                    8 => 3,
                    _ => unimplemented!(),
                };

                self.asm.add_sh(
                    dest.into(),
                    (*scratch).into(),
                    index.into(),
                    Shift::LSL,
                    shift,
                );
            }

            Mem::Offset(_, _, _) => unimplemented!(),
        }
    }

    pub fn emit_barrier(&mut self, src: Reg, card_table_offset: usize) {
        let scratch1 = self.get_scratch();
        self.asm
            .lsr_imm((*scratch1).into(), src.into(), CARD_SIZE_BITS as u32);
        let scratch2 = self.get_scratch();
        self.load_int_const(MachineMode::Ptr, *scratch2, card_table_offset as i64);
        self.asm.str_ind_b(
            REG_ZERO.into(),
            (*scratch1).into(),
            (*scratch2).into(),
            Extend::LSL,
            0,
        );
    }

    pub fn store_mem(&mut self, mode: MachineMode, mem: Mem, src: AnyReg) {
        match mem {
            Mem::Local(offset) => {
                self.common_store_base_with_offset(mode, src, REG_FP, offset);
            }

            Mem::Base(base, disp) => {
                self.common_store_base_with_offset(mode, src, base, disp);
            }

            Mem::Index(base, index, scale, disp) => {
                assert!(mode.size() == scale);

                let scratch = self.get_scratch();
                self.load_int_const(MachineMode::Ptr, *scratch, disp as i64);
                self.asm
                    .add((*scratch).into(), (*scratch).into(), base.into());

                match mode {
                    MachineMode::Int8 => self.asm.str_ind_b(
                        src.reg().into(),
                        (*scratch).into(),
                        index.into(),
                        Extend::LSL,
                        0,
                    ),
                    MachineMode::Int32 => self.asm.str_ind_w(
                        src.reg().into(),
                        (*scratch).into(),
                        index.into(),
                        Extend::LSL,
                        1,
                    ),
                    MachineMode::IntPtr | MachineMode::Int64 | MachineMode::Ptr => {
                        self.asm.str_ind(
                            src.reg().into(),
                            (*scratch).into(),
                            index.into(),
                            Extend::LSL,
                            1,
                        )
                    }
                    MachineMode::Float32 => self.asm.str_ind_s(
                        src.freg().into(),
                        (*scratch).into(),
                        index.into(),
                        Extend::LSL,
                        1,
                    ),
                    MachineMode::Float64 => self.asm.str_ind_d(
                        src.freg().into(),
                        (*scratch).into(),
                        index.into(),
                        Extend::LSL,
                        1,
                    ),
                }
            }

            Mem::Offset(_, _, _) => unimplemented!(),
        }
    }

    pub fn store_zero(&mut self, mode: MachineMode, mem: Mem) {
        self.store_mem(mode, mem, REG_ZERO.into());
    }

    pub fn common_store_base_with_offset(
        &mut self,
        mode: MachineMode,
        src: AnyReg,
        base: Reg,
        offset: i32,
    ) {
        if offset >= 0
            && offset % mode.size() == 0
            && asm::fits_addsub_imm((offset / mode.size()) as u32)
        {
            let offset = (offset / mode.size()) as u32;
            match mode {
                MachineMode::Int8 => self.asm.str_imm_b(src.reg().into(), base.into(), offset),
                MachineMode::Int32 => self.asm.str_imm_w(src.reg().into(), base.into(), offset),
                MachineMode::IntPtr | MachineMode::Int64 | MachineMode::Ptr => {
                    self.asm.str_imm(src.reg().into(), base.into(), offset)
                }
                MachineMode::Float32 => self.asm.str_imm_s(src.freg().into(), base.into(), offset),
                MachineMode::Float64 => self.asm.str_imm_d(src.freg().into(), base.into(), offset),
            }
        } else if asm::fits_ldst_unscaled(offset) {
            match mode {
                MachineMode::Int8 => self
                    .asm
                    .str_unscaled_b(src.reg().into(), base.into(), offset),
                MachineMode::Int32 => {
                    self.asm
                        .str_unscaled_w(src.reg().into(), base.into(), offset)
                }
                MachineMode::IntPtr | MachineMode::Int64 | MachineMode::Ptr => {
                    self.asm.str_unscaled(src.reg().into(), base.into(), offset)
                }
                MachineMode::Float32 => {
                    self.asm
                        .str_unscaled_s(src.freg().into(), base.into(), offset)
                }
                MachineMode::Float64 => {
                    self.asm
                        .str_unscaled_d(src.freg().into(), base.into(), offset)
                }
            }
        } else {
            let scratch = self.get_scratch();
            self.load_int_const(MachineMode::Ptr, *scratch, offset as i64);
            match mode {
                MachineMode::Int8 => self.asm.str_ind_b(
                    src.reg().into(),
                    base.into(),
                    (*scratch).into(),
                    Extend::LSL,
                    0,
                ),
                MachineMode::Int32 => self.asm.str_ind_w(
                    src.reg().into(),
                    base.into(),
                    (*scratch).into(),
                    Extend::LSL,
                    0,
                ),
                MachineMode::IntPtr | MachineMode::Int64 | MachineMode::Ptr => self.asm.str_ind(
                    src.reg().into(),
                    base.into(),
                    (*scratch).into(),
                    Extend::LSL,
                    0,
                ),
                MachineMode::Float32 => self.asm.str_ind_s(
                    src.freg().into(),
                    base.into(),
                    (*scratch).into(),
                    Extend::LSL,
                    0,
                ),
                MachineMode::Float64 => self.asm.str_ind_d(
                    src.freg().into(),
                    base.into(),
                    (*scratch).into(),
                    Extend::LSL,
                    0,
                ),
            };
        }
    }

    pub fn copy_reg(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        match mode {
            MachineMode::Int32 | MachineMode::Int8 => {
                self.asm.mov_w(dest.into(), src.into());
            }
            MachineMode::Ptr | MachineMode::Int64 => {
                self.asm.mov(dest.into(), src.into());
            }
            _ => unreachable!(),
        }
    }

    pub fn copy_sp(&mut self, dest: Reg) {
        self.asm.add_imm(dest.into(), REG_SP.into(), 0, 0);
    }

    pub fn set_sp(&mut self, src: Reg) {
        self.asm.add_imm(REG_SP.into(), src.into(), 0, 0);
    }

    pub fn copy_pc(&mut self, dest: Reg) {
        self.asm.adr_imm(dest.into(), 0);
    }

    pub fn copy_ra(&mut self, dest: Reg) {
        self.copy_reg(MachineMode::Ptr, dest, REG_LR);
    }

    pub fn copy_freg(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        match mode {
            MachineMode::Float32 => self.asm.fmov_s(dest.into(), src.into()),
            MachineMode::Float64 => self.asm.fmov_d(dest.into(), src.into()),
            _ => unreachable!(),
        }
    }

    pub fn extend_int_long(&mut self, dest: Reg, src: Reg) {
        self.asm.sxtw(dest.into(), src.into());
    }

    pub fn extend_byte(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        match mode {
            MachineMode::Int32 => {}
            MachineMode::Int64 => self.asm.uxtw(dest.into(), src.into()),
            _ => panic!("unimplemented mode {:?}", mode),
        }
    }

    pub fn load_constpool(&mut self, dest: Reg, disp: i32) {
        self.asm.adr_imm(dest.into(), -disp);
        self.load_mem(MachineMode::Ptr, dest.into(), Mem::Base(dest, 0));
    }

    pub fn call_reg(&mut self, reg: Reg) {
        self.asm.bl_r(reg.into());
    }

    pub fn debug(&mut self) {
        self.asm.brk(0);
    }

    pub fn load_int_const(&mut self, mode: MachineMode, dest: Reg, imm: i64) {
        let sf = size_flag(mode);
        let register_size = match mode {
            MachineMode::Int8 => 32,
            MachineMode::Int32 => 32,
            MachineMode::IntPtr | MachineMode::Ptr | MachineMode::Int64 => 64,
            MachineMode::Float32 | MachineMode::Float64 => unreachable!(),
        };
        let imm = imm as u64;

        if asm::fits_movz(imm, register_size) {
            let shift = asm::shift_movz(imm);
            let imm = ((imm >> (shift * 16)) & 0xFFFF) as u32;

            if sf != 0 {
                self.asm.movz(dest.into(), imm, shift);
            } else {
                self.asm.movz_w(dest.into(), imm, shift);
            }
        } else if asm::fits_movn(imm, register_size) {
            let shift = asm::shift_movn(imm);
            let imm = (((!imm) >> (shift * 16)) & 0xFFFF) as u32;

            if sf != 0 {
                self.asm.movn(dest.into(), imm, shift);
            } else {
                self.asm.movn_w(dest.into(), imm, shift);
            }
        } else {
            let (halfword, invert) = if asm::count_empty_half_words(!imm, register_size)
                > asm::count_empty_half_words(imm, register_size)
            {
                (0xFFFF, true)
            } else {
                (0, false)
            };

            let mut first = true;

            for ind in 0..(register_size / 16) {
                let cur_shift = 16 * ind;
                let cur_halfword = ((imm >> cur_shift) & 0xFFFF) as u32;

                if cur_halfword != halfword {
                    if first {
                        if invert {
                            if sf != 0 {
                                self.asm.movn(dest.into(), (!cur_halfword) & 0xFFFF, ind);
                            } else {
                                self.asm.movn_w(dest.into(), (!cur_halfword) & 0xFFFF, ind);
                            }
                        } else {
                            if sf != 0 {
                                self.asm.movz(dest.into(), cur_halfword, ind);
                            } else {
                                self.asm.movz_w(dest.into(), cur_halfword, ind);
                            }
                        }

                        first = false;
                    } else {
                        if sf != 0 {
                            self.asm.movk(dest.into(), cur_halfword, ind);
                        } else {
                            self.asm.movk_w(dest.into(), cur_halfword, ind);
                        }
                    }
                }
            }
        }
    }

    pub fn load_true(&mut self, dest: Reg) {
        self.asm.movz_w(dest.into(), 1, 0);
    }

    pub fn load_false(&mut self, dest: Reg) {
        self.asm.movz_w(dest.into(), 0, 0);
    }

    pub fn int_neg(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        match mode {
            MachineMode::Int32 => self.asm.sub_w(dest.into(), REG_ZERO.into(), src.into()),
            MachineMode::Int64 => self.asm.sub(dest.into(), REG_ZERO.into(), src.into()),
            _ => panic!("unimplemented mode {:?}", mode),
        }
    }

    pub fn int_not(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        match mode {
            MachineMode::Int32 => {
                self.asm
                    .orn_sh_w(dest.into(), REG_ZERO.into(), src.into(), Shift::LSL, 0)
            }
            MachineMode::Int64 => {
                self.asm
                    .orn_sh(dest.into(), REG_ZERO.into(), src.into(), Shift::LSL, 0)
            }
            _ => unreachable!(),
        }
    }

    pub fn bool_not(&mut self, dest: Reg, src: Reg) {
        let scratch = self.get_scratch();

        self.asm.movz_w((*scratch).into(), 1, 0);
        self.asm
            .eor_sh_w(dest.into(), src.into(), (*scratch).into(), Shift::LSL, 0);
        self.asm.uxtb(dest.into(), dest.into());
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
}

#[derive(Debug)]
pub struct ForwardJump {
    at: usize,
    to: Label,
    ty: JumpType,
}

#[derive(Debug)]
enum JumpType {
    Jump,
    JumpIf(CondCode),
}

fn size_flag(mode: MachineMode) -> u32 {
    match mode {
        MachineMode::Int8 | MachineMode::Int32 => 0,
        MachineMode::IntPtr | MachineMode::Ptr | MachineMode::Int64 => 1,
        MachineMode::Float32 | MachineMode::Float64 => unimplemented!(),
    }
}

impl From<FReg> for NeonRegister {
    fn from(reg: FReg) -> NeonRegister {
        NeonRegister::new(reg.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ty::MachineMode::{Int32, Ptr};
    use byteorder::{LittleEndian, WriteBytesExt};

    macro_rules! assert_emit {
        (
            $($expr:expr),*;
            $name:ident
        ) => {{
            $name.finish();
            let expected: Vec<u32> = vec![$($expr,)*];
            let mut buffer: Vec<u8> = Vec::new();

            for insn in expected {
                buffer.write_u32::<LittleEndian>(insn).unwrap();
            }

            assert_eq!(buffer, $name.data());
        }};
    }

    #[test]
    fn test_jump_forward() {
        let mut masm = MacroAssembler::new();
        let lbl = masm.create_label();
        masm.jump(lbl);
        masm.bind_label(lbl);

        assert_emit!(0x14000001; masm);
    }

    #[test]
    fn test_jump_if_forward() {
        let mut masm = MacroAssembler::new();
        let lbl = masm.create_label();
        masm.jump_if(CondCode::Zero, lbl);
        masm.bind_label(lbl);

        assert_emit!(0x54000020; masm);
    }

    #[test]
    fn test_jump_forward_with_gap() {
        let mut masm = MacroAssembler::new();
        let lbl = masm.create_label();
        masm.jump(lbl);
        masm.emit_u32(0);
        masm.bind_label(lbl);

        assert_emit!(0x14000002, 0; masm);
    }

    #[test]
    fn test_jump_if_forward_with_gap() {
        let mut masm = MacroAssembler::new();
        let lbl = masm.create_label();
        masm.jump_if(CondCode::NonZero, lbl);
        masm.emit_u32(0);
        masm.bind_label(lbl);

        assert_emit!(0x54000041, 0; masm);
    }

    #[test]
    fn test_jump_backward() {
        let mut masm = MacroAssembler::new();
        let lbl = masm.create_label();
        masm.bind_label(lbl);
        masm.jump(lbl);

        assert_emit!(0x14000000; masm);
    }

    #[test]
    fn test_jump_if_backward() {
        let mut masm = MacroAssembler::new();
        let lbl = masm.create_label();
        masm.bind_label(lbl);
        masm.jump_if(CondCode::Less, lbl);

        assert_emit!(0x5400000B; masm);
    }

    #[test]
    fn test_jump_backward_with_gap() {
        let mut masm = MacroAssembler::new();
        let lbl = masm.create_label();
        masm.bind_label(lbl);
        masm.emit_u32(0);
        masm.jump(lbl);

        assert_emit!(0, 0x17FFFFFF; masm);
    }

    #[test]
    fn test_jump_if_backward_with_gap() {
        let mut masm = MacroAssembler::new();
        let lbl = masm.create_label();
        masm.bind_label(lbl);
        masm.emit_u32(0);
        masm.jump_if(CondCode::LessEq, lbl);

        assert_emit!(0, 0x54FFFFED; masm);
    }

    #[test]
    fn test_load_int_const() {
        let mut masm = MacroAssembler::new();
        masm.load_int_const(Int32, R0, 0);
        assert_emit!(0x52800000; masm);

        let mut masm = MacroAssembler::new();
        masm.load_int_const(Int32, R0, 0xFFFF);
        assert_emit!(0x529FFFE0; masm);

        let mut masm = MacroAssembler::new();
        masm.load_int_const(Int32, R0, 1i64 << 16);
        assert_emit!(0x52a00020; masm);

        let mut masm = MacroAssembler::new();
        masm.load_int_const(Ptr, R0, 0);
        assert_emit!(0xD2800000; masm);

        let mut masm = MacroAssembler::new();
        masm.load_int_const(Int32, R0, -1);
        assert_emit!(0x12800000; masm);

        let mut masm = MacroAssembler::new();
        masm.load_int_const(Ptr, R0, -1);
        assert_emit!(0x92800000; masm);
    }

    #[test]
    fn test_load_int_const_multiple_halfwords() {
        let mut masm = MacroAssembler::new();
        masm.load_int_const(Int32, R0, 0x10001);
        assert_emit!(0x52800020, 0x72a00020; masm);

        let mut masm = MacroAssembler::new();
        masm.load_int_const(Ptr, R0, !0x10001);
        assert_emit!(0x92800020, 0xF2BFFFC0; masm);
    }
}
