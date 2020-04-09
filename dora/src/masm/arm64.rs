use byteorder::{LittleEndian, WriteBytesExt};
use dora_parser::lexer::position::Position;

use crate::compiler::codegen::AnyReg;
use crate::compiler::fct::LazyCompilationSite;
use crate::cpu::asm;
use crate::cpu::asm::*;
use crate::cpu::reg::*;
use crate::cpu::{FReg, Mem, Reg};
use crate::gc::swiper::CARD_SIZE_BITS;
use crate::masm::{CondCode, Label, MacroAssembler};
use crate::mem::ptr_width;
use crate::object::{offset_of_array_data, offset_of_array_length, Header};
use crate::threads::ThreadLocalData;
use crate::ty::{MachineMode, TypeList};
use crate::vm::{get_vm, FctId, Trap};
use crate::vtable::VTable;

impl MacroAssembler {
    pub fn prolog(&mut self) -> usize {
        self.emit_u32(asm::stp_pre(1, REG_FP, REG_LR, REG_SP, -2));
        self.emit_u32(asm::add_extreg(
            1,
            REG_FP,
            REG_SP,
            REG_ZERO,
            Extend::UXTX,
            0,
        ));

        let patch_offset = self.pos();

        self.emit_u32(asm::movz(1, REG_TMP1, 0, 0));
        self.emit_u32(asm::movk(1, REG_TMP1, 0, 1));
        self.emit_u32(asm::sub_extreg(
            1,
            REG_SP,
            REG_SP,
            REG_TMP1,
            Extend::UXTX,
            0,
        ));

        patch_offset
    }

    pub fn prolog_size(&mut self, stacksize: i32) {
        self.emit_u32(asm::stp_pre(1, REG_FP, REG_LR, REG_SP, -2));
        self.emit_u32(asm::add_extreg(
            1,
            REG_FP,
            REG_SP,
            REG_ZERO,
            Extend::UXTX,
            0,
        ));

        if stacksize > 0 {
            self.load_int_const(MachineMode::Ptr, REG_TMP1, stacksize as i64);
            self.emit_u32(asm::sub_extreg(
                1,
                REG_SP,
                REG_SP,
                REG_TMP1,
                Extend::UXTX,
                0,
            ));
        }
    }

    pub fn patch_stacksize(&mut self, patch_offset: usize, stacksize: i32) {
        let stacksize = stacksize as u32;
        self.emit_u32_at(
            patch_offset as i32,
            asm::movz(1, REG_TMP1, stacksize & 0xFFFF, 0),
        );
        self.emit_u32_at(
            (patch_offset + 4) as i32,
            asm::movk(1, REG_TMP1, (stacksize >> 16) & 0xFFFF, 1),
        );
    }

    pub fn check_stack_pointer(&mut self, lbl_overflow: Label) {
        let offset = ThreadLocalData::guard_stack_limit_offset() as u32;
        assert!(offset % 8 == 0);
        self.emit_u32(asm::ldrx_imm(REG_TMP1, REG_THREAD, offset / 8));
        self.emit_u32(asm::add_extreg(
            1,
            REG_TMP2,
            REG_SP,
            REG_ZERO,
            Extend::UXTX,
            0,
        ));
        self.cmp_reg(MachineMode::Ptr, REG_TMP1, REG_TMP2);
        self.jump_if(CondCode::UnsignedGreater, lbl_overflow);
    }

    pub fn fix_result(&mut self, _result: Reg, _mode: MachineMode) {
        // nothing to do on ARM64, see version for x64 for more info.
    }

    pub fn epilog(&mut self) {
        self.epilog_without_return();
        self.emit_u32(asm::ret());
    }

    pub fn epilog_without_return(&mut self) {
        self.emit_u32(asm::add_extreg(
            1,
            REG_SP,
            REG_FP,
            REG_ZERO,
            Extend::UXTX,
            0,
        ));
        self.emit_u32(asm::ldp_post(1, REG_FP, REG_LR, REG_SP, 2));
    }

    pub fn increase_stack_frame(&mut self, size: i32) {
        if size > 0 {
            self.load_int_const(MachineMode::Ptr, REG_TMP1, size as i64);
            self.emit_u32(asm::sub_extreg(
                1,
                REG_SP,
                REG_SP,
                REG_TMP1,
                Extend::UXTX,
                0,
            ));
        }
    }

    pub fn decrease_stack_frame(&mut self, size: i32) {
        if size > 0 {
            self.load_int_const(MachineMode::Ptr, REG_TMP1, size as i64);
            self.emit_u32(asm::add_extreg(
                1,
                REG_SP,
                REG_SP,
                REG_TMP1,
                Extend::UXTX,
                0,
            ));
        }
    }

    pub fn direct_call(
        &mut self,
        fct_id: FctId,
        ptr: *const u8,
        cls_tps: TypeList,
        fct_tps: TypeList,
    ) {
        let disp = self.add_addr(ptr);
        let pos = self.pos() as i32;

        let scratch = self.get_scratch();

        self.load_constpool(*scratch, disp + pos);
        self.emit_u32(asm::blr(*scratch));

        let pos = self.pos() as i32;
        self.emit_lazy_compilation_site(LazyCompilationSite::Compile(
            fct_id,
            disp + pos,
            cls_tps,
            fct_tps,
        ));
    }

    pub fn raw_call(&mut self, ptr: *const u8) {
        let disp = self.add_addr(ptr);
        let pos = self.pos() as i32;

        let scratch = self.get_scratch();

        self.load_constpool(*scratch, disp + pos);
        self.emit_u32(asm::blr(*scratch));
    }

    pub fn indirect_call(&mut self, pos: Position, index: u32, cls_type_params: TypeList) {
        let obj = REG_PARAMS[0];

        self.test_if_nil_bailout(pos, obj, Trap::NIL);

        // need to use scratch register instead of REG_RESULT for calculations
        // since REG_RESULT (x0) is also the first parameter
        let scratch = self.get_scratch();

        // scratch = [obj] (load vtable)
        self.load_mem(MachineMode::Ptr, (*scratch).into(), Mem::Base(obj, 0));

        // calculate offset of VTable entry
        let disp = VTable::offset_of_method_table() + (index as i32) * ptr_width();

        // load vtable entry into scratch
        self.load_mem(
            MachineMode::Ptr,
            scratch.reg().into(),
            Mem::Base(*scratch, disp),
        );

        // call *scratch
        self.emit_u32(asm::blr(*scratch));
        self.emit_lazy_compilation_site(LazyCompilationSite::VirtCompile(
            index,
            cls_type_params,
            TypeList::empty(),
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
        self.emit_u32(asm::cset(0, dest, op.into()));
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
        self.emit_u32(asm::cmp_shreg(size_flag(mode), lhs, rhs, Shift::LSL, 0));
    }

    pub fn cmp_reg_imm(&mut self, mode: MachineMode, lhs: Reg, imm: i32) {
        let scratch = self.get_scratch();
        self.load_int_const(mode, *scratch, imm as i64);
        self.cmp_reg(mode, lhs, *scratch);
    }

    pub fn cmp_zero(&mut self, mode: MachineMode, lhs: Reg) {
        self.emit_u32(asm::cmp_imm(size_flag(mode), lhs, 0, 0));
    }

    pub fn test_and_jump_if(&mut self, cond: CondCode, reg: Reg, lbl: Label) {
        assert!(cond == CondCode::Zero || cond == CondCode::NonZero);

        self.emit_u32(asm::cmp_imm(0, reg, 0, 0));
        self.jump_if(cond, lbl);
    }

    pub fn jump_if(&mut self, cond: CondCode, lbl: Label) {
        let value = self.labels[lbl.index()];

        match value {
            Some(idx) => {
                let current = self.pos();
                let target = idx;

                let diff = -((current - target) as i32);
                assert!(diff % 4 == 0);
                let diff = diff / 4;

                self.emit_u32(asm::b_cond_imm(cond.into(), diff));
            }

            None => {
                let pos = self.pos();
                self.emit_u32(0);
                self.jumps.push(ForwardJump {
                    at: pos,
                    to: lbl,
                    ty: JumpType::JumpIf(cond),
                });
            }
        }
    }

    pub fn jump(&mut self, lbl: Label) {
        let value = self.labels[lbl.index()];

        match value {
            Some(idx) => {
                let current = self.pos();
                let target = idx;

                let diff = -((current - target) as i32);
                assert!(diff % 4 == 0);
                self.emit_u32(asm::b_imm(diff / 4));
            }

            None => {
                let pos = self.pos();
                self.emit_u32(0);
                self.jumps.push(ForwardJump {
                    at: pos,
                    to: lbl,
                    ty: JumpType::Jump,
                });
            }
        }
    }

    pub fn jump_reg(&mut self, reg: Reg) {
        self.emit_u32(asm::br(reg));
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
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode),
        };

        let lbl_zero = self.create_label();

        self.cmp_reg_imm(mode, rhs, 0);
        self.jump_if(CondCode::Equal, lbl_zero);
        self.emit_bailout(lbl_zero, Trap::DIV0, pos);

        if is_div {
            self.emit_u32(asm::sdiv(x64, dest, lhs, rhs));
        } else {
            let scratch = self.get_scratch();
            self.emit_u32(asm::sdiv(x64, *scratch, lhs, rhs));
            self.emit_u32(asm::msub(x64, dest, *scratch, rhs, lhs));
        }
    }

    pub fn int_mul(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode),
        };

        self.emit_u32(asm::mul(x64, dest, lhs, rhs));
    }

    pub fn int_add(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 | MachineMode::Ptr => 1,
            _ => panic!("unimplemented mode {:?}", mode),
        };

        self.emit_u32(asm::add_reg(x64, dest, lhs, rhs));
    }

    pub fn int_add_imm(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, value: i64) {
        if (value as u32) as i64 == value && asm::fits_u12(value as u32) {
            let x64 = match mode {
                MachineMode::Int32 => 0,
                MachineMode::Int64 | MachineMode::Ptr => 1,
                _ => panic!("unimplemented mode {:?}", mode),
            };

            let inst = asm::add_imm(x64, dest, lhs, value as u32, 0);
            self.emit_u32(inst);
        } else {
            let scratch = self.get_scratch();
            self.load_int_const(mode, *scratch, value);
            self.int_add(mode, dest, lhs, *scratch);
        }
    }

    pub fn int_sub(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode),
        };

        self.emit_u32(asm::sub_reg(x64, dest, lhs, rhs));
    }

    pub fn int_shl(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode),
        };

        self.emit_u32(asm::lslv(x64, dest, lhs, rhs));
    }

    pub fn int_shr(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode),
        };

        self.emit_u32(asm::lsrv(x64, dest, lhs, rhs));
    }

    pub fn int_sar(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode),
        };

        self.emit_u32(asm::asrv(x64, dest, lhs, rhs));
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
        self.emit_u32(asm::sub_reg(x64, *scratch, *scratch, rhs));
        self.emit_u32(asm::rorv(x64, dest, lhs, *scratch));
    }

    pub fn int_ror(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode),
        };

        self.emit_u32(asm::rorv(x64, dest, lhs, rhs));
    }

    pub fn int_or(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode),
        };

        self.emit_u32(asm::orr_shreg(x64, dest, lhs, rhs, Shift::LSL, 0));
    }

    pub fn int_and(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode),
        };

        self.emit_u32(asm::and_shreg(x64, dest, lhs, rhs, Shift::LSL, 0));
    }

    pub fn int_xor(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode),
        };

        self.emit_u32(asm::eor_shreg(x64, dest, lhs, rhs, Shift::LSL, 0));
    }

    pub fn count_bits(&mut self, mode: MachineMode, dest: Reg, src: Reg, count_one_bits: bool) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode),
        };

        let fty = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode),
        };

        let scratch = FREG_TMP1;
        let scratch_src = self.get_scratch();

        let src = if count_one_bits {
            src
        } else {
            self.int_not(mode, *scratch_src, src);
            *scratch_src
        };

        self.emit_u32(asm::fmov_fs(x64, fty, scratch, src));
        self.emit_u32(asm::cnt(0, 0b00, scratch, scratch));
        self.emit_u32(asm::addv(0, 0b00, scratch, scratch));
        self.emit_u32(asm::fmov_sf(x64, fty, dest, scratch));
    }

    pub fn count_bits_leading(
        &mut self,
        mode: MachineMode,
        dest: Reg,
        src: Reg,
        count_one_bits: bool,
    ) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode),
        };

        if count_one_bits {
            self.int_not(mode, dest, src);
            self.emit_u32(asm::clz(x64, dest, dest));
        } else {
            self.emit_u32(asm::clz(x64, dest, src));
        }
    }

    pub fn count_bits_trailing(
        &mut self,
        mode: MachineMode,
        dest: Reg,
        src: Reg,
        count_one_bits: bool,
    ) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode),
        };

        self.emit_u32(asm::rbit(x64, dest, src));

        if count_one_bits {
            self.int_not(mode, dest, dest);
        }

        self.emit_u32(asm::clz(x64, dest, dest));
    }

    pub fn int_to_float(
        &mut self,
        dest_mode: MachineMode,
        dest: FReg,
        src_mode: MachineMode,
        src: Reg,
    ) {
        let x64 = match src_mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => unreachable!(),
        };

        let flt = match dest_mode {
            MachineMode::Float32 => 0,
            MachineMode::Float64 => 1,
            _ => unreachable!(),
        };

        self.emit_u32(asm::scvtf(x64, flt, dest, src));
    }

    pub fn float_to_int(
        &mut self,
        dest_mode: MachineMode,
        dest: Reg,
        src_mode: MachineMode,
        src: FReg,
    ) {
        let x64 = match dest_mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => unreachable!(),
        };

        let flt = match src_mode {
            MachineMode::Float32 => 0,
            MachineMode::Float64 => 1,
            _ => unreachable!(),
        };

        self.emit_u32(asm::fcvtzs(x64, flt, dest, src));
    }

    pub fn float_to_double(&mut self, dest: FReg, src: FReg) {
        self.emit_u32(asm::fcvt_sd(dest, src));
    }

    pub fn double_to_float(&mut self, dest: FReg, src: FReg) {
        self.emit_u32(asm::fcvt_ds(dest, src));
    }

    pub fn int_as_float(
        &mut self,
        dest_mode: MachineMode,
        dest: FReg,
        src_mode: MachineMode,
        src: Reg,
    ) {
        assert!(src_mode.size() == dest_mode.size());

        let x64 = match src_mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => unreachable!(),
        };

        let flt = match dest_mode {
            MachineMode::Float32 => 0,
            MachineMode::Float64 => 1,
            _ => unreachable!(),
        };

        self.emit_u32(asm::fmov_fs(x64, flt, dest, src));
    }

    pub fn float_as_int(
        &mut self,
        dest_mode: MachineMode,
        dest: Reg,
        src_mode: MachineMode,
        src: FReg,
    ) {
        assert!(src_mode.size() == dest_mode.size());

        let x64 = match dest_mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => unreachable!(),
        };

        let flt = match src_mode {
            MachineMode::Float32 => 0,
            MachineMode::Float64 => 1,
            _ => unreachable!(),
        };

        self.emit_u32(asm::fmov_sf(x64, flt, dest, src));
    }

    pub fn float_add(&mut self, mode: MachineMode, dest: FReg, lhs: FReg, rhs: FReg) {
        let dbl = match mode {
            MachineMode::Float32 => 0,
            MachineMode::Float64 => 1,
            _ => unimplemented!(),
        };

        self.emit_u32(asm::fadd(dbl, dest, lhs, rhs));
    }

    pub fn float_sub(&mut self, mode: MachineMode, dest: FReg, lhs: FReg, rhs: FReg) {
        let dbl = match mode {
            MachineMode::Float32 => 0,
            MachineMode::Float64 => 1,
            _ => unimplemented!(),
        };

        self.emit_u32(asm::fsub(dbl, dest, lhs, rhs));
    }

    pub fn float_mul(&mut self, mode: MachineMode, dest: FReg, lhs: FReg, rhs: FReg) {
        let dbl = match mode {
            MachineMode::Float32 => 0,
            MachineMode::Float64 => 1,
            _ => unimplemented!(),
        };

        self.emit_u32(asm::fmul(dbl, dest, lhs, rhs));
    }

    pub fn float_div(&mut self, mode: MachineMode, dest: FReg, lhs: FReg, rhs: FReg) {
        let dbl = match mode {
            MachineMode::Float32 => 0,
            MachineMode::Float64 => 1,
            _ => unimplemented!(),
        };

        self.emit_u32(asm::fdiv(dbl, dest, lhs, rhs));
    }

    pub fn float_neg(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        let dbl = match mode {
            MachineMode::Float32 => 0,
            MachineMode::Float64 => 1,
            _ => unimplemented!(),
        };

        self.emit_u32(asm::fneg(dbl, dest, src));
    }

    pub fn float_sqrt(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        let dbl = match mode {
            MachineMode::Float32 => 0,
            MachineMode::Float64 => 1,
            _ => unimplemented!(),
        };

        self.emit_u32(asm::fsqrt(dbl, dest, src));
    }

    pub fn float_cmp(
        &mut self,
        mode: MachineMode,
        dest: Reg,
        lhs: FReg,
        rhs: FReg,
        cond: CondCode,
    ) {
        let dbl = match mode {
            MachineMode::Float32 => 0,
            MachineMode::Float64 => 1,
            _ => unimplemented!(),
        };

        let cond = match cond {
            CondCode::Equal => Cond::EQ,
            CondCode::NotEqual => Cond::NE,
            CondCode::Greater => Cond::GT,
            CondCode::GreaterEq => Cond::GE,
            CondCode::Less => Cond::MI,
            CondCode::LessEq => Cond::LS,
            _ => unreachable!(),
        };

        self.emit_u32(asm::fcmp(dbl, lhs, rhs));
        self.emit_u32(asm::cset(0, dest, cond));
    }

    pub fn float_cmp_nan(&mut self, mode: MachineMode, dest: Reg, src: FReg) {
        let dbl = match mode {
            MachineMode::Float32 => 0,
            MachineMode::Float64 => 1,
            _ => unimplemented!(),
        };

        self.emit_u32(asm::fcmp(dbl, src, src));
        self.emit_u32(asm::cset(0, dest, Cond::VS));
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
        self.emit_u32(asm::adr(*scratch, -disp));

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
            self.emit_u32(asm::add_imm(1, dest, length, size as u32, 0));
        } else if element_size == 2 || element_size == 4 || element_size == 8 {
            let shift = match element_size {
                2 => 1,
                4 => 2,
                8 => 3,
                _ => unreachable!(),
            };

            self.emit_u32(asm::lsl_imm(1, dest, length, shift));
            self.emit_u32(asm::add_imm(1, dest, dest, size as u32, 0));
        } else {
            let scratch = self.get_scratch();
            self.load_int_const(MachineMode::Ptr, *scratch, element_size as i64);
            self.emit_u32(asm::mul(1, dest, length, *scratch));
            self.emit_u32(asm::add_imm(1, dest, dest, size as u32, 0));
        }

        if element_size != ptr_width() {
            self.emit_u32(asm::and_imm(1, dest, dest, -ptr_width() as u64));
        }
    }

    pub fn array_address(&mut self, dest: Reg, obj: Reg, index: Reg, element_size: i32) {
        let offset = Header::size() + ptr_width();
        let scratch = self.get_scratch();

        self.load_int_const(MachineMode::Ptr, *scratch, element_size as i64);
        self.emit_u32(asm::mul(1, *scratch, index, *scratch));
        self.emit_u32(asm::add_imm(1, *scratch, *scratch, offset as u32, 0));
        self.emit_u32(asm::add_reg(1, dest, obj, *scratch));
    }

    pub fn check_index_out_of_bounds(&mut self, pos: Position, array: Reg, index: Reg) {
        let scratch = self.get_scratch();
        self.load_mem(
            MachineMode::Int32,
            (*scratch).into(),
            Mem::Base(array, offset_of_array_length()),
        );
        self.cmp_reg(MachineMode::Int32, index, *scratch);

        let lbl = self.create_label();
        self.jump_if(CondCode::UnsignedGreaterEq, lbl);
        self.emit_bailout(lbl, Trap::INDEX_OUT_OF_BOUNDS, pos);
    }

    pub fn load_nil(&mut self, dest: Reg) {
        self.emit_u32(movz(1, dest, 0, 0));
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
                self.emit_u32(asm::add_reg(1, *scratch, *scratch, base));

                let inst = match mode {
                    MachineMode::Int8 => {
                        asm::ldrb_ind(dest.reg(), *scratch, index, LdStExtend::LSL, 0)
                    }
                    MachineMode::Int32 => {
                        asm::ldrw_ind(dest.reg(), *scratch, index, LdStExtend::LSL, 1)
                    }
                    MachineMode::Int64 | MachineMode::Ptr => {
                        asm::ldrx_ind(dest.reg(), *scratch, index, LdStExtend::LSL, 1)
                    }
                    MachineMode::Float32 => {
                        asm::ldrs_ind(dest.freg(), *scratch, index, LdStExtend::LSL, 1)
                    }
                    MachineMode::Float64 => {
                        asm::ldrd_ind(dest.freg(), *scratch, index, LdStExtend::LSL, 1)
                    }
                };

                self.emit_u32(inst);
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
        if disp >= 0 && disp % mode.size() == 0 && asm::fits_u12((disp / mode.size()) as u32) {
            let disp = (disp / mode.size()) as u32;
            let inst = match mode {
                MachineMode::Int8 => asm::ldrb_imm(dest.reg(), base, disp),
                MachineMode::Int32 => asm::ldrw_imm(dest.reg(), base, disp),
                MachineMode::Int64 | MachineMode::Ptr => asm::ldrx_imm(dest.reg(), base, disp),
                MachineMode::Float32 => asm::ldrs_imm(dest.freg(), base, disp),
                MachineMode::Float64 => asm::ldrd_imm(dest.freg(), base, disp),
            };
            self.emit_u32(inst);
        } else if asm::fits_i9(disp) {
            let inst = match mode {
                MachineMode::Int8 => asm::ldrb_unscaled_imm(dest.reg(), base, disp),
                MachineMode::Int32 => asm::ldrw_unscaled_imm(dest.reg(), base, disp),
                MachineMode::Int64 | MachineMode::Ptr => {
                    asm::ldrx_unscaled_imm(dest.reg(), base, disp)
                }
                MachineMode::Float32 => asm::ldrs_unscaled_imm(dest.freg(), base, disp),
                MachineMode::Float64 => asm::ldrd_unscaled_imm(dest.freg(), base, disp),
            };
            self.emit_u32(inst);
        } else {
            let scratch = self.get_scratch();
            self.load_int_const(MachineMode::Ptr, *scratch, disp as i64);
            let inst = match mode {
                MachineMode::Int8 => asm::ldrb_ind(dest.reg(), base, *scratch, LdStExtend::LSL, 0),
                MachineMode::Int32 => asm::ldrw_ind(dest.reg(), base, *scratch, LdStExtend::LSL, 0),
                MachineMode::Int64 | MachineMode::Ptr => {
                    asm::ldrx_ind(dest.reg(), base, *scratch, LdStExtend::LSL, 0)
                }
                MachineMode::Float32 => {
                    asm::ldrs_ind(dest.freg(), base, *scratch, LdStExtend::LSL, 0)
                }
                MachineMode::Float64 => {
                    asm::ldrd_ind(dest.freg(), base, *scratch, LdStExtend::LSL, 0)
                }
            };
            self.emit_u32(inst);
        }
    }

    pub fn lea(&mut self, dest: Reg, mem: Mem) {
        match mem {
            Mem::Local(offset) => {
                if fits_u12(offset as u32) {
                    self.emit_u32(asm::add_imm(1, dest, REG_FP, offset as u32, 0));
                } else {
                    let scratch = self.get_scratch();
                    self.load_int_const(MachineMode::Ptr, *scratch, offset as i64);
                    self.emit_u32(asm::add_shreg(1, dest, REG_FP, *scratch, Shift::LSL, 0));
                }
            }

            Mem::Base(base, disp) => {
                if fits_u12(disp as u32) {
                    self.emit_u32(asm::add_imm(1, dest, base, disp as u32, 0));
                } else {
                    let scratch = self.get_scratch();
                    self.load_int_const(MachineMode::Ptr, *scratch, disp as i64);
                    self.emit_u32(asm::add_shreg(1, dest, base, *scratch, Shift::LSL, 0));
                }
            }

            Mem::Index(base, index, scale, disp) => {
                let scratch = self.get_scratch();

                if fits_u12(disp as u32) {
                    self.emit_u32(asm::add_imm(1, *scratch, base, disp as u32, 0));
                } else {
                    self.load_int_const(MachineMode::Ptr, *scratch, disp as i64);
                    self.emit_u32(asm::add_shreg(1, *scratch, base, *scratch, Shift::LSL, 0));
                }

                let shift = match scale {
                    1 => 0,
                    2 => 1,
                    4 => 2,
                    8 => 3,
                    _ => unimplemented!(),
                };

                let inst = asm::add_shreg(1, dest, *scratch, index, Shift::LSL, shift);
                self.emit_u32(inst);
            }

            Mem::Offset(_, _, _) => unimplemented!(),
        }
    }

    pub fn emit_barrier(&mut self, src: Reg, card_table_offset: usize) {
        let scratch1 = self.get_scratch();
        self.emit_u32(asm::lsr_imm(1, *scratch1, src, CARD_SIZE_BITS as u32));
        let scratch2 = self.get_scratch();
        self.load_int_const(MachineMode::Ptr, *scratch2, card_table_offset as i64);
        let inst = asm::strb_ind(REG_ZERO, *scratch1, *scratch2, LdStExtend::LSL, 0);
        self.emit_u32(inst);
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
                self.emit_u32(asm::add_reg(1, *scratch, *scratch, base));

                let inst = match mode {
                    MachineMode::Int8 => {
                        asm::strb_ind(src.reg(), *scratch, index, LdStExtend::LSL, 0)
                    }
                    MachineMode::Int32 => {
                        asm::strw_ind(src.reg(), *scratch, index, LdStExtend::LSL, 1)
                    }
                    MachineMode::Int64 | MachineMode::Ptr => {
                        asm::strx_ind(src.reg(), *scratch, index, LdStExtend::LSL, 1)
                    }
                    MachineMode::Float32 => {
                        asm::strs_ind(src.freg(), *scratch, index, LdStExtend::LSL, 1)
                    }
                    MachineMode::Float64 => {
                        asm::strd_ind(src.freg(), *scratch, index, LdStExtend::LSL, 1)
                    }
                };

                self.emit_u32(inst);
            }

            Mem::Offset(_, _, _) => unimplemented!(),
        }
    }

    pub fn common_store_base_with_offset(
        &mut self,
        mode: MachineMode,
        src: AnyReg,
        base: Reg,
        offset: i32,
    ) {
        if offset >= 0 && offset % mode.size() == 0 && asm::fits_u12((offset / mode.size()) as u32)
        {
            let offset = (offset / mode.size()) as u32;
            let inst = match mode {
                MachineMode::Int8 => asm::strb_imm(src.reg(), base, offset),
                MachineMode::Int32 => asm::strw_imm(src.reg(), base, offset),
                MachineMode::Int64 | MachineMode::Ptr => asm::strx_imm(src.reg(), base, offset),
                MachineMode::Float32 => asm::strs_imm(src.freg(), base, offset),
                MachineMode::Float64 => asm::strd_imm(src.freg(), base, offset),
            };
            self.emit_u32(inst);
        } else if asm::fits_i9(offset) {
            let inst = match mode {
                MachineMode::Int8 => asm::strb_unscaled_imm(src.reg(), base, offset),
                MachineMode::Int32 => asm::strw_unscaled_imm(src.reg(), base, offset),
                MachineMode::Int64 | MachineMode::Ptr => {
                    asm::strx_unscaled_imm(src.reg(), base, offset)
                }
                MachineMode::Float32 => asm::strs_unscaled_imm(src.freg(), base, offset),
                MachineMode::Float64 => asm::strd_unscaled_imm(src.freg(), base, offset),
            };
            self.emit_u32(inst);
        } else {
            let scratch = self.get_scratch();
            self.load_int_const(MachineMode::Ptr, *scratch, offset as i64);
            let inst = match mode {
                MachineMode::Int8 => asm::strb_ind(src.reg(), base, *scratch, LdStExtend::LSL, 0),
                MachineMode::Int32 => asm::strw_ind(src.reg(), base, *scratch, LdStExtend::LSL, 0),
                MachineMode::Int64 | MachineMode::Ptr => {
                    asm::strx_ind(src.reg(), base, *scratch, LdStExtend::LSL, 0)
                }
                MachineMode::Float32 => {
                    asm::strs_ind(src.freg(), base, *scratch, LdStExtend::LSL, 0)
                }
                MachineMode::Float64 => {
                    asm::strd_ind(src.freg(), base, *scratch, LdStExtend::LSL, 0)
                }
            };
            self.emit_u32(inst);
        }
    }

    pub fn copy_reg(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        if dest == REG_SP || src == REG_SP {
            self.emit_u32(asm::add_imm(size_flag(mode), dest, src, 0, 0));
        } else {
            self.emit_u32(asm::orr_shreg(
                size_flag(mode),
                dest,
                REG_ZERO,
                src,
                Shift::LSL,
                0,
            ));
        }
    }

    pub fn copy_sp(&mut self, dest: Reg) {
        self.emit_u32(asm::add_imm(1, dest, REG_SP, 0, 0));
    }

    pub fn set_sp(&mut self, src: Reg) {
        self.emit_u32(asm::add_imm(1, REG_SP, src, 0, 0));
    }

    pub fn copy_pc(&mut self, dest: Reg) {
        self.emit_u32(asm::adr(dest, 0));
    }

    pub fn copy_ra(&mut self, dest: Reg) {
        self.copy_reg(MachineMode::Ptr, dest, REG_LR);
    }

    pub fn copy_freg(&mut self, mode: MachineMode, dest: FReg, src: FReg) {
        let dbl = match mode {
            MachineMode::Float32 => 0,
            MachineMode::Float64 => 1,
            _ => unreachable!(),
        };

        self.emit_u32(fmov(dbl, dest, src));
    }

    pub fn extend_int_long(&mut self, dest: Reg, src: Reg) {
        self.emit_u32(asm::sxtw(dest, src));
    }

    pub fn extend_byte(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        match mode {
            MachineMode::Int32 => {}
            MachineMode::Int64 => self.emit_u32(asm::uxtw(dest, src)),
            _ => panic!("unimplemented mode {:?}", mode),
        }
    }

    pub fn load_constpool(&mut self, dest: Reg, disp: i32) {
        self.emit_u32(asm::adr(dest, -disp));
        self.load_mem(MachineMode::Ptr, dest.into(), Mem::Base(dest, 0));
    }

    pub fn call_reg(&mut self, reg: Reg) {
        self.emit_u32(asm::blr(reg));
    }

    pub fn debug(&mut self) {
        self.emit_u32(asm::brk(0));
    }

    pub fn load_int_const(&mut self, mode: MachineMode, dest: Reg, imm: i64) {
        let sf = size_flag(mode);
        let register_size = match mode {
            MachineMode::Int8 => 32,
            MachineMode::Int32 => 32,
            MachineMode::Int64 => 64,
            MachineMode::Ptr => 64,
            MachineMode::Float32 | MachineMode::Float64 => unreachable!(),
        };
        let imm = imm as u64;

        if fits_movz(imm, register_size) {
            let shift = shift_movz(imm);
            let imm = ((imm >> (shift * 16)) & 0xFFFF) as u32;
            self.emit_u32(movz(sf, dest, imm, shift));
        } else if fits_movn(imm, register_size) {
            let shift = shift_movn(imm);
            let imm = (((!imm) >> (shift * 16)) & 0xFFFF) as u32;
            self.emit_u32(movn(sf, dest, imm, shift));
        } else {
            let (halfword, invert) = if count_empty_half_words(!imm, register_size)
                > count_empty_half_words(imm, register_size)
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
                        let insn = if invert {
                            asm::movn(sf, dest, (!cur_halfword) & 0xFFFF, ind)
                        } else {
                            asm::movz(sf, dest, cur_halfword, ind)
                        };

                        self.emit_u32(insn);
                        first = false;
                    } else {
                        let insn = asm::movk(sf, dest, cur_halfword, ind);
                        self.emit_u32(insn);
                    }
                }
            }
        }
    }

    pub fn load_true(&mut self, dest: Reg) {
        self.emit_u32(movz(0, dest, 1, 0));
    }

    pub fn load_false(&mut self, dest: Reg) {
        self.emit_u32(movz(0, dest, 0, 0));
    }

    pub fn int_neg(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode),
        };

        self.emit_u32(sub_reg(x64, dest, REG_ZERO, src));
    }

    pub fn int_not(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode),
        };

        self.emit_u32(orn_shreg(x64, dest, REG_ZERO, src, Shift::LSL, 0));
    }

    pub fn bool_not(&mut self, dest: Reg, src: Reg) {
        let scratch = self.get_scratch();

        self.emit_u32(movz(0, *scratch, 1, 0));
        self.emit_u32(eor_shreg(0, dest, src, *scratch, Shift::LSL, 0));
        self.emit_u32(uxtb(dest, dest));
    }

    pub fn trap(&mut self, trap: Trap, pos: Position) {
        let vm = get_vm();
        self.load_int_const(MachineMode::Int32, REG_PARAMS[0], trap.int() as i64);
        self.raw_call(vm.trap_stub().to_ptr());
        self.emit_position(pos);
    }

    pub fn nop(&mut self) {
        self.emit_u32(asm::nop());
    }

    pub fn fix_forward_jumps(&mut self) {
        for jmp in &self.jumps {
            let target = self.labels[jmp.to.0].expect("label not defined");
            let diff = (target - jmp.at) as i32;
            assert!(diff % 4 == 0);
            let diff = diff / 4;

            let insn = match jmp.ty {
                JumpType::Jump => asm::b_imm(diff),
                JumpType::JumpIf(cond) => asm::b_cond_imm(cond.into(), diff),
            };

            let mut slice = &mut self.asm.code_mut()[jmp.at..];
            slice.write_u32::<LittleEndian>(insn).unwrap();
        }
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
        MachineMode::Ptr | MachineMode::Int64 => 1,
        MachineMode::Float32 | MachineMode::Float64 => unimplemented!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ty::MachineMode::{Int32, Int8, Ptr};

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

    #[test]
    fn test_load_mem_base_ptr() {
        let i1 = asm::ldrx_imm(R1, R10, 1);
        let mut masm = MacroAssembler::new();
        masm.load_mem(Ptr, R1.into(), Mem::Base(R10, 8));
        assert_emit!(i1; masm);
    }

    #[test]
    fn test_load_mem_base_int32() {
        let i1 = asm::ldrw_imm(R1, R2, 2);
        let mut masm = MacroAssembler::new();
        masm.load_mem(Int32, R1.into(), Mem::Base(R2, 8));
        assert_emit!(i1; masm);
    }

    #[test]
    fn test_load_mem_base_int8() {
        let i1 = asm::ldrb_imm(R1, R3, 3);
        let mut masm = MacroAssembler::new();
        masm.load_mem(Int8, R1.into(), Mem::Base(R3, 3));
        assert_emit!(i1; masm);
    }

    #[test]
    fn test_load_mem_index_ptr() {
        let i1 = asm::movz(1, R9, 1, 0);
        let i2 = asm::add_reg(1, R9, R9, R10);
        let i3 = asm::ldrx_ind(R1, R9, R11, LdStExtend::LSL, 1);

        let mut masm = MacroAssembler::new();
        masm.load_mem(Ptr, R1.into(), Mem::Index(R10, R11, 8, 1));
        assert_emit!(i1, i2, i3; masm);
    }

    #[test]
    fn test_load_mem_index_int32() {
        let i1 = asm::movz(1, R9, 2, 0);
        let i2 = asm::add_reg(1, R9, R9, R2);
        let i3 = asm::ldrw_ind(R1, R9, R12, LdStExtend::LSL, 1);

        let mut masm = MacroAssembler::new();
        masm.load_mem(Int32, R1.into(), Mem::Index(R2, R12, 4, 2));
        assert_emit!(i1, i2, i3; masm);
    }

    #[test]
    fn test_load_mem_index_int8() {
        let i1 = asm::movz(1, R9, 3, 0);
        let i2 = asm::add_reg(1, R9, R9, R3);
        let i3 = asm::ldrb_ind(R1, R9, R13, LdStExtend::LSL, 0);

        let mut masm = MacroAssembler::new();
        masm.load_mem(Int8, R1.into(), Mem::Index(R3, R13, 1, 3));
        assert_emit!(i1, i2, i3; masm);
    }
}
