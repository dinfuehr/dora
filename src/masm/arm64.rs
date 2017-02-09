use baseline::fct::BailoutInfo;
use baseline::codegen::CondCode;
use byteorder::{LittleEndian, WriteBytesExt};
use cpu::asm;
use cpu::asm::*;
use cpu::reg::*;
use cpu::{Mem, Reg};
use ctxt::FctId;
use lexer::position::Position;
use masm::{MacroAssembler, Label};
use mem::ptr_width;
use object::{offset_of_array_data, offset_of_array_length};
use os::signal::Trap;
use ty::MachineMode;
use vtable::VTable;

impl MacroAssembler {
    pub fn prolog(&mut self, stacksize: i32) {
        self.emit_u32(asm::stp_pre(1, REG_FP, REG_LR, REG_SP, -2));
        self.emit_u32(asm::add_extreg(1, REG_FP, REG_SP, REG_ZERO, Extend::UXTX, 0));

        if stacksize > 0 {
            let scratch = self.get_scratch();
            self.load_int_const(MachineMode::Ptr, *scratch, stacksize as i64);
            self.emit_u32(asm::sub_extreg(1, REG_SP, REG_SP, *scratch, Extend::UXTX, 0));
        }
    }

    pub fn epilog(&mut self, stacksize: i32) {
        if stacksize > 0 {
            let scratch = self.get_scratch();
            self.load_int_const(MachineMode::Ptr, *scratch, stacksize as i64);
            self.emit_u32(asm::add_extreg(1, REG_SP, REG_SP, *scratch, Extend::UXTX, 0));
        }

        self.emit_u32(asm::add_extreg(1, REG_SP, REG_FP, REG_ZERO, Extend::UXTX, 0));
        self.emit_u32(asm::ldp_post(1, REG_FP, REG_LR, REG_SP, 2));
        self.emit_u32(asm::ret());
    }

    pub fn direct_call(&mut self, fct_id: FctId, ptr: *const u8) {
        let disp = self.add_addr(ptr);
        let pos = self.pos() as i32;

        let scratch = self.get_scratch();

        self.load_constpool(*scratch, disp + pos);
        self.emit_u32(asm::blr(*scratch));

        let pos = self.pos() as i32;
        self.emit_bailout_info(BailoutInfo::Compile(fct_id, disp + pos));
    }

    pub fn indirect_call(&mut self, index: u32) {
        let obj = REG_PARAMS[0];

        // need to use scratch register instead of REG_RESULT for calculations
        // since REG_RESULT (x0) is also the first parameter
        let scratch = self.get_scratch();

        // scratch = [obj] (load vtable)
        self.load_mem(MachineMode::Ptr, *scratch, Mem::Base(obj, 0));

        // calculate offset of VTable entry
        let disp = VTable::offset_of_method_table() + (index as i32) * ptr_width();

        // load vtable entry into scratch
        self.load_mem(MachineMode::Ptr, *scratch, Mem::Base(*scratch, disp));

        // call *scratch
        self.emit_u32(asm::blr(*scratch));
        self.emit_bailout_info(BailoutInfo::VirtCompile(index));
    }

    pub fn load_array_elem(&mut self, mode: MachineMode, dest: Reg, array: Reg, index: Reg) {
        self.load_mem(mode,
                      dest,
                      Mem::Index(array, index, mode.size(), offset_of_array_data()));
    }

    pub fn store_array_elem(&mut self, mode: MachineMode, array: Reg, index: Reg, value: Reg) {
        self.store_mem(mode,
                       Mem::Index(array, index, mode.size(), offset_of_array_data()),
                       value);
    }

    pub fn set(&mut self, dest: Reg, op: CondCode) {
        self.emit_u32(asm::cset(0, dest, op.into()));
    }

    pub fn cmp_mem(&mut self, mode: MachineMode, mem: Mem, rhs: Reg) {
        let scratch = self.get_scratch();

        self.load_mem(mode, *scratch, mem);
        self.cmp_reg(mode, *scratch, rhs);
    }

    pub fn cmp_mem_imm(&mut self, mode: MachineMode, mem: Mem, imm: i32) {
        let scratch1 = self.get_scratch();
        self.load_mem(mode, *scratch1, mem);

        let scratch2 = self.get_scratch();
        self.load_int_const(mode, *scratch2, imm as i64);

        self.cmp_reg(mode, *scratch1, *scratch2);
    }

    pub fn cmp_reg(&mut self, mode: MachineMode, lhs: Reg, rhs: Reg) {
        self.emit_u32(asm::cmp_shreg(size_flag(mode), lhs, rhs, Shift::LSL, 0));
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

    pub fn int_div(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode)
        };

        self.emit_u32(asm::sdiv(x64, dest, lhs, rhs));
    }

    pub fn int_mod(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        let scratch = self.get_scratch();
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode)
        };

        self.emit_u32(asm::sdiv(x64, *scratch, lhs, rhs));
        self.emit_u32(asm::msub(x64, dest, *scratch, rhs, lhs));
    }

    pub fn int_mul(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode)
        };

        self.emit_u32(asm::mul(x64, dest, lhs, rhs));
    }

    pub fn int_add(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode)
        };

        self.emit_u32(asm::add_reg(x64, dest, lhs, rhs));
    }

    pub fn int_sub(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode)
        };

        self.emit_u32(asm::sub_reg(x64, dest, lhs, rhs));
    }

    pub fn int_shl(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode)
        };

        self.emit_u32(asm::lslv(x64, dest, lhs, rhs));
    }

    pub fn int_shr(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode)
        };

        self.emit_u32(asm::lsrv(x64, dest, lhs, rhs));
    }

    pub fn int_sar(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode)
        };

        self.emit_u32(asm::asrv(x64, dest, lhs, rhs));
    }

    pub fn int_or(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode)
        };

        self.emit_u32(asm::orr_shreg(x64, dest, lhs, rhs, Shift::LSL, 0));
    }

    pub fn int_and(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode)
        };

        self.emit_u32(asm::and_shreg(x64, dest, lhs, rhs, Shift::LSL, 0));
    }

    pub fn int_xor(&mut self, mode: MachineMode, dest: Reg, lhs: Reg, rhs: Reg) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode)
        };

        self.emit_u32(asm::eor_shreg(x64, dest, lhs, rhs, Shift::LSL, 0));
    }

    pub fn check_index_out_of_bounds(&mut self, pos: Position, array: Reg, index: Reg, temp: Reg) {
        self.load_mem(MachineMode::Int32,
                      temp,
                      Mem::Base(array, offset_of_array_length()));
        self.cmp_reg(MachineMode::Int32, index, temp);

        let lbl = self.create_label();
        self.jump_if(CondCode::UnsignedGreaterEq, lbl);
        self.emit_bailout(lbl, Trap::INDEX_OUT_OF_BOUNDS, pos);
    }

    pub fn load_nil(&mut self, dest: Reg) {
        self.emit_u32(movz(1, dest, 0, 0));
    }

    pub fn load_mem(&mut self, mode: MachineMode, dest: Reg, mem: Mem) {
        match mem {
            Mem::Local(offset) => {
                let scratch = self.get_scratch();
                self.load_int_const(MachineMode::Ptr, *scratch, offset as i64);

                let inst = match mode {
                    MachineMode::Int8 => asm::ldrb_ind(dest, REG_FP, *scratch, LdStExtend::LSL, 0),
                    MachineMode::Int32 => asm::ldrw_ind(dest, REG_FP, *scratch, LdStExtend::LSL, 0),
                    MachineMode::Int64 |
                    MachineMode::Ptr => asm::ldrx_ind(dest, REG_FP, *scratch, LdStExtend::LSL, 0),
                };

                self.emit_u32(inst);
            }

            Mem::Base(base, disp) => {
                let scratch = self.get_scratch();
                self.load_int_const(MachineMode::Ptr, *scratch, disp as i64);

                let inst = match mode {
                    MachineMode::Int8 => asm::ldrb_ind(dest, base, *scratch, LdStExtend::LSL, 0),
                    MachineMode::Int32 => asm::ldrw_ind(dest, base, *scratch, LdStExtend::LSL, 0),
                    MachineMode::Int64 |
                    MachineMode::Ptr => asm::ldrx_ind(dest, base, *scratch, LdStExtend::LSL, 0),
                };

                self.emit_u32(inst);
            }

            Mem::Index(base, index, scale, disp) => {
                assert!(mode.size() == scale);

                let scratch = self.get_scratch();
                self.load_int_const(MachineMode::Ptr, *scratch, disp as i64);
                self.emit_u32(asm::add_reg(1, *scratch, *scratch, base));

                let inst = match mode {
                    MachineMode::Int8 => asm::ldrb_ind(dest, *scratch, index, LdStExtend::LSL, 0),
                    MachineMode::Int32 => asm::ldrw_ind(dest, *scratch, index, LdStExtend::LSL, 1),
                    MachineMode::Int64 |
                    MachineMode::Ptr => asm::ldrx_ind(dest, *scratch, index, LdStExtend::LSL, 1),
                };

                self.emit_u32(inst);
            }
        }
    }

    pub fn store_mem(&mut self, mode: MachineMode, mem: Mem, src: Reg) {
        match mem {
            Mem::Local(offset) => {
                let scratch = self.get_scratch();
                self.load_int_const(MachineMode::Ptr, *scratch, offset as i64);

                let inst = match mode {
                    MachineMode::Int8 => asm::strb_ind(src, REG_FP, *scratch, LdStExtend::LSL, 0),
                    MachineMode::Int32 => asm::strw_ind(src, REG_FP, *scratch, LdStExtend::LSL, 0),
                    MachineMode::Int64 |
                    MachineMode::Ptr => asm::strx_ind(src, REG_FP, *scratch, LdStExtend::LSL, 0),
                };

                self.emit_u32(inst);
            }

            Mem::Base(base, disp) => {
                let scratch = self.get_scratch();
                self.load_int_const(MachineMode::Ptr, *scratch, disp as i64);

                let inst = match mode {
                    MachineMode::Int8 => asm::strb_ind(src, base, *scratch, LdStExtend::LSL, 0),
                    MachineMode::Int32 => asm::strw_ind(src, base, *scratch, LdStExtend::LSL, 0),
                    MachineMode::Int64 |
                    MachineMode::Ptr => asm::strx_ind(src, base, *scratch, LdStExtend::LSL, 0),
                };

                self.emit_u32(inst);
            }

            Mem::Index(base, index, scale, disp) => {
                assert!(mode.size() == scale);

                let scratch = self.get_scratch();
                self.load_int_const(MachineMode::Ptr, *scratch, disp as i64);
                self.emit_u32(asm::add_reg(1, *scratch, *scratch, base));

                let inst = match mode {
                    MachineMode::Int8 => asm::strb_ind(src, *scratch, index, LdStExtend::LSL, 0),
                    MachineMode::Int32 => asm::strw_ind(src, *scratch, index, LdStExtend::LSL, 1),
                    MachineMode::Int64 |
                    MachineMode::Ptr => asm::strx_ind(src, *scratch, index, LdStExtend::LSL, 1),
                };

                self.emit_u32(inst);
            }
        }
    }

    pub fn copy_reg(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        self.emit_u32(orr_shreg(size_flag(mode), dest, REG_ZERO, src, Shift::LSL, 0));
    }

    pub fn extend_int_long(&mut self, dest: Reg, src: Reg) {
        self.emit_u32(asm::sxtw(dest, src));
    }

    pub fn extend_byte(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        match mode {
            MachineMode::Int32 => {},
            MachineMode::Int64 => self.emit_u32(asm::uxtw(dest, src)),
            _ => panic!("unimplemented mode {:?}", mode)
        }
    }

    pub fn load_constpool(&mut self, dest: Reg, disp: i32) {
        self.emit_u32(asm::adr(dest, -disp));
        self.load_mem(MachineMode::Ptr, dest, Mem::Base(dest, 0));
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
            let (halfword, invert) = if count_empty_half_words(!imm, register_size) >
                                        count_empty_half_words(imm, register_size) {
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
            _ => panic!("unimplemented mode {:?}", mode)
        };

        self.emit_u32(sub_reg(x64, dest, REG_ZERO, src));
    }

    pub fn int_not(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        let x64 = match mode {
            MachineMode::Int32 => 0,
            MachineMode::Int64 => 1,
            _ => panic!("unimplemented mode {:?}", mode)
        };

        self.emit_u32(orn_shreg(x64, dest, REG_ZERO, src, Shift::LSL, 0));
    }

    pub fn bool_not(&mut self, dest: Reg, src: Reg) {
        let scratch = self.get_scratch();

        self.emit_u32(movz(0, *scratch, 1, 0));
        self.emit_u32(eor_shreg(0, dest, src, *scratch, Shift::LSL, 0));
        self.emit_u32(uxtb(dest, dest));
    }

    pub fn trap(&mut self, trap: Trap) {
        self.emit_u32(asm::trap(trap));
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

            let mut slice = &mut self.data[jmp.at..];
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
        MachineMode::Int8 |
        MachineMode::Int32 => 0,
        MachineMode::Ptr |
        MachineMode::Int64 => 1,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ty::MachineMode::{Int8, Int32, Ptr};

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
    fn test_load_mem_local_ptr() {
        let i1 = asm::movz(1, R16, 1, 0);
        let i2 = asm::ldrx_ind(R1, REG_FP, R16, LdStExtend::LSL, 0);

        let mut masm = MacroAssembler::new();
        masm.load_mem(Ptr, R1, Mem::Local(1));
        assert_emit!(i1, i2; masm);
    }

    #[test]
    fn test_load_mem_local_int32() {
        let i1 = asm::movz(1, R16, 2, 0);
        let i2 = asm::ldrw_ind(R1, REG_FP, R16, LdStExtend::LSL, 0);

        let mut masm = MacroAssembler::new();
        masm.load_mem(Int32, R1, Mem::Local(2));
        assert_emit!(i1, i2; masm);
    }

    #[test]
    fn test_load_mem_local_int8() {
        let i1 = asm::movz(1, R16, 3, 0);
        let i2 = asm::ldrb_ind(R1, REG_FP, R16, LdStExtend::LSL, 0);

        let mut masm = MacroAssembler::new();
        masm.load_mem(Int8, R1, Mem::Local(3));
        assert_emit!(i1, i2; masm);
    }

    #[test]
    fn test_load_mem_base_ptr() {
        let i1 = asm::movz(1, R16, 1, 0);
        let i2 = asm::ldrx_ind(R1, R10, R16, LdStExtend::LSL, 0);

        let mut masm = MacroAssembler::new();
        masm.load_mem(Ptr, R1, Mem::Base(R10, 1));
        assert_emit!(i1, i2; masm);
    }

    #[test]
    fn test_load_mem_base_int32() {
        let i1 = asm::movz(1, R16, 2, 0);
        let i2 = asm::ldrw_ind(R1, R2, R16, LdStExtend::LSL, 0);

        let mut masm = MacroAssembler::new();
        masm.load_mem(Int32, R1, Mem::Base(R2, 2));
        assert_emit!(i1, i2; masm);
    }

    #[test]
    fn test_load_mem_base_int8() {
        let i1 = asm::movz(1, R16, 3, 0);
        let i2 = asm::ldrb_ind(R1, R3, R16, LdStExtend::LSL, 0);

        let mut masm = MacroAssembler::new();
        masm.load_mem(Int8, R1, Mem::Base(R3, 3));
        assert_emit!(i1, i2; masm);
    }

    #[test]
    fn test_load_mem_index_ptr() {
        let i1 = asm::movz(1, R16, 1, 0);
        let i2 = asm::add_reg(1, R16, R16, R10);
        let i3 = asm::ldrx_ind(R1, R16, R11, LdStExtend::LSL, 1);

        let mut masm = MacroAssembler::new();
        masm.load_mem(Ptr, R1, Mem::Index(R10, R11, 8, 1));
        assert_emit!(i1, i2, i3; masm);
    }

    #[test]
    fn test_load_mem_index_int32() {
        let i1 = asm::movz(1, R16, 2, 0);
        let i2 = asm::add_reg(1, R16, R16, R2);
        let i3 = asm::ldrw_ind(R1, R16, R12, LdStExtend::LSL, 1);

        let mut masm = MacroAssembler::new();
        masm.load_mem(Int32, R1, Mem::Index(R2, R12, 4, 2));
        assert_emit!(i1, i2, i3; masm);
    }

    #[test]
    fn test_load_mem_index_int8() {
        let i1 = asm::movz(1, R16, 3, 0);
        let i2 = asm::add_reg(1, R16, R16, R3);
        let i3 = asm::ldrb_ind(R1, R16, R13, LdStExtend::LSL, 0);

        let mut masm = MacroAssembler::new();
        masm.load_mem(Int8, R1, Mem::Index(R3, R13, 1, 3));
        assert_emit!(i1, i2, i3; masm);
    }
}
