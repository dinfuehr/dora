use baseline::codegen::CondCode;
use cpu::asm;
use cpu::arm64::asm::*;
use cpu::arm64::reg::*;
use cpu::{Mem, Reg};
use lexer::position::Position;
use masm::{MacroAssembler, Label};
use mem::ptr_width;
use object::IntArray;
use os::signal::Trap;
use ty::MachineMode;
use vtable::VTable;

impl MacroAssembler {
    pub fn prolog(&mut self, stacksize: i32) {
        self.emit_u32(asm::stp_pre(1, REG_FP, REG_LR, REG_SP, -2));
        self.emit_u32(asm::add_reg(1, REG_FP, REG_SP, REG_ZERO));

        if stacksize > 0 {
            let scratch = get_scratch();
            self.load_int_const(MachineMode::Ptr, scratch, stacksize);
            self.emit_u32(asm::sub_reg(1, REG_SP, REG_SP, scratch));
        }
    }

    pub fn epilog(&mut self, stacksize: i32) {
        if stacksize > 0 {
            let scratch = get_scratch();
            self.load_int_const(MachineMode::Ptr, scratch, stacksize);
            self.emit_u32(asm::add_reg(1, REG_SP, REG_SP, scratch));
        }

        self.emit_u32(asm::add_reg(1, REG_SP, REG_FP, REG_ZERO));
        self.emit_u32(asm::ldp_post(1, REG_FP, REG_LR, REG_SP, 2));
        self.emit_u32(asm::ret());
    }

    pub fn direct_call(&mut self, ptr: *const u8) {
        let disp = self.add_addr(ptr);
        let pos = self.pos() as i32;

        let scratch = get_scratch();

        self.load_constpool(REG_RESULT, disp + pos);
        self.emit_u32(asm::br(scratch));
    }

    pub fn indirect_call(&mut self, index: u32) {
        let obj = REG_PARAMS[0];

        // REG_RESULT = [obj] (load vtable)
        self.load_mem(MachineMode::Ptr, REG_RESULT, Mem::Base(obj, 0));

        // calculate offset of VTable entry
        let disp = VTable::offset_of_method_table() + (index as i32) * ptr_width();

        // load vtable entry
        self.load_mem(MachineMode::Ptr, REG_RESULT, Mem::Base(REG_RESULT, disp));

        // call *REG_RESULT
        self.emit_u32(asm::blr(REG_RESULT));
    }

    pub fn load_array_elem(&mut self, mode: MachineMode, dest: Reg, array: Reg, index: Reg) {
        assert!(mode == MachineMode::Int32);

        self.load_mem(mode,
                      dest,
                      Mem::Index(array, index, mode.size(), IntArray::offset_of_data()));
    }

    pub fn store_array_elem(&mut self, mode: MachineMode, array: Reg, index: Reg, value: Reg) {
        assert!(mode == MachineMode::Int32);

        self.store_mem(MachineMode::Int32,
                       Mem::Index(array, index, 4, IntArray::offset_of_data()),
                       value);
    }

    pub fn test_if_nil_bailout(&mut self, pos: Position, reg: Reg) {
        self.cmp_reg(MachineMode::Ptr, reg, reg);

        let lbl = self.create_label();
        self.jump_if(CondCode::Zero, lbl);
        self.emit_bailout(lbl, Trap::NIL, pos);
    }

    pub fn test_if_nil(&mut self, reg: Reg) -> Label {
        self.cmp_reg(MachineMode::Ptr, reg, reg);

        let lbl = self.create_label();
        self.jump_if(CondCode::Zero, lbl);

        lbl
    }

    pub fn set(&mut self, dest: Reg, op: CondCode) {
        self.emit_u32(asm::cmp_imm(0, dest, 0, 0));
        self.emit_u32(asm::cset(0, dest, CondCode::NotEqual.into()));
    }

    pub fn cmp_mem(&mut self, mode: MachineMode, mem: Mem, rhs: Reg) {
        let scratch = get_scratch();

        self.load_mem(mode, scratch, mem);
        self.cmp_reg(mode, scratch, rhs);
    }

    pub fn cmp_mem_imm(&mut self, mode: MachineMode, mem: Mem, imm: i32) {
        let (scratch1, scratch2) = get_scratch_registers();

        self.load_mem(mode, scratch1, mem);
        self.load_int_const(mode, scratch2, imm);

        self.cmp_reg(mode, scratch1, scratch2);
    }

    pub fn cmp_reg(&mut self, mode: MachineMode, lhs: Reg, rhs: Reg) {
        self.emit_u32(asm::cmp_shreg(size_flag(mode), lhs, rhs, Shift::LSL, 0));
    }

    pub fn test_and_jump_if(&mut self, cond: CondCode, reg: Reg, lbl: Label) {
        assert!(cond == CondCode::Zero || cond == CondCode::NonZero);

        unimplemented!();
    }

    pub fn jump_if(&mut self, cond: CondCode, lbl: Label) {
        unimplemented!();
    }

    pub fn jump(&mut self, lbl: Label) {
        unimplemented!();
    }

    pub fn int_div(&mut self, dest: Reg, lhs: Reg, rhs: Reg) {
        self.emit_u32(asm::sdiv(0, dest, lhs, rhs));
    }

    pub fn int_mod(&mut self, dest: Reg, lhs: Reg, rhs: Reg) {
        self.emit_u32(asm::sdiv(0, dest, lhs, rhs));
        self.emit_u32(asm::msub(0, dest, dest, rhs, lhs));
    }

    pub fn int_mul(&mut self, dest: Reg, lhs: Reg, rhs: Reg) {
        self.emit_u32(asm::mul(0, dest, lhs, rhs));
    }

    pub fn int_add(&mut self, dest: Reg, lhs: Reg, rhs: Reg) {
        self.emit_u32(asm::add_reg(0, dest, lhs, rhs));
    }

    pub fn int_sub(&mut self, dest: Reg, lhs: Reg, rhs: Reg) {
        self.emit_u32(asm::sub_reg(0, dest, lhs, rhs));
    }

    pub fn int_shl(&mut self, dest: Reg, lhs: Reg, rhs: Reg) {
        self.emit_u32(asm::lslv(0, dest, lhs, rhs));
    }

    pub fn int_or(&mut self, dest: Reg, lhs: Reg, rhs: Reg) {
        self.emit_u32(asm::orr_shreg(0, dest, lhs, rhs, Shift::LSL, 0));
    }

    pub fn int_and(&mut self, dest: Reg, lhs: Reg, rhs: Reg) {
        self.emit_u32(asm::and_shreg(0, dest, lhs, rhs, Shift::LSL, 0));
    }

    pub fn int_xor(&mut self, dest: Reg, lhs: Reg, rhs: Reg) {
        self.emit_u32(asm::eor_shreg(0, dest, lhs, rhs, Shift::LSL, 0));
    }

    pub fn check_index_out_of_bounds(&mut self, pos: Position, array: Reg, index: Reg, temp: Reg) {
        self.load_mem(MachineMode::Int32,
                      temp,
                      Mem::Base(array, IntArray::offset_of_length()));
        self.cmp_reg(MachineMode::Int32, index, temp);

        let lbl = self.create_label();
        self.jump_if(CondCode::UnsignedGreaterEq, lbl);
        self.emit_bailout(lbl, Trap::INDEX_OUT_OF_BOUNDS, pos);
    }

    pub fn load_nil(&mut self, dest: Reg) {
        self.emit_u32(add_imm(1, dest, REG_ZERO, 0, 0));
    }

    pub fn load_mem(&mut self, mode: MachineMode, dest: Reg, mem: Mem) {
        unimplemented!();
    }

    pub fn store_mem(&mut self, mode: MachineMode, mem: Mem, src: Reg) {
        unimplemented!();
    }

    pub fn copy_reg(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        self.emit_u32(orr_shreg(size_flag(mode), dest, REG_ZERO, src, Shift::LSL, 0));
    }

    pub fn load_constpool(&mut self, dest: Reg, disp: i32) {
        unimplemented!();
    }

    pub fn call_reg(&mut self, reg: Reg) {
        self.emit_u32(blr(reg));
    }

    pub fn debug(&mut self) {
        unimplemented!();
    }

    pub fn load_int_const(&mut self, mode: MachineMode, dest: Reg, imm: i32) {
        let sf = size_flag(mode);
        let register_size = match mode {
            MachineMode::Int8 => unimplemented!(),
            MachineMode::Int32 => 32,
            MachineMode::Ptr => 64,
        };
        let imm = imm as i64 as u64;

        if fits_movz(imm, register_size) {
            self.emit_u32(movz(sf, dest, imm as u32, shift_movz(imm)));

        } else if fits_movn(imm, register_size) {
            self.emit_u32(movn(sf, dest, imm as u32, shift_movn(imm)));

        } else {
            unimplemented!();
        }
    }

    pub fn load_true(&mut self, dest: Reg) {
        self.emit_u32(movz(0, dest, 1, 0));
    }

    pub fn load_false(&mut self, dest: Reg) {
        self.emit_u32(movz(0, dest, 0, 0));
    }

    pub fn int_neg(&mut self, dest: Reg, src: Reg) {
        self.emit_u32(sub_reg(0, dest, REG_ZERO, src));
    }

    pub fn int_not(&mut self, dest: Reg, src: Reg) {
        self.emit_u32(orn_shreg(0, dest, REG_ZERO, src, Shift::LSL, 0));
    }

    pub fn bool_not(&mut self, dest: Reg, src: Reg) {
        let scratch = get_scratch();

        self.emit_u32(movz(0, scratch, 1, 0));
        self.emit_u32(eor_shreg(0, dest, src, scratch, Shift::LSL, 0));
        self.emit_u32(uxtb(dest, dest));
    }

    pub fn trap(&mut self, trap: Trap) {
        unimplemented!();
    }
}

fn size_flag(mode: MachineMode) -> u32 {
    match mode {
        MachineMode::Int8 |
        MachineMode::Int32 => 0,
        MachineMode::Ptr => 1,
    }
}

fn get_scratch() -> Reg {
    SCRATCH[0]
}

fn get_scratch_registers() -> (Reg, Reg) {
    (SCRATCH[0], SCRATCH[1])
}
