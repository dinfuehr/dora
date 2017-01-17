use baseline::fct::BailoutInfo;
use baseline::codegen::CondCode;
use byteorder::{LittleEndian, WriteBytesExt};
use cpu::*;
use ctxt::FctId;
use lexer::position::Position;
use masm::{MacroAssembler, Label};
use mem::ptr_width;
use object::IntArray;
use os::signal::Trap;
use ty::MachineMode;
use vtable::VTable;

impl MacroAssembler {
    pub fn prolog(&mut self, stacksize: i32) {
        asm::emit_pushq_reg(self, RBP);
        asm::emit_movq_reg_reg(self, RSP, RBP);

        if stacksize > 0 {
            asm::emit_subq_imm_reg(self, stacksize, RSP);
        }
    }

    pub fn epilog(&mut self, stacksize: i32) {
        if stacksize > 0 {
            asm::emit_addq_imm_reg(self, stacksize, RSP);
        }

        asm::emit_popq_reg(self, RBP);
        asm::emit_retq(self);
    }

    pub fn direct_call(&mut self, fct_id: FctId, ptr: *const u8) {
        let disp = self.add_addr(ptr);
        let pos = self.pos() as i32;

        self.load_constpool(REG_RESULT, disp + pos);
        self.call_reg(REG_RESULT);

        let pos = self.pos() as i32;
        self.emit_bailout_info(BailoutInfo::Compile(fct_id, disp + pos));
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
        self.call_reg(REG_RESULT);
        self.emit_bailout_info(BailoutInfo::VirtCompile(index));
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

    pub fn set(&mut self, dest: Reg, op: CondCode) {
        asm::emit_setb_reg(self, op, dest);
        asm::emit_movzbl_reg_reg(self, dest, dest);
    }

    pub fn cmp_mem(&mut self, mode: MachineMode, mem: Mem, rhs: Reg) {
        match mem {
            Mem::Local(offset) => asm::emit_cmp_mem_reg(self, mode, REG_FP, offset, rhs),
            Mem::Base(base, disp) => asm::emit_cmp_mem_reg(self, mode, base, disp, rhs),
            Mem::Index(base, index, scale, disp) => {
                asm::emit_cmp_memindex_reg(self, mode, base, index, scale, disp, rhs)
            }
        }
    }

    pub fn cmp_mem_imm(&mut self, mode: MachineMode, mem: Mem, imm: i32) {
        match mem {
            Mem::Local(_) => unimplemented!(),
            Mem::Base(base, disp) => asm::emit_cmp_mem_imm(self, mode, base, disp, imm),
            Mem::Index(_, _, _, _) => unimplemented!(),
        }
    }

    pub fn cmp_reg(&mut self, mode: MachineMode, lhs: Reg, rhs: Reg) {
        match mode {
            MachineMode::Int8 |
            MachineMode::Int32 => asm::emit_cmpl_reg_reg(self, rhs, lhs),
            MachineMode::Ptr => asm::emit_cmpq_reg_reg(self, rhs, lhs),
        }
    }

    pub fn cmp_zero(&mut self, mode: MachineMode, lhs: Reg) {
        asm::emit_cmp_imm_reg(self, mode, 0, lhs);
    }

    pub fn test_and_jump_if(&mut self, cond: CondCode, reg: Reg, lbl: Label) {
        assert!(cond == CondCode::Zero || cond == CondCode::NonZero);

        asm::emit_testl_reg_reg(self, reg, reg);
        self.jump_if(cond, lbl);
    }

    pub fn jump_if(&mut self, cond: CondCode, lbl: Label) {
        asm::emit_jcc(self, cond, lbl);
    }

    pub fn jump(&mut self, lbl: Label) {
        asm::emit_jmp(self, lbl);
    }

    pub fn int_div(&mut self, dest: Reg, lhs: Reg, rhs: Reg) {
        assert_eq!(RAX, lhs);

        asm::emit_cltd(self);
        asm::emit_idivl_reg_reg(self, rhs);

        if dest != RAX {
            asm::emit_movl_reg_reg(self, RAX, dest);
        }
    }

    pub fn int_mod(&mut self, dest: Reg, lhs: Reg, rhs: Reg) {
        assert_eq!(RAX, lhs);

        asm::emit_cltd(self);
        asm::emit_idivl_reg_reg(self, rhs);

        if dest != RDX {
            asm::emit_movl_reg_reg(self, RDX, dest);
        }
    }

    pub fn int_mul(&mut self, dest: Reg, lhs: Reg, rhs: Reg) {
        asm::emit_imull_reg_reg(self, rhs, lhs);

        if dest != lhs {
            asm::emit_movl_reg_reg(self, lhs, dest);
        }
    }

    pub fn int_add(&mut self, dest: Reg, lhs: Reg, rhs: Reg) {
        asm::emit_addl_reg_reg(self, rhs, lhs);

        if dest != lhs {
            asm::emit_movl_reg_reg(self, lhs, dest);
        }
    }

    pub fn int_sub(&mut self, dest: Reg, lhs: Reg, rhs: Reg) {
        asm::emit_subl_reg_reg(self, rhs, lhs);

        if dest != lhs {
            asm::emit_movl_reg_reg(self, lhs, dest);
        }
    }

    pub fn int_shl(&mut self, dest: Reg, lhs: Reg, rhs: Reg) {
        if rhs != RCX {
            assert!(lhs != RCX);
            asm::emit_movq_reg_reg(self, rhs, RCX);
        }

        asm::emit_shll_reg_cl(self, lhs);

        if dest != lhs {
            asm::emit_movl_reg_reg(self, lhs, dest);
        }
    }

    pub fn int_or(&mut self, dest: Reg, lhs: Reg, rhs: Reg) {
        asm::emit_orl_reg_reg(self, rhs, lhs);

        if dest != lhs {
            asm::emit_movl_reg_reg(self, lhs, dest);
        }
    }

    pub fn int_and(&mut self, dest: Reg, lhs: Reg, rhs: Reg) {
        asm::emit_andl_reg_reg(self, rhs, lhs);

        if dest != lhs {
            asm::emit_movl_reg_reg(self, lhs, dest);
        }
    }

    pub fn int_xor(&mut self, dest: Reg, lhs: Reg, rhs: Reg) {
        asm::emit_xorl_reg_reg(self, rhs, lhs);

        if dest != lhs {
            asm::emit_movl_reg_reg(self, lhs, dest);
        }
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
        asm::emit_movl_imm_reg(self, 0, dest);
    }

    pub fn load_mem(&mut self, mode: MachineMode, dest: Reg, mem: Mem) {
        match mem {
            Mem::Local(offset) => {
                match mode {
                    MachineMode::Int8 => asm::emit_movzbl_memq_reg(self, RBP, offset, dest),
                    MachineMode::Int32 => asm::emit_movl_memq_reg(self, RBP, offset, dest),
                    MachineMode::Ptr => asm::emit_movq_memq_reg(self, RBP, offset, dest),
                }
            }

            Mem::Base(base, disp) => {
                match mode {
                    MachineMode::Int8 => asm::emit_movzbl_memq_reg(self, base, disp, dest),
                    MachineMode::Int32 => asm::emit_movl_memq_reg(self, base, disp, dest),
                    MachineMode::Ptr => asm::emit_movq_memq_reg(self, base, disp, dest),
                }
            }

            Mem::Index(base, index, scale, disp) => {
                asm::emit_mov_memindex_reg(self, mode, base, index, scale, disp, dest)
            }
        }
    }

    pub fn store_mem(&mut self, mode: MachineMode, mem: Mem, src: Reg) {
        match mem {
            Mem::Local(offset) => {
                match mode {
                    MachineMode::Int8 => asm::emit_movb_reg_memq(self, src, RBP, offset),
                    MachineMode::Int32 => asm::emit_movl_reg_memq(self, src, RBP, offset),
                    MachineMode::Ptr => asm::emit_movq_reg_memq(self, src, RBP, offset),
                }
            }

            Mem::Base(base, disp) => {
                match mode {
                    MachineMode::Int8 => asm::emit_movb_reg_memq(self, src, base, disp),
                    MachineMode::Int32 => asm::emit_movl_reg_memq(self, src, base, disp),
                    MachineMode::Ptr => asm::emit_movq_reg_memq(self, src, base, disp),
                }
            }

            Mem::Index(base, index, scale, disp) => {
                asm::emit_mov_reg_memindex(self, mode, src, base, index, scale, disp)
            }
        }
    }

    pub fn copy_reg(&mut self, mode: MachineMode, dest: Reg, src: Reg) {
        match mode {
            MachineMode::Int8 |
            MachineMode::Int32 => asm::emit_movl_reg_reg(self, src, dest),
            MachineMode::Ptr => asm::emit_movq_reg_reg(self, src, dest),
        }
    }

    pub fn load_constpool(&mut self, dest: Reg, disp: i32) {
        // next instruction has 7 bytes
        let disp = -(disp + 7);

        asm::emit_movq_memq_reg(self, RIP, disp, dest); // 7 bytes
    }

    pub fn call_reg(&mut self, reg: Reg) {
        asm::emit_callq_reg(self, reg);
    }

    // emit debug instruction
    pub fn debug(&mut self) {
        // emit int3 = 0xCC
        asm::emit_op(self, 0xCC);
    }

    pub fn load_int_const(&mut self, mode: MachineMode, dest: Reg, imm: i32) {
        match mode {
            MachineMode::Int8 => unimplemented!(),
            MachineMode::Int32 => asm::emit_movl_imm_reg(self, imm, dest),
            MachineMode::Ptr => asm::emit_movq_imm_reg(self, imm, dest),
        }
    }

    pub fn load_true(&mut self, dest: Reg) {
        asm::emit_movl_imm_reg(self, 1, dest);
    }

    pub fn load_false(&mut self, dest: Reg) {
        asm::emit_movl_imm_reg(self, 0, dest);
    }

    pub fn int_neg(&mut self, dest: Reg, src: Reg) {
        asm::emit_negl_reg(self, src);

        if dest != src {
            asm::emit_movl_reg_reg(self, src, dest);
        }
    }

    pub fn int_not(&mut self, dest: Reg, src: Reg) {
        asm::emit_notl_reg(self, src);

        if dest != src {
            asm::emit_movl_reg_reg(self, src, dest);
        }
    }

    pub fn bool_not(&mut self, dest: Reg, src: Reg) {
        asm::emit_xorb_imm_reg(self, 1, src);
        asm::emit_andb_imm_reg(self, 1, src);

        if dest != src {
            asm::emit_movl_reg_reg(self, src, dest);
        }
    }

    pub fn trap(&mut self, trap: Trap) {
        let dest = R10;

        // mov r10, [Trap::COMPILER]
        asm::emit_rex(self, 1, dest.msb(), 0, 0);
        asm::emit_op(self, 0x8b);
        asm::emit_modrm(self, 0, dest.and7(), 0b100);
        asm::emit_sib(self, 0, 0b100, 0b101);
        asm::emit_u32(self, trap.int());
    }

    pub fn emit_label(&mut self, lbl: Label) {
        let value = self.labels[lbl.index()];

        match value {
            // backward jumps already know their target
            Some(idx) => {
                let current = self.pos() + 4;
                let target = idx;

                let diff = -((current - target) as i32);
                self.emit_u32(diff as u32);
            }

            // forward jumps do not know their target yet
            // we need to do this later...
            None => {
                let pos = self.pos();
                self.emit_u32(0);
                self.jumps.push(ForwardJump { at: pos, to: lbl });
            }
        }
    }

    pub fn fix_forward_jumps(&mut self) {
        for jmp in &self.jumps {
            let target = self.labels[jmp.to.0].expect("label not defined");
            let diff = (target - jmp.at - 4) as i32;

            let mut slice = &mut self.data[jmp.at..];
            slice.write_u32::<LittleEndian>(diff as u32).unwrap();
        }
    }
}

#[derive(Debug)]
pub struct ForwardJump {
    at: usize,
    to: Label,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_backward() {
        let mut masm = MacroAssembler::new();
        let lbl = masm.create_label();
        masm.bind_label(lbl);
        masm.emit_label(lbl);

        assert_eq!(vec![0xfc, 0xff, 0xff, 0xff], masm.data());
    }

    #[test]
    fn test_forward_with_gap() {
        let mut masm = MacroAssembler::new();
        let lbl = masm.create_label();
        masm.emit_label(lbl);
        masm.emit_u8(0x11);
        masm.bind_label(lbl);

        assert_eq!(vec![1, 0, 0, 0, 0x11], masm.data());
    }

    #[test]
    fn test_forward() {
        let mut masm = MacroAssembler::new();
        let lbl = masm.create_label();
        masm.emit_label(lbl);
        masm.bind_label(lbl);

        assert_eq!(vec![0, 0, 0, 0], masm.data());
    }

    #[test]
    fn test_backward_with_gap() {
        let mut masm = MacroAssembler::new();
        let lbl = masm.create_label();
        masm.bind_label(lbl);
        masm.emit_u8(0x33);
        masm.emit_label(lbl);

        assert_eq!(vec![0x33, 0xfb, 0xff, 0xff, 0xff], masm.data());
    }

    #[test]
    #[should_panic]
    fn test_label_undefined() {
        let mut masm = MacroAssembler::new();
        let lbl = masm.create_label();

        masm.emit_label(lbl);
        masm.jit(0);
    }
}
