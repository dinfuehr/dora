use baseline::codegen::CondCode;
use cpu::asm;
use cpu::emit;
use cpu::*;
use lexer::position::Position;
use masm::{MacroAssembler, Label};
use mem::ptr_width;
use object::IntArray;
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

    pub fn direct_call(&mut self, ptr: *const u8) {
        let disp = self.add_addr(ptr);
        let pos = self.pos() as i32;

        emit::load_constpool(self, REG_RESULT, disp + pos);
        emit::call_reg(self, REG_RESULT);
    }

    pub fn indirect_call(&mut self, index: u32) {
        let obj = REG_PARAMS[0];

        // REG_RESULT = [obj] (load vtable)
        emit::load_mem(self, MachineMode::Ptr, REG_RESULT, Mem::Base(obj, 0));

        // calculate offset of VTable entry
        let disp = VTable::offset_of_method_table() + (index as i32) * ptr_width();

        // load vtable entry
        emit::load_mem(self, MachineMode::Ptr, REG_RESULT, Mem::Base(REG_RESULT, disp));

        // call *REG_RESULT
        emit::call_reg(self, REG_RESULT);
    }

    pub fn load_array_elem(&mut self, mode: MachineMode, dest: Reg, array: Reg, index: Reg) {
        assert!(mode == MachineMode::Int32);

        emit::load_mem(self, mode, dest, Mem::Index(array, index,
                       mode.size(), IntArray::offset_of_data()));
    }

    pub fn store_array_elem(&mut self, mode: MachineMode, array: Reg, index: Reg, value: Reg) {
        assert!(mode == MachineMode::Int32);

        emit::store_mem(self, MachineMode::Int32, Mem::Index(array, index,
                        4, IntArray::offset_of_data()), value);
    }

    pub fn test_if_nil_bailout(&mut self, pos: Position, reg: Reg) {
        asm::emit_testq_reg_reg(self, reg, reg);

        let lbl = self.create_label();
        emit::jump_if(self, CondCode::Zero, lbl);
        self.emit_bailout(lbl, trap::NIL, pos);
    }

    pub fn test_if_nil(&mut self, reg: Reg) -> Label {
        asm::emit_testq_reg_reg(self, reg, reg);

        let lbl = self.create_label();
        emit::jump_if(self, CondCode::Zero, lbl);

        lbl
    }

    pub fn set(&mut self, dest: Reg, op: CondCode) {
        asm::emit_setb_reg(self, op, dest);
        asm::emit_movzbl_reg_reg(self, dest, dest);
    }

    pub fn cmp_mem(&mut self, mode: MachineMode, mem: Mem, rhs: Reg) {
        match mem {
            Mem::Local(offset) => asm::emit_cmp_mem_reg(self, mode, REG_FP, offset, rhs),
            Mem::Base(base, disp) => asm::emit_cmp_mem_reg(self, mode, base, disp, rhs),
            Mem::Index(base, index, scale, disp) =>
                asm::emit_cmp_memindex_reg(self, mode, base, index, scale, disp, rhs)
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
            MachineMode::Int8
                | MachineMode::Int32 => asm::emit_cmpl_reg_reg(self, rhs, lhs),
            MachineMode::Ptr => asm::emit_cmpq_reg_reg(self, rhs, lhs),
        }
    }
}