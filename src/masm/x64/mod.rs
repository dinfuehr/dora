use baseline::codegen::CondCode;
use cpu::asm;
use cpu::*;
use masm::MacroAssembler;

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

    pub fn set(&mut self, dest: Reg, op: CondCode) {
        asm::emit_setb_reg(self, op, dest);
        asm::emit_movzbl_reg_reg(self, dest, dest);
    }
}