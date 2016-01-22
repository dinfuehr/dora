use ast::CmpOp;
use cpu::instr::*;
use cpu::*;
use ctxt::*;
use jit::buffer::*;
use jit::codegen::JumpCond;
use sym::BuiltinType;

pub fn prolog(buf: &mut Buffer, stacksize: i32) {
    emit_pushq_reg(buf, RBP);
    emit_movq_reg_reg(buf, RSP, RBP);

    if stacksize > 0 {
        emit_subq_imm_reg(buf, stacksize, RSP);
    }
}

pub fn epilog(buf: &mut Buffer, stacksize: i32) {
    if stacksize > 0 {
        emit_addq_imm_reg(buf, stacksize, RSP);
    }

    emit_popq_reg(buf, RBP);
    emit_retq(buf);
}

pub fn cmpl_setl(buf: &mut Buffer, lhs: Reg, op: CmpOp, rhs: Reg, dest: Reg) {
    emit_cmpl_reg_reg(buf, rhs, lhs);
    emit_setb_reg(buf, op, dest);
    emit_movzbl_reg_reg(buf, dest, dest);
}

pub fn jump_if(buf: &mut Buffer, cond: JumpCond, reg: Reg, lbl: Label) {
    emit_testl_reg_reg(buf, reg, reg);

    match cond {
        JumpCond::Zero => emit_jz(buf, lbl),
        JumpCond::NonZero => emit_jnz(buf, lbl)
    }
}

pub fn jump(buf: &mut Buffer, lbl: Label) {
    emit_jmp(buf, lbl);
}

pub fn divl(buf: &mut Buffer, lhs: Reg, rhs: Reg, dest: Reg) -> Reg {
    assert_eq!(RAX, lhs);

    emit_cltd(buf);
    emit_idivl_reg_reg(buf, rhs);

    RAX
}

pub fn modl(buf: &mut Buffer, lhs: Reg, rhs: Reg, dest: Reg) -> Reg {
    assert_eq!(RAX, lhs);

    emit_cltd(buf);
    emit_idivl_reg_reg(buf, rhs);

    RDX
}

pub fn mull(buf: &mut Buffer, lhs: Reg, rhs: Reg, dest: Reg) -> Reg {
    emit_imull_reg_reg(buf, rhs, lhs);

    lhs
}

pub fn addl(buf: &mut Buffer, lhs: Reg, rhs: Reg, dest: Reg) -> Reg {
    emit_addl_reg_reg(buf, rhs, lhs);

    lhs
}

pub fn subl(buf: &mut Buffer, lhs: Reg, rhs: Reg, dest: Reg) -> Reg {
    emit_subl_reg_reg(buf, rhs, lhs);

    lhs
}

pub fn orl(buf: &mut Buffer, lhs: Reg, rhs: Reg, dest: Reg) -> Reg {
    emit_orl_reg_reg(buf, rhs, lhs);

    lhs
}

pub fn andl(buf: &mut Buffer, lhs: Reg, rhs: Reg, dest: Reg) -> Reg {
    emit_andl_reg_reg(buf, rhs, lhs);

    lhs
}

pub fn xorl(buf: &mut Buffer, lhs: Reg, rhs: Reg, dest: Reg) -> Reg {
    emit_xorl_reg_reg(buf, rhs, lhs);

    lhs
}

pub fn mov_local_reg(buf: &mut Buffer, ty: BuiltinType, offset: i32, dest: Reg) {
    match ty {
        BuiltinType::Bool => emit_movzbl_memq_reg(buf, RBP, offset, dest),
        BuiltinType::Int => emit_movl_memq_reg(buf, RBP, offset, dest),
        BuiltinType::Str => emit_movq_memq_reg(buf, RBP, offset, dest),
        BuiltinType::Unit => {},
    }
}

pub fn mov_reg_local(buf: &mut Buffer, ty: BuiltinType, src: Reg, offset: i32) {
    match ty {
        BuiltinType::Bool => emit_movb_reg_memq(buf, src, RBP, offset),
        BuiltinType::Int => emit_movl_reg_memq(buf, src, RBP, offset),
        BuiltinType::Str => emit_movq_reg_memq(buf, src, RBP, offset),
        BuiltinType::Unit => {},
    }
}

pub fn movl_reg_reg(buf: &mut Buffer, src: Reg, dest: Reg) {
    emit_movl_reg_reg(buf, src, dest);
}

pub fn call(buf: &mut Buffer, disp: i32) {
    // next instruction has 7 bytes
    let disp = -(disp + 7);

    emit_movq_memq_reg(buf, RIP, disp, REG_RESULT); // 7 bytes
    emit_callq_reg(buf, REG_RESULT);
}

pub fn push_param(buf: &mut Buffer, reg: Reg) {
    emit_pushq_reg(buf, reg);
}

// emit debug instruction
pub fn debug(buf: &mut Buffer) {
    // emit int3 = 0xCC
    emit_op(buf, 0xCC);
}

// emit stub instruction
pub fn trap(buf: &mut Buffer, id: u32) {
    let dest = R10;

    // mov r10, [trap::COMPILER]
    emit_rex(buf, 1, dest.msb(), 0, 0);
    emit_op(buf, 0x8b);
    emit_modrm(buf, 0, dest.and7(), 0b100);
    emit_sib(buf, 0, 0b100, 0b101);
    emit_u32(buf, id);
}

pub fn movl_imm_reg(buf: &mut Buffer, imm: u32, dest: Reg) {
    emit_movl_imm_reg(buf, imm, dest);
}

pub fn negl_reg(buf: &mut Buffer, dest: Reg) {
    emit_negl_reg(buf, dest);
}

pub fn notl_reg(buf: &mut Buffer, dest: Reg) {
    emit_notl_reg(buf, dest);
}

pub fn bool_not_reg(buf: &mut Buffer, dest: Reg) {
    emit_xorb_imm_reg(buf, 1, dest);
    emit_andb_imm_reg(buf, 1, dest);
}

#[cfg(test)]
mod tests {
    use super::*;

    use cpu::trap;
    use jit::buffer::Buffer;

    #[test]
    fn test_debug() {
        let mut buf = Buffer::new();
        debug(&mut buf);

        assert_eq!(vec![0xCC], buf.finish());
    }

    #[test]
    fn test_trap() {
        let mut buf = Buffer::new();
        trap(&mut buf, trap::COMPILER);

        assert_eq!(vec![0x4C, 0x8B, 0x14, 0x25, 7, 0, 0, 0], buf.finish());
    }
}
