use ast::CmpOp;
use cpu::instr::*;
use cpu::*;
use ctxt::*;
use jit::buffer::*;
use jit::codegen::JumpCond;
use ty::BuiltinType;

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

pub fn cmp_setl(buf: &mut Buffer, ty: BuiltinType, lhs: Reg, op: CmpOp, rhs: Reg, dest: Reg) {
    match ty {
        BuiltinType::Bool | BuiltinType::Int => emit_cmpl_reg_reg(buf, rhs, lhs),
        BuiltinType::Str | BuiltinType::Class(_) => emit_cmpq_reg_reg(buf, rhs, lhs),
        BuiltinType::Unit => unreachable!(),
    }

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
        BuiltinType::Str | BuiltinType::Class(_) => emit_movq_memq_reg(buf, RBP, offset, dest),
        BuiltinType::Unit => {},
    }
}

pub fn mov_reg_local(buf: &mut Buffer, ty: BuiltinType, src: Reg, offset: i32) {
    match ty {
        BuiltinType::Bool => emit_movb_reg_memq(buf, src, RBP, offset),
        BuiltinType::Int => emit_movl_reg_memq(buf, src, RBP, offset),
        BuiltinType::Str | BuiltinType::Class(_) => emit_movq_reg_memq(buf, src, RBP, offset),
        BuiltinType::Unit => {},
    }
}

pub fn movl_reg_reg(buf: &mut Buffer, src: Reg, dest: Reg) {
    emit_movl_reg_reg(buf, src, dest);
}

pub fn mov_reg_reg(buf: &mut Buffer, ty: BuiltinType, src: Reg, dest: Reg) {
    match ty {
        BuiltinType::Unit => unreachable!(),
        BuiltinType::Int | BuiltinType::Bool => emit_movl_reg_reg(buf, src, dest),
        BuiltinType::Str | BuiltinType::Class(_) => emit_movq_reg_reg(buf, src, dest),
    }
}

pub fn movq_addr_reg(buf: &mut Buffer, disp: i32, dest: Reg) {
    // next instruction has 7 bytes
    let disp = -(disp + 7);

    emit_movq_memq_reg(buf, RIP, disp, dest); // 7 bytes
}

pub fn call(buf: &mut Buffer, reg: Reg) {
    emit_callq_reg(buf, reg);
}

pub fn push_param(buf: &mut Buffer, reg: Reg) {
    emit_pushq_reg(buf, reg);
}

// emit debug instruction
pub fn debug(buf: &mut Buffer) {
    // emit int3 = 0xCC
    emit_op(buf, 0xCC);
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
        trap::emit(&mut buf, trap::COMPILER);

        assert_eq!(vec![0x4C, 0x8B, 0x14, 0x25, 7, 0, 0, 0], buf.finish());
    }
}
