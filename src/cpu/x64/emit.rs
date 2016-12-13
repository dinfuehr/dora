use cpu::asm::*;
use cpu::*;
use baseline::buffer::*;
use baseline::codegen::CondCode;
use lexer::position::Position;
use object::IntArray;
use ty::MachineMode;

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

pub fn direct_call(buf: &mut Buffer, ptr: *const u8) {
    let disp = buf.add_addr(ptr);
    let pos = buf.pos() as i32;

    movq_addr_reg(buf, disp + pos, REG_RESULT);
    call(buf, REG_RESULT);
}

pub fn add_imm_reg(buf: &mut Buffer, mode: MachineMode, imm: i32, dest: Reg) {
    match mode {
        MachineMode::Int8 => unimplemented!(),
        MachineMode::Int32 => unimplemented!(),
        MachineMode::Ptr => emit_addq_imm_reg(buf, imm, dest),
    }
}

pub fn nil_ptr_check_bailout(buf: &mut Buffer, pos: Position, reg: Reg) {
    emit_testq_reg_reg(buf, reg, reg);

    let lbl = buf.create_label();
    emit_jcc(buf, CondCode::Zero, lbl);
    buf.emit_bailout(lbl, trap::NIL, pos);
}

pub fn nil_ptr_check(buf: &mut Buffer, reg: Reg) -> Label {
    emit_testq_reg_reg(buf, reg, reg);

    let lbl = buf.create_label();
    emit_jcc(buf, CondCode::Zero, lbl);

    lbl
}

pub fn cmp_setl(buf: &mut Buffer, mode: MachineMode, lhs: Reg, op: CondCode, rhs: Reg, dest: Reg) {
    match mode {
        MachineMode::Int8
            | MachineMode::Int32 => emit_cmpl_reg_reg(buf, rhs, lhs),
        MachineMode::Ptr => emit_cmpq_reg_reg(buf, rhs, lhs),
    }

    emit_setb_reg(buf, op, dest);
    emit_movzbl_reg_reg(buf, dest, dest);
}

pub fn set(buf: &mut Buffer, mode: MachineMode, op: CondCode, dest: Reg) {
    emit_setb_reg(buf, op, dest);

    match mode {
        MachineMode::Int8 => {},
        MachineMode::Int32
            | MachineMode::Ptr => emit_movzbl_reg_reg(buf, dest, dest),
    }
}

pub fn cmp_memindex_reg(buf: &mut Buffer, mode: MachineMode,
                   base: Reg, index: Reg, scale: i32, disp: i32,
                   dest: Reg) {
    emit_cmp_memindex_reg(buf, mode, base, index, scale, disp, dest);
}

pub fn cmp_mem_reg(buf: &mut Buffer, mode: MachineMode,
                        base: Reg, disp: i32, dest: Reg) {
    emit_cmp_mem_reg(buf, mode, base, disp, dest);
}

pub fn cmp_mem_imm(buf: &mut Buffer, mode: MachineMode,
                        base: Reg, disp: i32, imm: i32) {
    emit_cmp_mem_imm(buf, mode, base, disp, imm);
}

pub fn cmp_reg_reg(buf: &mut Buffer, mode: MachineMode, lhs: Reg, rhs: Reg) {
    match mode {
        MachineMode::Int8
            | MachineMode::Int32 => emit_cmpl_reg_reg(buf, rhs, lhs),
        MachineMode::Ptr => emit_cmpq_reg_reg(buf, rhs, lhs),
    }
}

pub fn test_and_jump_if(buf: &mut Buffer, cond: CondCode, reg: Reg, lbl: Label) {
    emit_testl_reg_reg(buf, reg, reg);
    emit_jcc(buf, cond, lbl);
}

pub fn jump_if(buf: &mut Buffer, cond: CondCode, lbl: Label) {
    emit_jcc(buf, cond, lbl);
}

pub fn jump(buf: &mut Buffer, lbl: Label) {
    emit_jmp(buf, lbl);
}

pub fn divl(buf: &mut Buffer, lhs: Reg, rhs: Reg, _: Reg) -> Reg {
    assert_eq!(RAX, lhs);

    emit_cltd(buf);
    emit_idivl_reg_reg(buf, rhs);

    RAX
}

pub fn modl(buf: &mut Buffer, lhs: Reg, rhs: Reg, _: Reg) -> Reg {
    assert_eq!(RAX, lhs);

    emit_cltd(buf);
    emit_idivl_reg_reg(buf, rhs);

    RDX
}

pub fn mull(buf: &mut Buffer, lhs: Reg, rhs: Reg, _: Reg) -> Reg {
    emit_imull_reg_reg(buf, rhs, lhs);

    lhs
}

pub fn addl(buf: &mut Buffer, lhs: Reg, rhs: Reg, _: Reg) -> Reg {
    emit_addl_reg_reg(buf, rhs, lhs);

    lhs
}

pub fn subl(buf: &mut Buffer, lhs: Reg, rhs: Reg, _: Reg) -> Reg {
    emit_subl_reg_reg(buf, rhs, lhs);

    lhs
}

pub fn orl(buf: &mut Buffer, lhs: Reg, rhs: Reg, _: Reg) -> Reg {
    emit_orl_reg_reg(buf, rhs, lhs);

    lhs
}

pub fn andl(buf: &mut Buffer, lhs: Reg, rhs: Reg, _: Reg) -> Reg {
    emit_andl_reg_reg(buf, rhs, lhs);

    lhs
}

pub fn xorl(buf: &mut Buffer, lhs: Reg, rhs: Reg, _: Reg) -> Reg {
    emit_xorl_reg_reg(buf, rhs, lhs);

    lhs
}

pub fn check_index_out_of_bounds(buf: &mut Buffer, pos: Position, array: Reg,
                                 index: Reg, temp: Reg) {
    emit_movq_memq_reg(buf, array, IntArray::offset_of_length(), temp);
    emit_cmpq_reg_reg(buf, temp, index);

    let lbl = buf.create_label();
    emit_jcc(buf, CondCode::UnsignedGreaterEq, lbl);
    buf.emit_bailout(lbl, trap::INDEX_OUT_OF_BOUNDS, pos);
}

pub fn nil(buf: &mut Buffer, dest: Reg) {
    emit_movl_imm_reg(buf, 0, dest);
}

pub fn mov_mem_reg(buf: &mut Buffer, mode: MachineMode, src: Reg, offset: i32, dest: Reg) {
    match mode {
        MachineMode::Int8 => emit_movzbl_memq_reg(buf, src, offset, dest),
        MachineMode::Int32 => emit_movl_memq_reg(buf, src, offset, dest),
        MachineMode::Ptr => emit_movq_memq_reg(buf, src, offset, dest),
    }
}

pub fn mov_array_reg(buf: &mut Buffer, mode: MachineMode, base: Reg,
                     index: Reg, scale: u8, dest: Reg) {
    match mode {
        MachineMode::Int8 => panic!("not supported"),
        MachineMode::Int32 => emit_movl_ar(buf, base, index, scale, dest),
        MachineMode::Ptr => emit_movq_ar(buf, base, index, scale, dest),
    }
}

pub fn mov_reg_array(buf: &mut Buffer, mode: MachineMode, src: Reg, base: Reg,
                     index: Reg, scale: u8) {
    match mode {
        MachineMode::Int8 => panic!("not supported"),
        MachineMode::Int32 => emit_movl_ra(buf, src, base, index, scale),
        MachineMode::Ptr => emit_movq_ra(buf, src, base, index, scale),
    }
}

pub fn mov_reg_mem(buf: &mut Buffer, mode: MachineMode, src: Reg, dest: Reg, offset: i32) {
    match mode {
        MachineMode::Int8 => emit_movb_reg_memq(buf, src, dest, offset),
        MachineMode::Int32 => emit_movl_reg_memq(buf, src, dest, offset),
        MachineMode::Ptr => emit_movq_reg_memq(buf, src, dest, offset),
    }
}

pub fn mov_local_reg(buf: &mut Buffer, mode: MachineMode, offset: i32, dest: Reg) {
    match mode {
        MachineMode::Int8 => emit_movzbl_memq_reg(buf, RBP, offset, dest),
        MachineMode::Int32 => emit_movl_memq_reg(buf, RBP, offset, dest),
        MachineMode::Ptr => emit_movq_memq_reg(buf, RBP, offset, dest),
    }
}

pub fn mov_reg_local(buf: &mut Buffer, mode: MachineMode, src: Reg, offset: i32) {
    match mode {
        MachineMode::Int8 => emit_movb_reg_memq(buf, src, RBP, offset),
        MachineMode::Int32 => emit_movl_reg_memq(buf, src, RBP, offset),
        MachineMode::Ptr => emit_movq_reg_memq(buf, src, RBP, offset),
    }
}

pub fn movl_reg_reg(buf: &mut Buffer, src: Reg, dest: Reg) {
    emit_movl_reg_reg(buf, src, dest);
}

pub fn movp_reg_reg(buf: &mut Buffer, src: Reg, dest: Reg) {
    emit_movq_reg_reg(buf, src, dest);
}

pub fn mov_reg_reg(buf: &mut Buffer, mode: MachineMode, src: Reg, dest: Reg) {
    match mode {
        MachineMode::Int8 | MachineMode::Int32 => emit_movl_reg_reg(buf, src, dest),
        MachineMode::Ptr => emit_movq_reg_reg(buf, src, dest),
    }
}

pub fn reserve_stack(buf: &mut Buffer, imm: i32) {
    emit_subq_imm_reg(buf, imm, RSP);
}

pub fn free_stack(buf: &mut Buffer, imm: i32) {
    emit_addq_imm_reg(buf, imm, RSP);
}

pub fn addq_reg_reg(buf: &mut Buffer, src: Reg, dest: Reg) {
    emit_addq_reg_reg(buf, src, dest);
}

pub fn shll_reg_cl(buf: &mut Buffer, dest: Reg) {
    emit_shll_reg_cl(buf, dest);
}

pub fn shiftlq_imm_reg(buf: &mut Buffer, imm: u8, dest: Reg) {
    emit_shlq_reg(buf, imm, dest);
}

pub fn shiftll_imm_reg(buf: &mut Buffer, imm: u8, dest: Reg) {
    emit_shll_reg(buf, imm, dest);
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

    use baseline::buffer::Buffer;
    use cpu::trap;

    #[test]
    fn test_debug() {
        let mut buf = Buffer::new();
        debug(&mut buf);

        assert_eq!(vec![0xCC], buf.data());
    }

    #[test]
    fn test_trap() {
        let mut buf = Buffer::new();
        trap::emit(&mut buf, trap::COMPILER);

        assert_eq!(vec![0x4C, 0x8B, 0x14, 0x25, 7, 0, 0, 0], buf.data());
    }
}
