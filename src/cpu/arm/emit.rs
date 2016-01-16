pub fn prolog(buf: &mut Buffer, stacksize: i32) {
    // TODO
}

pub fn epilog(buf: &mut Buffer, stacksize: i32) {
    // TODO
}

pub fn cmpl_setl(buf: &mut Buffer, lhs: Reg, op: CmpOp, rhs: Reg, dest: Reg) {
    // TODO
}

pub fn jump_if(buf: &mut Buffer, cond: JumpCond, reg: Reg, lbl: Label) {
    // TODO
}

pub fn jump(buf: &mut Buffer, lbl: Label) {
    // TODO
}

pub fn divl(buf: &mut Buffer, lhs: Reg, rhs: Reg, dest: Reg) -> Reg {
    // TODO
}

pub fn modl(buf: &mut Buffer, lhs: Reg, rhs: Reg, dest: Reg) -> Reg {
    // TODO
}

pub fn mull(buf: &mut Buffer, lhs: Reg, rhs: Reg, dest: Reg) -> Reg {
    // TODO
}

pub fn addl(buf: &mut Buffer, lhs: Reg, rhs: Reg, dest: Reg) -> Reg {
    emit_add(buf, dest, lhs, rhs);

    dest
}

pub fn subl(buf: &mut Buffer, lhs: Reg, rhs: Reg, dest: Reg) -> Reg {
    emit_sub(buf, dest, lhs, rhs);

    dest
}

pub fn orl(buf: &mut Buffer, lhs: Reg, rhs: Reg, dest: Reg) -> Reg {
    emit_or(buf, dest, lhs, rhs);

    dest
}

pub fn andl(buf: &mut Buffer, lhs: Reg, rhs: Reg, dest: Reg) -> Reg {
    emit_and(buf, dest, lhs, rhs);

    dest
}

pub fn xorl(buf: &mut Buffer, lhs: Reg, rhs: Reg, dest: Reg) -> Reg {
    emit_xor(buf, dest, lhs, rhs);

    dest
}

pub fn mov_local_reg(buf: &mut Buffer, ty: BuiltinType, offset: i32, dest: Reg) {
    // TODO
}

pub fn mov_reg_local(buf: &mut Buffer, ty: BuiltinType, src: Reg, offset: i32) {
    // TODO
}

pub fn movl_reg_reg(buf: &mut Buffer, src: Reg, dest: Reg) {
    // TODO
}

pub fn call(buf: &mut Buffer, disp: i32) {
    // TODO
}

pub fn push_param(buf: &mut Buffer, reg: Reg) {
    emit_pushq_reg(buf, reg);
}

pub fn debug(buf: &mut Buffer) {
    // TODO
}

pub fn stub(buf: &mut Buffer) {
    // TODO
}

pub fn movl_imm_reg(buf: &mut Buffer, imm: u32, dest: Reg) {
    // TODO
}

pub fn negl_reg(buf: &mut Buffer, dest: Reg) {
    // TODO
}

pub fn notl_reg(buf: &mut Buffer, reg: Reg) {
    emit_not(buf, reg, reg);

    dest
}

pub fn bool_not_reg(buf: &mut Buffer, dest: Reg) {
    // TODO
}
