use cpu::Reg;
use cpu::instr::*;
use cpu::Reg::*;
use ctxt::*;
use jit::buffer::*;
use sym::BuiltinType;

pub fn prolog(buf: &mut Buffer, stacksize: i32) {
    emit_pushq_reg(buf, Reg::RBP);
    emit_movq_reg_reg(buf, Reg::RSP, Reg::RBP);

    if stacksize > 0 {
        emit_subq_imm_reg(buf, stacksize, RSP);
    }
}

pub fn epilog(buf: &mut Buffer, stacksize: i32) {
    if stacksize > 0 {
        emit_addq_imm_reg(buf, stacksize, RSP);
    }

    emit_popq_reg(buf, Reg::RBP);
    emit_retq(buf);
}

// emit debug instruction
pub fn debug(buf: &mut Buffer) {
    // emit int3 = 0xCC
    buf.emit_u8(0xCC);
}

pub fn var_store(buf: &mut Buffer, ctxt: &Context, src: Reg, var: VarInfoId) {
    let var_infos = ctxt.var_infos.borrow();
    let var = &var_infos[var.0];

    match var.data_type {
        BuiltinType::Bool => emit_movb_reg_memq(buf, src, RBP, var.offset),
        BuiltinType::Int => emit_movl_reg_memq(buf, src, RBP, var.offset),
        BuiltinType::Str => emit_movq_reg_memq(buf, src, RBP, var.offset),
        BuiltinType::Unit => {},
    }
}

pub fn var_load(buf: &mut Buffer, ctxt: &Context, var: VarInfoId, dest: Reg) {
    let var_infos = ctxt.var_infos.borrow();
    let var = &var_infos[var.0];

    match var.data_type {
        BuiltinType::Bool => emit_movb_memq_reg(buf, RBP, var.offset, dest),
        BuiltinType::Int => emit_movl_memq_reg(buf, RBP, var.offset, dest),
        BuiltinType::Str => emit_movq_memq_reg(buf, RBP, var.offset, dest),
        BuiltinType::Unit => {},
    }
}
