use cpu::asm::*;
use cpu::*;
use baseline::buffer::*;
use baseline::codegen::CondCode;
use lexer::position::Position;
use mem::ptr_width;
use object::IntArray;
use ty::MachineMode;
use vtable::VTable;

pub fn prolog(buf: &mut MacroAssembler, stacksize: i32) {
    emit_pushq_reg(buf, RBP);
    emit_movq_reg_reg(buf, RSP, RBP);

    if stacksize > 0 {
        emit_subq_imm_reg(buf, stacksize, RSP);
    }
}

pub fn epilog(buf: &mut MacroAssembler, stacksize: i32) {
    if stacksize > 0 {
        emit_addq_imm_reg(buf, stacksize, RSP);
    }

    emit_popq_reg(buf, RBP);
    emit_retq(buf);
}

pub fn direct_call(buf: &mut MacroAssembler, ptr: *const u8) {
    let disp = buf.add_addr(ptr);
    let pos = buf.pos() as i32;

    load_constpool(buf, REG_RESULT, disp + pos);
    call_reg(buf, REG_RESULT);
}

pub fn indirect_call(buf: &mut MacroAssembler, index: u32) {
    let obj = REG_PARAMS[0];

    // REG_RESULT = [obj] (load vtable)
    load_mem(buf, MachineMode::Ptr, REG_RESULT, Mem::Base(obj, 0));

    // calculate offset of VTable entry
    let disp = VTable::offset_of_method_table() + (index as i32) * ptr_width();

    // load vtable entry
    load_mem(buf, MachineMode::Ptr, REG_RESULT, Mem::Base(REG_RESULT, disp));

    // call *REG_RESULT
    call_reg(buf, REG_RESULT);
}

pub fn load_array_elem(buf: &mut MacroAssembler, mode: MachineMode, dest: Reg, array: Reg, index: Reg) {
    assert!(mode == MachineMode::Int32);

    load_mem(buf, mode, dest, Mem::Index(array, index, mode.size(), IntArray::offset_of_data()));
}

pub fn store_array_elem(buf: &mut MacroAssembler, mode: MachineMode, array: Reg, index: Reg, value: Reg) {
    assert!(mode == MachineMode::Int32);

    store_mem(buf, MachineMode::Int32,
              Mem::Index(array, index, 4, IntArray::offset_of_data()), value);
}

pub fn test_if_nil_bailout(buf: &mut MacroAssembler, pos: Position, reg: Reg) {
    emit_testq_reg_reg(buf, reg, reg);

    let lbl = buf.create_label();
    jump_if(buf, CondCode::Zero, lbl);
    buf.emit_bailout(lbl, trap::NIL, pos);
}

pub fn test_if_nil(buf: &mut MacroAssembler, reg: Reg) -> Label {
    emit_testq_reg_reg(buf, reg, reg);

    let lbl = buf.create_label();
    jump_if(buf, CondCode::Zero, lbl);

    lbl
}

pub fn set(buf: &mut MacroAssembler, dest: Reg, op: CondCode) {
    emit_setb_reg(buf, op, dest);
    emit_movzbl_reg_reg(buf, dest, dest);
}

pub fn cmp_mem(buf: &mut MacroAssembler, mode: MachineMode, mem: Mem, rhs: Reg) {
    match mem {
        Mem::Local(offset) => emit_cmp_mem_reg(buf, mode, REG_FP, offset, rhs),
        Mem::Base(base, disp) => emit_cmp_mem_reg(buf, mode, base, disp, rhs),
        Mem::Index(base, index, scale, disp) =>
            emit_cmp_memindex_reg(buf, mode, base, index, scale, disp, rhs)
    }
}

pub fn cmp_mem_imm(buf: &mut MacroAssembler, mode: MachineMode, mem: Mem, imm: i32) {
    match mem {
        Mem::Local(_) => unimplemented!(),
        Mem::Base(base, disp) => emit_cmp_mem_imm(buf, mode, base, disp, imm),
        Mem::Index(_, _, _, _) => unimplemented!(),
    }
}

pub fn cmp_reg(buf: &mut MacroAssembler, mode: MachineMode, lhs: Reg, rhs: Reg) {
    match mode {
        MachineMode::Int8
            | MachineMode::Int32 => emit_cmpl_reg_reg(buf, rhs, lhs),
        MachineMode::Ptr => emit_cmpq_reg_reg(buf, rhs, lhs),
    }
}

pub fn test_and_jump_if(buf: &mut MacroAssembler, cond: CondCode, reg: Reg, lbl: Label) {
    assert!(cond == CondCode::Zero || cond == CondCode::NonZero);

    emit_testl_reg_reg(buf, reg, reg);
    emit_jcc(buf, cond, lbl);
}

pub fn jump_if(buf: &mut MacroAssembler, cond: CondCode, lbl: Label) {
    emit_jcc(buf, cond, lbl);
}

pub fn jump(buf: &mut MacroAssembler, lbl: Label) {
    emit_jmp(buf, lbl);
}

pub fn int_div(buf: &mut MacroAssembler, dest: Reg, lhs: Reg, rhs: Reg) {
    assert_eq!(RAX, lhs);

    emit_cltd(buf);
    emit_idivl_reg_reg(buf, rhs);

    if dest != RAX {
        emit_movl_reg_reg(buf, RAX, dest);
    }
}

pub fn int_mod(buf: &mut MacroAssembler, dest: Reg, lhs: Reg, rhs: Reg) {
    assert_eq!(RAX, lhs);

    emit_cltd(buf);
    emit_idivl_reg_reg(buf, rhs);

    if dest != RDX {
        emit_movl_reg_reg(buf, RDX, dest);
    }
}

pub fn int_mul(buf: &mut MacroAssembler, dest: Reg, lhs: Reg, rhs: Reg) {
    emit_imull_reg_reg(buf, rhs, lhs);

    if dest != lhs {
        emit_movl_reg_reg(buf, lhs, dest);
    }
}

pub fn int_add(buf: &mut MacroAssembler, dest: Reg, lhs: Reg, rhs: Reg) {
    emit_addl_reg_reg(buf, rhs, lhs);

    if dest != lhs {
        emit_movl_reg_reg(buf, lhs, dest);
    }
}

pub fn int_sub(buf: &mut MacroAssembler, dest: Reg, lhs: Reg, rhs: Reg) {
    emit_subl_reg_reg(buf, rhs, lhs);

    if dest != lhs {
        emit_movl_reg_reg(buf, lhs, dest);
    }
}

pub fn int_shl(buf: &mut MacroAssembler, dest: Reg, lhs: Reg, rhs: Reg) {
    if rhs != RCX {
        assert!(lhs != RCX);
        emit_movq_reg_reg(buf, rhs, RCX);
    }

    emit_shll_reg_cl(buf, lhs);

    if dest != lhs {
        emit_movl_reg_reg(buf, lhs, dest);
    }
}

pub fn int_or(buf: &mut MacroAssembler, dest: Reg, lhs: Reg, rhs: Reg) {
    emit_orl_reg_reg(buf, rhs, lhs);

    if dest != lhs {
        emit_movl_reg_reg(buf, lhs, dest);
    }
}

pub fn int_and(buf: &mut MacroAssembler, dest: Reg, lhs: Reg, rhs: Reg) {
    emit_andl_reg_reg(buf, rhs, lhs);

    if dest != lhs {
        emit_movl_reg_reg(buf, lhs, dest);
    }
}

pub fn int_xor(buf: &mut MacroAssembler, dest: Reg, lhs: Reg, rhs: Reg) {
    emit_xorl_reg_reg(buf, rhs, lhs);

    if dest != lhs {
        emit_movl_reg_reg(buf, lhs, dest);
    }
}

pub fn check_index_out_of_bounds(buf: &mut MacroAssembler, pos: Position, array: Reg,
                                 index: Reg, temp: Reg) {
    load_mem(buf, MachineMode::Int32, temp,
             Mem::Base(array, IntArray::offset_of_length()));
    cmp_reg(buf, MachineMode::Int32, index, temp);

    let lbl = buf.create_label();
    jump_if(buf, CondCode::UnsignedGreaterEq, lbl);
    buf.emit_bailout(lbl, trap::INDEX_OUT_OF_BOUNDS, pos);
}

pub fn load_nil(buf: &mut MacroAssembler, dest: Reg) {
    emit_movl_imm_reg(buf, 0, dest);
}

pub fn load_mem(buf: &mut MacroAssembler, mode: MachineMode, dest: Reg, mem: Mem) {
    match mem {
        Mem::Local(offset) => {
            match mode {
                MachineMode::Int8 => emit_movzbl_memq_reg(buf, RBP, offset, dest),
                MachineMode::Int32 => emit_movl_memq_reg(buf, RBP, offset, dest),
                MachineMode::Ptr => emit_movq_memq_reg(buf, RBP, offset, dest),
            }
        }

        Mem::Base(base, disp) => {
            match mode {
                MachineMode::Int8 => emit_movzbl_memq_reg(buf, base, disp, dest),
                MachineMode::Int32 => emit_movl_memq_reg(buf, base, disp, dest),
                MachineMode::Ptr => emit_movq_memq_reg(buf, base, disp, dest),
            }
        }

        Mem::Index(base, index, scale, disp) =>
            emit_mov_memindex_reg(buf, mode, base, index, scale, disp, dest)
    }
}

pub fn store_mem(buf: &mut MacroAssembler, mode: MachineMode, mem: Mem, src: Reg) {
    match mem {
        Mem::Local(offset) => {
            match mode {
                MachineMode::Int8 => emit_movb_reg_memq(buf, src, RBP, offset),
                MachineMode::Int32 => emit_movl_reg_memq(buf, src, RBP, offset),
                MachineMode::Ptr => emit_movq_reg_memq(buf, src, RBP, offset),
            }
        }

        Mem::Base(base, disp) => {
            match mode {
                MachineMode::Int8 => emit_movb_reg_memq(buf, src, base, disp),
                MachineMode::Int32 => emit_movl_reg_memq(buf, src, base, disp),
                MachineMode::Ptr => emit_movq_reg_memq(buf, src, base, disp),
            }
        }

        Mem::Index(base, index, scale, disp) =>
            emit_mov_reg_memindex(buf, mode, src, base, index, scale, disp)
    }
}

pub fn copy_reg(buf: &mut MacroAssembler, mode: MachineMode, dest: Reg, src: Reg) {
    match mode {
        MachineMode::Int8 | MachineMode::Int32 => emit_movl_reg_reg(buf, src, dest),
        MachineMode::Ptr => emit_movq_reg_reg(buf, src, dest),
    }
}

pub fn load_constpool(buf: &mut MacroAssembler, dest: Reg, disp: i32) {
    // next instruction has 7 bytes
    let disp = -(disp + 7);

    emit_movq_memq_reg(buf, RIP, disp, dest); // 7 bytes
}

pub fn call_reg(buf: &mut MacroAssembler, reg: Reg) {
    emit_callq_reg(buf, reg);
}

// emit debug instruction
pub fn debug(buf: &mut MacroAssembler) {
    // emit int3 = 0xCC
    emit_op(buf, 0xCC);
}

pub fn load_int_const(buf: &mut MacroAssembler, mode: MachineMode, dest: Reg, imm: i32) {
    match mode {
        MachineMode::Int8 => unimplemented!(),
        MachineMode::Int32 => emit_movl_imm_reg(buf, imm, dest),
        MachineMode::Ptr => emit_movq_imm_reg(buf, imm, dest),
    }
}

pub fn load_true(buf: &mut MacroAssembler, dest: Reg) {
    emit_movl_imm_reg(buf, 1, dest);
}

pub fn load_false(buf: &mut MacroAssembler, dest: Reg) {
    emit_movl_imm_reg(buf, 0, dest);
}

pub fn int_neg(buf: &mut MacroAssembler, dest: Reg, src: Reg) {
    emit_negl_reg(buf, src);

    if dest != src {
        emit_movl_reg_reg(buf, src, dest);
    }
}

pub fn int_not(buf: &mut MacroAssembler, dest: Reg, src: Reg) {
    emit_notl_reg(buf, src);

    if dest != src {
        emit_movl_reg_reg(buf, src, dest);
    }
}

pub fn bool_not(buf: &mut MacroAssembler, dest: Reg, src: Reg) {
    emit_xorb_imm_reg(buf, 1, src);
    emit_andb_imm_reg(buf, 1, src);

    if dest != src {
        emit_movl_reg_reg(buf, src, dest);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use baseline::buffer::MacroAssembler;
    use cpu::trap;

    #[test]
    fn test_debug() {
        let mut buf = MacroAssembler::new();
        debug(&mut buf);

        assert_eq!(vec![0xCC], buf.data());
    }

    #[test]
    fn test_trap() {
        let mut buf = MacroAssembler::new();
        trap::emit(&mut buf, trap::COMPILER);

        assert_eq!(vec![0x4C, 0x8B, 0x14, 0x25, 7, 0, 0, 0], buf.data());
    }
}
