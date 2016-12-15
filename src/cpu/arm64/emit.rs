use cpu::arm64::asm::*;
use cpu::arm64::*;
use cpu::Reg;
use cpu::arm64::reg::*;
use baseline::buffer::*;

pub fn prolog(buf: &mut Buffer, stacksize: i32) {
    buf.emit_u32(stp_pre(1, REG_FP, REG_LR, REG_SP, -2));
    buf.emit_u32(add_reg(1, REG_FP, REG_SP, REG_ZERO));

    if stacksize > 0 {
        let scratch = get_scratch();
        load_int_const(buf, scratch, stacksize);
        buf.emit_u32(sub_reg(1, REG_SP, REG_SP, scratch));
    }
}

pub fn epilog(buf: &mut Buffer, stacksize: i32) {
    if stacksize > 0 {
        let scratch = get_scratch();
        load_int_const(buf, scratch, stacksize);
        buf.emit_u32(add_reg(1, REG_SP, REG_SP, scratch));
    }

    buf.emit_u32(add_reg(1, REG_SP, REG_FP, REG_ZERO));
    buf.emit_u32(ldp_post(1, REG_FP, REG_LR, REG_SP, 2));
    buf.emit_u32(ret());
}

pub fn direct_call(buf: &mut Buffer, ptr: *const u8) {
    let disp = buf.add_addr(ptr);
    let pos = buf.pos() as i32;

    let scratch = get_scratch();
    // TODO: load from const pool
    unimplemented!();
    br(scratch);
}

pub fn indirect_call(buf: &mut Buffer, index: u32) {
    unimplemented!();
}

pub fn load_int_const(buf: &mut Buffer, dest: Reg, imm: i32) {
    let register_size = 32;
    let imm = imm as i64 as u64;

    if fits_movz(imm, register_size) {
        buf.emit_u32(movz(0, dest, imm as u32, 0));

    } else if fits_movn(imm, register_size) {
        buf.emit_u32(movn(0, dest, imm as u32, 0));

    } else {
        unimplemented!();
    }
}

pub fn load_true(buf: &mut Buffer, dest: Reg) {
    buf.emit_u32(movz(0, dest, 1, 0));
}

pub fn load_false(buf: &mut Buffer, dest: Reg) {
    buf.emit_u32(movz(0, dest, 0, 0));
}

pub fn int_neg(buf: &mut Buffer, dest: Reg, src: Reg) {
    buf.emit_u32(sub_reg(0, dest, REG_ZERO, src));
}

pub fn int_not(buf: &mut Buffer, dest: Reg, src: Reg) {
    buf.emit_u32(orn_shreg(0, dest, REG_ZERO, src, Shift::LSL, 0));
}

pub fn bool_not(buf: &mut Buffer, dest: Reg, src: Reg) {
    let scratch = get_scratch();

    buf.emit_u32(movz(0, scratch, 1, 0));
    buf.emit_u32(eor_shreg(0, dest, src, scratch, Shift::LSL, 0));
    buf.emit_u32(uxtb(dest, dest));
}

fn get_scratch() -> Reg {
    SCRATCH[0]
}
