use crate::cpu::*;
use crate::masm::{CondCode, Label, MacroAssembler};
use crate::ty::MachineMode;

pub const VEXM_0F: u8 = 1;
pub const VEXM_0F38: u8 = 2;
pub const VEXM_0F3A: u8 = 3;

pub const VEXL_Z: u8 = 0;
pub const VEXL_128: u8 = 0;
pub const VEXL_256: u8 = 1;

pub const VEXP_NONE: u8 = 0;
pub const VEXP_66: u8 = 1;
pub const VEXP_F3: u8 = 2;
pub const VEXP_F2: u8 = 3;

fn emit_membase(buf: &mut MacroAssembler, base: Reg, disp: i32, dest: Reg) {
    if base == RSP || base == R12 {
        if disp == 0 {
            emit_modrm(buf, 0, dest.and7(), RSP.and7());
            emit_sib(buf, 0, RSP.and7(), RSP.and7());
        } else if fits_i8(disp) {
            emit_modrm(buf, 1, dest.and7(), RSP.and7());
            emit_sib(buf, 0, RSP.and7(), RSP.and7());
            emit_u8(buf, disp as u8);
        } else {
            emit_modrm(buf, 2, dest.and7(), RSP.and7());
            emit_sib(buf, 0, RSP.and7(), RSP.and7());
            emit_u32(buf, disp as u32);
        }
    } else if disp == 0 && base != RBP && base != R13 && base != RIP {
        emit_modrm(buf, 0, dest.and7(), base.and7());
    } else if base == RIP {
        emit_modrm(buf, 0, dest.and7(), RBP.and7());
        emit_u32(buf, disp as u32);
    } else if fits_i8(disp) {
        emit_modrm(buf, 1, dest.and7(), base.and7());
        emit_u8(buf, disp as u8);
    } else {
        emit_modrm(buf, 2, dest.and7(), base.and7());
        emit_u32(buf, disp as u32);
    }
}

pub fn emit_u64(buf: &mut MacroAssembler, val: u64) {
    buf.emit_u64(val)
}

pub fn emit_u32(buf: &mut MacroAssembler, val: u32) {
    buf.emit_u32(val)
}

pub fn emit_u8(buf: &mut MacroAssembler, val: u8) {
    buf.emit_u8(val)
}

pub fn emit_op(buf: &mut MacroAssembler, opcode: u8) {
    buf.emit_u8(opcode);
}

pub fn emit_vex3_rxbm(buf: &mut MacroAssembler, r: u8, x: u8, b: u8, m: u8) {
    assert!(r == 0 || r == 1);
    assert!(x == 0 || x == 1);
    assert!(b == 0 || b == 1);
    assert!(m > 1 && m < 4);

    buf.emit_u8(0b11000100);
    buf.emit_u8(!r << 7 | (!x << 6) & 0b1000000 | (!b << 5) & 0b100000 | m);
}

pub fn emit_vex3_wvlp(buf: &mut MacroAssembler, w: bool, v: u8, l: u8, p: u8) {
    assert!(v < 16);
    assert!(l == 0 || l == 1);
    assert!(p < 4);

    buf.emit_u8((w as u8) << 7 | (!v << 3) & 0b1111000 | l << 2 | p);
}

pub fn emit_vex2(buf: &mut MacroAssembler, r: u8, v: u8, l: u8, p: u8) {
    assert!(r == 0 || r == 1);
    assert!(v < 16);
    assert!(l == 0 || l == 1);
    assert!(p < 4);

    buf.emit_u8(0b11000101);
    buf.emit_u8(!r << 7 | (!v << 3) & 0b1111000 | l << 2 | p);
}

pub fn emit_rex(buf: &mut MacroAssembler, w: bool, r: u8, x: u8, b: u8) {
    assert!(r == 0 || r == 1);
    assert!(x == 0 || x == 1);
    assert!(b == 0 || b == 1);

    buf.emit_u8(0x40 | (w as u8) << 3 | r << 2 | x << 1 | b);
}

pub fn emit_modrm(buf: &mut MacroAssembler, mode: u8, reg: u8, rm: u8) {
    assert!(mode < 4);
    assert!(reg < 8);
    assert!(rm < 8);

    buf.emit_u8(mode << 6 | reg << 3 | rm);
}

pub fn emit_sib(buf: &mut MacroAssembler, scale: u8, index: u8, base: u8) {
    assert!(scale < 4);
    assert!(index < 8);
    assert!(base < 8);

    buf.emit_u8(scale << 6 | index << 3 | base);
}

pub fn fits_i8(imm: i32) -> bool {
    imm == (imm as i8) as i32
}

pub fn emit_jcc(buf: &mut MacroAssembler, cond: CondCode, lbl: Label) {
    let opcode = match cond {
        CondCode::Zero | CondCode::Equal => 0x84,
        CondCode::NonZero | CondCode::NotEqual => 0x85,
        CondCode::Greater => 0x8F,
        CondCode::GreaterEq => 0x8D,
        CondCode::Less => 0x8C,
        CondCode::LessEq => 0x8E,
        CondCode::UnsignedGreater => 0x87,   // above
        CondCode::UnsignedGreaterEq => 0x83, // above or equal
        CondCode::UnsignedLess => 0x82,      // below
        CondCode::UnsignedLessEq => 0x86,    // below or equal
    };

    emit_op(buf, 0x0f);
    emit_op(buf, opcode);
    buf.emit_label(lbl);
}

pub fn emit_movsx(buf: &mut MacroAssembler, src: Reg, dest: Reg) {
    emit_rex(buf, true, dest.msb(), 0, src.msb());

    emit_op(buf, 0x63);
    emit_modrm(buf, 0b11, dest.and7(), src.and7());
}

pub fn emit_jmp(buf: &mut MacroAssembler, lbl: Label) {
    emit_op(buf, 0xe9);
    buf.emit_label(lbl);
}

pub fn emit_jmp_reg(buf: &mut MacroAssembler, reg: Reg) {
    if reg.msb() != 0 {
        emit_rex(buf, false, 0, 0, reg.msb());
    }
    emit_op(buf, 0xFF);
    emit_modrm(buf, 0b11, 0b100, reg.and7());
}

pub fn lea(buf: &mut MacroAssembler, dest: Reg, src: Mem) {
    emit_rex_mem(buf, true, dest, &src);
    emit_op(buf, 0x8D);
    emit_mem(buf, dest, &src);
}

fn emit_rex_mem(buf: &mut MacroAssembler, x64: bool, dest: Reg, src: &Mem) {
    let (base_msb, index_msb) = match src {
        &Mem::Local(_) => (RBP.msb(), 0),
        &Mem::Base(base, _) => {
            let base_msb = if base == RIP { 0 } else { base.msb() };

            (base_msb, 0)
        }

        &Mem::Index(base, index, _, _) => (base.msb(), index.msb()),
        &Mem::Offset(index, _, _) => (0, index.msb()),
    };

    if dest.msb() != 0 || index_msb != 0 || base_msb != 0 || x64 {
        emit_rex(buf, x64, dest.msb(), index_msb, base_msb);
    }
}

fn emit_mem(buf: &mut MacroAssembler, dest: Reg, src: &Mem) {
    match src {
        &Mem::Local(offset) => {
            emit_membase(buf, RBP, offset, dest);
        }

        &Mem::Base(base, disp) => {
            emit_membase(buf, base, disp, dest);
        }

        &Mem::Index(base, index, scale, disp) => {
            emit_membase_with_index_and_scale(buf, base, index, scale, disp, dest);
        }

        &Mem::Offset(index, scale, disp) => {
            emit_membase_without_base(buf, index, scale, disp, dest);
        }
    }
}

pub fn emit_cmp_mem_reg(
    buf: &mut MacroAssembler,
    mode: MachineMode,
    base: Reg,
    disp: i32,
    dest: Reg,
) {
    let base_msb = if base == RIP { 0 } else { base.msb() };

    let (x64, opcode) = match mode {
        MachineMode::Int8 => (false, 0x38),
        MachineMode::Int32 => (false, 0x39),
        MachineMode::Int64 => unimplemented!(),
        MachineMode::Float32 | MachineMode::Float64 => unreachable!(),
        MachineMode::Ptr => (true, 0x39),
    };

    if x64 || dest.msb() != 0 || base_msb != 0 {
        emit_rex(buf, x64, dest.msb(), 0, base_msb);
    }

    emit_op(buf, opcode);
    emit_membase(buf, base, disp, dest);
}

pub fn emit_cmp_mem_imm(
    buf: &mut MacroAssembler,
    mode: MachineMode,
    base: Reg,
    disp: i32,
    imm: i32,
) {
    let base_msb = if base == RIP { 0 } else { base.msb() };

    let opcode = if fits_i8(imm) { 0x83 } else { 0x81 };

    let (x64, opcode) = match mode {
        MachineMode::Int8 => (false, 0x80),
        MachineMode::Int32 => (false, opcode),
        MachineMode::Int64 => unimplemented!(),
        MachineMode::Ptr => (true, opcode),
        MachineMode::Float32 | MachineMode::Float64 => unreachable!(),
    };

    if x64 || base_msb != 0 {
        emit_rex(buf, x64, 0, 0, base_msb);
    }

    emit_op(buf, opcode);
    emit_membase(buf, base, disp, RDI);

    if fits_i8(imm) {
        emit_u8(buf, imm as u8);
    } else {
        if mode == MachineMode::Int8 {
            panic!("Int8 does not support 32 bit values");
        }

        emit_u32(buf, imm as u32);
    }
}

pub fn emit_cmp_memindex_reg(
    buf: &mut MacroAssembler,
    mode: MachineMode,
    base: Reg,
    index: Reg,
    scale: i32,
    disp: i32,
    dest: Reg,
) {
    assert!(scale == 8 || scale == 4 || scale == 2 || scale == 1);

    let (x64, opcode) = match mode {
        MachineMode::Int8 => (false, 0x38),
        MachineMode::Int32 => (false, 0x39),
        MachineMode::Int64 => unimplemented!(),
        MachineMode::Ptr => (true, 0x39),
        MachineMode::Float32 | MachineMode::Float64 => unreachable!(),
    };

    if x64 || dest.msb() != 0 || index.msb() != 0 || base.msb() != 0 {
        emit_rex(buf, x64, dest.msb(), index.msb(), base.msb());
    }

    emit_op(buf, opcode);
    emit_membase_with_index_and_scale(buf, base, index, scale, disp, dest);
}

fn emit_membase_without_base(
    buf: &mut MacroAssembler,
    index: Reg,
    scale: i32,
    disp: i32,
    dest: Reg,
) {
    assert!(scale == 8 || scale == 4 || scale == 2 || scale == 1);

    let scale = match scale {
        8 => 3,
        4 => 2,
        2 => 1,
        _ => 0,
    };

    emit_modrm(buf, 0, dest.and7(), 4);
    emit_sib(buf, scale, index.and7(), 5);
    emit_u32(buf, disp as u32);
}

fn emit_membase_with_index_and_scale(
    buf: &mut MacroAssembler,
    base: Reg,
    index: Reg,
    scale: i32,
    disp: i32,
    dest: Reg,
) {
    assert!(scale == 8 || scale == 4 || scale == 2 || scale == 1);

    let scale = match scale {
        8 => 3,
        4 => 2,
        2 => 1,
        _ => 0,
    };

    if disp == 0 {
        emit_modrm(buf, 0, dest.and7(), 4);
        emit_sib(buf, scale, index.and7(), base.and7());
    } else if fits_i8(disp) {
        emit_modrm(buf, 1, dest.and7(), 4);
        emit_sib(buf, scale, index.and7(), base.and7());
        emit_u8(buf, disp as u8);
    } else {
        emit_modrm(buf, 2, dest.and7(), 4);
        emit_sib(buf, scale, index.and7(), base.and7());
        emit_u32(buf, disp as u32);
    }
}

pub fn emit_shlx(buf: &mut MacroAssembler, x64: bool, dest: Reg, lhs: Reg, rhs: Reg) {
    emit_vex3_rxbm(buf, dest.msb(), 0, lhs.msb(), VEXM_0F38);
    emit_vex3_wvlp(buf, x64, rhs.int(), VEXL_Z, VEXP_66);
    emit_u8(buf, 0xF7);
    emit_modrm(buf, 0b11, dest.and7(), lhs.and7());
}

pub fn emit_shlq_reg(buf: &mut MacroAssembler, imm: u8, dest: Reg) {
    emit_rex(buf, true, 0, 0, dest.msb());
    emit_op(buf, 0xC1);
    emit_modrm(buf, 0b11, 0b100, dest.and7());
    emit_u8(buf, imm);
}

pub fn emit_shll_reg(buf: &mut MacroAssembler, imm: u8, dest: Reg) {
    if dest.msb() != 0 {
        emit_rex(buf, false, 0, 0, dest.msb());
    }

    emit_op(buf, 0xC1);
    emit_modrm(buf, 0b11, 0b100, dest.and7());
    emit_u8(buf, imm);
}

pub fn emit_shl_reg_cl(buf: &mut MacroAssembler, x64: bool, dest: Reg) {
    if dest.msb() != 0 || x64 {
        emit_rex(buf, x64, 0, 0, dest.msb());
    }

    emit_op(buf, 0xD3);
    emit_modrm(buf, 0b11, 0b100, dest.and7());
}

pub fn emit_shrx(buf: &mut MacroAssembler, x64: bool, dest: Reg, lhs: Reg, rhs: Reg) {
    emit_vex3_rxbm(buf, dest.msb(), 0, lhs.msb(), VEXM_0F38);
    emit_vex3_wvlp(buf, x64, rhs.int(), VEXL_Z, VEXP_F2);
    emit_u8(buf, 0xF7);
    emit_modrm(buf, 0b11, dest.and7(), lhs.and7());
}

pub fn emit_shr_reg_cl(buf: &mut MacroAssembler, x64: bool, dest: Reg) {
    if dest.msb() != 0 || x64 {
        emit_rex(buf, x64, 0, 0, dest.msb());
    }

    emit_op(buf, 0xD3);
    emit_modrm(buf, 0b11, 0b101, dest.and7());
}

pub fn emit_shr_reg_imm(buf: &mut MacroAssembler, x64: bool, dest: Reg, imm: u8) {
    if dest.msb() != 0 || x64 {
        emit_rex(buf, x64, 0, 0, dest.msb());
    }

    emit_op(buf, if imm == 1 { 0xD1 } else { 0xC1 });
    emit_modrm(buf, 0b11, 0b101, dest.and7());

    if imm != 1 {
        emit_u8(buf, imm);
    }
}

pub fn emit_sarx(buf: &mut MacroAssembler, x64: bool, dest: Reg, lhs: Reg, rhs: Reg) {
    emit_vex3_rxbm(buf, dest.msb(), 0, lhs.msb(), VEXM_0F38);
    emit_vex3_wvlp(buf, x64, rhs.int(), VEXL_Z, VEXP_F3);
    emit_u8(buf, 0xF7);
    emit_modrm(buf, 0b11, dest.and7(), lhs.and7());
}

pub fn emit_sar_reg_cl(buf: &mut MacroAssembler, x64: bool, dest: Reg) {
    if dest.msb() != 0 || x64 {
        emit_rex(buf, x64, 0, 0, dest.msb());
    }

    emit_op(buf, 0xD3);
    emit_modrm(buf, 0b11, 0b111, dest.and7());
}

pub fn emit_rol_reg_cl(buf: &mut MacroAssembler, x64: bool, dest: Reg) {
    if dest.msb() != 0 || x64 {
        emit_rex(buf, x64, dest.msb(), 0, dest.msb());
    }
    emit_op(buf, 0xD3);
    emit_modrm(buf, 0b11, 0b000, dest.and7());
}

pub fn emit_ror_reg_cl(buf: &mut MacroAssembler, x64: bool, dest: Reg) {
    if dest.msb() != 0 || x64 {
        emit_rex(buf, x64, dest.msb(), 0, dest.msb());
    }
    emit_op(buf, 0xD3);
    emit_modrm(buf, 0b11, 0b001, dest.and7());
}

pub fn addss(buf: &mut MacroAssembler, dest: FReg, src: FReg) {
    sse_float_freg_freg(buf, false, 0x58, dest, src);
}

pub fn addsd(buf: &mut MacroAssembler, dest: FReg, src: FReg) {
    sse_float_freg_freg(buf, true, 0x58, dest, src);
}

pub fn subss(buf: &mut MacroAssembler, dest: FReg, src: FReg) {
    sse_float_freg_freg(buf, false, 0x5c, dest, src);
}

pub fn subsd(buf: &mut MacroAssembler, dest: FReg, src: FReg) {
    sse_float_freg_freg(buf, true, 0x5c, dest, src);
}

pub fn mulss(buf: &mut MacroAssembler, dest: FReg, src: FReg) {
    sse_float_freg_freg(buf, false, 0x59, dest, src);
}

pub fn mulsd(buf: &mut MacroAssembler, dest: FReg, src: FReg) {
    sse_float_freg_freg(buf, true, 0x59, dest, src);
}

pub fn divss(buf: &mut MacroAssembler, dest: FReg, src: FReg) {
    sse_float_freg_freg(buf, false, 0x5e, dest, src);
}

pub fn divsd(buf: &mut MacroAssembler, dest: FReg, src: FReg) {
    sse_float_freg_freg(buf, true, 0x5e, dest, src);
}

pub fn sqrtss(buf: &mut MacroAssembler, dest: FReg, src: FReg) {
    sse_float_freg_freg(buf, false, 0x51, dest, src);
}

pub fn sqrtsd(buf: &mut MacroAssembler, dest: FReg, src: FReg) {
    sse_float_freg_freg(buf, true, 0x51, dest, src);
}

pub fn movss(buf: &mut MacroAssembler, dest: FReg, src: FReg) {
    sse_float_freg_freg(buf, false, 0x10, dest, src);
}

pub fn movss_load(buf: &mut MacroAssembler, dest: FReg, mem: Mem) {
    sse_float_freg_mem(buf, false, 0x10, dest, mem);
}

pub fn movss_store(buf: &mut MacroAssembler, mem: Mem, src: FReg) {
    sse_float_freg_mem(buf, false, 0x11, src, mem);
}

pub fn movsd(buf: &mut MacroAssembler, dest: FReg, src: FReg) {
    sse_float_freg_freg(buf, true, 0x10, dest, src);
}

pub fn movsd_load(buf: &mut MacroAssembler, dest: FReg, mem: Mem) {
    sse_float_freg_mem(buf, true, 0x10, dest, mem);
}

pub fn movsd_store(buf: &mut MacroAssembler, mem: Mem, src: FReg) {
    sse_float_freg_mem(buf, true, 0x11, src, mem);
}

pub fn cvtsd2ss(buf: &mut MacroAssembler, dest: FReg, src: FReg) {
    sse_float_freg_freg(buf, true, 0x5a, dest, src);
}

pub fn cvtss2sd(buf: &mut MacroAssembler, dest: FReg, src: FReg) {
    sse_float_freg_freg(buf, false, 0x5a, dest, src);
}

pub fn cvtsi2ss(buf: &mut MacroAssembler, dest: FReg, x64: bool, src: Reg) {
    sse_float_freg_reg(buf, false, 0x2a, dest, x64, src);
}

pub fn cvtsi2sd(buf: &mut MacroAssembler, dest: FReg, x64: bool, src: Reg) {
    sse_float_freg_reg(buf, true, 0x2a, dest, x64, src);
}

pub fn cvttss2si(buf: &mut MacroAssembler, x64: bool, dest: Reg, src: FReg) {
    sse_float_reg_freg(buf, false, 0x2c, x64, dest, src);
}

pub fn cvttsd2si(buf: &mut MacroAssembler, x64: bool, dest: Reg, src: FReg) {
    sse_float_reg_freg(buf, true, 0x2c, x64, dest, src);
}

pub fn movd_f2i(buf: &mut MacroAssembler, x64: bool, dest: Reg, src: FReg) {
    sse_movd_freg_reg(buf, 0x7e, x64, dest, src);
}

pub fn movd_i2f(buf: &mut MacroAssembler, x64: bool, dest: FReg, src: Reg) {
    sse_movd_reg_freg(buf, 0x6e, x64, dest, src);
}

pub fn xorps(buf: &mut MacroAssembler, dest: FReg, src: Mem) {
    sse_float_freg_mem_66(buf, false, 0x57, dest, src);
}

pub fn xorpd(buf: &mut MacroAssembler, dest: FReg, src: Mem) {
    sse_float_freg_mem_66(buf, true, 0x57, dest, src);
}

fn sse_float_freg_freg(buf: &mut MacroAssembler, dbl: bool, op: u8, dest: FReg, src: FReg) {
    let prefix = if dbl { 0xf2 } else { 0xf3 };

    emit_op(buf, prefix);

    if dest.msb() != 0 || src.msb() != 0 {
        emit_rex(buf, false, dest.msb(), 0, src.msb());
    }

    emit_op(buf, 0x0f);
    emit_op(buf, op);
    emit_modrm(buf, 0b11, dest.and7(), src.and7());
}

fn sse_float_freg_mem(buf: &mut MacroAssembler, dbl: bool, op: u8, dest: FReg, src: Mem) {
    let prefix = if dbl { 0xf2 } else { 0xf3 };

    emit_op(buf, prefix);
    emit_rex_mem(buf, false, Reg(dest.0), &src);
    emit_op(buf, 0x0f);
    emit_op(buf, op);
    emit_mem(buf, Reg(dest.0), &src);
}

fn sse_float_freg_mem_66(buf: &mut MacroAssembler, dbl: bool, op: u8, dest: FReg, src: Mem) {
    if dbl {
        emit_op(buf, 0x66);
    }

    emit_rex_mem(buf, false, Reg(dest.0), &src);
    emit_op(buf, 0x0f);
    emit_op(buf, op);
    emit_mem(buf, Reg(dest.0), &src);
}

fn sse_float_freg_reg(
    buf: &mut MacroAssembler,
    dbl: bool,
    op: u8,
    dest: FReg,
    x64: bool,
    src: Reg,
) {
    let prefix = if dbl { 0xf2 } else { 0xf3 };

    emit_op(buf, prefix);

    if x64 || dest.msb() != 0 || src.msb() != 0 {
        emit_rex(buf, x64, dest.msb(), 0, src.msb());
    }

    emit_op(buf, 0x0f);
    emit_op(buf, op);
    emit_modrm(buf, 0b11, dest.and7(), src.and7());
}

fn sse_float_reg_freg(
    buf: &mut MacroAssembler,
    dbl: bool,
    op: u8,
    x64: bool,
    dest: Reg,
    src: FReg,
) {
    let prefix = if dbl { 0xf2 } else { 0xf3 };

    emit_op(buf, prefix);

    if x64 || dest.msb() != 0 || src.msb() != 0 {
        emit_rex(buf, x64, dest.msb(), 0, src.msb());
    }

    emit_op(buf, 0x0f);
    emit_op(buf, op);
    emit_modrm(buf, 0b11, dest.and7(), src.and7());
}

fn sse_movd_reg_freg(buf: &mut MacroAssembler, op: u8, x64: bool, dest: FReg, src: Reg) {
    emit_op(buf, 0x66);

    if x64 || dest.msb() != 0 || src.msb() != 0 {
        emit_rex(buf, x64, dest.msb(), 0, src.msb());
    }

    emit_op(buf, 0x0f);
    emit_op(buf, op);
    emit_modrm(buf, 0b11, dest.and7(), src.and7());
}

fn sse_movd_freg_reg(buf: &mut MacroAssembler, op: u8, x64: bool, dest: Reg, src: FReg) {
    emit_op(buf, 0x66);

    if x64 || dest.msb() != 0 || src.msb() != 0 {
        emit_rex(buf, x64, src.msb(), 0, dest.msb());
    }

    emit_op(buf, 0x0f);
    emit_op(buf, op);
    emit_modrm(buf, 0b11, src.and7(), dest.and7());
}

pub fn pxor(buf: &mut MacroAssembler, dest: FReg, src: FReg) {
    emit_op(buf, 0x66);

    if dest.msb() != 0 || src.msb() != 0 {
        emit_rex(buf, false, dest.msb(), 0, src.msb());
    }

    emit_op(buf, 0x0f);
    emit_op(buf, 0xef);
    emit_modrm(buf, 0b11, dest.and7(), src.and7());
}

pub fn ucomiss(buf: &mut MacroAssembler, dest: FReg, src: FReg) {
    sse_cmp(buf, false, dest, src);
}

pub fn ucomisd(buf: &mut MacroAssembler, dest: FReg, src: FReg) {
    sse_cmp(buf, true, dest, src);
}

pub fn popcnt(buf: &mut MacroAssembler, x64: bool, dest: Reg, src: Reg) {
    emit_op(buf, 0xf3);

    if x64 || src.msb() != 0 || dest.msb() != 0 {
        emit_rex(buf, x64, src.msb(), 0, dest.msb());
    }

    emit_op(buf, 0x0f);
    emit_op(buf, 0xb8);
    emit_modrm(buf, 0b11, src.and7(), dest.and7());
}

pub fn lzcnt(buf: &mut MacroAssembler, x64: bool, dest: Reg, src: Reg) {
    emit_op(buf, 0xf3);

    if x64 || src.msb() != 0 || dest.msb() != 0 {
        emit_rex(buf, x64, src.msb(), 0, dest.msb());
    }

    emit_op(buf, 0x0f);
    emit_op(buf, 0xbd);
    emit_modrm(buf, 0b11, src.and7(), dest.and7());
}

pub fn tzcnt(buf: &mut MacroAssembler, x64: bool, dest: Reg, src: Reg) {
    emit_op(buf, 0xf3);

    if x64 || src.msb() != 0 || dest.msb() != 0 {
        emit_rex(buf, x64, src.msb(), 0, dest.msb());
    }

    emit_op(buf, 0x0f);
    emit_op(buf, 0xbc);
    emit_modrm(buf, 0b11, src.and7(), dest.and7());
}

fn sse_cmp(buf: &mut MacroAssembler, dbl: bool, dest: FReg, src: FReg) {
    if dbl {
        emit_op(buf, 0x66);
    }

    if dest.msb() != 0 || src.msb() != 0 {
        emit_rex(buf, false, dest.msb(), 0, src.msb());
    }

    emit_op(buf, 0x0f);
    emit_op(buf, 0x2e);
    emit_modrm(buf, 0b11, dest.and7(), src.and7());
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::masm::{CondCode, MacroAssembler};
    use crate::ty::MachineMode;

    macro_rules! assert_emit {
        (
            $($expr:expr),*;
            $name:ident
        ) => {{
            let mut buf = MacroAssembler::new();
            $name(&mut buf);
            let expected = vec![$($expr,)*];

            assert_eq!(expected, buf.data());
        }};

        (
            $($expr:expr),*;
            $name:ident
            (
                    $($param:expr),+
            )
        ) => {{
            let mut buf = MacroAssembler::new();
            $name(&mut buf, $($param,)*);
            let expected = vec![$($expr,)*];
            let data = buf.data();

            if expected != data {
                print!("exp: ");

                for (ind, val) in expected.iter().enumerate() {
                    if ind > 0 { print!(", "); }

                    print!("{:02x}", val);
                }

                print!("\ngot: ");

                for (ind, val) in data.iter().enumerate() {
                    if ind > 0 { print!(", "); }

                    print!("{:02x}", val);
                }

                println!("");

                panic!("emitted code wrong.");
            }
        }};
    }

    #[test]
    fn test_fits8() {
        assert!(fits_i8(1));
        assert!(fits_i8(0));
        assert!(fits_i8(-1));
        assert!(fits_i8(127));
        assert!(fits_i8(-128));

        assert!(!fits_i8(128));
        assert!(!fits_i8(-129));
    }

    #[test]
    fn test_shlq_reg() {
        assert_emit!(0x48, 0xC1, 0xE0, 0x02; emit_shlq_reg(2, RAX));
        assert_emit!(0x49, 0xC1, 0xE4, 0x07; emit_shlq_reg(7, R12));
    }

    #[test]
    fn test_shll_reg() {
        assert_emit!(0xC1, 0xE0, 0x02; emit_shll_reg(2, RAX));
        assert_emit!(0x41, 0xC1, 0xE4, 0x07; emit_shll_reg(7, R12));
    }

    #[test]
    fn test_emit_jcc_zero() {
        let mut buf = MacroAssembler::new();
        let lbl = buf.create_label();
        emit_jcc(&mut buf, CondCode::Zero, lbl);
        buf.nop();
        buf.bind_label(lbl);
        assert_eq!(vec![0x0f, 0x84, 1, 0, 0, 0, 0x90], buf.data());
    }

    #[test]
    fn test_emit_jcc_non_zero() {
        let mut buf = MacroAssembler::new();
        let lbl = buf.create_label();
        emit_jcc(&mut buf, CondCode::NonZero, lbl);
        buf.nop();
        buf.bind_label(lbl);
        assert_eq!(vec![0x0f, 0x85, 1, 0, 0, 0, 0x90], buf.data());
    }

    #[test]
    fn test_emit_jcc_greater() {
        let mut buf = MacroAssembler::new();
        let lbl = buf.create_label();
        emit_jcc(&mut buf, CondCode::Greater, lbl);
        buf.nop();
        buf.bind_label(lbl);
        assert_eq!(vec![0x0f, 0x8F, 1, 0, 0, 0, 0x90], buf.data());
    }

    #[test]
    fn test_emit_jcc_greater_or_equal() {
        let mut buf = MacroAssembler::new();
        let lbl = buf.create_label();
        emit_jcc(&mut buf, CondCode::GreaterEq, lbl);
        buf.nop();
        buf.bind_label(lbl);
        assert_eq!(vec![0x0f, 0x8D, 1, 0, 0, 0, 0x90], buf.data());
    }

    #[test]
    fn test_emit_jcc_less() {
        let mut buf = MacroAssembler::new();
        let lbl = buf.create_label();
        emit_jcc(&mut buf, CondCode::Less, lbl);
        buf.nop();
        buf.bind_label(lbl);
        assert_eq!(vec![0x0f, 0x8C, 1, 0, 0, 0, 0x90], buf.data());
    }

    #[test]
    fn test_emit_jcc_less_or_equal() {
        let mut buf = MacroAssembler::new();
        let lbl = buf.create_label();
        emit_jcc(&mut buf, CondCode::LessEq, lbl);
        buf.nop();
        buf.bind_label(lbl);
        assert_eq!(vec![0x0f, 0x8E, 1, 0, 0, 0, 0x90], buf.data());
    }

    #[test]
    fn test_emit_jcc_unsigned_greater() {
        let mut buf = MacroAssembler::new();
        let lbl = buf.create_label();
        emit_jcc(&mut buf, CondCode::UnsignedGreater, lbl);
        buf.nop();
        buf.bind_label(lbl);
        assert_eq!(vec![0x0f, 0x87, 1, 0, 0, 0, 0x90], buf.data());
    }

    #[test]
    fn test_emit_jcc_unsigned_greater_or_equal() {
        let mut buf = MacroAssembler::new();
        let lbl = buf.create_label();
        emit_jcc(&mut buf, CondCode::UnsignedGreaterEq, lbl);
        buf.nop();
        buf.bind_label(lbl);
        assert_eq!(vec![0x0f, 0x83, 1, 0, 0, 0, 0x90], buf.data());
    }

    #[test]
    fn test_emit_jcc_unsigned_less() {
        let mut buf = MacroAssembler::new();
        let lbl = buf.create_label();
        emit_jcc(&mut buf, CondCode::UnsignedLess, lbl);
        buf.nop();
        buf.bind_label(lbl);
        assert_eq!(vec![0x0f, 0x82, 1, 0, 0, 0, 0x90], buf.data());
    }

    #[test]
    fn test_emit_jcc_unsigned_less_or_equal() {
        let mut buf = MacroAssembler::new();
        let lbl = buf.create_label();
        emit_jcc(&mut buf, CondCode::UnsignedLessEq, lbl);
        buf.nop();
        buf.bind_label(lbl);
        assert_eq!(vec![0x0f, 0x86, 1, 0, 0, 0, 0x90], buf.data());
    }

    #[test]
    fn test_emit_jmp() {
        let mut buf = MacroAssembler::new();
        let lbl = buf.create_label();
        emit_jmp(&mut buf, lbl);
        buf.nop();
        buf.bind_label(lbl);
        assert_eq!(vec![0xe9, 1, 0, 0, 0, 0x90], buf.data());
    }

    #[test]
    fn test_int_shlx() {
        assert_emit!(0xC4, 0xE2, 0xE1, 0xF7, 0xC2; emit_shlx(true, RAX, RDX, RBX));
        assert_emit!(0xC4, 0x42, 0x29, 0xF7, 0xC1; emit_shlx(false, R8, R9, R10));
    }

    #[test]
    fn test_int_shrx() {
        assert_emit!(0xC4, 0xE2, 0xE3, 0xF7, 0xC2; emit_shrx(true, RAX, RDX, RBX));
        assert_emit!(0xC4, 0x42, 0x2B, 0xF7, 0xC1; emit_shrx(false, R8, R9, R10));
    }

    #[test]
    fn test_int_sarx() {
        assert_emit!(0xC4, 0xE2, 0xE2, 0xF7, 0xC2; emit_sarx(true, RAX, RDX, RBX));
        assert_emit!(0xC4, 0x42, 0x22, 0xF7, 0xCA; emit_sarx(false, R9, R10, R11));
    }

    #[test]
    fn test_shl_reg_cl() {
        assert_emit!(0xD3, 0xE0; emit_shl_reg_cl(false, RAX));
        assert_emit!(0x41, 0xD3, 0xE1; emit_shl_reg_cl(false, R9));

        assert_emit!(0x48, 0xD3, 0xE0; emit_shl_reg_cl(true, RAX));
        assert_emit!(0x49, 0xD3, 0xE1; emit_shl_reg_cl(true, R9));
    }

    #[test]
    fn test_shr_reg_reg() {
        assert_emit!(0xD3, 0xE8; emit_shr_reg_cl(false, RAX));
        assert_emit!(0x41, 0xD3, 0xE9; emit_shr_reg_cl(false, R9));

        assert_emit!(0x48, 0xD3, 0xE8; emit_shr_reg_cl(true, RAX));
        assert_emit!(0x49, 0xD3, 0xE9; emit_shr_reg_cl(true, R9));
    }

    #[test]
    fn test_shr_reg_imm() {
        assert_emit!(0x48, 0xC1, 0xE8, 0x02; emit_shr_reg_imm(true, RAX, 2));
        assert_emit!(0x48, 0xC1, 0xE8, 0x09; emit_shr_reg_imm(true, RAX, 9));
        assert_emit!(0x49, 0xC1, 0xEA, 0x09; emit_shr_reg_imm(true, R10, 9));
        assert_emit!(0x49, 0xC1, 0xEF, 0x09; emit_shr_reg_imm(true, R15, 9));

        assert_emit!(0xC1, 0xE8, 0x09; emit_shr_reg_imm(false, RAX, 9));
        assert_emit!(0x41, 0xC1, 0xE9, 0x09; emit_shr_reg_imm(false, R9, 9));

        assert_emit!(0xD1, 0xE8; emit_shr_reg_imm(false, RAX, 1));
        assert_emit!(0x41, 0xD1, 0xE9; emit_shr_reg_imm(false, R9, 1));
    }

    #[test]
    fn test_sar_reg_reg() {
        assert_emit!(0xD3, 0xF8; emit_sar_reg_cl(false, RAX));
        assert_emit!(0x41, 0xD3, 0xF9; emit_sar_reg_cl(false, R9));

        assert_emit!(0x48, 0xD3, 0xF8; emit_sar_reg_cl(true, RAX));
        assert_emit!(0x49, 0xD3, 0xF9; emit_sar_reg_cl(true, R9));
    }

    #[test]
    fn test_cmp_memindex_reg() {
        let p = MachineMode::Ptr;

        // cmp [rax+rbx*8+1],rcx
        assert_emit!(0x48, 0x39, 0x4c, 0xd8, 1; emit_cmp_memindex_reg(p, RAX, RBX, 8, 1, RCX));

        // cmp [rax+rbx*8],rcx
        assert_emit!(0x48, 0x39, 0x0c, 0xd8; emit_cmp_memindex_reg(p, RAX, RBX, 8, 0, RCX));

        // cmp [rax+rbx*8+256],rcx
        assert_emit!(0x48, 0x39, 0x8c, 0xd8, 0, 1, 0, 0;
                     emit_cmp_memindex_reg(p, RAX, RBX, 8, 256, RCX));

        // cmp [r8+rbp*1],rsp
        assert_emit!(0x49, 0x39, 0x24, 0x28;
                     emit_cmp_memindex_reg(p, R8, RBP, 1, 0, RSP));

        // cmp [rsi+r9*1],rdi
        assert_emit!(0x4a, 0x39, 0x3c, 0x0e; emit_cmp_memindex_reg(p, RSI, R9, 1, 0, RDI));

        // cmp [rsp+rsi*1],r15
        assert_emit!(0x4c, 0x39, 0x3c, 0x34; emit_cmp_memindex_reg(p, RSP, RSI, 1, 0, R15));

        // cmp [rsp+rbp],rax
        assert_emit!(0x48, 0x39, 0x04, 0x2c; emit_cmp_memindex_reg(p, RSP, RBP, 1, 0, RAX));
    }

    #[test]
    #[should_panic]
    fn test_cmp_memindex_reg_base_rip() {
        let mut buf = MacroAssembler::new();
        emit_cmp_memindex_reg(&mut buf, MachineMode::Ptr, RIP, RAX, 1, 0, RAX);
    }

    #[test]
    #[should_panic]
    fn test_cmp_memindex_reg_index_rip() {
        let mut buf = MacroAssembler::new();
        emit_cmp_memindex_reg(&mut buf, MachineMode::Ptr, RAX, RIP, 1, 0, RAX);
    }

    #[test]
    #[should_panic]
    fn test_cmp_memindex_reg_dest_rip() {
        let mut buf = MacroAssembler::new();
        emit_cmp_memindex_reg(&mut buf, MachineMode::Ptr, RAX, RBX, 1, 0, RIP);
    }

    #[test]
    fn test_cmp_mem_reg() {
        let p = MachineMode::Ptr;

        // cmp [rbx+1],rax
        assert_emit!(0x48, 0x39, 0x43, 1; emit_cmp_mem_reg(p, RBX, 1, RAX));

        // cmp [rbx+256],rax
        assert_emit!(0x48, 0x39, 0x83, 0, 1, 0, 0; emit_cmp_mem_reg(p, RBX, 256, RAX));

        // cmp [rdi+1],rax
        assert_emit!(0x48, 0x39, 0x47, 1; emit_cmp_mem_reg(p, RDI, 1, RAX));

        // cmp [r9+1],rax
        assert_emit!(0x49, 0x39, 0x41, 1; emit_cmp_mem_reg(p, R9, 1, RAX));

        // cmp [rdi+1],r10
        assert_emit!(0x4c, 0x39, 0x57, 1; emit_cmp_mem_reg(p, RDI, 1, R10));

        // cmp [rip+1], rax
        assert_emit!(0x48, 0x39, 0x05, 1, 0, 0, 0; emit_cmp_mem_reg(p, RIP, 1, RAX));

        let i = MachineMode::Int32;

        // cmp [rbx+1], eax
        assert_emit!(0x39, 0x43, 1; emit_cmp_mem_reg(i, RBX, 1, RAX));

        // cmp [rbx+1], r10d
        assert_emit!(0x44, 0x39, 0x53, 1; emit_cmp_mem_reg(i, RBX, 1, R10));
    }

    #[test]
    fn test_cmp_mem_imm() {
        let p = MachineMode::Ptr;

        // cmp [rbx+1], 2
        assert_emit!(0x48, 0x83, 0x7b, 1, 2; emit_cmp_mem_imm(p, RBX, 1, 2));

        // cmp [rbx+256], 2
        assert_emit!(0x48, 0x83, 0xBB, 0, 1, 0, 0, 2; emit_cmp_mem_imm(p, RBX, 256, 2));

        // cmp [rdi+1], 256
        assert_emit!(0x48, 0x81, 0x7F, 1, 0, 1, 0, 0; emit_cmp_mem_imm(p, RDI, 1, 256));

        // cmp [r9+1], 2
        assert_emit!(0x49, 0x83, 0x79, 1, 2; emit_cmp_mem_imm(p, R9, 1, 2));

        let i = MachineMode::Int32;

        // cmp [rbx+1], 2
        assert_emit!(0x83, 0x7B, 1, 2; emit_cmp_mem_imm(i, RBX, 1, 2));

        // cmp [rbx+1], 256
        assert_emit!(0x81, 0x7B, 1, 0, 1, 0, 0; emit_cmp_mem_imm(i, RBX, 1, 256));

        let b = MachineMode::Int8;

        // cmp [rbx+1], 2
        assert_emit!(0x80, 0x7B, 1, 2; emit_cmp_mem_imm(b, RBX, 1, 2));

        // cmp [R15+256], 2
        assert_emit!(0x41, 0x80, 0xBF, 0, 1, 0, 0, 2; emit_cmp_mem_imm(b, R15, 256, 2));
    }

    #[test]
    #[should_panic]
    fn test_cmp_mem_imm_i32_for_i8() {
        let mut buf = MacroAssembler::new();
        emit_cmp_mem_imm(&mut buf, MachineMode::Int8, R15, 256, 256);
    }

    #[test]
    #[should_panic]
    fn test_cmp_mem_reg_dest_rip() {
        let mut buf = MacroAssembler::new();
        emit_cmp_mem_reg(&mut buf, MachineMode::Ptr, RAX, 1, RIP);
    }

    #[test]
    fn test_emit_movsx() {
        assert_emit!(0x48, 0x63, 0xc0; emit_movsx(RAX, RAX));
        assert_emit!(0x4c, 0x63, 0xc8; emit_movsx(RAX, R9));
        assert_emit!(0x4c, 0x63, 0xf8; emit_movsx(RAX, R15));
        assert_emit!(0x49, 0x63, 0xc2; emit_movsx(R10, RAX));
        assert_emit!(0x4d, 0x63, 0xfe; emit_movsx(R14, R15));
    }

    #[test]
    fn test_addss() {
        assert_emit!(0xf3, 0x0f, 0x58, 0xc1; addss(XMM0, XMM1));
        assert_emit!(0xf3, 0x41, 0x0f, 0x58, 0xdf; addss(XMM3, XMM15));
        assert_emit!(0xf3, 0x44, 0x0f, 0x58, 0xc4; addss(XMM8, XMM4));
    }

    #[test]
    fn test_addsd() {
        assert_emit!(0xf2, 0x0f, 0x58, 0xc1; addsd(XMM0, XMM1));
        assert_emit!(0xf2, 0x41, 0x0f, 0x58, 0xdf; addsd(XMM3, XMM15));
        assert_emit!(0xf2, 0x44, 0x0f, 0x58, 0xc4; addsd(XMM8, XMM4));
    }

    #[test]
    fn test_subss() {
        assert_emit!(0xf3, 0x0f, 0x5c, 0xc1; subss(XMM0, XMM1));
        assert_emit!(0xf3, 0x41, 0x0f, 0x5c, 0xdf; subss(XMM3, XMM15));
        assert_emit!(0xf3, 0x44, 0x0f, 0x5c, 0xc4; subss(XMM8, XMM4));
    }

    #[test]
    fn test_subsd() {
        assert_emit!(0xf2, 0x0f, 0x5c, 0xc1; subsd(XMM0, XMM1));
        assert_emit!(0xf2, 0x41, 0x0f, 0x5c, 0xdf; subsd(XMM3, XMM15));
        assert_emit!(0xf2, 0x44, 0x0f, 0x5c, 0xc4; subsd(XMM8, XMM4));
    }

    #[test]
    fn test_mulss() {
        assert_emit!(0xf3, 0x0f, 0x59, 0xc1; mulss(XMM0, XMM1));
        assert_emit!(0xf3, 0x41, 0x0f, 0x59, 0xdf; mulss(XMM3, XMM15));
        assert_emit!(0xf3, 0x44, 0x0f, 0x59, 0xc4; mulss(XMM8, XMM4));
    }

    #[test]
    fn test_mulsd() {
        assert_emit!(0xf2, 0x0f, 0x59, 0xc1; mulsd(XMM0, XMM1));
        assert_emit!(0xf2, 0x41, 0x0f, 0x59, 0xdf; mulsd(XMM3, XMM15));
        assert_emit!(0xf2, 0x44, 0x0f, 0x59, 0xc4; mulsd(XMM8, XMM4));
    }

    #[test]
    fn test_divss() {
        assert_emit!(0xf3, 0x0f, 0x5e, 0xc1; divss(XMM0, XMM1));
        assert_emit!(0xf3, 0x41, 0x0f, 0x5e, 0xdf; divss(XMM3, XMM15));
        assert_emit!(0xf3, 0x44, 0x0f, 0x5e, 0xc4; divss(XMM8, XMM4));
    }

    #[test]
    fn test_divsd() {
        assert_emit!(0xf2, 0x0f, 0x5e, 0xc1; divsd(XMM0, XMM1));
        assert_emit!(0xf2, 0x41, 0x0f, 0x5e, 0xdf; divsd(XMM3, XMM15));
        assert_emit!(0xf2, 0x44, 0x0f, 0x5e, 0xc4; divsd(XMM8, XMM4));
    }

    #[test]
    fn test_movss() {
        assert_emit!(0xf3, 0x0f, 0x10, 0xc1; movss(XMM0, XMM1));
        assert_emit!(0xf3, 0x41, 0x0f, 0x10, 0xdf; movss(XMM3, XMM15));
        assert_emit!(0xf3, 0x44, 0x0f, 0x10, 0xc4; movss(XMM8, XMM4));
    }

    #[test]
    fn test_movsd() {
        assert_emit!(0xf2, 0x0f, 0x10, 0xc1; movsd(XMM0, XMM1));
        assert_emit!(0xf2, 0x41, 0x0f, 0x10, 0xdf; movsd(XMM3, XMM15));
        assert_emit!(0xf2, 0x44, 0x0f, 0x10, 0xc4; movsd(XMM8, XMM4));
    }

    #[test]
    fn test_cvtss2sd() {
        assert_emit!(0xf3, 0x0f, 0x5a, 0xc1; cvtss2sd(XMM0, XMM1));
        assert_emit!(0xf3, 0x41, 0x0f, 0x5a, 0xdf; cvtss2sd(XMM3, XMM15));
        assert_emit!(0xf3, 0x44, 0x0f, 0x5a, 0xc4; cvtss2sd(XMM8, XMM4));
    }

    #[test]
    fn test_cvtsd2ss() {
        assert_emit!(0xf2, 0x0f, 0x5a, 0xc1; cvtsd2ss(XMM0, XMM1));
        assert_emit!(0xf2, 0x41, 0x0f, 0x5a, 0xdf; cvtsd2ss(XMM3, XMM15));
        assert_emit!(0xf2, 0x44, 0x0f, 0x5a, 0xc4; cvtsd2ss(XMM8, XMM4));
    }

    #[test]
    fn test_cvtsi2ss() {
        assert_emit!(0xf3, 0x0f, 0x2a, 0xc1; cvtsi2ss(XMM0, false, RCX));
        assert_emit!(0xf3, 0x41, 0x0f, 0x2a, 0xdf; cvtsi2ss(XMM3, false, R15));
        assert_emit!(0xf3, 0x44, 0x0f, 0x2a, 0xc4; cvtsi2ss(XMM8, false, RSP));

        assert_emit!(0xf3, 0x48, 0x0f, 0x2a, 0xc1; cvtsi2ss(XMM0, true, RCX));
        assert_emit!(0xf3, 0x49, 0x0f, 0x2a, 0xdf; cvtsi2ss(XMM3, true, R15));
        assert_emit!(0xf3, 0x4c, 0x0f, 0x2a, 0xc4; cvtsi2ss(XMM8, true, RSP));
    }

    #[test]
    fn test_cvtsi2sd() {
        assert_emit!(0xf2, 0x0f, 0x2a, 0xc1; cvtsi2sd(XMM0, false, RCX));
        assert_emit!(0xf2, 0x41, 0x0f, 0x2a, 0xdf; cvtsi2sd(XMM3, false, R15));
        assert_emit!(0xf2, 0x44, 0x0f, 0x2a, 0xc4; cvtsi2sd(XMM8, false, RSP));

        assert_emit!(0xf2, 0x48, 0x0f, 0x2a, 0xc1; cvtsi2sd(XMM0, true, RCX));
        assert_emit!(0xf2, 0x49, 0x0f, 0x2a, 0xdf; cvtsi2sd(XMM3, true, R15));
        assert_emit!(0xf2, 0x4c, 0x0f, 0x2a, 0xc4; cvtsi2sd(XMM8, true, RSP));
    }

    #[test]
    fn test_cvttss2si() {
        assert_emit!(0xf3, 0x0f, 0x2c, 0xc8; cvttss2si(false, RCX, XMM0));
        assert_emit!(0xf3, 0x44, 0x0f, 0x2c, 0xfb; cvttss2si(false, R15, XMM3));
        assert_emit!(0xf3, 0x41, 0x0f, 0x2c, 0xe0; cvttss2si(false, RSP, XMM8));

        assert_emit!(0xf3, 0x48, 0x0f, 0x2c, 0xc8; cvttss2si(true, RCX, XMM0));
        assert_emit!(0xf3, 0x4c, 0x0f, 0x2c, 0xfb; cvttss2si(true, R15, XMM3));
        assert_emit!(0xf3, 0x49, 0x0f, 0x2c, 0xe0; cvttss2si(true, RSP, XMM8));
    }

    #[test]
    fn test_cvttsd2si() {
        assert_emit!(0xf2, 0x0f, 0x2c, 0xc8; cvttsd2si(false, RCX, XMM0));
        assert_emit!(0xf2, 0x44, 0x0f, 0x2c, 0xfb; cvttsd2si(false, R15, XMM3));
        assert_emit!(0xf2, 0x41, 0x0f, 0x2c, 0xe0; cvttsd2si(false, RSP, XMM8));

        assert_emit!(0xf2, 0x48, 0x0f, 0x2c, 0xc8; cvttsd2si(true, RCX, XMM0));
        assert_emit!(0xf2, 0x4c, 0x0f, 0x2c, 0xfb; cvttsd2si(true, R15, XMM3));
        assert_emit!(0xf2, 0x49, 0x0f, 0x2c, 0xe0; cvttsd2si(true, RSP, XMM8));
    }

    #[test]
    fn test_movd_f2i() {
        assert_emit!(0x66, 0x0F, 0x7E, 0xC7; movd_f2i(false, RDI, XMM0));
        assert_emit!(0x66, 0x41, 0x0F, 0x7E, 0xC0; movd_f2i(false, R8, XMM0));
        assert_emit!(0x66, 0x41, 0x0F, 0x7E, 0xF8; movd_f2i(false, R8, XMM7));
        assert_emit!(0x66, 0x45, 0x0F, 0x7E, 0xC0; movd_f2i(false, R8, XMM8));
        assert_emit!(0x66, 0x45, 0x0F, 0x7E, 0xF8; movd_f2i(false, R8, XMM15));
        assert_emit!(0x66, 0x41, 0x0F, 0x7E, 0xC7; movd_f2i(false, R15, XMM0));
        assert_emit!(0x66, 0x41, 0x0F, 0x7E, 0xFF; movd_f2i(false, R15, XMM7));
        assert_emit!(0x66, 0x45, 0x0F, 0x7E, 0xC7; movd_f2i(false, R15, XMM8));
        assert_emit!(0x66, 0x45, 0x0F, 0x7E, 0xFF; movd_f2i(false, R15, XMM15));

        assert_emit!(0x66, 0x48, 0x0F, 0x7E, 0xC7; movd_f2i(true, RDI, XMM0));
        assert_emit!(0x66, 0x49, 0x0F, 0x7E, 0xC0; movd_f2i(true, R8, XMM0));
        assert_emit!(0x66, 0x49, 0x0F, 0x7E, 0xF8; movd_f2i(true, R8, XMM7));
        assert_emit!(0x66, 0x4D, 0x0F, 0x7E, 0xC0; movd_f2i(true, R8, XMM8));
        assert_emit!(0x66, 0x4D, 0x0F, 0x7E, 0xF8; movd_f2i(true, R8, XMM15));
        assert_emit!(0x66, 0x49, 0x0F, 0x7E, 0xC7; movd_f2i(true, R15, XMM0));
        assert_emit!(0x66, 0x49, 0x0F, 0x7E, 0xFF; movd_f2i(true, R15, XMM7));
        assert_emit!(0x66, 0x4D, 0x0F, 0x7E, 0xC7; movd_f2i(true, R15, XMM8));
        assert_emit!(0x66, 0x4D, 0x0F, 0x7E, 0xFF; movd_f2i(true, R15, XMM15));
    }

    #[test]
    fn test_movd_i2f() {
        assert_emit!(0x66, 0x0F, 0x6E, 0xC0; movd_i2f(false, XMM0, RAX));
        assert_emit!(0x66, 0x41, 0x0F, 0x6E, 0xC0; movd_i2f(false, XMM0, R8));
        assert_emit!(0x66, 0x41, 0x0F, 0x6E, 0xF8; movd_i2f(false, XMM7, R8));
        assert_emit!(0x66, 0x45, 0x0F, 0x6E, 0xC0; movd_i2f(false, XMM8, R8));
        assert_emit!(0x66, 0x45, 0x0F, 0x6E, 0xF8; movd_i2f(false, XMM15, R8));
        assert_emit!(0x66, 0x41, 0x0F, 0x6E, 0xC7; movd_i2f(false, XMM0, R15));
        assert_emit!(0x66, 0x41, 0x0F, 0x6E, 0xFF; movd_i2f(false, XMM7, R15));
        assert_emit!(0x66, 0x45, 0x0F, 0x6E, 0xC7; movd_i2f(false, XMM8, R15));
        assert_emit!(0x66, 0x45, 0x0F, 0x6E, 0xFF; movd_i2f(false, XMM15, R15));

        assert_emit!(0x66, 0x48, 0x0F, 0x6E, 0xC0; movd_i2f(true, XMM0, RAX));
        assert_emit!(0x66, 0x49, 0x0F, 0x6E, 0xC0; movd_i2f(true, XMM0, R8));
        assert_emit!(0x66, 0x49, 0x0F, 0x6E, 0xF8; movd_i2f(true, XMM7, R8));
        assert_emit!(0x66, 0x4D, 0x0F, 0x6E, 0xC0; movd_i2f(true, XMM8, R8));
        assert_emit!(0x66, 0x4D, 0x0F, 0x6E, 0xF8; movd_i2f(true, XMM15, R8));
        assert_emit!(0x66, 0x49, 0x0F, 0x6E, 0xC7; movd_i2f(true, XMM0, R15));
        assert_emit!(0x66, 0x49, 0x0F, 0x6E, 0xFF; movd_i2f(true, XMM7, R15));
        assert_emit!(0x66, 0x4D, 0x0F, 0x6E, 0xC7; movd_i2f(true, XMM8, R15));
        assert_emit!(0x66, 0x4D, 0x0F, 0x6E, 0xFF; movd_i2f(true, XMM15, R15));
    }

    #[test]
    fn test_pxor() {
        assert_emit!(0x66, 0x0f, 0xef, 0xc8; pxor(XMM1, XMM0));
        assert_emit!(0x66, 0x44, 0x0f, 0xef, 0xfb; pxor(XMM15, XMM3));
        assert_emit!(0x66, 0x41, 0x0f, 0xef, 0xe0; pxor(XMM4, XMM8));
    }

    #[test]
    fn test_ucomiss() {
        assert_emit!(0x0f, 0x2e, 0xc8; ucomiss(XMM1, XMM0));
        assert_emit!(0x44, 0x0f, 0x2e, 0xfb; ucomiss(XMM15, XMM3));
        assert_emit!(0x41, 0x0f, 0x2e, 0xe0; ucomiss(XMM4, XMM8));
    }

    #[test]
    fn test_ucomisd() {
        assert_emit!(0x66, 0x0f, 0x2e, 0xc8; ucomisd(XMM1, XMM0));
        assert_emit!(0x66, 0x44, 0x0f, 0x2e, 0xfb; ucomisd(XMM15, XMM3));
        assert_emit!(0x66, 0x41, 0x0f, 0x2e, 0xe0; ucomisd(XMM4, XMM8));
    }

    #[test]
    fn test_popcnt() {
        assert_emit!(0xf3, 0x48, 0x0f, 0xb8, 0xc7; popcnt(true, RDI, RAX));
        assert_emit!(0xf3, 0x0f, 0xb8, 0xc7; popcnt(false, RDI, RAX));
    }

    #[test]
    fn test_lzcnt() {
        assert_emit!(0xf3, 0x48, 0x0f, 0xbd, 0xc7; lzcnt(true, RDI, RAX));
        assert_emit!(0xf3, 0x0f, 0xbd, 0xc7; lzcnt(false, RDI, RAX));
    }

    #[test]
    fn test_tzcnt() {
        assert_emit!(0xf3, 0x48, 0x0f, 0xbc, 0xc7; tzcnt(true, RDI, RAX));
        assert_emit!(0xf3, 0x0f, 0xbc, 0xc7; tzcnt(false, RDI, RAX));
    }

    #[test]
    fn test_movss_load() {
        assert_emit!(0xf3, 0x0f, 0x10, 0x44, 0x88, 1; movss_load(XMM0, Mem::Index(RAX, RCX, 4, 1)));
        assert_emit!(0xf2, 0x0f, 0x10, 0x48, 1; movsd_load(XMM1, Mem::Base(RAX, 1)));
        assert_emit!(0xf3, 0x44, 0x0f, 0x10, 0xbc, 0x88, 0, 1, 0, 0;
                     movss_load(XMM15, Mem::Index(RAX, RCX, 4, 256)));
        assert_emit!(0xf3, 0x41, 0x0f, 0x10, 0x4c, 0x8f, 1;
                     movss_load(XMM1, Mem::Index(R15, RCX, 4, 1)));
        assert_emit!(0xf2, 0x43, 0x0f, 0x10, 0x4c, 0xbf, 2;
                     movsd_load(XMM1, Mem::Index(R15, R15, 4, 2)));
        assert_emit!(0xf3, 0x0f, 0x10, 0x05, 0xec, 0xff, 0xff, 0xff;
                     movss_load(XMM0, Mem::Base(RIP, -20)));
    }

    #[test]
    fn test_movss_store() {
        assert_emit!(0xf3, 0x0f, 0x11, 0x40, 1; movss_store(Mem::Base(RAX, 1), XMM0));
        assert_emit!(0xf2, 0x44, 0x0f, 0x11, 0x78, 1; movsd_store(Mem::Base(RAX, 1), XMM15));
    }

    #[test]
    fn test_xorps() {
        assert_emit!(0x0f, 0x57, 0x05, 0xf6, 0xff, 0xff, 0xff;
                     xorps(XMM0, Mem::Base(RIP, -10)));
        assert_emit!(0x66, 0x0f, 0x57, 0x05, 0xf6, 0xff, 0xff, 0xff;
                     xorpd(XMM0, Mem::Base(RIP, -10)));
    }

    #[test]
    fn test_lea() {
        // lea rax, [rax*8]
        assert_emit!(0x48, 0x8d, 0x04, 0xc5, 0, 0, 0, 0;
                     lea(RAX, Mem::Offset(RAX, 8, 0)));

        // lea rax, [rax*8+16]
        assert_emit!(0x48, 0x8d, 0x04, 0xc5, 0x10, 0, 0, 0;
                     lea(RAX, Mem::Offset(RAX, 8, 16)));

        // lea r9,[rax*8+16]
        assert_emit!(0x4c, 0x8d, 0x0c, 0xc5, 0x10, 0, 0, 0;
                     lea(R9, Mem::Offset(RAX, 8, 16)));

        // lea rax,[r9*8+16]
        assert_emit!(0x4a, 0x8d, 0x04, 0xcd, 0x10, 0, 0, 0;
                     lea(RAX, Mem::Offset(R9, 8, 16)));
    }

    #[test]
    fn test_emit_jmp_reg() {
        assert_emit!(0xFF, 0xE0;
                     emit_jmp_reg(RAX));
        assert_emit!(0x41, 0xFF, 0xE2;
                     emit_jmp_reg(R10));
        assert_emit!(0x41, 0xFF, 0xE1;
                     emit_jmp_reg(R9));
        assert_emit!(0xFF, 0xE2;
                     emit_jmp_reg(RDX));
    }
}
