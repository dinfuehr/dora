use cpu::*;
use baseline::codegen::CondCode;
use masm::{MacroAssembler, Label};
use ty::MachineMode;

pub fn emit_or_reg_reg(buf: &mut MacroAssembler, x64: u8, src: Reg, dest: Reg) {
    emit_alu_reg_reg(buf, x64, 0x09, src, dest);
}

pub fn emit_and_reg_reg(buf: &mut MacroAssembler, x64: u8, src: Reg, dest: Reg) {
    emit_alu_reg_reg(buf, x64, 0x21, src, dest);
}

pub fn emit_xor_reg_reg(buf: &mut MacroAssembler, x64: u8, src: Reg, dest: Reg) {
    emit_alu_reg_reg(buf, x64, 0x31, src, dest);
}

fn emit_alu_reg_reg(buf: &mut MacroAssembler, x64: u8, opcode: u8, src: Reg, dest: Reg) {
    if x64 != 0 || src.msb() != 0 || dest.msb() != 0 {
        emit_rex(buf, x64, src.msb(), 0, dest.msb());
    }

    emit_op(buf, opcode);
    emit_modrm(buf, 0b11, src.and7(), dest.and7());
}

pub fn emit_movl_imm_reg(buf: &mut MacroAssembler, imm: i32, reg: Reg) {
    if reg.msb() != 0 {
        emit_rex(buf, 0, 0, 0, 1);
    }

    emit_op(buf, (0xB8 as u8) + reg.and7());
    emit_u32(buf, imm as u32);
}

// mov 32bit immediate and sign-extend into 64bit-register
pub fn emit_movq_imm_reg(buf: &mut MacroAssembler, imm: i32, reg: Reg) {
    emit_rex(buf, 1, 0, 0, reg.msb());
    emit_op(buf, 0xc7);
    emit_modrm(buf, 0b11, 0, reg.and7());
    emit_u32(buf, imm as u32);
}

pub fn emit_movq_imm64_reg(buf: &mut MacroAssembler, imm: i64, reg: Reg) {
    emit_rex(buf, 1, 0, 0, reg.msb());
    emit_op(buf, 0xb8 + reg.and7());
    emit_u64(buf, imm as u64);
}

pub fn emit_movb_memq_reg(buf: &mut MacroAssembler, src: Reg, disp: i32, dest: Reg) {
    let rex_prefix = if dest != RAX && dest != RBX && dest != RCX && dest != RDX {
        1
    } else {
        0
    };

    emit_mov_memq_reg(buf, rex_prefix, 0, 0x8a, src, disp, dest);
}

pub fn emit_movl_memq_reg(buf: &mut MacroAssembler, src: Reg, disp: i32, dest: Reg) {
    emit_mov_memq_reg(buf, 0, 0, 0x8b, src, disp, dest);
}

pub fn emit_movzbl_memq_reg(buf: &mut MacroAssembler, src: Reg, disp: i32, dest: Reg) {
    let src_msb = if src == RIP { 0 } else { src.msb() };

    if dest.msb() != 0 || src_msb != 0 {
        emit_rex(buf, 0, dest.msb(), 0, src_msb);
    }

    emit_op(buf, 0x0F);
    emit_op(buf, 0xB6);
    emit_membase(buf, src, disp, dest);
}

pub fn emit_movq_memq_reg(buf: &mut MacroAssembler, src: Reg, disp: i32, dest: Reg) {
    emit_mov_memq_reg(buf, 0, 1, 0x8b, src, disp, dest);
}

fn emit_mov_memq_reg(buf: &mut MacroAssembler,
                     rex_prefix: u8,
                     x64: u8,
                     opcode: u8,
                     src: Reg,
                     disp: i32,
                     dest: Reg) {
    let src_msb = if src == RIP { 0 } else { src.msb() };

    if src_msb != 0 || dest.msb() != 0 || x64 != 0 || rex_prefix != 0 {
        emit_rex(buf, x64, dest.msb(), 0, src_msb);
    }

    emit_op(buf, opcode);
    emit_membase(buf, src, disp, dest);
}

pub fn emit_movq_reg_memq(buf: &mut MacroAssembler, src: Reg, dest: Reg, disp: i32) {
    emit_mov_reg_memq(buf, 0x89, 1, src, dest, disp);
}

pub fn emit_movl_reg_memq(buf: &mut MacroAssembler, src: Reg, dest: Reg, disp: i32) {
    emit_mov_reg_memq(buf, 0x89, 0, src, dest, disp);
}

pub fn emit_movb_reg_memq(buf: &mut MacroAssembler, src: Reg, dest: Reg, disp: i32) {
    let dest_msb = if dest == RIP { 0 } else { dest.msb() };

    if dest_msb != 0 || src.msb() != 0 || (src != RAX && src != RBX && src != RCX && src != RDX) {
        emit_rex(buf, 0, src.msb(), 0, dest.msb());
    }

    emit_op(buf, 0x88);
    emit_membase(buf, dest, disp, src);
}

pub fn emit_movq_ar(buf: &mut MacroAssembler, base: Reg, index: Reg, scale: u8, dest: Reg) {
    emit_mov_ar(buf, 1, 0x8b, base, index, scale, dest);
}

pub fn emit_movl_ar(buf: &mut MacroAssembler, base: Reg, index: Reg, scale: u8, dest: Reg) {
    emit_mov_ar(buf, 0, 0x8b, base, index, scale, dest);
}

pub fn emit_movq_ra(buf: &mut MacroAssembler, src: Reg, base: Reg, index: Reg, scale: u8) {
    emit_mov_ar(buf, 1, 0x89, base, index, scale, src);
}

pub fn emit_movl_ra(buf: &mut MacroAssembler, src: Reg, base: Reg, index: Reg, scale: u8) {
    emit_mov_ar(buf, 0, 0x89, base, index, scale, src);
}

fn emit_mov_ar(buf: &mut MacroAssembler,
               x64: u8,
               opcode: u8,
               base: Reg,
               index: Reg,
               scale: u8,
               dest: Reg) {
    assert!(scale == 8 || scale == 4 || scale == 2 || scale == 1);

    if x64 != 0 || dest.msb() != 0 || index.msb() != 0 || base.msb() != 0 {
        emit_rex(buf, x64, dest.msb(), index.msb(), base.msb());
    }

    let scale = match scale {
        8 => 3,
        4 => 2,
        2 => 1,
        _ => 0,
    };

    emit_op(buf, opcode);
    emit_modrm(buf, 0b00, dest.and7(), 0b100);
    emit_sib(buf, scale, index.and7(), base.and7());
}

fn emit_mov_reg_memq(buf: &mut MacroAssembler,
                     opcode: u8,
                     x64: u8,
                     src: Reg,
                     dest: Reg,
                     disp: i32) {
    let dest_msb = if dest == RIP { 0 } else { dest.msb() };

    if dest_msb != 0 || src.msb() != 0 || x64 != 0 {
        emit_rex(buf, x64, src.msb(), 0, dest_msb);
    }

    emit_op(buf, opcode);
    emit_membase(buf, dest, disp, src);
}

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

pub fn emit_cmp_imm_reg(buf: &mut MacroAssembler, mode: MachineMode, imm: i32, reg: Reg) {
    let x64 = match mode {
        MachineMode::Int8 |
        MachineMode::Int32 => 0,
        MachineMode::Int64 => unimplemented!(),
        MachineMode::Float32 |
        MachineMode::Float64 => unreachable!(),
        MachineMode::Ptr => 1,
    };

    emit_aluq_imm_reg(buf, x64, imm, reg, 0x3d, 0b111);
}

pub fn emit_subq_imm_reg(buf: &mut MacroAssembler, imm: i32, reg: Reg) {
    emit_aluq_imm_reg(buf, 1, imm, reg, 0x2d, 0b101);
}

pub fn emit_addq_imm_reg(buf: &mut MacroAssembler, imm: i32, reg: Reg) {
    emit_aluq_imm_reg(buf, 1, imm, reg, 0x05, 0);
}

fn emit_aluq_imm_reg(buf: &mut MacroAssembler,
                     x64: u8,
                     imm: i32,
                     reg: Reg,
                     rax_opcode: u8,
                     modrm_reg: u8) {
    assert!(x64 == 0 || x64 == 1);

    if x64 != 0 || reg.msb() != 0 {
        emit_rex(buf, x64, 0, 0, reg.msb());
    }

    if fits_i8(imm) {
        emit_op(buf, 0x83);
        emit_modrm(buf, 0b11, modrm_reg, reg.and7());
        emit_u8(buf, imm as u8);

    } else if reg == RAX {
        emit_op(buf, rax_opcode);
        emit_u32(buf, imm as u32);

    } else {
        emit_op(buf, 0x81);
        emit_modrm(buf, 0b11, modrm_reg, reg.and7());
        emit_u32(buf, imm as u32);
    }
}

pub fn emit_mov_reg_reg(buf: &mut MacroAssembler, x64: u8, src: Reg, dest: Reg) {
    if x64 != 0 || src.msb() != 0 || dest.msb() != 0 {
        emit_rex(buf, x64, src.msb(), 0, dest.msb());
    }

    emit_op(buf, 0x89);
    emit_modrm(buf, 0b11, src.and7(), dest.and7());
}

pub fn emit_neg_reg(buf: &mut MacroAssembler, x64: u8, reg: Reg) {
    emit_alul_reg(buf, 0xf7, 0b11, x64, reg);
}

pub fn emit_not_reg(buf: &mut MacroAssembler, x64: u8, reg: Reg) {
    emit_alul_reg(buf, 0xf7, 0b10, x64, reg);
}

pub fn emit_not_reg_byte(buf: &mut MacroAssembler, reg: Reg) {
    emit_alul_reg(buf, 0xf6, 0b10, 0, reg);
}

fn emit_alul_reg(buf: &mut MacroAssembler, opcode: u8, modrm_reg: u8, x64: u8, reg: Reg) {
    if reg.msb() != 0 || x64 != 0 {
        emit_rex(buf, x64, 0, 0, reg.msb());
    }

    emit_op(buf, opcode);
    emit_modrm(buf, 0b11, modrm_reg, reg.and7());
}

pub fn emit_xorb_imm_reg(buf: &mut MacroAssembler, imm: u8, dest: Reg) {
    emit_alub_imm_reg(buf, 0x80, 0x34, 0b110, imm, dest);
}

pub fn emit_andb_imm_reg(buf: &mut MacroAssembler, imm: u8, dest: Reg) {
    emit_alub_imm_reg(buf, 0x80, 0x24, 0b100, imm, dest);
}

fn emit_alub_imm_reg(buf: &mut MacroAssembler,
                     opcode: u8,
                     rax_opcode: u8,
                     modrm_reg: u8,
                     imm: u8,
                     dest: Reg) {
    if dest == RAX {
        emit_op(buf, rax_opcode);
        emit_u8(buf, imm);
    } else {
        if dest.msb() != 0 || !dest.is_basic_reg() {
            emit_rex(buf, 0, 0, 0, dest.msb());
        }

        emit_op(buf, opcode);
        emit_modrm(buf, 0b11, modrm_reg, dest.and7());
        emit_u8(buf, imm);
    }
}

pub fn emit_sub_imm_mem(buf: &mut MacroAssembler, mode: MachineMode, base: Reg, imm: u8) {
    let (x64, opcode) = match mode {
        MachineMode::Ptr => (1, 0x83),
        MachineMode::Int32 => (0, 0x83),
        MachineMode::Int64 => unimplemented!(),
        MachineMode::Float32 |
        MachineMode::Float64 => unreachable!(),
        MachineMode::Int8 => (0, 0x80),
    };

    if x64 != 0 || base.msb() != 0 {
        emit_rex(buf, x64, 0, 0, base.msb());
    }

    emit_op(buf, opcode);
    emit_modrm(buf, 0b00, 0b101, base.and7());
    emit_u8(buf, imm);
}

pub fn emit_pushq_reg(buf: &mut MacroAssembler, reg: Reg) {
    if reg.msb() != 0 {
        emit_rex(buf, 0, 0, 0, 1);
    }

    emit_op(buf, 0x50 + reg.and7());
}

pub fn emit_popq_reg(buf: &mut MacroAssembler, reg: Reg) {
    if reg.msb() != 0 {
        emit_rex(buf, 0, 0, 0, 1);
    }

    emit_op(buf, 0x58 + reg.and7());
}

pub fn emit_retq(buf: &mut MacroAssembler) {
    emit_op(buf, 0xC3);
}

pub fn emit_nop(buf: &mut MacroAssembler) {
    emit_op(buf, 0x90);
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

pub fn emit_rex(buf: &mut MacroAssembler, w: u8, r: u8, x: u8, b: u8) {
    assert!(w == 0 || w == 1);
    assert!(r == 0 || r == 1);
    assert!(x == 0 || x == 1);
    assert!(b == 0 || b == 1);

    buf.emit_u8(0x4 << 4 | w << 3 | r << 2 | x << 1 | b);
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
        CondCode::UnsignedGreater => 0x87, // above
        CondCode::UnsignedGreaterEq => 0x83, // above or equal
        CondCode::UnsignedLess => 0x82, // below
        CondCode::UnsignedLessEq => 0x86, // below or equal
    };

    emit_op(buf, 0x0f);
    emit_op(buf, opcode);
    buf.emit_label(lbl);
}

pub fn emit_movsx(buf: &mut MacroAssembler, src: Reg, dest: Reg) {
    emit_rex(buf, 1, dest.msb(), 0, src.msb());

    emit_op(buf, 0x63);
    emit_modrm(buf, 0b11, dest.and7(), src.and7());
}

pub fn emit_jmp(buf: &mut MacroAssembler, lbl: Label) {
    emit_op(buf, 0xe9);
    buf.emit_label(lbl);
}

pub fn emit_testl_reg_reg(buf: &mut MacroAssembler, op1: Reg, op2: Reg) {
    if op1.msb() != 0 || op2.msb() != 0 {
        emit_rex(buf, 0, op1.msb(), 0, op2.msb());
    }

    emit_op(buf, 0x85);
    emit_modrm(buf, 0b11, op1.and7(), op2.and7());
}

pub fn testl_reg_mem(buf: &mut MacroAssembler, dest: Reg, src: Mem) {
    emit_rex_mem(buf, dest, &src);
    emit_op(buf, 0x85);
    emit_mem(buf, dest, &src);

}

fn emit_rex_mem(buf: &mut MacroAssembler, dest: Reg, src: &Mem) {
    let (base_msb, index_msb) = match src {
        &Mem::Local(_) => (RBP.msb(), 0),
        &Mem::Base(base, _) => {
            let base_msb = if base == RIP { 0 } else { base.msb() };

            (base_msb, 0)
        }

        &Mem::Index(base, index, _, _) => (base.msb(), index.msb()),
    };

    if dest.msb() != 0 || index_msb != 0 || base_msb != 0 {
        emit_rex(buf, 0, dest.msb(), index_msb, base_msb);
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
    }
}

pub fn emit_testq_reg_reg(buf: &mut MacroAssembler, op1: Reg, op2: Reg) {
    emit_rex(buf, 1, op1.msb(), 0, op2.msb());

    emit_op(buf, 0x85);
    emit_modrm(buf, 0b11, op1.and7(), op2.and7());
}

pub fn emit_add_reg_reg(buf: &mut MacroAssembler, x64: u8, src: Reg, dest: Reg) {
    if src.msb() != 0 || dest.msb() != 0 || x64 != 0 {
        emit_rex(buf, x64, src.msb(), 0, dest.msb());
    }

    emit_op(buf, 0x01);
    emit_modrm(buf, 0b11, src.and7(), dest.and7());
}

pub fn emit_sub_reg_reg(buf: &mut MacroAssembler, x64: u8, src: Reg, dest: Reg) {
    if src.msb() != 0 || dest.msb() != 0 || x64 != 0 {
        emit_rex(buf, x64, src.msb(), 0, dest.msb());
    }

    emit_op(buf, 0x29);
    emit_modrm(buf, 0b11, src.and7(), dest.and7());
}

pub fn emit_imul_reg_reg(buf: &mut MacroAssembler, x64: u8, src: Reg, dest: Reg) {
    if src.msb() != 0 || dest.msb() != 0 || x64 != 0 {
        emit_rex(buf, x64, dest.msb(), 0, src.msb());
    }

    emit_op(buf, 0x0f);
    emit_op(buf, 0xaf);
    emit_modrm(buf, 0b11, dest.and7(), src.and7());
}

pub fn emit_idiv_reg_reg(buf: &mut MacroAssembler, x64: u8, reg: Reg) {
    if reg.msb() != 0 || x64 != 0 {
        emit_rex(buf, x64, 0, 0, reg.msb());
    }

    emit_op(buf, 0xf7);
    emit_modrm(buf, 0b11, 0b111, reg.and7());
}

pub fn emit_cmp_reg_reg(buf: &mut MacroAssembler, x64: u8, src: Reg, dest: Reg) {
    emit_alu_reg_reg(buf, x64, 0x39, src, dest);
}

pub fn emit_cmp_mem_reg(buf: &mut MacroAssembler,
                        mode: MachineMode,
                        base: Reg,
                        disp: i32,
                        dest: Reg) {
    let base_msb = if base == RIP { 0 } else { base.msb() };

    let (x64, opcode) = match mode {
        MachineMode::Int8 => (0, 0x38),
        MachineMode::Int32 => (0, 0x39),
        MachineMode::Int64 => unimplemented!(),
        MachineMode::Float32 |
        MachineMode::Float64 => unreachable!(),
        MachineMode::Ptr => (1, 0x39),
    };

    if x64 != 0 || dest.msb() != 0 || base_msb != 0 {
        emit_rex(buf, x64, dest.msb(), 0, base_msb);
    }

    emit_op(buf, opcode);
    emit_membase(buf, base, disp, dest);
}

pub fn emit_mov_memindex_reg(buf: &mut MacroAssembler,
                             mode: MachineMode,
                             base: Reg,
                             index: Reg,
                             scale: i32,
                             disp: i32,
                             dest: Reg) {
    assert!(scale == 8 || scale == 4 || scale == 2 || scale == 1);
    assert!(mode.size() == scale);

    let (x64, opcode) = match mode {
        MachineMode::Int8 => (0, 0x8a),
        MachineMode::Int32 => (0, 0x8b),
        MachineMode::Int64 |
        MachineMode::Ptr => (1, 0x8b),
        MachineMode::Float32 |
        MachineMode::Float64 => unreachable!(),
    };

    if x64 != 0 || dest.msb() != 0 || index.msb() != 0 || base.msb() != 0 {
        emit_rex(buf, x64, dest.msb(), index.msb(), base.msb());
    }

    emit_op(buf, opcode);
    emit_membase_with_index_and_scale(buf, base, index, scale, disp, dest);
}

pub fn emit_movzx_memindex_byte_reg(buf: &mut MacroAssembler,
                                    x64: u8,
                                    base: Reg,
                                    index: Reg,
                                    disp: i32,
                                    dest: Reg) {
    if x64 != 0 || dest.msb() != 0 || index.msb() != 0 || base.msb() != 0 {
        emit_rex(buf, x64, dest.msb(), index.msb(), base.msb());
    }

    emit_op(buf, 0x0f);
    emit_op(buf, 0xb6);
    emit_membase_with_index_and_scale(buf, base, index, 1, disp, dest);
}

pub fn emit_mov_reg_memindex(buf: &mut MacroAssembler,
                             mode: MachineMode,
                             src: Reg,
                             base: Reg,
                             index: Reg,
                             scale: i32,
                             disp: i32) {
    assert!(scale == 8 || scale == 4 || scale == 2 || scale == 1);

    let (x64, opcode) = match mode {
        MachineMode::Int8 => (0, 0x88),
        MachineMode::Int32 => (0, 0x89),
        MachineMode::Int64 |
        MachineMode::Ptr => (1, 0x89),
        MachineMode::Float32 |
        MachineMode::Float64 => unreachable!(),
    };

    if x64 != 0 || src.msb() != 0 || index.msb() != 0 || base.msb() != 0 {
        emit_rex(buf, x64, src.msb(), index.msb(), base.msb());
    }

    emit_op(buf, opcode);
    emit_membase_with_index_and_scale(buf, base, index, scale, disp, src);
}

pub fn emit_cmp_mem_imm(buf: &mut MacroAssembler,
                        mode: MachineMode,
                        base: Reg,
                        disp: i32,
                        imm: i32) {
    let base_msb = if base == RIP { 0 } else { base.msb() };

    let opcode = if fits_i8(imm) { 0x83 } else { 0x81 };

    let (x64, opcode) = match mode {
        MachineMode::Int8 => (0, 0x80),
        MachineMode::Int32 => (0, opcode),
        MachineMode::Int64 => unimplemented!(),
        MachineMode::Ptr => (1, opcode),
        MachineMode::Float32 |
        MachineMode::Float64 => unreachable!(),
    };

    if x64 != 0 || base_msb != 0 {
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

pub fn emit_cmp_memindex_reg(buf: &mut MacroAssembler,
                             mode: MachineMode,
                             base: Reg,
                             index: Reg,
                             scale: i32,
                             disp: i32,
                             dest: Reg) {
    assert!(scale == 8 || scale == 4 || scale == 2 || scale == 1);

    let (x64, opcode) = match mode {
        MachineMode::Int8 => (0, 0x38),
        MachineMode::Int32 => (0, 0x39),
        MachineMode::Int64 => unimplemented!(),
        MachineMode::Ptr => (1, 0x39),
        MachineMode::Float32 |
        MachineMode::Float64 => unreachable!(),
    };

    if x64 != 0 || dest.msb() != 0 || index.msb() != 0 || base.msb() != 0 {
        emit_rex(buf, x64, dest.msb(), index.msb(), base.msb());
    }

    emit_op(buf, opcode);
    emit_membase_with_index_and_scale(buf, base, index, scale, disp, dest);
}

fn emit_membase_with_index_and_scale(buf: &mut MacroAssembler,
                                     base: Reg,
                                     index: Reg,
                                     scale: i32,
                                     disp: i32,
                                     dest: Reg) {
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

pub fn emit_cdq(buf: &mut MacroAssembler) {
    emit_op(buf, 0x99);
}

pub fn emit_cqo(buf: &mut MacroAssembler) {
    emit_rex(buf, 1, 0, 0, 0);
    emit_op(buf, 0x99);
}

pub fn emit_setb_reg(buf: &mut MacroAssembler, op: CondCode, reg: Reg) {
    if reg.msb() != 0 || !reg.is_basic_reg() {
        emit_rex(buf, 0, 0, 0, reg.msb());
    }

    let op = match op {
        CondCode::Less => 0x9c,
        CondCode::LessEq => 0x9e,
        CondCode::Greater => 0x9f,
        CondCode::GreaterEq => 0x9d,
        CondCode::UnsignedGreater => 0x97, // above
        CondCode::UnsignedGreaterEq => 0x93, // above or equal
        CondCode::UnsignedLess => 0x92, // below
        CondCode::UnsignedLessEq => 0x96, // below or equal
        CondCode::Zero | CondCode::Equal => 0x94,
        CondCode::NonZero | CondCode::NotEqual => 0x95,
    };

    emit_op(buf, 0x0f);
    emit_op(buf, op);
    emit_modrm(buf, 0b11, 0, reg.and7());
}

pub fn emit_setb_reg_parity(buf: &mut MacroAssembler, reg: Reg, parity: bool) {
    if reg.msb() != 0 || !reg.is_basic_reg() {
        emit_rex(buf, 0, 0, 0, reg.msb());
    }

    let opcode = if parity { 0x9a } else { 0x9b };

    emit_op(buf, 0x0f);
    emit_op(buf, opcode);
    emit_modrm(buf, 0b11, 0, reg.and7());
}

pub fn emit_movb_reg_reg(buf: &mut MacroAssembler, src: Reg, dest: Reg) {
    if src.msb() != 0 || dest.msb() != 0 || !src.is_basic_reg() {
        emit_rex(buf, 0, dest.msb(), 0, src.msb());
    }

    emit_op(buf, 0x88);
    emit_modrm(buf, 0b11, src.and7(), dest.and7());
}

pub fn emit_movzbl_reg_reg(buf: &mut MacroAssembler, src: Reg, dest: Reg) {
    if src.msb() != 0 || dest.msb() != 0 || !src.is_basic_reg() {
        emit_rex(buf, 0, dest.msb(), 0, src.msb());
    }

    emit_op(buf, 0x0f);
    emit_op(buf, 0xb6);

    emit_modrm(buf, 0b11, dest.and7(), src.and7());
}

pub fn emit_cmpb_imm_reg(buf: &mut MacroAssembler, imm: u8, dest: Reg) {
    if dest == RAX {
        emit_op(buf, 0x3c);
        emit_u8(buf, imm);
        return;
    }

    if dest.msb() != 0 || !dest.is_basic_reg() {
        emit_rex(buf, 0, 0, 0, dest.msb());
    }

    emit_op(buf, 0x80);
    emit_modrm(buf, 0b11, 0b111, dest.and7());
    emit_u8(buf, imm);
}

pub fn cmov(buf: &mut MacroAssembler, x64: u8, dest: Reg, src: Reg, cond: CondCode) {
    let opcode = match cond {
        CondCode::Zero | CondCode::Equal => 0x44,
        CondCode::NonZero | CondCode::NotEqual => 0x45,
        CondCode::Greater => 0x4F,
        CondCode::GreaterEq => 0x4D,
        CondCode::Less => 0x4C,
        CondCode::LessEq => 0x4E,
        CondCode::UnsignedGreater => 0x47, // above
        CondCode::UnsignedGreaterEq => 0x43, // above or equal
        CondCode::UnsignedLess => 0x42, // below
        CondCode::UnsignedLessEq => 0x46, // below or equal
    };

    if src.msb() != 0 || dest.msb() != 0 || x64 != 0 {
        emit_rex(buf, x64, dest.msb(), 0, src.msb());
    }

    emit_op(buf, 0x0f);
    emit_op(buf, opcode);
    emit_modrm(buf, 0b11, dest.and7(), src.and7());
}

pub fn emit_callq_reg(buf: &mut MacroAssembler, dest: Reg) {
    if dest.msb() != 0 {
        emit_rex(buf, 0, 0, 0, dest.msb());
    }

    emit_op(buf, 0xff);
    emit_modrm(buf, 0b11, 0b10, dest.and7());
}

pub fn emit_shlq_reg(buf: &mut MacroAssembler, imm: u8, dest: Reg) {
    emit_rex(buf, 1, 0, 0, dest.msb());
    emit_op(buf, 0xC1);
    emit_modrm(buf, 0b11, 0b100, dest.and7());
    emit_u8(buf, imm);
}

pub fn emit_shll_reg(buf: &mut MacroAssembler, imm: u8, dest: Reg) {
    if dest.msb() != 0 {
        emit_rex(buf, 0, 0, 0, dest.msb());
    }

    emit_op(buf, 0xC1);
    emit_modrm(buf, 0b11, 0b100, dest.and7());
    emit_u8(buf, imm);
}

pub fn emit_shl_reg_cl(buf: &mut MacroAssembler, x64: u8, dest: Reg) {
    if dest.msb() != 0 || x64 != 0 {
        emit_rex(buf, x64, 0, 0, dest.msb());
    }

    emit_op(buf, 0xD3);
    emit_modrm(buf, 0b11, 0b100, dest.and7());
}

pub fn emit_shr_reg_cl(buf: &mut MacroAssembler, x64: u8, dest: Reg) {
    if dest.msb() != 0 || x64 != 0 {
        emit_rex(buf, x64, 0, 0, dest.msb());
    }

    emit_op(buf, 0xD3);
    emit_modrm(buf, 0b11, 0b101, dest.and7());
}

pub fn emit_sar_reg_cl(buf: &mut MacroAssembler, x64: u8, dest: Reg) {
    if dest.msb() != 0 || x64 != 0 {
        emit_rex(buf, x64, 0, 0, dest.msb());
    }

    emit_op(buf, 0xD3);
    emit_modrm(buf, 0b11, 0b111, dest.and7());
}

pub fn emit_movzx_byte(buf: &mut MacroAssembler, x64: u8, src: Reg, dest: Reg) {
    if src.msb() != 0 || dest.msb() != 0 || x64 != 0 {
        emit_rex(buf, x64, dest.msb(), 0, src.msb());
    }

    emit_op(buf, 0x0f);
    emit_op(buf, 0xb6);
    emit_modrm(buf, 0b11, dest.and7(), src.and7());
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

pub fn cvtsi2ss(buf: &mut MacroAssembler, dest: FReg, x64: u8, src: Reg) {
    sse_float_freg_reg(buf, false, 0x2a, dest, x64, src);
}

pub fn cvtsi2sd(buf: &mut MacroAssembler, dest: FReg, x64: u8, src: Reg) {
    sse_float_freg_reg(buf, true, 0x2a, dest, x64, src);
}

pub fn cvttss2si(buf: &mut MacroAssembler, x64: u8, dest: Reg, src: FReg) {
    sse_float_reg_freg(buf, false, 0x2c, x64, dest, src);
}

pub fn cvttsd2si(buf: &mut MacroAssembler, x64: u8, dest: Reg, src: FReg) {
    sse_float_reg_freg(buf, true, 0x2c, x64, dest, src);
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
        emit_rex(buf, 0, dest.msb(), 0, src.msb());
    }

    emit_op(buf, 0x0f);
    emit_op(buf, op);
    emit_modrm(buf, 0b11, dest.and7(), src.and7());
}

fn sse_float_freg_mem(buf: &mut MacroAssembler, dbl: bool, op: u8, dest: FReg, src: Mem) {
    let prefix = if dbl { 0xf2 } else { 0xf3 };

    emit_op(buf, prefix);

    let (base_msb, index_msb) = match src {
        Mem::Local(_) => (RBP.msb(), 0),
        Mem::Base(base, _) => {
            let base_msb = if base == RIP { 0 } else { base.msb() };

            (base_msb, 0)
        }

        Mem::Index(base, index, _, _) => (base.msb(), index.msb()),
    };

    if dest.msb() != 0 || index_msb != 0 || base_msb != 0 {
        emit_rex(buf, 0, dest.msb(), index_msb, base_msb);
    }

    emit_op(buf, 0x0f);
    emit_op(buf, op);

    match src {
        Mem::Local(offset) => {
            emit_membase(buf, RBP, offset, Reg(dest.0));
        }

        Mem::Base(base, disp) => {
            emit_membase(buf, base, disp, Reg(dest.0));
        }

        Mem::Index(base, index, scale, disp) => {
            emit_membase_with_index_and_scale(buf, base, index, scale, disp, Reg(dest.0));
        }
    }
}

fn sse_float_freg_mem_66(buf: &mut MacroAssembler, dbl: bool, op: u8, dest: FReg, src: Mem) {
    if dbl {
        emit_op(buf, 0x66);
    }

    let (base_msb, index_msb) = match src {
        Mem::Local(_) => (RBP.msb(), 0),
        Mem::Base(base, _) => {
            let base_msb = if base == RIP { 0 } else { base.msb() };

            (base_msb, 0)
        }

        Mem::Index(base, index, _, _) => (base.msb(), index.msb()),
    };

    if dest.msb() != 0 || index_msb != 0 || base_msb != 0 {
        emit_rex(buf, 0, dest.msb(), index_msb, base_msb);
    }

    emit_op(buf, 0x0f);
    emit_op(buf, op);

    match src {
        Mem::Local(offset) => {
            emit_membase(buf, RBP, offset, Reg(dest.0));
        }

        Mem::Base(base, disp) => {
            emit_membase(buf, base, disp, Reg(dest.0));
        }

        Mem::Index(base, index, scale, disp) => {
            emit_membase_with_index_and_scale(buf, base, index, scale, disp, Reg(dest.0));
        }
    }
}

fn sse_float_freg_reg(buf: &mut MacroAssembler, dbl: bool, op: u8, dest: FReg, x64: u8, src: Reg) {
    let prefix = if dbl { 0xf2 } else { 0xf3 };

    emit_op(buf, prefix);

    if x64 != 0 || dest.msb() != 0 || src.msb() != 0 {
        emit_rex(buf, x64, dest.msb(), 0, src.msb());
    }

    emit_op(buf, 0x0f);
    emit_op(buf, op);
    emit_modrm(buf, 0b11, dest.and7(), src.and7());
}

fn sse_float_reg_freg(buf: &mut MacroAssembler, dbl: bool, op: u8, x64: u8, dest: Reg, src: FReg) {
    let prefix = if dbl { 0xf2 } else { 0xf3 };

    emit_op(buf, prefix);

    if x64 != 0 || dest.msb() != 0 || src.msb() != 0 {
        emit_rex(buf, x64, dest.msb(), 0, src.msb());
    }

    emit_op(buf, 0x0f);
    emit_op(buf, op);
    emit_modrm(buf, 0b11, dest.and7(), src.and7());
}

pub fn pxor(buf: &mut MacroAssembler, dest: FReg, src: FReg) {
    emit_op(buf, 0x66);

    if dest.msb() != 0 || src.msb() != 0 {
        emit_rex(buf, 0, dest.msb(), 0, src.msb());
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

fn sse_cmp(buf: &mut MacroAssembler, dbl: bool, dest: FReg, src: FReg) {
    if dbl {
        emit_op(buf, 0x66);
    }

    if dest.msb() != 0 || src.msb() != 0 {
        emit_rex(buf, 0, dest.msb(), 0, src.msb());
    }

    emit_op(buf, 0x0f);
    emit_op(buf, 0x2e);
    emit_modrm(buf, 0b11, dest.and7(), src.and7());
}

#[cfg(test)]
mod tests {
    use super::*;

    use baseline::codegen::CondCode;
    use cpu::*;
    use masm::MacroAssembler;
    use ty::MachineMode;

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
    fn test_emit_retq() {
        assert_emit!(0xc3; emit_retq);
    }

    #[test]
    fn test_emit_popq_reg() {
        assert_emit!(0x58; emit_popq_reg(RAX));
        assert_emit!(0x5c; emit_popq_reg(RSP));
        assert_emit!(0x41, 0x58; emit_popq_reg(R8));
        assert_emit!(0x41, 0x5F; emit_popq_reg(R15));
    }

    #[test]
    fn test_emit_pushq_reg() {
        assert_emit!(0x50; emit_pushq_reg(RAX));
        assert_emit!(0x54; emit_pushq_reg(RSP));
        assert_emit!(0x41, 0x50; emit_pushq_reg(R8));
        assert_emit!(0x41, 0x57; emit_pushq_reg(R15));
    }

    #[test]
    fn test_emit_mov_reg_reg() {
        assert_emit!(0x4c, 0x89, 0xf8; emit_mov_reg_reg(1, R15, RAX));
        assert_emit!(0x49, 0x89, 0xc7; emit_mov_reg_reg(1, RAX, R15));

        assert_emit!(0x44, 0x89, 0xf8; emit_mov_reg_reg(0, R15, RAX));
        assert_emit!(0x41, 0x89, 0xc7; emit_mov_reg_reg(0, RAX, R15));
        assert_emit!(0x89, 0xc8; emit_mov_reg_reg(0, RCX, RAX));
    }

    #[test]
    fn test_emit_movl_imm_reg() {
        assert_emit!(0xb8, 2, 0, 0, 0; emit_movl_imm_reg(2, RAX));
        assert_emit!(0x41, 0xbe, 3, 0, 0, 0; emit_movl_imm_reg(3, R14));
    }

    #[test]
    fn test_emit_movq_imm_reg() {
        assert_emit!(0x48, 0xc7, 0xc0, 1, 0, 0, 0; emit_movq_imm_reg(1, RAX));
        assert_emit!(0x49, 0xc7, 0xc7, 0xFF, 0xFF, 0xFF, 0xFF; emit_movq_imm_reg(-1, R15));
    }

    #[test]
    fn test_emit_subq_imm_reg() {
        assert_emit!(0x48, 0x83, 0xe8, 0x11; emit_subq_imm_reg(0x11, RAX));
        assert_emit!(0x49, 0x83, 0xef, 0x11; emit_subq_imm_reg(0x11, R15));
        assert_emit!(0x48, 0x2d, 0x11, 0x22, 0, 0; emit_subq_imm_reg(0x2211, RAX));
        assert_emit!(0x48, 0x81, 0xe9, 0x11, 0x22, 0, 0; emit_subq_imm_reg(0x2211, RCX));
        assert_emit!(0x49, 0x81, 0xef, 0x11, 0x22, 0, 0; emit_subq_imm_reg(0x2211, R15));
    }

    #[test]
    fn test_emit_addq_imm_reg() {
        assert_emit!(0x48, 0x83, 0xc0, 0x11; emit_addq_imm_reg(0x11, RAX));
        assert_emit!(0x49, 0x83, 0xc7, 0x11; emit_addq_imm_reg(0x11, R15));
        assert_emit!(0x48, 0x05, 0x11, 0x22, 0, 0; emit_addq_imm_reg(0x2211, RAX));
        assert_emit!(0x48, 0x81, 0xc1, 0x11, 0x22, 0, 0; emit_addq_imm_reg(0x2211, RCX));
        assert_emit!(0x49, 0x81, 0xc7, 0x11, 0x22, 0, 0; emit_addq_imm_reg(0x2211, R15));
    }

    #[test]
    fn test_emit_testl_reg_reg() {
        assert_emit!(0x85, 0xc0; emit_testl_reg_reg(RAX, RAX));
        assert_emit!(0x85, 0xc6; emit_testl_reg_reg(RAX, RSI));
        assert_emit!(0x41, 0x85, 0xc7; emit_testl_reg_reg(RAX, R15));
    }

    #[test]
    fn test_testl_reg_mem() {
        assert_emit!(0x85, 0x05, 0xf6, 0xff, 0xff, 0xff; testl_reg_mem(RAX, Mem::Base(RIP, -10)));
        assert_emit!(0x44, 0x85, 0x3d, 0xf6, 0xff, 0xff, 0xff; testl_reg_mem(R15, Mem::Base(RIP, -10)));
        assert_emit!(0x44, 0x85, 0x79, 0xf6; testl_reg_mem(R15, Mem::Base(RCX, -10)));
    }

    #[test]
    fn test_emit_testq_reg_reg() {
        assert_emit!(0x48, 0x85, 0xc0; emit_testq_reg_reg(RAX, RAX));
        assert_emit!(0x48, 0x85, 0xc6; emit_testq_reg_reg(RAX, RSI));
        assert_emit!(0x49, 0x85, 0xc7; emit_testq_reg_reg(RAX, R15));
    }

    #[test]
    fn test_emit_jcc_zero() {
        let mut buf = MacroAssembler::new();
        let lbl = buf.create_label();
        emit_jcc(&mut buf, CondCode::Zero, lbl);
        emit_nop(&mut buf);
        buf.bind_label(lbl);
        assert_eq!(vec![0x0f, 0x84, 1, 0, 0, 0, 0x90], buf.data());
    }

    #[test]
    fn test_emit_jcc_non_zero() {
        let mut buf = MacroAssembler::new();
        let lbl = buf.create_label();
        emit_jcc(&mut buf, CondCode::NonZero, lbl);
        emit_nop(&mut buf);
        buf.bind_label(lbl);
        assert_eq!(vec![0x0f, 0x85, 1, 0, 0, 0, 0x90], buf.data());
    }

    #[test]
    fn test_emit_jcc_greater() {
        let mut buf = MacroAssembler::new();
        let lbl = buf.create_label();
        emit_jcc(&mut buf, CondCode::Greater, lbl);
        emit_nop(&mut buf);
        buf.bind_label(lbl);
        assert_eq!(vec![0x0f, 0x8F, 1, 0, 0, 0, 0x90], buf.data());
    }

    #[test]
    fn test_emit_jcc_greater_or_equal() {
        let mut buf = MacroAssembler::new();
        let lbl = buf.create_label();
        emit_jcc(&mut buf, CondCode::GreaterEq, lbl);
        emit_nop(&mut buf);
        buf.bind_label(lbl);
        assert_eq!(vec![0x0f, 0x8D, 1, 0, 0, 0, 0x90], buf.data());
    }

    #[test]
    fn test_emit_jcc_less() {
        let mut buf = MacroAssembler::new();
        let lbl = buf.create_label();
        emit_jcc(&mut buf, CondCode::Less, lbl);
        emit_nop(&mut buf);
        buf.bind_label(lbl);
        assert_eq!(vec![0x0f, 0x8C, 1, 0, 0, 0, 0x90], buf.data());
    }

    #[test]
    fn test_emit_jcc_less_or_equal() {
        let mut buf = MacroAssembler::new();
        let lbl = buf.create_label();
        emit_jcc(&mut buf, CondCode::LessEq, lbl);
        emit_nop(&mut buf);
        buf.bind_label(lbl);
        assert_eq!(vec![0x0f, 0x8E, 1, 0, 0, 0, 0x90], buf.data());
    }

    #[test]
    fn test_emit_jcc_unsigned_greater() {
        let mut buf = MacroAssembler::new();
        let lbl = buf.create_label();
        emit_jcc(&mut buf, CondCode::UnsignedGreater, lbl);
        emit_nop(&mut buf);
        buf.bind_label(lbl);
        assert_eq!(vec![0x0f, 0x87, 1, 0, 0, 0, 0x90], buf.data());
    }

    #[test]
    fn test_emit_jcc_unsigned_greater_or_equal() {
        let mut buf = MacroAssembler::new();
        let lbl = buf.create_label();
        emit_jcc(&mut buf, CondCode::UnsignedGreaterEq, lbl);
        emit_nop(&mut buf);
        buf.bind_label(lbl);
        assert_eq!(vec![0x0f, 0x83, 1, 0, 0, 0, 0x90], buf.data());
    }

    #[test]
    fn test_emit_jcc_unsigned_less() {
        let mut buf = MacroAssembler::new();
        let lbl = buf.create_label();
        emit_jcc(&mut buf, CondCode::UnsignedLess, lbl);
        emit_nop(&mut buf);
        buf.bind_label(lbl);
        assert_eq!(vec![0x0f, 0x82, 1, 0, 0, 0, 0x90], buf.data());
    }

    #[test]
    fn test_emit_jcc_unsigned_less_or_equal() {
        let mut buf = MacroAssembler::new();
        let lbl = buf.create_label();
        emit_jcc(&mut buf, CondCode::UnsignedLessEq, lbl);
        emit_nop(&mut buf);
        buf.bind_label(lbl);
        assert_eq!(vec![0x0f, 0x86, 1, 0, 0, 0, 0x90], buf.data());
    }

    #[test]
    fn test_emit_jmp() {
        let mut buf = MacroAssembler::new();
        let lbl = buf.create_label();
        emit_jmp(&mut buf, lbl);
        emit_nop(&mut buf);
        buf.bind_label(lbl);
        assert_eq!(vec![0xe9, 1, 0, 0, 0, 0x90], buf.data());
    }

    #[test]
    fn test_emit_movl_memq_reg() {
        assert_emit!(0x8b, 0x44, 0x24, 1; emit_movl_memq_reg(RSP, 1, RAX));
        assert_emit!(0x8b, 0x04, 0x24; emit_movl_memq_reg(RSP, 0, RAX));

        assert_emit!(0x44, 0x8b, 0x45, 0; emit_movl_memq_reg(RBP, 0, R8));

        assert_emit!(0x8b, 0x05, 0, 0, 0, 0; emit_movl_memq_reg(RIP, 0, RAX));
        assert_emit!(0x8b, 0x0d, 0, 0, 0, 0; emit_movl_memq_reg(RIP, 0, RCX));
    }

    #[test]
    fn test_emit_movzbl_memq_reg() {
        assert_emit!(0x0f, 0xb6, 0x45, 0; emit_movzbl_memq_reg(RBP, 0, RAX));
        assert_emit!(0x0F, 0xB6, 0x71, 0x11; emit_movzbl_memq_reg(RCX, 0x11, RSI));

        assert_emit!(0x0F, 0xB6, 0x3D, 0x00, 0x00, 0x00, 0x00;
            emit_movzbl_memq_reg(RIP, 0, RDI));

        assert_emit!(0x44, 0x0F, 0xB6, 0x94, 0x24, 0x22, 0x11, 0, 0;
            emit_movzbl_memq_reg(RSP, 0x1122, R10));
    }

    #[test]
    fn test_emit_movq_memq_reg() {
        assert_emit!(0x48, 0x8b, 0x44, 0x24, 1; emit_movq_memq_reg(RSP, 1, RAX));

        assert_emit!(0x48, 0x8b, 0x05, 0xff, 0xff, 0xff, 0xff; emit_movq_memq_reg(RIP, -1, RAX));
        assert_emit!(0x48, 0x8b, 0x05, 0, 0, 0, 0; emit_movq_memq_reg(RIP, 0, RAX));
        assert_emit!(0x48, 0x8b, 0x05, 1, 0, 0, 0; emit_movq_memq_reg(RIP, 1, RAX));
        assert_emit!(0x48, 0x8b, 0; emit_movq_memq_reg(RAX, 0, RAX));
    }

    #[test]
    fn test_emit_movb_memq_reg() {
        assert_emit!(0x8a, 0x45, 1; emit_movb_memq_reg(RBP, 1, RAX));
        assert_emit!(0x8a, 0x44, 0x24, 1; emit_movb_memq_reg(RSP, 1, RAX));
        assert_emit!(0x8a, 0x44, 0x24, 0xff; emit_movb_memq_reg(RSP, -1, RAX));
        assert_emit!(0x8a, 0x5d, 1; emit_movb_memq_reg(RBP, 1, RBX));
        assert_emit!(0x8a, 0x4d, 1; emit_movb_memq_reg(RBP, 1, RCX));
        assert_emit!(0x8a, 0x55, 1; emit_movb_memq_reg(RBP, 1, RDX));
        assert_emit!(0x44, 0x8a, 0x7d, 1; emit_movb_memq_reg(RBP, 1, R15));
        assert_emit!(0x40, 0x8a, 0x75, 1; emit_movb_memq_reg(RBP, 1, RSI));
        assert_emit!(0x40, 0x8a, 0x7d, 1; emit_movb_memq_reg(RBP, 1, RDI));
    }

    #[test]
    fn test_emit_movl_reg_memq() {
        assert_emit!(0x89, 0x0d, 0, 0, 0, 0; emit_movl_reg_memq(RCX, RIP, 0));
        assert_emit!(0x89, 0x48, 3; emit_movl_reg_memq(RCX, RAX, 3));
    }

    #[test]
    fn test_emit_movq_reg_memq() {
        assert_emit!(0x48, 0x89, 0x0d, 0, 0, 0, 0; emit_movq_reg_memq(RCX, RIP, 0));
        assert_emit!(0x48, 0x89, 0x48, 3; emit_movq_reg_memq(RCX, RAX, 3));
        assert_emit!(0x4c, 0x89, 0x42, 0x08; emit_movq_reg_memq(R8, RDX, 8));
        assert_emit!(0x4d, 0x89, 0x42, 0x08; emit_movq_reg_memq(R8, R10, 8));
        assert_emit!(0x49, 0x89, 0x42, 0x08; emit_movq_reg_memq(RAX, R10, 8));
        assert_emit!(0x48, 0x89, 0x42, 0x08; emit_movq_reg_memq(RAX, RDX, 8));
    }

    #[test]
    fn test_emit_movb_reg_memq() {
        assert_emit!(0x88, 0x0d, 0, 0, 0, 0; emit_movb_reg_memq(RCX, RIP, 0));
        assert_emit!(0x88, 0x48, 3; emit_movb_reg_memq(RCX, RAX, 3));
        assert_emit!(0x40, 0x88, 0x75, 0xFF; emit_movb_reg_memq(RSI, RBP, -1));

        assert_emit!(0x88, 0x45, 0x01; emit_movb_reg_memq(RAX, RBP, 1));
        assert_emit!(0x88, 0x44, 0x24, 0x01; emit_movb_reg_memq(RAX, RSP, 1));
        assert_emit!(0x88, 0x5d, 0x01; emit_movb_reg_memq(RBX, RBP, 1));
        assert_emit!(0x88, 0x4d, 0x01; emit_movb_reg_memq(RCX, RBP, 1));
        assert_emit!(0x88, 0x55, 0x01; emit_movb_reg_memq(RDX, RBP, 1));

        assert_emit!(0x44, 0x88, 0x42, 0x08; emit_movb_reg_memq(R8, RDX, 8));
        assert_emit!(0x45, 0x88, 0x42, 0x08; emit_movb_reg_memq(R8, R10, 8));
        assert_emit!(0x41, 0x88, 0x42, 0x08; emit_movb_reg_memq(RAX, R10, 8));
        assert_emit!(0x88, 0x42, 0x08; emit_movb_reg_memq(RAX, RDX, 8));
    }

    #[test]
    fn test_neg_reg() {
        assert_emit!(0xf7, 0xd8; emit_neg_reg(0, RAX));
        assert_emit!(0x41, 0xf7, 0xdf; emit_neg_reg(0, R15));

        assert_emit!(0x48, 0xf7, 0xd8; emit_neg_reg(1, RAX));
        assert_emit!(0x49, 0xf7, 0xdf; emit_neg_reg(1, R15));
    }

    #[test]
    fn test_not_reg() {
        assert_emit!(0xf7, 0xd0; emit_not_reg(0, RAX));
        assert_emit!(0x41, 0xf7, 0xd7; emit_not_reg(0, R15));

        assert_emit!(0x48, 0xf7, 0xd0; emit_not_reg(1, RAX));
        assert_emit!(0x49, 0xf7, 0xd7; emit_not_reg(1, R15));
    }

    #[test]
    fn test_xorb_imm_reg() {
        assert_emit!(0x34, 1; emit_xorb_imm_reg(1, RAX));
        assert_emit!(0x80, 0xf3, 1; emit_xorb_imm_reg(1, RBX));
        assert_emit!(0x80, 0xf1, 1; emit_xorb_imm_reg(1, RCX));
        assert_emit!(0x80, 0xf2, 1; emit_xorb_imm_reg(1, RDX));
        assert_emit!(0x40, 0x80, 0xf6, 1; emit_xorb_imm_reg(1, RSI));
        assert_emit!(0x40, 0x80, 0xf7, 1; emit_xorb_imm_reg(1, RDI));

        assert_emit!(0x41, 0x80, 0xf0, 3; emit_xorb_imm_reg(3, R8));
        assert_emit!(0x41, 0x80, 0xf7, 4; emit_xorb_imm_reg(4, R15));
    }

    #[test]
    fn test_andb_imm_reg() {
        assert_emit!(0x24, 1; emit_andb_imm_reg(1, RAX));
        assert_emit!(0x80, 0xe1, 2; emit_andb_imm_reg(2, RCX));
        assert_emit!(0x41, 0x80, 0xe0, 3; emit_andb_imm_reg(3, R8));
        assert_emit!(0x41, 0x80, 0xe7, 4; emit_andb_imm_reg(4, R15));
    }

    #[test]
    fn test_add_reg_reg() {
        assert_emit!(0x01, 0xd8; emit_add_reg_reg(0, RBX, RAX));
        assert_emit!(0x44, 0x01, 0xf9; emit_add_reg_reg(0, R15, RCX));

        assert_emit!(0x48, 0x01, 0xD8; emit_add_reg_reg(1, RBX, RAX));
        assert_emit!(0x4C, 0x01, 0xE0; emit_add_reg_reg(1, R12, RAX));
        assert_emit!(0x49, 0x01, 0xC4; emit_add_reg_reg(1, RAX, R12));
        assert_emit!(0x49, 0x01, 0xE7; emit_add_reg_reg(1, RSP, R15));
    }

    #[test]
    fn test_sub_reg_reg() {
        assert_emit!(0x29, 0xd8; emit_sub_reg_reg(0, RBX, RAX));
        assert_emit!(0x44, 0x29, 0xf9; emit_sub_reg_reg(0, R15, RCX));
        assert_emit!(0x48, 0x29, 0xd8; emit_sub_reg_reg(1, RBX, RAX));
        assert_emit!(0x4c, 0x29, 0xf9; emit_sub_reg_reg(1, R15, RCX));
    }

    #[test]
    fn test_imul_reg_reg() {
        assert_emit!(0x0f, 0xaf, 0xc3; emit_imul_reg_reg(0, RBX, RAX));
        assert_emit!(0x41, 0x0f, 0xaf, 0xcf; emit_imul_reg_reg(0, R15, RCX));

        assert_emit!(0x48, 0x0f, 0xaf, 0xc3; emit_imul_reg_reg(1, RBX, RAX));
        assert_emit!(0x49, 0x0f, 0xaf, 0xcf; emit_imul_reg_reg(1, R15, RCX));
    }

    #[test]
    fn test_idiv_reg_reg() {
        assert_emit!(0xf7, 0xf8; emit_idiv_reg_reg(0, RAX));
        assert_emit!(0x41, 0xf7, 0xff; emit_idiv_reg_reg(0, R15));

        assert_emit!(0x48, 0xf7, 0xf8; emit_idiv_reg_reg(1, RAX));
        assert_emit!(0x49, 0xf7, 0xff; emit_idiv_reg_reg(1, R15));
    }

    #[test]
    fn test_cdq_cqo() {
        assert_emit!(0x99; emit_cdq);
        assert_emit!(0x48, 0x99; emit_cqo);
    }

    #[test]
    fn test_orl_reg_reg() {
        assert_emit!(0x44, 0x09, 0xf8; emit_or_reg_reg(0, R15, RAX));
        assert_emit!(0x09, 0xc8; emit_or_reg_reg(0, RCX, RAX));
        assert_emit!(0x41, 0x09, 0xc7; emit_or_reg_reg(0, RAX, R15));

        assert_emit!(0x4c, 0x09, 0xf8; emit_or_reg_reg(1, R15, RAX));
        assert_emit!(0x48, 0x09, 0xc8; emit_or_reg_reg(1, RCX, RAX));
        assert_emit!(0x49, 0x09, 0xc7; emit_or_reg_reg(1, RAX, R15));
    }

    #[test]
    fn test_and_reg_reg() {
        assert_emit!(0x44, 0x21, 0xf8; emit_and_reg_reg(0, R15, RAX));
        assert_emit!(0x21, 0xc8; emit_and_reg_reg(0, RCX, RAX));
        assert_emit!(0x41, 0x21, 0xc7; emit_and_reg_reg(0, RAX, R15));
    }

    #[test]
    fn test_xor_reg_reg() {
        assert_emit!(0x44, 0x31, 0xf8; emit_xor_reg_reg(0, R15, RAX));
        assert_emit!(0x31, 0xc8; emit_xor_reg_reg(0, RCX, RAX));
        assert_emit!(0x41, 0x31, 0xc7; emit_xor_reg_reg(0, RAX, R15));
    }

    #[test]
    fn test_cmp_reg_reg() {
        assert_emit!(0x44, 0x39, 0xf8; emit_cmp_reg_reg(0, R15, RAX));
        assert_emit!(0x41, 0x39, 0xdf; emit_cmp_reg_reg(0, RBX, R15));
        assert_emit!(0x39, 0xd8; emit_cmp_reg_reg(0, RBX, RAX));

        assert_emit!(0x4C, 0x39, 0xf8; emit_cmp_reg_reg(1, R15, RAX));
        assert_emit!(0x49, 0x39, 0xdf; emit_cmp_reg_reg(1, RBX, R15));
        assert_emit!(0x48, 0x39, 0xd8; emit_cmp_reg_reg(1, RBX, RAX));
    }

    #[test]
    fn test_setb_reg() {
        assert_emit!(0x0f, 0x94, 0xc0; emit_setb_reg(CondCode::Equal, RAX));
        assert_emit!(0x41, 0x0f, 0x95, 0xc7; emit_setb_reg(CondCode::NotEqual, R15));
        assert_emit!(0x0f, 0x9d, 0xc1; emit_setb_reg(CondCode::GreaterEq, RCX));
        assert_emit!(0x0f, 0x9f, 0xc2; emit_setb_reg(CondCode::Greater, RDX));
        assert_emit!(0x40, 0x0f, 0x9e, 0xc6; emit_setb_reg(CondCode::LessEq, RSI));
        assert_emit!(0x40, 0x0f, 0x9c, 0xc7; emit_setb_reg(CondCode::Less, RDI));
    }

    #[test]
    fn test_movb_reg_reg() {
        assert_emit!(0x88, 0xd8; emit_movb_reg_reg(RBX, RAX));
        assert_emit!(0x88, 0xd1; emit_movb_reg_reg(RDX, RCX));
        assert_emit!(0x45, 0x88, 0xd1; emit_movb_reg_reg(R10, R9));
        assert_emit!(0x40, 0x88, 0xfe; emit_movb_reg_reg(RDI, RSI));
        assert_emit!(0x45, 0x88, 0xf7; emit_movb_reg_reg(R14, R15));
    }

    #[test]
    fn test_movzbl_reg_reg() {
        assert_emit!(0x0f, 0xb6, 0xc0; emit_movzbl_reg_reg(RAX, RAX));
        assert_emit!(0x41, 0x0f, 0xb6, 0xc7; emit_movzbl_reg_reg(R15, RAX));
        assert_emit!(0x44, 0x0f, 0xb6, 0xfb; emit_movzbl_reg_reg(RBX, R15));
        assert_emit!(0x40, 0x0f, 0xb6, 0xce; emit_movzbl_reg_reg(RSI, RCX));
    }

    #[test]
    fn test_emit_cmpb_imm_reg() {
        assert_emit!(0x3c, 0; emit_cmpb_imm_reg(0, RAX));
        assert_emit!(0x80, 0xf9, 0; emit_cmpb_imm_reg(0, RCX));
        assert_emit!(0x41, 0x80, 0xff, 0; emit_cmpb_imm_reg(0, R15));
        assert_emit!(0x40, 0x80, 0xfe, 0; emit_cmpb_imm_reg(0, RSI));
    }

    #[test]
    fn test_callq_reg() {
        assert_emit!(0xff, 0xd0; emit_callq_reg(RAX));
        assert_emit!(0x41, 0xff, 0xd7; emit_callq_reg(R15));
    }

    #[test]
    fn test_movq_ar() {
        assert_emit!(0x48, 0x8b, 0x0C, 0xD8; emit_movq_ar(RAX, RBX, 8, RCX));
        assert_emit!(0x48, 0x8b, 0x1C, 0x81; emit_movq_ar(RCX, RAX, 4, RBX));
        assert_emit!(0x48, 0x8b, 0x04, 0x4B; emit_movq_ar(RBX, RCX, 2, RAX));
        assert_emit!(0x4D, 0x8b, 0x3C, 0x03; emit_movq_ar(R11, RAX, 1, R15));
    }

    #[test]
    fn test_movl_ar() {
        assert_emit!(0x8b, 0x0c, 0xd8; emit_movl_ar(RAX, RBX, 8, RCX));
        assert_emit!(0x8b, 0x1C, 0x81; emit_movl_ar(RCX, RAX, 4, RBX));
        assert_emit!(0x8b, 0x04, 0x4B; emit_movl_ar(RBX, RCX, 2, RAX));
        assert_emit!(0x45, 0x8b, 0x3C, 0x03; emit_movl_ar(R11, RAX, 1, R15));
    }

    #[test]
    fn test_movq_ra() {
        assert_emit!(0x48, 0x89, 0x0C, 0xD8; emit_movq_ra(RCX, RAX, RBX, 8));
        assert_emit!(0x48, 0x89, 0x1C, 0x81; emit_movq_ra(RBX, RCX, RAX, 4));
        assert_emit!(0x48, 0x89, 0x04, 0x4B; emit_movq_ra(RAX, RBX, RCX, 2));
        assert_emit!(0x4D, 0x89, 0x3C, 0x03; emit_movq_ra(R15, R11, RAX, 1));
    }

    #[test]
    fn test_movl_ra() {
        assert_emit!(0x89, 0x0c, 0xd8; emit_movl_ra(RCX, RAX, RBX, 8));
        assert_emit!(0x89, 0x1C, 0x81; emit_movl_ra(RBX, RCX, RAX, 4));
        assert_emit!(0x89, 0x04, 0x4B; emit_movl_ra(RAX, RBX, RCX, 2));
        assert_emit!(0x45, 0x89, 0x3C, 0x03; emit_movl_ra(R15, R11, RAX, 1));
    }

    #[test]
    fn test_shl_reg_cl() {
        assert_emit!(0xD3, 0xE0; emit_shl_reg_cl(0, RAX));
        assert_emit!(0x41, 0xD3, 0xE1; emit_shl_reg_cl(0, R9));

        assert_emit!(0x48, 0xD3, 0xE0; emit_shl_reg_cl(1, RAX));
        assert_emit!(0x49, 0xD3, 0xE1; emit_shl_reg_cl(1, R9));
    }

    #[test]
    fn test_shr_reg_reg() {
        assert_emit!(0xD3, 0xE8; emit_shr_reg_cl(0, RAX));
        assert_emit!(0x41, 0xD3, 0xE9; emit_shr_reg_cl(0, R9));

        assert_emit!(0x48, 0xD3, 0xE8; emit_shr_reg_cl(1, RAX));
        assert_emit!(0x49, 0xD3, 0xE9; emit_shr_reg_cl(1, R9));
    }

    #[test]
    fn test_sar_reg_reg() {
        assert_emit!(0xD3, 0xF8; emit_sar_reg_cl(0, RAX));
        assert_emit!(0x41, 0xD3, 0xF9; emit_sar_reg_cl(0, R9));

        assert_emit!(0x48, 0xD3, 0xF8; emit_sar_reg_cl(1, RAX));
        assert_emit!(0x49, 0xD3, 0xF9; emit_sar_reg_cl(1, R9));
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
    fn test_sub_imm_mem() {
        assert_emit!(0x83, 0x28, 1; emit_sub_imm_mem(MachineMode::Int32, RAX, 1));
        assert_emit!(0x48, 0x83, 0x28, 1; emit_sub_imm_mem(MachineMode::Ptr, RAX, 1));
        assert_emit!(0x49, 0x83, 0x29, 1; emit_sub_imm_mem(MachineMode::Ptr, R9, 1));
        assert_emit!(0x49, 0x83, 0x28, 1; emit_sub_imm_mem(MachineMode::Ptr, R8, 1));
        assert_emit!(0x41, 0x80, 0x28, 1; emit_sub_imm_mem(MachineMode::Int8, R8, 1));
    }

    #[test]
    fn test_mov_memindex_reg() {
        assert_emit!(0x48, 0x8b, 0x54, 0xc8, 0x0c;
            emit_mov_memindex_reg(MachineMode::Ptr, RAX, RCX, 8, 12, RDX));
        assert_emit!(0x8b, 0x54, 0x88, 0x0c;
            emit_mov_memindex_reg(MachineMode::Int32, RAX, RCX, 4, 12, RDX));
        assert_emit!(0x8a, 0x54, 0x08, 0x0c;
            emit_mov_memindex_reg(MachineMode::Int8, RAX, RCX, 1, 12, RDX));
        assert_emit!(0x8a, 0x44, 0x08, 0x20;
            emit_mov_memindex_reg(MachineMode::Int8, RAX, RCX, 1, 0x20, RAX));

        assert_emit!(0x4f, 0x8b, 0x6c, 0xfe, 0x10;
            emit_mov_memindex_reg(MachineMode::Ptr, R14, R15, 8, 16, R13));
    }

    #[test]
    fn test_emit_movzx_memindex_byte_reg() {
        assert_emit!(0x0f, 0xb6, 0x44, 0x08, 0x0c;
            emit_movzx_memindex_byte_reg(0, RAX, RCX, 12, RAX));
        assert_emit!(0x48, 0x0f, 0xb6, 0x44, 0x08, 0x0c;
            emit_movzx_memindex_byte_reg(1, RAX, RCX, 12, RAX));
    }

    #[test]
    fn test_mov_reg_memindex() {
        assert_emit!(0x48, 0x89, 0x54, 0x88, 0x0c;
            emit_mov_reg_memindex(MachineMode::Ptr, RDX, RAX, RCX, 4, 12));
        assert_emit!(0x89, 0x54, 0x88, 0x0c;
            emit_mov_reg_memindex(MachineMode::Int32, RDX, RAX, RCX, 4, 12));
        assert_emit!(0x88, 0x54, 0x88, 0x0c;
            emit_mov_reg_memindex(MachineMode::Int8, RDX, RAX, RCX, 4, 12));

        assert_emit!(0x4f, 0x89, 0x6c, 0xfe, 0x10;
            emit_mov_reg_memindex(MachineMode::Ptr, R13, R14, R15, 8, 16));
    }

    #[test]
    fn test_cmp_reg_imm() {
        assert_emit!(0x48, 0x83, 0xf8, 0; emit_cmp_imm_reg(MachineMode::Ptr, 0, RAX));
        assert_emit!(0x83, 0xf8, 0; emit_cmp_imm_reg(MachineMode::Int32, 0, RAX));
        assert_emit!(0x49, 0x83, 0xff, 0; emit_cmp_imm_reg(MachineMode::Ptr, 0, R15));
        assert_emit!(0x41, 0x83, 0xff, 0; emit_cmp_imm_reg(MachineMode::Int32, 0, R15));
        assert_emit!(0x49, 0x83, 0xf9, 0; emit_cmp_imm_reg(MachineMode::Ptr, 0, R9));
        assert_emit!(0x41, 0x83, 0xf9, 0; emit_cmp_imm_reg(MachineMode::Int32, 0, R9));
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
    fn test_emit_mov_imm64() {
        assert_emit!(0x48, 0xb8, 0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11;
            emit_movq_imm64_reg(0x1122334455667788, RAX));
        assert_emit!(0x49, 0xbf, 0, 0x55, 0x44, 0x33, 0x22, 0x11, 0, 0;
            emit_movq_imm64_reg(0x0000112233445500, R15));
        assert_emit!(0x48, 0xbc, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11, 0;
            emit_movq_imm64_reg(0x0011223344556677, RSP));
    }

    #[test]
    fn test_emit_not_reg() {
        assert_emit!(0xf6, 0xd0; emit_not_reg_byte(RAX));
        assert_emit!(0x41, 0xf6, 0xd1; emit_not_reg_byte(R9));
        assert_emit!(0x41, 0xf6, 0xd7; emit_not_reg_byte(R15));
    }

    #[test]
    fn test_emit_movzx_byte() {
        assert_emit!(0x0f, 0xb6, 0xc0; emit_movzx_byte(0, RAX, RAX));
        assert_emit!(0x48, 0x0f, 0xb6, 0xc0; emit_movzx_byte(1, RAX, RAX));
        assert_emit!(0x44, 0x0f, 0xb6, 0xf9; emit_movzx_byte(0, RCX, R15));
        assert_emit!(0x4d, 0x0f, 0xb6, 0xf9; emit_movzx_byte(1, R9, R15));
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
        assert_emit!(0xf3, 0x0f, 0x2a, 0xc1; cvtsi2ss(XMM0, 0, RCX));
        assert_emit!(0xf3, 0x41, 0x0f, 0x2a, 0xdf; cvtsi2ss(XMM3, 0, R15));
        assert_emit!(0xf3, 0x44, 0x0f, 0x2a, 0xc4; cvtsi2ss(XMM8, 0, RSP));

        assert_emit!(0xf3, 0x48, 0x0f, 0x2a, 0xc1; cvtsi2ss(XMM0, 1, RCX));
        assert_emit!(0xf3, 0x49, 0x0f, 0x2a, 0xdf; cvtsi2ss(XMM3, 1, R15));
        assert_emit!(0xf3, 0x4c, 0x0f, 0x2a, 0xc4; cvtsi2ss(XMM8, 1, RSP));
    }

    #[test]
    fn test_cvtsi2sd() {
        assert_emit!(0xf2, 0x0f, 0x2a, 0xc1; cvtsi2sd(XMM0, 0, RCX));
        assert_emit!(0xf2, 0x41, 0x0f, 0x2a, 0xdf; cvtsi2sd(XMM3, 0, R15));
        assert_emit!(0xf2, 0x44, 0x0f, 0x2a, 0xc4; cvtsi2sd(XMM8, 0, RSP));

        assert_emit!(0xf2, 0x48, 0x0f, 0x2a, 0xc1; cvtsi2sd(XMM0, 1, RCX));
        assert_emit!(0xf2, 0x49, 0x0f, 0x2a, 0xdf; cvtsi2sd(XMM3, 1, R15));
        assert_emit!(0xf2, 0x4c, 0x0f, 0x2a, 0xc4; cvtsi2sd(XMM8, 1, RSP));
    }

    #[test]
    fn test_cvttss2si() {
        assert_emit!(0xf3, 0x0f, 0x2c, 0xc8; cvttss2si(0, RCX, XMM0));
        assert_emit!(0xf3, 0x44, 0x0f, 0x2c, 0xfb; cvttss2si(0, R15, XMM3));
        assert_emit!(0xf3, 0x41, 0x0f, 0x2c, 0xe0; cvttss2si(0, RSP, XMM8));

        assert_emit!(0xf3, 0x48, 0x0f, 0x2c, 0xc8; cvttss2si(1, RCX, XMM0));
        assert_emit!(0xf3, 0x4c, 0x0f, 0x2c, 0xfb; cvttss2si(1, R15, XMM3));
        assert_emit!(0xf3, 0x49, 0x0f, 0x2c, 0xe0; cvttss2si(1, RSP, XMM8));
    }

    #[test]
    fn test_cvttsd2si() {
        assert_emit!(0xf2, 0x0f, 0x2c, 0xc8; cvttsd2si(0, RCX, XMM0));
        assert_emit!(0xf2, 0x44, 0x0f, 0x2c, 0xfb; cvttsd2si(0, R15, XMM3));
        assert_emit!(0xf2, 0x41, 0x0f, 0x2c, 0xe0; cvttsd2si(0, RSP, XMM8));

        assert_emit!(0xf2, 0x48, 0x0f, 0x2c, 0xc8; cvttsd2si(1, RCX, XMM0));
        assert_emit!(0xf2, 0x4c, 0x0f, 0x2c, 0xfb; cvttsd2si(1, R15, XMM3));
        assert_emit!(0xf2, 0x49, 0x0f, 0x2c, 0xe0; cvttsd2si(1, RSP, XMM8));
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
    fn test_cmov() {
        assert_emit!(0x44, 0x0f, 0x44, 0xf8; cmov(0, R15, RAX, CondCode::Equal));
        assert_emit!(0x41, 0x0f, 0x45, 0xc5; cmov(0, RAX, R13, CondCode::NotEqual));
        assert_emit!(0x48, 0x0f, 0x4f, 0xc1; cmov(1, RAX, RCX, CondCode::Greater));
    }

    #[test]
    fn test_setb_reg_parity() {
        assert_emit!(0x0f, 0x9a, 0xc0; emit_setb_reg_parity(RAX, true));
        assert_emit!(0x0f, 0x9a, 0xc1; emit_setb_reg_parity(RCX, true));
        assert_emit!(0x0f, 0x9a, 0xc2; emit_setb_reg_parity(RDX, true));
        assert_emit!(0x0f, 0x9a, 0xc3; emit_setb_reg_parity(RBX, true));

        assert_emit!(0x40, 0x0f, 0x9a, 0xc7; emit_setb_reg_parity(RDI, true));
        assert_emit!(0x41, 0x0f, 0x9a, 0xc7; emit_setb_reg_parity(R15, true));

        assert_emit!(0x0f, 0x9b, 0xc0; emit_setb_reg_parity(RAX, false));
        assert_emit!(0x0f, 0x9b, 0xc1; emit_setb_reg_parity(RCX, false));
        assert_emit!(0x0f, 0x9b, 0xc2; emit_setb_reg_parity(RDX, false));
        assert_emit!(0x0f, 0x9b, 0xc3; emit_setb_reg_parity(RBX, false));

        assert_emit!(0x40, 0x0f, 0x9b, 0xc7; emit_setb_reg_parity(RDI, false));
        assert_emit!(0x41, 0x0f, 0x9b, 0xc7; emit_setb_reg_parity(R15, false));
    }

    #[test]
    fn test_xorps() {
        assert_emit!(0x0f, 0x57, 0x05, 0xf6, 0xff, 0xff, 0xff;
                     xorps(XMM0, Mem::Base(RIP, -10)));
        assert_emit!(0x66, 0x0f, 0x57, 0x05, 0xf6, 0xff, 0xff, 0xff;
                     xorpd(XMM0, Mem::Base(RIP, -10)));
    }
}
