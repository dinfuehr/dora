static AND: u8 = 0b0000;
static EOR: u8 = 0b0001;
static SUB: u8 = 0b0010;
static ADD: u8 = 0b0100;
static ORR: u8 = 0b1100;
static MVN: u8 = 0b1111;

static COND_EQ: u8 = 0b0000;
static COND_NE: u8 = 0b0001;
static COND_CS: u8 = 0b0010;
static COND_CC: u8 = 0b0011;
static COND_MI: u8 = 0b0100;
static COND_PL: u8 = 0b0101;
static COND_VS: u8 = 0b0110;
static COND_VC: u8 = 0b0111;
static COND_HI: u8 = 0b1000;
static COND_LS: u8 = 0b1001;
static COND_GE: u8 = 0b1010;
static COND_LT: u8 = 0b1011;
static COND_GT: u8 = 0b1100;
static COND_LE: u8 = 0b1101;
static COND_ALWAYS: u8 = 0b1110;

static IMM: u8 = 1;
static NON_IMM: u8 = 0;

static NOT_SET_COND: u8 = 0;
static SET_COND: u8 = 1;

pub fn emit_or(buf: &mut Buffer, dest: Reg, lhs: Reg, rhs: Reg) {
    emit_data_proc(buf, COND_ALWAYS, NON_IMM, ORR, NOT_SET_COND, lhs, dest, rhs);
}

pub fn emit_and(buf: &mut Buffer, dest: Reg, lhs: Reg, rhs: Reg) {
    emit_data_proc(buf, COND_ALWAYS, NON_IMM, AND, NOT_SET_COND, lhs, dest, rhs);
}

pub fn emit_add(buf: &mut Buffer, dest: Reg, lhs: Reg, rhs: Reg) {
    emit_data_proc(buf, COND_ALWAYS, NON_IMM, ADD, NOT_SET_COND, lhs, dest, rhs);
}

pub fn emit_sub(buf: &mut Buffer, dest: Reg, lhs: Reg, rhs: Reg) {
    emit_data_proc(buf, COND_ALWAYS, NON_IMM, SUB, NOT_SET_COND, lhs, dest, rhs);
}

pub fn emit_xor(buf: &mut Buffer, dest: Reg, lhs: Reg, rhs: Reg) {
    emit_data_proc(buf, COND_ALWAYS, NON_IMM, EOR, NOT_SET_COND, lhs, dest, rhs);
}

pub fn emit_data_proc(buf: &mut Buffer, cond: u32, i: u32, opcode: u32, s: u32,
                      rn: u32, rd: u32, opnd: u32) {
    let instr: u32 = (opnd & 0xFFF)   |
                     (rd & 0xF) << 12 |
                     (rn & 0xF) << 16 |
                     (s << 20)        |
                     (opcode << 21)   |
                     (i << 25)        |
                     (cond << 28);

    buf.write_u32(instr);
}
