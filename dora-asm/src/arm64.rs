use crate::{AssemblerBuffer, Label};

pub const R0: Register = Register(0);
pub const R1: Register = Register(1);
pub const R2: Register = Register(2);
pub const R3: Register = Register(3);
pub const R4: Register = Register(4);
pub const R5: Register = Register(5);
pub const R6: Register = Register(6);
pub const R7: Register = Register(7);
pub const R8: Register = Register(8);
pub const R9: Register = Register(9);
pub const R10: Register = Register(10);
pub const R11: Register = Register(11);
pub const R12: Register = Register(12);
pub const R13: Register = Register(13);
pub const R14: Register = Register(14);
pub const R15: Register = Register(15);
pub const R16: Register = Register(16);
pub const R17: Register = Register(17);
pub const R18: Register = Register(18);
pub const R19: Register = Register(19);
pub const R20: Register = Register(20);
pub const R21: Register = Register(21);
pub const R22: Register = Register(22);
pub const R23: Register = Register(23);
pub const R24: Register = Register(24);
pub const R25: Register = Register(25);
pub const R26: Register = Register(26);
pub const R27: Register = Register(27);
pub const R28: Register = Register(28);
pub const R29: Register = Register(29);
pub const R30: Register = Register(30);
pub const REG_FP: Register = R29;
pub const REG_LR: Register = R30;
pub const REG_ZERO: Register = Register(100);
pub const REG_SP: Register = Register(101);

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Register(u8);

impl Register {
    pub fn new(value: u8) -> Register {
        assert!(value < 31);
        Register(value)
    }

    fn encoding(self) -> u32 {
        assert!(self.is_gpr());
        self.0 as u32
    }

    fn encoding_zero(self) -> u32 {
        assert!(self.is_gpr_or_zero());

        if self.is_gpr() {
            self.0 as u32
        } else {
            31
        }
    }

    fn encoding_sp(self) -> u32 {
        assert!(self.is_gpr_or_sp());

        if self.is_gpr() {
            self.0 as u32
        } else {
            31
        }
    }

    fn encoding_zero_or_sp(self) -> u32 {
        if self.is_gpr() {
            self.0 as u32
        } else if self == REG_ZERO || self == REG_SP {
            31
        } else {
            unreachable!()
        }
    }

    fn is_gpr(&self) -> bool {
        self.0 <= R30.0
    }

    fn is_gpr_or_zero(&self) -> bool {
        self.0 <= R30.0 || *self == REG_ZERO
    }

    fn is_gpr_or_sp(&self) -> bool {
        self.0 <= R30.0 || *self == REG_SP
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct NeonRegister(u8);

impl NeonRegister {
    pub fn new(value: u8) -> NeonRegister {
        assert!(value < 32);
        NeonRegister(value)
    }

    fn encoding(self) -> u32 {
        self.0 as u32
    }
}

pub const F0: NeonRegister = NeonRegister(0);
pub const F1: NeonRegister = NeonRegister(1);
pub const F2: NeonRegister = NeonRegister(2);
pub const F3: NeonRegister = NeonRegister(3);
pub const F4: NeonRegister = NeonRegister(4);
pub const F5: NeonRegister = NeonRegister(5);
pub const F6: NeonRegister = NeonRegister(6);
pub const F7: NeonRegister = NeonRegister(7);
pub const F8: NeonRegister = NeonRegister(8);
pub const F9: NeonRegister = NeonRegister(9);
pub const F10: NeonRegister = NeonRegister(10);
pub const F11: NeonRegister = NeonRegister(11);
pub const F12: NeonRegister = NeonRegister(12);
pub const F13: NeonRegister = NeonRegister(13);
pub const F14: NeonRegister = NeonRegister(14);
pub const F15: NeonRegister = NeonRegister(15);
pub const F16: NeonRegister = NeonRegister(16);
pub const F17: NeonRegister = NeonRegister(17);
pub const F18: NeonRegister = NeonRegister(18);
pub const F19: NeonRegister = NeonRegister(19);
pub const F20: NeonRegister = NeonRegister(20);
pub const F21: NeonRegister = NeonRegister(21);
pub const F22: NeonRegister = NeonRegister(22);
pub const F23: NeonRegister = NeonRegister(23);
pub const F24: NeonRegister = NeonRegister(24);
pub const F25: NeonRegister = NeonRegister(25);
pub const F26: NeonRegister = NeonRegister(26);
pub const F27: NeonRegister = NeonRegister(27);
pub const F28: NeonRegister = NeonRegister(28);
pub const F29: NeonRegister = NeonRegister(29);
pub const F30: NeonRegister = NeonRegister(30);
pub const F31: NeonRegister = NeonRegister(31);

struct ForwardJump {
    offset: u32,
    label: Label,
    kind: JumpKind,
}

enum JumpKind {
    Adr(Register),
    Unconditional,
    Conditional(Cond),
    Zero {
        sf: bool,
        is_non_zero: bool,
        rt: Register,
    },
    Test {
        op: u32,
        bit: u32,
        rt: Register,
    },
}

pub enum MemOperand {
    Immediate {
        base: Register,
        offset: i64,
    },
    RegOffset {
        base: Register,
        regoffset: Register,
        extend: Extend,
        shiftamount: u32,
    },
}

impl MemOperand {
    pub fn offset(base: Register, offset: i64) -> MemOperand {
        MemOperand::Immediate { base, offset }
    }

    pub fn regoffset(
        base: Register,
        regoffset: Register,
        extend: Extend,
        shiftamount: u32,
    ) -> MemOperand {
        MemOperand::RegOffset {
            base,
            regoffset,
            extend,
            shiftamount,
        }
    }
}

pub struct AssemblerArm64 {
    unresolved_jumps: Vec<ForwardJump>,
    buffer: AssemblerBuffer,
}

impl AssemblerArm64 {
    pub fn new() -> AssemblerArm64 {
        AssemblerArm64 {
            unresolved_jumps: Vec::new(),
            buffer: AssemblerBuffer::new(),
        }
    }

    pub fn create_label(&mut self) -> Label {
        self.buffer.create_label()
    }

    pub fn create_and_bind_label(&mut self) -> Label {
        self.buffer.create_and_bind_label()
    }

    pub fn bind_label(&mut self, lbl: Label) {
        self.buffer.bind_label(lbl);
    }

    pub fn offset(&self, lbl: Label) -> Option<u32> {
        self.buffer.offset(lbl)
    }

    pub fn finalize(mut self, alignment: usize) -> AssemblerBuffer {
        self.resolve_jumps();
        self.align_to(alignment);
        self.buffer
    }

    pub fn align_to(&mut self, alignment: usize) {
        while self.buffer.code.len() % alignment != 0 {
            self.brk(0);
        }
        assert_eq!(self.buffer.code.len() % alignment, 0);
    }

    pub fn position(&self) -> usize {
        self.buffer.position()
    }

    pub fn set_position(&mut self, pos: usize) {
        self.buffer.set_position(pos);
    }

    pub fn set_position_end(&mut self) {
        self.buffer.set_position_end();
    }

    pub fn emit_u8(&mut self, value: u8) {
        self.buffer.emit_u8(value);
    }

    pub fn emit_u32(&mut self, value: u32) {
        self.buffer.emit_u32(value);
    }

    pub fn emit_u64(&mut self, value: u64) {
        self.buffer.emit_u64(value);
    }

    pub fn emit_u128(&mut self, value: u128) {
        self.buffer.emit_u128(value);
    }
}

impl AssemblerArm64 {
    fn resolve_jumps(&mut self) {
        let unresolved_jumps = std::mem::replace(&mut self.unresolved_jumps, Vec::new());

        let old_position = self.position();

        for jmp in &unresolved_jumps {
            if let Some(lbl_offset) = self.offset(jmp.label) {
                let distance: i32 = lbl_offset as i32 - jmp.offset as i32;
                assert!(distance % 4 == 0);
                let distance = distance / 4;

                self.set_position(jmp.offset as usize);

                match jmp.kind {
                    JumpKind::Adr(rd) => {
                        let distance: i32 = lbl_offset as i32 - jmp.offset as i32;
                        self.adr_imm(rd, distance);
                    }

                    JumpKind::Conditional(cond) => {
                        self.bc_imm(cond.into(), distance);
                    }

                    JumpKind::Unconditional => {
                        self.b_imm(distance);
                    }

                    JumpKind::Zero {
                        sf,
                        is_non_zero,
                        rt,
                    } => {
                        if fits_i19(distance) {
                            self.emit_u32(cls::cmp_branch_imm(
                                sf as u32,
                                is_non_zero as u32,
                                rt,
                                distance,
                            ));
                            assert_eq!(self.position(), jmp.offset as usize + 4);
                            self.nop();
                        } else {
                            self.emit_u32(cls::cmp_branch_imm(
                                sf as u32,
                                (is_non_zero as u32) ^ 1,
                                rt,
                                2,
                            ));
                            assert_eq!(self.position(), jmp.offset as usize + 4);
                            // Distance is off by 1 instruction in this case.
                            self.b_imm(distance - 1);
                        }
                    }

                    JumpKind::Test { op, bit, rt } => {
                        if fits_i14(distance) {
                            self.emit_u32(cls::test_and_branch(op, bit, distance, rt));
                            assert_eq!(self.position(), jmp.offset as usize + 4);
                            self.nop();
                        } else {
                            self.emit_u32(cls::test_and_branch(op ^ 1, bit, 2, rt));
                            assert_eq!(self.position(), jmp.offset as usize + 4);
                            // Distance is off by 1 instruction in this case.
                            self.b_imm(distance - 1);
                        }
                    }
                }
            } else {
                panic!("unbound label");
            }
        }

        self.set_position(old_position);
    }

    pub fn add(&mut self, rd: Register, rn: Register, rm: Register) {
        if rd == REG_SP || rn == REG_SP {
            self.add_ext(rd, rn, rm, Extend::UXTX, 0);
        } else {
            self.add_sh(rd, rn, rm, Shift::LSL, 0);
        }
    }

    pub fn add_w(&mut self, rd: Register, rn: Register, rm: Register) {
        if rd == REG_SP || rn == REG_SP {
            self.add_ext_w(rd, rn, rm, Extend::UXTX, 0);
        } else {
            self.add_sh_w(rd, rn, rm, Shift::LSL, 0);
        }
    }

    pub fn add_ext(
        &mut self,
        rd: Register,
        rn: Register,
        rm: Register,
        extend: Extend,
        amount: u32,
    ) {
        self.emit_u32(cls::addsub_extreg(1, 0, 0, 0, rm, extend, amount, rn, rd));
    }

    pub fn add_ext_w(
        &mut self,
        rd: Register,
        rn: Register,
        rm: Register,
        extend: Extend,
        amount: u32,
    ) {
        self.emit_u32(cls::addsub_extreg(0, 0, 0, 0, rm, extend, amount, rn, rd));
    }

    pub fn add_sh(&mut self, rd: Register, rn: Register, rm: Register, shift: Shift, amount: u32) {
        self.emit_u32(cls::addsub_shreg(1, 0, 0, shift, rm, amount, rn, rd));
    }

    pub fn add_sh_w(
        &mut self,
        rd: Register,
        rn: Register,
        rm: Register,
        shift: Shift,
        amount: u32,
    ) {
        self.emit_u32(cls::addsub_shreg(0, 0, 0, shift, rm, amount, rn, rd));
    }

    pub fn add_imm(&mut self, rd: Register, rn: Register, imm: u32) {
        let (shift, imm12) = encode_addsub_imm(imm).expect("illegal value");
        self.emit_u32(cls::addsub_imm(1, 0, 0, shift, imm12, rn, rd));
    }

    pub fn add_imm_w(&mut self, rd: Register, rn: Register, imm: u32) {
        let (shift, imm12) = encode_addsub_imm(imm).expect("illegal value");
        self.emit_u32(cls::addsub_imm(0, 0, 0, shift, imm12, rn, rd));
    }

    pub fn adds_imm(&mut self, rd: Register, rn: Register, imm: u32) {
        let (shift, imm12) = encode_addsub_imm(imm).expect("illegal value");
        self.emit_u32(cls::addsub_imm(1, 0, 1, shift, imm12, rn, rd));
    }

    pub fn adds_imm_w(&mut self, rd: Register, rn: Register, imm: u32) {
        let (shift, imm12) = encode_addsub_imm(imm).expect("illegal value");
        self.emit_u32(cls::addsub_imm(0, 0, 1, shift, imm12, rn, rd));
    }

    pub fn adds_sh(&mut self, rd: Register, rn: Register, rm: Register, shift: Shift, amount: u32) {
        self.emit_u32(cls::addsub_shreg(1, 0, 1, shift, rm, amount, rn, rd));
    }

    pub fn adds_sh_w(
        &mut self,
        rd: Register,
        rn: Register,
        rm: Register,
        shift: Shift,
        amount: u32,
    ) {
        self.emit_u32(cls::addsub_shreg(0, 0, 1, shift, rm, amount, rn, rd));
    }

    pub fn adds(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls::addsub_shreg(1, 0, 1, Shift::LSL, rm, 0, rn, rd));
    }

    pub fn adds_w(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls::addsub_shreg(0, 0, 1, Shift::LSL, rm, 0, rn, rd));
    }

    pub fn addv(&mut self, q: u32, size: u32, rd: NeonRegister, rn: NeonRegister) {
        self.emit_u32(cls::simd_across_lanes(q, 0, size, 0b11011, rn, rd));
    }

    pub fn adr_imm(&mut self, rd: Register, imm: i32) {
        self.emit_u32(cls::pcrel(0, imm, rd));
    }

    pub fn adr_label(&mut self, rd: Register, label: Label) {
        self.unresolved_jumps.push(ForwardJump {
            offset: self.position().try_into().expect("overflow"),
            label,
            kind: JumpKind::Adr(rd),
        });
        self.emit_u32(0);
    }

    pub fn adrp_imm(&mut self, rd: Register, imm: i32) {
        self.emit_u32(cls::pcrel(1, imm, rd));
    }

    pub fn and_imm(&mut self, rd: Register, rn: Register, imm: u64) {
        let n_immr_imms = encode_logical_imm(imm, 64).unwrap();
        self.emit_u32(cls::logical_imm(1, 0b00, n_immr_imms, rn, rd));
    }

    pub fn and_imm_w(&mut self, rd: Register, rn: Register, imm: u64) {
        let n_immr_imms = encode_logical_imm(imm, 32).unwrap();
        self.emit_u32(cls::logical_imm(0, 0b00, n_immr_imms, rn, rd));
    }

    pub fn and_sh(&mut self, rd: Register, rn: Register, rm: Register, shift: Shift, imm6: u32) {
        self.emit_u32(cls::logical_shreg(1, 0b00, shift, 0, rm, imm6, rn, rd));
    }

    pub fn and_sh_w(&mut self, rd: Register, rn: Register, rm: Register, shift: Shift, imm6: u32) {
        self.emit_u32(cls::logical_shreg(0, 0b00, shift, 0, rm, imm6, rn, rd));
    }

    pub fn ands_sh(&mut self, rd: Register, rn: Register, rm: Register, shift: Shift, imm6: u32) {
        self.emit_u32(cls::logical_shreg(1, 0b11, shift, 0, rm, imm6, rn, rd));
    }

    pub fn ands_sh_w(&mut self, rd: Register, rn: Register, rm: Register, shift: Shift, imm6: u32) {
        self.emit_u32(cls::logical_shreg(0, 0b11, shift, 0, rm, imm6, rn, rd));
    }

    pub fn asrv(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls::dataproc2(1, 0, rm, 0b1010, rn, rd));
    }

    pub fn asrv_w(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls::dataproc2(0, 0, rm, 0b1010, rn, rd));
    }

    fn b_imm(&mut self, imm26: i32) {
        self.emit_u32(cls::uncond_branch_imm(0, imm26));
    }

    pub fn b(&mut self, target: Label) {
        let value = self.offset(target);

        match value {
            Some(target_offset) => {
                let diff = -(self.position() as i32 - target_offset as i32);
                assert!(diff % 4 == 0);
                self.b_imm(diff / 4);
            }

            None => {
                let pos = self.position() as u32;
                self.emit_u32(0);
                self.unresolved_jumps.push(ForwardJump {
                    offset: pos,
                    label: target,
                    kind: JumpKind::Unconditional,
                });
            }
        }
    }

    pub fn b_r(&mut self, rn: Register) {
        self.emit_u32(cls::uncond_branch_reg(0b0000, 0b11111, 0, rn, 0));
    }

    fn bc_imm(&mut self, cond: Cond, diff: i32) {
        self.emit_u32(inst::b_cond_imm(cond.into(), diff));
    }

    pub fn bc(&mut self, cond: Cond, target: Label) {
        let value = self.offset(target);

        match value {
            Some(target_offset) => {
                let diff = -(self.position() as i32 - target_offset as i32);
                assert!(diff % 4 == 0);
                self.bc_imm(cond, diff / 4);
            }

            None => {
                let pos = self.position() as u32;
                self.emit_u32(0);
                self.unresolved_jumps.push(ForwardJump {
                    offset: pos,
                    label: target,
                    kind: JumpKind::Conditional(cond),
                });
            }
        }
    }

    pub fn bfm(&mut self, rd: Register, rn: Register, immr: u32, imms: u32) {
        self.emit_u32(cls::bitfield(1, 0b01, 1, immr, imms, rn, rd));
    }

    pub fn bfm_w(&mut self, rd: Register, rn: Register, immr: u32, imms: u32) {
        self.emit_u32(cls::bitfield(0, 0b01, 0, immr, imms, rn, rd));
    }

    pub fn bic_sh(&mut self, rd: Register, rn: Register, rm: Register, shift: Shift, imm6: u32) {
        self.emit_u32(cls::logical_shreg(1, 0b00, shift, 1, rm, imm6, rn, rd));
    }

    pub fn bic_sh_w(&mut self, rd: Register, rn: Register, rm: Register, shift: Shift, imm6: u32) {
        self.emit_u32(cls::logical_shreg(0, 0b00, shift, 1, rm, imm6, rn, rd));
    }

    pub fn bics_sh(&mut self, rd: Register, rn: Register, rm: Register, shift: Shift, imm6: u32) {
        self.emit_u32(cls::logical_shreg(1, 0b11, shift, 1, rm, imm6, rn, rd));
    }

    pub fn bics_sh_w(&mut self, rd: Register, rn: Register, rm: Register, shift: Shift, imm6: u32) {
        self.emit_u32(cls::logical_shreg(0, 0b11, shift, 1, rm, imm6, rn, rd));
    }

    pub fn bl_r(&mut self, rn: Register) {
        self.emit_u32(cls::uncond_branch_reg(0b0001, 0b11111, 0, rn, 0));
    }

    pub fn brk(&mut self, imm16: u32) {
        self.emit_u32(cls::exception(0b001, imm16, 0, 0));
    }

    pub fn cas(&mut self, cmp: Register, new: Register, addr: Register) {
        self.emit_u32(cls::ldst_exclusive(
            0b11, 1, 0, 1, cmp, 0, REG_ZERO, addr, new,
        ))
    }

    pub fn cas_w(&mut self, cmp: Register, new: Register, addr: Register) {
        self.emit_u32(cls::ldst_exclusive(
            0b10, 1, 0, 1, cmp, 0, REG_ZERO, addr, new,
        ))
    }

    pub fn casa(&mut self, cmp: Register, new: Register, addr: Register) {
        self.emit_u32(cls::ldst_exclusive(
            0b11, 1, 1, 1, cmp, 0, REG_ZERO, addr, new,
        ))
    }

    pub fn casa_w(&mut self, cmp: Register, new: Register, addr: Register) {
        self.emit_u32(cls::ldst_exclusive(
            0b10, 1, 1, 1, cmp, 0, REG_ZERO, addr, new,
        ))
    }

    pub fn casal(&mut self, cmp: Register, new: Register, addr: Register) {
        self.emit_u32(cls::ldst_exclusive(
            0b11, 1, 1, 1, cmp, 1, REG_ZERO, addr, new,
        ))
    }

    pub fn casal_w(&mut self, cmp: Register, new: Register, addr: Register) {
        self.emit_u32(cls::ldst_exclusive(
            0b10, 1, 1, 1, cmp, 1, REG_ZERO, addr, new,
        ))
    }

    pub fn casl(&mut self, cmp: Register, new: Register, addr: Register) {
        self.emit_u32(cls::ldst_exclusive(
            0b11, 1, 0, 1, cmp, 1, REG_ZERO, addr, new,
        ))
    }

    pub fn casl_w(&mut self, cmp: Register, new: Register, addr: Register) {
        self.emit_u32(cls::ldst_exclusive(
            0b10, 1, 0, 1, cmp, 1, REG_ZERO, addr, new,
        ))
    }

    pub fn cbnz(&mut self, reg: Register, target: Label) {
        self.common_cbz_cbnz(true, true, reg, target);
    }

    pub fn cbnz_w(&mut self, reg: Register, target: Label) {
        self.common_cbz_cbnz(false, true, reg, target);
    }

    pub fn cbnz_imm(&mut self, reg: Register, diff: i32) {
        self.emit_u32(cls::cmp_branch_imm(1, 1, reg, diff));
    }

    pub fn cbnz_imm_w(&mut self, reg: Register, diff: i32) {
        self.emit_u32(cls::cmp_branch_imm(0, 1, reg, diff));
    }

    pub fn cbz(&mut self, reg: Register, target: Label) {
        self.common_cbz_cbnz(true, false, reg, target);
    }

    pub fn cbz_w(&mut self, reg: Register, target: Label) {
        self.common_cbz_cbnz(false, false, reg, target);
    }

    fn common_cbz_cbnz(&mut self, sf: bool, is_non_zero: bool, reg: Register, target: Label) {
        let target_offset = self.offset(target);

        match target_offset {
            Some(target_offset) => {
                let diff = -(self.position() as i32 - target_offset as i32);
                assert!(diff % 4 == 0);
                let diff = diff / 4;

                if fits_i19(diff) {
                    self.emit_u32(cls::cmp_branch_imm(
                        sf as u32,
                        is_non_zero as u32,
                        reg,
                        diff,
                    ));
                } else {
                    self.emit_u32(cls::cmp_branch_imm(
                        sf as u32,
                        (is_non_zero as u32) ^ 1,
                        reg,
                        2,
                    ));
                    self.b_imm(diff - 1);
                }
            }

            None => {
                let pos = self.position() as u32;
                self.emit_u32(0);
                self.emit_u32(0);
                self.unresolved_jumps.push(ForwardJump {
                    offset: pos,
                    label: target,
                    kind: JumpKind::Zero {
                        sf,
                        is_non_zero,
                        rt: reg,
                    },
                });
            }
        }
    }

    pub fn cbz_imm(&mut self, reg: Register, diff: i32) {
        self.emit_u32(cls::cmp_branch_imm(1, 0, reg, diff));
    }

    pub fn cbz_imm_w(&mut self, reg: Register, diff: i32) {
        self.emit_u32(cls::cmp_branch_imm(0, 0, reg, diff));
    }

    pub fn cls_w(&mut self, rd: Register, rn: Register) {
        self.emit_u32(cls::dataproc1(0, 0, 0b00000, 0b000101, rn, rd));
    }

    pub fn cls(&mut self, rd: Register, rn: Register) {
        self.emit_u32(cls::dataproc1(1, 0, 0b00000, 0b000101, rn, rd));
    }

    pub fn clz_w(&mut self, rd: Register, rn: Register) {
        self.emit_u32(cls::dataproc1(0, 0, 0b00000, 0b000100, rn, rd));
    }

    pub fn clz(&mut self, rd: Register, rn: Register) {
        self.emit_u32(cls::dataproc1(1, 0, 0b00000, 0b000100, rn, rd));
    }

    pub fn cmn_imm(&mut self, rn: Register, imm: u32) {
        self.adds_imm(REG_ZERO, rn, imm);
    }

    pub fn cmn_imm_w(&mut self, rn: Register, imm: u32) {
        self.adds_imm_w(REG_ZERO, rn, imm);
    }

    pub fn cmp(&mut self, rn: Register, rm: Register) {
        self.subs(REG_ZERO, rn, rm);
    }

    pub fn cmp_w(&mut self, rn: Register, rm: Register) {
        self.subs_w(REG_ZERO, rn, rm);
    }

    pub fn cmp_ext(&mut self, rn: Register, rm: Register, extend: Extend, amount: u32) {
        self.subs_ext(REG_ZERO, rn, rm, extend, amount);
    }

    pub fn cmp_ext_w(&mut self, rn: Register, rm: Register, extend: Extend, amount: u32) {
        self.subs_ext(REG_ZERO, rn, rm, extend, amount);
    }

    pub fn cmp_imm(&mut self, rn: Register, imm: u32) {
        self.subs_imm(REG_ZERO, rn, imm);
    }

    pub fn cmp_imm_w(&mut self, rn: Register, imm: u32) {
        self.subs_imm_w(REG_ZERO, rn, imm);
    }

    pub fn cmp_sh(&mut self, rn: Register, rm: Register, shift: Shift, amount: u32) {
        self.emit_u32(cls::addsub_shreg(1, 1, 1, shift, rm, amount, rn, REG_ZERO));
    }

    pub fn cmp_sh_w(&mut self, rn: Register, rm: Register, shift: Shift, amount: u32) {
        self.emit_u32(cls::addsub_shreg(0, 1, 1, shift, rm, amount, rn, REG_ZERO));
    }

    pub fn cnt(&mut self, q: u32, size: u32, rd: NeonRegister, rn: NeonRegister) {
        self.emit_u32(cls::simd_2regs_misc(q, 0, size, 0b00101, rn, rd));
    }

    pub fn csel(&mut self, rd: Register, rn: Register, rm: Register, cond: Cond) {
        self.emit_u32(cls::csel(1, 0, 0, rm, cond, 0, rn, rd));
    }

    pub fn csel_w(&mut self, rd: Register, rn: Register, rm: Register, cond: Cond) {
        self.emit_u32(cls::csel(0, 0, 0, rm, cond, 0, rn, rd));
    }

    pub fn cset(&mut self, rd: Register, cond: Cond) {
        self.csinc(rd, REG_ZERO, REG_ZERO, cond.invert());
    }

    pub fn cset_w(&mut self, rd: Register, cond: Cond) {
        self.csinc_w(rd, REG_ZERO, REG_ZERO, cond.invert());
    }

    pub fn csinc(&mut self, rd: Register, rn: Register, rm: Register, cond: Cond) {
        self.emit_u32(cls::csel(1, 0, 0, rm, cond, 1, rn, rd));
    }

    pub fn csinc_w(&mut self, rd: Register, rn: Register, rm: Register, cond: Cond) {
        self.emit_u32(cls::csel(0, 0, 0, rm, cond, 1, rn, rd));
    }

    pub fn csinv(&mut self, rd: Register, rn: Register, rm: Register, cond: Cond) {
        self.emit_u32(cls::csel(1, 1, 0, rm, cond, 0, rn, rd));
    }

    pub fn csinv_w(&mut self, rd: Register, rn: Register, rm: Register, cond: Cond) {
        self.emit_u32(cls::csel(0, 1, 0, rm, cond, 0, rn, rd));
    }

    pub fn dmb_ish(&mut self) {
        self.dmb(0b1011);
    }

    pub fn dmb(&mut self, imm: u32) {
        self.emit_u32(cls::system_cls(0, 0b00, 0b011, 0b0011, imm, 0b101, 0b11111));
    }

    pub fn eon_sh(&mut self, rd: Register, rn: Register, rm: Register, shift: Shift, imm6: u32) {
        self.emit_u32(cls::logical_shreg(1, 0b10, shift, 1, rm, imm6, rn, rd));
    }

    pub fn eon_sh_w(&mut self, rd: Register, rn: Register, rm: Register, shift: Shift, imm6: u32) {
        self.emit_u32(cls::logical_shreg(0, 0b10, shift, 1, rm, imm6, rn, rd));
    }

    pub fn eor_sh(&mut self, rd: Register, rn: Register, rm: Register, shift: Shift, imm6: u32) {
        self.emit_u32(cls::logical_shreg(1, 0b10, shift, 0, rm, imm6, rn, rd));
    }

    pub fn eor_sh_w(&mut self, rd: Register, rn: Register, rm: Register, shift: Shift, imm6: u32) {
        self.emit_u32(cls::logical_shreg(0, 0b10, shift, 0, rm, imm6, rn, rd));
    }

    pub fn fadd_s(&mut self, rd: NeonRegister, rn: NeonRegister, rm: NeonRegister) {
        self.emit_u32(cls::fp_dataproc2(
            0,
            0,
            FLOAT_TYPE_SINGLE,
            rm,
            0b0010,
            rn,
            rd,
        ));
    }

    pub fn fadd_d(&mut self, rd: NeonRegister, rn: NeonRegister, rm: NeonRegister) {
        self.emit_u32(cls::fp_dataproc2(
            0,
            0,
            FLOAT_TYPE_DOUBLE,
            rm,
            0b0010,
            rn,
            rd,
        ));
    }

    pub fn fcmp_d(&mut self, rn: NeonRegister, rm: NeonRegister) {
        self.emit_u32(cls::fp_compare(0, 0, FLOAT_TYPE_DOUBLE, rm, 0, rn, 0));
    }

    pub fn fcmp_s(&mut self, rn: NeonRegister, rm: NeonRegister) {
        self.emit_u32(cls::fp_compare(0, 0, FLOAT_TYPE_SINGLE, rm, 0, rn, 0));
    }

    pub fn fcmpe_d(&mut self, rn: NeonRegister, rm: NeonRegister) {
        self.emit_u32(cls::fp_compare(0, 0, FLOAT_TYPE_DOUBLE, rm, 0, rn, 0b10000));
    }

    pub fn fcmpe_s(&mut self, rn: NeonRegister, rm: NeonRegister) {
        self.emit_u32(cls::fp_compare(0, 0, FLOAT_TYPE_SINGLE, rm, 0, rn, 0b10000));
    }

    pub fn fcvt_ds(&mut self, rd: NeonRegister, rn: NeonRegister) {
        self.emit_u32(cls::fp_dataproc1(
            0,
            0,
            FLOAT_TYPE_SINGLE,
            0b000100 | FLOAT_TYPE_DOUBLE,
            rn,
            rd,
        ));
    }

    pub fn fcvt_sd(&mut self, rd: NeonRegister, rn: NeonRegister) {
        self.emit_u32(cls::fp_dataproc1(
            0,
            0,
            FLOAT_TYPE_DOUBLE,
            0b000100 | FLOAT_TYPE_SINGLE,
            rn,
            rd,
        ));
    }

    pub fn fcvtzs_d(&mut self, rd: Register, rn: NeonRegister) {
        self.emit_u32(cls::fp_int(
            1,
            0,
            FLOAT_TYPE_DOUBLE,
            0b11,
            0b000,
            rn.encoding(),
            rd.encoding(),
        ));
    }

    pub fn fcvtzs_s(&mut self, rd: Register, rn: NeonRegister) {
        self.emit_u32(cls::fp_int(
            1,
            0,
            FLOAT_TYPE_SINGLE,
            0b11,
            0b000,
            rn.encoding(),
            rd.encoding(),
        ));
    }

    pub fn fcvtzs_wd(&mut self, rd: Register, rn: NeonRegister) {
        self.emit_u32(cls::fp_int(
            0,
            0,
            FLOAT_TYPE_DOUBLE,
            0b11,
            0b000,
            rn.encoding(),
            rd.encoding(),
        ));
    }

    pub fn fcvtzs_ws(&mut self, rd: Register, rn: NeonRegister) {
        self.emit_u32(cls::fp_int(
            0,
            0,
            FLOAT_TYPE_SINGLE,
            0b11,
            0b000,
            rn.encoding(),
            rd.encoding(),
        ));
    }

    pub fn fdiv_d(&mut self, rd: NeonRegister, rn: NeonRegister, rm: NeonRegister) {
        self.emit_u32(cls::fp_dataproc2(
            0,
            0,
            FLOAT_TYPE_DOUBLE,
            rm,
            0b0001,
            rn,
            rd,
        ));
    }

    pub fn fdiv_s(&mut self, rd: NeonRegister, rn: NeonRegister, rm: NeonRegister) {
        self.emit_u32(cls::fp_dataproc2(
            0,
            0,
            FLOAT_TYPE_SINGLE,
            rm,
            0b0001,
            rn,
            rd,
        ));
    }

    pub fn fmov_d(&mut self, rd: NeonRegister, rn: NeonRegister) {
        self.emit_u32(cls::fp_dataproc1(0, 0, FLOAT_TYPE_DOUBLE, 0b000000, rn, rd));
    }

    pub fn fmov_s(&mut self, rd: NeonRegister, rn: NeonRegister) {
        self.emit_u32(cls::fp_dataproc1(0, 0, FLOAT_TYPE_SINGLE, 0b000000, rn, rd));
    }

    pub fn fmov_fs_d(&mut self, rd: NeonRegister, rn: Register) {
        self.emit_u32(cls::fp_int(
            1,
            0,
            FLOAT_TYPE_DOUBLE,
            0b00,
            0b111,
            rn.encoding(),
            rd.encoding(),
        ));
    }

    pub fn fmov_fs_s(&mut self, rd: NeonRegister, rn: Register) {
        self.emit_u32(cls::fp_int(
            0,
            0,
            FLOAT_TYPE_SINGLE,
            0b00,
            0b111,
            rn.encoding(),
            rd.encoding(),
        ));
    }

    pub fn fmov_sf_d(&mut self, rd: Register, rn: NeonRegister) {
        self.emit_u32(cls::fp_int(
            1,
            0,
            FLOAT_TYPE_DOUBLE,
            0b00,
            0b110,
            rn.encoding(),
            rd.encoding(),
        ));
    }

    pub fn fmov_sf_s(&mut self, rd: Register, rn: NeonRegister) {
        self.emit_u32(cls::fp_int(
            0,
            0,
            FLOAT_TYPE_SINGLE,
            0b00,
            0b110,
            rn.encoding(),
            rd.encoding(),
        ));
    }

    pub fn fmul_s(&mut self, rd: NeonRegister, rn: NeonRegister, rm: NeonRegister) {
        self.emit_u32(cls::fp_dataproc2(
            0,
            0,
            FLOAT_TYPE_SINGLE,
            rm,
            0b0000,
            rn,
            rd,
        ));
    }

    pub fn fmul_d(&mut self, rd: NeonRegister, rn: NeonRegister, rm: NeonRegister) {
        self.emit_u32(cls::fp_dataproc2(
            0,
            0,
            FLOAT_TYPE_DOUBLE,
            rm,
            0b0000,
            rn,
            rd,
        ));
    }

    pub fn fabs_d(&mut self, rd: NeonRegister, rn: NeonRegister) {
        self.emit_u32(cls::fp_dataproc1(0, 0, FLOAT_TYPE_DOUBLE, 0b000001, rn, rd));
    }

    pub fn fabs_s(&mut self, rd: NeonRegister, rn: NeonRegister) {
        self.emit_u32(cls::fp_dataproc1(0, 0, FLOAT_TYPE_SINGLE, 0b000001, rn, rd));
    }

    pub fn fneg_d(&mut self, rd: NeonRegister, rn: NeonRegister) {
        self.emit_u32(cls::fp_dataproc1(0, 0, FLOAT_TYPE_DOUBLE, 0b000010, rn, rd));
    }

    pub fn fneg_s(&mut self, rd: NeonRegister, rn: NeonRegister) {
        self.emit_u32(cls::fp_dataproc1(0, 0, FLOAT_TYPE_SINGLE, 0b000010, rn, rd));
    }

    pub fn frintn_d(&mut self, rd: NeonRegister, rn: NeonRegister) {
        self.emit_u32(cls::fp_dataproc1(0, 0, FLOAT_TYPE_DOUBLE, 0b001000, rn, rd));
    }

    pub fn frintn_s(&mut self, rd: NeonRegister, rn: NeonRegister) {
        self.emit_u32(cls::fp_dataproc1(0, 0, FLOAT_TYPE_SINGLE, 0b001000, rn, rd));
    }

    pub fn frintp_d(&mut self, rd: NeonRegister, rn: NeonRegister) {
        self.emit_u32(cls::fp_dataproc1(0, 0, FLOAT_TYPE_DOUBLE, 0b001001, rn, rd));
    }

    pub fn frintp_s(&mut self, rd: NeonRegister, rn: NeonRegister) {
        self.emit_u32(cls::fp_dataproc1(0, 0, FLOAT_TYPE_SINGLE, 0b001001, rn, rd));
    }

    pub fn frintm_d(&mut self, rd: NeonRegister, rn: NeonRegister) {
        self.emit_u32(cls::fp_dataproc1(0, 0, FLOAT_TYPE_DOUBLE, 0b001010, rn, rd));
    }

    pub fn frintm_s(&mut self, rd: NeonRegister, rn: NeonRegister) {
        self.emit_u32(cls::fp_dataproc1(0, 0, FLOAT_TYPE_SINGLE, 0b001010, rn, rd));
    }

    pub fn frintz_d(&mut self, rd: NeonRegister, rn: NeonRegister) {
        self.emit_u32(cls::fp_dataproc1(0, 0, FLOAT_TYPE_DOUBLE, 0b001011, rn, rd));
    }

    pub fn frintz_s(&mut self, rd: NeonRegister, rn: NeonRegister) {
        self.emit_u32(cls::fp_dataproc1(0, 0, FLOAT_TYPE_SINGLE, 0b001011, rn, rd));
    }

    pub fn frinta_d(&mut self, rd: NeonRegister, rn: NeonRegister) {
        self.emit_u32(cls::fp_dataproc1(0, 0, FLOAT_TYPE_DOUBLE, 0b001100, rn, rd));
    }

    pub fn frinta_s(&mut self, rd: NeonRegister, rn: NeonRegister) {
        self.emit_u32(cls::fp_dataproc1(0, 0, FLOAT_TYPE_SINGLE, 0b001100, rn, rd));
    }

    pub fn fsqrt_d(&mut self, rd: NeonRegister, rn: NeonRegister) {
        self.emit_u32(cls::fp_dataproc1(0, 0, FLOAT_TYPE_DOUBLE, 0b000011, rn, rd));
    }

    pub fn fsqrt_s(&mut self, rd: NeonRegister, rn: NeonRegister) {
        self.emit_u32(cls::fp_dataproc1(0, 0, FLOAT_TYPE_SINGLE, 0b000011, rn, rd));
    }

    pub fn fsub_s(&mut self, rd: NeonRegister, rn: NeonRegister, rm: NeonRegister) {
        self.emit_u32(cls::fp_dataproc2(
            0,
            0,
            FLOAT_TYPE_SINGLE,
            rm,
            0b0011,
            rn,
            rd,
        ));
    }

    pub fn fsub_d(&mut self, rd: NeonRegister, rn: NeonRegister, rm: NeonRegister) {
        self.emit_u32(cls::fp_dataproc2(
            0,
            0,
            FLOAT_TYPE_DOUBLE,
            rm,
            0b0011,
            rn,
            rd,
        ));
    }

    pub fn ldadd(&mut self, value: Register, dest: Register, address: Register) {
        self.emit_u32(cls::atomic_op(
            0b11, 0, 0, 0, value, 0, 0b000, address, dest,
        ))
    }

    pub fn ldadd_w(&mut self, value: Register, dest: Register, address: Register) {
        self.emit_u32(cls::atomic_op(
            0b10, 0, 0, 0, value, 0, 0b000, address, dest,
        ))
    }

    pub fn ldadda(&mut self, value: Register, dest: Register, address: Register) {
        self.emit_u32(cls::atomic_op(
            0b11, 0, 1, 0, value, 0, 0b000, address, dest,
        ))
    }

    pub fn ldadda_w(&mut self, value: Register, dest: Register, address: Register) {
        self.emit_u32(cls::atomic_op(
            0b10, 0, 1, 0, value, 0, 0b000, address, dest,
        ))
    }

    pub fn ldaddal(&mut self, value: Register, dest: Register, address: Register) {
        self.emit_u32(cls::atomic_op(
            0b11, 0, 1, 1, value, 0, 0b000, address, dest,
        ))
    }

    pub fn ldaddal_w(&mut self, value: Register, dest: Register, address: Register) {
        self.emit_u32(cls::atomic_op(
            0b10, 0, 1, 1, value, 0, 0b000, address, dest,
        ))
    }

    pub fn ldaddl(&mut self, value: Register, dest: Register, address: Register) {
        self.emit_u32(cls::atomic_op(
            0b11, 0, 0, 1, value, 0, 0b000, address, dest,
        ))
    }

    pub fn ldaddl_w(&mut self, value: Register, dest: Register, address: Register) {
        self.emit_u32(cls::atomic_op(
            0b10, 0, 0, 1, value, 0, 0b000, address, dest,
        ))
    }

    pub fn ldar(&mut self, rt: Register, rn: Register) {
        self.emit_u32(cls::ldst_exclusive(
            0b11, 1, 1, 0, REG_ZERO, 1, REG_ZERO, rn, rt,
        ))
    }

    pub fn ldarb(&mut self, rt: Register, rn: Register) {
        self.emit_u32(cls::ldst_exclusive(
            0b00, 1, 1, 0, REG_ZERO, 1, REG_ZERO, rn, rt,
        ))
    }

    pub fn ldarh(&mut self, rt: Register, rn: Register) {
        self.emit_u32(cls::ldst_exclusive(
            0b01, 1, 1, 0, REG_ZERO, 1, REG_ZERO, rn, rt,
        ))
    }

    pub fn ldar_w(&mut self, rt: Register, rn: Register) {
        self.emit_u32(cls::ldst_exclusive(
            0b10, 1, 1, 0, REG_ZERO, 1, REG_ZERO, rn, rt,
        ))
    }

    pub fn ldaxr(&mut self, rt: Register, rn: Register) {
        self.emit_u32(cls::ldst_exclusive(
            0b11, 0, 1, 0, REG_ZERO, 1, REG_ZERO, rn, rt,
        ))
    }

    pub fn ldaxr_w(&mut self, rt: Register, rn: Register) {
        self.emit_u32(cls::ldst_exclusive(
            0b10, 0, 1, 0, REG_ZERO, 1, REG_ZERO, rn, rt,
        ))
    }

    pub fn ldp(&mut self, rt: Register, rt2: Register, rn: Register, imm: i32) {
        assert_eq!(imm % 8, 0);
        let imm = imm / 8;
        self.emit_u32(cls::ldst_pair(0b10, 0, 1, imm, rt2, rn, rt));
    }

    pub fn ldp_w(&mut self, rt: Register, rt2: Register, rn: Register, imm: i32) {
        assert_eq!(imm % 4, 0);
        let imm = imm / 4;
        self.emit_u32(cls::ldst_pair(0b00, 0, 1, imm, rt2, rn, rt));
    }

    pub fn ldp_post(&mut self, rt: Register, rt2: Register, rn: Register, imm7: i32) {
        self.emit_u32(cls::ldst_pair_post(0b10, 0, 1, imm7, rt2, rn, rt));
    }

    pub fn ldp_post_w(&mut self, rt: Register, rt2: Register, rn: Register, imm7: i32) {
        self.emit_u32(cls::ldst_pair_post(0b00, 0, 1, imm7, rt2, rn, rt));
    }

    pub fn ldr(&mut self, rt: Register, opnd: MemOperand) {
        match opnd {
            MemOperand::Immediate { base, offset } => {
                assert!(rt.is_gpr());
                assert_eq!(offset % 8, 0);
                let offset = offset / 8;
                self.emit_u32(cls::ldst_regimm(
                    0b11,
                    0,
                    0b01,
                    offset as u32,
                    base,
                    rt.encoding(),
                ));
            }

            MemOperand::RegOffset {
                base,
                regoffset,
                extend,
                shiftamount,
            } => {
                assert!(rt.is_gpr());
                let amount = if shiftamount == 0 {
                    0
                } else {
                    assert_eq!(shiftamount, 3);
                    1
                };
                self.emit_u32(cls::ldst_regoffset(
                    0b11,
                    0,
                    0b01,
                    regoffset,
                    extend,
                    amount,
                    base,
                    rt.encoding(),
                ));
            }
        }
    }

    pub fn ldrb_imm(&mut self, rt: Register, rn: Register, imm12: u32) {
        assert!(rt.is_gpr());
        self.emit_u32(cls::ldst_regimm(0b00, 0, 0b01, imm12, rn, rt.encoding()));
    }

    pub fn ldr_imm_d(&mut self, rt: NeonRegister, rn: Register, imm: u32) {
        assert_eq!(imm % 8, 0);
        let imm = imm / 8;
        self.emit_u32(cls::ldst_regimm(0b11, 1, 0b01, imm, rn, rt.encoding()));
    }

    pub fn ldrh_imm(&mut self, rt: Register, rn: Register, imm: u32) {
        assert!(rt.is_gpr());
        assert_eq!(imm % 2, 0);
        let imm = imm / 2;
        self.emit_u32(cls::ldst_regimm(0b01, 0, 0b01, imm, rn, rt.encoding()));
    }

    pub fn ldr_imm_s(&mut self, rt: NeonRegister, rn: Register, imm: u32) {
        assert_eq!(imm % 4, 0);
        let imm = imm / 4;
        self.emit_u32(cls::ldst_regimm(0b10, 1, 0b01, imm, rn, rt.encoding()));
    }

    pub fn ldr_imm_w(&mut self, rt: Register, rn: Register, imm: u32) {
        assert!(rt.is_gpr());
        assert_eq!(imm % 4, 0);
        let imm = imm / 4;
        self.emit_u32(cls::ldst_regimm(0b10, 0, 0b01, imm, rn, rt.encoding()));
    }

    pub fn ldrb_reg(
        &mut self,
        rt: Register,
        rn: Register,
        rm: Register,
        extend: Extend,
        amount: u32,
    ) {
        assert!(rt.is_gpr());
        assert_eq!(amount, 0);
        self.emit_u32(cls::ldst_regoffset(
            0b00,
            0,
            0b01,
            rm,
            extend,
            amount,
            rn,
            rt.encoding(),
        ));
    }

    pub fn ldr_reg_d(
        &mut self,
        rt: NeonRegister,
        rn: Register,
        rm: Register,
        extend: Extend,
        amount: u32,
    ) {
        let amount = if amount == 0 {
            0
        } else {
            assert_eq!(amount, 3);
            1
        };
        self.emit_u32(cls::ldst_regoffset(
            0b11,
            1,
            0b01,
            rm,
            extend,
            amount,
            rn,
            rt.encoding(),
        ));
    }

    pub fn ldrh_reg(
        &mut self,
        rt: Register,
        rn: Register,
        rm: Register,
        extend: Extend,
        amount: u32,
    ) {
        let amount = if amount == 0 {
            0
        } else {
            assert_eq!(amount, 1);
            1
        };
        assert!(rt.is_gpr());
        self.emit_u32(cls::ldst_regoffset(
            0b01,
            0,
            0b01,
            rm,
            extend,
            amount,
            rn,
            rt.encoding(),
        ));
    }

    pub fn ldr_reg_s(
        &mut self,
        rt: NeonRegister,
        rn: Register,
        rm: Register,
        extend: Extend,
        amount: u32,
    ) {
        let amount = if amount == 0 {
            0
        } else {
            assert_eq!(amount, 2);
            1
        };
        self.emit_u32(cls::ldst_regoffset(
            0b10,
            1,
            0b01,
            rm,
            extend,
            amount,
            rn,
            rt.encoding(),
        ));
    }

    pub fn ldr_reg_w(
        &mut self,
        rt: Register,
        rn: Register,
        rm: Register,
        extend: Extend,
        amount: u32,
    ) {
        assert!(rt.is_gpr());
        let amount = if amount == 0 {
            0
        } else {
            assert_eq!(amount, 2);
            1
        };
        self.emit_u32(cls::ldst_regoffset(
            0b10,
            0,
            0b01,
            rm,
            extend,
            amount,
            rn,
            rt.encoding(),
        ));
    }

    pub fn ldur(&mut self, rt: Register, rn: Register, imm9: i32) {
        assert!(rt.is_gpr());
        self.emit_u32(cls::ldst_reg_unscaledimm(
            0b11,
            0,
            0b01,
            imm9,
            rn,
            rt.encoding(),
        ));
    }

    pub fn ldurb(&mut self, rt: Register, rn: Register, imm9: i32) {
        assert!(rt.is_gpr());
        self.emit_u32(cls::ldst_reg_unscaledimm(
            0b00,
            0,
            0b01,
            imm9,
            rn,
            rt.encoding(),
        ));
    }

    pub fn ldur_d(&mut self, rt: NeonRegister, rn: Register, imm9: i32) {
        self.emit_u32(cls::ldst_reg_unscaledimm(
            0b11,
            1,
            0b01,
            imm9,
            rn,
            rt.encoding(),
        ));
    }

    pub fn ldurh(&mut self, rt: Register, rn: Register, imm9: i32) {
        assert!(rt.is_gpr());
        self.emit_u32(cls::ldst_reg_unscaledimm(
            0b01,
            0,
            0b01,
            imm9,
            rn,
            rt.encoding(),
        ));
    }

    pub fn ldur_s(&mut self, rt: NeonRegister, rn: Register, imm9: i32) {
        self.emit_u32(cls::ldst_reg_unscaledimm(
            0b10,
            1,
            0b01,
            imm9,
            rn,
            rt.encoding(),
        ));
    }

    pub fn ldur_w(&mut self, rt: Register, rn: Register, imm9: i32) {
        assert!(rt.is_gpr());
        self.emit_u32(cls::ldst_reg_unscaledimm(
            0b10,
            0,
            0b01,
            imm9,
            rn,
            rt.encoding(),
        ));
    }

    pub fn ldxr(&mut self, rt: Register, rn: Register) {
        self.emit_u32(cls::ldst_exclusive(
            0b11, 0, 1, 0, REG_ZERO, 0, REG_ZERO, rn, rt,
        ))
    }

    pub fn ldxr_w(&mut self, rt: Register, rn: Register) {
        self.emit_u32(cls::ldst_exclusive(
            0b10, 0, 1, 0, REG_ZERO, 0, REG_ZERO, rn, rt,
        ))
    }

    pub fn lsl_imm(&mut self, rd: Register, rn: Register, shift: u32) {
        let (val, mask) = (64, 0x3f);
        self.ubfm(rd, rn, (val - shift) & mask, val - 1 - shift);
    }

    pub fn lsl_imm_w(&mut self, rd: Register, rn: Register, shift: u32) {
        let (val, mask) = (32, 0x1f);
        self.ubfm_w(rd, rn, (val - shift) & mask, val - 1 - shift);
    }

    pub fn lsl(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls::dataproc2(1, 0, rm, 0b1000, rn, rd));
    }

    pub fn lsl_w(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls::dataproc2(0, 0, rm, 0b1000, rn, rd));
    }

    pub fn lsr_imm(&mut self, rd: Register, rn: Register, shift: u32) {
        self.ubfm(rd, rn, shift, 63);
    }

    pub fn lsr_imm_w(&mut self, rd: Register, rn: Register, shift: u32) {
        self.ubfm_w(rd, rn, shift, 31);
    }

    pub fn lsr(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls::dataproc2(1, 0, rm, 0b1001, rn, rd));
    }

    pub fn lsr_w(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls::dataproc2(0, 0, rm, 0b1001, rn, rd));
    }

    pub fn madd(&mut self, rd: Register, rn: Register, rm: Register, ra: Register) {
        self.emit_u32(cls::dataproc3(1, 0, 0, rm, 0, ra, rn, rd));
    }

    pub fn madd_w(&mut self, rd: Register, rn: Register, rm: Register, ra: Register) {
        self.emit_u32(cls::dataproc3(0, 0, 0, rm, 0, ra, rn, rd));
    }

    pub fn mov(&mut self, rd: Register, rs: Register) {
        if rd == REG_SP || rs == REG_SP {
            self.add_imm(rd, rs, 0);
        } else {
            self.orr_sh(rd, REG_ZERO, rs, Shift::LSL, 0);
        }
    }

    pub fn mov_w(&mut self, rd: Register, rs: Register) {
        if rd == REG_SP || rs == REG_SP {
            self.add_imm_w(rd, rs, 0);
        } else {
            self.orr_sh_w(rd, REG_ZERO, rs, Shift::LSL, 0);
        }
    }

    pub fn movn(&mut self, rd: Register, imm16: u32, shift: u32) {
        assert!(shift % 16 == 0);
        assert!(shift < 64);
        let shift = shift / 16;
        self.emit_u32(cls::move_wide_imm(1, 0b00, shift, imm16, rd));
    }

    pub fn movn_w(&mut self, rd: Register, imm16: u32, shift: u32) {
        assert!(shift % 16 == 0);
        assert!(shift < 32);
        let shift = shift / 16;
        self.emit_u32(cls::move_wide_imm(0, 0b00, shift, imm16, rd));
    }

    pub fn movz(&mut self, rd: Register, imm16: u32, shift: u32) {
        assert!(shift % 16 == 0);
        assert!(shift < 64);
        let shift = shift / 16;
        self.emit_u32(cls::move_wide_imm(1, 0b10, shift, imm16, rd));
    }

    pub fn movz_w(&mut self, rd: Register, imm16: u32, shift: u32) {
        assert!(shift % 16 == 0);
        assert!(shift < 32);
        let shift = shift / 16;
        self.emit_u32(cls::move_wide_imm(0, 0b10, shift, imm16, rd));
    }

    pub fn movk(&mut self, rd: Register, imm16: u32, shift: u32) {
        assert!(shift % 16 == 0);
        assert!(shift < 64);
        let shift = shift / 16;
        self.emit_u32(cls::move_wide_imm(1, 0b11, shift, imm16, rd));
    }

    pub fn movk_w(&mut self, rd: Register, imm16: u32, shift: u32) {
        assert!(shift % 16 == 0);
        assert!(shift < 32);
        let shift = shift / 16;
        self.emit_u32(cls::move_wide_imm(0, 0b11, shift, imm16, rd));
    }

    pub fn msub(&mut self, rd: Register, rn: Register, rm: Register, ra: Register) {
        self.emit_u32(cls::dataproc3(1, 0, 0, rm, 1, ra, rn, rd));
    }

    pub fn msub_w(&mut self, rd: Register, rn: Register, rm: Register, ra: Register) {
        self.emit_u32(cls::dataproc3(0, 0, 0, rm, 1, ra, rn, rd));
    }

    pub fn mul(&mut self, rd: Register, rn: Register, rm: Register) {
        self.madd(rd, rn, rm, REG_ZERO);
    }

    pub fn mul_w(&mut self, rd: Register, rn: Register, rm: Register) {
        self.madd_w(rd, rn, rm, REG_ZERO);
    }

    pub fn nop(&mut self) {
        self.emit_u32(cls::system(0));
    }

    pub fn orn_sh(&mut self, rd: Register, rn: Register, rm: Register, shift: Shift, imm6: u32) {
        self.emit_u32(cls::logical_shreg(1, 0b01, shift, 1, rm, imm6, rn, rd));
    }

    pub fn orn_sh_w(&mut self, rd: Register, rn: Register, rm: Register, shift: Shift, imm6: u32) {
        self.emit_u32(cls::logical_shreg(0, 0b01, shift, 1, rm, imm6, rn, rd));
    }

    pub fn orr_sh(&mut self, rd: Register, rn: Register, rm: Register, shift: Shift, imm6: u32) {
        self.emit_u32(cls::logical_shreg(1, 0b01, shift, 0, rm, imm6, rn, rd));
    }

    pub fn orr_sh_w(&mut self, rd: Register, rn: Register, rm: Register, shift: Shift, imm6: u32) {
        self.emit_u32(cls::logical_shreg(0, 0b01, shift, 0, rm, imm6, rn, rd));
    }

    pub fn rbit_w(&mut self, rd: Register, rn: Register) {
        self.emit_u32(cls::dataproc1(0, 0, 0b00000, 0b000000, rn, rd));
    }

    pub fn rbit(&mut self, rd: Register, rn: Register) {
        self.emit_u32(cls::dataproc1(1, 0, 0b00000, 0b000000, rn, rd));
    }

    pub fn ret(&mut self, rn: Register) {
        self.emit_u32(cls::uncond_branch_reg(0b0010, 0b11111, 0, rn, 0));
    }

    pub fn rev_w(&mut self, rd: Register, rn: Register) {
        self.emit_u32(cls::dataproc1(0, 0, 0b00000, 0b000010, rn, rd));
    }

    pub fn rev(&mut self, rd: Register, rn: Register) {
        self.emit_u32(cls::dataproc1(1, 0, 0b00000, 0b000011, rn, rd));
    }

    pub fn ror(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls::dataproc2(1, 0, rm, 0b1011, rn, rd));
    }

    pub fn ror_w(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls::dataproc2(0, 0, rm, 0b1011, rn, rd));
    }

    pub fn sbfm(&mut self, rd: Register, rn: Register, immr: u32, imms: u32) {
        self.emit_u32(cls::bitfield(1, 0b00, 1, immr, imms, rn, rd));
    }

    pub fn sbfm_w(&mut self, rd: Register, rn: Register, immr: u32, imms: u32) {
        self.emit_u32(cls::bitfield(0, 0b00, 0, immr, imms, rn, rd));
    }

    pub fn sdiv(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls::dataproc2(1, 0, rm, 0b11, rn, rd));
    }

    pub fn sdiv_w(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls::dataproc2(0, 0, rm, 0b11, rn, rd));
    }

    // scvtf (scalar, integer) - 32-bit to double-precision
    pub fn scvtf_si_dw(&mut self, rd: NeonRegister, rn: Register) {
        self.emit_u32(cls::fp_int(
            0,
            0,
            FLOAT_TYPE_DOUBLE,
            0b00,
            0b010,
            rn.encoding(),
            rd.encoding(),
        ));
    }

    // scvtf (scalar, integer) - 64-bit to double-precision
    pub fn scvtf_si_dx(&mut self, rd: NeonRegister, rn: Register) {
        self.emit_u32(cls::fp_int(
            1,
            0,
            FLOAT_TYPE_DOUBLE,
            0b00,
            0b010,
            rn.encoding(),
            rd.encoding(),
        ));
    }

    // scvtf (scalar, integer) - 32-bit to single-precision
    pub fn scvtf_si_sw(&mut self, rd: NeonRegister, rn: Register) {
        self.emit_u32(cls::fp_int(
            0,
            0,
            FLOAT_TYPE_SINGLE,
            0b00,
            0b010,
            rn.encoding(),
            rd.encoding(),
        ));
    }

    // scvtf (scalar, integer) - 64-bit to single-precision
    pub fn scvtf_si_sx(&mut self, rd: NeonRegister, rn: Register) {
        self.emit_u32(cls::fp_int(
            1,
            0,
            FLOAT_TYPE_SINGLE,
            0b00,
            0b010,
            rn.encoding(),
            rd.encoding(),
        ));
    }

    pub fn smaddl(&mut self, rd: Register, rn: Register, rm: Register, ra: Register) {
        self.emit_u32(cls::dataproc3(1, 0b00, 0b001, rm, 0, ra, rn, rd));
    }

    pub fn smull(&mut self, rd: Register, rn: Register, rm: Register) {
        self.smaddl(rd, rn, rm, REG_ZERO);
    }

    pub fn smulh(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls::dataproc3(1, 0, 0b010, rm, 0, REG_ZERO, rn, rd));
    }

    pub fn stlr(&mut self, rt: Register, rn: Register) {
        self.emit_u32(cls::ldst_exclusive(
            0b11, 1, 0, 0, REG_ZERO, 1, REG_ZERO, rn, rt,
        ))
    }

    pub fn stlrb(&mut self, rt: Register, rn: Register) {
        self.emit_u32(cls::ldst_exclusive(
            0b00, 1, 0, 0, REG_ZERO, 1, REG_ZERO, rn, rt,
        ))
    }

    pub fn stlrh(&mut self, rt: Register, rn: Register) {
        self.emit_u32(cls::ldst_exclusive(
            0b01, 1, 0, 0, REG_ZERO, 1, REG_ZERO, rn, rt,
        ))
    }

    pub fn stlr_w(&mut self, rt: Register, rn: Register) {
        self.emit_u32(cls::ldst_exclusive(
            0b10, 1, 0, 0, REG_ZERO, 1, REG_ZERO, rn, rt,
        ))
    }

    pub fn stp(&mut self, rt: Register, rt2: Register, rn: Register, imm7: i32) {
        self.emit_u32(cls::ldst_pair(0b10, 0, 0, imm7, rt2, rn, rt));
    }

    pub fn stp_w(&mut self, rt: Register, rt2: Register, rn: Register, imm7: i32) {
        self.emit_u32(cls::ldst_pair(0b00, 0, 0, imm7, rt2, rn, rt));
    }

    pub fn stp_post(&mut self, rt: Register, rt2: Register, rn: Register, imm7: i32) {
        assert!(imm7 % 8 == 0);
        self.emit_u32(cls::ldst_pair_post(0b10, 0, 0, imm7 / 8, rt2, rn, rt));
    }

    pub fn stp_post_w(&mut self, rt: Register, rt2: Register, rn: Register, imm7: i32) {
        assert!(imm7 % 4 == 0);
        self.emit_u32(cls::ldst_pair_post(0b00, 0, 0, imm7 / 4, rt2, rn, rt));
    }

    pub fn stp_pre(&mut self, rt: Register, rt2: Register, rn: Register, imm7: i32) {
        self.emit_u32(cls::ldst_pair_pre(0b10, 0, 0, imm7, rt2, rn, rt));
    }

    pub fn stp_pre_w(&mut self, rt: Register, rt2: Register, rn: Register, imm7: i32) {
        self.emit_u32(cls::ldst_pair_pre(0b00, 0, 0, imm7, rt2, rn, rt));
    }

    pub fn str_imm(&mut self, rt: Register, rn: Register, imm: u32) {
        assert!(rt.is_gpr_or_zero());
        assert_eq!(imm % 8, 0);
        let imm = imm / 8;
        self.emit_u32(cls::ldst_regimm(0b11, 0, 0b00, imm, rn, rt.encoding_zero()));
    }

    pub fn strb_imm(&mut self, rt: Register, rn: Register, imm: u32) {
        assert!(rt.is_gpr_or_zero());
        self.emit_u32(cls::ldst_regimm(0b00, 0, 0b00, imm, rn, rt.encoding_zero()));
    }

    pub fn str_imm_d(&mut self, rt: NeonRegister, rn: Register, imm: u32) {
        assert_eq!(imm % 8, 0);
        let imm = imm / 8;
        self.emit_u32(cls::ldst_regimm(0b11, 1, 0b00, imm, rn, rt.encoding()));
    }

    pub fn strh_imm(&mut self, rt: Register, rn: Register, imm: u32) {
        assert!(rt.is_gpr_or_zero());
        assert_eq!(imm % 2, 0);
        let imm = imm / 2;
        self.emit_u32(cls::ldst_regimm(0b01, 0, 0b00, imm, rn, rt.encoding_zero()));
    }

    pub fn str_imm_s(&mut self, rt: NeonRegister, rn: Register, imm: u32) {
        assert_eq!(imm % 4, 0);
        let imm = imm / 4;
        self.emit_u32(cls::ldst_regimm(0b10, 1, 0b00, imm, rn, rt.encoding()));
    }

    pub fn str_imm_w(&mut self, rt: Register, rn: Register, imm: u32) {
        assert!(rt.is_gpr_or_zero());
        assert_eq!(imm % 4, 0);
        let imm = imm / 4;
        self.emit_u32(cls::ldst_regimm(0b10, 0, 0b00, imm, rn, rt.encoding_zero()));
    }

    pub fn str_reg(
        &mut self,
        rt: Register,
        rn: Register,
        rm: Register,
        extend: Extend,
        amount: u32,
    ) {
        assert!(rt.is_gpr_or_zero());
        let amount = if amount == 0 {
            0
        } else {
            assert_eq!(amount, 3);
            1
        };
        self.emit_u32(cls::ldst_regoffset(
            0b11,
            0,
            0b00,
            rm,
            extend,
            amount,
            rn,
            rt.encoding_zero(),
        ));
    }

    pub fn strb_reg(
        &mut self,
        rt: Register,
        rn: Register,
        rm: Register,
        extend: Extend,
        amount: u32,
    ) {
        assert!(rt.is_gpr_or_zero());
        assert_eq!(amount, 0);
        self.emit_u32(cls::ldst_regoffset(
            0b00,
            0,
            0b00,
            rm,
            extend,
            amount,
            rn,
            rt.encoding_zero(),
        ));
    }

    pub fn str_reg_d(
        &mut self,

        rt: NeonRegister,
        rn: Register,
        rm: Register,
        extend: Extend,
        amount: u32,
    ) {
        let amount = if amount == 0 {
            0
        } else {
            assert_eq!(amount, 3);
            1
        };
        self.emit_u32(cls::ldst_regoffset(
            0b11,
            1,
            0b00,
            rm,
            extend,
            amount,
            rn,
            rt.encoding(),
        ));
    }

    pub fn strh_reg(
        &mut self,
        rt: Register,
        rn: Register,
        rm: Register,
        extend: Extend,
        amount: u32,
    ) {
        assert!(rt.is_gpr_or_zero());
        let amount = if amount == 0 {
            0
        } else {
            assert_eq!(amount, 1);
            1
        };
        self.emit_u32(cls::ldst_regoffset(
            0b01,
            0,
            0b00,
            rm,
            extend,
            amount,
            rn,
            rt.encoding_zero(),
        ));
    }

    pub fn str_reg_s(
        &mut self,
        rt: NeonRegister,
        rn: Register,
        rm: Register,
        extend: Extend,
        amount: u32,
    ) {
        let amount = if amount == 0 {
            0
        } else {
            assert_eq!(amount, 2);
            1
        };
        self.emit_u32(cls::ldst_regoffset(
            0b10,
            1,
            0b00,
            rm,
            extend,
            amount,
            rn,
            rt.encoding(),
        ));
    }

    pub fn str_reg_w(
        &mut self,
        rt: Register,
        rn: Register,
        rm: Register,
        extend: Extend,
        amount: u32,
    ) {
        assert!(rt.is_gpr_or_zero());
        let amount = if amount == 0 {
            0
        } else {
            assert_eq!(amount, 2);
            1
        };
        self.emit_u32(cls::ldst_regoffset(
            0b10,
            0,
            0b00,
            rm,
            extend,
            amount,
            rn,
            rt.encoding_zero(),
        ));
    }

    pub fn stur(&mut self, rt: Register, rn: Register, imm9: i32) {
        assert!(rt.is_gpr_or_zero());
        self.emit_u32(cls::ldst_reg_unscaledimm(
            0b11,
            0,
            0b00,
            imm9,
            rn,
            rt.encoding_zero(),
        ));
    }

    pub fn sturb(&mut self, rt: Register, rn: Register, imm9: i32) {
        assert!(rt.is_gpr_or_zero());
        self.emit_u32(cls::ldst_reg_unscaledimm(
            0b00,
            0,
            0b00,
            imm9,
            rn,
            rt.encoding_zero(),
        ));
    }

    pub fn stur_d(&mut self, rt: NeonRegister, rn: Register, imm9: i32) {
        self.emit_u32(cls::ldst_reg_unscaledimm(
            0b11,
            1,
            0b00,
            imm9,
            rn,
            rt.encoding(),
        ));
    }

    pub fn sturh(&mut self, rt: Register, rn: Register, imm9: i32) {
        assert!(rt.is_gpr_or_zero());
        self.emit_u32(cls::ldst_reg_unscaledimm(
            0b01,
            0,
            0b00,
            imm9,
            rn,
            rt.encoding_zero(),
        ));
    }

    pub fn stur_s(&mut self, rt: NeonRegister, rn: Register, imm9: i32) {
        self.emit_u32(cls::ldst_reg_unscaledimm(
            0b10,
            1,
            0b00,
            imm9,
            rn,
            rt.encoding(),
        ));
    }

    pub fn stur_w(&mut self, rt: Register, rn: Register, imm9: i32) {
        assert!(rt.is_gpr_or_zero());
        self.emit_u32(cls::ldst_reg_unscaledimm(
            0b10,
            0,
            0b00,
            imm9,
            rn,
            rt.encoding_zero(),
        ));
    }

    pub fn stxr(&mut self, status: Register, src: Register, addr: Register) {
        self.emit_u32(cls::ldst_exclusive(
            0b11, 0, 0, 0, status, 0, REG_ZERO, addr, src,
        ));
    }

    pub fn stxr_w(&mut self, status: Register, src: Register, addr: Register) {
        self.emit_u32(cls::ldst_exclusive(
            0b10, 0, 0, 0, status, 0, REG_ZERO, addr, src,
        ));
    }

    pub fn stlxr(&mut self, status: Register, src: Register, addr: Register) {
        self.emit_u32(cls::ldst_exclusive(
            0b11, 0, 0, 0, status, 1, REG_ZERO, addr, src,
        ));
    }

    pub fn stlxr_w(&mut self, status: Register, src: Register, addr: Register) {
        self.emit_u32(cls::ldst_exclusive(
            0b10, 0, 0, 0, status, 1, REG_ZERO, addr, src,
        ));
    }

    pub fn sub(&mut self, rd: Register, rn: Register, rm: Register) {
        if rd == REG_SP || rn == REG_SP {
            self.sub_ext(rd, rn, rm, Extend::UXTX, 0);
        } else {
            self.sub_sh(rd, rn, rm, Shift::LSL, 0);
        }
    }

    pub fn sub_w(&mut self, rd: Register, rn: Register, rm: Register) {
        if rd == REG_SP || rn == REG_SP {
            self.sub_ext_w(rd, rn, rm, Extend::UXTW, 0);
        } else {
            self.sub_sh_w(rd, rn, rm, Shift::LSL, 0);
        }
    }

    pub fn sub_ext(
        &mut self,
        rd: Register,
        rn: Register,
        rm: Register,
        extend: Extend,
        amount: u32,
    ) {
        self.emit_u32(cls::addsub_extreg(1, 1, 0, 0, rm, extend, amount, rn, rd));
    }

    pub fn sub_ext_w(
        &mut self,
        rd: Register,
        rn: Register,
        rm: Register,
        extend: Extend,
        amount: u32,
    ) {
        self.emit_u32(cls::addsub_extreg(0, 1, 0, 0, rm, extend, amount, rn, rd));
    }

    pub fn sub_sh(&mut self, rd: Register, rn: Register, rm: Register, shift: Shift, amount: u32) {
        self.emit_u32(cls::addsub_shreg(1, 1, 0, shift, rm, amount, rn, rd));
    }

    pub fn sub_sh_w(
        &mut self,
        rd: Register,
        rn: Register,
        rm: Register,
        shift: Shift,
        amount: u32,
    ) {
        self.emit_u32(cls::addsub_shreg(0, 1, 0, shift, rm, amount, rn, rd));
    }

    pub fn sub_imm(&mut self, rd: Register, rn: Register, imm: u32) {
        let (shift, imm12) = encode_addsub_imm(imm).expect("illegal value");
        self.emit_u32(cls::addsub_imm(1, 1, 0, shift, imm12, rn, rd));
    }

    pub fn sub_imm_w(&mut self, rd: Register, rn: Register, imm: u32) {
        let (shift, imm12) = encode_addsub_imm(imm).expect("illegal value");
        self.emit_u32(cls::addsub_imm(0, 1, 0, shift, imm12, rn, rd));
    }

    pub fn subs(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls::addsub_shreg(1, 1, 1, Shift::LSL, rm, 0, rn, rd));
    }

    pub fn subs_w(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls::addsub_shreg(0, 1, 1, Shift::LSL, rm, 0, rn, rd));
    }

    pub fn subs_ext(
        &mut self,
        rd: Register,
        rn: Register,
        rm: Register,
        extend: Extend,
        amount: u32,
    ) {
        self.emit_u32(cls::addsub_extreg(1, 1, 1, 0, rm, extend, amount, rn, rd));
    }

    pub fn subs_ext_w(
        &mut self,
        rd: Register,
        rn: Register,
        rm: Register,
        extend: Extend,
        amount: u32,
    ) {
        self.emit_u32(cls::addsub_extreg(0, 1, 1, 0, rm, extend, amount, rn, rd));
    }

    pub fn subs_imm(&mut self, rd: Register, rn: Register, imm: u32) {
        let (shift, imm12) = encode_addsub_imm(imm).expect("illegal value");
        self.emit_u32(cls::addsub_imm(1, 1, 1, shift, imm12, rn, rd));
    }

    pub fn subs_imm_w(&mut self, rd: Register, rn: Register, imm: u32) {
        let (shift, imm12) = encode_addsub_imm(imm).expect("illegal value");
        self.emit_u32(cls::addsub_imm(0, 1, 1, shift, imm12, rn, rd));
    }

    pub fn subs_sh(&mut self, rd: Register, rn: Register, rm: Register, shift: Shift, amount: u32) {
        self.emit_u32(cls::addsub_shreg(1, 1, 1, shift, rm, amount, rn, rd));
    }

    pub fn subs_sh_w(
        &mut self,
        rd: Register,
        rn: Register,
        rm: Register,
        shift: Shift,
        amount: u32,
    ) {
        self.emit_u32(cls::addsub_shreg(0, 1, 1, shift, rm, amount, rn, rd));
    }

    pub fn swp(&mut self, new: Register, old: Register, address: Register) {
        self.emit_u32(cls::atomic_op(0b11, 0, 0, 0, new, 1, 0b000, address, old))
    }

    pub fn swp_w(&mut self, new: Register, old: Register, address: Register) {
        self.emit_u32(cls::atomic_op(0b10, 0, 0, 0, new, 1, 0b000, address, old))
    }

    pub fn swpa(&mut self, new: Register, old: Register, address: Register) {
        self.emit_u32(cls::atomic_op(0b11, 0, 1, 0, new, 1, 0b000, address, old))
    }

    pub fn swpa_w(&mut self, new: Register, old: Register, address: Register) {
        self.emit_u32(cls::atomic_op(0b10, 0, 1, 0, new, 1, 0b000, address, old))
    }

    pub fn swpal(&mut self, new: Register, old: Register, address: Register) {
        self.emit_u32(cls::atomic_op(0b11, 0, 1, 1, new, 1, 0b000, address, old))
    }

    pub fn swpal_w(&mut self, new: Register, old: Register, address: Register) {
        self.emit_u32(cls::atomic_op(0b10, 0, 1, 1, new, 1, 0b000, address, old))
    }

    pub fn swpl(&mut self, new: Register, old: Register, address: Register) {
        self.emit_u32(cls::atomic_op(0b11, 0, 0, 1, new, 1, 0b000, address, old))
    }

    pub fn swpl_w(&mut self, new: Register, old: Register, address: Register) {
        self.emit_u32(cls::atomic_op(0b10, 0, 0, 1, new, 1, 0b000, address, old))
    }

    pub fn sxtw(&mut self, rd: Register, rn: Register) {
        self.sbfm(rd, rn, 0, 31);
    }

    #[allow(dead_code)]
    fn tbnz_imm(&mut self, rt: Register, bit: u32, imm14: i32) {
        assert_eq!(imm14 % 4, 0);
        self.emit_u32(cls::test_and_branch(0b1, bit, imm14 / 4, rt))
    }

    pub fn tbnz(&mut self, rt: Register, bit: u32, target: Label) {
        self.common_tbz_tbnz(1, bit, rt, target);
    }

    #[allow(dead_code)]
    fn tbz_imm(&mut self, rt: Register, bit: u32, imm14: i32) {
        assert_eq!(imm14 % 4, 0);
        self.emit_u32(cls::test_and_branch(0b0, bit, imm14 / 4, rt))
    }

    pub fn tbz(&mut self, rt: Register, bit: u32, target: Label) {
        self.common_tbz_tbnz(0, bit, rt, target);
    }

    fn common_tbz_tbnz(&mut self, op: u32, bit: u32, rt: Register, target: Label) {
        let target_offset = self.offset(target);

        match target_offset {
            Some(target_offset) => {
                let diff = -(self.position() as i32 - target_offset as i32);
                assert!(diff % 4 == 0);
                let diff = diff / 4;
                if fits_i14(diff) {
                    self.emit_u32(cls::test_and_branch(op, bit, diff, rt));
                } else {
                    unimplemented!();
                }
            }

            None => {
                let pos = self.position() as u32;
                self.emit_u32(0);
                self.emit_u32(0);
                self.unresolved_jumps.push(ForwardJump {
                    offset: pos,
                    label: target,
                    kind: JumpKind::Test { op, bit, rt },
                });
            }
        }
    }

    pub fn ubfm(&mut self, rd: Register, rn: Register, immr: u32, imms: u32) {
        self.emit_u32(cls::bitfield(1, 0b10, 1, immr, imms, rn, rd));
    }

    pub fn ubfm_w(&mut self, rd: Register, rn: Register, immr: u32, imms: u32) {
        self.emit_u32(cls::bitfield(0, 0b10, 0, immr, imms, rn, rd));
    }

    pub fn udiv(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls::dataproc2(1, 0, rm, 0b10, rn, rd));
    }

    pub fn udiv_w(&mut self, rd: Register, rn: Register, rm: Register) {
        self.emit_u32(cls::dataproc2(0, 0, rm, 0b10, rn, rd));
    }

    pub fn uxtb(&mut self, rd: Register, rn: Register) {
        self.ubfm_w(rd, rn, 0, 7);
    }

    pub fn uxtw(&mut self, rd: Register, rn: Register) {
        self.ubfm(rd, rn, 0, 31);
    }
}

mod inst {
    use super::*;

    pub(super) fn b_cond_imm(cond: Cond, imm19: i32) -> u32 {
        cls::cond_branch_imm(cond, imm19)
    }
}

pub mod cls {
    use super::*;

    pub(super) fn addsub_extreg(
        sf: u32,
        op: u32,
        s: u32,
        opt: u32,
        rm: Register,
        option: Extend,
        imm3: u32,
        rn: Register,
        rd: Register,
    ) -> u32 {
        assert!(fits_bit(sf));
        assert!(fits_bit(op));
        assert!(fits_bit(s));
        assert!(opt == 0);
        assert!(rm.is_gpr_or_zero());
        assert!(fits_u2(imm3));
        assert!(rn.is_gpr_or_sp());

        // Register 31 is xzr when setting flags or xsp when not.
        if s != 0 {
            assert!(rd.is_gpr_or_zero());
        } else {
            assert!(rd.is_gpr_or_sp());
        }

        sf << 31
            | op << 30
            | s << 29
            | 0b01011u32 << 24
            | opt << 22
            | 1u32 << 21
            | rm.encoding_zero() << 16
            | option.encoding() << 13
            | imm3 << 10
            | rn.encoding_sp() << 5
            | rd.encoding_zero_or_sp()
    }

    pub(super) fn addsub_shreg(
        sf: u32,
        op: u32,
        s: u32,
        shift: Shift,
        rm: Register,
        imm6: u32,
        rn: Register,
        rd: Register,
    ) -> u32 {
        assert!(fits_bit(sf));
        assert!(fits_bit(op));
        assert!(fits_bit(s));
        assert!(!shift.is_ror());
        assert!(rm.is_gpr());
        assert!(fits_u6(imm6));
        assert!(rn.is_gpr_or_zero());
        assert!(rd.is_gpr_or_zero());

        0b01011u32 << 24
            | sf << 31
            | op << 30
            | s << 29
            | shift.u32() << 22
            | rm.encoding() << 16
            | imm6 << 10
            | rn.encoding_zero() << 5
            | rd.encoding_zero()
    }

    pub(super) fn addsub_imm(
        sf: u32,
        op: u32,
        s: u32,
        shift: u32,
        imm12: u32,
        rn: Register,
        rd: Register,
    ) -> u32 {
        assert!(fits_bit(sf));
        assert!(fits_bit(op));
        assert!(fits_bit(s));
        assert!(fits_bit(shift));
        assert!(fits_u12(imm12));
        assert!(rn.is_gpr_or_sp());

        let rd = if s != 0 {
            assert!(rd.is_gpr_or_zero());
            rd.encoding_zero()
        } else {
            assert!(rd.is_gpr_or_sp());
            rd.encoding_sp()
        };

        (0b10001 as u32) << 24
            | sf << 31
            | op << 30
            | s << 29
            | shift << 22
            | imm12 << 10
            | rn.encoding_sp() << 5
            | rd
    }

    pub(super) fn atomic_op(
        size: u32,
        v: u32,
        a: u32,
        r: u32,
        rs: Register,
        o3: u32,
        opc: u32,
        rn: Register,
        rt: Register,
    ) -> u32 {
        assert!(fits_u2(size));
        assert!(fits_bit(v));
        assert!(fits_bit(a));
        assert!(fits_bit(r));
        assert!(rs.is_gpr());
        assert!(fits_bit(o3));
        assert!(fits_u2(opc));
        assert!(rn.is_gpr());
        assert!(rt.is_gpr());

        size << 30
            | 0b111 << 27
            | v << 26
            | a << 23
            | r << 22
            | 1 << 21
            | rs.encoding() << 16
            | o3 << 15
            | opc << 12
            | rn.encoding() << 5
            | rt.encoding()
    }

    pub(super) fn bitfield(
        sf: u32,
        opc: u32,
        n: u32,
        immr: u32,
        imms: u32,
        rn: Register,
        rd: Register,
    ) -> u32 {
        assert!(fits_bit(sf));
        assert!(fits_u2(opc));
        assert!(fits_bit(n));
        assert!(fits_u6(immr));
        assert!(fits_u6(imms));
        assert!(rn.is_gpr());
        assert!(rd.is_gpr());

        sf << 31
            | opc << 29
            | 0b100110u32 << 23
            | n << 22
            | (immr & 0x3F) << 16
            | (imms & 0x3F) << 10
            | rn.encoding() << 5
            | rd.encoding()
    }

    pub(super) fn cmp_branch_imm(sf: u32, op: u32, rt: Register, imm19: i32) -> u32 {
        assert!(fits_bit(sf));
        assert!(fits_bit(op));
        assert!(fits_i19(imm19));
        assert!(rt.is_gpr());
        let imm = (imm19 as u32) & 0x7FFFF;

        sf << 31 | 0b011010u32 << 25 | op << 24 | imm << 5 | rt.encoding()
    }

    pub(super) fn cond_branch_imm(cond: Cond, imm19: i32) -> u32 {
        assert!(fits_i19(imm19));

        let imm = (imm19 as u32) & 0x7FFFF;

        0b01010100u32 << 24 | imm << 5 | cond.u32()
    }

    pub(super) fn csel(
        sf: u32,
        op: u32,
        s: u32,
        rm: Register,
        cond: Cond,
        op2: u32,
        rn: Register,
        rd: Register,
    ) -> u32 {
        assert!(fits_bit(sf));
        assert!(fits_bit(op));
        assert!(fits_bit(s));
        assert!(rm.is_gpr_or_zero());
        assert!(fits_bit(op2));
        assert!(rn.is_gpr_or_zero());
        assert!(rd.is_gpr());

        0b11010100u32 << 21
            | sf << 31
            | op << 30
            | s << 29
            | rm.encoding_zero() << 16
            | cond.u32() << 12
            | op2 << 10
            | rn.encoding_zero() << 5
            | rd.encoding()
    }

    pub(super) fn dataproc1(
        sf: u32,
        s: u32,
        opcode2: u32,
        opcode: u32,
        rn: Register,
        rd: Register,
    ) -> u32 {
        assert!(fits_bit(sf));
        assert!(fits_bit(sf));
        assert!(fits_u5(opcode2));
        assert!(fits_u6(opcode));
        assert!(rn.is_gpr());
        assert!(rd.is_gpr());

        sf << 31
            | 1 << 30
            | s << 29
            | 0b11010110 << 21
            | opcode2 << 16
            | opcode << 10
            | rn.encoding() << 5
            | rd.encoding()
    }

    pub(super) fn dataproc2(
        sf: u32,
        s: u32,
        rm: Register,
        opcode: u32,
        rn: Register,
        rd: Register,
    ) -> u32 {
        assert!(fits_bit(sf));
        assert!(fits_bit(s));
        assert!(rm.is_gpr());
        assert!(fits_u6(opcode));
        assert!(rn.is_gpr());
        assert!(rd.is_gpr());

        sf << 31
            | s << 29
            | 0b11010110u32 << 21
            | rm.encoding() << 16
            | opcode << 10
            | rn.encoding() << 5
            | rd.encoding()
    }

    pub(super) fn dataproc3(
        sf: u32,
        op54: u32,
        op31: u32,
        rm: Register,
        o0: u32,
        ra: Register,
        rn: Register,
        rd: Register,
    ) -> u32 {
        assert!(fits_bit(sf));
        assert!(fits_u2(op54));
        assert!(fits_u3(op31));
        assert!(rm.is_gpr());
        assert!(fits_bit(o0));
        assert!(ra.is_gpr_or_zero());
        assert!(rn.is_gpr());
        assert!(rd.is_gpr());

        sf << 31
            | op54 << 29
            | 0b11011u32 << 24
            | op31 << 21
            | rm.encoding() << 16
            | o0 << 15
            | ra.encoding_zero() << 10
            | rn.encoding() << 5
            | rd.encoding()
    }

    pub(super) fn exception(opc: u32, imm16: u32, op2: u32, ll: u32) -> u32 {
        assert!(fits_u3(opc));
        assert!(fits_u16(imm16));
        assert!(op2 == 0);
        assert!(fits_u2(ll));

        0b11010100u32 << 24 | opc << 21 | imm16 << 5 | op2 << 2 | ll
    }

    pub(super) fn fp_compare(
        m: u32,
        s: u32,
        ty: u32,
        rm: NeonRegister,
        op: u32,
        rn: NeonRegister,
        opcode2: u32,
    ) -> u32 {
        assert!(m == 0);
        assert!(s == 0);
        assert!(fits_bit(ty));
        assert!(fits_u2(op));
        assert!(fits_u5(opcode2));

        m << 31
            | s << 29
            | 0b11110 << 24
            | ty << 22
            | 1 << 21
            | rm.encoding() << 16
            | op << 14
            | 0b1000 << 10
            | rn.encoding() << 5
            | opcode2
    }

    pub(super) fn fp_dataproc1(
        m: u32,
        s: u32,
        ty: u32,
        opcode: u32,
        rn: NeonRegister,
        rd: NeonRegister,
    ) -> u32 {
        assert!(m == 0);
        assert!(s == 0);
        assert!(fits_u2(ty));
        assert!(fits_u6(opcode));

        m << 31
            | s << 29
            | 0b11110 << 24
            | ty << 22
            | 1 << 21
            | opcode << 15
            | 0b10000 << 10
            | rn.encoding() << 5
            | rd.encoding()
    }

    pub(super) fn fp_dataproc2(
        m: u32,
        s: u32,
        ty: u32,
        rm: NeonRegister,
        opcode: u32,
        rn: NeonRegister,
        rd: NeonRegister,
    ) -> u32 {
        assert!(m == 0);
        assert!(s == 0);
        assert!(fits_bit(ty));
        assert!(fits_u4(opcode));

        m << 31
            | s << 29
            | 0b11110 << 24
            | ty << 22
            | 1 << 21
            | rm.encoding() << 16
            | opcode << 12
            | 0b10 << 10
            | rn.encoding() << 5
            | rd.encoding()
    }

    pub(super) fn fp_int(
        sf: u32,
        s: u32,
        ty: u32,
        rmode: u32,
        opcode: u32,
        rn: u32,
        rd: u32,
    ) -> u32 {
        assert!(fits_bit(sf));
        assert!(fits_bit(s));
        assert!(fits_u2(ty));
        assert!(fits_u2(rmode));
        assert!(fits_u3(opcode));
        assert!(fits_u5(rn));
        assert!(fits_u5(rd));

        sf << 31
            | s << 29
            | 0b11110 << 24
            | ty << 22
            | 1 << 21
            | rmode << 19
            | opcode << 16
            | rn << 5
            | rd
    }

    pub(super) fn ldst_exclusive(
        size: u32,
        o2: u32,
        l: u32,
        o1: u32,
        rs: Register,
        o0: u32,
        rt2: Register,
        rn: Register,
        rt: Register,
    ) -> u32 {
        assert!(fits_u2(size));
        assert!(fits_bit(o2));
        assert!(fits_bit(l));
        assert!(fits_bit(o1));
        assert!(fits_bit(o0));
        assert!(rs.is_gpr_or_zero());
        assert!(rt2.is_gpr_or_zero());
        assert!(rn.is_gpr_or_sp());
        assert!(rt.is_gpr_or_zero());

        size << 30
            | 0b001000 << 24
            | o2 << 23
            | l << 22
            | o1 << 21
            | rs.encoding_zero() << 16
            | o0 << 15
            | rt2.encoding_zero() << 10
            | rn.encoding_sp() << 5
            | rt.encoding_zero()
    }

    pub(super) fn ldst_pair(
        opc: u32,
        v: u32,
        l: u32,
        imm7: i32,
        rt2: Register,
        rn: Register,
        rt: Register,
    ) -> u32 {
        assert!(fits_u2(opc));
        assert!(fits_bit(v));
        assert!(fits_bit(l));
        assert!(fits_i7(imm7));
        assert!(rt2.is_gpr());
        assert!(rn.is_gpr_or_sp());
        assert!(rt.is_gpr());

        let imm = (imm7 as u32) & 0x7F;

        opc << 30
            | 0b101u32 << 27
            | 1u32 << 24
            | l << 22
            | imm << 15
            | rt2.encoding() << 10
            | rn.encoding_sp() << 5
            | rt.encoding()
    }

    pub(super) fn ldst_pair_post(
        opc: u32,
        v: u32,
        l: u32,
        imm7: i32,
        rt2: Register,
        rn: Register,
        rt: Register,
    ) -> u32 {
        assert!(fits_u2(opc));
        assert!(fits_bit(v));
        assert!(fits_bit(l));
        assert!(fits_i7(imm7));
        assert!(rt2.is_gpr_or_zero());
        assert!(rn.is_gpr_or_sp());
        assert!(rt.is_gpr_or_zero());

        let imm7 = (imm7 as u32) & 0x7F;

        opc << 30
            | 0b101u32 << 27
            | v << 26
            | 0b001u32 << 23
            | l << 22
            | imm7 << 15
            | rt2.encoding_zero() << 10
            | rn.encoding_sp() << 5
            | rt.encoding_zero()
    }

    pub(super) fn ldst_pair_pre(
        opc: u32,
        v: u32,
        l: u32,
        imm7: i32,
        rt2: Register,
        rn: Register,
        rt: Register,
    ) -> u32 {
        assert!(fits_u2(opc));
        assert!(fits_bit(v));
        assert!(fits_bit(l));
        assert!(fits_i7(imm7));
        assert!(rt2.is_gpr());
        assert!(rn.is_gpr_or_sp());
        assert!(rt.is_gpr());

        let imm7 = (imm7 as u32) & 0x7F;

        opc << 30
            | 0b101u32 << 27
            | v << 26
            | 0b011u32 << 23
            | l << 22
            | imm7 << 15
            | rt2.encoding() << 10
            | rn.encoding_sp() << 5
            | rt.encoding()
    }

    pub(super) fn logical_imm(
        sf: u32,
        opc: u32,
        n_immr_imms: u32,
        rn: Register,
        rd: Register,
    ) -> u32 {
        assert!(fits_bit(sf));
        assert!(fits_u2(opc));
        assert!(fits_u13(n_immr_imms));
        assert!(rn.is_gpr());
        assert!(rd.is_gpr());

        sf << 31
            | opc << 29
            | 0b100100u32 << 23
            | n_immr_imms << 10
            | rn.encoding() << 5
            | rd.encoding()
    }

    pub(super) fn logical_shreg(
        sf: u32,
        opc: u32,
        shift: Shift,
        n: u32,
        rm: Register,
        imm6: u32,
        rn: Register,
        rd: Register,
    ) -> u32 {
        assert!(fits_bit(sf));
        assert!(fits_u2(opc));
        assert!(fits_bit(n));
        assert!(rm.is_gpr_or_zero());
        assert!(fits_u5(imm6));
        assert!(rn.is_gpr_or_zero());
        assert!(rd.is_gpr());

        0b01010u32 << 24
            | sf << 31
            | opc << 29
            | shift.u32() << 22
            | n << 21
            | rm.encoding_zero() << 16
            | imm6 << 10
            | rn.encoding_zero() << 5
            | rd.encoding()
    }

    pub(super) fn move_wide_imm(sf: u32, opc: u32, hw: u32, imm16: u32, rd: Register) -> u32 {
        assert!(fits_bit(sf));
        assert!(fits_u2(opc));
        assert!(fits_u2(hw));
        if sf == 0 {
            assert!(fits_bit(hw));
        }
        assert!(fits_u16(imm16));
        assert!(rd.is_gpr());

        0b100101u32 << 23 | sf << 31 | opc << 29 | hw << 21 | imm16 << 5 | rd.encoding()
    }

    pub(super) fn ldst_regimm(
        size: u32,
        v: u32,
        opc: u32,
        imm12: u32,
        rn: Register,
        rt: u32,
    ) -> u32 {
        assert!(fits_u2(size));
        assert!(fits_bit(v));
        assert!(fits_u2(opc));
        assert!(fits_u12(imm12));
        assert!(rn.is_gpr_or_sp());
        assert!(fits_u5(rt));

        0b111001u32 << 24
            | size << 30
            | v << 26
            | opc << 22
            | imm12 << 10
            | rn.encoding_sp() << 5
            | rt
    }

    pub(super) fn ldst_regoffset(
        size: u32,
        v: u32,
        opc: u32,
        rm: Register,
        option: Extend,
        s: u32,
        rn: Register,
        rt: u32,
    ) -> u32 {
        assert!(fits_u2(size));
        assert!(fits_bit(v));
        assert!(fits_u2(opc));
        assert!(rm.is_gpr_or_zero());
        assert!(fits_bit(s));
        assert!(rn.is_gpr_or_sp());
        assert!(fits_u5(rt));

        size << 30
            | 0b111u32 << 27
            | 1u32 << 21
            | 0b10u32 << 10
            | v << 26
            | opc << 22
            | rm.encoding_zero() << 16
            | option.ldst_encoding() << 13
            | s << 12
            | rn.encoding_sp() << 5
            | rt
    }

    pub(super) fn ldst_reg_unscaledimm(
        size: u32,
        v: u32,
        opc: u32,
        imm9: i32,
        rn: Register,
        rt: u32,
    ) -> u32 {
        assert!(fits_u2(size));
        assert!(fits_bit(v));
        assert!(fits_u2(opc));
        assert!(fits_i9(imm9));
        assert!(rn.is_gpr_or_sp());
        assert!(fits_u5(rt));

        let imm = (imm9 as u32) & 0x1FF;

        size << 30 | 0b111 << 27 | v << 26 | opc << 22 | imm << 12 | rn.encoding_sp() << 5 | rt
    }

    pub(super) fn simd_across_lanes(
        q: u32,
        u: u32,
        size: u32,
        opcode: u32,
        rn: NeonRegister,
        rd: NeonRegister,
    ) -> u32 {
        assert!(fits_bit(q));
        assert!(fits_bit(u));
        assert!(fits_u2(size));
        assert!(fits_u5(opcode));

        q << 30
            | u << 29
            | 0b01110 << 24
            | size << 22
            | 0b11000 << 17
            | opcode << 12
            | 0b10 << 10
            | rn.encoding() << 5
            | rd.encoding()
    }

    pub(super) fn pcrel(op: u32, imm: i32, rd: Register) -> u32 {
        assert!(fits_i21(imm));
        assert!(fits_bit(op));
        assert!(rd.is_gpr());

        let imm = imm as u32;

        let immlo = imm & 3;
        let immhi = (imm >> 2) & 0x7FFFF;

        1u32 << 28 | op << 31 | immlo << 29 | immhi << 5 | rd.encoding()
    }

    pub(super) fn test_and_branch(op: u32, bit: u32, imm14: i32, rt: Register) -> u32 {
        assert!(fits_bit(op));
        assert!(fits_i14(imm14));
        assert!(rt.is_gpr_or_zero());
        assert!(fits_u6(bit));

        let imm14 = (imm14 as u32) & 0x3FFF;

        let b5 = (bit >> 5) & 1;
        let b40 = bit & 0x1F;

        b5 << 31 | 0b011011 << 25 | op << 24 | b40 << 19 | imm14 << 5 | rt.encoding()
    }

    pub(super) fn simd_2regs_misc(
        q: u32,
        u: u32,
        size: u32,
        opcode: u32,
        rn: NeonRegister,
        rd: NeonRegister,
    ) -> u32 {
        assert!(fits_bit(q));
        assert!(fits_bit(u));
        assert!(fits_u2(size));
        assert!(fits_u5(opcode));

        q << 30
            | u << 29
            | 0b01110 << 24
            | size << 22
            | 0b10000 << 17
            | opcode << 12
            | 0b10 << 10
            | rn.encoding() << 5
            | rd.encoding()
    }

    pub(super) fn system(imm: u32) -> u32 {
        assert!(fits_u7(imm));

        0xD503201F | imm << 5
    }

    pub(super) fn system_cls(
        l: u32,
        op0: u32,
        op1: u32,
        crn: u32,
        crm: u32,
        op2: u32,
        rt: u32,
    ) -> u32 {
        assert!(fits_bit(l));
        assert!(fits_u2(op0));
        assert!(fits_u3(op1));
        assert!(fits_u4(crn));
        assert!(fits_u4(crm));
        assert!(fits_u3(op2));

        0b1101_0101_00 << 22
            | l << 21
            | op0 << 19
            | op1 << 16
            | crn << 12
            | crm << 8
            | op2 << 5
            | rt
    }

    pub fn uncond_branch_imm(op: u32, imm26: i32) -> u32 {
        assert!(fits_bit(op));
        assert!(fits_i26(imm26));

        0b101u32 << 26 | op << 31 | ((imm26 as u32) & 0x3FFFFFF)
    }

    pub(super) fn uncond_branch_reg(opc: u32, op2: u32, op3: u32, rn: Register, op4: u32) -> u32 {
        assert!(fits_u4(opc));
        assert!(fits_u5(op2));
        assert!(fits_u6(op3));
        assert!(fits_u5(op4));

        (0b1101011 as u32) << 25 | opc << 21 | op2 << 16 | op3 << 10 | encoding_rn(rn) | op4
    }
}

fn encoding_rn(reg: Register) -> u32 {
    assert!(reg.is_gpr());
    reg.encoding() << 5
}

fn fits_bit(imm: u32) -> bool {
    imm < (1 << 1)
}

fn fits_i7(imm: i32) -> bool {
    -(1 << 6) <= imm && imm < (1 << 6)
}

fn fits_i9(imm: i32) -> bool {
    -(1 << 8) <= imm && imm < (1 << 8)
}

fn fits_i14(imm: i32) -> bool {
    -(1 << 14) <= imm && imm < (1 << 14)
}

fn fits_i19(imm: i32) -> bool {
    -(1 << 18) <= imm && imm < (1 << 18)
}

fn fits_i21(imm: i32) -> bool {
    -(1 << 20) <= imm && imm < (1 << 20)
}

fn fits_i26(imm: i32) -> bool {
    -(1 << 25) <= imm && imm < (1 << 25)
}

fn fits_u2(imm: u32) -> bool {
    imm < 4
}

fn fits_u3(imm: u32) -> bool {
    imm < 8
}

fn fits_u4(imm: u32) -> bool {
    imm < (1 << 4)
}

fn fits_u5(imm: u32) -> bool {
    imm < (1 << 5)
}

fn fits_u6(imm: u32) -> bool {
    imm < (1 << 6)
}

fn fits_u7(imm: u32) -> bool {
    imm < (1 << 7)
}

fn fits_u12(imm: u32) -> bool {
    imm < (1 << 12)
}

fn fits_u13(imm: u32) -> bool {
    imm < (1 << 13)
}

fn fits_u16(imm: u32) -> bool {
    imm < (1 << 16)
}

#[derive(Copy, Clone)]
pub enum Cond {
    EQ, // equal
    NE, // not equal
    CS,
    HS, // carry set, unsigned higher or same
    CC,
    LO, // carry clear, unsigned lower
    MI, // negative
    PL, // positive or zero
    VS, // overflow
    VC, // no overflow
    HI, // unsigned higher
    LS, // unsigned lower or same
    GE, // signed greater than or equal
    LT, // signed less than
    GT, // signed greater than
    LE, // signed less than or equal
}

impl Cond {
    pub fn invert(self) -> Cond {
        match self {
            Cond::EQ => Cond::NE,
            Cond::NE => Cond::EQ,
            Cond::CS | Cond::HS => Cond::CC,
            Cond::CC | Cond::LO => Cond::CS,
            Cond::MI => Cond::PL,
            Cond::PL => Cond::MI,
            Cond::VS => Cond::VC,
            Cond::VC => Cond::VS,
            Cond::HI => Cond::LS,
            Cond::LS => Cond::HI,
            Cond::GE => Cond::LT,
            Cond::LT => Cond::GE,
            Cond::GT => Cond::LE,
            Cond::LE => Cond::GT,
        }
    }

    pub fn u32(self) -> u32 {
        match self {
            Cond::EQ => 0b0000,
            Cond::NE => 0b0001,
            Cond::CS | Cond::HS => 0b0010,
            Cond::CC | Cond::LO => 0b0011,
            Cond::MI => 0b0100,
            Cond::PL => 0b0101,
            Cond::VS => 0b0110,
            Cond::VC => 0b0111,
            Cond::HI => 0b1000,
            Cond::LS => 0b1001,
            Cond::GE => 0b1010,
            Cond::LT => 0b1011,
            Cond::GT => 0b1100,
            Cond::LE => 0b1101,
        }
    }
}

#[derive(Copy, Clone)]
pub enum Extend {
    UXTB,
    UXTH,
    LSL,
    UXTW,
    UXTX,
    SXTB,
    SXTH,
    SXTW,
    SXTX,
}

impl Extend {
    fn encoding(self) -> u32 {
        match self {
            Extend::UXTB => 0b000,
            Extend::UXTH => 0b001,
            Extend::LSL => 0b010,
            Extend::UXTW => 0b010,
            Extend::UXTX => 0b011,
            Extend::SXTB => 0b100,
            Extend::SXTH => 0b101,
            Extend::SXTW => 0b110,
            Extend::SXTX => 0b111,
        }
    }

    fn ldst_encoding(self) -> u32 {
        match self {
            Extend::UXTW => 0b010,
            Extend::LSL => 0b011,
            Extend::SXTW => 0b110,
            Extend::SXTX => 0b111,
            _ => unreachable!(),
        }
    }
}

#[derive(Copy, Clone)]
pub enum Shift {
    LSL,
    LSR,
    ASR,
    ROR,
}

impl Shift {
    fn is_ror(self) -> bool {
        match self {
            Shift::ROR => true,
            _ => false,
        }
    }

    pub fn u32(self) -> u32 {
        match self {
            Shift::LSL => 0,
            Shift::LSR => 1,
            Shift::ASR => 2,
            Shift::ROR => 3,
        }
    }
}

fn encode_logical_imm(mut imm: u64, reg_size: u32) -> Option<u32> {
    assert!(reg_size == 32 || reg_size == 64);

    if imm == 0 || imm == !0 || (reg_size != 64 && (imm >> reg_size != 0 || imm == !0u32 as u64)) {
        return None;
    }

    // determine element size, use smallest possible element size
    let mut size: u32 = reg_size;

    while size > 2 {
        size /= 2;

        let mask = (1 << size) - 1;

        if (imm & mask) != (imm >> size) & mask {
            size *= 2;
            break;
        }
    }

    // truncate immediate to element size
    let mask = !0u64 >> (64 - size);
    imm &= mask;

    let rotation;
    let ones;

    if is_shifted_mask(imm) {
        let tz = imm.trailing_zeros();

        rotation = (size - tz) & (size - 1);
        ones = (!(imm >> tz)).trailing_zeros();

    // not all immediates are shifted masks: e.g. 1001
    } else {
        // extend imm again to 64 bits: e.g. 1..1|1001
        imm |= !mask;

        // the negation of the immediate now needs to be
        // a shifted mask
        if !is_shifted_mask(!imm) {
            return None;
        }

        let lo = (!imm).leading_zeros();
        let to = (!imm).trailing_zeros();

        rotation = lo - (64 - size);
        ones = lo + to - (64 - size);
    }

    assert!(ones > 0);
    assert!(rotation < size);

    let immr = rotation;
    let mut nimms = !(size - 1) << 1;
    nimms |= ones - 1;

    let n = ((nimms >> 6) & 1) ^ 1;

    Some(n << 12 | immr << 6 | nimms & 0x3f)
}

fn is_shifted_mask(imm: u64) -> bool {
    imm != 0 && is_mask((imm - 1) | imm)
}

fn is_mask(imm: u64) -> bool {
    imm != 0 && imm.wrapping_add(1) & imm == 0
}

pub fn fits_movz(imm: u64, register_size: u32) -> bool {
    assert!(register_size == 32 || register_size == 64);

    count_empty_half_words(imm, register_size) >= (register_size / 16 - 1)
}

pub fn fits_movn(imm: u64, register_size: u32) -> bool {
    fits_movz(!imm, register_size)
}

pub fn shift_movz(mut imm: u64) -> u32 {
    for count in 0..64 {
        if (imm & 0xFFFF) != 0 {
            return count * 16;
        }

        imm >>= 16;
    }

    0
}

pub fn shift_movn(imm: u64) -> u32 {
    shift_movz(!imm)
}

pub fn count_empty_half_words(mut imm: u64, register_size: u32) -> u32 {
    assert!(register_size == 32 || register_size == 64);

    let mut count = 0;

    for _ in 0..(register_size / 16) {
        if (imm & 0xFFFF) == 0 {
            count += 1;
        }

        imm >>= 16;
    }

    count
}

pub fn fits_addsub_imm(value: u32) -> bool {
    fits_u12(value)
}

pub fn fits_ldst_unscaled(value: i32) -> bool {
    fits_i9(value)
}

fn encode_addsub_imm(imm: u32) -> Option<(u32, u32)> {
    let mask = 0xF_FF;
    if (imm & !mask) == 0 {
        Some((0, imm))
    } else if imm & !(mask << 12) == 0 {
        Some((1, imm >> 12))
    } else {
        None
    }
}

const FLOAT_TYPE_SINGLE: u32 = 0b00;
const FLOAT_TYPE_DOUBLE: u32 = 0b01;

#[cfg(test)]
mod tests {
    use super::*;
    use byteorder::{LittleEndian, WriteBytesExt};

    macro_rules! assert_emit {
        (
            $($expr:expr),*;
            $name:ident
            (
                    $($param:expr),*
            )
        ) => {{
            let mut buf = AssemblerArm64::new();
            buf.$name($($param,)*);
            let mut expected: Vec<u8> = Vec::new();
            $(
                expected.write_u32::<LittleEndian>($expr).unwrap();
            )*
            let data = buf.finalize(4).code();

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
    fn test_encode_addsub_imm() {
        assert_eq!(encode_addsub_imm(1), Some((0, 1)));
        assert_eq!(encode_addsub_imm(1 << 12), Some((1, 1)));
        assert_eq!(encode_addsub_imm((1 << 12) + 1), None);
    }

    #[test]
    fn test_b_i() {
        assert_emit!(0x14000000; b_imm(0));
        assert_emit!(0x17FFFFFF; b_imm(-1));
        assert_emit!(0x14000001; b_imm(1));
    }

    #[test]
    fn test_tbnz_i() {
        assert_emit!(0x37080000; tbnz_imm(R0, 1, 0));
    }

    #[test]
    fn test_tbz_i() {
        assert_emit!(0x36080000; tbz_imm(R0, 1, 0));
    }

    #[test]
    fn test_br_bl_r() {
        assert_emit!(0xd61f0000; b_r(R0));
        assert_emit!(0xd61f03c0; b_r(R30));
        assert_emit!(0xd63f0000; bl_r(R0));
        assert_emit!(0xd63f03c0; bl_r(R30));
    }

    #[test]
    fn test_brk() {
        assert_emit!(0xd4200000; brk(0));
        assert_emit!(0xd43fffe0; brk(0xFFFF));
    }

    #[test]
    fn test_fcmp() {
        assert_emit!(0x1e212000; fcmp_s(F0, F1));
        assert_emit!(0x1e612000; fcmp_d(F0, F1));
        assert_emit!(0x1e252080; fcmp_s(F4, F5));
    }

    #[test]
    fn test_fcmpe() {
        assert_emit!(0x1e212010; fcmpe_s(F0, F1));
        assert_emit!(0x1e612010; fcmpe_d(F0, F1));
        assert_emit!(0x1e252090; fcmpe_s(F4, F5));
    }

    #[test]
    fn test_fabs() {
        assert_emit!(0x1e20c041; fabs_s(F1, F2));
        assert_emit!(0x1e60c083; fabs_d(F3, F4));
    }

    #[test]
    fn test_fneg() {
        assert_emit!(0x1e214041; fneg_s(F1, F2));
        assert_emit!(0x1e614083; fneg_d(F3, F4));
    }

    #[test]
    fn test_fsqrt() {
        assert_emit!(0x1e21c020; fsqrt_s(F0, F1)); // fsqrt s0, s1
        assert_emit!(0x1e61c020; fsqrt_d(F0, F1)); // fsqrt d0, d1
        assert_emit!(0x1e21c149; fsqrt_s(F9, F10)); // fsqrt s9, s10
        assert_emit!(0x1e61c149; fsqrt_d(F9, F10)); // fsqrt d9, d10
    }

    #[test]
    fn test_ret() {
        assert_emit!(0xd65f03c0; ret(REG_LR));
        assert_emit!(0xd65f0000; ret(R0));
        assert_emit!(0xd65f0140; ret(R10));
    }

    #[test]
    fn test_nop() {
        assert_emit!(0xd503201f; nop());
    }

    #[test]
    fn test_div() {
        assert_emit!(0x1ac20820; udiv_w(R0, R1, R2));
        assert_emit!(0x9ac50c83; sdiv(R3, R4, R5));
        assert_emit!(0x1ac820e6; lsl_w(R6, R7, R8));
        assert_emit!(0x1acb2549; lsr_w(R9, R10, R11));
        assert_emit!(0x1ace29ac; asrv_w(R12, R13, R14));
        assert_emit!(0x1ad12e0f; ror_w(R15, R16, R17));
    }

    #[test]
    fn test_b_cond_imm() {
        assert_eq!(0x54ffffe0, inst::b_cond_imm(Cond::EQ, -1));
        assert_eq!(0x54ffffc1, inst::b_cond_imm(Cond::NE, -2));
        assert_eq!(0x54000044, inst::b_cond_imm(Cond::MI, 2));
        assert_eq!(0x5400002b, inst::b_cond_imm(Cond::LT, 1));
    }

    #[test]
    fn test_bfm() {
        assert_emit!(0x53010820; ubfm_w(R0, R1, 1, 2));
        assert_emit!(0xd3431062; ubfm(R2, R3, 3, 4));
        assert_emit!(0x33010820; bfm_w(R0, R1, 1, 2));
        assert_emit!(0xb3431062; bfm(R2, R3, 3, 4));
        assert_emit!(0x13010820; sbfm_w(R0, R1, 1, 2));
        assert_emit!(0x93431062; sbfm(R2, R3, 3, 4));
        assert_emit!(0x53001c20; uxtb(R0, R1));

        assert_emit!(0x93400420; sbfm(R0, R1, 0, 1)); // sbfx x0, x1, #0, #2
        assert_emit!(0x13041462; sbfm_w(R2, R3, 4, 5)); // sbfx w2, w3, #4, #2
    }

    #[test]
    fn test_uxtw() {
        assert_emit!(0xD3407c00; uxtw(R0, R0));
        assert_emit!(0xD3407d8f; uxtw(R15, R12));
    }

    #[test]
    fn test_uxtb() {
        assert_emit!(0x53001c20; uxtb(R0, R1)); // uxtb w0, w1
    }

    #[test]
    fn test_sxtw() {
        assert_emit!(0x93407c00; sxtw(R0, R0));
        assert_emit!(0x93407d8f; sxtw(R15, R12));
    }

    #[test]
    fn test_lsl_imm() {
        assert_emit!(0xd37ff820; lsl_imm(R0, R1, 1)); // lsl x0, x1, #1
        assert_emit!(0x531f7820; lsl_imm_w(R0, R1, 1)); // lsl w0, w1, #1
        assert_emit!(0xd37ef462; lsl_imm(R2, R3, 2)); // lsl x2, x3, #2
        assert_emit!(0x531e7462; lsl_imm_w(R2, R3, 2)); // lsl w2, w3, #2
    }

    #[test]
    fn test_lsr_imm() {
        assert_emit!(0xd341fc20; lsr_imm(R0, R1, 1)); // lsr x0, x1, #1
        assert_emit!(0x53017c20; lsr_imm_w(R0, R1, 1)); // lsr w0, w1, #1
        assert_emit!(0xd342fc62; lsr_imm(R2, R3, 2)); // lsr x2, x3, #2
        assert_emit!(0x53027c62; lsr_imm_w(R2, R3, 2)); // lsr w2, w3, #2
    }

    #[test]
    fn test_scvtf() {
        assert_emit!(0x1e220020; scvtf_si_sw(F0, R1)); // scvtf s0, w1
        assert_emit!(0x9e220020; scvtf_si_sx(F0, R1)); // scvtf s0, x1
        assert_emit!(0x1e620020; scvtf_si_dw(F0, R1)); // scvtf d0, w1
        assert_emit!(0x9e620020; scvtf_si_dx(F0, R1)); // scvtf d0, x1
    }

    #[test]
    fn test_fcvt() {
        assert_emit!(0x1e624020; fcvt_sd(F0, F1)); // fcvt s0, d1
        assert_emit!(0x1e624062; fcvt_sd(F2, F3)); // fcvt s2, d3
        assert_emit!(0x1e22c020; fcvt_ds(F0, F1)); // fcvt d0, s1
        assert_emit!(0x1e22c062; fcvt_ds(F2, F3)); // fcvt d2, s3
    }

    #[test]
    fn test_fcvtzs() {
        assert_emit!(0x9e780020; fcvtzs_d(R0, F1)); // x0, d1
        assert_emit!(0x9e380047; fcvtzs_s(R7, F2)); // x7, s2
        assert_emit!(0x1e780020; fcvtzs_wd(R0, F1)); // w0, d1
        assert_emit!(0x1e380047; fcvtzs_ws(R7, F2)); // w7, s2
    }

    #[test]
    fn test_add_imm() {
        assert_emit!(0x11000420; add_imm_w(R0, R1, 1));
        assert_emit!(0x11400c62; add_imm_w(R2, R3, 3 << 12));
        assert_emit!(0x91000420; add_imm(R0, R1, 1));
        assert_emit!(0x91400c62; add_imm(R2, R3, 3 << 12));
    }

    #[test]
    fn test_adds_imm() {
        assert_emit!(0x31000420; adds_imm_w(R0, R1, 1));
        assert_emit!(0x31400c62; adds_imm_w(R2, R3, 3 << 12));
        assert_emit!(0xb1000420; adds_imm(R0, R1, 1));
        assert_emit!(0xb1400c62; adds_imm(R2, R3, 3 << 12));
    }

    #[test]
    fn test_sub_imm() {
        assert_emit!(0x51000420; sub_imm_w(R0, R1, 1));
        assert_emit!(0x51400c62; sub_imm_w(R2, R3, 3 << 12));
        assert_emit!(0xd1000420; sub_imm(R0, R1, 1));
        assert_emit!(0xd1400c62; sub_imm(R2, R3, 3 << 12));
        assert_emit!(0xd1000420; sub_imm(R0, R1, 1)); // sub x0, x1, #1
        assert_emit!(0x513ffc62; sub_imm_w(R2, R3, 0xFFF)); // sub w2, w3, #4095
    }

    #[test]
    fn test_subs_imm() {
        assert_emit!(0x71000420; subs_imm_w(R0, R1, 1));
        assert_emit!(0x71400c62; subs_imm_w(R2, R3, 3 << 12));
        assert_emit!(0xf1000420; subs_imm(R0, R1, 1));
        assert_emit!(0xf1400c62; subs_imm(R2, R3, 3 << 12));
    }

    #[test]
    fn test_cmp_imm() {
        assert_emit!(0x7100043f; cmp_imm_w(R1, 1));
        assert_emit!(0x71400c5f; cmp_imm_w(R2, 3 << 12));
        assert_emit!(0xf100047f; cmp_imm(R3, 1));
        assert_emit!(0xf1400c9f; cmp_imm(R4, 3 << 12));
    }

    #[test]
    fn test_fp_dataproc2() {
        assert_emit!(0x1e222820; fadd_s(F0, F1, F2));
        assert_emit!(0x1e622820; fadd_d(F0, F1, F2));
        assert_emit!(0x1e653883; fsub_d(F3, F4, F5));
        assert_emit!(0x1e6808e6; fmul_d(F6, F7, F8));
        assert_emit!(0x1e6b1949; fdiv_d(F9, F10, F11));
    }

    #[test]
    fn test_madd_msub() {
        assert_emit!(0x9b031041; madd(R1, R2, R3, R4));
        assert_emit!(0x1b0720c5; madd_w(R5, R6, R7, R8));
        assert_emit!(0x9b0bb149; msub(R9, R10, R11, R12));
        assert_emit!(0x1b0fc1cd; msub_w(R13, R14, R15, R16));
    }

    #[test]
    fn test_mul() {
        assert_emit!(0x9b037c41; mul(R1, R2, R3));
        assert_emit!(0x1b067ca4; mul_w(R4, R5, R6));
    }

    #[test]
    fn test_ldp() {
        assert_emit!(0x29400440; ldp_w(R0, R1, R2, 0));
        assert_emit!(0x294090a3; ldp_w(R3, R4, R5, 4));
        assert_emit!(0x294110a3; ldp_w(R3, R4, R5, 8));
        assert_emit!(0xa9400440; ldp(R0, R1, R2, 0));
        assert_emit!(0xa94090a3; ldp(R3, R4, R5, 8));
        assert_emit!(0xa94110a3; ldp(R3, R4, R5, 16));
    }

    #[test]
    fn test_stp() {
        assert_emit!(0x29000440; stp_w(R0, R1, R2, 0));
        assert_emit!(0x290090a3; stp_w(R3, R4, R5, 1));
        assert_emit!(0x290110a3; stp_w(R3, R4, R5, 2));
        assert_emit!(0xa9000440; stp(R0, R1, R2, 0));
        assert_emit!(0xa90090a3; stp(R3, R4, R5, 1));
        assert_emit!(0xa90110a3; stp(R3, R4, R5, 2));
    }

    #[test]
    fn test_ldst_pair_pre() {
        assert_emit!(0xa9be7bfd; stp_pre(REG_FP, REG_LR, REG_SP, -4));
        assert_emit!(0x29840440; stp_pre_w(R0, R1, R2, 8));
    }

    #[test]
    fn test_ldst_pair_post() {
        assert_emit!(0xa8fe7bfd; ldp_post(REG_FP, REG_LR, REG_SP, -4));
        assert_emit!(0x28c40440; ldp_post_w(R0, R1, R2, 8));

        assert_emit!(0xa8817fff; stp_post(REG_ZERO, REG_ZERO, REG_SP, 16)); // stp xzr, xzr, [sp], #0x10
        assert_emit!(0xa8817e1f; stp_post(REG_ZERO, REG_ZERO, R16, 16)); // stp xzr, xzr, [x16], #0x10
    }

    #[test]
    fn test_csel() {
        assert_emit!(0x1a821020; csel_w(R0, R1, R2, Cond::NE));
        assert_emit!(0x9a856083; csel(R3, R4, R5, Cond::VS));
    }

    #[test]
    fn test_csinc() {
        assert_emit!(0x1a821420; csinc_w(R0, R1, R2, Cond::NE));
        assert_emit!(0x9a856483; csinc(R3, R4, R5, Cond::VS));
    }

    #[test]
    fn test_cset() {
        assert_emit!(0x1a9f17e0; cset_w(R0, Cond::EQ));
        assert_emit!(0x9a9fc7e3; cset(R3, Cond::LE));

        assert_emit!(0x1a9f17e0; cset_w(R0, Cond::EQ));
        assert_emit!(0x1a9f07e0; cset_w(R0, Cond::NE));
        assert_emit!(0x1a9f37e0; cset_w(R0, Cond::CS));
        assert_emit!(0x1a9f37e0; cset_w(R0, Cond::HS));

        assert_emit!(0x1a9f27e0; cset_w(R0, Cond::CC));
        assert_emit!(0x1a9f27e0; cset_w(R0, Cond::LO));
        assert_emit!(0x1a9f57e0; cset_w(R0, Cond::MI));
        assert_emit!(0x1a9f47e0; cset_w(R0, Cond::PL));

        assert_emit!(0x1a9f77e0; cset_w(R0, Cond::VS));
        assert_emit!(0x1a9f67e0; cset_w(R0, Cond::VC));
        assert_emit!(0x1a9f97e0; cset_w(R0, Cond::HI));
        assert_emit!(0x1a9f87e0; cset_w(R0, Cond::LS));

        assert_emit!(0x1a9fb7e0; cset_w(R0, Cond::GE));
        assert_emit!(0x1a9fa7e0; cset_w(R0, Cond::LT));
        assert_emit!(0x1a9fd7e0; cset_w(R0, Cond::GT));
        assert_emit!(0x1a9fc7e0; cset_w(R0, Cond::LE));
    }

    #[test]
    fn test_mov_imm() {
        assert_emit!(0x12800100; movn_w(R0, 8, 0));
        assert_emit!(0x52800100; movz_w(R0, 8, 0));
        assert_emit!(0x72a00100; movk_w(R0, 8, 16));
    }

    #[test]
    fn test_adr_adrp() {
        assert_emit!(0x10ffffe0; adr_imm(R0 , -4));
        assert_emit!(0x10ffffde; adr_imm(R30, -8));
        assert_emit!(0x1000001d; adr_imm(R29,  0));
        assert_emit!(0x1000003c; adr_imm(R28,  4));
        assert_emit!(0x1000005b; adr_imm(R27,  8));
        assert_emit!(0x10000000; adr_imm(R0,  0));
        assert_emit!(0x10000001; adr_imm(R1,  0));

        assert_emit!(0x90ffffe0; adrp_imm(R0 , -4));
        assert_emit!(0x90ffffde; adrp_imm(R30, -8));
        assert_emit!(0x9000001d; adrp_imm(R29,  0));
        assert_emit!(0x9000003c; adrp_imm(R28,  4));
        assert_emit!(0x9000005b; adrp_imm(R27,  8));
        assert_emit!(0x90000000; adrp_imm(R0,  0));
        assert_emit!(0x90000001; adrp_imm(R1,  0));
    }

    #[test]
    fn test_add_extreg() {
        assert_emit!(0x8b22643f; add_ext(REG_SP, R1, R2, Extend::UXTX, 1));
        assert_emit!(0x8b226be1; add_ext(R1, REG_SP, R2, Extend::UXTX, 2));
        assert_emit!(0x0b22443f; add_ext_w(REG_SP, R1, R2, Extend::UXTW, 1));
        assert_emit!(0x0b2243e1; add_ext_w(R1, REG_SP, R2, Extend::UXTW, 0));
    }

    #[test]
    fn test_add_sh() {
        assert_emit!(0x0b030441; add_sh_w(R1, R2, R3, Shift::LSL, 1));
        assert_emit!(0x8b0608a4; add_sh(R4, R5, R6, Shift::LSL, 2));
        assert_emit!(0x0b430441; add_sh_w(R1, R2, R3, Shift::LSR, 1));
        assert_emit!(0x8b8608a4; add_sh(R4, R5, R6, Shift::ASR, 2));
    }

    #[test]
    fn test_sub_sh() {
        assert_emit!(0x4b030441; sub_sh_w(R1, R2, R3, Shift::LSL, 1));
        assert_emit!(0xcb0608a4; sub_sh(R4, R5, R6, Shift::LSL, 2));
        assert_emit!(0x4b430441; sub_sh_w(R1, R2, R3, Shift::LSR, 1));
        assert_emit!(0xcb8608a4; sub_sh(R4, R5, R6, Shift::ASR, 2));
    }

    #[test]
    fn test_adds_subs() {
        assert_emit!(0x2b030041; adds_w(R1, R2, R3));
        assert_emit!(0xeb0600a4; subs(R4, R5, R6));
        assert_emit!(0x2b830841; adds_sh_w(R1, R2, R3, Shift::ASR, 2));
        assert_emit!(0xeb060ca4; subs_sh(R4, R5, R6, Shift::LSL, 3));
    }

    #[test]
    fn test_cmp() {
        assert_emit!(0x6b0600bf; cmp_w(R5, R6));
        assert_emit!(0x6b060cbf; cmp_sh_w(R5, R6, Shift::LSL, 3));
        assert_emit!(0xeb0600bf; cmp(R5, R6));
        assert_emit!(0xeb060cbf; cmp_sh(R5, R6, Shift::LSL, 3));
    }

    #[test]
    fn test_add_reg() {
        assert_emit!(0x0b010000; add_w(R0, R0, R1));
        assert_emit!(0x8b010000; add(R0, R0, R1));
        assert_emit!(0x0b030041; add_w(R1, R2, R3));
        assert_emit!(0x8b030041; add(R1, R2, R3));
    }

    #[test]
    fn test_sub_reg() {
        assert_emit!(0x4b010000; sub_w(R0, R0, R1));
        assert_emit!(0xcb010000; sub(R0, R0, R1));
        assert_emit!(0x4b030041; sub_w(R1, R2, R3));
        assert_emit!(0xcb030041; sub(R1, R2, R3));
    }

    #[test]
    fn test_str_imm() {
        assert_emit!(0xf9000820; str_imm(R0, R1, 16)); // str x0, [x1, #16]
        assert_emit!(0x39006420; strb_imm(R0, R1, 25)); // strb w0, [x1, #25]
        assert_emit!(0x79002420; strh_imm(R0, R1, 18)); // strh w0, [x1, #18]
        assert_emit!(0xb9000c20; str_imm_w(R0, R1, 12)); // str w0, [x1, #12]
        assert_emit!(0xfd000c20; str_imm_d(F0, R1, 24)); // str d0, [x1, #24]
        assert_emit!(0xbd000420; str_imm_s(F0, R1, 4)); // str s0, [x1, #4]
    }

    #[test]
    fn test_str_reg() {
        assert_emit!(0xf8227820; str_reg(R0, R1, R2, Extend::LSL, 3)); // str x0, [x1, x2, lsl #3]
        assert_emit!(0x38226820; strb_reg(R0, R1, R2, Extend::LSL, 0)); // strb w0, [x1, x2]
        assert_emit!(0x78227820; strh_reg(R0, R1, R2, Extend::LSL, 1)); // strh w0, [x1, x2, lsl #1]
        assert_emit!(0xb8227820; str_reg_w(R0, R1, R2, Extend::LSL, 2)); // str w0, [x1, x2, lsl #2]
        assert_emit!(0xfc227820; str_reg_d(F0, R1, R2, Extend::LSL, 3)); // str d0, [x1, x2, lsl #3]
        assert_emit!(0xbc227820; str_reg_s(F0, R1, R2, Extend::LSL, 2)); // str s0, [x1, x2, lsl #2]
    }

    #[test]
    fn test_ldur() {
        assert_emit!(0xf8401020; ldur(R0, R1, 1)); // ldur x0, [x1, #1]
        assert_emit!(0x385fd020; ldurb(R0, R1, -3)); // ldurb w0, [x1, #-3]
        assert_emit!(0x78405020; ldurh(R0, R1, 5)); // ldurh w0, [x1, #5]
        assert_emit!(0xb8407020; ldur_w(R0, R1, 7)); // ldur w0, [x1, #7]
        assert_emit!(0xfc409020; ldur_d(F0, R1, 9)); // ldur d0, [x1, #9]
        assert_emit!(0xbc40b020; ldur_s(F0, R1, 11)); // ldur s0, [x1, #11]
    }

    #[test]
    fn test_stur() {
        assert_emit!(0xf8001020; stur(R0, R1, 1)); // stur x0, [x1, #1]
        assert_emit!(0x381fd020; sturb(R0, R1, -3)); // sturb w0, [x1, #-3]
        assert_emit!(0x78005020; sturh(R0, R1, 5)); // sturh w0, [x1, #5]
        assert_emit!(0xb8007020; stur_w(R0, R1, 7)); // stur w0, [x1, #7]
        assert_emit!(0xfc009020; stur_d(F0, R1, 9)); // stur d0, [x1, #9]
        assert_emit!(0xbc00b020; stur_s(F0, R1, 11)); // stur s0, [x1, #11]
    }

    #[test]
    fn test_ldr_reg() {
        assert_emit!(0x38626820; ldrb_reg(R0, R1, R2, Extend::LSL, 0));
        assert_emit!(0x38656883; ldrb_reg(R3, R4, R5, Extend::LSL, 0));

        assert_emit!(0x78626820; ldrh_reg(R0, R1, R2, Extend::LSL, 0));
        assert_emit!(0x78656883; ldrh_reg(R3, R4, R5, Extend::LSL, 0));

        assert_emit!(0xb8626820; ldr_reg_w(R0, R1, R2, Extend::LSL, 0));
        assert_emit!(0xb8657883; ldr_reg_w(R3, R4, R5, Extend::LSL, 2));
        assert_emit!(0xb86858e6; ldr_reg_w(R6, R7, R8, Extend::UXTW, 2));
        assert_emit!(0xb86bd949; ldr_reg_w(R9, R10, R11, Extend::SXTW, 2));

        assert_emit!(0xf8626820; ldr(R0, MemOperand::regoffset(R1, R2, Extend::LSL, 0)));
        assert_emit!(0xf8657883; ldr(R3, MemOperand::regoffset(R4, R5, Extend::LSL, 3)));
        assert_emit!(0xf86858e6; ldr(R6, MemOperand::regoffset(R7, R8, Extend::UXTW, 3)));
        assert_emit!(0xf86bd949; ldr(R9, MemOperand::regoffset(R10, R11, Extend::SXTW, 3)));

        assert_emit!(0xf8627820; ldr(R0, MemOperand::regoffset(R1, R2, Extend::LSL, 3))); // ldr x0, [x1, x2, lsl #3]
        assert_emit!(0x38626820; ldrb_reg(R0, R1, R2, Extend::LSL, 0)); // ldrb w0, [x1, x2]
        assert_emit!(0x78627820; ldrh_reg(R0, R1, R2, Extend::LSL, 1)); // ldrh w0, [x1, x2, lsl #1]
        assert_emit!(0xb8627820; ldr_reg_w(R0, R1, R2, Extend::LSL, 2)); // ldr w0, [x1, x2, lsl #2]
        assert_emit!(0xfc627820; ldr_reg_d(F0, R1, R2, Extend::LSL, 3)); // ldr d0, [x1, x2, lsl #3]
        assert_emit!(0xbc627820; ldr_reg_s(F0, R1, R2, Extend::LSL, 2)); // ldr s0, [x1, x2, lsl #2]
    }

    #[test]
    fn test_ldr_imm() {
        assert_emit!(0xf9400820; ldr(R0, MemOperand::offset(R1, 16))); // ldr x0, [x1, #16]
        assert_emit!(0x39406420; ldrb_imm(R0, R1, 25)); // ldrb w0, [x1, #25]
        assert_emit!(0x79402420; ldrh_imm(R0, R1, 18)); // ldrh w0, [x1, #18]
        assert_emit!(0xb9400c20; ldr_imm_w(R0, R1, 12)); // ldr w0, [x1, #12]
        assert_emit!(0xfd400c20; ldr_imm_d(F0, R1, 24)); // ldr d0, [x1, #24]
        assert_emit!(0xbd400420; ldr_imm_s(F0, R1, 4)); // ldr s0, [x1, #4]
    }

    #[test]
    fn test_logical_shreg() {
        assert_emit!(0x0a020420; and_sh_w(R0, R1, R2, Shift::LSL, 1));
        assert_emit!(0x0a650883; bic_sh_w(R3, R4, R5, Shift::LSR, 2));
        assert_emit!(0xaa880ce6; orr_sh(R6, R7, R8, Shift::ASR, 3));
        assert_emit!(0xaaeb1149; orn_sh(R9, R10, R11, Shift::ROR, 4));
        assert_emit!(0xca0e15ac; eor_sh(R12, R13, R14, Shift::LSL, 5));
        assert_emit!(0xca711a0f; eon_sh(R15, R16, R17, Shift::LSR, 6));
        assert_emit!(0xea941e72; ands_sh(R18, R19, R20, Shift::ASR, 7));
        assert_emit!(0xeaf726d5; bics_sh(R21, R22, R23, Shift::ROR, 9));
    }

    #[test]
    fn test_is_shifted_mask() {
        assert_eq!(true, is_shifted_mask(8));
        assert_eq!(true, is_shifted_mask(6));
        assert_eq!(true, is_shifted_mask(7));
        assert_eq!(true, is_shifted_mask(3));
        assert_eq!(true, is_shifted_mask(1));
        assert_eq!(true, is_shifted_mask(!0));

        assert_eq!(false, is_shifted_mask(0));
        assert_eq!(false, is_shifted_mask(9));
        assert_eq!(false, is_shifted_mask(1 << 63 | 1));
    }

    #[test]
    fn test_and_imm() {
        assert_emit!(0x12000020; and_imm_w(R0, R1, 1)); // and w0, w1, #1
        assert_emit!(0x92400020; and_imm(R0, R1, 1)); // and x0, x1, #1
        assert_emit!(0x1201f062; and_imm_w(R2, R3, 0xaaaaaaaa)); // and w2, w3, #aaaaaaaa

        // and x2, x3, #aaaaaaaaaaaaaaaa
        assert_emit!(0x9201f062; and_imm(R2, R3, 0xaaaaaaaaaaaaaaaa));
    }

    #[test]
    fn test_ldar() {
        assert_emit!(0xc8dfffff; ldar(REG_ZERO, REG_SP));
        assert_emit!(0xc8dffc20; ldar(R0, R1));

        assert_emit!(0x08dfffff; ldarb(REG_ZERO, REG_SP));
        assert_emit!(0x08dffc20; ldarb(R0, R1));

        assert_emit!(0x48dfffff; ldarh(REG_ZERO, REG_SP));
        assert_emit!(0x48dffc20; ldarh(R0, R1));

        assert_emit!(0x88dfffff; ldar_w(REG_ZERO, REG_SP));
        assert_emit!(0x88dffc20; ldar_w(R0, R1));
    }

    #[test]
    fn test_stlr() {
        assert_emit!(0xc89fffff; stlr(REG_ZERO, REG_SP));
        assert_emit!(0xc89ffc20; stlr(R0, R1));

        assert_emit!(0x089fffff; stlrb(REG_ZERO, REG_SP));
        assert_emit!(0x089ffc20; stlrb(R0, R1));

        assert_emit!(0x489fffff; stlrh(REG_ZERO, REG_SP));
        assert_emit!(0x489ffc20; stlrh(R0, R1));

        assert_emit!(0x889fffff; stlr_w(REG_ZERO, REG_SP));
        assert_emit!(0x889ffc20; stlr_w(R0, R1));
    }

    #[test]
    fn test_ldxr() {
        assert_emit!(0xc85f7fff; ldxr(REG_ZERO, REG_SP));
        assert_emit!(0xc85f7c20; ldxr(R0, R1));

        assert_emit!(0x885f7fff; ldxr_w(REG_ZERO, REG_SP));
        assert_emit!(0x885f7c20; ldxr_w(R0, R1));
    }

    #[test]
    fn test_ldaxr() {
        assert_emit!(0xc85fffff; ldaxr(REG_ZERO, REG_SP));
        assert_emit!(0xc85ffc20; ldaxr(R0, R1));

        assert_emit!(0x885fffff; ldaxr_w(REG_ZERO, REG_SP));
        assert_emit!(0x885ffc20; ldaxr_w(R0, R1));
    }

    #[test]
    fn test_stxr() {
        assert_emit!(0xc8007fff; stxr(R0, REG_ZERO, REG_SP));
        assert_emit!(0xc8047c41; stxr(R4, R1, R2));

        assert_emit!(0x88027fff; stxr_w(R2, REG_ZERO, REG_SP));
        assert_emit!(0x88077cc1; stxr_w(R7, R1, R6));
    }

    #[test]
    fn test_stlxr() {
        assert_emit!(0xc800ffff; stlxr(R0, REG_ZERO, REG_SP));
        assert_emit!(0xc804fc41; stlxr(R4, R1, R2));

        assert_emit!(0x8802ffff; stlxr_w(R2, REG_ZERO, REG_SP));
        assert_emit!(0x8807fcc1; stlxr_w(R7, R1, R6));
    }

    #[test]
    fn test_cas() {
        assert_emit!(0xc8a77d28; cas(R7, R8, R9));
        assert_emit!(0xc8e77d28; casa(R7, R8, R9));
        assert_emit!(0xc8e7fd28; casal(R7, R8, R9));
        assert_emit!(0xc8a7fd28; casl(R7, R8, R9));

        assert_emit!(0x88a77d28; cas_w(R7, R8, R9));
        assert_emit!(0x88e77d28; casa_w(R7, R8, R9));
        assert_emit!(0x88e7fd28; casal_w(R7, R8, R9));
        assert_emit!(0x88a7fd28; casl_w(R7, R8, R9));
    }

    #[test]
    fn test_ldadd() {
        assert_emit!(0xf8270128; ldadd(R7, R8, R9));
        assert_emit!(0xf8a70128; ldadda(R7, R8, R9));
        assert_emit!(0xf8e70128; ldaddal(R7, R8, R9));
        assert_emit!(0xf8670128; ldaddl(R7, R8, R9));

        assert_emit!(0xb8270128; ldadd_w(R7, R8, R9));
        assert_emit!(0xb8a70128; ldadda_w(R7, R8, R9));
        assert_emit!(0xb8e70128; ldaddal_w(R7, R8, R9));
        assert_emit!(0xb8670128; ldaddl_w(R7, R8, R9));
    }

    #[test]
    fn test_swp() {
        assert_emit!(0xf8278128; swp(R7, R8, R9));
        assert_emit!(0xf8a78128; swpa(R7, R8, R9));
        assert_emit!(0xf8e78128; swpal(R7, R8, R9));
        assert_emit!(0xf8678128; swpl(R7, R8, R9));

        assert_emit!(0xb8278128; swp_w(R7, R8, R9));
        assert_emit!(0xb8a78128; swpa_w(R7, R8, R9));
        assert_emit!(0xb8e78128; swpal_w(R7, R8, R9));
        assert_emit!(0xb8678128; swpl_w(R7, R8, R9));
    }

    #[test]
    fn test_encode_logical_imm() {
        assert_eq!(None, encode_logical_imm(0, 64));
        assert_eq!(None, encode_logical_imm(!0, 64));

        assert_eq!(None, encode_logical_imm(0, 32));
        assert_eq!(None, encode_logical_imm((1 << 32) - 1, 32));

        assert_eq!(Some(0b0_000000_111100), encode_logical_imm(0x55555555, 32));
        assert_eq!(
            Some(0b0_000000_111100),
            encode_logical_imm(0x5555555555555555, 64)
        );
        assert_eq!(Some(0b0_000001_111100), encode_logical_imm(0xaaaaaaaa, 32));
        assert_eq!(
            Some(0b0_000001_111100),
            encode_logical_imm(0xaaaaaaaaaaaaaaaa, 64)
        );
        assert_eq!(Some(0b0_000000_111000), encode_logical_imm(0x11111111, 32));
        assert_eq!(Some(0b0_000001_111000), encode_logical_imm(0x88888888, 32));
        assert_eq!(Some(0b0_000001_111001), encode_logical_imm(0x99999999, 32));
    }

    #[test]
    fn test_is_mask() {
        assert_eq!(true, is_mask(7));
        assert_eq!(true, is_mask(3));
        assert_eq!(true, is_mask(1));

        assert_eq!(false, is_mask(0));
        assert_eq!(false, is_mask(8));

        assert_eq!(true, is_mask(!0));
    }

    #[test]
    fn test_count_empty_half_words() {
        assert_eq!(4, count_empty_half_words(0, 64));
        assert_eq!(3, count_empty_half_words(1, 64));
        assert_eq!(3, count_empty_half_words(1u64 << 16, 64));
        assert_eq!(3, count_empty_half_words(1u64 << 32, 64));
        assert_eq!(3, count_empty_half_words(1u64 << 48, 64));

        assert_eq!(2, count_empty_half_words(0, 32));
        assert_eq!(1, count_empty_half_words(1, 32));
        assert_eq!(1, count_empty_half_words(1u64 << 16, 32));
    }

    #[test]
    fn test_fits_movz() {
        assert!(fits_movz(1, 64));
        assert!(fits_movz(0xFFFF, 64));
        assert!(fits_movz(0xFFFFu64 << 16, 64));
        assert!(!fits_movz(0x1FFFF, 64));
        assert!(fits_movz(1u64 << 16, 64));
        assert!(!fits_movz(0x10001, 64));

        assert!(fits_movz(1, 32));
        assert!(fits_movz(0xFFFF, 32));
        assert!(fits_movz(0xFFFFu64 << 16, 32));
        assert!(!fits_movz(0x1FFFF, 32));
    }

    #[test]
    fn test_fits_movn() {
        let min1 = !0u64;

        assert!(fits_movn(min1, 64));
        assert!(fits_movn(min1, 32));
        assert!(fits_movn(min1 << 2, 32));
        assert!(fits_movn(min1 << 16, 32));
        assert!(!fits_movn(min1 << 17, 32));
    }

    #[test]
    fn test_shift_movz() {
        assert_eq!(0, shift_movz(0));
        assert_eq!(0, shift_movz(1));
        assert_eq!(0, shift_movz(0xFFFF));
        assert_eq!(16, shift_movz(0x10000));
        assert_eq!(32, shift_movz(0x100000000));
    }

    #[test]
    fn test_shift_movn() {
        assert_eq!(0, shift_movn(!0u64));
        assert_eq!(0, shift_movn(0));
        assert_eq!(0, shift_movn(1));
        assert_eq!(16, shift_movn(0xFFFF));
        assert_eq!(32, shift_movn(0xFFFFFFFF));
    }

    #[test]
    fn test_fdiv() {
        assert_emit!(0x1e221820; fdiv_s(F0, F1, F2)); // fdiv s0, s1, s2
        assert_emit!(0x1e651883; fdiv_d(F3, F4, F5)); // fdiv d3, d4, d5
    }

    #[test]
    fn test_fmov() {
        assert_emit!(0x1e204020; fmov_s(F0, F1)); // fmov s0, s1
        assert_emit!(0x1e604062; fmov_d(F2, F3)); // fmov d2, d3
    }

    #[test]
    fn test_fmov_fs() {
        assert_emit!(0x1e270020; fmov_fs_s(F0, R1)); // fmov s0, w1
        assert_emit!(0x9e670062; fmov_fs_d(F2, R3)); // fmov d2, x3
    }

    #[test]
    fn test_fmov_sf() {
        assert_emit!(0x1e260020; fmov_sf_s(R0, F1)); // fmov w0, s1
        assert_emit!(0x9e660062; fmov_sf_d(R2, F3)); // fmov x2, d3
    }

    #[test]
    fn test_frintn() {
        assert_emit!(0x1e244020; frintn_s(F0, F1)); // frintn s0, s1
        assert_emit!(0x1e644062; frintn_d(F2, F3)); // frintn d2, d3
    }

    #[test]
    fn test_frintp() {
        assert_emit!(0x1e24c020; frintp_s(F0, F1)); // frintp s0, s1
        assert_emit!(0x1e64c062; frintp_d(F2, F3)); // frintp d2, d3
    }

    #[test]
    fn test_frintm() {
        assert_emit!(0x1e254020; frintm_s(F0, F1)); // frintm s0, s1
        assert_emit!(0x1e654062; frintm_d(F2, F3)); // frintm d2, d3
    }

    #[test]
    fn test_frintz() {
        assert_emit!(0x1e25c020; frintz_s(F0, F1)); // frintz s0, s1
        assert_emit!(0x1e65c062; frintz_d(F2, F3)); // frintz d2, d3
    }

    #[test]
    fn test_frinta() {
        assert_emit!(0x1e264020; frinta_s(F0, F1)); // frinta s0, s1
        assert_emit!(0x1e664062; frinta_d(F2, F3)); // frinta d2, d3
    }

    #[test]
    fn test_fsub() {
        assert_emit!(0x1e223820; fsub_s(F0, F1, F2)); // fsub s0, s1, s2
        assert_emit!(0x1e653883; fsub_d(F3, F4, F5)); // fsub d3, d4, d5
    }

    #[test]
    fn test_lsl() {
        assert_emit!(0x9ac22020; lsl(R0, R1, R2)); // lsl x0, x1, x2
        assert_emit!(0x1ac42062; lsl_w(R2, R3, R4)); // lsl w2, w3, w4
    }

    #[test]
    fn test_lsr() {
        assert_emit!(0x9ac22420; lsr(R0, R1, R2)); // lsr x0, x1, x2
        assert_emit!(0x1ac42462; lsr_w(R2, R3, R4)); // lsr w2, w3, w4
    }

    #[test]
    fn test_mov() {
        assert_emit!(0x910003e0; mov(R0, REG_SP)); // mov x0, sp
        assert_emit!(0x9100001f; mov(REG_SP, R0)); // mov sp, x0
        assert_emit!(0xaa0203e1; mov(R1, R2)); // mov x1, x2
        assert_emit!(0x110003e0; mov_w(R0, REG_SP)); // mov w0, wsp
        assert_emit!(0x1100001f; mov_w(REG_SP, R0)); // mov wsp, w0
        assert_emit!(0x2a0203e1; mov_w(R1, R2)); // mov w1, w2
    }

    #[test]
    fn test_movn() {
        assert_emit!(0x929fffe0; movn(R0, 0xFFFF, 0)); // mov x0, #-65536
        assert_emit!(0x92bfffe1; movn(R1, 0xFFFF, 16)); // mov x1, #-4294901761
        assert_emit!(0x92ffffe2; movn(R2, 0xFFFF, 48)); // mov x2, #281474976710655
        assert_emit!(0x129fffe0; movn_w(R0, 0xFFFF, 0)); // movn w0, #65535
        assert_emit!(0x12bfffe1; movn_w(R1, 0xFFFF, 16)); // movn w1, #65535, lsl #16
    }

    #[test]
    fn test_movz() {
        assert_emit!(0xd29fffe0; movz(R0, 0xFFFF, 0)); // mov x0, #65535
        assert_emit!(0xd2bfffe1; movz(R1, 0xFFFF, 16)); // mov x1, #4294901760
        assert_emit!(0xd2ffffe2; movz(R2, 0xFFFF, 48)); // mov x2, #-281474976710656
        assert_emit!(0x529fffe0; movz_w(R0, 0xFFFF, 0)); // mov w0, #65535
        assert_emit!(0x52bfffe1; movz_w(R1, 0xFFFF, 16)); // mov w1, #-65536
    }

    #[test]
    fn test_movk() {
        assert_emit!(0xf2ffffe0; movk(R0, 0xFFFF, 48)); // movk x0, #65535, lsl #48
        assert_emit!(0x72bfffe1;  movk_w(R1, 0xFFFF, 16)); // movk w1, #65535, lsl #16
    }

    #[test]
    fn test_orr() {
        assert_emit!(0xaa020020; orr_sh(R0, R1, R2, Shift::LSL, 0)); // orr x0, x1, x2
        assert_emit!(0x2a0608a4; orr_sh_w(R4, R5, R6, Shift::LSL, 2)); // orr w4, w5, w6, lsl #2
    }

    #[test]
    fn test_orn() {
        assert_emit!(0xaa220020; orn_sh(R0, R1, R2, Shift::LSL, 0)); // orn x0, x1, x2
        assert_emit!(0x2a2608a4; orn_sh_w(R4, R5, R6, Shift::LSL, 2)); // orn w4, w5, w6, lsl #2
    }

    #[test]
    fn test_rbit() {
        assert_emit!(0xdac00020; rbit(R0, R1)); // rbit x0, x1
        assert_emit!(0x5ac00062; rbit_w(R2, R3)); // rbit w2, w3
    }

    #[test]
    fn test_rev() {
        assert_emit!(0xdac00c20; rev(R0, R1)); // rev x0, x1
        assert_emit!(0x5ac00862; rev_w(R2, R3)); // rev w2, w3
    }

    #[test]
    fn test_ror() {
        assert_emit!(0x9ac22c20; ror(R0, R1, R2)); // ror x0, x1, x2
        assert_emit!(0x1ac52c83; ror_w(R3, R4, R5)); // ror w3, w4, w5
    }

    #[test]
    fn test_smaddl() {
        assert_emit!(0x9b220c20; smaddl(R0, R1, R2, R3)); // smaddl x0, w1, w2, x3
    }

    #[test]
    fn test_smull() {
        assert_emit!(0x9b227c20; smull(R0, R1, R2)); // smull x0, w1, w2
    }

    #[test]
    fn test_smulh() {
        assert_emit!(0x9b427c20; smulh(R0, R1, R2)); // smulh x0, x1, x2
    }

    #[test]
    fn test_sub() {
        assert_emit!(0xcb2063ff; sub(REG_SP, REG_SP, R0)); // sub sp, sp, x0
        assert_emit!(0xcb020020; sub(R0, R1, R2)); // sub x0, x1, x2
        assert_emit!(0x4b2043ff; sub_w(REG_SP, REG_SP, R0)); // sub wsp, wsp, w0
        assert_emit!(0x4b050083; sub_w(R3, R4, R5)); // sub w3, w4, w5
    }

    #[test]
    fn test_sub_ext() {
        assert_emit!(0xcb226820; sub_ext(R0, R1, R2, Extend::UXTX, 2)); // sub x0, x1, x2, uxtx #2
        assert_emit!(0x4b244462; sub_ext_w(R2, R3, R4, Extend::UXTW, 1)); // sub w2, w3, w4, uxtw #1
    }

    #[test]
    fn test_ubfm() {
        assert_emit!(0xd3410820; ubfm(R0, R1, 1, 2)); // ubfx x0, x1, #1, #2
        assert_emit!(0x53031062; ubfm_w(R2, R3, 3, 4)); // ubfx w2, w3, #3, #2
    }

    #[test]
    fn test_udiv() {
        assert_emit!(0x9ac20820; udiv(R0, R1, R2)); // udiv x0, x1, x2
        assert_emit!(0x1ac50883; udiv_w(R3, R4, R5)); // udiv w3, w4, w5
    }

    #[test]
    fn test_dmb_ish() {
        assert_emit!(0xd5033bbf; dmb_ish());
    }
}
