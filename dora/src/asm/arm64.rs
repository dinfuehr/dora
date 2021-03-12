use crate::asm::{Assembler, Label, Register};
use crate::cpu::arm64::asm;
use crate::cpu::Reg;

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
pub const REG_ZERO: Register = Register(31);
pub const REG_SP: Register = Register(32);

impl Register {
    fn value(self) -> u8 {
        self.0
    }

    pub fn to_reg(self) -> Reg {
        assert!(self.0 <= 30);
        Reg(self.0)
    }
}

pub(super) enum JumpKind {
    Unconditional,
    Conditional(asm::Cond),
    NonZero(bool, Register),
}

impl Assembler {
    pub(super) fn resolve_jumps(&mut self) {
        let unresolved_jumps = std::mem::replace(&mut self.unresolved_jumps, Vec::new());

        for (pc, lbl, kind) in unresolved_jumps {
            if let Some(lbl_offset) = self.offset(lbl) {
                let distance: i32 = lbl_offset as i32 - pc as i32;
                assert!(distance % 4 == 0);
                let distance = distance / 4;

                match kind {
                    JumpKind::Conditional(cond) => {
                        self.patch_u32(pc, asm::b_cond_imm(cond.into(), distance));
                    }

                    JumpKind::Unconditional => {
                        self.patch_u32(pc, asm::b_imm(distance));
                    }

                    JumpKind::NonZero(sf, rt) => {
                        let sf = if sf { 1 } else { 0 };
                        self.patch_u32(pc, asm::cbnz(sf, rt.to_reg(), distance));
                    }
                }
            } else {
                panic!("unbound label");
            }
        }
    }

    pub fn b(&mut self, target: Label) {
        let value = self.offset(target);

        match value {
            Some(target_offset) => {
                let diff = -(self.pc() as i32 - target_offset as i32);
                assert!(diff % 4 == 0);
                self.emit_u32(asm::b_imm(diff / 4));
            }

            None => {
                let pos = self.pc() as u32;
                self.emit_u32(0);
                self.unresolved_jumps
                    .push((pos, target, JumpKind::Unconditional));
            }
        }
    }

    pub fn bc(&mut self, cond: asm::Cond, target: Label) {
        let value = self.offset(target);

        match value {
            Some(target_offset) => {
                let diff = -(self.pc() as i32 - target_offset as i32);
                assert!(diff % 4 == 0);
                self.emit_u32(asm::b_cond_imm(cond.into(), diff / 4));
            }

            None => {
                let pos = self.pc() as u32;
                self.emit_u32(0);
                self.unresolved_jumps
                    .push((pos, target, JumpKind::Conditional(cond)));
            }
        }
    }

    pub fn cbnzx(&mut self, reg: Register, target: Label) {
        let value = self.offset(target);

        match value {
            Some(target_offset) => {
                let diff = -(self.pc() as i32 - target_offset as i32);
                assert!(diff % 4 == 0);
                self.emit_u32(asm::cbnz(1, Reg(reg.value()), diff / 4));
            }

            None => {
                let pos = self.pc() as u32;
                self.emit_u32(0);
                self.unresolved_jumps
                    .push((pos, target, JumpKind::NonZero(true, reg)));
            }
        }
    }

    pub fn nop(&mut self) {
        self.emit_u32(cls_system(0));
    }
}

fn cls_system(imm: u32) -> u32 {
    assert!(fits_u7(imm));

    0xD503201F | imm << 5
}

fn fits_u7(imm: u32) -> bool {
    imm < 128
}

#[cfg(test)]
mod tests {
    use crate::asm::*;
    use byteorder::{LittleEndian, WriteBytesExt};

    macro_rules! assert_emit {
        (
            $($expr:expr),*;
            $name:ident
            (
                    $($param:expr),*
            )
        ) => {{
            let mut buf = Assembler::new();
            buf.$name($($param,)*);
            let mut expected: Vec<u8> = Vec::new();
            $(
                expected.write_u32::<LittleEndian>($expr).unwrap();
            )*
            let data = buf.code();

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
    fn test_nop() {
        assert_emit!(0xd503201f; nop());
    }
}
