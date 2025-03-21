use crate::{AssemblerBuffer, Label};
use std::convert::TryInto;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Register(u8);

impl Register {
    pub fn new(value: u8) -> Register {
        assert!(value < 16);
        Register(value)
    }

    fn low_bits(self) -> u8 {
        self.0 & 0b111
    }

    fn value(self) -> u8 {
        self.0
    }

    fn needs_rex(self) -> bool {
        self.0 > 7
    }
}

pub const RAX: Register = Register(0);
pub const RCX: Register = Register(1);
pub const RDX: Register = Register(2);
pub const RBX: Register = Register(3);
pub const RSP: Register = Register(4);
pub const RBP: Register = Register(5);
pub const RSI: Register = Register(6);
pub const RDI: Register = Register(7);

pub const R8: Register = Register(8);
pub const R9: Register = Register(9);
pub const R10: Register = Register(10);
pub const R11: Register = Register(11);
pub const R12: Register = Register(12);
pub const R13: Register = Register(13);
pub const R14: Register = Register(14);
pub const R15: Register = Register(15);

pub const XMM0: XmmRegister = XmmRegister(0);
pub const XMM1: XmmRegister = XmmRegister(1);
pub const XMM2: XmmRegister = XmmRegister(2);
pub const XMM3: XmmRegister = XmmRegister(3);
pub const XMM4: XmmRegister = XmmRegister(4);
pub const XMM5: XmmRegister = XmmRegister(5);
pub const XMM6: XmmRegister = XmmRegister(6);
pub const XMM7: XmmRegister = XmmRegister(7);

pub const XMM8: XmmRegister = XmmRegister(8);
pub const XMM9: XmmRegister = XmmRegister(9);
pub const XMM10: XmmRegister = XmmRegister(10);
pub const XMM11: XmmRegister = XmmRegister(11);
pub const XMM12: XmmRegister = XmmRegister(12);
pub const XMM13: XmmRegister = XmmRegister(13);
pub const XMM14: XmmRegister = XmmRegister(14);
pub const XMM15: XmmRegister = XmmRegister(15);

struct ForwardJump {
    offset: u32,
    label: Label,
    distance: JumpDistance,
}

pub enum JumpDistance {
    Near,
    Far,
}

pub struct AssemblerX64 {
    unresolved_jumps: Vec<ForwardJump>,
    buffer: AssemblerBuffer,
    has_avx2: bool,
}

impl AssemblerX64 {
    pub fn new(has_avx2: bool) -> AssemblerX64 {
        AssemblerX64 {
            unresolved_jumps: Vec::new(),
            buffer: AssemblerBuffer::new(),
            has_avx2,
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
            self.int3();
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

impl AssemblerX64 {
    fn resolve_jumps(&mut self) {
        let unresolved_jumps = std::mem::replace(&mut self.unresolved_jumps, Vec::new());

        let old_position = self.position();

        for jump in unresolved_jumps {
            let lbl_offset = self.offset(jump.label).expect("unbound label");
            self.set_position(jump.offset as usize);

            match jump.distance {
                JumpDistance::Near => {
                    let distance: i32 = lbl_offset as i32 - (jump.offset as i32 + 1);
                    assert!(-128 <= distance && distance < 128);
                    self.emit_u8(distance as u8);
                }

                JumpDistance::Far => {
                    let distance: i32 = lbl_offset as i32 - (jump.offset as i32 + 4);
                    self.emit_u32(distance as u32);
                }
            }
        }

        self.set_position(old_position);
    }

    pub fn addl_ri(&mut self, dest: Register, imm: Immediate) {
        self.emit_alu32_imm(dest, imm, 0b000, 0x05);
    }

    pub fn addl_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex32_optional(src, dest);
        self.emit_u8(0x01);
        self.emit_modrm(0b11, src.low_bits(), dest.low_bits());
    }

    pub fn addq_ri(&mut self, dest: Register, imm: Immediate) {
        self.emit_alu64_imm(dest, imm, 0b000, 0x05);
    }

    pub fn addq_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex64_modrm(src, dest);
        self.emit_u8(0x01);
        self.emit_modrm(0b11, src.low_bits(), dest.low_bits());
    }

    pub fn addss_rr(&mut self, dest: XmmRegister, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf3);
        self.emit_rex_sse_modrm_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0x58);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn addsd_rr(&mut self, dest: XmmRegister, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf2);
        self.emit_rex_sse_modrm_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0x58);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn andl_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex32_optional(src, dest);
        self.emit_u8(0x21);
        self.emit_modrm(0b11, src.low_bits(), dest.low_bits());
    }

    pub fn andps_ra(&mut self, dest: XmmRegister, src: Address) {
        debug_assert!(!self.has_avx2);
        self.emit_rex_sse_address_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0x54);
        self.emit_address(dest.low_bits(), src);
    }

    pub fn andps_rl(&mut self, dest: XmmRegister, src: Label) {
        debug_assert!(!self.has_avx2);
        if dest.needs_rex() {
            self.emit_rex(false, true, false, false);
        }
        self.emit_u8(0x0f);
        self.emit_u8(0x54);
        self.emit_label_address(dest.low_bits(), src);
    }

    pub fn andq_ri(&mut self, dest: Register, imm: Immediate) {
        self.emit_alu64_imm(dest, imm, 0b100, 0x25);
    }

    pub fn andq_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex64_modrm(src, dest);
        self.emit_u8(0x21);
        self.emit_modrm(0b11, src.low_bits(), dest.low_bits());
    }

    pub fn call_r(&mut self, reg: Register) {
        self.emit_rex32_rm_optional(reg);
        self.emit_u8(0xFF);
        self.emit_modrm(0b11, 0b010, reg.low_bits());
    }

    pub fn cdq(&mut self) {
        self.emit_u8(0x99);
    }

    pub fn cmovl(&mut self, condition: Condition, dest: Register, src: Register) {
        self.emit_rex32_optional(dest, src);
        self.emit_u8(0x0F);
        self.emit_u8((0x40 + condition.int()) as u8);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn cmovq(&mut self, condition: Condition, dest: Register, src: Register) {
        self.emit_rex64_modrm(dest, src);
        self.emit_u8(0x0F);
        self.emit_u8((0x40 + condition.int()) as u8);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn cmpb_ai(&mut self, lhs: Address, rhs: Immediate) {
        assert!(rhs.is_int8() || rhs.is_uint8());
        self.emit_rex32_address_optional(lhs);
        self.emit_u8(0x80);
        self.emit_address(0b111, lhs);
        self.emit_u8(rhs.uint8());
    }

    pub fn cmpb_ar(&mut self, lhs: Address, rhs: Register) {
        self.emit_rex32_byte_address_optional(rhs, lhs);
        self.emit_u8(0x38);
        self.emit_address(rhs.low_bits(), lhs);
    }

    pub fn cmpb_rr(&mut self, lhs: Register, rhs: Register) {
        self.emit_rex8_modrm_optional(rhs, lhs);
        self.emit_u8(0x38u8);
        self.emit_modrm(0b11, rhs.low_bits(), lhs.low_bits());
    }

    pub fn cmpl_ai(&mut self, lhs: Address, rhs: Immediate) {
        assert!(rhs.is_int32() || rhs.is_uint32());

        if rhs.is_int8() {
            self.emit_rex32_address_optional(lhs);
            self.emit_u8(0x83);
            self.emit_address(0b111, lhs);
            self.emit_u8(rhs.int8() as u8);
        } else {
            self.emit_rex32_address_optional(lhs);
            self.emit_u8(0x81);
            self.emit_address(0b111, lhs);
            self.emit_u32(rhs.uint32());
        }
    }

    pub fn cmpl_ar(&mut self, lhs: Address, rhs: Register) {
        self.emit_rex32_modrm_address_optional(rhs, lhs);
        self.emit_u8(0x39);
        self.emit_address(rhs.low_bits(), lhs);
    }

    pub fn cmpl_ri(&mut self, reg: Register, imm: Immediate) {
        self.emit_alu32_imm(reg, imm, 0b111, 0x3d);
    }

    pub fn cmpl_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex32_optional(src, dest);
        self.emit_u8(0x39);
        self.emit_modrm(0b11, src.low_bits(), dest.low_bits());
    }

    pub fn cmpq_ai(&mut self, lhs: Address, rhs: Immediate) {
        assert!(rhs.is_int32());

        if rhs.is_int8() {
            self.emit_rex64_address(lhs);
            self.emit_u8(0x83);
            self.emit_address(0b111, lhs);
            self.emit_u8(rhs.int8() as u8);
        } else {
            self.emit_rex64_address(lhs);
            self.emit_u8(0x81);
            self.emit_address(0b111, lhs);
            self.emit_u32(rhs.int32() as u32);
        }
    }

    pub fn cmpq_ar(&mut self, lhs: Address, rhs: Register) {
        self.emit_rex64_modrm_address(rhs, lhs);
        self.emit_u8(0x39);
        self.emit_address(rhs.low_bits(), lhs);
    }

    pub fn cmpq_ri(&mut self, reg: Register, imm: Immediate) {
        self.emit_alu64_imm(reg, imm, 0b111, 0x3d);
    }

    pub fn cmpq_rr(&mut self, lhs: Register, rhs: Register) {
        self.emit_rex64_modrm(rhs, lhs);
        self.emit_u8(0x39);
        self.emit_modrm(0b11, rhs.low_bits(), lhs.low_bits());
    }

    pub fn cmpxchgl_ar(&mut self, dest: Address, src: Register) {
        self.emit_rex32_modrm_address_optional(src, dest);
        self.emit_u8(0x0f);
        self.emit_u8(0xb1);
        self.emit_address(src.low_bits(), dest);
    }

    pub fn cmpxchgq_ar(&mut self, dest: Address, src: Register) {
        self.emit_rex64_modrm_address(src, dest);
        self.emit_u8(0x0f);
        self.emit_u8(0xb1);
        self.emit_address(src.low_bits(), dest);
    }

    pub fn cqo(&mut self) {
        self.emit_rex(true, false, false, false);
        self.emit_u8(0x99);
    }

    pub fn cvtsd2ss_rr(&mut self, dest: XmmRegister, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf2);
        self.emit_rex_sse_modrm_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0x5a);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn cvtsi2sdd_rr(&mut self, dest: XmmRegister, src: Register) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf2);
        self.emit_rex_optional(false, dest.needs_rex(), false, src.needs_rex());
        self.emit_u8(0x0f);
        self.emit_u8(0x2a);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn cvtsi2sdq_rr(&mut self, dest: XmmRegister, src: Register) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf2);
        self.emit_rex_optional(true, dest.needs_rex(), false, src.needs_rex());
        self.emit_u8(0x0f);
        self.emit_u8(0x2a);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn cvtsi2ssd_rr(&mut self, dest: XmmRegister, src: Register) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf3);
        self.emit_rex_optional(false, dest.needs_rex(), false, src.needs_rex());
        self.emit_u8(0x0f);
        self.emit_u8(0x2a);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn cvtsi2ssq_rr(&mut self, dest: XmmRegister, src: Register) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf3);
        self.emit_rex_optional(true, dest.needs_rex(), false, src.needs_rex());
        self.emit_u8(0x0f);
        self.emit_u8(0x2a);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn cvtss2sd_rr(&mut self, dest: XmmRegister, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf3);
        self.emit_rex_sse_modrm_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0x5a);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn cvttsd2sid_rr(&mut self, dest: Register, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf2);
        self.emit_rex_optional(false, dest.needs_rex(), false, src.needs_rex());
        self.emit_u8(0x0f);
        self.emit_u8(0x2c);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn cvttsd2siq_rr(&mut self, dest: Register, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf2);
        self.emit_rex_optional(true, dest.needs_rex(), false, src.needs_rex());
        self.emit_u8(0x0f);
        self.emit_u8(0x2c);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn cvttss2sid_rr(&mut self, dest: Register, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf3);
        self.emit_rex_optional(false, dest.needs_rex(), false, src.needs_rex());
        self.emit_u8(0x0f);
        self.emit_u8(0x2c);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn cvttss2siq_rr(&mut self, dest: Register, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf3);
        self.emit_rex_optional(true, dest.needs_rex(), false, src.needs_rex());
        self.emit_u8(0x0f);
        self.emit_u8(0x2c);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn divss_rr(&mut self, dest: XmmRegister, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf3);
        self.emit_rex_sse_modrm_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0x5e);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn divsd_rr(&mut self, dest: XmmRegister, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf2);
        self.emit_rex_sse_modrm_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0x5e);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn idivl_r(&mut self, reg: Register) {
        self.emit_rex32_rm_optional(reg);
        self.emit_u8(0xF7);
        self.emit_modrm(0b11, 0b111, reg.low_bits());
    }

    pub fn idivq_r(&mut self, src: Register) {
        self.emit_rex(true, false, false, src.needs_rex());
        self.emit_u8(0xF7);
        self.emit_modrm(0b11, 0b111, src.low_bits());
    }

    pub fn imull_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex32_optional(dest, src);
        self.emit_u8(0x0F);
        self.emit_u8(0xAF);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn imulq_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex64_modrm(dest, src);
        self.emit_u8(0x0F);
        self.emit_u8(0xAF);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn int3(&mut self) {
        self.emit_u8(0xCC);
    }

    pub fn jcc(&mut self, condition: Condition, target: Label) {
        if let Some(target_offset) = self.offset(target) {
            // backwards jump
            // rip = end of current instruction = pc + 2
            let target_offset = target_offset as usize;
            assert!(target_offset <= self.position());
            let distance = self.position() + 2 - target_offset;
            let distance = -(distance as isize);
            assert!(distance <= -2);

            if distance >= -128 {
                self.emit_u8(0x70 + condition.int());
                self.emit_u8(distance as u8);
            } else {
                let distance = self.position() + 6 - target_offset;
                let distance = -(distance as isize);
                self.emit_u8(0x0F);
                self.emit_u8(0x80 + condition.int());
                self.emit_u32(distance as u32);
            }
        } else {
            // forward jump - conservatively assume far jump
            self.emit_u8(0x0F);
            self.emit_u8(0x80 + condition.int());
            self.unresolved_jumps.push(ForwardJump {
                offset: self.position().try_into().unwrap(),
                label: target,
                distance: JumpDistance::Far,
            });
            self.emit_u32(0);
        }
    }

    pub fn jcc_near(&mut self, condition: Condition, target: Label) {
        if let Some(target_offset) = self.offset(target) {
            // backwards jump
            // rip = end of current instruction = pc + 2
            let target_offset = target_offset as usize;
            assert!(target_offset <= self.position());
            let distance = self.position() + 2 - target_offset;
            let distance = -(distance as isize);
            assert!(-128 <= distance && distance <= -2);
            self.emit_u8(0x70 + condition.int());
            self.emit_u8(distance as u8);
        } else {
            // forward jump
            self.emit_u8(0x70 + condition.int());
            self.unresolved_jumps.push(ForwardJump {
                offset: self.position().try_into().unwrap(),
                label: target,
                distance: JumpDistance::Near,
            });
            self.emit_u8(0);
        }
    }

    pub fn jmp(&mut self, target: Label) {
        if let Some(target_offset) = self.offset(target) {
            // backwards jump
            // rip = end of current instruction = pc + 2
            let target_offset = target_offset as usize;
            assert!(target_offset <= self.position());
            let distance = self.position() + 2 - target_offset;
            let distance = -(distance as isize);
            assert!(distance <= -2);

            if distance >= -128 {
                self.emit_u8(0xEB);
                self.emit_u8(distance as u8);
            } else {
                let distance = self.position() + 5 - target_offset;
                let distance = -(distance as isize);
                self.emit_u8(0xE9);
                self.emit_u32(distance as u32);
            }
        } else {
            // forward jump - conservatively assume far jump
            self.emit_u8(0xE9);
            self.unresolved_jumps.push(ForwardJump {
                offset: self.position().try_into().unwrap(),
                label: target,
                distance: JumpDistance::Far,
            });
            self.emit_u32(0);
        }
    }

    pub fn jmp_near(&mut self, target: Label) {
        if let Some(target_offset) = self.offset(target) {
            // backwards jump
            // rip = end of current instruction = pc + 2
            let target_offset = target_offset as usize;
            assert!(target_offset <= self.position());
            let distance = self.position() + 2 - target_offset;
            let distance = -(distance as isize);
            assert!(-128 <= distance && distance <= -2);
            self.emit_u8(0xEB);
            self.emit_u8(distance as u8);
        } else {
            // forward jump - conservatively assume far jump
            self.emit_u8(0xEB);
            self.unresolved_jumps.push(ForwardJump {
                offset: self.position().try_into().unwrap(),
                label: target,
                distance: JumpDistance::Near,
            });
            self.emit_u8(0);
        }
    }

    pub fn jmp_r(&mut self, reg: Register) {
        self.emit_rex32_rm_optional(reg);
        self.emit_u8(0xff);
        self.emit_modrm(0b11, 0b100, reg.low_bits());
    }

    pub fn lea(&mut self, dest: Register, src: Address) {
        self.emit_rex64_modrm_address(dest, src);
        self.emit_u8(0x8D);
        self.emit_address(dest.low_bits(), src);
    }

    pub fn lock_cmpxchgq_ar(&mut self, dest: Address, src: Register) {
        self.emit_lock_prefix();
        self.cmpxchgq_ar(dest, src);
    }

    pub fn lock_cmpxchgl_ar(&mut self, dest: Address, src: Register) {
        self.emit_lock_prefix();
        self.cmpxchgl_ar(dest, src);
    }

    pub fn lock_xaddq_ar(&mut self, dest: Address, src: Register) {
        self.emit_lock_prefix();
        self.xaddq_ar(dest, src);
    }

    pub fn lock_xaddl_ar(&mut self, dest: Address, src: Register) {
        self.emit_lock_prefix();
        self.xaddl_ar(dest, src);
    }

    pub fn lzcntl_rr(&mut self, dest: Register, src: Register) {
        self.emit_u8(0xf3);
        self.emit_rex32_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0xbd);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn lzcntq_rr(&mut self, dest: Register, src: Register) {
        self.emit_u8(0xf3);
        self.emit_rex64_modrm(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0xbd);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn mfence(&mut self) {
        self.emit_u8(0x0f);
        self.emit_u8(0xae);
        self.emit_u8(0xf0);
    }

    pub fn movaps_ar(&mut self, dest: Address, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_rex_sse_address_optional(src, dest);
        self.emit_u8(0x0f);
        self.emit_u8(0x29);
        self.emit_address(src.low_bits(), dest);
    }

    pub fn movb_ai(&mut self, dest: Address, src: Immediate) {
        assert!(src.is_int8() || src.is_uint8());
        self.emit_rex32_address_optional(dest);
        self.emit_u8(0xc6);
        self.emit_address(0b000, dest);
        self.emit_u8(src.uint8());
    }

    pub fn movb_ar(&mut self, dest: Address, src: Register) {
        self.emit_rex32_byte_address_optional(src, dest);
        self.emit_u8(0x88);
        self.emit_address(src.low_bits(), dest);
    }

    pub fn movb_ra(&mut self, dest: Register, src: Address) {
        self.emit_rex32_byte_address_optional(dest, src);
        self.emit_u8(0x8a);
        self.emit_address(dest.low_bits(), src);
    }

    pub fn movd_rx(&mut self, dest: Register, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0x66);
        self.emit_rex_optional(false, src.needs_rex(), false, dest.needs_rex());
        self.emit_u8(0x0f);
        self.emit_u8(0x7e);
        self.emit_modrm(0b11, src.low_bits(), dest.low_bits());
    }

    pub fn movd_xr(&mut self, dest: XmmRegister, src: Register) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0x66);
        self.emit_rex_optional(false, dest.needs_rex(), false, src.needs_rex());
        self.emit_u8(0x0f);
        self.emit_u8(0x6e);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn movl_ai(&mut self, dest: Address, imm: Immediate) {
        assert!(imm.is_int32() || imm.is_uint32());
        self.emit_rex32_address_optional(dest);
        self.emit_u8(0xc7);
        self.emit_address(0b000, dest);
        self.emit_u32(imm.uint32());
    }

    pub fn movl_ar(&mut self, dest: Address, src: Register) {
        self.emit_rex32_modrm_address_optional(src, dest);
        self.emit_u8(0x89);
        self.emit_address(src.low_bits(), dest);
    }

    pub fn movl_ra(&mut self, dest: Register, src: Address) {
        self.emit_rex32_modrm_address_optional(dest, src);
        self.emit_u8(0x8B);
        self.emit_address(dest.low_bits(), src);
    }

    pub fn movl_ri(&mut self, dest: Register, imm: Immediate) {
        assert!(imm.is_int32());
        self.emit_rex32_rm_optional(dest);
        self.emit_u8(0xB8 + dest.low_bits());
        self.emit_u32(imm.int32() as u32);
    }

    pub fn movl_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex32_optional(src, dest);
        self.emit_u8(0x89);
        self.emit_modrm(0b11, src.low_bits(), dest.low_bits());
    }

    pub fn movq_ai(&mut self, dest: Address, imm: Immediate) {
        assert!(imm.is_int32());
        self.emit_rex64_address(dest);
        self.emit_u8(0xc7);
        self.emit_address(0b000, dest);
        self.emit_u32(imm.int32() as u32);
    }

    pub fn movq_ar(&mut self, dest: Address, src: Register) {
        self.emit_rex64_modrm_address(src, dest);
        self.emit_u8(0x89);
        self.emit_address(src.low_bits(), dest);
    }

    pub fn movq_ra(&mut self, dest: Register, src: Address) {
        self.emit_rex64_modrm_address(dest, src);
        self.emit_u8(0x8B);
        self.emit_address(dest.low_bits(), src);
    }

    pub fn movq_ri(&mut self, dest: Register, imm: Immediate) {
        if imm.is_int32() {
            self.emit_rex(true, false, false, dest.needs_rex());
            self.emit_u8(0xC7);
            self.emit_modrm(0b11, 0b000, dest.low_bits());
            self.emit_u32(imm.int32() as u32);
        } else {
            self.emit_rex(true, false, false, dest.needs_rex());
            self.emit_u8(0xB8 + dest.low_bits());
            self.emit_u64(imm.int64() as u64);
        }
    }

    pub fn movq_rl(&mut self, dest: Register, label: Label) {
        self.emit_rex(true, dest.needs_rex(), false, false);
        self.emit_u8(0x8B);
        self.emit_label_address(dest.low_bits(), label);
    }

    pub fn movq_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex64_modrm(src, dest);
        self.emit_u8(0x89);
        self.emit_modrm(0b11, src.low_bits(), dest.low_bits());
    }

    pub fn movq_rx(&mut self, dest: Register, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0x66);
        self.emit_rex_optional(true, src.needs_rex(), false, dest.needs_rex());
        self.emit_u8(0x0f);
        self.emit_u8(0x7e);
        self.emit_modrm(0b11, src.low_bits(), dest.low_bits());
    }

    pub fn movq_xr(&mut self, dest: XmmRegister, src: Register) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0x66);
        self.emit_rex_optional(true, dest.needs_rex(), false, src.needs_rex());
        self.emit_u8(0x0f);
        self.emit_u8(0x6e);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn movsd_ra(&mut self, dest: XmmRegister, src: Address) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf2);
        self.emit_rex_sse_address_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0x10);
        self.emit_address(dest.low_bits(), src);
    }

    pub fn movsd_rl(&mut self, dest: XmmRegister, label: Label) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf2);
        if dest.needs_rex() {
            self.emit_rex(false, true, false, false);
        }
        self.emit_u8(0x0f);
        self.emit_u8(0x10);
        self.emit_label_address(dest.low_bits(), label);
    }

    pub fn movsd_rr(&mut self, dest: XmmRegister, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf2);
        self.emit_rex_sse_modrm_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0x10);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn movsd_ar(&mut self, dest: Address, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf2);
        self.emit_rex_sse_address_optional(src, dest);
        self.emit_u8(0x0f);
        self.emit_u8(0x11);
        self.emit_address(src.low_bits(), dest);
    }

    pub fn movss_ar(&mut self, dest: Address, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf3);
        self.emit_rex_sse_address_optional(src, dest);
        self.emit_u8(0x0f);
        self.emit_u8(0x11);
        self.emit_address(src.low_bits(), dest);
    }

    pub fn movss_ra(&mut self, dest: XmmRegister, src: Address) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf3);
        self.emit_rex_sse_address_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0x10);
        self.emit_address(dest.low_bits(), src);
    }

    pub fn movss_rl(&mut self, dest: XmmRegister, label: Label) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf3);
        if dest.needs_rex() {
            self.emit_rex(false, true, false, false);
        }
        self.emit_u8(0x0f);
        self.emit_u8(0x10);
        self.emit_label_address(dest.low_bits(), label);
    }

    pub fn movss_rr(&mut self, dest: XmmRegister, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf3);
        self.emit_rex_sse_modrm_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0x10);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn movsxbl_ra(&mut self, dest: Register, src: Address) {
        self.emit_rex32_modrm_address_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0xbe);
        self.emit_address(dest.low_bits(), src);
    }

    pub fn movsxbl_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex32_byte_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0xbe);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn movsxbq_ra(&mut self, dest: Register, src: Address) {
        self.emit_rex64_modrm_address(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0xbe);
        self.emit_address(dest.low_bits(), src);
    }

    pub fn movsxbq_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex64_modrm(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0xbe);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn movsxlq_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex64_modrm(dest, src);
        self.emit_u8(0x63);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn movups_ar(&mut self, dest: Address, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_rex_sse_address_optional(src, dest);
        self.emit_u8(0x0f);
        self.emit_u8(0x11);
        self.emit_address(src.low_bits(), dest);
    }

    pub fn movzxb_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex32_byte_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0xb6);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn movzxb_ra(&mut self, dest: Register, src: Address) {
        self.emit_rex32_modrm_address_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0xb6);
        self.emit_address(dest.low_bits(), src);
    }

    pub fn mulsd_rr(&mut self, dest: XmmRegister, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf2);
        self.emit_rex_sse_modrm_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0x59);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn mulss_rr(&mut self, dest: XmmRegister, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf3);
        self.emit_rex_sse_modrm_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0x59);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn negl(&mut self, reg: Register) {
        self.emit_rex32_rm_optional(reg);
        self.emit_u8(0xF7);
        self.emit_modrm(0b11, 0b011, reg.low_bits());
    }

    pub fn negq(&mut self, reg: Register) {
        self.emit_rex(true, false, false, reg.needs_rex());
        self.emit_u8(0xF7);
        self.emit_modrm(0b11, 0b011, reg.low_bits());
    }

    pub fn nop(&mut self) {
        self.emit_u8(0x90);
    }

    pub fn notl(&mut self, reg: Register) {
        self.emit_rex32_rm_optional(reg);
        self.emit_u8(0xF7);
        self.emit_modrm(0b11, 0b010, reg.low_bits());
    }

    pub fn notq(&mut self, reg: Register) {
        self.emit_rex(true, false, false, reg.needs_rex());
        self.emit_u8(0xF7);
        self.emit_modrm(0b11, 0b010, reg.low_bits());
    }

    pub fn orl_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex32_optional(src, dest);
        self.emit_u8(0x09);
        self.emit_modrm(0b11, src.low_bits(), dest.low_bits());
    }

    pub fn orq_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex64_modrm(src, dest);
        self.emit_u8(0x09);
        self.emit_modrm(0b11, src.low_bits(), dest.low_bits());
    }

    pub fn pushq_r(&mut self, reg: Register) {
        self.emit_rex32_rm_optional(reg);
        self.emit_u8(0x50 + reg.low_bits());
    }

    pub fn popcntl_rr(&mut self, dest: Register, src: Register) {
        self.emit_u8(0xf3);
        self.emit_rex32_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0xb8);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn popcntq_rr(&mut self, dest: Register, src: Register) {
        self.emit_u8(0xf3);
        self.emit_rex64_modrm(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0xb8);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn popq_r(&mut self, reg: Register) {
        self.emit_rex32_rm_optional(reg);
        self.emit_u8(0x58 + reg.low_bits());
    }

    pub fn pxor_rr(&mut self, dest: XmmRegister, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0x66);
        self.emit_rex_sse_modrm_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0xef);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn retq(&mut self) {
        self.emit_u8(0xC3);
    }

    pub fn roll_r(&mut self, opnd: Register) {
        self.emit_rex32_rm_optional(opnd);
        self.emit_u8(0xd3);
        self.emit_modrm(0b11, 0b000, opnd.low_bits());
    }

    pub fn rolq_r(&mut self, opnd: Register) {
        self.emit_rex(true, false, false, opnd.needs_rex());
        self.emit_u8(0xd3);
        self.emit_modrm(0b11, 0b000, opnd.low_bits());
    }

    pub fn roundsd_ri(&mut self, dest: XmmRegister, src: XmmRegister, mode: u8) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0x66);
        self.emit_rex_sse_modrm_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0x3a);
        self.emit_u8(0x0b);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
        self.emit_u8(mode);
    }

    pub fn roundss_ri(&mut self, dest: XmmRegister, src: XmmRegister, mode: u8) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0x66);
        self.emit_rex_sse_modrm_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0x3a);
        self.emit_u8(0x0a);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
        self.emit_u8(mode);
    }

    pub fn rorl_r(&mut self, opnd: Register) {
        self.emit_rex32_rm_optional(opnd);
        self.emit_u8(0xd3);
        self.emit_modrm(0b11, 0b001, opnd.low_bits());
    }

    pub fn rorq_r(&mut self, opnd: Register) {
        self.emit_rex(true, false, false, opnd.needs_rex());
        self.emit_u8(0xd3);
        self.emit_modrm(0b11, 0b001, opnd.low_bits());
    }

    pub fn sarl_r(&mut self, lhs: Register) {
        self.emit_rex32_rm_optional(lhs);
        self.emit_u8(0xd3);
        self.emit_modrm(0b11, 0b111, lhs.low_bits());
    }

    pub fn sarl_ri(&mut self, lhs: Register, rhs: Immediate) {
        assert!(rhs.is_int8());
        self.emit_rex32_rm_optional(lhs);
        self.emit_u8(0xc1);
        self.emit_modrm(0b11, 0b111, lhs.low_bits());
        self.emit_u8(rhs.int8() as u8);
    }

    pub fn sarq_r(&mut self, lhs: Register) {
        self.emit_rex(true, false, false, lhs.needs_rex());
        self.emit_u8(0xd3);
        self.emit_modrm(0b11, 0b111, lhs.low_bits());
    }

    pub fn sarq_ri(&mut self, lhs: Register, rhs: Immediate) {
        assert!(rhs.is_int8());
        self.emit_rex(true, false, false, lhs.needs_rex());
        self.emit_u8(0xc1);
        self.emit_modrm(0b11, 0b111, lhs.low_bits());
        self.emit_u8(rhs.int8() as u8);
    }

    pub fn setcc_r(&mut self, condition: Condition, dest: Register) {
        if dest.needs_rex() || dest.low_bits() > 3 {
            self.emit_rex(false, false, false, dest.needs_rex());
        }

        self.emit_u8(0x0F);
        self.emit_u8((0x90 + condition.int()) as u8);
        self.emit_modrm(0b11, 0b000, dest.low_bits());
    }

    pub fn shll_r(&mut self, lhs: Register) {
        self.emit_rex32_rm_optional(lhs);
        self.emit_u8(0xd3);
        self.emit_modrm(0b11, 0b100, lhs.low_bits());
    }

    pub fn shll_ri(&mut self, lhs: Register, rhs: Immediate) {
        assert!(rhs.is_int8());
        self.emit_rex32_rm_optional(lhs);
        self.emit_u8(0xc1);
        self.emit_modrm(0b11, 0b100, lhs.low_bits());
        self.emit_u8(rhs.int8() as u8);
    }

    pub fn shlq_r(&mut self, lhs: Register) {
        self.emit_rex(true, false, false, lhs.needs_rex());
        self.emit_u8(0xd3);
        self.emit_modrm(0b11, 0b100, lhs.low_bits());
    }

    pub fn shlq_ri(&mut self, lhs: Register, rhs: Immediate) {
        assert!(rhs.is_int8());
        self.emit_rex(true, false, false, lhs.needs_rex());
        self.emit_u8(0xc1);
        self.emit_modrm(0b11, 0b100, lhs.low_bits());
        self.emit_u8(rhs.int8() as u8);
    }

    pub fn shrl_r(&mut self, lhs: Register) {
        self.emit_rex32_rm_optional(lhs);
        self.emit_u8(0xd3);
        self.emit_modrm(0b11, 0b101, lhs.low_bits());
    }

    pub fn shrl_ri(&mut self, lhs: Register, rhs: Immediate) {
        assert!(rhs.is_int8());
        self.emit_rex32_rm_optional(lhs);
        self.emit_u8(0xc1);
        self.emit_modrm(0b11, 0b101, lhs.low_bits());
        self.emit_u8(rhs.int8() as u8);
    }

    pub fn shrq_r(&mut self, lhs: Register) {
        self.emit_rex(true, false, false, lhs.needs_rex());
        self.emit_u8(0xd3);
        self.emit_modrm(0b11, 0b101, lhs.low_bits());
    }

    pub fn shrq_ri(&mut self, lhs: Register, rhs: Immediate) {
        assert!(rhs.is_int8());
        self.emit_rex(true, false, false, lhs.needs_rex());
        self.emit_u8(0xc1);
        self.emit_modrm(0b11, 0b101, lhs.low_bits());
        self.emit_u8(rhs.int8() as u8);
    }

    pub fn sqrtsd_rr(&mut self, dest: XmmRegister, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf2);
        self.emit_rex_sse_modrm_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0x51);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn sqrtss_rr(&mut self, dest: XmmRegister, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf3);
        self.emit_rex_sse_modrm_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0x51);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn subl_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex32_optional(src, dest);
        self.emit_u8(0x29);
        self.emit_modrm(0b11, src.low_bits(), dest.low_bits());
    }

    pub fn subq_ri(&mut self, dest: Register, imm: Immediate) {
        self.emit_alu64_imm(dest, imm, 0b101, 0x2D);
    }

    pub fn subq_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex64_modrm(src, dest);
        self.emit_u8(0x29);
        self.emit_modrm(0b11, src.low_bits(), dest.low_bits());
    }

    pub fn subsd_rr(&mut self, dest: XmmRegister, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf2);
        self.emit_rex_sse_modrm_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0x5c);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn subss_rr(&mut self, dest: XmmRegister, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0xf3);
        self.emit_rex_sse_modrm_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0x5c);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn testb_ai(&mut self, lhs: Address, rhs: Immediate) {
        assert!(rhs.is_uint8());
        self.emit_rex32_address_optional(lhs);
        self.emit_u8(0xf6);
        self.emit_address(0b000, lhs);
        self.emit_u8(rhs.uint8());
    }

    pub fn testb_rr(&mut self, lhs: Register, rhs: Register) {
        self.emit_rex8_modrm_optional(rhs, lhs);
        self.emit_u8(0x84);
        self.emit_modrm(0b11, rhs.low_bits(), lhs.low_bits());
    }

    pub fn testl_ai(&mut self, lhs: Address, rhs: Immediate) {
        assert!(rhs.is_int32());
        self.emit_rex32_address_optional(lhs);
        self.emit_u8(0xf7);
        self.emit_address(0b000, lhs);
        self.emit_u32(rhs.int32() as u32);
    }

    pub fn testl_ar(&mut self, lhs: Address, rhs: Register) {
        self.emit_rex32_modrm_address_optional(rhs, lhs);
        self.emit_u8(0x85);
        self.emit_address(rhs.low_bits(), lhs);
    }

    pub fn testl_ri(&mut self, lhs: Register, rhs: Immediate) {
        assert!(rhs.is_int32());

        if rhs.is_uint8() {
            if lhs == RAX {
                self.emit_u8(0xa8);
            } else if lhs.value() < 4 {
                self.emit_u8(0xf6);
                self.emit_modrm(0b11, 0b000, lhs.low_bits());
            } else {
                self.emit_rex(false, false, false, lhs.needs_rex());
                self.emit_u8(0xf6);
                self.emit_modrm(0b11, 0b000, lhs.low_bits());
            }
            self.emit_u8(rhs.uint8());
        } else if lhs == RAX {
            self.emit_u8(0xa9);
            self.emit_u32(rhs.int32() as u32);
        } else {
            self.emit_u8(0xf7);
            self.emit_modrm(0b11, 0b000, lhs.low_bits());
            self.emit_u32(rhs.int32() as u32);
        }
    }

    pub fn testl_rr(&mut self, lhs: Register, rhs: Register) {
        self.emit_rex32_optional(rhs, lhs);
        self.emit_u8(0x85);
        self.emit_modrm(0b11, rhs.low_bits(), lhs.low_bits());
    }

    pub fn testq_ai(&mut self, lhs: Address, rhs: Immediate) {
        assert!(rhs.is_int32());
        self.emit_rex64_address(lhs);
        self.emit_u8(0xf7);
        self.emit_address(0b000, lhs);
        self.emit_u32(rhs.int32() as u32);
    }

    pub fn testq_ar(&mut self, lhs: Address, rhs: Register) {
        self.emit_rex64_modrm_address(rhs, lhs);
        self.emit_u8(0x85);
        self.emit_address(rhs.low_bits(), lhs);
    }

    pub fn testq_rr(&mut self, lhs: Register, rhs: Register) {
        self.emit_rex64_modrm(rhs, lhs);
        self.emit_u8(0x85);
        self.emit_modrm(0b11, rhs.low_bits(), lhs.low_bits());
    }

    pub fn tzcntl_rr(&mut self, dest: Register, src: Register) {
        self.emit_u8(0xf3);
        self.emit_rex32_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0xbc);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn tzcntq_rr(&mut self, dest: Register, src: Register) {
        self.emit_u8(0xf3);
        self.emit_rex64_modrm(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0xbc);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn ucomisd_rr(&mut self, dest: XmmRegister, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_u8(0x66);
        self.emit_rex_sse_modrm_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0x2e);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn ucomiss_rr(&mut self, dest: XmmRegister, src: XmmRegister) {
        debug_assert!(!self.has_avx2);
        self.emit_rex_sse_modrm_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0x2e);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn vaddsd_rr(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            rhs.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_F2,
        );
        self.emit_u8(0x58);
        self.emit_modrm(0b11, dest.low_bits(), rhs.low_bits());
    }

    pub fn vaddss_rr(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            rhs.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_F3,
        );
        self.emit_u8(0x58);
        self.emit_modrm(0b11, dest.low_bits(), rhs.low_bits());
    }

    pub fn vandpd_ra(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: Address) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            rhs.rex_x(),
            rhs.rex_b(),
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_66,
        );
        self.emit_u8(0x54);
        self.emit_address(dest.low_bits(), rhs);
    }

    pub fn vandpd_rl(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: Label) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            false,
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_66,
        );
        self.emit_u8(0x54);
        self.emit_label_address(dest.low_bits(), rhs);
    }

    pub fn vandps_ra(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: Address) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            rhs.rex_x(),
            rhs.rex_b(),
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_NONE,
        );
        self.emit_u8(0x54);
        self.emit_address(dest.low_bits(), rhs);
    }

    pub fn vandps_rl(&mut self, dest: XmmRegister, lhs: XmmRegister, label: Label) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            false,
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_NONE,
        );
        self.emit_u8(0x54);
        self.emit_label_address(dest.low_bits(), label);
    }

    pub fn vcvtsd2ss_rr(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            rhs.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_F2,
        );
        self.emit_u8(0x5a);
        self.emit_modrm(0b11, dest.low_bits(), rhs.low_bits());
    }

    pub fn vcvtsi2sdd_rr(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: Register) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            rhs.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_F2,
        );
        self.emit_u8(0x2a);
        self.emit_modrm(0b11, dest.low_bits(), rhs.low_bits());
    }

    pub fn vcvtsi2sdq_rr(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: Register) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            rhs.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W1,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_F2,
        );
        self.emit_u8(0x2a);
        self.emit_modrm(0b11, dest.low_bits(), rhs.low_bits());
    }

    pub fn vcvtsi2ssd_rr(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: Register) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            rhs.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_F3,
        );
        self.emit_u8(0x2a);
        self.emit_modrm(0b11, dest.low_bits(), rhs.low_bits());
    }

    pub fn vcvtsi2ssq_rr(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: Register) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            rhs.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W1,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_F3,
        );
        self.emit_u8(0x2a);
        self.emit_modrm(0b11, dest.low_bits(), rhs.low_bits());
    }

    pub fn vcvtss2sd_rr(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            rhs.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_F3,
        );
        self.emit_u8(0x5a);
        self.emit_modrm(0b11, dest.low_bits(), rhs.low_bits());
    }

    pub fn vcvttsd2sid_rr(&mut self, dest: Register, src: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            src.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            VEX_VVVV_UNUSED,
            VEX_L_SCALAR_128,
            VEX_PP_F2,
        );
        self.emit_u8(0x2c);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn vcvttsd2siq_rr(&mut self, dest: Register, src: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            src.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W1,
            VEX_VVVV_UNUSED,
            VEX_L_SCALAR_128,
            VEX_PP_F2,
        );
        self.emit_u8(0x2c);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn vcvttss2sid_rr(&mut self, dest: Register, src: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            src.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            VEX_VVVV_UNUSED,
            VEX_L_SCALAR_128,
            VEX_PP_F3,
        );
        self.emit_u8(0x2c);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn vcvttss2siq_rr(&mut self, dest: Register, src: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            src.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W1,
            VEX_VVVV_UNUSED,
            VEX_L_SCALAR_128,
            VEX_PP_F3,
        );
        self.emit_u8(0x2c);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn vdivsd_rr(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            rhs.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_F2,
        );
        self.emit_u8(0x5e);
        self.emit_modrm(0b11, dest.low_bits(), rhs.low_bits());
    }

    pub fn vdivss_rr(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            rhs.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_F3,
        );
        self.emit_u8(0x5e);
        self.emit_modrm(0b11, dest.low_bits(), rhs.low_bits());
    }

    pub fn vmovapd_rr(&mut self, dest: XmmRegister, src: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            src.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            VEX_VVVV_UNUSED,
            VEX_L_SCALAR_128,
            VEX_PP_66,
        );
        self.emit_u8(0x28);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn vmovaps_rr(&mut self, dest: XmmRegister, src: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            src.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            VEX_VVVV_UNUSED,
            VEX_L_SCALAR_128,
            VEX_PP_NONE,
        );
        self.emit_u8(0x28);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn vmovd_rx(&mut self, dest: Register, src: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            src.needs_rex(),
            false,
            dest.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            VEX_VVVV_UNUSED,
            VEX_L_SCALAR_128,
            VEX_PP_66,
        );
        self.emit_u8(0x7E);
        self.emit_modrm(0b11, src.low_bits(), dest.low_bits());
    }

    pub fn vmovd_xr(&mut self, dest: XmmRegister, src: Register) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            src.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            VEX_VVVV_UNUSED,
            VEX_L_SCALAR_128,
            VEX_PP_66,
        );
        self.emit_u8(0x6E);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn vmovq_rx(&mut self, dest: Register, src: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            src.needs_rex(),
            false,
            dest.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W1,
            VEX_VVVV_UNUSED,
            VEX_L_SCALAR_128,
            VEX_PP_66,
        );
        self.emit_u8(0x7E);
        self.emit_modrm(0b11, src.low_bits(), dest.low_bits());
    }

    pub fn vmovq_xr(&mut self, dest: XmmRegister, src: Register) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            src.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W1,
            VEX_VVVV_UNUSED,
            VEX_L_SCALAR_128,
            VEX_PP_66,
        );
        self.emit_u8(0x6E);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn vmovsd_ar(&mut self, dest: Address, src: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            src.needs_rex(),
            dest.rex_x(),
            dest.rex_b(),
            VEX_MMMMM_0F,
            VEX_W0,
            VEX_VVVV_UNUSED,
            VEX_L_SCALAR_128,
            VEX_PP_F2,
        );
        self.emit_u8(0x11);
        self.emit_address(src.low_bits(), dest);
    }

    pub fn vmovsd_ra(&mut self, dest: XmmRegister, src: Address) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            src.rex_x(),
            src.rex_b(),
            VEX_MMMMM_0F,
            VEX_W0,
            VEX_VVVV_UNUSED,
            VEX_L_SCALAR_128,
            VEX_PP_F2,
        );
        self.emit_u8(0x10);
        self.emit_address(dest.low_bits(), src);
    }

    pub fn vmovsd_rl(&mut self, dest: XmmRegister, label: Label) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            false,
            VEX_MMMMM_0F,
            VEX_W0,
            VEX_VVVV_UNUSED,
            VEX_L_SCALAR_128,
            VEX_PP_F2,
        );
        self.emit_u8(0x10);
        self.emit_label_address(dest.low_bits(), label);
    }

    pub fn vmovsd_rr(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            rhs.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_F2,
        );
        self.emit_u8(0x10);
        self.emit_modrm(0b11, dest.low_bits(), rhs.low_bits());
    }

    pub fn vmovss_ar(&mut self, dest: Address, src: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            src.needs_rex(),
            dest.rex_x(),
            dest.rex_b(),
            VEX_MMMMM_0F,
            VEX_W0,
            VEX_VVVV_UNUSED,
            VEX_L_SCALAR_128,
            VEX_PP_F3,
        );
        self.emit_u8(0x11);
        self.emit_address(src.low_bits(), dest);
    }

    pub fn vmovss_ra(&mut self, dest: XmmRegister, src: Address) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            src.rex_x(),
            src.rex_b(),
            VEX_MMMMM_0F,
            VEX_W0,
            VEX_VVVV_UNUSED,
            VEX_L_SCALAR_128,
            VEX_PP_F3,
        );
        self.emit_u8(0x10);
        self.emit_address(dest.low_bits(), src);
    }

    pub fn vmovss_rl(&mut self, dest: XmmRegister, label: Label) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            false,
            VEX_MMMMM_0F,
            VEX_W0,
            VEX_VVVV_UNUSED,
            VEX_L_SCALAR_128,
            VEX_PP_F3,
        );
        self.emit_u8(0x10);
        self.emit_label_address(dest.low_bits(), label);
    }

    pub fn vmovss_rr(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            rhs.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_F3,
        );
        self.emit_u8(0x10);
        self.emit_modrm(0b11, dest.low_bits(), rhs.low_bits());
    }

    pub fn vmulsd_rr(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            rhs.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_F2,
        );
        self.emit_u8(0x59);
        self.emit_modrm(0b11, dest.low_bits(), rhs.low_bits());
    }

    pub fn vmulss_rr(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            rhs.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_F3,
        );
        self.emit_u8(0x59);
        self.emit_modrm(0b11, dest.low_bits(), rhs.low_bits());
    }

    pub fn vroundsd_ri(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: XmmRegister, mode: u8) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            rhs.needs_rex(),
            VEX_MMMMM_0F_3A,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_66,
        );
        self.emit_u8(0x0b);
        self.emit_modrm(0b11, dest.low_bits(), rhs.low_bits());
        self.emit_u8(mode);
    }

    pub fn vroundss_ri(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: XmmRegister, mode: u8) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            rhs.needs_rex(),
            VEX_MMMMM_0F_3A,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_66,
        );
        self.emit_u8(0x0a);
        self.emit_modrm(0b11, dest.low_bits(), rhs.low_bits());
        self.emit_u8(mode);
    }

    pub fn vsqrtsd_rr(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            rhs.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_F2,
        );
        self.emit_u8(0x51);
        self.emit_modrm(0b11, dest.low_bits(), rhs.low_bits());
    }

    pub fn vsqrtss_rr(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            rhs.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_F3,
        );
        self.emit_u8(0x51);
        self.emit_modrm(0b11, dest.low_bits(), rhs.low_bits());
    }

    pub fn vsubsd_rr(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            rhs.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_F2,
        );
        self.emit_u8(0x5c);
        self.emit_modrm(0b11, dest.low_bits(), rhs.low_bits());
    }

    pub fn vsubss_rr(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            rhs.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_F3,
        );
        self.emit_u8(0x5c);
        self.emit_modrm(0b11, dest.low_bits(), rhs.low_bits());
    }

    pub fn vucomisd_rr(&mut self, lhs: XmmRegister, rhs: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            lhs.needs_rex(),
            false,
            rhs.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            VEX_VVVV_UNUSED,
            VEX_L_SCALAR_128,
            VEX_PP_66,
        );
        self.emit_u8(0x2e);
        self.emit_modrm(0b11, lhs.low_bits(), rhs.low_bits());
    }

    pub fn vucomiss_rr(&mut self, lhs: XmmRegister, rhs: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            lhs.needs_rex(),
            false,
            rhs.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            VEX_VVVV_UNUSED,
            VEX_L_SCALAR_128,
            VEX_PP_NONE,
        );
        self.emit_u8(0x2e);
        self.emit_modrm(0b11, lhs.low_bits(), rhs.low_bits());
    }

    pub fn vxorpd_ra(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: Address) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            rhs.rex_x(),
            rhs.rex_b(),
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_66,
        );
        self.emit_u8(0x57);
        self.emit_address(dest.low_bits(), rhs);
    }

    pub fn vxorpd_rl(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: Label) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            false,
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_66,
        );
        self.emit_u8(0x57);
        self.emit_label_address(dest.low_bits(), rhs);
    }

    pub fn vxorps_ra(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: Address) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            rhs.rex_x(),
            rhs.rex_b(),
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_NONE,
        );
        self.emit_u8(0x57);
        self.emit_address(dest.low_bits(), rhs);
    }

    pub fn vxorps_rl(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: Label) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            false,
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_NONE,
        );
        self.emit_u8(0x57);
        self.emit_label_address(dest.low_bits(), rhs);
    }

    pub fn vxorps_rr(&mut self, dest: XmmRegister, lhs: XmmRegister, rhs: XmmRegister) {
        debug_assert!(self.has_avx2);
        self.emit_vex(
            dest.needs_rex(),
            false,
            rhs.needs_rex(),
            VEX_MMMMM_0F,
            VEX_W0,
            lhs.value(),
            VEX_L_SCALAR_128,
            VEX_PP_NONE,
        );
        self.emit_u8(0x57);
        self.emit_modrm(0b11, dest.low_bits(), rhs.low_bits());
    }

    pub fn xaddl_ar(&mut self, dest: Address, src: Register) {
        self.emit_rex32_modrm_address_optional(src, dest);
        self.emit_u8(0x0f);
        self.emit_u8(0xc1);
        self.emit_address(src.low_bits(), dest);
    }

    pub fn xaddq_ar(&mut self, dest: Address, src: Register) {
        self.emit_rex64_modrm_address(src, dest);
        self.emit_u8(0x0f);
        self.emit_u8(0xc1);
        self.emit_address(src.low_bits(), dest);
    }

    pub fn xchgb_ar(&mut self, dest: Address, src: Register) {
        self.emit_rex32_byte_address_optional(src, dest);
        self.emit_u8(0x86);
        self.emit_address(src.low_bits(), dest);
    }

    pub fn xchgl_ar(&mut self, dest: Address, src: Register) {
        self.emit_rex32_modrm_address_optional(src, dest);
        self.emit_u8(0x87);
        self.emit_address(src.low_bits(), dest);
    }

    pub fn xchgq_ar(&mut self, dest: Address, src: Register) {
        self.emit_rex64_modrm_address(src, dest);
        self.emit_u8(0x87);
        self.emit_address(src.low_bits(), dest);
    }

    pub fn xorl_ri(&mut self, lhs: Register, rhs: Immediate) {
        self.emit_alu32_imm(lhs, rhs, 0b110, 0x35);
    }

    pub fn xorl_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex32_optional(src, dest);
        self.emit_u8(0x31);
        self.emit_modrm(0b11, src.low_bits(), dest.low_bits());
    }

    pub fn xorpd_ra(&mut self, dest: XmmRegister, src: Address) {
        self.emit_u8(0x66);
        self.emit_rex_sse_address_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0x57);
        self.emit_address(dest.low_bits(), src);
    }

    pub fn xorpd_rl(&mut self, dest: XmmRegister, src: Label) {
        self.emit_u8(0x66);
        if dest.needs_rex() {
            self.emit_rex(false, true, false, false);
        }
        self.emit_u8(0x0f);
        self.emit_u8(0x57);
        self.emit_label_address(dest.low_bits(), src);
    }

    pub fn xorps_ra(&mut self, dest: XmmRegister, src: Address) {
        self.emit_rex_sse_address_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0x57);
        self.emit_address(dest.low_bits(), src);
    }

    pub fn xorps_rl(&mut self, dest: XmmRegister, src: Label) {
        if dest.needs_rex() {
            self.emit_rex(false, true, false, false);
        }
        self.emit_u8(0x0f);
        self.emit_u8(0x57);
        self.emit_label_address(dest.low_bits(), src);
    }

    pub fn xorps_rr(&mut self, dest: XmmRegister, src: XmmRegister) {
        self.emit_rex_sse_modrm_optional(dest, src);
        self.emit_u8(0x0f);
        self.emit_u8(0x57);
        self.emit_modrm(0b11, dest.low_bits(), src.low_bits());
    }

    pub fn xorq_rr(&mut self, dest: Register, src: Register) {
        self.emit_rex64_modrm(src, dest);
        self.emit_u8(0x31);
        self.emit_modrm(0b11, src.low_bits(), dest.low_bits());
    }

    fn emit_lock_prefix(&mut self) {
        self.emit_u8(0xF0);
    }

    fn emit_rex_sse_modrm_optional(&mut self, reg: XmmRegister, rm: XmmRegister) {
        if reg.needs_rex() || rm.needs_rex() {
            self.emit_rex(false, reg.needs_rex(), false, rm.needs_rex());
        }
    }

    fn emit_rex_sse_address_optional(&mut self, reg: XmmRegister, address: Address) {
        if address.rex != 0 || reg.needs_rex() {
            self.emit_u8(0x40 | address.rex | if reg.needs_rex() { 0x04 } else { 0 });
        }
    }

    fn emit_rex8_modrm_optional(&mut self, modrm_reg: Register, modrm_rm: Register) {
        if modrm_reg.needs_rex() || modrm_reg.0 > 3u8 || modrm_rm.needs_rex() || modrm_rm.0 > 3u8 {
            self.emit_rex(false, modrm_reg.needs_rex(), false, modrm_rm.needs_rex());
        }
    }

    fn emit_rex32_rm_optional(&mut self, reg: Register) {
        if reg.needs_rex() {
            self.emit_rex(false, false, false, true);
        }
    }

    fn emit_rex32_byte_optional(&mut self, reg: Register, rm: Register) {
        if reg.needs_rex() || rm.needs_rex() || rm.value() > 3 {
            self.emit_rex(false, reg.needs_rex(), false, rm.needs_rex());
        }
    }

    fn emit_rex32_optional(&mut self, reg: Register, rm: Register) {
        if reg.needs_rex() || rm.needs_rex() {
            self.emit_rex(false, reg.needs_rex(), false, rm.needs_rex());
        }
    }

    fn emit_rex64_modrm_address(&mut self, reg: Register, address: Address) {
        let rex = 0x48 | address.rex | if reg.needs_rex() { 0x04 } else { 0 };
        self.emit_u8(rex);
    }

    fn emit_rex64_address(&mut self, address: Address) {
        self.emit_u8(0x48 | address.rex);
    }

    fn emit_rex32_modrm_address_optional(&mut self, reg: Register, address: Address) {
        if address.rex != 0 || reg.needs_rex() {
            self.emit_u8(0x40 | address.rex | if reg.needs_rex() { 0x04 } else { 0 });
        }
    }

    fn emit_rex32_byte_address_optional(&mut self, reg: Register, address: Address) {
        if address.rex != 0 || reg.value() > 3 {
            self.emit_u8(0x40 | address.rex | if reg.needs_rex() { 0x04 } else { 0 });
        }
    }

    fn emit_rex32_address_optional(&mut self, address: Address) {
        if address.rex != 0 {
            self.emit_u8(0x40 | address.rex);
        }
    }

    fn emit_rex64_modrm(&mut self, reg: Register, rm: Register) {
        self.emit_rex(true, reg.needs_rex(), false, rm.needs_rex());
    }

    fn emit_rex(&mut self, w: bool, r: bool, x: bool, b: bool) {
        // w - 64-bit width
        // r - extension of modrm-reg field
        // x - extension of sib index field
        // b - extension of modrm-rm/sib base/opcode reg field
        let opcode = 0x40 | (w as u8) << 3 | (r as u8) << 2 | (x as u8) << 1 | b as u8;
        self.emit_u8(opcode);
    }

    fn emit_rex_optional(&mut self, w: bool, r: bool, x: bool, b: bool) {
        if w || r || x || b {
            self.emit_rex(w, r, x, b);
        }
    }

    // r - rex.r bit (1 bit)
    // x - rex.x bit (1 bit)
    // b - rex.b bit (1 bit)
    // mmmmm - leading opcode byte (5 bits)
    //       - 00001B --> OF
    //       - 00010B --> OF 38
    //       - 00011B --> OF 3A
    //       - rest reserved
    // w - rex.w/opcode (1 bit)
    // vvvv - register specifier (4 bits)
    // l - vector length (1 bit)
    //       - 0 --> scalar or 128-bit vector
    //       - 1 --> 256-bit vector
    // pp - simd prefix (2 bits)
    //       - 00 --> None
    //       - 01 --> 66
    //       - 10 --> F3
    //       - 11 --> F2
    fn emit_vex(&mut self, r: bool, x: bool, b: bool, mmmmm: u8, w: u8, vvvv: u8, l: u8, pp: u8) {
        assert!(fits_u5(mmmmm as u32));
        assert!(fits_u4(vvvv as u32));
        assert!(fits_u2(pp as u32));
        assert!(fits_u1(l as u32));

        if x || b || mmmmm != 0b00001 || w != 0 {
            // 0xC4 ...
            self.emit_vex3(r, x, b, mmmmm, w, vvvv, l, pp);
        } else {
            // 0xC5 ...
            self.emit_vex2(r, vvvv, l, pp);
        }
    }

    // r - rex.r bit (1 bit)
    // x - rex.x bit (1 bit)
    // b - rex.b bit (1 bit)
    // mmmmm - leading opcode byte (5 bits)
    // w - rex.w/opcode (1 bit)
    // vvvv - register specifier (4 bits)
    // l - vector length (1 bit)
    // pp - simd prefix (2 bits)
    fn emit_vex3(&mut self, r: bool, x: bool, b: bool, mmmmm: u8, w: u8, vvvv: u8, l: u8, pp: u8) {
        assert!(fits_u5(mmmmm as u32));
        assert!(fits_u4(vvvv as u32));
        assert!(fits_u2(pp as u32));
        assert!(fits_u1(l as u32));
        assert!(fits_u1(w as u32));
        self.emit_u8(0xC4);

        let byte = (!r as u8) << 7 | (!x as u8) << 6 | (!b as u8) << 5 | mmmmm;
        self.emit_u8(byte);

        let vvvv = !vvvv & 0b1111;
        let byte = (w as u8) << 7 | vvvv << 3 | (l as u8) << 2 | pp;
        self.emit_u8(byte);
    }

    // r - rex.r bit (1 bit)
    // vvvv - register specifier (4 bits)
    // l - vector length (1 bit)
    // pp - simd prefix (2 bits)
    fn emit_vex2(&mut self, r: bool, vvvv: u8, l: u8, pp: u8) {
        assert!(fits_u4(vvvv as u32));
        assert!(fits_u2(pp as u32));
        assert!(fits_u1(l as u32));
        self.emit_u8(0xC5);

        let vvvv = !vvvv & 0b1111;
        let byte = (!r as u8) << 7 | vvvv << 3 | (l as u8) << 2 | pp;
        self.emit_u8(byte);
    }

    fn emit_modrm(&mut self, mode: u8, reg: u8, rm: u8) {
        assert!(fits_u2(mode as u32));
        assert!(fits_u3(reg as u32));
        assert!(fits_u3(rm as u32));
        self.emit_u8(mode << 6 | reg << 3 | rm);
    }

    fn emit_address(&mut self, reg_or_opcode: u8, address: Address) {
        assert!(fits_u3(reg_or_opcode as u32));

        let bytes = address.encoded_bytes();

        // emit modrm-byte with the given rm value
        self.emit_u8(reg_or_opcode << 3 | bytes[0]);

        for &byte in &bytes[1..] {
            self.emit_u8(byte);
        }
    }

    fn emit_label_address(&mut self, reg: u8, label: Label) {
        self.emit_modrm(0b00, reg, 0b101);

        self.unresolved_jumps.push(ForwardJump {
            offset: self.position().try_into().unwrap(),
            label,
            distance: JumpDistance::Far,
        });
        self.emit_u32(0);
    }

    fn emit_alu64_imm(&mut self, reg: Register, imm: Immediate, modrm_reg: u8, rax_opcode: u8) {
        assert!(imm.is_int32());
        self.emit_rex(true, false, false, reg.needs_rex());

        if imm.is_int8() {
            self.emit_u8(0x83);
            self.emit_modrm(0b11, modrm_reg, reg.low_bits());
            self.emit_u8(imm.int8() as u8);
        } else if reg == RAX {
            self.emit_u8(rax_opcode);
            self.emit_u32(imm.int32() as u32);
        } else {
            self.emit_u8(0x81);
            self.emit_modrm(0b11, modrm_reg, reg.low_bits());
            self.emit_u32(imm.int32() as u32);
        }
    }

    fn emit_alu32_imm(&mut self, reg: Register, imm: Immediate, modrm_reg: u8, rax_opcode: u8) {
        assert!(imm.is_int32());
        self.emit_rex32_rm_optional(reg);

        if imm.is_int8() {
            self.emit_u8(0x83);
            self.emit_modrm(0b11, modrm_reg, reg.low_bits());
            self.emit_u8(imm.int8() as u8);
        } else if reg == RAX {
            self.emit_u8(rax_opcode);
            self.emit_u32(imm.int32() as u32);
        } else {
            self.emit_u8(0x81);
            self.emit_modrm(0b11, modrm_reg, reg.low_bits());
            self.emit_u32(imm.int32() as u32);
        }
    }
}

#[derive(Copy, Clone)]
pub enum Condition {
    Overflow,
    NoOverflow,
    Below,
    NeitherAboveNorEqual,
    NotBelow,
    AboveOrEqual,
    Equal,
    Zero,
    NotEqual,
    NotZero,
    BelowOrEqual,
    NotAbove,
    NeitherBelowNorEqual,
    Above,
    Sign,
    NoSign,
    Parity,
    ParityEven,
    NoParity,
    ParityOdd,
    Less,
    NeitherGreaterNorEqual,
    NotLess,
    GreaterOrEqual,
    LessOrEqual,
    NotGreater,
    NeitherLessNorEqual,
    Greater,
}

impl Condition {
    pub fn int(self) -> u8 {
        match self {
            Condition::Overflow => 0b0000,
            Condition::NoOverflow => 0b0001,
            Condition::Below | Condition::NeitherAboveNorEqual => 0b0010,
            Condition::NotBelow | Condition::AboveOrEqual => 0b0011,
            Condition::Equal | Condition::Zero => 0b0100,
            Condition::NotEqual | Condition::NotZero => 0b0101,
            Condition::BelowOrEqual | Condition::NotAbove => 0b0110,
            Condition::NeitherBelowNorEqual | Condition::Above => 0b0111,
            Condition::Sign => 0b1000,
            Condition::NoSign => 0b1001,
            Condition::Parity | Condition::ParityEven => 0b1010,
            Condition::NoParity | Condition::ParityOdd => 0b1011,
            Condition::Less | Condition::NeitherGreaterNorEqual => 0b1100,
            Condition::NotLess | Condition::GreaterOrEqual => 0b1101,
            Condition::LessOrEqual | Condition::NotGreater => 0b1110,
            Condition::NeitherLessNorEqual | Condition::Greater => 0b1111,
        }
    }
}

pub struct Immediate(pub i64);

impl Immediate {
    pub fn is_int8(&self) -> bool {
        let limit = 1i64 << 7;
        -limit <= self.0 && self.0 < limit
    }

    pub fn is_int32(&self) -> bool {
        let limit = 1i64 << 31;
        -limit <= self.0 && self.0 < limit
    }

    pub fn is_uint8(&self) -> bool {
        0 <= self.0 && self.0 < 256
    }

    pub fn is_uint32(&self) -> bool {
        let limit = 1i64 << 32;
        0 <= self.0 && self.0 < limit
    }

    pub fn uint8(&self) -> u8 {
        self.0 as u8
    }

    pub fn int8(&self) -> i8 {
        self.0 as i8
    }

    pub fn int32(&self) -> i32 {
        self.0 as i32
    }

    pub fn uint32(&self) -> u32 {
        self.0 as u32
    }

    pub fn int64(&self) -> i64 {
        self.0
    }
}

#[derive(Copy, Clone)]
pub struct XmmRegister(u8);

impl XmmRegister {
    pub fn new(value: u8) -> XmmRegister {
        XmmRegister(value)
    }

    pub fn low_bits(self) -> u8 {
        self.0 & 0b111
    }

    pub fn value(self) -> u8 {
        self.0
    }

    pub fn needs_rex(self) -> bool {
        self.0 > 7
    }
}

#[derive(Copy, Clone)]
pub enum ScaleFactor {
    One,
    Two,
    Four,
    Eight,
}

impl ScaleFactor {
    fn value(self) -> u8 {
        match self {
            ScaleFactor::One => 0,
            ScaleFactor::Two => 1,
            ScaleFactor::Four => 2,
            ScaleFactor::Eight => 3,
        }
    }
}

#[derive(Copy, Clone)]
pub struct Address {
    rex: u8,
    length: u8,
    bytes: [u8; 6],
}

impl Address {
    fn new() -> Address {
        Address {
            rex: 0,
            length: 0,
            bytes: [0; 6],
        }
    }

    fn set_modrm(&mut self, mode: u8, reg: Register) {
        assert!(mode < 4);
        assert_eq!(self.length, 0);

        if reg.needs_rex() {
            self.rex |= 0x41;
        }

        self.bytes[0] = mode << 6 | reg.low_bits();
        self.length += 1;
    }

    fn set_sib(&mut self, scale: ScaleFactor, index: Register, base: Register) {
        assert_eq!(self.length, 1);

        if base.needs_rex() {
            self.rex |= 0x41;
        }

        if index.needs_rex() {
            self.rex |= 0x42;
        }

        self.bytes[1] = scale.value() << 6 | index.low_bits() << 3 | base.low_bits();
        self.length += 1;
    }

    fn set_disp8(&mut self, imm: i8) {
        assert!(self.length == 1 || self.length == 2);
        self.bytes[self.length as usize] = imm as u8;
        self.length += 1;
    }

    fn set_disp32(&mut self, imm: i32) {
        assert!(self.length == 1 || self.length == 2);
        let idx = self.length as usize;
        let imm = imm as u32;
        self.bytes[idx] = imm as u8;
        self.bytes[idx + 1] = (imm >> 8) as u8;
        self.bytes[idx + 2] = (imm >> 16) as u8;
        self.bytes[idx + 3] = (imm >> 24) as u8;
        self.length += 4;
    }

    pub fn reg(base: Register) -> Address {
        Address::offset(base, 0)
    }

    pub fn offset(base: Register, offset: i32) -> Address {
        let mut address = Address::new();

        let mode = if offset == 0 && base != RBP && base != R13 {
            0b00
        } else if -128 <= offset && offset < 128 {
            0b01
        } else {
            0b10
        };

        address.set_modrm(mode, base);

        if base == RSP || base == R12 {
            address.set_sib(ScaleFactor::One, RSP, base);
        }

        match mode {
            0b00 => {}
            0b01 => address.set_disp8(offset as i8),
            0b10 => address.set_disp32(offset),
            _ => unreachable!(),
        }

        address
    }

    pub fn index(index: Register, factor: ScaleFactor, disp: i32) -> Address {
        let mut address = Address::new();

        address.set_modrm(0b00, RSP);
        assert_ne!(index, RSP);

        address.set_sib(factor, index, RBP);
        address.set_disp32(disp);

        address
    }

    pub fn array(base: Register, index: Register, factor: ScaleFactor, disp: i32) -> Address {
        let mut address = Address::new();

        let mode = if disp == 0 && base != RBP && base != R13 {
            0b00
        } else if -128 <= disp && disp < 128 {
            0b01
        } else {
            0b10
        };

        address.set_modrm(mode, RSP);
        assert_ne!(index, RSP);
        assert_ne!(index, R12);

        address.set_sib(factor, index, base);

        match mode {
            0b00 => {}
            0b01 => address.set_disp8(disp as i8),
            0b10 => address.set_disp32(disp),
            _ => unreachable!(),
        }

        address
    }

    pub fn rip(disp: i32) -> Address {
        let mut address = Address::new();

        address.set_modrm(0b00, RBP);
        address.set_disp32(disp);

        address
    }

    pub fn encoded_bytes(&self) -> &[u8] {
        &self.bytes[0..self.length as usize]
    }

    pub fn rex_x(&self) -> bool {
        (self.rex & 0x2) != 0
    }

    pub fn rex_b(&self) -> bool {
        (self.rex & 0x1) != 0
    }
}

#[allow(unused)]
const VEX_PP_NONE: u8 = 0b00;
#[allow(unused)]
const VEX_PP_66: u8 = 0b01;
const VEX_PP_F3: u8 = 0b10;
const VEX_PP_F2: u8 = 0b11;

#[allow(unused)]
const VEX_L_SCALAR_128: u8 = 0b0;
#[allow(unused)]
const VEX_L_256: u8 = 0b0;

#[allow(unused)]
const VEX_MMMMM_0F: u8 = 0b00001;
#[allow(unused)]
const VEX_MMMMM_0F_38: u8 = 0b00010;
#[allow(unused)]
const VEX_MMMMM_0F_3A: u8 = 0b00011;

const VEX_VVVV_UNUSED: u8 = 0b0000;

const VEX_W0: u8 = 0;
const VEX_W1: u8 = 1;

fn fits_u1(imm: u32) -> bool {
    imm < 2
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

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_emit {
        (
            $($expr:expr),*;
            $name:ident
        ) => {{
            let mut buf = AssemblerX64::new(false);
            buf.$name();
            let expected = vec![$($expr,)*];
            assert_asm_bytes(expected, buf.finalize(1).code());
        }};

        (
            $($expr:expr),*;
            $name:ident
            (
                    $($param:expr),+
            )
        ) => {{
            let mut buf = AssemblerX64::new(false);
            buf.$name($($param,)*);
            let expected = vec![$($expr,)*];
            assert_asm_bytes(expected, buf.finalize(1).code());
        }};

        (
            $($expr:expr),*;
            $name:ident
            (
                    $($param:expr),+
            ); avx2
        ) => {{
            let mut buf = AssemblerX64::new(true);
            buf.$name($($param,)*);
            let expected = vec![$($expr,)*];
            assert_asm_bytes(expected, buf.finalize(1).code());
        }};
    }

    fn assert_asm_bytes(expected: Vec<u8>, got: Vec<u8>) {
        if expected != got {
            print!("exp: ");

            for (ind, val) in expected.iter().enumerate() {
                if ind > 0 {
                    print!(", ");
                }

                print!("{:02x}", val);
            }

            print!("\ngot: ");

            for (ind, val) in got.iter().enumerate() {
                if ind > 0 {
                    print!(", ");
                }

                print!("{:02x}", val);
            }

            println!("");

            panic!("emitted code wrong.");
        }
    }

    #[test]
    fn test_popq_r() {
        assert_emit!(0x58; popq_r(RAX));
        assert_emit!(0x5c; popq_r(RSP));
        assert_emit!(0x41, 0x58; popq_r(R8));
        assert_emit!(0x41, 0x5F; popq_r(R15));
    }

    #[test]
    fn test_pushq_r() {
        assert_emit!(0x50; pushq_r(RAX));
        assert_emit!(0x54; pushq_r(RSP));
        assert_emit!(0x41, 0x50; pushq_r(R8));
        assert_emit!(0x41, 0x57; pushq_r(R15));
    }

    #[test]
    fn test_retq() {
        assert_emit!(0xc3; retq);
    }

    #[test]
    fn test_nop() {
        assert_emit!(0x90; nop);
    }

    #[test]
    fn test_emit_movq_rr() {
        assert_emit!(0x49, 0x89, 0xc7; movq_rr(R15, RAX));
        assert_emit!(0x4c, 0x89, 0xf8; movq_rr(RAX, R15));
        assert_emit!(0x48, 0x89, 0xe5; movq_rr(RBP, RSP));
        assert_emit!(0x48, 0x89, 0xec; movq_rr(RSP, RBP));
    }

    #[test]
    fn test_emit_movl_rr() {
        assert_emit!(0x41, 0x89, 0xc7; movl_rr(R15, RAX));
        assert_emit!(0x44, 0x89, 0xf8; movl_rr(RAX, R15));
        assert_emit!(0x89, 0xc1; movl_rr(RCX, RAX));
    }

    #[test]
    fn test_emit_addq_rr() {
        assert_emit!(0x48, 0x01, 0xD8; addq_rr(RAX, RBX));
        assert_emit!(0x4C, 0x01, 0xE0; addq_rr(RAX, R12));
        assert_emit!(0x49, 0x01, 0xC4; addq_rr(R12, RAX));
        assert_emit!(0x49, 0x01, 0xE7; addq_rr(R15, RSP));
    }

    #[test]
    fn test_emit_addl_rr() {
        assert_emit!(0x01, 0xd8; addl_rr(RAX, RBX));
        assert_emit!(0x44, 0x01, 0xf9; addl_rr(RCX, R15));
    }

    #[test]
    fn test_cdq_cqo() {
        assert_emit!(0x99; cdq);
        assert_emit!(0x48, 0x99; cqo);
    }

    #[test]
    fn test_setcc_r() {
        assert_emit!(0x0f, 0x94, 0xc0; setcc_r(Condition::Equal, RAX));
        assert_emit!(0x41, 0x0f, 0x95, 0xc7; setcc_r(Condition::NotEqual, R15));
        assert_emit!(0x0f, 0x9d, 0xc1; setcc_r(Condition::GreaterOrEqual, RCX));
        assert_emit!(0x0f, 0x9f, 0xc2; setcc_r(Condition::Greater, RDX));
        assert_emit!(0x40, 0x0f, 0x9e, 0xc6; setcc_r(Condition::LessOrEqual, RSI));
        assert_emit!(0x40, 0x0f, 0x9c, 0xc7; setcc_r(Condition::Less, RDI));
    }

    #[test]
    fn test_xorl_rr() {
        assert_emit!(0x44, 0x31, 0xf8; xorl_rr(RAX, R15));
        assert_emit!(0x31, 0xc8; xorl_rr(RAX, RCX));
        assert_emit!(0x41, 0x31, 0xc7; xorl_rr(R15, RAX));
    }

    #[test]
    fn test_xorq_rr() {
        assert_emit!(0x4C, 0x31, 0xf8; xorq_rr(RAX, R15));
        assert_emit!(0x48, 0x31, 0xc8; xorq_rr(RAX, RCX));
        assert_emit!(0x49, 0x31, 0xc7; xorq_rr(R15, RAX));
    }

    #[test]
    fn test_testl_rr() {
        assert_emit!(0x85, 0xc0; testl_rr(RAX, RAX));
        assert_emit!(0x85, 0xc6; testl_rr(RSI, RAX));
        assert_emit!(0x41, 0x85, 0xc7; testl_rr(R15, RAX));
    }

    #[test]
    fn test_testq_rr() {
        assert_emit!(0x48, 0x85, 0xc0; testq_rr(RAX, RAX));
        assert_emit!(0x48, 0x85, 0xc6; testq_rr(RSI, RAX));
        assert_emit!(0x49, 0x85, 0xc7; testq_rr(R15, RAX));
    }

    #[test]
    fn test_subl_rr() {
        assert_emit!(0x29, 0xd8; subl_rr(RAX, RBX));
        assert_emit!(0x44, 0x29, 0xf9; subl_rr(RCX, R15));
    }

    #[test]
    fn test_subq_rr() {
        assert_emit!(0x48, 0x29, 0xd8; subq_rr(RAX, RBX));
        assert_emit!(0x4c, 0x29, 0xf9; subq_rr(RCX, R15));
    }

    #[test]
    fn test_andl_rr() {
        assert_emit!(0x44, 0x21, 0xf8; andl_rr(RAX, R15));
        assert_emit!(0x21, 0xc8; andl_rr(RAX, RCX));
        assert_emit!(0x41, 0x21, 0xc7; andl_rr(R15, RAX));
    }

    #[test]
    fn test_andq_rr() {
        assert_emit!(0x4C, 0x21, 0xf8; andq_rr(RAX, R15));
        assert_emit!(0x48, 0x21, 0xc8; andq_rr(RAX, RCX));
        assert_emit!(0x49, 0x21, 0xc7; andq_rr(R15, RAX));
    }

    #[test]
    fn test_orl_rr() {
        assert_emit!(0x44, 0x09, 0xf8; orl_rr(RAX, R15));
        assert_emit!(0x09, 0xc8; orl_rr(RAX, RCX));
        assert_emit!(0x41, 0x09, 0xc7; orl_rr(R15, RAX));
    }

    #[test]
    fn test_orq_rr() {
        assert_emit!(0x4c, 0x09, 0xf8; orq_rr(RAX, R15));
        assert_emit!(0x48, 0x09, 0xc8; orq_rr(RAX, RCX));
        assert_emit!(0x49, 0x09, 0xc7; orq_rr(R15, RAX));
    }

    #[test]
    fn test_cmpl_rr() {
        assert_emit!(0x44, 0x39, 0xf8; cmpl_rr(RAX, R15));
        assert_emit!(0x41, 0x39, 0xdf; cmpl_rr(R15, RBX));
        assert_emit!(0x39, 0xd8; cmpl_rr(RAX, RBX));
    }

    #[test]
    fn test_cmpq_rr() {
        assert_emit!(0x4C, 0x39, 0xf8; cmpq_rr(RAX, R15));
        assert_emit!(0x49, 0x39, 0xdf; cmpq_rr(R15, RBX));
        assert_emit!(0x48, 0x39, 0xd8; cmpq_rr(RAX, RBX));
    }

    #[test]
    fn test_imull_rr() {
        assert_emit!(0x0f, 0xaf, 0xc3; imull_rr(RAX, RBX));
        assert_emit!(0x41, 0x0f, 0xaf, 0xcf; imull_rr(RCX, R15));
    }

    #[test]
    fn test_imulq_rr() {
        assert_emit!(0x48, 0x0f, 0xaf, 0xc3; imulq_rr(RAX, RBX));
        assert_emit!(0x49, 0x0f, 0xaf, 0xcf; imulq_rr(RCX, R15));
    }

    #[test]
    fn test_idivl_r() {
        assert_emit!(0xf7, 0xf8; idivl_r(RAX));
        assert_emit!(0x41, 0xf7, 0xff; idivl_r(R15));
    }

    #[test]
    fn test_idivq_r() {
        assert_emit!(0x48, 0xf7, 0xf8; idivq_r(RAX));
        assert_emit!(0x49, 0xf7, 0xff; idivq_r(R15));
    }

    #[test]
    fn test_call_r() {
        assert_emit!(0xff, 0xd0; call_r(RAX));
        assert_emit!(0x41, 0xff, 0xd7; call_r(R15));
    }

    #[test]
    fn test_movl_ri() {
        assert_emit!(0xb8, 2, 0, 0, 0; movl_ri(RAX, Immediate(2)));
        assert_emit!(0x41, 0xbe, 3, 0, 0, 0; movl_ri(R14, Immediate(3)));
    }

    #[test]
    fn test_movq_ri() {
        assert_emit!(0x48, 0xc7, 0xc0, 1, 0, 0, 0; movq_ri(RAX, Immediate(1)));
        assert_emit!(0x49, 0xc7, 0xc7, 0xFF, 0xFF, 0xFF, 0xFF; movq_ri(R15, Immediate(-1)));
        assert_emit!(0x48, 0xb8, 0, 0, 0, 0, 1, 0, 0, 0; movq_ri(RAX, Immediate(1 << 32)));
    }

    #[test]
    fn test_subq_ri() {
        assert_emit!(0x48, 0x83, 0xe8, 0x11; subq_ri(RAX, Immediate(0x11)));
        assert_emit!(0x49, 0x83, 0xef, 0x11; subq_ri(R15, Immediate(0x11)));
        assert_emit!(0x48, 0x2d, 0x11, 0x22, 0, 0; subq_ri(RAX, Immediate(0x2211)));
        assert_emit!(0x48, 0x81, 0xe9, 0x11, 0x22, 0, 0; subq_ri(RCX, Immediate(0x2211)));
        assert_emit!(0x49, 0x81, 0xef, 0x11, 0x22, 0, 0; subq_ri(R15, Immediate(0x2211)));
    }

    #[test]
    fn test_addq_ri() {
        assert_emit!(0x48, 0x83, 0xc0, 0x11; addq_ri(RAX, Immediate(0x11)));
        assert_emit!(0x49, 0x83, 0xc7, 0x11; addq_ri(R15, Immediate(0x11)));
        assert_emit!(0x48, 0x05, 0x11, 0x22, 0, 0; addq_ri(RAX, Immediate(0x2211)));
        assert_emit!(0x48, 0x81, 0xc1, 0x11, 0x22, 0, 0; addq_ri(RCX, Immediate(0x2211)));
        assert_emit!(0x49, 0x81, 0xc7, 0x11, 0x22, 0, 0; addq_ri(R15, Immediate(0x2211)));
    }

    #[test]
    fn test_andq_ri() {
        assert_emit!(0x48, 0x83, 0xe0, 0xf8; andq_ri(RAX, Immediate(-8)));
        assert_emit!(0x48, 0x25, 0x80, 0, 0, 0; andq_ri(RAX, Immediate(128)));
        assert_emit!(0x49, 0x83, 0xe1, 0xf8; andq_ri(R9, Immediate(-8)));
        assert_emit!(0x49, 0x81, 0xe1, 0x80, 0, 0, 0; andq_ri(R9, Immediate(128)));
    }

    #[test]
    fn test_cmpq_ri() {
        assert_emit!(0x48, 0x83, 0xf8, 0x7f; cmpq_ri(RAX, Immediate(127)));
        assert_emit!(0x49, 0x83, 0xff, 0x80; cmpq_ri(R15, Immediate(-128)));
        assert_emit!(0x49, 0x83, 0xf9, 0; cmpq_ri(R9, Immediate(0)));
    }

    #[test]
    fn test_cmpl_ri() {
        assert_emit!(0x83, 0xf8, 0; cmpl_ri(RAX, Immediate(0)));
        assert_emit!(0x41, 0x83, 0xff, 0; cmpl_ri(R15, Immediate(0)));
        assert_emit!(0x41, 0x83, 0xf9, 0; cmpl_ri(R9, Immediate(0)));
    }

    #[test]
    fn test_cmov() {
        assert_emit!(0x44, 0x0f, 0x44, 0xf8; cmovl(Condition::Equal, R15, RAX));
        assert_emit!(0x41, 0x0f, 0x45, 0xc5; cmovl(Condition::NotEqual, RAX, R13));
        assert_emit!(0x48, 0x0f, 0x4f, 0xc1; cmovq(Condition::Greater, RAX, RCX));
    }

    #[test]
    fn test_notl() {
        assert_emit!(0xf7, 0xd0; notl(RAX));
        assert_emit!(0x41, 0xf7, 0xd7; notl(R15));
    }

    #[test]
    fn test_notq() {
        assert_emit!(0x48, 0xf7, 0xd0; notq(RAX));
        assert_emit!(0x49, 0xf7, 0xd7; notq(R15));
    }

    #[test]
    fn test_negl() {
        assert_emit!(0xf7, 0xd8; negl(RAX));
        assert_emit!(0x41, 0xf7, 0xdf; negl(R15));
    }

    #[test]
    fn test_negq() {
        assert_emit!(0x48, 0xf7, 0xd8; negq(RAX));
        assert_emit!(0x49, 0xf7, 0xdf; negq(R15));
    }

    #[test]
    fn test_movzxb_rr() {
        assert_emit!(0x0f, 0xb6, 0xc0; movzxb_rr(RAX, RAX));
        assert_emit!(0x40, 0x0f, 0xb6, 0xc7; movzxb_rr(RAX, RDI));
        assert_emit!(0x0f, 0xb6, 0xf8; movzxb_rr(RDI, RAX));
        assert_emit!(0x41, 0x0f, 0xb6, 0xc7; movzxb_rr(RAX, R15));
        assert_emit!(0x44, 0x0f, 0xb6, 0xfb; movzxb_rr(R15, RBX));
        assert_emit!(0x40, 0x0f, 0xb6, 0xce; movzxb_rr(RCX, RSI));
    }

    #[test]
    #[should_panic]
    fn test_address_array_with_rsp_index() {
        Address::array(RAX, RSP, ScaleFactor::Four, 0);
    }

    #[test]
    #[should_panic]
    fn test_address_index_with_rsp_index() {
        Address::index(RSP, ScaleFactor::Four, 0);
    }

    #[test]
    fn test_movq_ra() {
        assert_emit!(0x48, 0x8b, 0x04, 0x24; movq_ra(RAX, Address::offset(RSP, 0)));
        assert_emit!(0x48, 0x8b, 0x44, 0x24, 1; movq_ra(RAX, Address::offset(RSP, 1)));

        assert_emit!(0x4c, 0x8b, 0x3c, 0x24; movq_ra(R15, Address::offset(RSP, 0)));
        assert_emit!(0x4c, 0x8b, 0x7c, 0x24, 1; movq_ra(R15, Address::offset(RSP, 1)));

        assert_emit!(0x4c, 0x8b, 0x7c, 0x24, 0x7f; movq_ra(R15, Address::offset(RSP, 127)));
        assert_emit!(0x4c, 0x8b, 0x7c, 0x24, 0x80; movq_ra(R15, Address::offset(RSP, -128)));

        assert_emit!(0x4c, 0x8b, 0xbc, 0x24, 0x80, 0, 0, 0; movq_ra(R15, Address::offset(RSP, 128)));
        assert_emit!(0x4c, 0x8b, 0xbc, 0x24, 0x7f, 0xff, 0xff, 0xff; movq_ra(R15, Address::offset(RSP, -129)));

        assert_emit!(0x48, 0x8b, 0x45, 0; movq_ra(RAX, Address::offset(RBP, 0)));
        assert_emit!(0x48, 0x8b, 0x45, 1; movq_ra(RAX, Address::offset(RBP, 1)));

        assert_emit!(0x48, 0x8b, 0x04, 0xf8; movq_ra(RAX, Address::array(RAX, RDI, ScaleFactor::Eight, 0)));
        assert_emit!(0x48, 0x8b, 0x44, 0xf8, 1; movq_ra(RAX, Address::array(RAX, RDI, ScaleFactor::Eight, 1)));
        assert_emit!(0x48, 0x8b, 0x44, 0xf8, 0x7f; movq_ra(RAX, Address::array(RAX, RDI, ScaleFactor::Eight, 127)));
        assert_emit!(0x48, 0x8b, 0x44, 0xf8, 0x80; movq_ra(RAX, Address::array(RAX, RDI, ScaleFactor::Eight, -128)));
        assert_emit!(0x48, 0x8b, 0x84, 0xf8, 0x80, 0, 0, 0; movq_ra(RAX, Address::array(RAX, RDI, ScaleFactor::Eight, 128)));
        assert_emit!(0x48, 0x8b, 0x84, 0xf8, 0x7f, 0xff, 0xff, 0xff; movq_ra(RAX, Address::array(RAX, RDI, ScaleFactor::Eight, -129)));

        assert_emit!(0x48, 0x8b, 0x44, 0xed, 0; movq_ra(RAX, Address::array(RBP, RBP, ScaleFactor::Eight, 0)));
        assert_emit!(0x49, 0x8b, 0x44, 0xed, 0; movq_ra(RAX, Address::array(R13, RBP, ScaleFactor::Eight, 0)));
        assert_emit!(0x48, 0x8b, 0x04, 0xe8; movq_ra(RAX, Address::array(RAX, RBP, ScaleFactor::Eight, 0)));
        assert_emit!(0x48, 0x8b, 0x44, 0xc5, 0; movq_ra(RAX, Address::array(RBP, RAX, ScaleFactor::Eight, 0)));
        assert_emit!(0x49, 0x8b, 0x44, 0xc5, 0; movq_ra(RAX, Address::array(R13, RAX, ScaleFactor::Eight, 0)));

        assert_emit!(0x48, 0x8b, 0x04, 0xed, 0, 0, 0, 0; movq_ra(RAX, Address::index(RBP, ScaleFactor::Eight, 0)));
        assert_emit!(0x48, 0x8b, 0x04, 0xed, 1, 0, 0, 0; movq_ra(RAX, Address::index(RBP, ScaleFactor::Eight, 1)));
    }

    #[test]
    fn test_movq_ar() {
        assert_emit!(0x48, 0x89, 0x45, 0; movq_ar(Address::offset(RBP, 0), RAX));
        assert_emit!(0x48, 0x89, 0x44, 0xa8, 1; movq_ar(Address::array(RAX, RBP, ScaleFactor::Four, 1), RAX));
        assert_emit!(0x49, 0x89, 0x45, 0; movq_ar(Address::offset(R13, 0), RAX));
    }

    #[test]
    fn test_movl_ar() {
        assert_emit!(0x89, 0x45, 0; movl_ar(Address::offset(RBP, 0), RAX));
        assert_emit!(0x44, 0x89, 0x7d, 0; movl_ar(Address::offset(RBP, 0), R15));
        assert_emit!(0x41, 0x89, 0x1c, 0x24; movl_ar(Address::offset(R12, 0), RBX));
        assert_emit!(0x45, 0x89, 0x38; movl_ar(Address::offset(R8, 0), R15));
        assert_emit!(0x47, 0x89, 0x3c, 0x88; movl_ar(Address::array(R8, R9, ScaleFactor::Four, 0), R15));
        assert_emit!(0x89, 0x44, 0xa8, 1; movl_ar(Address::array(RAX, RBP, ScaleFactor::Four, 1), RAX));
    }

    #[test]
    fn test_movl_ra() {
        assert_emit!(0x8b, 0x45, 0; movl_ra(RAX, Address::offset(RBP, 0)));
        assert_emit!(0x8b, 0x05, 0, 0, 0, 0; movl_ra(RAX, Address::rip(0)));
    }

    #[test]
    fn test_cmpq_ar() {
        assert_emit!(0x48, 0x39, 0x43, 1; cmpq_ar(Address::offset(RBX, 1), RAX));
        assert_emit!(0x48, 0x39, 0x83, 0, 1, 0, 0; cmpq_ar(Address::offset(RBX, 256), RAX));
        assert_emit!(0x48, 0x39, 0x47, 1; cmpq_ar(Address::offset(RDI, 1), RAX));
        assert_emit!(0x49, 0x39, 0x41, 1; cmpq_ar(Address::offset(R9, 1), RAX));
        assert_emit!(0x4c, 0x39, 0x57, 1; cmpq_ar(Address::offset(RDI, 1), R10));
        assert_emit!(0x48, 0x39, 0x05, 1, 0, 0, 0; cmpq_ar(Address::rip(1), RAX));
    }

    #[test]
    fn test_cmpl_ar() {
        assert_emit!(0x39, 0x43, 1; cmpl_ar(Address::offset(RBX, 1), RAX));
        assert_emit!(0x44, 0x39, 0x53, 1; cmpl_ar(Address::offset(RBX, 1), R10));
    }

    #[test]
    fn test_testl_ar() {
        assert_emit!(0x85, 0x47, 1; testl_ar(Address::offset(RDI, 1), RAX));
        assert_emit!(0x85, 0x07; testl_ar(Address::offset(RDI, 0), RAX));

        assert_emit!(0x41, 0x85, 0x00; testl_ar(Address::offset(R8, 0), RAX));
        assert_emit!(0x41, 0x85, 0x40, 1; testl_ar(Address::offset(R8, 1), RAX));
        assert_emit!(0x45, 0x85, 0x38; testl_ar(Address::offset(R8, 0), R15));

        assert_emit!(0x44, 0x85, 0x38; testl_ar(Address::offset(RAX, 0), R15));
    }

    #[test]
    fn test_testq_ar() {
        assert_emit!(0x48, 0x85, 0x47, 1; testq_ar(Address::offset(RDI, 1), RAX));
        assert_emit!(0x4D, 0x85, 0x38; testq_ar(Address::offset(R8, 0), R15));
    }

    #[test]
    fn test_testb_ai() {
        assert_emit!(0xf6, 0x00, 1; testb_ai(Address::offset(RAX, 0), Immediate(1)));
        assert_emit!(0xf6, 0x40, 1, 1; testb_ai(Address::offset(RAX, 1), Immediate(1)));

        assert_emit!(0x41, 0xf6, 0x40, 0x09, 128; testb_ai(Address::offset(R8, 9), Immediate(128)));
    }

    #[test]
    fn test_testb_rr() {
        assert_emit!(0x84, 0xC0; testb_rr(RAX, RAX));
        assert_emit!(0x84, 0xDB; testb_rr(RBX, RBX));
        assert_emit!(0x40, 0x84, 0xC6; testb_rr(RSI, RAX));
        assert_emit!(0x41, 0x84, 0xC7; testb_rr(R15, RAX));
        assert_emit!(0x40, 0x84, 0xFF; testb_rr(RDI, RDI));
        assert_emit!(0x40, 0x84, 0xF3; testb_rr(RBX, RSI));
    }

    #[test]
    fn test_testl_ai() {
        assert_emit!(0xf7, 0x00, 1, 0, 0, 0; testl_ai(Address::offset(RAX, 0), Immediate(1)));
        assert_emit!(0xf7, 0x40, 1, 1, 0, 0, 0; testl_ai(Address::offset(RAX, 1), Immediate(1)));

        assert_emit!(0x41, 0xf7, 0x00, 1, 0, 0, 0; testl_ai(Address::offset(R8, 0), Immediate(1)));
    }

    #[test]
    fn test_testq_ai() {
        assert_emit!(0x48, 0xf7, 0x00, 1, 0, 0, 0; testq_ai(Address::offset(RAX, 0), Immediate(1)));
        assert_emit!(0x48, 0xf7, 0x40, 1, 1, 0, 0, 0; testq_ai(Address::offset(RAX, 1), Immediate(1)));

        assert_emit!(0x49, 0xf7, 0x00, 1, 0, 0, 0; testq_ai(Address::offset(R8, 0), Immediate(1)));
    }

    #[test]
    fn test_testl_ri() {
        assert_emit!(0xa8, 1; testl_ri(RAX, Immediate(1)));
        assert_emit!(0xf6, 0xc1, 255; testl_ri(RCX, Immediate(255)));
        assert_emit!(0xf6, 0xc2, 1; testl_ri(RDX, Immediate(1)));
        assert_emit!(0xf6, 0xc3, 1; testl_ri(RBX, Immediate(1)));

        assert_emit!(0x40, 0xf6, 0xc6, 1; testl_ri(RSI, Immediate(1)));
        assert_emit!(0x40, 0xf6, 0xc7, 1; testl_ri(RDI, Immediate(1)));
        assert_emit!(0x41, 0xf6, 0xc0, 1; testl_ri(R8, Immediate(1)));
        assert_emit!(0x41, 0xf6, 0xc7, 1; testl_ri(R15, Immediate(1)));

        assert_emit!(0xa9, 0, 1, 0, 0; testl_ri(RAX, Immediate(256)));
        assert_emit!(0xf7, 0xc7, 0, 1, 0, 0; testl_ri(RDI, Immediate(256)));
    }

    #[test]
    fn test_lea() {
        assert_emit!(0x48, 0x8d, 0x00; lea(RAX, Address::offset(RAX, 0)));
        assert_emit!(0x48, 0x8d, 0x40, 1; lea(RAX, Address::offset(RAX, 1)));
        assert_emit!(0x49, 0x8d, 0x00; lea(RAX, Address::offset(R8, 0)));
        assert_emit!(0x4c, 0x8d, 0x00; lea(R8, Address::offset(RAX, 0)));
    }

    #[test]
    fn test_movb_ar() {
        assert_emit!(0x88, 0x04, 0x24; movb_ar(Address::offset(RSP, 0), RAX));
        assert_emit!(0x40, 0x88, 0x34, 0x24; movb_ar(Address::offset(RSP, 0), RSI));
        assert_emit!(0x44, 0x88, 0x04, 0x24; movb_ar(Address::offset(RSP, 0), R8));
    }

    #[test]
    fn test_movb_ra() {
        assert_emit!(0x8A, 0x04, 0x24; movb_ra(RAX, Address::offset(RSP, 0)));
        assert_emit!(0x40, 0x8A, 0x34, 0x24; movb_ra(RSI, Address::offset(RSP, 0)));
        assert_emit!(0x44, 0x8A, 0x04, 0x24; movb_ra(R8, Address::offset(RSP, 0)));
    }

    #[test]
    fn test_movzxb_ra() {
        assert_emit!(0x0f, 0xb6, 0x00; movzxb_ra(RAX, Address::offset(RAX, 0)));
        assert_emit!(0x44, 0x0f, 0xb6, 0x00; movzxb_ra(R8, Address::offset(RAX, 0)));
        assert_emit!(0x41, 0x0f, 0xb6, 0x00; movzxb_ra(RAX, Address::offset(R8, 0)));
    }

    #[test]
    fn test_movsxlq_rr() {
        assert_emit!(0x4c, 0x63, 0xf8; movsxlq_rr(R15, RAX));
        assert_emit!(0x49, 0x63, 0xc7; movsxlq_rr(RAX, R15));
    }

    #[test]
    fn test_movsxbl_rr() {
        assert_emit!(0x0f, 0xbe, 0xc0; movsxbl_rr(RAX, RAX));
        assert_emit!(0x41, 0x0f, 0xbe, 0xc0; movsxbl_rr(RAX, R8));
        assert_emit!(0x0f, 0xbe, 0xe0; movsxbl_rr(RSP, RAX));
        assert_emit!(0x44, 0x0f, 0xbe, 0xf8; movsxbl_rr(R15, RAX));

        assert_emit!(0x0f, 0xbe, 0xc3; movsxbl_rr(RAX, RBX));
        assert_emit!(0x40, 0x0f, 0xbe, 0xc4; movsxbl_rr(RAX, RSP));
        assert_emit!(0x40, 0x0f, 0xbe, 0xc7; movsxbl_rr(RAX, RDI));
    }

    #[test]
    fn test_movsxbl_ra() {
        assert_emit!(0x0f, 0xbe, 0x00; movsxbl_ra(RAX, Address::offset(RAX, 0)));
        assert_emit!(0x44, 0x0f, 0xbe, 0x00; movsxbl_ra(R8, Address::offset(RAX, 0)));
        assert_emit!(0x0f, 0xbe, 0x20; movsxbl_ra(RSP, Address::offset(RAX, 0)));
        assert_emit!(0x0f, 0xbe, 0x38; movsxbl_ra(RDI, Address::offset(RAX, 0)));
    }

    #[test]
    fn test_movsxbq_rr() {
        assert_emit!(0x48, 0x0f, 0xbe, 0xc0; movsxbq_rr(RAX, RAX));
        assert_emit!(0x49, 0x0f, 0xbe, 0xc0; movsxbq_rr(RAX, R8));
        assert_emit!(0x48, 0x0f, 0xbe, 0xe0; movsxbq_rr(RSP, RAX));
        assert_emit!(0x4c, 0x0f, 0xbe, 0xf8; movsxbq_rr(R15, RAX));

        assert_emit!(0x48, 0x0f, 0xbe, 0xc3; movsxbq_rr(RAX, RBX));
        assert_emit!(0x48, 0x0f, 0xbe, 0xc4; movsxbq_rr(RAX, RSP));
        assert_emit!(0x48, 0x0f, 0xbe, 0xc7; movsxbq_rr(RAX, RDI));
    }

    #[test]
    fn test_movsxbq_ra() {
        assert_emit!(0x48, 0x0f, 0xbe, 0x00; movsxbq_ra(RAX, Address::offset(RAX, 0)));
        assert_emit!(0x4c, 0x0f, 0xbe, 0x00; movsxbq_ra(R8, Address::offset(RAX, 0)));
        assert_emit!(0x48, 0x0f, 0xbe, 0x20; movsxbq_ra(RSP, Address::offset(RAX, 0)));
        assert_emit!(0x48, 0x0f, 0xbe, 0x38; movsxbq_ra(RDI, Address::offset(RAX, 0)));
    }

    #[test]
    fn test_movb_ai() {
        assert_emit!(0xc6, 0x00, 1; movb_ai(Address::offset(RAX, 0), Immediate(1)));
        assert_emit!(0xc6, 0x00, 0x7f; movb_ai(Address::offset(RAX, 0), Immediate(127)));
        assert_emit!(0xc6, 0x00, 0xff; movb_ai(Address::offset(RAX, 0), Immediate(255)));
        assert_emit!(0xc6, 0x00, 0x80; movb_ai(Address::offset(RAX, 0), Immediate(-128)));
        assert_emit!(0xc6, 0x00, 0x80; movb_ai(Address::offset(RAX, 0), Immediate(128)));
    }

    #[test]
    fn test_xorl_ri() {
        assert_emit!(0x83, 0xf0, 1; xorl_ri(RAX, Immediate(1)));
        assert_emit!(0x41, 0x83, 0xf0, 0x7f; xorl_ri(R8, Immediate(127)));
        assert_emit!(0x41, 0x83, 0xf7, 0x80; xorl_ri(R15, Immediate(-128)));

        assert_emit!(0x35, 0x80, 0, 0, 0; xorl_ri(RAX, Immediate(128)));
        assert_emit!(0x41, 0x81, 0xf0, 0x7f, 0xff, 0xff, 0xff; xorl_ri(R8, Immediate(-129)));
        assert_emit!(0x41, 0x81, 0xf7, 0x80, 0, 0, 0; xorl_ri(R15, Immediate(128)));
    }

    #[test]
    fn test_jmp_r() {
        assert_emit!(0xFF, 0xE0; jmp_r(RAX));
        assert_emit!(0x41, 0xFF, 0xE0; jmp_r(R8));
    }

    #[test]
    fn test_cmpb_ar() {
        assert_emit!(0x38, 0x00; cmpb_ar(Address::offset(RAX, 0), RAX));
        assert_emit!(0x38, 0x18; cmpb_ar(Address::offset(RAX, 0), RBX));
        assert_emit!(0x40, 0x38, 0x20; cmpb_ar(Address::offset(RAX, 0), RSP));
        assert_emit!(0x40, 0x38, 0x38; cmpb_ar(Address::offset(RAX, 0), RDI));
        assert_emit!(0x44, 0x38, 0x00; cmpb_ar(Address::offset(RAX, 0), R8));
        assert_emit!(0x41, 0x38, 0x00; cmpb_ar(Address::offset(R8, 0), RAX));
    }

    #[test]
    fn test_cmpb_rr() {
        assert_emit!(0x38, 0xC0; cmpb_rr(RAX, RAX));
        assert_emit!(0x38, 0xDB; cmpb_rr(RBX, RBX));
        assert_emit!(0x40, 0x38, 0xC6; cmpb_rr(RSI, RAX));
        assert_emit!(0x41, 0x38, 0xC7; cmpb_rr(R15, RAX));
        assert_emit!(0x40, 0x38, 0xFF; cmpb_rr(RDI, RDI));
        assert_emit!(0x40, 0x38, 0xF3; cmpb_rr(RBX, RSI));
    }

    #[test]
    fn test_cmpb_ai() {
        assert_emit!(0x80, 0x38, 1; cmpb_ai(Address::offset(RAX, 0), Immediate(1)));
        assert_emit!(0x80, 0x38, 0x7f; cmpb_ai(Address::offset(RAX, 0), Immediate(127)));
        assert_emit!(0x80, 0x38, 0x80; cmpb_ai(Address::offset(RAX, 0), Immediate(-128)));
        assert_emit!(0x80, 0x38, 0xff; cmpb_ai(Address::offset(RAX, 0), Immediate(255)));
        assert_emit!(0x41, 0x80, 0x38, 0xff; cmpb_ai(Address::offset(R8, 0), Immediate(255)));
    }

    #[test]
    fn test_cmpl_ai() {
        assert_emit!(0x83, 0x38, 0x7f; cmpl_ai(Address::offset(RAX, 0), Immediate(127)));
        assert_emit!(0x83, 0x38, 0x80; cmpl_ai(Address::offset(RAX, 0), Immediate(-128)));
        assert_emit!(0x81, 0x38, 0x80, 0, 0, 0; cmpl_ai(Address::offset(RAX, 0), Immediate(128)));
        assert_emit!(0x41, 0x81, 0x38, 0x80, 0, 0, 0; cmpl_ai(Address::offset(R8, 0), Immediate(128)));
    }

    #[test]
    fn test_cmpq_ai() {
        assert_emit!(0x48, 0x83, 0x38, 0x7f; cmpq_ai(Address::offset(RAX, 0), Immediate(127)));
        assert_emit!(0x48, 0x83, 0x38, 0x80; cmpq_ai(Address::offset(RAX, 0), Immediate(-128)));
        assert_emit!(0x48, 0x81, 0x38, 0x80, 0, 0, 0; cmpq_ai(Address::offset(RAX, 0), Immediate(128)));
        assert_emit!(0x49, 0x81, 0x38, 0x80, 0, 0, 0; cmpq_ai(Address::offset(R8, 0), Immediate(128)));
    }

    #[test]
    fn test_movss_ra() {
        assert_emit!(0xf3, 0x0f, 0x10, 0x00; movss_ra(XMM0, Address::offset(RAX, 0)));
        assert_emit!(0xf3, 0x44, 0x0f, 0x10, 0x00; movss_ra(XMM8, Address::offset(RAX, 0)));

        assert_emit!(0xf3, 0x41, 0x0f, 0x10, 0x00; movss_ra(XMM0, Address::offset(R8, 0)));
        assert_emit!(0xf3, 0x45, 0x0f, 0x10, 0x07; movss_ra(XMM8, Address::offset(R15, 0)));

        assert_emit!(0xf3, 0x44, 0x0f, 0x10, 0x08; movss_ra(XMM9, Address::offset(RAX, 0)));
        assert_emit!(0xf3, 0x44, 0x0f, 0x10, 0x38; movss_ra(XMM15, Address::offset(RAX, 0)));
    }

    #[test]
    fn test_movsd_ra() {
        assert_emit!(0xf2, 0x0f, 0x10, 0x00; movsd_ra(XMM0, Address::offset(RAX, 0)));
        assert_emit!(0xf2, 0x44, 0x0f, 0x10, 0x00; movsd_ra(XMM8, Address::offset(RAX, 0)));

        assert_emit!(0xf2, 0x41, 0x0f, 0x10, 0x00; movsd_ra(XMM0, Address::offset(R8, 0)));
        assert_emit!(0xf2, 0x45, 0x0f, 0x10, 0x07; movsd_ra(XMM8, Address::offset(R15, 0)));

        assert_emit!(0xf2, 0x44, 0x0f, 0x10, 0x08; movsd_ra(XMM9, Address::offset(RAX, 0)));
        assert_emit!(0xf2, 0x44, 0x0f, 0x10, 0x38; movsd_ra(XMM15, Address::offset(RAX, 0)));
    }

    #[test]
    fn test_movss_ar() {
        assert_emit!(0xf3, 0x0f, 0x11, 0x00; movss_ar(Address::offset(RAX, 0), XMM0));
        assert_emit!(0xf3, 0x44, 0x0f, 0x11, 0x00; movss_ar(Address::offset(RAX, 0), XMM8));
    }

    #[test]
    fn test_movsd_ar() {
        assert_emit!(0xf2, 0x0f, 0x11, 0x00; movsd_ar(Address::offset(RAX, 0), XMM0));
        assert_emit!(0xf2, 0x44, 0x0f, 0x11, 0x00; movsd_ar(Address::offset(RAX, 0), XMM8));
    }

    #[test]
    fn test_movss_rr() {
        assert_emit!(0xf3, 0x0f, 0x10, 0xc1; movss_rr(XMM0, XMM1));
        assert_emit!(0xf3, 0x44, 0x0f, 0x10, 0xc1; movss_rr(XMM8, XMM1));
        assert_emit!(0xf3, 0x41, 0x0f, 0x10, 0xc1; movss_rr(XMM0, XMM9));
    }

    #[test]
    fn test_movsd_rr() {
        assert_emit!(0xf2, 0x0f, 0x10, 0xc1; movsd_rr(XMM0, XMM1));
        assert_emit!(0xf2, 0x44, 0x0f, 0x10, 0xc1; movsd_rr(XMM8, XMM1));
        assert_emit!(0xf2, 0x41, 0x0f, 0x10, 0xc1; movsd_rr(XMM0, XMM9));
    }

    #[test]
    fn test_addss_rr() {
        assert_emit!(0xf3, 0x0f, 0x58, 0xc1; addss_rr(XMM0, XMM1));
        assert_emit!(0xf3, 0x41, 0x0f, 0x58, 0xdf; addss_rr(XMM3, XMM15));
        assert_emit!(0xf3, 0x44, 0x0f, 0x58, 0xc4; addss_rr(XMM8, XMM4));
    }

    #[test]
    fn test_addsd_rr() {
        assert_emit!(0xf2, 0x0f, 0x58, 0xc1; addsd_rr(XMM0, XMM1));
        assert_emit!(0xf2, 0x41, 0x0f, 0x58, 0xdf; addsd_rr(XMM3, XMM15));
        assert_emit!(0xf2, 0x44, 0x0f, 0x58, 0xc4; addsd_rr(XMM8, XMM4));
    }

    #[test]
    fn test_subss_rr() {
        assert_emit!(0xf3, 0x0f, 0x5c, 0xc1; subss_rr(XMM0, XMM1));
        assert_emit!(0xf3, 0x41, 0x0f, 0x5c, 0xdf; subss_rr(XMM3, XMM15));
        assert_emit!(0xf3, 0x44, 0x0f, 0x5c, 0xc4; subss_rr(XMM8, XMM4));
    }

    #[test]
    fn test_subsd_rr() {
        assert_emit!(0xf2, 0x0f, 0x5c, 0xc1; subsd_rr(XMM0, XMM1));
        assert_emit!(0xf2, 0x41, 0x0f, 0x5c, 0xdf; subsd_rr(XMM3, XMM15));
        assert_emit!(0xf2, 0x44, 0x0f, 0x5c, 0xc4; subsd_rr(XMM8, XMM4));
    }

    #[test]
    fn test_mulss_rr() {
        assert_emit!(0xf3, 0x0f, 0x59, 0xc1; mulss_rr(XMM0, XMM1));
        assert_emit!(0xf3, 0x41, 0x0f, 0x59, 0xdf; mulss_rr(XMM3, XMM15));
        assert_emit!(0xf3, 0x44, 0x0f, 0x59, 0xc4; mulss_rr(XMM8, XMM4));
    }

    #[test]
    fn test_mulsd_rr() {
        assert_emit!(0xf2, 0x0f, 0x59, 0xc1; mulsd_rr(XMM0, XMM1));
        assert_emit!(0xf2, 0x41, 0x0f, 0x59, 0xdf; mulsd_rr(XMM3, XMM15));
        assert_emit!(0xf2, 0x44, 0x0f, 0x59, 0xc4; mulsd_rr(XMM8, XMM4));
    }

    #[test]
    fn test_divss_rr() {
        assert_emit!(0xf3, 0x0f, 0x5e, 0xc1; divss_rr(XMM0, XMM1));
        assert_emit!(0xf3, 0x41, 0x0f, 0x5e, 0xdf; divss_rr(XMM3, XMM15));
        assert_emit!(0xf3, 0x44, 0x0f, 0x5e, 0xc4; divss_rr(XMM8, XMM4));
    }

    #[test]
    fn test_divsd_rr() {
        assert_emit!(0xf2, 0x0f, 0x5e, 0xc1; divsd_rr(XMM0, XMM1));
        assert_emit!(0xf2, 0x41, 0x0f, 0x5e, 0xdf; divsd_rr(XMM3, XMM15));
        assert_emit!(0xf2, 0x44, 0x0f, 0x5e, 0xc4; divsd_rr(XMM8, XMM4));
    }

    #[test]
    fn test_ucomiss_rr() {
        assert_emit!(0x0f, 0x2e, 0xc8; ucomiss_rr(XMM1, XMM0));
        assert_emit!(0x44, 0x0f, 0x2e, 0xfb; ucomiss_rr(XMM15, XMM3));
        assert_emit!(0x41, 0x0f, 0x2e, 0xe0; ucomiss_rr(XMM4, XMM8));
    }

    #[test]
    fn test_ucomisd_rr() {
        assert_emit!(0x66, 0x0f, 0x2e, 0xc8; ucomisd_rr(XMM1, XMM0));
        assert_emit!(0x66, 0x44, 0x0f, 0x2e, 0xfb; ucomisd_rr(XMM15, XMM3));
        assert_emit!(0x66, 0x41, 0x0f, 0x2e, 0xe0; ucomisd_rr(XMM4, XMM8));
    }

    #[test]
    fn test_pxor_rr() {
        assert_emit!(0x66, 0x0f, 0xef, 0xc8; pxor_rr(XMM1, XMM0));
        assert_emit!(0x66, 0x44, 0x0f, 0xef, 0xfb; pxor_rr(XMM15, XMM3));
        assert_emit!(0x66, 0x41, 0x0f, 0xef, 0xe0; pxor_rr(XMM4, XMM8));
    }

    #[test]
    fn test_roundss_ri() {
        assert_emit!(0x66, 0x41, 0x0f, 0x3a, 0x0a, 0xc7, 0x08; roundss_ri(XMM0, XMM15, 8)); // roundss xmm0, xmm15, 8
        assert_emit!(0x66, 0x41, 0x0f, 0x3a, 0x0a, 0xf8, 0x09; roundss_ri(XMM7, XMM8, 9)); // roundss xmm7, xmm8, 9
        assert_emit!(0x66, 0x44, 0x0f, 0x3a, 0x0a, 0xc7, 0x0a; roundss_ri(XMM8, XMM7, 10)); // roundss xmm8, xmm7, 10
        assert_emit!(0x66, 0x44, 0x0f, 0x3a, 0x0a, 0xf8, 0x0b; roundss_ri(XMM15, XMM0, 11));
        // roundss xmm15, xmm0, 11
    }

    #[test]
    fn test_roundsd_ri() {
        assert_emit!(0x66, 0x41, 0x0f, 0x3a, 0x0b, 0xc7, 0x08; roundsd_ri(XMM0, XMM15, 8)); // roundsd xmm0, xmm15, 8
        assert_emit!(0x66, 0x41, 0x0f, 0x3a, 0x0b, 0xf8, 0x09; roundsd_ri(XMM7, XMM8, 9)); // roundsd xmm7, xmm8, 9
        assert_emit!(0x66, 0x44, 0x0f, 0x3a, 0x0b, 0xc7, 0x0a; roundsd_ri(XMM8, XMM7, 10)); // roundsd xmm8, xmm7, 10
        assert_emit!(0x66, 0x44, 0x0f, 0x3a, 0x0b, 0xf8, 0x0b; roundsd_ri(XMM15, XMM0, 11));
        // roundsd xmm15, xmm0, 11
    }

    #[test]
    fn test_sqrtss_rr() {
        assert_emit!(0xf3, 0x0f, 0x51, 0xc8; sqrtss_rr(XMM1, XMM0));
        assert_emit!(0xf3, 0x44, 0x0f, 0x51, 0xfb; sqrtss_rr(XMM15, XMM3));
        assert_emit!(0xf3, 0x41, 0x0f, 0x51, 0xe0; sqrtss_rr(XMM4, XMM8));
    }

    #[test]
    fn test_sqrtsd_rr() {
        assert_emit!(0xf2, 0x0f, 0x51, 0xc8; sqrtsd_rr(XMM1, XMM0));
        assert_emit!(0xf2, 0x44, 0x0f, 0x51, 0xfb; sqrtsd_rr(XMM15, XMM3));
        assert_emit!(0xf2, 0x41, 0x0f, 0x51, 0xe0; sqrtsd_rr(XMM4, XMM8));
    }

    #[test]
    fn test_tzcnt() {
        assert_emit!(0xF3, 0x48, 0x0F, 0xBC, 0xF8; tzcntq_rr(RDI, RAX));
        assert_emit!(0xF3, 0x48, 0x0F, 0xBC, 0xC7; tzcntq_rr(RAX, RDI));

        assert_emit!(0xF3, 0x0F, 0xBC, 0xF8; tzcntl_rr(RDI, RAX));
        assert_emit!(0xF3, 0x0F, 0xBC, 0xC7; tzcntl_rr(RAX, RDI));
    }

    #[test]
    fn test_lzcnt() {
        assert_emit!(0xF3, 0x48, 0x0F, 0xBD, 0xF8; lzcntq_rr(RDI, RAX));
        assert_emit!(0xF3, 0x48, 0x0F, 0xBD, 0xC7; lzcntq_rr(RAX, RDI));

        assert_emit!(0xF3, 0x0F, 0xBD, 0xF8; lzcntl_rr(RDI, RAX));
        assert_emit!(0xF3, 0x0F, 0xBD, 0xC7; lzcntl_rr(RAX, RDI));
    }

    #[test]
    fn test_popcnt() {
        assert_emit!(0xF3, 0x48, 0x0F, 0xB8, 0xF8; popcntq_rr(RDI, RAX));
        assert_emit!(0xF3, 0x48, 0x0F, 0xB8, 0xC7; popcntq_rr(RAX, RDI));

        assert_emit!(0xF3, 0x0F, 0xB8, 0xF8; popcntl_rr(RDI, RAX));
        assert_emit!(0xF3, 0x0F, 0xB8, 0xC7; popcntl_rr(RAX, RDI));
    }

    #[test]
    fn test_cvtss2sd_rr() {
        assert_emit!(0xf3, 0x0f, 0x5a, 0xc1; cvtss2sd_rr(XMM0, XMM1));
        assert_emit!(0xf3, 0x41, 0x0f, 0x5a, 0xdf; cvtss2sd_rr(XMM3, XMM15));
        assert_emit!(0xf3, 0x44, 0x0f, 0x5a, 0xc4; cvtss2sd_rr(XMM8, XMM4));
    }

    #[test]
    fn test_cvtsd2ss_rr() {
        assert_emit!(0xf2, 0x0f, 0x5a, 0xc1; cvtsd2ss_rr(XMM0, XMM1));
        assert_emit!(0xf2, 0x41, 0x0f, 0x5a, 0xdf; cvtsd2ss_rr(XMM3, XMM15));
        assert_emit!(0xf2, 0x44, 0x0f, 0x5a, 0xc4; cvtsd2ss_rr(XMM8, XMM4));
    }

    #[test]
    fn test_movd_xr() {
        assert_emit!(0x66, 0x0F, 0x6E, 0xC0; movd_xr(XMM0, RAX));
        assert_emit!(0x66, 0x41, 0x0F, 0x6E, 0xC0; movd_xr(XMM0, R8));
        assert_emit!(0x66, 0x41, 0x0F, 0x6E, 0xF8; movd_xr(XMM7, R8));
        assert_emit!(0x66, 0x45, 0x0F, 0x6E, 0xC0; movd_xr(XMM8, R8));
        assert_emit!(0x66, 0x45, 0x0F, 0x6E, 0xF8; movd_xr(XMM15, R8));
        assert_emit!(0x66, 0x41, 0x0F, 0x6E, 0xC7; movd_xr(XMM0, R15));
        assert_emit!(0x66, 0x41, 0x0F, 0x6E, 0xFF; movd_xr(XMM7, R15));
        assert_emit!(0x66, 0x45, 0x0F, 0x6E, 0xC7; movd_xr(XMM8, R15));
        assert_emit!(0x66, 0x45, 0x0F, 0x6E, 0xFF; movd_xr(XMM15, R15));
    }

    #[test]
    fn test_movq_xr() {
        assert_emit!(0x66, 0x48, 0x0F, 0x6E, 0xC0; movq_xr(XMM0, RAX));
        assert_emit!(0x66, 0x49, 0x0F, 0x6E, 0xC0; movq_xr(XMM0, R8));
        assert_emit!(0x66, 0x49, 0x0F, 0x6E, 0xF8; movq_xr(XMM7, R8));
        assert_emit!(0x66, 0x4D, 0x0F, 0x6E, 0xC0; movq_xr(XMM8, R8));
        assert_emit!(0x66, 0x4D, 0x0F, 0x6E, 0xF8; movq_xr(XMM15, R8));
        assert_emit!(0x66, 0x49, 0x0F, 0x6E, 0xC7; movq_xr(XMM0, R15));
        assert_emit!(0x66, 0x49, 0x0F, 0x6E, 0xFF; movq_xr(XMM7, R15));
        assert_emit!(0x66, 0x4D, 0x0F, 0x6E, 0xC7; movq_xr(XMM8, R15));
        assert_emit!(0x66, 0x4D, 0x0F, 0x6E, 0xFF; movq_xr(XMM15, R15));
    }

    #[test]
    fn test_movd_rx() {
        assert_emit!(0x66, 0x0F, 0x7E, 0xC7; movd_rx(RDI, XMM0));
        assert_emit!(0x66, 0x41, 0x0F, 0x7E, 0xC0; movd_rx(R8, XMM0));
        assert_emit!(0x66, 0x41, 0x0F, 0x7E, 0xF8; movd_rx(R8, XMM7));
        assert_emit!(0x66, 0x45, 0x0F, 0x7E, 0xC0; movd_rx(R8, XMM8));
        assert_emit!(0x66, 0x45, 0x0F, 0x7E, 0xF8; movd_rx(R8, XMM15));
        assert_emit!(0x66, 0x41, 0x0F, 0x7E, 0xC7; movd_rx(R15, XMM0));
        assert_emit!(0x66, 0x41, 0x0F, 0x7E, 0xFF; movd_rx(R15, XMM7));
        assert_emit!(0x66, 0x45, 0x0F, 0x7E, 0xC7; movd_rx(R15, XMM8));
        assert_emit!(0x66, 0x45, 0x0F, 0x7E, 0xFF; movd_rx(R15, XMM15));
    }

    #[test]
    fn test_movq_rx() {
        assert_emit!(0x66, 0x48, 0x0F, 0x7E, 0xC7; movq_rx(RDI, XMM0));
        assert_emit!(0x66, 0x49, 0x0F, 0x7E, 0xC0; movq_rx(R8, XMM0));
        assert_emit!(0x66, 0x49, 0x0F, 0x7E, 0xF8; movq_rx(R8, XMM7));
        assert_emit!(0x66, 0x4D, 0x0F, 0x7E, 0xC0; movq_rx(R8, XMM8));
        assert_emit!(0x66, 0x4D, 0x0F, 0x7E, 0xF8; movq_rx(R8, XMM15));
        assert_emit!(0x66, 0x49, 0x0F, 0x7E, 0xC7; movq_rx(R15, XMM0));
        assert_emit!(0x66, 0x49, 0x0F, 0x7E, 0xFF; movq_rx(R15, XMM7));
        assert_emit!(0x66, 0x4D, 0x0F, 0x7E, 0xC7; movq_rx(R15, XMM8));
        assert_emit!(0x66, 0x4D, 0x0F, 0x7E, 0xFF; movq_rx(R15, XMM15));
    }

    #[test]
    fn test_cvtsi2ssd_rr() {
        assert_emit!(0xf3, 0x0f, 0x2a, 0xc1; cvtsi2ssd_rr(XMM0, RCX));
        assert_emit!(0xf3, 0x41, 0x0f, 0x2a, 0xdf; cvtsi2ssd_rr(XMM3, R15));
        assert_emit!(0xf3, 0x44, 0x0f, 0x2a, 0xc4; cvtsi2ssd_rr(XMM8, RSP));
    }

    #[test]
    fn test_cvtsi2ssq_rr() {
        assert_emit!(0xf3, 0x48, 0x0f, 0x2a, 0xc1; cvtsi2ssq_rr(XMM0, RCX));
        assert_emit!(0xf3, 0x49, 0x0f, 0x2a, 0xdf; cvtsi2ssq_rr(XMM3, R15));
        assert_emit!(0xf3, 0x4c, 0x0f, 0x2a, 0xc4; cvtsi2ssq_rr(XMM8, RSP));
    }

    #[test]
    fn test_cvtsi2sdd_rr() {
        assert_emit!(0xf2, 0x0f, 0x2a, 0xc1; cvtsi2sdd_rr(XMM0, RCX));
        assert_emit!(0xf2, 0x41, 0x0f, 0x2a, 0xdf; cvtsi2sdd_rr(XMM3, R15));
        assert_emit!(0xf2, 0x44, 0x0f, 0x2a, 0xc4; cvtsi2sdd_rr(XMM8, RSP));
    }

    #[test]
    fn test_cvtsi2sdq_rr() {
        assert_emit!(0xf2, 0x48, 0x0f, 0x2a, 0xc1; cvtsi2sdq_rr(XMM0, RCX));
        assert_emit!(0xf2, 0x49, 0x0f, 0x2a, 0xdf; cvtsi2sdq_rr(XMM3, R15));
        assert_emit!(0xf2, 0x4c, 0x0f, 0x2a, 0xc4; cvtsi2sdq_rr(XMM8, RSP));
    }

    #[test]
    fn test_cvttss2sid_rr() {
        assert_emit!(0xf3, 0x0f, 0x2c, 0xc8; cvttss2sid_rr(RCX, XMM0));
        assert_emit!(0xf3, 0x44, 0x0f, 0x2c, 0xfb; cvttss2sid_rr(R15, XMM3));
        assert_emit!(0xf3, 0x41, 0x0f, 0x2c, 0xe0; cvttss2sid_rr(RSP, XMM8));
    }

    #[test]
    fn test_cvttss2siq_rr() {
        assert_emit!(0xf3, 0x48, 0x0f, 0x2c, 0xc8; cvttss2siq_rr(RCX, XMM0));
        assert_emit!(0xf3, 0x4c, 0x0f, 0x2c, 0xfb; cvttss2siq_rr(R15, XMM3));
        assert_emit!(0xf3, 0x49, 0x0f, 0x2c, 0xe0; cvttss2siq_rr(RSP, XMM8));
    }

    #[test]
    fn test_cvttsd2sid_rr() {
        assert_emit!(0xf2, 0x0f, 0x2c, 0xc8; cvttsd2sid_rr(RCX, XMM0));
        assert_emit!(0xf2, 0x44, 0x0f, 0x2c, 0xfb; cvttsd2sid_rr(R15, XMM3));
        assert_emit!(0xf2, 0x41, 0x0f, 0x2c, 0xe0; cvttsd2sid_rr(RSP, XMM8));
    }

    #[test]
    fn test_cvttsd2siq_rr() {
        assert_emit!(0xf2, 0x48, 0x0f, 0x2c, 0xc8; cvttsd2siq_rr(RCX, XMM0));
        assert_emit!(0xf2, 0x4c, 0x0f, 0x2c, 0xfb; cvttsd2siq_rr(R15, XMM3));
        assert_emit!(0xf2, 0x49, 0x0f, 0x2c, 0xe0; cvttsd2siq_rr(RSP, XMM8));
    }

    #[test]
    fn test_shrl_ri() {
        assert_emit!(0xc1, 0xe8, 2; shrl_ri(RAX, Immediate(2)));
        assert_emit!(0x41, 0xc1, 0xe8, 2; shrl_ri(R8, Immediate(2)));
    }

    #[test]
    fn test_shrl_r() {
        assert_emit!(0xd3, 0xe8; shrl_r(RAX));
        assert_emit!(0x41, 0xd3, 0xe8; shrl_r(R8));
    }

    #[test]
    fn test_shrq_ri() {
        assert_emit!(0x48, 0xc1, 0xe8, 2; shrq_ri(RAX, Immediate(2)));
        assert_emit!(0x49, 0xc1, 0xe8, 2; shrq_ri(R8, Immediate(2)));
    }

    #[test]
    fn test_shrq_r() {
        assert_emit!(0x48, 0xd3, 0xe8; shrq_r(RAX));
        assert_emit!(0x49, 0xd3, 0xe8; shrq_r(R8));
    }

    #[test]
    fn test_sarl_ri() {
        assert_emit!(0xc1, 0xf8, 2; sarl_ri(RAX, Immediate(2)));
        assert_emit!(0x41, 0xc1, 0xf8, 2; sarl_ri(R8, Immediate(2)));
    }

    #[test]
    fn test_sarq_ri() {
        assert_emit!(0x48, 0xc1, 0xf8, 2; sarq_ri(RAX, Immediate(2)));
        assert_emit!(0x49, 0xc1, 0xf8, 2; sarq_ri(R8, Immediate(2)));
    }

    #[test]
    fn test_shll_ri() {
        assert_emit!(0xc1, 0xe0, 2; shll_ri(RAX, Immediate(2)));
        assert_emit!(0x41, 0xc1, 0xe0, 2; shll_ri(R8, Immediate(2)));
    }

    #[test]
    fn test_shlq_ri() {
        assert_emit!(0x48, 0xc1, 0xe0, 2; shlq_ri(RAX, Immediate(2)));
        assert_emit!(0x49, 0xc1, 0xe0, 2; shlq_ri(R8, Immediate(2)));
    }

    #[test]
    fn test_sarl_r() {
        assert_emit!(0xd3, 0xf8; sarl_r(RAX));
        assert_emit!(0x41, 0xd3, 0xf8; sarl_r(R8));
    }

    #[test]
    fn test_sarq_r() {
        assert_emit!(0x48, 0xd3, 0xf8; sarq_r(RAX));
        assert_emit!(0x49, 0xd3, 0xf8; sarq_r(R8));
    }

    #[test]
    fn test_shll_r() {
        assert_emit!(0xd3, 0xe0; shll_r(RAX));
        assert_emit!(0x41, 0xd3, 0xe0; shll_r(R8));
    }

    #[test]
    fn test_shlq_r() {
        assert_emit!(0x48, 0xd3, 0xe0; shlq_r(RAX));
        assert_emit!(0x49, 0xd3, 0xe0; shlq_r(R8));
    }

    #[test]
    fn test_roll_r() {
        assert_emit!(0xd3, 0xc0; roll_r(RAX));
        assert_emit!(0x41, 0xd3, 0xc0; roll_r(R8));
    }

    #[test]
    fn test_rolq_r() {
        assert_emit!(0x48, 0xd3, 0xc0; rolq_r(RAX));
        assert_emit!(0x49, 0xd3, 0xc0; rolq_r(R8));
    }

    #[test]
    fn test_rorl_r() {
        assert_emit!(0xd3, 0xc8; rorl_r(RAX));
        assert_emit!(0x41, 0xd3, 0xc8; rorl_r(R8));
    }

    #[test]
    fn test_rorq_r() {
        assert_emit!(0x48, 0xd3, 0xc8; rorq_r(RAX));
        assert_emit!(0x49, 0xd3, 0xc8; rorq_r(R8));
    }

    #[test]
    fn test_xorps_ra() {
        assert_emit!(0x0f, 0x57, 0x05, 0x04, 0x00, 0x00, 0x00; xorps_ra(XMM0, Address::rip(4)));
        assert_emit!(0x44, 0x0f, 0x57, 0x05, 0x08, 0x00, 0x00, 0x00; xorps_ra(XMM8, Address::rip(8)));
    }

    #[test]
    fn test_xorps_rr() {
        assert_emit!(0x0f, 0x57, 0xc1; xorps_rr(XMM0, XMM1));
        assert_emit!(0x41, 0x0f, 0x57, 0xf8; xorps_rr(XMM7, XMM8));
    }

    #[test]
    fn test_andps_ra() {
        assert_emit!(0x0f, 0x54, 0x05, 0x04, 0x00, 0x00, 0x00; andps_ra(XMM0, Address::rip(4)));
        assert_emit!(0x44, 0x0f, 0x54, 0x05, 0x08, 0x00, 0x00, 0x00; andps_ra(XMM8, Address::rip(8)));
    }

    #[test]
    fn test_movl_ai() {
        assert_emit!(0xc7, 0x00, 1, 0, 0, 0; movl_ai(Address::offset(RAX, 0), Immediate(1)));
        assert_emit!(0x41, 0xc7, 0x00, 0xff, 0xff, 0xff, 0xff; movl_ai(Address::offset(R8, 0), Immediate(u32::max_value() as i64)));
        assert_emit!(0xc7, 0x07, 0xff, 0xff, 0xff, 0x7f; movl_ai(Address::offset(RDI, 0), Immediate(i32::max_value() as i64)));
        assert_emit!(0x41, 0xc7, 0x07, 0, 0, 0, 0x80; movl_ai(Address::offset(R15, 0), Immediate(i32::min_value() as i64)));
    }

    #[test]
    fn test_movq_ai() {
        assert_emit!(0x48, 0xc7, 0x00, 1, 0, 0, 0; movq_ai(Address::offset(RAX, 0), Immediate(1)));
        assert_emit!(0x49, 0xc7, 0x00, 0xff, 0xff, 0xff, 0x7f; movq_ai(Address::offset(R8, 0), Immediate(i32::max_value() as i64)));
        assert_emit!(0x48, 0xc7, 0x07, 0xff, 0xff, 0xff, 0x7f; movq_ai(Address::offset(RDI, 0), Immediate(i32::max_value() as i64)));
        assert_emit!(0x49, 0xc7, 0x07, 0, 0, 0, 0x80; movq_ai(Address::offset(R15, 0), Immediate(i32::min_value() as i64)));
    }

    #[test]
    fn test_xchgq_ar() {
        assert_emit!(0x48, 0x87, 0x38; xchgq_ar(Address::offset(RAX, 0), RDI));
        assert_emit!(0x48, 0x87, 0x07; xchgq_ar(Address::offset(RDI, 0), RAX));
        assert_emit!(0x49, 0x87, 0x07; xchgq_ar(Address::offset(R15, 0), RAX));
        assert_emit!(0x4C, 0x87, 0x38; xchgq_ar(Address::offset(RAX, 0), R15));
    }

    #[test]
    fn test_xchgl_ar() {
        assert_emit!(0x87, 0x38; xchgl_ar(Address::offset(RAX, 0), RDI));
        assert_emit!(0x87, 0x07; xchgl_ar(Address::offset(RDI, 0), RAX));
        assert_emit!(0x41, 0x87, 0x07; xchgl_ar(Address::offset(R15, 0), RAX));
        assert_emit!(0x44, 0x87, 0x38; xchgl_ar(Address::offset(RAX, 0), R15));
    }

    #[test]
    fn test_xchgb_ar() {
        assert_emit!(0x40, 0x86, 0x38; xchgb_ar(Address::offset(RAX, 0), RDI));
        assert_emit!(0x86, 0x07; xchgb_ar(Address::offset(RDI, 0), RAX));
        assert_emit!(0x41, 0x86, 0x07; xchgb_ar(Address::offset(R15, 0), RAX));
        assert_emit!(0x44, 0x86, 0x38; xchgb_ar(Address::offset(RAX, 0), R15));
    }

    #[test]
    fn test_cmpxchgq_ar() {
        assert_emit!(0x48, 0x0F, 0xB1, 0x38; cmpxchgq_ar(Address::offset(RAX, 0), RDI));
        assert_emit!(0x48, 0x0F, 0xB1, 0x07; cmpxchgq_ar(Address::offset(RDI, 0), RAX));
        assert_emit!(0x49, 0x0F, 0xB1, 0x07; cmpxchgq_ar(Address::offset(R15, 0), RAX));
        assert_emit!(0x4C, 0x0F, 0xB1, 0x38; cmpxchgq_ar(Address::offset(RAX, 0), R15));
    }

    #[test]
    fn test_cmpxchgl_ar() {
        assert_emit!(0x0F, 0xB1, 0x38; cmpxchgl_ar(Address::offset(RAX, 0), RDI));
        assert_emit!(0x0F, 0xB1, 0x07; cmpxchgl_ar(Address::offset(RDI, 0), RAX));
        assert_emit!(0x41, 0x0F, 0xB1, 0x07; cmpxchgl_ar(Address::offset(R15, 0), RAX));
        assert_emit!(0x44, 0x0F, 0xB1, 0x38; cmpxchgl_ar(Address::offset(RAX, 0), R15));
    }

    #[test]
    fn test_lock_cmpxchgq_ar() {
        assert_emit!(0xF0, 0x48, 0x0F, 0xB1, 0x38; lock_cmpxchgq_ar(Address::offset(RAX, 0), RDI));
        assert_emit!(0xF0, 0x48, 0x0F, 0xB1, 0x07; lock_cmpxchgq_ar(Address::offset(RDI, 0), RAX));
        assert_emit!(0xF0, 0x49, 0x0F, 0xB1, 0x07; lock_cmpxchgq_ar(Address::offset(R15, 0), RAX));
        assert_emit!(0xF0, 0x4C, 0x0F, 0xB1, 0x38; lock_cmpxchgq_ar(Address::offset(RAX, 0), R15));
    }

    #[test]
    fn test_lock_cmpxchgl_ar() {
        assert_emit!(0xF0, 0x0F, 0xB1, 0x38; lock_cmpxchgl_ar(Address::offset(RAX, 0), RDI));
        assert_emit!(0xF0, 0x0F, 0xB1, 0x07; lock_cmpxchgl_ar(Address::offset(RDI, 0), RAX));
        assert_emit!(0xF0, 0x41, 0x0F, 0xB1, 0x07; lock_cmpxchgl_ar(Address::offset(R15, 0), RAX));
        assert_emit!(0xF0, 0x44, 0x0F, 0xB1, 0x38; lock_cmpxchgl_ar(Address::offset(RAX, 0), R15));
    }

    #[test]
    fn test_xaddq_ar() {
        assert_emit!(0x48, 0x0F, 0xC1, 0x38; xaddq_ar(Address::offset(RAX, 0), RDI));
        assert_emit!(0x48, 0x0F, 0xC1, 0x07; xaddq_ar(Address::offset(RDI, 0), RAX));
        assert_emit!(0x49, 0x0F, 0xC1, 0x07; xaddq_ar(Address::offset(R15, 0), RAX));
        assert_emit!(0x4C, 0x0F, 0xC1, 0x38; xaddq_ar(Address::offset(RAX, 0), R15));
    }

    #[test]
    fn test_xaddl_ar() {
        assert_emit!(0x0F, 0xC1, 0x38; xaddl_ar(Address::offset(RAX, 0), RDI));
        assert_emit!(0x0F, 0xC1, 0x07; xaddl_ar(Address::offset(RDI, 0), RAX));
        assert_emit!(0x41, 0x0F, 0xC1, 0x07; xaddl_ar(Address::offset(R15, 0), RAX));
        assert_emit!(0x44, 0x0F, 0xC1, 0x38; xaddl_ar(Address::offset(RAX, 0), R15));
    }

    #[test]
    fn test_lock_xaddq_ar() {
        assert_emit!(0xF0, 0x48, 0x0F, 0xC1, 0x38; lock_xaddq_ar(Address::offset(RAX, 0), RDI));
        assert_emit!(0xF0, 0x48, 0x0F, 0xC1, 0x07; lock_xaddq_ar(Address::offset(RDI, 0), RAX));
        assert_emit!(0xF0, 0x49, 0x0F, 0xC1, 0x07; lock_xaddq_ar(Address::offset(R15, 0), RAX));
        assert_emit!(0xF0, 0x4C, 0x0F, 0xC1, 0x38; lock_xaddq_ar(Address::offset(RAX, 0), R15));
    }

    #[test]
    fn test_lock_xaddl_ar() {
        assert_emit!(0xF0, 0x0F, 0xC1, 0x38; lock_xaddl_ar(Address::offset(RAX, 0), RDI));
        assert_emit!(0xF0, 0x0F, 0xC1, 0x07; lock_xaddl_ar(Address::offset(RDI, 0), RAX));
        assert_emit!(0xF0, 0x41, 0x0F, 0xC1, 0x07; lock_xaddl_ar(Address::offset(R15, 0), RAX));
        assert_emit!(0xF0, 0x44, 0x0F, 0xC1, 0x38; lock_xaddl_ar(Address::offset(RAX, 0), R15));
    }

    #[test]
    fn test_addl_ri() {
        assert_emit!(0x83, 0xc0, 0x01; addl_ri(RAX, Immediate(1))); // add eax, 1
        assert_emit!(0x83, 0xc1, 0x01; addl_ri(RCX, Immediate(1))); // add ecx, 1
        assert_emit!(0x05, 0x80, 0x00, 0x00, 0x00; addl_ri(RAX, Immediate(128))); // add eax, 128
        assert_emit!(0x81, 0xc1, 0x80, 0x00, 0x00, 0x00; addl_ri(RCX, Immediate(128)));
        // add ecx, 128
    }

    #[test]
    fn test_mfence() {
        assert_emit!(0x0f, 0xae, 0xf0; mfence);
    }

    #[test]
    fn test_movaps_ar() {
        assert_emit!(0x0f, 0x29, 0x45, 16; movaps_ar(Address::offset(RBP, 16), XMM0));
        assert_emit!(0x44, 0x0f, 0x29, 0x45, 16; movaps_ar(Address::offset(RBP, 16), XMM8));
        assert_emit!(0x41, 0x0f, 0x29, 0x7d, 16; movaps_ar(Address::offset(R13, 16), XMM7));
    }

    #[test]
    fn test_movups_ar() {
        assert_emit!(0x0f, 0x11, 0x45, 16; movups_ar(Address::offset(RBP, 16), XMM0));
        assert_emit!(0x44, 0x0f, 0x11, 0x45, 16; movups_ar(Address::offset(RBP, 16), XMM8));
        assert_emit!(0x41, 0x0f, 0x11, 0x7d, 16; movups_ar(Address::offset(R13, 16), XMM7));
    }

    #[test]
    fn test_vmovss_ra() {
        assert_emit!(0xc5, 0xfa, 0x10, 0x00; vmovss_ra(XMM0, Address::reg(RAX)); avx2);
        assert_emit!(0xc5, 0xfa, 0x10, 0x38; vmovss_ra(XMM7, Address::reg(RAX)); avx2);
        assert_emit!(0xc5, 0x7a, 0x10, 0x00; vmovss_ra(XMM8, Address::reg(RAX)); avx2);
        assert_emit!(0xc5, 0x7a, 0x10, 0x38; vmovss_ra(XMM15, Address::reg(RAX)); avx2);

        assert_emit!(0xc5, 0xfa, 0x10, 0x07; vmovss_ra(XMM0, Address::reg(RDI)); avx2);
        assert_emit!(0xc4, 0xc1, 0x7a, 0x10, 0x00; vmovss_ra(XMM0, Address::reg(R8)); avx2);
        assert_emit!(0xc4, 0xc1, 0x7a, 0x10, 0x07; vmovss_ra(XMM0, Address::reg(R15)); avx2);

        assert_emit!(0xc5, 0xfa, 0x10, 0x04, 0xb8; vmovss_ra(XMM0, Address::array(RAX, RDI, ScaleFactor::Four, 0)); avx2);
        assert_emit!(0xc4, 0xa1, 0x7a, 0x10, 0x04, 0xb8; vmovss_ra(XMM0, Address::array(RAX, R15, ScaleFactor::Four, 0)); avx2);
    }

    #[test]
    fn test_vmovsd_ra() {
        assert_emit!(0xc5, 0xfb, 0x10, 0x00; vmovsd_ra(XMM0, Address::reg(RAX)); avx2);
    }

    #[test]
    fn test_vaddss_rr() {
        assert_emit!(0xc5, 0xf2, 0x58, 0xc2; vaddss_rr(XMM0, XMM1, XMM2); avx2);
        assert_emit!(0xc5, 0x72, 0x58, 0xc2; vaddss_rr(XMM8, XMM1, XMM2); avx2);
        assert_emit!(0xc5, 0xb2, 0x58, 0xc2; vaddss_rr(XMM0, XMM9, XMM2); avx2);
        assert_emit!(0xc4, 0xc1, 0x72, 0x58, 0xc2; vaddss_rr(XMM0, XMM1, XMM10); avx2);
    }

    #[test]
    fn test_vaddsd_rr() {
        assert_emit!(0xc5, 0xf3, 0x58, 0xc2; vaddsd_rr(XMM0, XMM1, XMM2); avx2);
        assert_emit!(0xc5, 0x73, 0x58, 0xc2; vaddsd_rr(XMM8, XMM1, XMM2); avx2);
        assert_emit!(0xc5, 0xb3, 0x58, 0xc2; vaddsd_rr(XMM0, XMM9, XMM2); avx2);
        assert_emit!(0xc4, 0xc1, 0x73, 0x58, 0xc2; vaddsd_rr(XMM0, XMM1, XMM10); avx2);
    }

    #[test]
    fn test_vsubss_rr() {
        assert_emit!(0xc5, 0xf2, 0x5c, 0xc2; vsubss_rr(XMM0, XMM1, XMM2); avx2);
    }

    #[test]
    fn test_vsubsd_rr() {
        assert_emit!(0xc5, 0xf3, 0x5c, 0xc2; vsubsd_rr(XMM0, XMM1, XMM2); avx2);
    }

    #[test]
    fn test_vmulss_rr() {
        assert_emit!(0xc5, 0xf2, 0x59, 0xc2; vmulss_rr(XMM0, XMM1, XMM2); avx2);
    }

    #[test]
    fn test_vmulsd_rr() {
        assert_emit!(0xc5, 0xf3, 0x59, 0xc2; vmulsd_rr(XMM0, XMM1, XMM2); avx2);
    }

    #[test]
    fn test_vdivss_rr() {
        assert_emit!(0xc5, 0xf2, 0x5e, 0xc2; vdivss_rr(XMM0, XMM1, XMM2); avx2);
    }

    #[test]
    fn test_vdivsd_rr() {
        assert_emit!(0xc5, 0xf3, 0x5e, 0xc2; vdivsd_rr(XMM0, XMM1, XMM2); avx2);
    }

    #[test]
    fn test_vsqrtss_rr() {
        assert_emit!(0xc5, 0xf2, 0x51, 0xc2; vsqrtss_rr(XMM0, XMM1, XMM2); avx2);
    }

    #[test]
    fn test_vsqrtsd_rr() {
        assert_emit!(0xc5, 0xf3, 0x51, 0xc2; vsqrtsd_rr(XMM0, XMM1, XMM2); avx2);
    }

    #[test]
    fn test_vandps_ra() {
        assert_emit!(0xc5, 0xf0, 0x54, 0x05, 0x04, 0x00, 0x00, 0x00; vandps_ra(XMM0, XMM1, Address::rip(4)); avx2);
        assert_emit!(0xc5, 0x30, 0x54, 0x05, 0x08, 0x00, 0x00, 0x00; vandps_ra(XMM8, XMM9, Address::rip(8)); avx2);
    }

    #[test]
    fn test_vandpd_ra() {
        assert_emit!(0xc5, 0xf1, 0x54, 0x05, 0x04, 0x00, 0x00, 0x00; vandpd_ra(XMM0, XMM1, Address::rip(4)); avx2);
        assert_emit!(0xc5, 0x31, 0x54, 0x05, 0x08, 0x00, 0x00, 0x00; vandpd_ra(XMM8, XMM9, Address::rip(8)); avx2);
    }

    #[test]
    fn test_vxorpd_ra() {
        assert_emit!(0xc5, 0xf0, 0x57, 0x05, 0x04, 0x00, 0x00, 0x00; vxorps_ra(XMM0, XMM1, Address::rip(4)); avx2);
        assert_emit!(0xc5, 0x30, 0x57, 0x05, 0x08, 0x00, 0x00, 0x00; vxorps_ra(XMM8, XMM9, Address::rip(8)); avx2);
    }

    #[test]
    fn test_vxorps_ra() {
        assert_emit!(0xc5, 0xf0, 0x57, 0x05, 0x04, 0x00, 0x00, 0x00; vxorps_ra(XMM0, XMM1, Address::rip(4)); avx2);
        assert_emit!(0xc5, 0x30, 0x57, 0x05, 0x08, 0x00, 0x00, 0x00; vxorps_ra(XMM8, XMM9, Address::rip(8)); avx2);
    }

    #[test]
    fn test_vroundss_ri() {
        assert_emit!(0xc4, 0xe3, 0x71, 0x0a, 0xc2, 1; vroundss_ri(XMM0, XMM1, XMM2, 1); avx2);
        assert_emit!(0xc4, 0x43, 0x31, 0x0a, 0xc2, 1; vroundss_ri(XMM8, XMM9, XMM10, 1); avx2);
    }

    #[test]
    fn test_vroundsd_ri() {
        assert_emit!(0xc4, 0xe3, 0x71, 0x0b, 0xc2, 1; vroundsd_ri(XMM0, XMM1, XMM2, 1); avx2);
        assert_emit!(0xc4, 0x43, 0x31, 0x0b, 0xc2, 1; vroundsd_ri(XMM8, XMM9, XMM10, 1); avx2);
    }

    #[test]
    fn test_vucomiss_rr() {
        assert_emit!(0xc5, 0xf8, 0x2e, 0xc8; vucomiss_rr(XMM1, XMM0); avx2);
        assert_emit!(0xc5, 0x78, 0x2e, 0xfb; vucomiss_rr(XMM15, XMM3); avx2);
        assert_emit!(0xc4, 0xc1, 0x78, 0x2e, 0xe0; vucomiss_rr(XMM4, XMM8); avx2);
    }

    #[test]
    fn test_vucomisd_rr() {
        assert_emit!(0xc5, 0xf9, 0x2e, 0xc8; vucomisd_rr(XMM1, XMM0); avx2);
        assert_emit!(0xc5, 0x79, 0x2e, 0xfb; vucomisd_rr(XMM15, XMM3); avx2);
        assert_emit!(0xc4, 0xc1, 0x79, 0x2e, 0xe0; vucomisd_rr(XMM4, XMM8); avx2);
    }

    #[test]
    fn test_vmovd_xr() {
        assert_emit!(0xc5, 0xf9, 0x6e, 0xc1; vmovd_xr(XMM0, RCX); avx2);
        assert_emit!(0xc5, 0x79, 0x6e, 0xc1; vmovd_xr(XMM8, RCX); avx2);
        assert_emit!(0xc4, 0xc1, 0x79, 0x6e, 0xc1; vmovd_xr(XMM0, R9); avx2);
    }

    #[test]
    fn test_vmovd_rx() {
        assert_emit!(0xc5, 0xf9, 0x7e, 0xc8; vmovd_rx(RAX, XMM1); avx2);
        assert_emit!(0xc4, 0xc1, 0x79, 0x7e, 0xc8; vmovd_rx(R8, XMM1); avx2);
        assert_emit!(0xc5, 0x79, 0x7e, 0xc8; vmovd_rx(RAX, XMM9); avx2);
    }

    #[test]
    fn test_vmovq_xr() {
        assert_emit!(0xc4, 0xe1, 0xf9, 0x6e, 0xc1; vmovq_xr(XMM0, RCX); avx2);
        assert_emit!(0xc4, 0x61, 0xf9, 0x6e, 0xc1; vmovq_xr(XMM8, RCX); avx2);
        assert_emit!(0xc4, 0xc1, 0xf9, 0x6e, 0xc1; vmovq_xr(XMM0, R9); avx2);
    }

    #[test]
    fn test_vmovq_rx() {
        assert_emit!(0xc4, 0xe1, 0xf9, 0x7e, 0xc8; vmovq_rx(RAX, XMM1); avx2);
        assert_emit!(0xc4, 0xc1, 0xf9, 0x7e, 0xc8; vmovq_rx(R8, XMM1); avx2);
        assert_emit!(0xc4, 0x61, 0xf9, 0x7e, 0xc8; vmovq_rx(RAX, XMM9); avx2);
    }

    #[test]
    fn test_vcvtsi2ssq_rr() {
        assert_emit!(0xc4, 0xe1, 0xf2, 0x2a, 0xc2; vcvtsi2ssq_rr(XMM0, XMM1, RDX); avx2);
        assert_emit!(0xc4, 0x61, 0xf2, 0x2a, 0xc2; vcvtsi2ssq_rr(XMM8, XMM1, RDX); avx2);
        assert_emit!(0xc4, 0xe1, 0xb2, 0x2a, 0xc2; vcvtsi2ssq_rr(XMM0, XMM9, RDX); avx2);
        assert_emit!(0xc4, 0xc1, 0xf2, 0x2a, 0xc2; vcvtsi2ssq_rr(XMM0, XMM1, R10); avx2);
    }

    #[test]
    fn test_vcvtsi2ssd_rr() {
        assert_emit!(0xc5, 0xf2, 0x2a, 0xc2; vcvtsi2ssd_rr(XMM0, XMM1, RDX); avx2);
        assert_emit!(0xc5, 0x72, 0x2a, 0xc2; vcvtsi2ssd_rr(XMM8, XMM1, RDX); avx2);
        assert_emit!(0xc5, 0xb2, 0x2a, 0xc2; vcvtsi2ssd_rr(XMM0, XMM9, RDX); avx2);
        assert_emit!(0xc4, 0xc1, 0x72, 0x2a, 0xc2; vcvtsi2ssd_rr(XMM0, XMM1, R10); avx2);
    }

    #[test]
    fn test_vcvtsi2sdq_rr() {
        assert_emit!(0xc4, 0xe1, 0xf3, 0x2a, 0xc2; vcvtsi2sdq_rr(XMM0, XMM1, RDX); avx2);
        assert_emit!(0xc4, 0x61, 0xf3, 0x2a, 0xc2; vcvtsi2sdq_rr(XMM8, XMM1, RDX); avx2);
        assert_emit!(0xc4, 0xe1, 0xb3, 0x2a, 0xc2; vcvtsi2sdq_rr(XMM0, XMM9, RDX); avx2);
        assert_emit!(0xc4, 0xc1, 0xf3, 0x2a, 0xc2; vcvtsi2sdq_rr(XMM0, XMM1, R10); avx2);
    }

    #[test]
    fn test_vcvtsi2sdd_rr() {
        assert_emit!(0xc5, 0xf3, 0x2a, 0xc2; vcvtsi2sdd_rr(XMM0, XMM1, RDX); avx2);
        assert_emit!(0xc5, 0x73, 0x2a, 0xc2; vcvtsi2sdd_rr(XMM8, XMM1, RDX); avx2);
        assert_emit!(0xc5, 0xb3, 0x2a, 0xc2; vcvtsi2sdd_rr(XMM0, XMM9, RDX); avx2);
        assert_emit!(0xc4, 0xc1, 0x73, 0x2a, 0xc2; vcvtsi2sdd_rr(XMM0, XMM1, R10); avx2);
    }

    #[test]
    fn test_vcvtsd2ss_rr() {
        assert_emit!(0xc5, 0xf3, 0x5a, 0xc2; vcvtsd2ss_rr(XMM0, XMM1, XMM2); avx2);
    }

    #[test]
    fn test_vcvtss2sd_rr() {
        assert_emit!(0xc5, 0xf2, 0x5a, 0xc2; vcvtss2sd_rr(XMM0, XMM1, XMM2); avx2);
    }

    #[test]
    fn test_vxorps_rr() {
        assert_emit!(0xc5, 0xf0, 0x57, 0xc2; vxorps_rr(XMM0, XMM1, XMM2); avx2);
    }

    #[test]
    fn test_vcvttsd2sid_rr() {
        assert_emit!(0xc5, 0xfb, 0x2c, 0xc1; vcvttsd2sid_rr(RAX, XMM1); avx2);
    }

    #[test]
    fn test_vcvttsd2siq_rr() {
        assert_emit!(0xc4, 0xe1, 0xfb, 0x2c, 0xc1; vcvttsd2siq_rr(RAX, XMM1); avx2);
    }

    #[test]
    fn test_vcvttss2sid_rr() {
        assert_emit!(0xc5, 0xfa, 0x2c, 0xc1; vcvttss2sid_rr(RAX, XMM1); avx2);
    }

    #[test]
    fn test_vcvttss2siq_rr() {
        assert_emit!(0xc4, 0xe1, 0xfa, 0x2c, 0xc1; vcvttss2siq_rr(RAX, XMM1); avx2);
    }

    #[test]
    fn test_vmovss_rr() {
        assert_emit!(0xc5, 0xf2, 0x10, 0xc2; vmovss_rr(XMM0, XMM1, XMM2); avx2);
    }

    #[test]
    fn test_vmovss_ar() {
        assert_emit!(0xc5, 0xfa, 0x11, 0x08; vmovss_ar(Address::offset(RAX, 0), XMM1); avx2);
    }

    #[test]
    fn test_vmovsd_rr() {
        assert_emit!(0xc5, 0xf3, 0x10, 0xc2; vmovsd_rr(XMM0, XMM1, XMM2); avx2);
    }

    #[test]
    fn test_vmovsd_ar() {
        assert_emit!(0xc5, 0xfb, 0x11, 0x08; vmovsd_ar(Address::offset(RAX, 0), XMM1); avx2);
    }

    #[test]
    fn test_vmovaps_rr() {
        assert_emit!(0xc5, 0xf8, 0x28, 0xc1; vmovaps_rr(XMM0, XMM1); avx2);
    }

    #[test]
    fn test_vmovapd_rr() {
        assert_emit!(0xc5, 0xf9, 0x28, 0xc1; vmovapd_rr(XMM0, XMM1); avx2);
    }

    #[test]
    fn test_movq_rl() {
        let mut buf = AssemblerX64::new(false);
        let lbl = buf.create_label();
        buf.movq_rl(RAX, lbl);
        buf.bind_label(lbl);

        assert_asm_bytes(vec![0x48, 0x8b, 0x05, 0, 0, 0, 0], buf.finalize(1).code());
    }
}
