#[cfg(target_arch = "x86_64")]
pub use self::x64::*;

#[cfg(target_arch = "x86_64")]
pub mod x64;

use std::convert::TryInto;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Register(u8);

impl Register {
    pub fn new(value: u8) -> Register {
        Register(value)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct FloatRegister(u8);

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Label(usize);

enum JumpDistance {
    Near,
    Far,
}

pub struct Assembler {
    code: Vec<u8>,
    labels: Vec<Option<u32>>,
    unresolved_jumps: Vec<(u32, Label, JumpDistance)>,
}

impl Assembler {
    pub fn new() -> Assembler {
        Assembler {
            code: Vec::new(),
            labels: Vec::new(),
            unresolved_jumps: Vec::new(),
        }
    }

    pub fn pc(&self) -> usize {
        self.code.len()
    }

    pub fn code_mut(&mut self) -> &mut Vec<u8> {
        &mut self.code
    }

    pub fn code(&self) -> &[u8] {
        &self.code
    }

    pub fn create_label(&mut self) -> Label {
        self.labels.push(None);

        Label(self.labels.len() - 1)
    }

    pub fn create_and_bind_label(&mut self) -> Label {
        self.labels.push(Some(self.pc().try_into().unwrap()));
        Label(self.labels.len() - 1)
    }

    pub fn bind_label(&mut self, lbl: Label) {
        let Label(idx) = lbl;
        debug_assert!(self.labels[idx].is_none());
        self.labels[idx] = Some(self.pc().try_into().unwrap());
    }

    pub fn bind_label_to(&mut self, lbl: Label, offset: u32) {
        let Label(idx) = lbl;
        debug_assert!(self.labels[idx].is_none());
        self.labels[idx] = Some(offset);
    }

    fn offset(&self, lbl: Label) -> Option<u32> {
        let Label(idx) = lbl;
        self.labels[idx]
    }

    pub fn finalize(mut self) -> Vec<u8> {
        self.resolve_jumps();
        self.code
    }

    fn emit_u8(&mut self, value: u8) {
        self.code.push(value);
    }

    fn emit_u32(&mut self, value: u32) {
        self.emit_u8(value as u8);
        self.emit_u8((value >> 8) as u8);
        self.emit_u8((value >> 16) as u8);
        self.emit_u8((value >> 24) as u8);
    }

    fn emit_u64(&mut self, value: u64) {
        self.emit_u32(value as u32);
        self.emit_u32((value >> 32) as u32);
    }

    fn patch_u8(&mut self, idx: u32, value: u8) {
        self.code[idx as usize] = value;
    }

    fn patch_u32(&mut self, idx: u32, value: u32) {
        self.code[idx as usize] = (value & 0xFF) as u8;
        self.code[idx as usize + 1] = ((value >> 8) & 0xFF) as u8;
        self.code[idx as usize + 2] = ((value >> 16) & 0xFF) as u8;
        self.code[idx as usize + 3] = ((value >> 24) & 0xFF) as u8;
    }
}
