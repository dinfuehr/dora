use byteorder::{LittleEndian, WriteBytesExt};

#[cfg(target_arch = "x86_64")]
pub use self::x64::*;

#[cfg(target_arch = "x86_64")]
pub mod x64;

#[cfg(target_arch = "aarch64")]
use self::arm64::*;

#[cfg(target_arch = "aarch64")]
pub mod arm64;

use std::convert::TryInto;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Label(usize);

pub struct Assembler {
    code: Vec<u8>,
    labels: Vec<Option<u32>>,
    unresolved_jumps: Vec<(u32, Label, JumpKind)>,
    position: usize,
}

impl Assembler {
    pub fn new() -> Assembler {
        Assembler {
            code: Vec::new(),
            labels: Vec::new(),
            unresolved_jumps: Vec::new(),
            position: 0,
        }
    }

    pub fn create_label(&mut self) -> Label {
        self.labels.push(None);

        Label(self.labels.len() - 1)
    }

    pub fn create_and_bind_label(&mut self) -> Label {
        self.labels.push(Some(self.position().try_into().unwrap()));
        Label(self.labels.len() - 1)
    }

    pub fn bind_label(&mut self, lbl: Label) {
        let Label(idx) = lbl;
        assert!(self.labels[idx].is_none());
        self.labels[idx] = Some(self.position().try_into().unwrap());
    }

    pub fn bind_label_to(&mut self, lbl: Label, offset: u32) {
        let Label(idx) = lbl;
        assert!(self.labels[idx].is_none());
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

    pub fn position(&self) -> usize {
        self.position
    }

    pub fn set_position(&mut self, pos: usize) {
        self.position = pos;
    }

    pub fn set_position_end(&mut self) {
        self.position = self.code.len();
    }

    pub fn emit_u8(&mut self, value: u8) {
        if self.position == self.code.len() {
            self.code.push(value);
        } else {
            self.code[self.position] = value;
        }
        self.position += 1;
    }

    pub fn emit_u32(&mut self, value: u32) {
        if self.position == self.code.len() {
            self.code.write_u32::<LittleEndian>(value).unwrap()
        } else {
            (&mut self.code[self.position..])
                .write_u32::<LittleEndian>(value)
                .unwrap();
        }
        self.position += 4;
    }

    pub fn emit_u64(&mut self, value: u64) {
        if self.position == self.code.len() {
            self.code.write_u64::<LittleEndian>(value).unwrap()
        } else {
            (&mut self.code[self.position..])
                .write_u64::<LittleEndian>(value)
                .unwrap();
        }
        self.position += 8;
    }
}
