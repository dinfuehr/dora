use byteorder::{LittleEndian, WriteBytesExt};

pub mod arm64;
pub mod x64;

use std::convert::TryInto;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Label(usize);

struct AssemblerBuffer {
    code: Vec<u8>,
    position: usize,
    labels: Vec<Option<u32>>,
}

impl AssemblerBuffer {
    fn new() -> AssemblerBuffer {
        AssemblerBuffer {
            code: Vec::new(),
            position: 0,
            labels: Vec::new(),
        }
    }

    fn create_label(&mut self) -> Label {
        self.labels.push(None);

        Label(self.labels.len() - 1)
    }

    fn create_and_bind_label(&mut self) -> Label {
        self.labels.push(Some(self.position().try_into().unwrap()));
        Label(self.labels.len() - 1)
    }

    fn bind_label(&mut self, lbl: Label) {
        let Label(idx) = lbl;
        assert!(self.labels[idx].is_none());
        self.labels[idx] = Some(self.position().try_into().unwrap());
    }

    fn bind_label_to(&mut self, lbl: Label, offset: u32) {
        let Label(idx) = lbl;
        assert!(self.labels[idx].is_none());
        self.labels[idx] = Some(offset);
    }

    fn offset(&self, lbl: Label) -> Option<u32> {
        let Label(idx) = lbl;
        self.labels[idx]
    }

    fn position(&self) -> usize {
        self.position
    }

    fn set_position(&mut self, pos: usize) {
        self.position = pos;
    }

    fn set_position_end(&mut self) {
        self.position = self.code.len();
    }

    fn emit_u8(&mut self, value: u8) {
        if self.position == self.code.len() {
            self.code.push(value);
        } else {
            self.code[self.position] = value;
        }
        self.position += 1;
    }

    fn emit_u32(&mut self, value: u32) {
        if self.position == self.code.len() {
            self.code.write_u32::<LittleEndian>(value).unwrap()
        } else {
            (&mut self.code[self.position..])
                .write_u32::<LittleEndian>(value)
                .unwrap();
        }
        self.position += 4;
    }

    fn emit_u64(&mut self, value: u64) {
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
