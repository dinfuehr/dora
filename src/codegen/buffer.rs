use byteorder::{LittleEndian, WriteBytesExt};

pub struct Buffer {
    data: Vec<u8>
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer {
            data: Vec::new()
        }
    }

    pub fn emit_u8(&mut self, value: u8) {
        self.data.write_u8(value).unwrap();
    }

    pub fn emit_u16(&mut self, value: u16) {
        self.data.write_u16::<LittleEndian>(value).unwrap();
    }

    pub fn emit_u32(&mut self, value: u32) {
        self.data.write_u32::<LittleEndian>(value).unwrap();
    }

    pub fn emit_u64(&mut self, value: u64) {
        self.data.write_u64::<LittleEndian>(value).unwrap();
    }
}
