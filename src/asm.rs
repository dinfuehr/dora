use byteorder::{LittleEndian, WriteBytesExt};

pub struct Assembler {
    code: Vec<u8>
}

impl Assembler {
    pub fn new() -> Assembler {
        Assembler { code: Vec::new() }
    }

    pub fn movq(&mut self, dest: Reg64, src: u64) {
        self.code.push(0x48);
        self.code.push(0xB8);
        self.code.write_u64::<LittleEndian>(src).unwrap();
    }

    pub fn nop(&mut self) {
        self.code.push(0x90);
    }

    pub fn ret(&mut self) {
        self.code.push(0xC3);
    }
}

enum Reg64 {
    rax
}
