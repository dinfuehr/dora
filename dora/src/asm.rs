#[cfg(target_arch = "x86_64")]
pub use self::x64::*;

#[cfg(target_arch = "x86_64")]
pub mod x64;

pub struct Assembler {
    data: Vec<u8>,
}

impl Assembler {
    pub fn new() -> Assembler {
        Assembler { data: Vec::new() }
    }

    pub fn pc(&self) -> usize {
        self.data.len()
    }

    pub fn data(&mut self) -> &mut Vec<u8> {
        &mut self.data
    }

    pub fn buffer(&self) -> &[u8] {
        &self.data
    }

    pub fn finalize(self) -> Vec<u8> {
        self.data
    }

    fn emit_u8(&mut self, value: u8) {
        self.data.push(value);
    }
}
