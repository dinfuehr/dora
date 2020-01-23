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
}
