pub struct Buffer {
    data: Vec<u8>
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer {
            data: Vec::new()
        }
    }
}
