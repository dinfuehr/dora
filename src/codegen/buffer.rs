use byteorder::{LittleEndian, WriteBytesExt};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Label(usize);

#[derive(Debug)]
struct ByteBuffer {
    data: Vec<u8>,
}

impl ByteBuffer {
    fn new() -> ByteBuffer {
        ByteBuffer {
            data: Vec::new()
        }
    }

    pub fn data(self) -> Vec<u8> {
        self.data
    }

    pub fn pos(&mut self) -> usize {
        self.data.len()
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

    pub fn emit_u32_at(&mut self, value: u32, pos: usize) {
        let mut slice = &mut self.data[pos..];
        slice.write_u32::<LittleEndian>(value).unwrap();
    }

    pub fn emit_u64(&mut self, value: u64) {
        self.data.write_u64::<LittleEndian>(value).unwrap();
    }
}

#[derive(Debug)]
pub struct Buffer {
    buf: ByteBuffer,
    labels: Vec<Option<usize>>,
    jumps: Vec<ForwardJump>,
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer {
            buf: ByteBuffer::new(),
            labels: Vec::new(),
            jumps: Vec::new(),
        }
    }

    pub fn pos(&mut self) -> usize {
        self.buf.pos()
    }

    pub fn finish(mut self) -> Vec<u8> {
        self.fix_forward_jumps();

        self.buf.data()
    }

    fn fix_forward_jumps(&mut self) {
        for jmp in &self.jumps {
            let target = self.labels[jmp.to.0].expect("label not defined");
            let diff = (target - jmp.at - 4) as i32;

            self.buf.emit_u32_at(diff as u32, jmp.at);
        }
    }

    pub fn create_label(&mut self) -> Label {
        let idx = self.labels.len();
        self.labels.push(None);

        Label(idx)
    }

    pub fn define_label(&mut self, lbl: Label) {
        assert!(self.labels[lbl.0].is_none());
        self.labels[lbl.0] = Some(self.buf.pos());
    }

    pub fn emit_label(&mut self, lbl: Label) {
        let value = self.labels[lbl.0];

        match value {
            // backward jumps already know their target
            Some(idx) => {
                let current = self.buf.pos() + 4;
                let target = idx;

                let diff = -((current - target) as i32);
                self.emit_u32(diff as u32);
            }

            // forward jumps do not know their target yet
            // we need to do this later...
            None => {
                let pos = self.buf.pos();
                self.emit_u32(0);
                self.jumps.push(ForwardJump {
                    at: pos,
                    to: lbl
                });
            }
        }
    }

    pub fn emit_u8(&mut self, value: u8) {
        self.buf.emit_u8(value);
    }

    pub fn emit_u32(&mut self, value: u32) {
        self.buf.emit_u32(value);
    }
}

#[derive(Debug)]
struct ForwardJump {
    at: usize,
    to: Label
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_label() {
        let mut buf = Buffer::new();

        assert_eq!(Label(0), buf.create_label());
        assert_eq!(Label(1), buf.create_label());
    }

    #[test]
    fn test_backward_with_gap() {
        let mut buf = Buffer::new();
        let lbl = buf.create_label();
        buf.define_label(lbl);
        buf.emit_u8(0x33);
        buf.emit_label(lbl);

        assert_eq!(vec![0x33, 0xfb, 0xff, 0xff, 0xff], buf.finish());
    }

    #[test]
    fn test_backward() {
        let mut buf = Buffer::new();
        let lbl = buf.create_label();
        buf.define_label(lbl);
        buf.emit_label(lbl);

        assert_eq!(vec![0xfc, 0xff, 0xff, 0xff], buf.finish());
    }

    #[test]
    fn test_forward_with_gap() {
        let mut buf = Buffer::new();
        let lbl = buf.create_label();
        buf.emit_label(lbl);
        buf.emit_u8(0x11);
        buf.define_label(lbl);

        assert_eq!(vec![1, 0, 0, 0, 0x11], buf.finish());
    }

    #[test]
    fn test_forward() {
        let mut buf = Buffer::new();
        let lbl = buf.create_label();
        buf.emit_label(lbl);
        buf.define_label(lbl);

        assert_eq!(vec![0, 0, 0, 0], buf.finish());
    }

    #[test]
    #[should_panic]
    fn test_define_label_twice() {
        let mut buf = Buffer::new();
        let lbl = buf.create_label();

        buf.define_label(lbl);
        buf.define_label(lbl);
    }

    #[test]
    #[should_panic]
    fn test_label_undefined() {
        let mut buf = Buffer::new();
        let lbl = buf.create_label();

        buf.emit_label(lbl);
        buf.finish();
    }
}
