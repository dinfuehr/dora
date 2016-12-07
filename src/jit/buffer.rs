use byteorder::{LittleEndian, WriteBytesExt};
use ctxt::FctId;
use cpu::trap::{self, TrapId};
use dseg::DSeg;
use jit::fct::{CatchType, Comments, Comment, ExHandler, JitFct, LineNumberTable, GcPoints, GcPoint};
use lexer::position::Position;

pub struct Buffer {
    data: Vec<u8>,
    labels: Vec<Option<usize>>,
    jumps: Vec<ForwardJump>,
    bailouts: Vec<(Label, TrapId, Position)>,
    dseg: DSeg,
    gcpoints: GcPoints,
    comments: Comments,
    linenos: LineNumberTable,
    exception_handlers: Vec<ExHandler>,
}

impl Buffer {
    pub fn new() -> Buffer {
        Buffer {
            data: Vec::new(),
            labels: Vec::new(),
            jumps: Vec::new(),
            bailouts: Vec::new(),
            dseg: DSeg::new(),
            gcpoints: GcPoints::new(),
            comments: Comments::new(),
            linenos: LineNumberTable::new(),
            exception_handlers: Vec::new(),
        }
    }

    pub fn jit(mut self, id: FctId, stacksize: i32) -> JitFct {
        self.finish();

        JitFct::from_buffer(id, &self.dseg, &self.data, self.gcpoints, stacksize,
                            self.comments, self.linenos, self.exception_handlers)
    }

    pub fn data(mut self) -> Vec<u8> {
        self.finish();

        self.data
    }

    fn finish(&mut self) {
        let bailouts = self.bailouts.drain(0..).collect::<Vec<_>>();

        for bailout in &bailouts {
            let (lbl, trap, pos) = *bailout;

            self.bind_label(lbl);
            self.emit_lineno(pos.line as i32);
            trap::emit(self, trap);
        }

        self.fix_forward_jumps();
    }

    pub fn add_addr(&mut self, ptr: *const u8) -> i32 {
        self.dseg.add_addr(ptr)
    }

    pub fn pos(&self) -> usize {
        self.data.len()
    }

    pub fn emit_lineno(&mut self, lineno: i32) {
        let pos = self.pos() as i32;
        self.linenos.insert(pos, lineno);
    }

    pub fn emit_gcpoint(&mut self, gcpoint: GcPoint) {
        let pos = self.pos() as i32;
        self.gcpoints.insert(pos, gcpoint);
    }

    fn fix_forward_jumps(&mut self) {
        for jmp in &self.jumps {
            let target = self.labels[jmp.to.0].expect("label not defined");
            let diff = (target - jmp.at - 4) as i32;

            let mut slice = &mut self.data[jmp.at..];
            slice.write_u32::<LittleEndian>(diff as u32).unwrap();
        }
    }

    pub fn create_label(&mut self) -> Label {
        let idx = self.labels.len();
        self.labels.push(None);

        Label(idx)
    }

    pub fn emit_comment(&mut self, comment: Comment) {
        let pos = self.pos() as i32;
        self.comments.insert(pos, comment);
    }

    pub fn bind_label(&mut self, lbl: Label) {
        let lbl_idx = lbl.index();

        assert!(self.labels[lbl_idx].is_none());
        self.labels[lbl_idx] = Some(self.pos());
    }

    pub fn emit_bailout(&mut self, lbl: Label, trap: TrapId, pos: Position) {
        self.bailouts.push((lbl, trap, pos));
    }

    pub fn emit_bailout_inplace(&mut self, trap: TrapId, pos: Position) {
        self.emit_lineno(pos.line as i32);
        trap::emit(self, trap);
    }

    pub fn emit_exception_handler(&mut self, span: (usize, usize), catch: usize,
                                  offset: Option<i32>, catch_type: CatchType) {
        self.exception_handlers.push(ExHandler {
            try_start: span.0,
            try_end: span.1,
            catch: catch,
            offset: offset,
            catch_type: catch_type
        });
    }

    pub fn emit_label(&mut self, lbl: Label) {
        let value = self.labels[lbl.index()];

        match value {
            // backward jumps already know their target
            Some(idx) => {
                let current = self.pos() + 4;
                let target = idx;

                let diff = -((current - target) as i32);
                self.emit_u32(diff as u32);
            }

            // forward jumps do not know their target yet
            // we need to do this later...
            None => {
                let pos = self.pos();
                self.emit_u32(0);
                self.jumps.push(ForwardJump {
                    at: pos,
                    to: lbl
                });
            }
        }
    }

    pub fn emit_u8(&mut self, value: u8) {
        self.data.write_u8(value).unwrap();
    }

    pub fn emit_u32(&mut self, value: u32) {
        self.data.write_u32::<LittleEndian>(value).unwrap();
    }
}

#[derive(Debug)]
struct ForwardJump {
    at: usize,
    to: Label
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Label(usize);

impl Label {
    pub fn index(&self) -> usize {
        self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ctxt::FctId;

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
        buf.bind_label(lbl);
        buf.emit_u8(0x33);
        buf.emit_label(lbl);

        assert_eq!(vec![0x33, 0xfb, 0xff, 0xff, 0xff], buf.data());
    }

    #[test]
    fn test_backward() {
        let mut buf = Buffer::new();
        let lbl = buf.create_label();
        buf.bind_label(lbl);
        buf.emit_label(lbl);

        assert_eq!(vec![0xfc, 0xff, 0xff, 0xff], buf.data());
    }

    #[test]
    fn test_forward_with_gap() {
        let mut buf = Buffer::new();
        let lbl = buf.create_label();
        buf.emit_label(lbl);
        buf.emit_u8(0x11);
        buf.bind_label(lbl);

        assert_eq!(vec![1, 0, 0, 0, 0x11], buf.data());
    }

    #[test]
    fn test_forward() {
        let mut buf = Buffer::new();
        let lbl = buf.create_label();
        buf.emit_label(lbl);
        buf.bind_label(lbl);

        assert_eq!(vec![0, 0, 0, 0], buf.data());
    }

    #[test]
    #[should_panic]
    fn test_bind_label_twice() {
        let mut buf = Buffer::new();
        let lbl = buf.create_label();

        buf.bind_label(lbl);
        buf.bind_label(lbl);
    }

    #[test]
    #[should_panic]
    fn test_label_undefined() {
        let mut buf = Buffer::new();
        let lbl = buf.create_label();

        buf.emit_label(lbl);
        buf.jit(FctId(1), 0);
    }
}
