use jit::buffer::Buffer;

pub fn nop(buf: &mut Buffer) {
    buf.emit_u32(0);
}