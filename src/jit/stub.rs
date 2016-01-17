use cpu::emit;
use cpu::trap;
use mem::code::CodeMemory;
use jit::buffer::Buffer;

struct Stub {
    mem: CodeMemory,
}

impl Stub {
    fn new() -> Stub {
        let mut buf = Buffer::new();
        emit::trap(&mut buf, trap::COMPILER);
        let buf = buf.finish();

        let code = CodeMemory::from_buffer(&buf);

        Stub {
            mem: code
        }
    }
}
