use cpu::emit;
use cpu::trap;
use mem::code::CodeMemory;
use mem::Ptr;
use jit::buffer::Buffer;

pub struct Stub {
    mem: CodeMemory,
}

impl Stub {
    pub fn new() -> Stub {
        let mut buf = Buffer::new();
        emit::trap(&mut buf, trap::COMPILER);
        let buf = buf.finish();

        let code = CodeMemory::from_buffer(&buf);

        Stub {
            mem: code
        }
    }

    pub fn ptr_start(&self) -> Ptr {
        self.mem.ptr()
    }
}
