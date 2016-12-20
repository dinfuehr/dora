use std::fmt;

use baseline::buffer::MacroAssembler;
use cpu::trap;
use ctxt::FctId;
use mem::code::CodeMemory;
use mem::Ptr;

pub struct Stub {
    mem: CodeMemory,
}

impl Stub {
    pub fn new(fct_id: FctId) -> Stub {
        let mut buf = MacroAssembler::new();
        trap::emit(&mut buf, trap::COMPILER);
        let code = buf.jit(fct_id, 0).code();

        Stub {
            mem: code
        }
    }

    pub fn ptr_start(&self) -> Ptr {
        self.mem.ptr_start()
    }

    pub fn ptr_end(&self) -> Ptr {
        self.mem.ptr_end()
    }
}

impl fmt::Debug for Stub {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Stub {{ start: {:x}, end: {:x} }}",
            self.ptr_start().raw() as usize, self.ptr_end().raw() as usize)
    }
}
