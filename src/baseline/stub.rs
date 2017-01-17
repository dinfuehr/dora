use std::fmt;

use masm::MacroAssembler;
use mem::code::CodeMemory;
use os::signal::Trap;

pub struct Stub {
    mem: CodeMemory,
}

impl Stub {
    pub fn new() -> Stub {
        let mut masm = MacroAssembler::new();
        masm.trap(Trap::COMPILER);
        let code = masm.jit(0).code();

        Stub { mem: code }
    }

    pub fn ptr_start(&self) -> *const u8 {
        self.mem.ptr_start()
    }

    pub fn ptr_end(&self) -> *const u8 {
        self.mem.ptr_end()
    }
}

impl fmt::Debug for Stub {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "Stub {{ start: {:x}, end: {:x} }}",
               self.ptr_start() as usize,
               self.ptr_end() as usize)
    }
}
