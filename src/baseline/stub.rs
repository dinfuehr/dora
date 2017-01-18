use std::fmt;

use baseline::map::CodeData;
use ctxt::Context;
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

pub fn ensure_stub<'ast>(ctxt: &Context<'ast>) -> *const u8 {
    let mut compile_stub = ctxt.compile_stub.borrow_mut();

    if let Some(ref stub) = *compile_stub {
        return stub.ptr_start();
    }

    let stub = Stub::new();

    {
        let mut code_map = ctxt.code_map.lock().unwrap();
        code_map.insert(stub.ptr_start(), stub.ptr_end(), CodeData::CompileStub);
    }

    if ctxt.args.flag_emit_stubs {
        println!("create stub at {:x}", stub.ptr_start() as usize);
    }

    let ptr = stub.ptr_start();
    *compile_stub = Some(stub);

    ptr
}

