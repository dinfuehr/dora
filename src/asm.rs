use byteorder::{LittleEndian, WriteBytesExt};
use libc::consts::os::posix88::*;
use libc::funcs::posix88::mman::mmap;
use libc::funcs::posix88::mman::munmap;
use libc::funcs::posix88::mman::mprotect;
use libc::types::common::c95::c_void;
use libc::types::os::arch::c95::size_t;
use std::ptr;
use std::mem;

pub struct Assembler {
    code: Vec<u8>
}

impl Assembler {
    pub fn new() -> Assembler {
        Assembler { code: Vec::new() }
    }

    pub fn movq(&mut self, dest: Reg64, src: u64) {
        self.code.push(0x48);
        self.code.push(0xB8);
        self.code.write_u64::<LittleEndian>(src).unwrap();
    }

    pub fn nop(&mut self) {
        self.code.push(0x90);
    }

    pub fn ret(&mut self) {
        self.code.push(0xC3);
    }
}

enum Reg64 {
    rax
}

static CODE : [u8;8] = [ 0x48, 0x89, 0xf8, 0x48, 0x83, 0xc0, 0x04, 0xc3 ];

pub struct JitFunction {
    pub size: size_t,
    pub call: extern fn (u64) -> u64
}

impl JitFunction {
    pub fn new(code: &[u8]) -> JitFunction {
        let size = code.len() as u64;

        unsafe {
            let fct = mmap(0 as *mut c_void, size, PROT_READ | PROT_WRITE,
                MAP_PRIVATE | MAP_ANON, -1, 0);

            if fct == MAP_FAILED {
                panic!("mmap failed");
            }

            ptr::copy_nonoverlapping(code.as_ptr() as *const c_void, fct, code.len());

            if mprotect(fct, size, PROT_READ | PROT_EXEC) == -1 {
                panic!("mprotect failed");
            }

            JitFunction {
                size: size,
                call: mem::transmute(fct)
            }
        }
    }
}

impl Drop for JitFunction {
    fn drop(&mut self) {
        unsafe {
            munmap(mem::transmute(self.call), self.size);
        }
    }
}

#[test]
fn test_create_function() {
    let fct = JitFunction::new(&CODE);

    assert_eq!(5, (fct.call)(1));
}
