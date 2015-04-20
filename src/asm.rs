use byteorder::{LittleEndian, WriteBytesExt};
use libc::consts::os::posix88::*;
use libc::funcs::posix88::mman::mmap;
use libc::types::common::c95::c_void;
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

static CODE : [u8;8] = [ 0x48u8, 0x89, 0xf8, 0x48, 0x83, 0xc0, 0x04, 0xc3 ];

fn create_function() -> extern fn (u64) -> u64 {
    unsafe {
        let fct = mmap(0 as *mut c_void, 1024, PROT_READ | PROT_WRITE | PROT_EXEC,
            MAP_PRIVATE | MAP_ANON, -1, 0);

        if fct == MAP_FAILED {
            panic!("mmap failed");
        }

        ptr::copy_nonoverlapping(CODE.as_ptr() as *const c_void, fct, CODE.len());

        mem::transmute(fct)
    }
}

#[test]
fn test_create_function() {
    let fct = create_function();

    assert_eq!(5, fct(1));
}
