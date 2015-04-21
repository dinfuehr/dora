use byteorder::{LittleEndian, WriteBytesExt};
use libc::consts::os::posix88::*;
use libc::funcs::posix88::mman::mmap;
use libc::funcs::posix88::mman::munmap;
use libc::funcs::posix88::mman::mprotect;
use libc::types::common::c95::c_void;
use libc::types::os::arch::c95::size_t;
use std::ptr;
use std::mem;
use asm::Reg::*;

enum Reg {
    rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi,
    r8, r9, r10, r11, r12, r13, r14, r15,
}

impl Reg {
    // most significant bit
    fn msb(&self) -> u8 {
        match *self {
            rax | rcx | rdx | rbx | rsp | rbp | rsi | rdi => 0,
            r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15 => 1,
        }
    }

    // least 3 significant bit
    fn lsb3(&self) -> u8 {
        match *self {
            rax => 0,
            rcx => 1,
            rdx => 2,
            rbx => 3,
            rsp => 4,
            rbp => 5,
            rsi => 6,
            rdi => 7,
            r8 => 0,
            r9 => 1,
            r10 => 2,
            r11 => 3,
            r12 => 4,
            r13 => 5,
            r14 => 6,
            r15 => 7,
        }
    }

    // index of enum
    fn idx(&self) -> u8 {
        match *self {
            rax => 0,
            rcx => 1,
            rdx => 2,
            rbx => 3,
            rsp => 4,
            rbp => 5,
            rsi => 6,
            rdi => 7,
            r8 => 8,
            r9 => 9,
            r10 => 10,
            r11 => 11,
            r12 => 12,
            r13 => 13,
            r14 => 14,
            r15 => 15,
        }
    }
}

macro_rules! i0p {
    ($i:ident, $w:expr) => {pub fn $i(&mut self) { self.opcode($w); }}
}

macro_rules! q1p_rpo {
    ($i:ident, $w:expr) => {pub fn $i(&mut self, dest: Reg) {
        if dest.msb() != 0 {
            self.rex_prefix(0, 0, 0, 1);
        }

        self.opcode($w + dest.lsb3());
    }}
}

pub struct Assembler {
    code: Vec<u8>
}

impl Assembler {
    pub fn new() -> Assembler {
        Assembler { code: Vec::new() }
    }

    i0p!(nop, 0x90);
    i0p!(ret, 0xC3);

    q1p_rpo!(pushq, 0x50);
    q1p_rpo!(popq, 0x58);

    //q2p_r2r!(movq_r2r);
    //q2p_m2r!(movq_m2r);
    //q2p_r2m!(movq_r2m);

    fn opcode(&mut self, c: u8) { self.code.push(c); }
    fn emitb(&mut self, c: u8) { self.code.push(c); }
    fn emitw(&mut self, w: u16) { self.code.write_u16::<LittleEndian>(w).unwrap(); }
    fn emitd(&mut self, d: u32) { self.code.write_u32::<LittleEndian>(d).unwrap(); }
    fn emitq(&mut self, q: u64) { self.code.write_u64::<LittleEndian>(q).unwrap(); }

    fn rex_prefix(&mut self, w: u8, r: u8, x: u8, b: u8) {
        let v = 0x40 | w << 3 | r << 2 | x << 1 | b;

        self.code.push(v);
    }
}


static CODE : [u8;8] = [ 0x48, 0x89, 0xf8, 0x48, 0x83, 0xc0, 0x04, 0xc3 ];

pub struct JitFunction {
    pub size: size_t,
    pub ptr: extern fn (u64) -> u64
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
                ptr: mem::transmute(fct)
            }
        }
    }
}

impl Drop for JitFunction {
    fn drop(&mut self) {
        unsafe {
            munmap(mem::transmute(self.ptr), self.size);
        }
    }
}

#[test]
fn test_create_function() {
    let fct = JitFunction::new(&CODE);

    assert_eq!(5, (fct.ptr)(1));
}
