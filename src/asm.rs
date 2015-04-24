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

#[test]
fn test_msb() {
    assert_eq!(0, rax.msb());
    assert_eq!(0, rcx.msb());
    assert_eq!(0, rdx.msb());
    assert_eq!(0, rbx.msb());
    assert_eq!(0, rsp.msb());
    assert_eq!(0, rbp.msb());
    assert_eq!(0, rsi.msb());
    assert_eq!(0, rdi.msb());

    assert_eq!(1, r8.msb());
    assert_eq!(1, r9.msb());
    assert_eq!(1, r10.msb());
    assert_eq!(1, r11.msb());
    assert_eq!(1, r12.msb());
    assert_eq!(1, r13.msb());
    assert_eq!(1, r14.msb());
    assert_eq!(1, r15.msb());
}

#[test]
fn test_idx() {
    assert_eq!(0, rax.idx());
    assert_eq!(1, rcx.idx());
    assert_eq!(2, rdx.idx());
    assert_eq!(3, rbx.idx());
    assert_eq!(4, rsp.idx());
    assert_eq!(5, rbp.idx());
    assert_eq!(6, rsi.idx());
    assert_eq!(7, rdi.idx());

    assert_eq!(8, r8.idx());
    assert_eq!(9, r9.idx());
    assert_eq!(10, r10.idx());
    assert_eq!(11, r11.idx());
    assert_eq!(12, r12.idx());
    assert_eq!(13, r13.idx());
    assert_eq!(14, r14.idx());
    assert_eq!(15, r15.idx());
}

#[test]
fn test_lsb3() {
    assert_eq!(0, rax.lsb3());
    assert_eq!(1, rcx.lsb3());
    assert_eq!(2, rdx.lsb3());
    assert_eq!(3, rbx.lsb3());
    assert_eq!(4, rsp.lsb3());
    assert_eq!(5, rbp.lsb3());
    assert_eq!(6, rsi.lsb3());
    assert_eq!(7, rdi.lsb3());

    assert_eq!(0, r8.lsb3());
    assert_eq!(1, r9.lsb3());
    assert_eq!(2, r10.lsb3());
    assert_eq!(3, r11.lsb3());
    assert_eq!(4, r12.lsb3());
    assert_eq!(5, r13.lsb3());
    assert_eq!(6, r14.lsb3());
    assert_eq!(7, r15.lsb3());
}

// Memory-Address: Register with positive or negative offset
struct Mem(Reg, i32);

fn fits8(v: i64) -> bool {
    v == (v as i8) as i64
}

fn fits16(v: i64) -> bool {
    v == (v as i16) as i64
}

fn fits32(v: i64) -> bool {
    v == (v as i32) as i64
}

#[test]
fn test_fits8() {
    assert!(fits8(0));
    assert!(fits8(127));
    assert!(fits8(-128));
    assert!(!fits8(128));
    assert!(!fits8(-129));
}

#[test]
fn test_fits16() {
    assert!(fits16(8));
    assert!(fits16(32767));
    assert!(fits16(-32768));
    assert!(!fits16(32768));
    assert!(!fits16(-32769));
}

// Machine instruction without any operands, just emits the given
// opcode
macro_rules! i0p {
   ($i:ident, $w:expr) => {pub fn $i(&mut self) { self.opcode($w); }}
}

// Machine Instruction with 1 register operand, register
// index is added to opcode
macro_rules! q1p_r_rpo {
    ($i:ident, $w:expr) => {pub fn $i(&mut self, dest: Reg) {
        if dest.msb() != 0 {
            self.rex_prefix(0, 0, 0, 1);
        }

        self.opcode($w + dest.lsb3());
    }}
}

// Machine instruction with 2 register operands, operands are
// saved in mod_rm-Byte. MSB is saved in rex-Prefix
macro_rules! q2p_rtr {
    ($i:ident, $w:expr) => {pub fn $i(&mut self, src: Reg, dest: Reg) {
        self.rex_prefix(1, src.msb(), 0, dest.msb());
        self.opcode($w);
        self.mod_rm(0b11, src.lsb3(), dest.lsb3());
    }}
}

macro_rules! q2p_mtr {
    ($i:ident, $w: expr) => {pub fn $i(&mut self, src: Mem, dest: Reg) {
        self.rex_prefix(1, dest.msb(), 0, src.0.msb());
        self.opcode($w);

        if(fits8(src.1 as i64)) {
            self.mod_rm(0b01, dest.lsb3(), src.0.lsb3());
            self.emitb(src.1 as u8);
        } else {
            self.mod_rm(0b10, dest.lsb3(), src.0.lsb3());
            self.emitd(src.1 as u32);
        }
    }}
}

macro_rules! q2p_rtm {
    ($i:ident, $w: expr) => {pub fn $i(&mut self, src: Reg, dest: Mem) {
        self.rex_prefix(1, src.msb(), 0, dest.0.msb());
        self.opcode($w);

        if(fits8(dest.1 as i64)) {
            self.mod_rm(0b01, src.lsb3(), dest.0.lsb3());
            self.emitb(dest.1 as u8);
        } else {
            self.mod_rm(0b10, src.lsb3(), dest.0.lsb3());
            self.emitd(dest.1 as u32);
        }
    }}
}

// machine operand with 2 operands. 64 bit immediate and destination register
// register is added to opcode
macro_rules! q2p_i64tr {
    ($i:ident, $w: expr) => {pub fn $i(&mut self, src: u64, dest: Reg) {
        self.rex_prefix(1, 0, 0, dest.msb());
        self.opcode($w + dest.lsb3());
        self.emitq(src);
    }}
}

// machine operand with 2 operands. 64 bit immediate and destination register
macro_rules! q2p_i32tr {
    ($i:ident, $w: expr) => {pub fn $i(&mut self, src: u32, dest: Reg) {
        self.rex_prefix(1, 0, 0, dest.msb());
        self.opcode($w);
        self.mod_rm(0b11, 0, dest.lsb3());
        self.emitd(src);
    }}
}

// machine operand with 2 operands. 64 bit immediate and destination memory
macro_rules! q2p_i32tm {
    ($i:ident, $w: expr) => {pub fn $i(&mut self, src: u32, dest: Mem) {
        self.rex_prefix(1, 0, 0, dest.0.msb());
        self.opcode($w);

        if(fits8(dest.1 as i64)) {
            self.mod_rm(0b01, 0, dest.0.lsb3());
            self.emitb(dest.1 as u8);
        } else {
            self.mod_rm(0b10, 0, dest.0.lsb3());
            self.emitd(dest.1 as u32);
        }

        self.emitd(src);
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

    q1p_r_rpo!(pushq, 0x50);
    q1p_r_rpo!(popq, 0x58);

    q2p_rtr!(addq_rtr, 0x01);
    q2p_rtm!(addq_rtm, 0x01);
    q2p_mtr!(addq_mtr, 0x03);
    q2p_i32tm!(addq_i32tm, 0x81);
    q2p_i32tr!(addq_i32tr, 0x81);

    q2p_rtr!(subq_rtr, 0x28);
    q2p_rtm!(subq_rtm, 0x29);
    q2p_mtr!(subq_mtr, 0x2B);
    q2p_i64tr!(subq_i32tm, 0x81); // /5 ?
    q2p_i64tr!(subq_i32tr, 0x81); // /5 ?

    q2p_rtr!(movq_rtr, 0x89);
    q2p_rtm!(movq_rtm, 0x89);
    q2p_mtr!(movq_mtr, 0x8B);
    q2p_i64tr!(movq_i64tr, 0xB8);
    q2p_i32tr!(movq_i32tr, 0xC7);
    q2p_i32tm!(movq_i32tm, 0xC7);

    fn opcode(&mut self, c: u8) { self.code.push(c); }
    fn emitb(&mut self, c: u8) { self.code.push(c); }
    fn emitw(&mut self, w: u16) { self.code.write_u16::<LittleEndian>(w).unwrap(); }
    fn emitd(&mut self, d: u32) { self.code.write_u32::<LittleEndian>(d).unwrap(); }
    fn emitq(&mut self, q: u64) { self.code.write_u64::<LittleEndian>(q).unwrap(); }

    fn mod_rm(&mut self, mode: u8, reg: u8, rm: u8) {
        self.emitb(mode << 6 | reg << 3 | rm );
    }

    fn rex_prefix(&mut self, w: u8, r: u8, x: u8, b: u8) {
        let v = 0x40 | w << 3 | r << 2 | x << 1 | b;

        self.code.push(v);
    }
}

#[test]
fn test_nop() {
    let mut asm = Assembler::new();
    asm.nop();

    assert_eq!(vec![0x90], asm.code);
}

#[test]
fn test_ret() {
    let mut asm = Assembler::new();
    asm.ret();

    assert_eq!(vec![0xc3], asm.code);
}

#[test]
fn test_push() {
    let mut asm = Assembler::new();
    asm.pushq(rax);
    asm.pushq(r8);
    asm.pushq(rcx);

    assert_eq!(vec![0x50, 0x41, 0x50, 0x51], asm.code);
}

#[test]
fn test_pop() {
    let mut asm = Assembler::new();
    asm.popq(rax);
    asm.popq(r8);
    asm.popq(rcx);

    assert_eq!(vec![0x58, 0x41, 0x58, 0x59], asm.code);
}

#[test]
fn test_mov() {
    let mut asm = Assembler::new();
    asm.movq_rtr(r15, rax);
    asm.movq_rtr(rsi, r8);

    assert_eq!(vec![0x4c, 0x89, 0xf8, 0x49, 0x89, 0xf0], asm.code);
}

#[test]
fn test_mov_i64_to_reg() {
    let mut asm = Assembler::new();
    asm.movq_i64tr(1, rax);
    asm.movq_i64tr(2, r8);

    assert_eq!(vec![0x48, 0xb8, 1, 0, 0, 0, 0, 0, 0, 0,
        0x49, 0xb8, 2, 0, 0, 0, 0, 0, 0, 0], asm.code);
}

#[test]
fn test_mov_i32_to_mem() {
    let mut asm = Assembler::new();
    asm.movq_i32tm(2, Mem(rbp, 1));
    asm.movq_i32tm(3, Mem(r8, -1));

    assert_eq!(vec![0x48, 0xC7, 0x45, 0x01, 2, 0, 0, 0,
        0x49, 0xC7, 0x40, 0xFF, 3, 0, 0, 0], asm.code);
}

#[test]
fn test_mov_i32_to_reg() {
    let mut asm = Assembler::new();
    asm.movq_i32tr(1, rax);
    asm.movq_i32tr(2, r9);

    assert_eq!(vec![0x48, 0xC7, 0xC0, 1, 0, 0, 0,
        0x49, 0xC7, 0xC1, 2, 0, 0, 0], asm.code);
}

#[test]
fn test_add_reg_to_reg() {
    let mut asm = Assembler::new();
    asm.addq_rtr(r10, rcx);
    asm.addq_rtr(rbx, r10);

    assert_eq!(vec![0x4c, 0x01, 0xd1,
        0x49, 0x01, 0xda], asm.code);
}

#[test]
fn test_add_reg_to_mem() {
    let mut asm = Assembler::new();
    asm.addq_rtm(r9, Mem(rbp, 1));
    asm.addq_rtm(rcx, Mem(rax, -1));

    assert_eq!(vec![0x4C, 0x01, 0x4D, 0x01,
        0x48, 0x01, 0x48, 0xFF], asm.code);
}

#[test]
fn test_add_mem_to_reg() {
    let mut asm = Assembler::new();
    asm.addq_mtr(Mem(rbp, 1), r9);
    asm.addq_mtr(Mem(rax, -1), rcx);

    assert_eq!(vec![0x4C, 0x03, 0x4D, 0x01,
        0x48, 0x03, 0x48, 0xFF], asm.code);
}

#[test]
fn test_add_i32_to_reg() {
    let mut asm = Assembler::new();
    asm.addq_i32tr(1, rcx);
    asm.addq_i32tr(0x100, r9);

    assert_eq!(vec![0x48, 0x81, 0xC1, 1, 0, 0, 0,
        0x49, 0x81, 0xC1, 0, 1, 0, 0], asm.code);
}

#[test]
fn test_add_i32_to_mem() {
    let mut asm = Assembler::new();
    asm.addq_i32tm(1, Mem(rcx,1));
    asm.addq_i32tm(0x100, Mem(r10,-1));

    assert_eq!(vec![0x48, 0x81, 0x41, 0x01, 1, 0, 0, 0,
        0x49, 0x81, 0x42, 0xFF, 0, 1, 0, 0], asm.code);
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
