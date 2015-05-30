use byteorder::{LittleEndian, WriteBytesExt};
use libc::consts::os::posix88::*;
use libc::mmap;
use libc::munmap;
use libc::mprotect;
use libc::c_void;
use libc::size_t;
use std::ptr;
use std::mem;
use asm::Reg::*;

#[allow(non_camel_case_types)]
#[derive(PartialEq,Eq,Debug,Copy,Clone)]
#[repr(C)]
pub enum Reg {
    rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi,
    r8, r9, r10, r11, r12, r13, r14, r15,
}

impl Reg {
    // most significant bit
    fn msb(&self) -> u8 {
        (self.idx() & 8) >> 3
    }

    // least 3 significant bit
    fn lsb3(&self) -> u8 {
        self.idx() & 7
    }

    // index of enum
    fn idx(&self) -> u8 {
        let val : u32 = unsafe { mem::transmute(*self) };

        val as u8
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

#[derive(Debug)]
pub struct Addr {
    base: Reg,
    index: Option<Reg>,
    scale: u8,
    disp: i32,

    direct: bool,
}

impl Addr {
    pub fn direct(base: Reg) -> Addr {
        Addr { base: base, index: None, scale: 0, disp: 0, direct: true }
    }

    pub fn indirect(base: Reg) -> Addr {
        Addr { base: base, index: None, scale: 0, disp: 0, direct: false }
    }

    pub fn with_disp(base: Reg, disp: i32) -> Addr {
        Addr { base: base, index: None, scale: 0, disp: disp, direct: false }
    }

    pub fn with_sib(base: Reg, idx: Reg, scale: u8, disp: i32) -> Addr {
        assert!(idx != rsp);
        assert!(scale <= 3);

        Addr { base: base, index: Some(idx), scale: scale, disp: disp, direct: false }
    }

    fn msb(&self) -> u8 {
        self.base.msb()
    }

    fn base_special(&self) -> bool {
        self.base == rsp || self.base == rbp || self.base == r12 || self.base == r13
    }

    fn emit(&self, asm: &mut Assembler, modrm_reg: u8) {
        if self.direct {
            asm.modrm(0b11, modrm_reg, self.base.lsb3());

        } else if self.index.is_none() {
            if !self.base_special() {
                self.emit_without_sib(asm, modrm_reg);
            } else if self.base == rsp || self.base == r13 {
                self.emit_rsp(asm, modrm_reg);

            } else {
                self.emit_rbp(asm, modrm_reg);
            }

        } else {
            self.emit_sib(asm, modrm_reg);
        }
    }

    fn emit_rsp(&self, asm: &mut Assembler, modrm_reg: u8) {
        let fits_into_byte = fits8(self.disp as i64);
        let mode =
            if fits_into_byte { 0b01 }
            else { 0b10 };

        asm.modrm(mode, modrm_reg, 0b100);
        asm.sib(0, 0b100, self.base.lsb3());

        if fits_into_byte {
            asm.emitb(self.disp as u8);
        } else {
            asm.emitd(self.disp as u32);
        }
    }

    fn emit_rbp(&self, asm: &mut Assembler, modrm_reg: u8) {
        let fits_into_byte = fits8(self.disp as i64);
        let mode =
            if fits_into_byte { 0b01 }
            else { 0b10 };

        asm.modrm(mode, modrm_reg, 0b101);

        if fits_into_byte {
            asm.emitb(self.disp as u8);
        } else {
            asm.emitd(self.disp as u32);
        }
    }

    fn emit_sib(&self, asm: &mut Assembler, modrm_reg: u8) {
        let fits_into_byte = fits8(self.disp as i64);

        let mode =
            if self.disp == 0 { 0b00 }
            else if fits_into_byte { 0b01 }
            else { 0b10 };

        asm.modrm(mode, modrm_reg, 0b100);
        asm.sib(self.scale, self.index.unwrap().lsb3(), self.base.lsb3());

        if self.disp != 0 {
            if fits_into_byte {
                asm.emitb(self.disp as u8);
            } else {
                asm.emitd(self.disp as u32);
            }
        }
    }

    fn emit_without_sib(&self, asm: &mut Assembler, modrm_reg: u8) {
        let fits_into_byte = fits8(self.disp as i64);

        let mode =
            if self.disp == 0 { 0b00 }
            else if fits_into_byte { 0b01 }
            else { 0b10 };

        asm.modrm(mode, modrm_reg, self.base.lsb3());

        if self.disp != 0 {
            if fits_into_byte {
                asm.emitb(self.disp as u8);
            } else {
                asm.emitd(self.disp as u32);
            }
        }
    }
}

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

macro_rules! q2p_atr {
    ($i:ident, $w: expr) => {pub fn $i(&mut self, src: Addr, dest: Reg) {
        self.rex_prefix(1, dest.msb(), 0, src.msb());
        self.opcode($w);
        src.emit(self, dest.lsb3());
    }}
}

// machine instruction with 2 operands. register to address
macro_rules! q2p_rta {
    ($i:ident, $w: expr) => {pub fn $i(&mut self, src: Reg, dest: Addr) {
        self.rex_prefix(1, src.msb(), 0, dest.msb());
        self.opcode($w);

        dest.emit(self, src.lsb3());
    }}
}

// machine instruction with 2 operands. u64 to register. register index is
// added to opcode
macro_rules! q2p_i64tr {
    ($i:ident, $w: expr) => {pub fn $i(&mut self, src: u64, dest: Reg) {
        self.rex_prefix(1, 0, 0, dest.msb());
        self.opcode($w + dest.lsb3());
        self.emitq(src);
    }}
}

// machine instruction with 2 operands. 32 bit immediate and destination operand
macro_rules! q2p_i32ta {
    ($i:ident, $w: expr, $modrm_reg: expr) => {pub fn $i(&mut self, src: u32, dest: Addr) {
        self.rex_prefix(1, 0, 0, dest.msb());
        self.opcode($w);
        dest.emit(self, $modrm_reg);
        self.emitd(src);
    }}
}

// machine instruction with 2 operands. 8 bit immediate and destination operand
macro_rules! q2p_i8ta {
    ($i:ident, $w: expr, $modrm_reg: expr) => {pub fn $i(&mut self, src: u8, dest: Addr) {
        self.rex_prefix(1, 0, 0, dest.msb());
        self.opcode($w);
        dest.emit(self, $modrm_reg);
        self.emitb(src);
    }}
}

#[derive(Copy,Clone)]
struct Label(usize);

pub struct Assembler {
    code: Vec<u8>,
    labels: Vec<Option<usize>>,
    link_jmps: Vec<(usize, usize)>,
}

impl Assembler {
    pub fn new() -> Assembler {
        Assembler { code: Vec::new(), labels: Vec::new(), link_jmps: Vec::new() }
    }

    i0p!(nop, 0x90);
    i0p!(ret, 0xC3);

    q1p_r_rpo!(pushq, 0x50);
    q1p_r_rpo!(popq, 0x58);

    q2p_rta!(addq_rta, 0x01);
    q2p_atr!(addq_atr, 0x03);
    q2p_i32ta!(addq_i32ta, 0x81, 0);
    q2p_i8ta!(addq_i8ta, 0x83, 0);

    q2p_rta!(subq_rta, 0x29);
    q2p_atr!(subq_atr, 0x2B);
    q2p_i32ta!(subq_i32ta, 0x81, 5);
    q2p_i8ta!(subq_i8ta, 0x83, 5);

    q2p_rta!(movq_rta, 0x89);
    q2p_atr!(movq_atr, 0x8B);
    q2p_i64tr!(movq_i64tr, 0xB8);
    q2p_i32ta!(movq_i32ta, 0xC7, 0);

    fn label(&mut self) -> Label {
        let id = self.labels.len();
        self.labels.push(None);

        Label(id)
    }

    fn jmp(&mut self, lbl: Label) {
        let Label(id) = lbl;
        let dest = self.labels[id];
        let src = self.code.len();

        if dest.is_some() {
            let dest = dest.unwrap() as i32;
            let src = src as i32;

            let offset = (dest-src-5) as u32;

            self.jmp_offset(offset);
        } else {
            self.reserve(5);
            self.link_jmps.push((id, src));
        }
    }

    fn jmp_offset(&mut self, offset: u32) {
        self.opcode(0xE9);
        self.emitd(offset);
    }

    fn bind(&mut self, lbl: Label) {
        let Label(id) = lbl;

        assert!(self.labels[id].is_none(), "label already bound");
        self.labels[id] = Some(self.code.len());
    }

    fn opcode(&mut self, c: u8) { self.code.push(c); }
    fn emitb(&mut self, c: u8) { self.code.push(c); }
    fn emitw(&mut self, w: u16) { self.code.write_u16::<LittleEndian>(w).unwrap(); }
    fn emitd(&mut self, d: u32) { self.code.write_u32::<LittleEndian>(d).unwrap(); }
    fn emitq(&mut self, q: u64) { self.code.write_u64::<LittleEndian>(q).unwrap(); }

    fn modrm(&mut self, mode: u8, reg: u8, rm: u8) {
        self.emitb(mode << 6 | reg << 3 | rm );
    }

    fn sib(&mut self, scale: u8, index: u8, base: u8) {
        self.emitb(scale << 6 | index << 3 | base);
    }

    fn rex_prefix(&mut self, w: u8, r: u8, x: u8, b: u8) {
        let v = 0x40 | w << 3 | r << 2 | x << 1 | b;

        self.code.push(v);
    }

    fn reserve(&mut self, size: usize) {
        for i in 0..size {
            self.nop();
        }
    }


    fn code(mut self) -> Vec<u8> {
        self.link();

        self.code
    }

    fn link(&mut self) {
        for &(lbl, pos) in &self.link_jmps {
            let dest = self.labels[lbl].unwrap();
            let offset = dest - pos - 5;

            self.code[pos] = 0xE9;
            (&mut self.code[pos+1..]).write_u32::<LittleEndian>(offset as u32).unwrap();
        }
    }
}

#[test]
fn test_backward_jump() {
    let mut asm = Assembler::new();
    let lbl = asm.label();
    asm.bind(lbl);
    asm.nop();
    asm.nop();
    asm.jmp(lbl);

    assert_eq!(vec![0x90, 0x90, 0xE9, 0xF9, 0xFF, 0xFF, 0xFF], asm.code);
}

#[test]
fn test_forward_jump() {
    let mut asm = Assembler::new();
    let lbl = asm.label();
    asm.jmp(lbl);
    asm.nop();
    asm.nop();
    asm.bind(lbl);

    assert_eq!(vec![0x90; 7], asm.code);
    assert_eq!(vec![0xE9, 2, 0, 0, 0, 0x90, 0x90], asm.code());
}

#[test]
#[should_panic]
fn test_label_not_bound() {
    let mut asm = Assembler::new();
    let lbl = asm.label();
    asm.jmp(lbl);

    asm.code();
}

#[test]
#[should_panic]
fn test_label_bound_twice() {
    let mut asm = Assembler::new();
    let lbl = asm.label();
    asm.bind(lbl);
    asm.bind(lbl);
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
#[should_panic]
fn test_sib_with_rsp() {
    Addr::with_sib(rsp, rsp, 1, 1);
}

#[test]
#[should_panic]
fn test_sib_with_illegal_scale() {
    Addr::with_sib(rsp, rbp, 4, 1);
}

#[test]
fn test_sib_without_rsp() {
    Addr::with_sib(rsp, rcx, 1, 1);
    Addr::with_sib(rsp, r9, 1, 1);
    Addr::with_sib(rsp, rbp, 1, 1);
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
    asm.movq_rta(r15, Addr::direct(rax));
    asm.movq_rta(rsi, Addr::direct(r8));

    assert_eq!(vec![0x4c, 0x89, 0xf8, 0x49, 0x89, 0xf0], asm.code);
}

#[test]
fn test_mov_addr_to_reg() {
    let mut asm = Assembler::new();
    asm.movq_atr(Addr::indirect(rax), r9);

    assert_eq!(vec![0x4C, 0x8B, 0x08], asm.code);
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
    asm.movq_i32ta(2, Addr::with_disp(rbp, 1));
    asm.movq_i32ta(3, Addr::with_disp(r8, -1));

    assert_eq!(vec![0x48, 0xC7, 0x45, 0x01, 2, 0, 0, 0,
        0x49, 0xC7, 0x40, 0xFF, 3, 0, 0, 0], asm.code);
}

#[test]
fn test_mov_i32_to_reg() {
    let mut asm = Assembler::new();
    asm.movq_i32ta(1, Addr::direct(rax));
    asm.movq_i32ta(2, Addr::direct(r9));

    assert_eq!(vec![0x48, 0xC7, 0xC0, 1, 0, 0, 0,
        0x49, 0xC7, 0xC1, 2, 0, 0, 0], asm.code);
}

#[test]
fn test_add_reg_to_reg() {
    let mut asm = Assembler::new();
    asm.addq_rta(r10, Addr::direct(rcx));
    asm.addq_rta(rbx, Addr::direct(r10));

    assert_eq!(vec![0x4c, 0x01, 0xd1,
        0x49, 0x01, 0xda], asm.code);
}

#[test]
fn test_add_reg_to_addr() {
    let mut asm = Assembler::new();
    asm.addq_rta(r9, Addr::with_disp(rbp, 1));
    asm.addq_rta(rcx, Addr::with_disp(rax, -1));
    asm.addq_rta(r9, Addr::indirect(rax));

    assert_eq!(vec![0x4C, 0x01, 0x4D, 0x01,
        0x48, 0x01, 0x48, 0xFF,
        0x4C, 0x01, 0x08], asm.code);
}

#[test]
fn test_add_addr_to_reg() {
    let mut asm = Assembler::new();
    asm.addq_atr(Addr::with_disp(rbp, 1), r9);
    asm.addq_atr(Addr::with_disp(rax, -1), rcx);
    asm.addq_atr(Addr::with_sib(rsp, rbp, 1, 3), r9);
    asm.addq_atr(Addr::with_sib(rsp, rbp, 2, 0), r9);

    assert_eq!(vec![0x4C, 0x03, 0x4D, 0x01,
        0x48, 0x03, 0x48, 0xFF,
        0x4C, 0x03, 0x4C, 0x6C, 0x03,
        0x4C, 0x03, 0x0C, 0xAC], asm.code);
}

#[test]
fn test_add_i32_to_reg() {
    let mut asm = Assembler::new();
    asm.addq_i32ta(1, Addr::direct(rcx));
    asm.addq_i32ta(0x100, Addr::direct(r9));

    assert_eq!(vec![0x48, 0x81, 0xC1, 1, 0, 0, 0,
        0x49, 0x81, 0xC1, 0, 1, 0, 0], asm.code);
}

#[test]
fn test_add_i8_to_reg() {
    let mut asm = Assembler::new();
    asm.addq_i8ta(1, Addr::direct(r8));
    asm.addq_i8ta(1, Addr::direct(rsi));

    assert_eq!(vec![0x49, 0x83, 0xC0, 0x01,
        0x48, 0x83, 0xC6, 0x01], asm.code);
}

#[test]
fn test_add_i32_to_addr() {
    let mut asm = Assembler::new();
    asm.addq_i32ta(1, Addr::with_disp(rcx,1));
    asm.addq_i32ta(0x100, Addr::with_disp(r10,-1));

    assert_eq!(vec![0x48, 0x81, 0x41, 0x01, 1, 0, 0, 0,
        0x49, 0x81, 0x42, 0xFF, 0, 1, 0, 0], asm.code);
}

#[test]
fn test_add_i8_to_addr() {
    let mut asm = Assembler::new();
    asm.addq_i8ta(1, Addr::with_disp(rcx, 1));
    asm.addq_i8ta(2, Addr::with_disp(r11, -1));

    assert_eq!(vec![0x48, 0x83, 0x41, 0x01, 0x01,
        0x49, 0x83, 0x43, 0xFF, 0x02], asm.code);
}

#[test]
fn test_sub_i32_from_addr() {
    let mut asm = Assembler::new();
    asm.subq_i32ta(1, Addr::with_disp(rcx,1));
    asm.subq_i32ta(0x100, Addr::with_disp(r10,-1));

    assert_eq!(vec![0x48, 0x81, 0x69, 0x01, 1, 0, 0, 0,
        0x49, 0x81, 0x6A, 0xFF, 0, 1, 0, 0], asm.code);
}

#[test]
fn test_sub_i8_from_addr() {
    let mut asm = Assembler::new();
    asm.subq_i8ta(1, Addr::with_disp(rcx, 1));
    asm.subq_i8ta(2, Addr::with_disp(r11, -1));

    assert_eq!(vec![0x48, 0x83, 0x69, 0x01, 0x01,
        0x49, 0x83, 0x6B, 0xFF, 0x02], asm.code);
}

#[test]
fn test_sub_reg_from_reg() {
    let mut asm = Assembler::new();
    asm.subq_rta(r10, Addr::direct(rcx));
    asm.subq_rta(rbx, Addr::direct(r10));

    assert_eq!(vec![0x4C, 0x29, 0xD1,
        0x49, 0x29, 0xDA], asm.code);
}

#[test]
fn test_sub_reg_from_addr() {
    let mut asm = Assembler::new();
    asm.subq_rta(r9, Addr::with_disp(rbp, 1));
    asm.subq_rta(rcx, Addr::with_disp(rax, -1));

    assert_eq!(vec![0x4C, 0x29, 0x4D, 0x01,
        0x48, 0x29, 0x48, 0xFF], asm.code);
}

#[test]
fn test_sub_addr_from_reg() {
    let mut asm = Assembler::new();
    asm.subq_atr(Addr::with_disp(rbp, 1), r9);
    asm.subq_atr(Addr::with_disp(rax, -1), rcx);

    assert_eq!(vec![0x4C, 0x2B, 0x4D, 0x01,
        0x48, 0x2B, 0x48, 0xFF], asm.code);
}

#[test]
fn test_sub_i32_from_reg() {
    let mut asm = Assembler::new();
    asm.subq_i32ta(1, Addr::direct(rcx));
    asm.subq_i32ta(0x100, Addr::direct(r9));

    assert_eq!(vec![0x48, 0x81, 0xE9, 1, 0, 0, 0,
        0x49, 0x81, 0xE9, 0, 1, 0, 0], asm.code);
}

#[test]
fn test_sub_i8_from_reg() {
    let mut asm = Assembler::new();
    asm.subq_i8ta(1, Addr::direct(r8));
    asm.subq_i8ta(1, Addr::direct(rsi));

    assert_eq!(vec![0x49, 0x83, 0xE8, 0x01,
        0x48, 0x83, 0xEE, 0x01], asm.code);
}

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
    let mut asm = Assembler::new();
    asm.movq_rta(rdi, Addr::direct(rax));
    asm.addq_i8ta(4, Addr::direct(rax));
    asm.ret();

    let fct = JitFunction::new(&asm.code);

    assert_eq!(5, (fct.ptr)(1));
}
