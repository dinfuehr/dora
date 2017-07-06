use std::cmp;

use byteorder::{ByteOrder, LittleEndian, WriteBytesExt};

use bytecode::opcodes::*;
use dora_parser::interner::Name;

pub struct BytecodeGenerator {
    params: Vec<Param>,
    vars: Vec<Var>,
    regs: usize,
    code: Vec<u8>,
}

impl BytecodeGenerator {
    pub fn add_param(&mut self, name: Name, ty: BytecodeType) -> BytecodeParamId {
        let len = self.params.len() as u32;
        self.params.push(Param(name, ty));

        BytecodeParamId(len)
    }

    pub fn add_var(&mut self, name: Name, ty: BytecodeType) -> BytecodeVarId {
        let len = self.vars.len() as u32;
        self.vars.push(Var(name, ty));

        BytecodeVarId(len)
    }

    pub fn get_local_int32(&mut self, dest: BytecodeReg, src: BytecodeVarId) {
        let width = cmp::max(dest.width(), src.width());
        self.emit_op(BC_GET_LOCAL_INT32);
        self.emit_u32(width, dest.0);
        self.emit_u32(width, src.0);
    }

    pub fn set_local_int32(&mut self, dest: BytecodeVarId, src: BytecodeReg) {
        let width = cmp::max(dest.width(), src.width());
        self.emit_op(BC_SET_LOCAL_INT32);
        self.emit_u32(width, dest.0);
        self.emit_u32(width, src.0);
    }

    pub fn add_int32(&mut self, dest: BytecodeReg, lhs: BytecodeReg, rhs: BytecodeReg) {
        let width = max_width(dest.width(), lhs.width(), rhs.width());
        self.emit_op(BC_ADD_INT32);
        self.emit_u32(width, dest.0);
        self.emit_u32(width, lhs.0);
        self.emit_u32(width, rhs.0);
    }

    fn emit_op(&mut self, op: u8) {
        self.code.push(op);
    }

    fn emit_u32(&mut self, width: OpndWidth, val: u32) {
        match width {
            OpndWidth::Small => self.code.push(val as u8),
            OpndWidth::Wide => self.code.write_u16::<LittleEndian>(val as u16).unwrap(),
            OpndWidth::ExtraWide => self.code.write_u32::<LittleEndian>(val).unwrap(),
        }
    }

    fn reg(&mut self) -> BytecodeReg {
        let reg = self.regs;
        self.regs += 1;

        BytecodeReg(reg as u32)
    }
}

fn max_width(op1: OpndWidth, op2: OpndWidth, op3: OpndWidth) -> OpndWidth {
    cmp::max(op1, cmp::max(op2, op3))
}

struct Param(Name, BytecodeType);
struct Var(Name, BytecodeType);

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum OpndWidth { Small, Wide, ExtraWide }

trait HasWidth {
    fn width(self) -> OpndWidth;
}

#[derive(Copy, Clone)]
pub struct BytecodeReg(u32);

impl HasWidth for BytecodeReg {
    fn width(self) -> OpndWidth {
        width(self.0)
    }
}

#[derive(Copy, Clone)]
pub struct BytecodeParamId(u32);

impl HasWidth for BytecodeParamId {
    fn width(self) -> OpndWidth {
        width(self.0)
    }
}

#[derive(Copy, Clone)]
pub struct BytecodeVarId(u32);

impl HasWidth for BytecodeVarId {
    fn width(self) -> OpndWidth {
        width(self.0)
    }
}

fn width(val: u32) -> OpndWidth {
    if val <= 0xFF {
        OpndWidth::Small
    } else if val <= 0xFFFF {
        OpndWidth::Wide
    } else {
        OpndWidth::ExtraWide
    }
}

#[derive(Copy, Clone)]
pub enum BytecodeType {
    Bool,
    Char,
    Int8, Int32, Int64,
    Float32, Float64,
    Ptr,
}

