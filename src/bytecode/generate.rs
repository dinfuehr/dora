use std::cmp;

use byteorder::{LittleEndian, WriteBytesExt};

use bytecode::opcodes::*;
use dora_parser::interner::Name;

macro_rules! emit {
    ( $gen:expr, $opcode:expr, $($opnd:expr),* ) => {
        let mut max_width = OpndWidth::Small;

        $(
            max_width = cmp::max(max_width, width($opnd));
        )*;

        match max_width {
            OpndWidth::Small => {}
            OpndWidth::Wide => {
                $gen.emit_op(BC_WIDE);
            }
            OpndWidth::ExtraWide => {
                $gen.emit_op(BC_XWIDE);
            }
        }

        $gen.emit_op($opcode);

        $(
            $gen.emit_opnd(max_width, $opnd);
        )*;
    };
}

pub struct BytecodeGenerator {
    vars: Vec<Var>,
    params: usize,
    regs: usize,
    code: Vec<u8>,
    return_type: Option<BytecodeType>,
}

impl BytecodeGenerator {
    pub fn new() -> BytecodeGenerator {
        BytecodeGenerator {
            vars: Vec::new(),
            params: 0,
            regs: 0,
            code: Vec::new(),
            return_type: None,
        }
    }

    pub fn add_param(&mut self, name: Name, ty: BytecodeType) -> BytecodeReg {
        self.vars.push(Var(Some(name), ty));
        self.params += 1;

        self.create_reg()
    }

    pub fn add_var(&mut self, name: Name, ty: BytecodeType) -> BytecodeReg {
        self.vars.push(Var(Some(name), ty));

        self.create_reg()
    }

    pub fn add_temp(&mut self, ty: BytecodeType) -> BytecodeReg {
        self.vars.push(Var(None, ty));

        self.create_reg()
    }

    pub fn set_return_type(&mut self, ty: BytecodeType) {
        self.return_type = Some(ty);
    }

    pub fn add_int32(&mut self, dest: BytecodeReg, lhs: BytecodeReg, rhs: BytecodeReg) {
        emit!(self, BC_ADD_INT32, dest.0, lhs.0, rhs.0);
    }

    pub fn add_int64(&mut self, dest: BytecodeReg, lhs: BytecodeReg, rhs: BytecodeReg) {
        emit!(self, BC_ADD_INT64, dest.0, lhs.0, rhs.0);
    }

    fn emit_op(&mut self, op: u8) {
        self.code.push(op);
    }

    fn emit_opnd(&mut self, width: OpndWidth, val: u32) {
        match width {
            OpndWidth::Small => self.code.push(val as u8),
            OpndWidth::Wide => self.code.write_u16::<LittleEndian>(val as u16).unwrap(),
            OpndWidth::ExtraWide => self.code.write_u32::<LittleEndian>(val).unwrap(),
        }
    }

    fn create_reg(&mut self) -> BytecodeReg {
        let reg = self.regs;
        self.regs += 1;

        BytecodeReg(reg as u32)
    }
}

fn max_width(op1: OpndWidth, op2: OpndWidth, op3: OpndWidth) -> OpndWidth {
    cmp::max(op1, cmp::max(op2, op3))
}

struct Var(Option<Name>, BytecodeType);

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum OpndWidth {
    Small,
    Wide,
    ExtraWide,
}

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
    Int8,
    Int32,
    Int64,
    Float32,
    Float64,
    Ptr,
}
