use crate::class::ClassDefId;
use crate::field::FieldId;
use crate::vm::{FctId, GlobalId};

use crate::bytecode::generate::Register;
use crate::bytecode::opcode::Bytecode;

#[derive(Copy, Clone)]
pub enum BytecodeInst {
    Wide,

    AddInt,
    AddLong,
    AddFloat,
    AddDouble,

    SubInt,
    NegInt,
    NegLong,
    MulInt,
    DivInt,
    ModInt,
    AndInt,
    OrInt,
    XorInt,
    NotBool,

    ShlInt,
    ShrInt,
    SarInt,

    MovBool,
    MovByte,
    MovChar,
    MovInt,
    MovLong,
    MovFloat,
    MovDouble,
    MovPtr,

    LoadFieldBool,
    LoadFieldByte,
    LoadFieldChar,
    LoadFieldInt,
    LoadFieldLong,
    LoadFieldFloat,
    LoadFieldDouble,
    LoadFieldPtr,

    LoadGlobalBool,
    LoadGlobalByte,
    LoadGlobalChar,
    LoadGlobalInt,
    LoadGlobalLong,
    LoadGlobalFloat,
    LoadGlobalDouble,
    LoadGlobalPtr,

    ConstNil,
    ConstTrue,
    ConstFalse,
    ConstZeroByte,
    ConstZeroInt,
    ConstZeroLong,
    ConstZeroFloat,
    ConstZeroDouble,
    ConstChar,
    ConstByte,
    ConstInt,
    ConstLong,
    ConstFloat,
    ConstDouble,
    ConstString,

    TestEqPtr,
    TestNePtr,

    TestEqInt,
    TestNeInt,
    TestGtInt,
    TestGeInt,
    TestLtInt,
    TestLeInt,

    JumpIfFalse,
    JumpIfTrue,
    Jump,

    InvokeDirectVoid,
    InvokeDirectBool,
    InvokeDirectByte,
    InvokeDirectChar,
    InvokeDirectInt,
    InvokeDirectLong,
    InvokeDirectFloat,
    InvokeDirectDouble,
    InvokeDirectPtr,

    InvokeVirtualVoid,
    InvokeVirtualBool,
    InvokeVirtualByte,
    InvokeVirtualChar,
    InvokeVirtualInt,
    InvokeVirtualLong,
    InvokeVirtualFloat,
    InvokeVirtualDouble,
    InvokeVirtualPtr,

    InvokeStaticVoid,
    InvokeStaticBool,
    InvokeStaticByte,
    InvokeStaticChar,
    InvokeStaticInt,
    InvokeStaticLong,
    InvokeStaticFloat,
    InvokeStaticDouble,
    InvokeStaticPtr,

    NewObject,

    Throw,

    RetBool,
    RetByte,
    RetChar,
    RetInt,
    RetLong,
    RetFloat,
    RetDouble,
    RetPtr,

    RetVoid,
}

pub struct BytecodeStreamGenerator {
    data: Vec<u8>,
}

impl BytecodeStreamGenerator {
    pub fn new() -> BytecodeStreamGenerator {
        BytecodeStreamGenerator { data: Vec::new() }
    }

    pub fn emit(&mut self, bytecode: Bytecode) {
        match bytecode {
            Bytecode::AddInt(dest, lhs, rhs) => {
                self.emit_reg3(BytecodeInst::AddInt, dest, lhs, rhs)
            }
            Bytecode::AddLong(dest, lhs, rhs) => {
                self.emit_reg3(BytecodeInst::AddLong, dest, lhs, rhs)
            }
            Bytecode::AddFloat(dest, lhs, rhs) => {
                self.emit_reg3(BytecodeInst::AddFloat, dest, lhs, rhs)
            }
            Bytecode::AddDouble(dest, lhs, rhs) => {
                self.emit_reg3(BytecodeInst::AddDouble, dest, lhs, rhs)
            }

            Bytecode::SubInt(dest, lhs, rhs) => {
                self.emit_reg3(BytecodeInst::SubInt, dest, lhs, rhs)
            }
            Bytecode::NegInt(dest, src) => self.emit_reg2(BytecodeInst::NegInt, dest, src),
            Bytecode::NegLong(dest, src) => self.emit_reg2(BytecodeInst::NegLong, dest, src),
            Bytecode::MulInt(dest, lhs, rhs) => {
                self.emit_reg3(BytecodeInst::MulInt, dest, lhs, rhs)
            }
            Bytecode::DivInt(dest, lhs, rhs) => {
                self.emit_reg3(BytecodeInst::DivInt, dest, lhs, rhs)
            }
            Bytecode::ModInt(dest, lhs, rhs) => {
                self.emit_reg3(BytecodeInst::ModInt, dest, lhs, rhs)
            }
            Bytecode::AndInt(dest, lhs, rhs) => {
                self.emit_reg3(BytecodeInst::AndInt, dest, lhs, rhs)
            }
            Bytecode::OrInt(dest, lhs, rhs) => self.emit_reg3(BytecodeInst::OrInt, dest, lhs, rhs),
            Bytecode::XorInt(dest, lhs, rhs) => {
                self.emit_reg3(BytecodeInst::XorInt, dest, lhs, rhs)
            }
            Bytecode::NotBool(dest, src) => self.emit_reg2(BytecodeInst::NotBool, dest, src),

            Bytecode::ShlInt(dest, lhs, rhs) => {
                self.emit_reg3(BytecodeInst::ShlInt, dest, lhs, rhs)
            }
            Bytecode::ShrInt(dest, lhs, rhs) => {
                self.emit_reg3(BytecodeInst::ShrInt, dest, lhs, rhs)
            }
            Bytecode::SarInt(dest, lhs, rhs) => {
                self.emit_reg3(BytecodeInst::SarInt, dest, lhs, rhs)
            }

            Bytecode::MovBool(dest, src) => self.emit_reg2(BytecodeInst::MovBool, dest, src),
            Bytecode::MovByte(dest, src) => self.emit_reg2(BytecodeInst::MovByte, dest, src),
            Bytecode::MovChar(dest, src) => self.emit_reg2(BytecodeInst::MovChar, dest, src),
            Bytecode::MovInt(dest, src) => self.emit_reg2(BytecodeInst::MovInt, dest, src),
            Bytecode::MovLong(dest, src) => self.emit_reg2(BytecodeInst::MovLong, dest, src),
            Bytecode::MovFloat(dest, src) => self.emit_reg2(BytecodeInst::MovDouble, dest, src),
            Bytecode::MovDouble(dest, src) => self.emit_reg2(BytecodeInst::MovDouble, dest, src),
            Bytecode::MovPtr(dest, src) => self.emit_reg2(BytecodeInst::MovPtr, dest, src),

            Bytecode::LoadFieldBool(dest, obj, cid, fid) => {
                self.emit_load_field(BytecodeInst::LoadFieldBool, dest, obj, cid, fid)
            }
            Bytecode::LoadFieldByte(dest, obj, cid, fid) => {
                self.emit_load_field(BytecodeInst::LoadFieldByte, dest, obj, cid, fid)
            }
            Bytecode::LoadFieldChar(dest, obj, cid, fid) => {
                self.emit_load_field(BytecodeInst::LoadFieldChar, dest, obj, cid, fid)
            }
            Bytecode::LoadFieldInt(dest, obj, cid, fid) => {
                self.emit_load_field(BytecodeInst::LoadFieldInt, dest, obj, cid, fid)
            }
            Bytecode::LoadFieldLong(dest, obj, cid, fid) => {
                self.emit_load_field(BytecodeInst::LoadFieldLong, dest, obj, cid, fid)
            }
            Bytecode::LoadFieldFloat(dest, obj, cid, fid) => {
                self.emit_load_field(BytecodeInst::LoadFieldFloat, dest, obj, cid, fid)
            }
            Bytecode::LoadFieldDouble(dest, obj, cid, fid) => {
                self.emit_load_field(BytecodeInst::LoadFieldDouble, dest, obj, cid, fid)
            }
            Bytecode::LoadFieldPtr(dest, obj, cid, fid) => {
                self.emit_load_field(BytecodeInst::LoadFieldPtr, dest, obj, cid, fid)
            }

            Bytecode::LoadGlobalBool(dest, gid) => {
                self.emit_load_global(BytecodeInst::LoadGlobalBool, dest, gid)
            }
            Bytecode::LoadGlobalByte(dest, gid) => {
                self.emit_load_global(BytecodeInst::LoadGlobalByte, dest, gid)
            }
            Bytecode::LoadGlobalChar(dest, gid) => {
                self.emit_load_global(BytecodeInst::LoadGlobalChar, dest, gid)
            }
            Bytecode::LoadGlobalInt(dest, gid) => {
                self.emit_load_global(BytecodeInst::LoadGlobalInt, dest, gid)
            }
            Bytecode::LoadGlobalLong(dest, gid) => {
                self.emit_load_global(BytecodeInst::LoadGlobalLong, dest, gid)
            }
            Bytecode::LoadGlobalFloat(dest, gid) => {
                self.emit_load_global(BytecodeInst::LoadGlobalFloat, dest, gid)
            }
            Bytecode::LoadGlobalDouble(dest, gid) => {
                self.emit_load_global(BytecodeInst::LoadGlobalDouble, dest, gid)
            }
            Bytecode::LoadGlobalPtr(dest, gid) => {
                self.emit_load_global(BytecodeInst::LoadGlobalPtr, dest, gid)
            }

            Bytecode::ConstNil(dest) => self.emit_reg1(BytecodeInst::ConstNil, dest),
            Bytecode::ConstTrue(dest) => self.emit_reg1(BytecodeInst::ConstTrue, dest),
            Bytecode::ConstFalse(dest) => self.emit_reg1(BytecodeInst::ConstFalse, dest),
            Bytecode::ConstZeroByte(dest) => self.emit_reg1(BytecodeInst::ConstZeroByte, dest),
            Bytecode::ConstZeroInt(dest) => self.emit_reg1(BytecodeInst::ConstZeroInt, dest),
            Bytecode::ConstZeroLong(dest) => self.emit_reg1(BytecodeInst::ConstZeroLong, dest),
            Bytecode::ConstZeroFloat(dest) => self.emit_reg1(BytecodeInst::ConstZeroFloat, dest),
            Bytecode::ConstZeroDouble(dest) => self.emit_reg1(BytecodeInst::ConstZeroDouble, dest),
            Bytecode::ConstChar(_, _) => unimplemented!(),
            Bytecode::ConstByte(_, _) => unimplemented!(),
            Bytecode::ConstInt(_, _) => unimplemented!(),
            Bytecode::ConstLong(_, _) => unimplemented!(),
            Bytecode::ConstFloat(_, _) => unimplemented!(),
            Bytecode::ConstDouble(_, _) => unimplemented!(),
            Bytecode::ConstString(_, _) => unimplemented!(),

            Bytecode::TestEqPtr(dest, lhs, rhs) => {
                self.emit_reg3(BytecodeInst::TestEqPtr, dest, lhs, rhs)
            }
            Bytecode::TestNePtr(dest, lhs, rhs) => {
                self.emit_reg3(BytecodeInst::TestNePtr, dest, lhs, rhs)
            }

            Bytecode::TestEqInt(dest, lhs, rhs) => {
                self.emit_reg3(BytecodeInst::TestEqInt, dest, lhs, rhs)
            }
            Bytecode::TestNeInt(dest, lhs, rhs) => {
                self.emit_reg3(BytecodeInst::TestNeInt, dest, lhs, rhs)
            }
            Bytecode::TestGtInt(dest, lhs, rhs) => {
                self.emit_reg3(BytecodeInst::TestGtInt, dest, lhs, rhs)
            }
            Bytecode::TestGeInt(dest, lhs, rhs) => {
                self.emit_reg3(BytecodeInst::TestGeInt, dest, lhs, rhs)
            }
            Bytecode::TestLtInt(dest, lhs, rhs) => {
                self.emit_reg3(BytecodeInst::TestLtInt, dest, lhs, rhs)
            }
            Bytecode::TestLeInt(dest, lhs, rhs) => {
                self.emit_reg3(BytecodeInst::TestLeInt, dest, lhs, rhs)
            }

            Bytecode::JumpIfFalse(_, _) => unimplemented!(),
            Bytecode::JumpIfTrue(_, _) => unimplemented!(),
            Bytecode::Jump(_) => unimplemented!(),

            Bytecode::InvokeDirectVoid(_, _, _) => unimplemented!(),
            Bytecode::InvokeDirectBool(_, _, _, _) => unimplemented!(),
            Bytecode::InvokeDirectByte(_, _, _, _) => unimplemented!(),
            Bytecode::InvokeDirectChar(_, _, _, _) => unimplemented!(),
            Bytecode::InvokeDirectInt(_, _, _, _) => unimplemented!(),
            Bytecode::InvokeDirectLong(_, _, _, _) => unimplemented!(),
            Bytecode::InvokeDirectFloat(_, _, _, _) => unimplemented!(),
            Bytecode::InvokeDirectDouble(_, _, _, _) => unimplemented!(),
            Bytecode::InvokeDirectPtr(_, _, _, _) => unimplemented!(),

            Bytecode::InvokeVirtualVoid(_, _, _) => unimplemented!(),
            Bytecode::InvokeVirtualBool(_, _, _, _) => unimplemented!(),
            Bytecode::InvokeVirtualByte(_, _, _, _) => unimplemented!(),
            Bytecode::InvokeVirtualChar(_, _, _, _) => unimplemented!(),
            Bytecode::InvokeVirtualInt(_, _, _, _) => unimplemented!(),
            Bytecode::InvokeVirtualLong(_, _, _, _) => unimplemented!(),
            Bytecode::InvokeVirtualFloat(_, _, _, _) => unimplemented!(),
            Bytecode::InvokeVirtualDouble(_, _, _, _) => unimplemented!(),
            Bytecode::InvokeVirtualPtr(_, _, _, _) => unimplemented!(),

            Bytecode::InvokeStaticVoid(_, _, _) => unimplemented!(),
            Bytecode::InvokeStaticBool(_, _, _, _) => unimplemented!(),
            Bytecode::InvokeStaticByte(_, _, _, _) => unimplemented!(),
            Bytecode::InvokeStaticChar(_, _, _, _) => unimplemented!(),
            Bytecode::InvokeStaticInt(_, _, _, _) => unimplemented!(),
            Bytecode::InvokeStaticLong(_, _, _, _) => unimplemented!(),
            Bytecode::InvokeStaticFloat(_, _, _, _) => unimplemented!(),
            Bytecode::InvokeStaticDouble(_, _, _, _) => unimplemented!(),
            Bytecode::InvokeStaticPtr(_, _, _, _) => unimplemented!(),

            Bytecode::NewObject(_, _) => unimplemented!(),

            Bytecode::Throw(src) => self.emit_reg1(BytecodeInst::Throw, src),

            Bytecode::RetBool(src) => self.emit_reg1(BytecodeInst::RetBool, src),
            Bytecode::RetByte(src) => self.emit_reg1(BytecodeInst::RetByte, src),
            Bytecode::RetChar(src) => self.emit_reg1(BytecodeInst::RetChar, src),
            Bytecode::RetInt(src) => self.emit_reg1(BytecodeInst::RetInt, src),
            Bytecode::RetLong(src) => self.emit_reg1(BytecodeInst::RetLong, src),
            Bytecode::RetFloat(src) => self.emit_reg1(BytecodeInst::RetFloat, src),
            Bytecode::RetDouble(src) => self.emit_reg1(BytecodeInst::RetDouble, src),
            Bytecode::RetPtr(src) => self.emit_reg1(BytecodeInst::RetPtr, src),

            Bytecode::RetVoid => self.emit_op(BytecodeInst::RetVoid),
        }
    }

    fn emit_reg3(&mut self, inst: BytecodeInst, r1: Register, r2: Register, r3: Register) {
        let values = [
            inst as u32,
            r1.to_usize() as u32,
            r2.to_usize() as u32,
            r3.to_usize() as u32,
        ];
        self.emit_values(&values);
    }

    fn emit_reg2(&mut self, inst: BytecodeInst, r1: Register, r2: Register) {
        let values = [inst as u32, r1.to_usize() as u32, r2.to_usize() as u32];
        self.emit_values(&values);
    }

    fn emit_reg1(&mut self, inst: BytecodeInst, r1: Register) {
        let values = [inst as u32, r1.to_usize() as u32];
        self.emit_values(&values);
    }

    fn emit_load_field(
        &mut self,
        inst: BytecodeInst,
        r1: Register,
        r2: Register,
        cid: ClassDefId,
        fid: FieldId,
    ) {
        let values = [
            inst as u32,
            r1.to_usize() as u32,
            r2.to_usize() as u32,
            cid.to_usize() as u32,
            fid.to_usize() as u32,
        ];
        self.emit_values(&values);
    }

    fn emit_load_global(&mut self, inst: BytecodeInst, r1: Register, gid: GlobalId) {
        let values = [inst as u32, r1.to_usize() as u32, gid.to_usize() as u32];
        self.emit_values(&values);
    }

    fn emit_op(&mut self, inst: BytecodeInst) {
        let values = [inst as u32];
        self.emit_values(&values);
    }

    fn emit_values(&mut self, values: &[u32]) {
        if is_wide(values) {
            self.emit_wide();
            for &value in values {
                self.emit_u32(value);
            }
        } else {
            for &value in values {
                self.emit_u8(value as u8);
            }
        }
    }

    fn emit_wide(&mut self) {
        self.data.push(BytecodeInst::Wide as u8);
    }

    fn emit_u8(&mut self, value: u8) {
        self.data.push(value);
    }

    fn emit_u32(&mut self, value: u32) {
        self.data.push((value & 0xFF) as u8);
        self.data.push(((value >> 8) & 0xFF) as u8);
        self.data.push(((value >> 16) & 0xFF) as u8);
        self.data.push(((value >> 24) & 0xFF) as u8);
    }

    pub fn finish(self) -> Vec<u8> {
        self.data
    }
}

fn is_wide(values: &[u32]) -> bool {
    values.iter().any(|&val| val > u8::max_value() as u32)
}
