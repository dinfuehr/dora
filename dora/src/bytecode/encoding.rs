use crate::bytecode::generate::Register;
use crate::bytecode::opcode::Bytecode;

pub enum BytecodeInst {
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

            Bytecode::LoadFieldBool(_, _, _, _) => unimplemented!(),
            Bytecode::LoadFieldByte(_, _, _, _) => unimplemented!(),
            Bytecode::LoadFieldChar(_, _, _, _) => unimplemented!(),
            Bytecode::LoadFieldInt(_, _, _, _) => unimplemented!(),
            Bytecode::LoadFieldLong(_, _, _, _) => unimplemented!(),
            Bytecode::LoadFieldFloat(_, _, _, _) => unimplemented!(),
            Bytecode::LoadFieldDouble(_, _, _, _) => unimplemented!(),
            Bytecode::LoadFieldPtr(_, _, _, _) => unimplemented!(),

            Bytecode::LoadGlobalBool(_, _) => unimplemented!(),
            Bytecode::LoadGlobalByte(_, _) => unimplemented!(),
            Bytecode::LoadGlobalChar(_, _) => unimplemented!(),
            Bytecode::LoadGlobalInt(_, _) => unimplemented!(),
            Bytecode::LoadGlobalLong(_, _) => unimplemented!(),
            Bytecode::LoadGlobalFloat(_, _) => unimplemented!(),
            Bytecode::LoadGlobalDouble(_, _) => unimplemented!(),
            Bytecode::LoadGlobalPtr(_, _) => unimplemented!(),

            Bytecode::ConstNil(_) => unimplemented!(),
            Bytecode::ConstTrue(_) => unimplemented!(),
            Bytecode::ConstFalse(_) => unimplemented!(),
            Bytecode::ConstZeroByte(_) => unimplemented!(),
            Bytecode::ConstZeroInt(_) => unimplemented!(),
            Bytecode::ConstZeroLong(_) => unimplemented!(),
            Bytecode::ConstZeroFloat(_) => unimplemented!(),
            Bytecode::ConstZeroDouble(_) => unimplemented!(),
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

    fn emit_reg3(&mut self, _inst: BytecodeInst, _r1: Register, _r2: Register, _r3: Register) {
        unimplemented!();
    }

    fn emit_reg2(&mut self, _inst: BytecodeInst, _r1: Register, _r2: Register) {
        unimplemented!();
    }

    fn emit_reg1(&mut self, _inst: BytecodeInst, _r1: Register) {
        unimplemented!();
    }

    fn emit_op(&mut self, _inst: BytecodeInst) {
        unimplemented!();
    }

    pub fn finish(self) -> Vec<u8> {
        self.data
    }
}
