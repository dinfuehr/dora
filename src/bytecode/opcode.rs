use bytecode::generate::{BytecodeIdx, Register};

#[derive(PartialEq, Debug)]
pub enum Bytecode {
    AddInt(Register, Register, Register),
    AddLong(Register, Register, Register),
    AddFloat(Register, Register, Register),
    AddDouble(Register, Register, Register),

    SubInt(Register, Register, Register),
    NegInt(Register, Register),
    NegLong(Register, Register),
    MulInt(Register, Register, Register),
    DivInt(Register, Register, Register),
    ModInt(Register, Register, Register),
    AndInt(Register, Register, Register),
    OrInt(Register, Register, Register),
    XorInt(Register, Register, Register),
    NotBool(Register, Register),

    ShlInt(Register, Register, Register),
    ShrInt(Register, Register, Register),
    SarInt(Register, Register, Register),

    MovBool(Register, Register),
    MovByte(Register, Register),
    MovChar(Register, Register),
    MovInt(Register, Register),
    MovLong(Register, Register),
    MovFloat(Register, Register),
    MovDouble(Register, Register),
    MovPtr(Register, Register),

    ConstTrue(Register),
    ConstFalse(Register),
    ConstZeroInt(Register),
    ConstInt(Register, u32),

    TestEqPtr(Register, Register, Register),
    TestNePtr(Register, Register, Register),

    TestEqInt(Register, Register, Register),
    TestNeInt(Register, Register, Register),
    TestGtInt(Register, Register, Register),
    TestGeInt(Register, Register, Register),
    TestLtInt(Register, Register, Register),
    TestLeInt(Register, Register, Register),

    JumpIfFalse(Register, BytecodeIdx),
    JumpIfTrue(Register, BytecodeIdx),
    Jump(BytecodeIdx),

    RetBool(Register),
    RetByte(Register),
    RetChar(Register),
    RetInt(Register),
    RetLong(Register),
    RetFloat(Register),
    RetDouble(Register),
    RetPtr(Register),

    RetVoid,
}
