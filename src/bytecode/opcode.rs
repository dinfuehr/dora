use bytecode::generate::{BytecodeIdx, Register};

#[derive(PartialEq, Debug)]
pub enum Bytecode {
    AddInt(Register),
    AddLong(Register),
    AddFloat(Register),
    AddDouble(Register),

    SubInt(Register),
    NegInt,
    MulInt(Register),
    DivInt(Register),
    ModInt(Register),
    AndInt(Register),
    OrInt(Register),
    XorInt(Register),
    NotBool,

    ShlInt(Register),
    ShrInt(Register),
    SarInt(Register),

    MovBool(Register, Register),
    MovByte(Register, Register),
    MovChar(Register, Register),
    MovInt(Register, Register),
    MovLong(Register, Register),
    MovFloat(Register, Register),
    MovDouble(Register, Register),
    MovPtr(Register, Register),

    Ldar(Register),
    LdaInt(u64),
    LdaZero,
    LdaTrue,
    LdaFalse,
    Star(Register),

    TestEqInt(Register),
    TestEqPtr(Register),
    TestNeInt(Register),
    TestNePtr(Register),
    TestGtInt(Register),
    TestGeInt(Register),
    TestLtInt(Register),
    TestLeInt(Register),

    JumpIfFalse(BytecodeIdx),
    JumpIfTrue(BytecodeIdx),
    Jump(BytecodeIdx),

    Ret,
    RetVoid,
}
