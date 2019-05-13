use bytecode::generate::{BytecodeIdx, Register};
use class::{ClassDefId, FieldId};
use ctxt::GlobalId;

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

    LoadFieldBool(Register, Register, ClassDefId, FieldId),
    LoadFieldByte(Register, Register, ClassDefId, FieldId),
    LoadFieldChar(Register, Register, ClassDefId, FieldId),
    LoadFieldInt(Register, Register, ClassDefId, FieldId),
    LoadFieldLong(Register, Register, ClassDefId, FieldId),
    LoadFieldFloat(Register, Register, ClassDefId, FieldId),
    LoadFieldDouble(Register, Register, ClassDefId, FieldId),
    LoadFieldPtr(Register, Register, ClassDefId, FieldId),

    LoadGlobalBool(Register, GlobalId),
    LoadGlobalByte(Register, GlobalId),
    LoadGlobalChar(Register, GlobalId),
    LoadGlobalInt(Register, GlobalId),
    LoadGlobalLong(Register, GlobalId),
    LoadGlobalFloat(Register, GlobalId),
    LoadGlobalDouble(Register, GlobalId),
    LoadGlobalPtr(Register, GlobalId),

    ConstTrue(Register),
    ConstFalse(Register),
    ConstZeroByte(Register),
    ConstZeroInt(Register),
    ConstZeroLong(Register),
    ConstByte(Register, u8),
    ConstInt(Register, u32),
    ConstLong(Register, u64),

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
