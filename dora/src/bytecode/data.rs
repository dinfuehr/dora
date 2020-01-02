use std::fmt;

use crate::mem::ptr_width;
use crate::ty::{BuiltinType, MachineMode};
use crate::vm::{ClassDefId, FctId, FieldId, GlobalId};

#[derive(PartialEq, Debug)]
pub enum Bytecode {
    AddInt(Register, Register, Register),
    AddLong(Register, Register, Register),
    AddFloat(Register, Register, Register),
    AddDouble(Register, Register, Register),

    SubInt(Register, Register, Register),
    SubLong(Register, Register, Register),
    SubFloat(Register, Register, Register),
    SubDouble(Register, Register, Register),

    NegInt(Register, Register),
    NegLong(Register, Register),
    NegFloat(Register, Register, Register),
    NegDouble(Register, Register, Register),

    MulInt(Register, Register, Register),
    MulLong(Register, Register, Register),
    MulFloat(Register, Register, Register),
    MulDouble(Register, Register, Register),

    DivInt(Register, Register, Register),
    DivLong(Register, Register, Register),
    DivFloat(Register, Register, Register),
    DivDouble(Register, Register, Register),

    ModInt(Register, Register, Register),
    ModLong(Register, Register, Register),

    AndInt(Register, Register, Register),
    AndLong(Register, Register, Register),
    OrInt(Register, Register, Register),
    OrLong(Register, Register, Register),
    XorInt(Register, Register, Register),
    XorLong(Register, Register, Register),
    NotBool(Register, Register),
    NotInt(Register, Register),
    NotLong(Register, Register),

    ShlInt(Register, Register, Register),
    ShrInt(Register, Register, Register),
    SarInt(Register, Register, Register),

    ShlLong(Register, Register, Register),
    ShrLong(Register, Register, Register),
    SarLong(Register, Register, Register),

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

    StoreFieldBool(Register, Register, ClassDefId, FieldId),
    StoreFieldByte(Register, Register, ClassDefId, FieldId),
    StoreFieldChar(Register, Register, ClassDefId, FieldId),
    StoreFieldInt(Register, Register, ClassDefId, FieldId),
    StoreFieldLong(Register, Register, ClassDefId, FieldId),
    StoreFieldFloat(Register, Register, ClassDefId, FieldId),
    StoreFieldDouble(Register, Register, ClassDefId, FieldId),
    StoreFieldPtr(Register, Register, ClassDefId, FieldId),

    LoadGlobalBool(Register, GlobalId),
    LoadGlobalByte(Register, GlobalId),
    LoadGlobalChar(Register, GlobalId),
    LoadGlobalInt(Register, GlobalId),
    LoadGlobalLong(Register, GlobalId),
    LoadGlobalFloat(Register, GlobalId),
    LoadGlobalDouble(Register, GlobalId),
    LoadGlobalPtr(Register, GlobalId),

    StoreGlobalBool(Register, GlobalId),
    StoreGlobalByte(Register, GlobalId),
    StoreGlobalChar(Register, GlobalId),
    StoreGlobalInt(Register, GlobalId),
    StoreGlobalLong(Register, GlobalId),
    StoreGlobalFloat(Register, GlobalId),
    StoreGlobalDouble(Register, GlobalId),
    StoreGlobalPtr(Register, GlobalId),

    ConstNil(Register),
    ConstTrue(Register),
    ConstFalse(Register),
    ConstZeroByte(Register),
    ConstZeroChar(Register),
    ConstZeroInt(Register),
    ConstZeroLong(Register),
    ConstZeroFloat(Register),
    ConstZeroDouble(Register),
    ConstByte(Register, u8),
    ConstChar(Register, char),
    ConstInt(Register, i32),
    ConstLong(Register, i64),
    ConstFloat(Register, f32),
    ConstDouble(Register, f64),
    ConstString(Register, String),

    TestEqPtr(Register, Register, Register),
    TestNePtr(Register, Register, Register),

    TestEqInt(Register, Register, Register),
    TestNeInt(Register, Register, Register),
    TestGtInt(Register, Register, Register),
    TestGeInt(Register, Register, Register),
    TestLtInt(Register, Register, Register),
    TestLeInt(Register, Register, Register),

    TestEqLong(Register, Register, Register),
    TestNeLong(Register, Register, Register),
    TestGtLong(Register, Register, Register),
    TestGeLong(Register, Register, Register),
    TestLtLong(Register, Register, Register),
    TestLeLong(Register, Register, Register),

    TestEqFloat(Register, Register, Register),
    TestNeFloat(Register, Register, Register),
    TestGtFloat(Register, Register, Register),
    TestGeFloat(Register, Register, Register),
    TestLtFloat(Register, Register, Register),
    TestLeFloat(Register, Register, Register),

    TestEqDouble(Register, Register, Register),
    TestNeDouble(Register, Register, Register),
    TestGtDouble(Register, Register, Register),
    TestGeDouble(Register, Register, Register),
    TestLtDouble(Register, Register, Register),
    TestLeDouble(Register, Register, Register),

    JumpLoop(BytecodeIdx),
    Jump(BytecodeIdx),
    JumpIfFalse(Register, BytecodeIdx),
    JumpIfTrue(Register, BytecodeIdx),

    InvokeDirectVoid(FctId, Register, usize),
    InvokeDirectBool(Register, FctId, Register, usize),
    InvokeDirectByte(Register, FctId, Register, usize),
    InvokeDirectChar(Register, FctId, Register, usize),
    InvokeDirectInt(Register, FctId, Register, usize),
    InvokeDirectLong(Register, FctId, Register, usize),
    InvokeDirectFloat(Register, FctId, Register, usize),
    InvokeDirectDouble(Register, FctId, Register, usize),
    InvokeDirectPtr(Register, FctId, Register, usize),

    InvokeVirtualVoid(FctId, Register, usize),
    InvokeVirtualBool(Register, FctId, Register, usize),
    InvokeVirtualByte(Register, FctId, Register, usize),
    InvokeVirtualChar(Register, FctId, Register, usize),
    InvokeVirtualInt(Register, FctId, Register, usize),
    InvokeVirtualLong(Register, FctId, Register, usize),
    InvokeVirtualFloat(Register, FctId, Register, usize),
    InvokeVirtualDouble(Register, FctId, Register, usize),
    InvokeVirtualPtr(Register, FctId, Register, usize),

    InvokeStaticVoid(FctId, Register, usize),
    InvokeStaticBool(Register, FctId, Register, usize),
    InvokeStaticByte(Register, FctId, Register, usize),
    InvokeStaticChar(Register, FctId, Register, usize),
    InvokeStaticInt(Register, FctId, Register, usize),
    InvokeStaticLong(Register, FctId, Register, usize),
    InvokeStaticFloat(Register, FctId, Register, usize),
    InvokeStaticDouble(Register, FctId, Register, usize),
    InvokeStaticPtr(Register, FctId, Register, usize),

    NewObject(Register, ClassDefId),

    Throw(Register),

    RetVoid,
    RetBool(Register),
    RetByte(Register),
    RetChar(Register),
    RetInt(Register),
    RetLong(Register),
    RetFloat(Register),
    RetDouble(Register),
    RetPtr(Register),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct BytecodeIdx(pub usize);

impl BytecodeIdx {
    pub fn invalid() -> BytecodeIdx {
        BytecodeIdx(usize::max_value())
    }

    pub fn is_invalid(&self) -> bool {
        self.0 == usize::max_value()
    }
}

impl fmt::Display for BytecodeIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "bc#{}", self.0)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BytecodeOffset(pub u32);

impl BytecodeOffset {
    pub fn to_u32(&self) -> u32 {
        self.0
    }

    pub fn to_usize(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BytecodeType {
    Bool,
    Byte,
    Char,
    Int,
    Long,
    Float,
    Double,
    Ptr,
}

impl BytecodeType {
    pub fn size(&self) -> i32 {
        match self {
            BytecodeType::Bool => 1,
            BytecodeType::Byte => 1,
            BytecodeType::Char => 4,
            BytecodeType::Int => 4,
            BytecodeType::Long => 8,
            BytecodeType::Float => 4,
            BytecodeType::Double => 8,
            BytecodeType::Ptr => ptr_width(),
        }
    }

    pub fn mode(&self) -> MachineMode {
        match self {
            BytecodeType::Bool => MachineMode::Int8,
            BytecodeType::Byte => MachineMode::Int8,
            BytecodeType::Char => MachineMode::Int32,
            BytecodeType::Int => MachineMode::Int32,
            BytecodeType::Long => MachineMode::Int64,
            BytecodeType::Float => MachineMode::Float32,
            BytecodeType::Double => MachineMode::Float64,
            BytecodeType::Ptr => MachineMode::Ptr,
        }
    }
}

impl From<BuiltinType> for BytecodeType {
    fn from(ty: BuiltinType) -> Self {
        match ty {
            BuiltinType::Bool => BytecodeType::Bool,
            BuiltinType::Byte => BytecodeType::Byte,
            BuiltinType::Char => BytecodeType::Char,
            BuiltinType::Int => BytecodeType::Int,
            BuiltinType::Long => BytecodeType::Long,
            BuiltinType::Float => BytecodeType::Float,
            BuiltinType::Double => BytecodeType::Double,
            BuiltinType::Class(_, _) => BytecodeType::Ptr,
            _ => panic!("BuiltinType cannot converted to BytecodeType"),
        }
    }
}

// Keep in sync with dora-boots/bytecode.dora

#[derive(Copy, Clone, PartialEq, Eq, FromPrimitive, ToPrimitive)]
pub enum BytecodeOpcode {
    Wide,

    AddInt,
    AddLong,
    AddFloat,
    AddDouble,

    SubInt,
    SubLong,
    SubFloat,
    SubDouble,

    NegInt,
    NegLong,
    NegFloat,
    NegDouble,

    MulInt,
    MulLong,
    MulFloat,
    MulDouble,

    DivInt,
    DivLong,
    DivFloat,
    DivDouble,

    ModInt,
    ModLong,

    AndInt,
    AndLong,

    OrInt,
    OrLong,

    XorInt,
    XorLong,

    NotBool,
    NotInt,
    NotLong,

    ShlInt,
    ShrInt,
    SarInt,

    ShlLong,
    ShrLong,
    SarLong,

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

    StoreFieldBool,
    StoreFieldByte,
    StoreFieldChar,
    StoreFieldInt,
    StoreFieldLong,
    StoreFieldFloat,
    StoreFieldDouble,
    StoreFieldPtr,

    LoadGlobalBool,
    LoadGlobalByte,
    LoadGlobalChar,
    LoadGlobalInt,
    LoadGlobalLong,
    LoadGlobalFloat,
    LoadGlobalDouble,
    LoadGlobalPtr,

    StoreGlobalBool,
    StoreGlobalByte,
    StoreGlobalChar,
    StoreGlobalInt,
    StoreGlobalLong,
    StoreGlobalFloat,
    StoreGlobalDouble,
    StoreGlobalPtr,

    ConstNil,
    ConstTrue,
    ConstFalse,
    ConstZeroByte,
    ConstZeroChar,
    ConstZeroInt,
    ConstZeroLong,
    ConstZeroFloat,
    ConstZeroDouble,
    ConstByte,
    ConstChar,
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

    TestEqLong,
    TestNeLong,
    TestGtLong,
    TestGeLong,
    TestLtLong,
    TestLeLong,

    TestEqFloat,
    TestNeFloat,
    TestGtFloat,
    TestGeFloat,
    TestLtFloat,
    TestLeFloat,

    TestEqDouble,
    TestNeDouble,
    TestGtDouble,
    TestGeDouble,
    TestLtDouble,
    TestLeDouble,

    // Backward jump
    JumpLoop,

    // Forward jumps
    Jump,
    JumpConst,
    JumpIfFalse,
    JumpIfFalseConst,
    JumpIfTrue,
    JumpIfTrueConst,

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

    RetVoid,
    RetBool,
    RetByte,
    RetChar,
    RetInt,
    RetLong,
    RetFloat,
    RetDouble,
    RetPtr,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Register(pub usize);

impl Register {
    pub fn invalid() -> Register {
        Register(usize::max_value())
    }

    pub fn zero() -> Register {
        Register(0)
    }

    pub fn to_usize(&self) -> usize {
        self.0
    }

    pub fn is_invalid(&self) -> bool {
        self.0 == usize::max_value()
    }

    pub fn offset(&self, value: usize) -> Register {
        Register(self.0 + value)
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "r{}", self.0)
    }
}
