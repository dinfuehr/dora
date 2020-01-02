use std::fmt;

use crate::mem::ptr_width;
use crate::ty::{BuiltinType, MachineMode};

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
