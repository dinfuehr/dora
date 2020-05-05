use std::fmt;

use crate::cpu::STACK_FRAME_ALIGNMENT;
use crate::mem::{align_i32, ptr_width};
use crate::ty::{BuiltinType, MachineMode};
use dora_parser::lexer::position::Position;

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
    UInt8,
    Char,
    Int,
    Int32,
    Int64,
    Float,
    Double,
    Ptr,
}

impl BytecodeType {
    pub fn size(&self) -> i32 {
        match self {
            BytecodeType::Bool => 1,
            BytecodeType::UInt8 => 1,
            BytecodeType::Char => 4,
            BytecodeType::Int => 4,
            BytecodeType::Int32 => 4,
            BytecodeType::Int64 => 8,
            BytecodeType::Float => 4,
            BytecodeType::Double => 8,
            BytecodeType::Ptr => ptr_width(),
        }
    }

    pub fn mode(&self) -> MachineMode {
        match self {
            BytecodeType::Bool => MachineMode::Int8,
            BytecodeType::UInt8 => MachineMode::Int8,
            BytecodeType::Char => MachineMode::Int32,
            BytecodeType::Int => MachineMode::Int32,
            BytecodeType::Int32 => MachineMode::Int32,
            BytecodeType::Int64 => MachineMode::Int64,
            BytecodeType::Float => MachineMode::Float32,
            BytecodeType::Double => MachineMode::Float64,
            BytecodeType::Ptr => MachineMode::Ptr,
        }
    }

    pub fn is_ptr(&self) -> bool {
        match self {
            BytecodeType::Ptr => true,
            _ => false,
        }
    }
}

impl From<BuiltinType> for BytecodeType {
    fn from(ty: BuiltinType) -> Self {
        match ty {
            BuiltinType::Bool => BytecodeType::Bool,
            BuiltinType::UInt8 => BytecodeType::UInt8,
            BuiltinType::Char => BytecodeType::Char,
            BuiltinType::Int => BytecodeType::Int,
            BuiltinType::Int32 => BytecodeType::Int32,
            BuiltinType::Int64 => BytecodeType::Int64,
            BuiltinType::Float => BytecodeType::Float,
            BuiltinType::Double => BytecodeType::Double,
            BuiltinType::Class(_, _) => BytecodeType::Ptr,
            BuiltinType::Enum(_, _) => BytecodeType::Int,
            _ => panic!("BuiltinType {:?} cannot converted to BytecodeType", ty),
        }
    }
}

impl From<BytecodeType> for BuiltinType {
    fn from(ty: BytecodeType) -> BuiltinType {
        match ty {
            BytecodeType::Bool => BuiltinType::Bool,
            BytecodeType::UInt8 => BuiltinType::UInt8,
            BytecodeType::Char => BuiltinType::Char,
            BytecodeType::Int => BuiltinType::Int,
            BytecodeType::Int32 => BuiltinType::Int32,
            BytecodeType::Int64 => BuiltinType::Int64,
            BytecodeType::Float => BuiltinType::Float,
            BytecodeType::Double => BuiltinType::Double,
            BytecodeType::Ptr => BuiltinType::Ptr,
        }
    }
}

// Keep in sync with dora-boots/bytecode.dora

#[derive(Copy, Clone, PartialEq, Eq, FromPrimitive, ToPrimitive)]
pub enum BytecodeOpcode {
    Wide,

    AddInt,
    AddInt32,
    AddInt64,
    AddFloat,
    AddDouble,

    SubInt,
    SubInt32,
    SubInt64,
    SubFloat,
    SubDouble,

    NegInt,
    NegInt32,
    NegInt64,
    NegFloat,
    NegDouble,

    MulInt,
    MulInt32,
    MulInt64,
    MulFloat,
    MulDouble,

    DivInt,
    DivInt32,
    DivInt64,
    DivFloat,
    DivDouble,

    ModInt,
    ModInt32,
    ModInt64,

    AndInt,
    AndInt32,
    AndInt64,

    OrInt,
    OrInt32,
    OrInt64,

    XorInt,
    XorInt32,
    XorInt64,

    NotBool,
    NotInt,
    NotInt32,
    NotInt64,

    ShlInt,
    ShrInt,
    SarInt,

    ShlInt32,
    ShrInt32,
    SarInt32,

    ShlInt64,
    ShrInt64,
    SarInt64,

    RolInt,
    RorInt,

    RolInt32,
    RorInt32,

    RolInt64,
    RorInt64,

    ReinterpretFloatAsInt,
    ReinterpretIntAsFloat,
    ReinterpretDoubleAsInt64,
    ReinterpretInt64AsDouble,

    ExtendUInt8ToChar,
    ExtendUInt8ToInt,
    ExtendUInt8ToInt64,
    ExtendIntToInt64,
    ExtendCharToInt64,
    CastCharToInt,
    CastIntToUInt8,
    CastIntToChar,
    CastIntToInt32,
    CastInt64ToUInt8,
    CastInt64ToChar,
    CastInt64ToInt,

    ConvertIntToFloat,
    ConvertIntToDouble,
    ConvertInt64ToFloat,
    ConvertInt64ToDouble,

    TruncateFloatToInt,
    TruncateFloatToInt64,
    TruncateDoubleToInt,
    TruncateDoubleToInt64,

    InstanceOf,
    CheckedCast,

    MovBool,
    MovUInt8,
    MovChar,
    MovInt,
    MovInt32,
    MovInt64,
    MovFloat,
    MovDouble,
    MovPtr,
    MovTuple,

    LoadTupleElement,
    StoreTupleElement,

    LoadFieldBool,
    LoadFieldUInt8,
    LoadFieldChar,
    LoadFieldInt,
    LoadFieldInt32,
    LoadFieldInt64,
    LoadFieldFloat,
    LoadFieldDouble,
    LoadFieldPtr,

    StoreFieldBool,
    StoreFieldUInt8,
    StoreFieldChar,
    StoreFieldInt,
    StoreFieldInt32,
    StoreFieldInt64,
    StoreFieldFloat,
    StoreFieldDouble,
    StoreFieldPtr,

    LoadGlobalBool,
    LoadGlobalUInt8,
    LoadGlobalChar,
    LoadGlobalInt,
    LoadGlobalInt32,
    LoadGlobalInt64,
    LoadGlobalFloat,
    LoadGlobalDouble,
    LoadGlobalPtr,

    StoreGlobalBool,
    StoreGlobalUInt8,
    StoreGlobalChar,
    StoreGlobalInt,
    StoreGlobalInt32,
    StoreGlobalInt64,
    StoreGlobalFloat,
    StoreGlobalDouble,
    StoreGlobalPtr,

    PushRegister,

    ConstNil,
    ConstTrue,
    ConstFalse,
    ConstZeroUInt8,
    ConstZeroChar,
    ConstZeroInt,
    ConstZeroInt32,
    ConstZeroInt64,
    ConstZeroFloat,
    ConstZeroDouble,
    ConstUInt8,
    ConstChar,
    ConstInt,
    ConstInt32,
    ConstInt64,
    ConstFloat,
    ConstDouble,
    ConstString,

    TestEqPtr,
    TestNePtr,

    TestEqBool,
    TestNeBool,

    TestEqUInt8,
    TestNeUInt8,
    TestGtUInt8,
    TestGeUInt8,
    TestLtUInt8,
    TestLeUInt8,

    TestEqChar,
    TestNeChar,
    TestGtChar,
    TestGeChar,
    TestLtChar,
    TestLeChar,

    TestEqEnum,
    TestNeEnum,

    TestEqInt,
    TestNeInt,
    TestGtInt,
    TestGeInt,
    TestLtInt,
    TestLeInt,

    TestEqInt32,
    TestNeInt32,
    TestGtInt32,
    TestGeInt32,
    TestLtInt32,
    TestLeInt32,

    TestEqInt64,
    TestNeInt64,
    TestGtInt64,
    TestGeInt64,
    TestLtInt64,
    TestLeInt64,

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

    Assert,

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
    InvokeDirectUInt8,
    InvokeDirectChar,
    InvokeDirectInt,
    InvokeDirectInt32,
    InvokeDirectInt64,
    InvokeDirectFloat,
    InvokeDirectDouble,
    InvokeDirectPtr,

    InvokeVirtualVoid,
    InvokeVirtualBool,
    InvokeVirtualUInt8,
    InvokeVirtualChar,
    InvokeVirtualInt,
    InvokeVirtualInt32,
    InvokeVirtualInt64,
    InvokeVirtualFloat,
    InvokeVirtualDouble,
    InvokeVirtualPtr,

    InvokeStaticVoid,
    InvokeStaticBool,
    InvokeStaticUInt8,
    InvokeStaticChar,
    InvokeStaticInt,
    InvokeStaticInt32,
    InvokeStaticInt64,
    InvokeStaticFloat,
    InvokeStaticDouble,
    InvokeStaticPtr,

    NewObject,
    NewArray,

    NilCheck,

    ArrayLength,
    ArrayBoundCheck,

    LoadArrayBool,
    LoadArrayUInt8,
    LoadArrayChar,
    LoadArrayInt,
    LoadArrayInt32,
    LoadArrayInt64,
    LoadArrayFloat,
    LoadArrayDouble,
    LoadArrayPtr,

    StoreArrayBool,
    StoreArrayUInt8,
    StoreArrayChar,
    StoreArrayInt,
    StoreArrayInt32,
    StoreArrayInt64,
    StoreArrayFloat,
    StoreArrayDouble,
    StoreArrayPtr,

    RetVoid,
    RetBool,
    RetUInt8,
    RetChar,
    RetInt,
    RetInt32,
    RetInt64,
    RetFloat,
    RetDouble,
    RetPtr,
}

impl BytecodeOpcode {
    pub fn need_position(&self) -> bool {
        match *self {
            BytecodeOpcode::DivInt
            | BytecodeOpcode::DivInt32
            | BytecodeOpcode::DivInt64
            | BytecodeOpcode::ModInt
            | BytecodeOpcode::ModInt32
            | BytecodeOpcode::ModInt64
            | BytecodeOpcode::InstanceOf
            | BytecodeOpcode::CheckedCast
            | BytecodeOpcode::LoadFieldBool
            | BytecodeOpcode::LoadFieldUInt8
            | BytecodeOpcode::LoadFieldChar
            | BytecodeOpcode::LoadFieldInt
            | BytecodeOpcode::LoadFieldInt32
            | BytecodeOpcode::LoadFieldInt64
            | BytecodeOpcode::LoadFieldFloat
            | BytecodeOpcode::LoadFieldDouble
            | BytecodeOpcode::LoadFieldPtr
            | BytecodeOpcode::StoreFieldBool
            | BytecodeOpcode::StoreFieldUInt8
            | BytecodeOpcode::StoreFieldChar
            | BytecodeOpcode::StoreFieldInt
            | BytecodeOpcode::StoreFieldInt32
            | BytecodeOpcode::StoreFieldInt64
            | BytecodeOpcode::StoreFieldFloat
            | BytecodeOpcode::StoreFieldDouble
            | BytecodeOpcode::StoreFieldPtr
            | BytecodeOpcode::InvokeDirectVoid
            | BytecodeOpcode::InvokeDirectBool
            | BytecodeOpcode::InvokeDirectUInt8
            | BytecodeOpcode::InvokeDirectChar
            | BytecodeOpcode::InvokeDirectInt
            | BytecodeOpcode::InvokeDirectInt32
            | BytecodeOpcode::InvokeDirectInt64
            | BytecodeOpcode::InvokeDirectFloat
            | BytecodeOpcode::InvokeDirectDouble
            | BytecodeOpcode::InvokeDirectPtr
            | BytecodeOpcode::InvokeVirtualVoid
            | BytecodeOpcode::InvokeVirtualBool
            | BytecodeOpcode::InvokeVirtualUInt8
            | BytecodeOpcode::InvokeVirtualChar
            | BytecodeOpcode::InvokeVirtualInt
            | BytecodeOpcode::InvokeVirtualInt32
            | BytecodeOpcode::InvokeVirtualInt64
            | BytecodeOpcode::InvokeVirtualFloat
            | BytecodeOpcode::InvokeVirtualDouble
            | BytecodeOpcode::InvokeVirtualPtr
            | BytecodeOpcode::InvokeStaticVoid
            | BytecodeOpcode::InvokeStaticBool
            | BytecodeOpcode::InvokeStaticUInt8
            | BytecodeOpcode::InvokeStaticChar
            | BytecodeOpcode::InvokeStaticInt
            | BytecodeOpcode::InvokeStaticInt32
            | BytecodeOpcode::InvokeStaticInt64
            | BytecodeOpcode::InvokeStaticFloat
            | BytecodeOpcode::InvokeStaticDouble
            | BytecodeOpcode::InvokeStaticPtr
            | BytecodeOpcode::NewObject
            | BytecodeOpcode::NewArray
            | BytecodeOpcode::NilCheck
            | BytecodeOpcode::ArrayLength
            | BytecodeOpcode::ArrayBoundCheck
            | BytecodeOpcode::LoadArrayBool
            | BytecodeOpcode::LoadArrayUInt8
            | BytecodeOpcode::LoadArrayChar
            | BytecodeOpcode::LoadArrayInt
            | BytecodeOpcode::LoadArrayInt32
            | BytecodeOpcode::LoadArrayInt64
            | BytecodeOpcode::LoadArrayFloat
            | BytecodeOpcode::LoadArrayDouble
            | BytecodeOpcode::LoadArrayPtr
            | BytecodeOpcode::StoreArrayBool
            | BytecodeOpcode::StoreArrayUInt8
            | BytecodeOpcode::StoreArrayChar
            | BytecodeOpcode::StoreArrayInt
            | BytecodeOpcode::StoreArrayInt32
            | BytecodeOpcode::StoreArrayInt64
            | BytecodeOpcode::StoreArrayFloat
            | BytecodeOpcode::StoreArrayDouble
            | BytecodeOpcode::StoreArrayPtr
            | BytecodeOpcode::Assert => true,
            _ => false,
        }
    }
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

pub struct BytecodeFunction {
    code: Vec<u8>,
    registers: Vec<BytecodeType>,
    const_pool: Vec<ConstPoolEntry>,
    offset: Vec<i32>,
    stacksize: i32,
    arguments: u32,
    positions: Vec<(u32, Position)>,
}

impl BytecodeFunction {
    pub fn new(
        code: Vec<u8>,
        const_pool: Vec<ConstPoolEntry>,
        registers: Vec<BytecodeType>,
        arguments: u32,
        positions: Vec<(u32, Position)>,
    ) -> BytecodeFunction {
        let (offset, stacksize) = determine_offsets(&registers);
        BytecodeFunction {
            code,
            const_pool,
            registers,
            offset,
            stacksize,
            arguments,
            positions,
        }
    }
    pub fn code(&self) -> &[u8] {
        &self.code
    }

    pub fn registers(&self) -> &[BytecodeType] {
        &self.registers
    }

    pub fn positions(&self) -> &[(u32, Position)] {
        &self.positions
    }

    pub fn register_type(&self, register: Register) -> BytecodeType {
        *self.registers.get(register.0).expect("register not found")
    }

    pub fn register_offset(&self, register: Register) -> i32 {
        *self.offset.get(register.0).expect("offset not found")
    }

    pub fn stacksize(&self) -> i32 {
        self.stacksize
    }

    pub fn arguments(&self) -> u32 {
        self.arguments
    }

    pub fn const_pool_entries(&self) -> &[ConstPoolEntry] {
        &self.const_pool
    }

    pub fn const_pool(&self, idx: ConstPoolIdx) -> &ConstPoolEntry {
        &self.const_pool[idx.to_usize()]
    }

    pub fn offset_position(&self, offset: u32) -> Position {
        let index = self.positions.binary_search_by_key(&offset, |&(o, _)| o);
        let index = match index {
            Err(index) => index - 1,
            Ok(index) => index,
        };
        self.positions[index].1
    }
}

fn determine_offsets(registers: &Vec<BytecodeType>) -> (Vec<i32>, i32) {
    let mut offset: Vec<i32> = vec![0; registers.len()];
    let mut stacksize: i32 = 0;
    for (index, ty) in registers.iter().enumerate() {
        stacksize = align_i32(stacksize + ty.size(), ty.size());
        offset[index] = -stacksize;
    }

    stacksize = align_i32(stacksize, STACK_FRAME_ALIGNMENT as i32);

    (offset, stacksize)
}

#[derive(FromPrimitive, ToPrimitive)]
pub enum ConstPoolOpcode {
    String,
    Float,
    Double,
    Int,
    Int64,
    Char,
}

pub enum ConstPoolEntry {
    String(String),
    Float(f32),
    Double(f64),
    Int(i32),
    Int64(i64),
    Char(char),
}

impl ConstPoolEntry {
    pub fn to_string(&self) -> Option<&str> {
        match self {
            ConstPoolEntry::String(ref value) => Some(value),
            _ => None,
        }
    }

    pub fn to_float(&self) -> Option<f32> {
        match self {
            ConstPoolEntry::Float(value) => Some(*value),
            _ => None,
        }
    }

    pub fn to_double(&self) -> Option<f64> {
        match self {
            ConstPoolEntry::Double(value) => Some(*value),
            _ => None,
        }
    }

    pub fn to_int(&self) -> Option<i32> {
        match self {
            ConstPoolEntry::Int(value) => Some(*value),
            _ => None,
        }
    }

    pub fn to_int64(&self) -> Option<i64> {
        match self {
            ConstPoolEntry::Int64(value) => Some(*value),
            _ => None,
        }
    }

    pub fn to_char(&self) -> Option<char> {
        match self {
            ConstPoolEntry::Char(value) => Some(*value),
            _ => None,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ConstPoolIdx(pub usize);

impl ConstPoolIdx {
    pub fn to_usize(self) -> usize {
        self.0
    }
}

impl From<usize> for ConstPoolIdx {
    fn from(value: usize) -> Self {
        ConstPoolIdx(value)
    }
}
