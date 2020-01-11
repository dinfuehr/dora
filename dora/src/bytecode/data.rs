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

    pub fn const_pool(&self, idx: ConstPoolIdx) -> &ConstPoolEntry {
        &self.const_pool[idx.to_usize()]
    }

    pub fn offset_position(&self, offset: u32) -> Position {
        let index = self
            .positions
            .binary_search_by_key(&offset, |&(o, _)| o)
            .expect("position not found");
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

pub enum ConstPoolEntry {
    String(String),
    Float32(f32),
    Float64(f64),
    Int(i32),
    Long(i64),
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
            ConstPoolEntry::Float32(value) => Some(*value),
            _ => None,
        }
    }

    pub fn to_double(&self) -> Option<f64> {
        match self {
            ConstPoolEntry::Float64(value) => Some(*value),
            _ => None,
        }
    }

    pub fn to_int(&self) -> Option<i32> {
        match self {
            ConstPoolEntry::Int(value) => Some(*value),
            _ => None,
        }
    }

    pub fn to_long(&self) -> Option<i64> {
        match self {
            ConstPoolEntry::Long(value) => Some(*value),
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
