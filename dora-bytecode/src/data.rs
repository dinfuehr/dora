use bincode::{Decode, Encode};
use std::fmt;

use crate::{
    BytecodeReader, BytecodeTraitType, BytecodeType, BytecodeTypeArray, ClassId, ConstId, EnumId,
    FunctionId, GlobalId, StructId,
};
use num_enum::{IntoPrimitive, TryFromPrimitive};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Decode, Encode)]
pub struct BytecodeOffset(pub u32);

impl BytecodeOffset {
    pub fn to_u32(&self) -> u32 {
        self.0
    }

    pub fn to_usize(&self) -> usize {
        self.0 as usize
    }
}

#[derive(IntoPrimitive, TryFromPrimitive, Copy, Clone, Debug, PartialEq)]
#[repr(u8)]
pub enum BytecodeTypeKind {
    Unit,
    Bool,
    UInt8,
    Char,
    Int32,
    Int64,
    Float32,
    Float64,
    Ptr,
    Tuple,
    Enum,
    Struct,
    TypeParam,
    Class,
    TraitObject,
    Lambda,
    TypeAlias,
    Assoc,
    GenericAssoc,
    This,
}

#[derive(IntoPrimitive, TryFromPrimitive, Copy, Clone, PartialEq, Eq)]
#[repr(u8)]
pub enum BytecodeOpcode {
    Add,
    Sub,
    Neg,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Not,
    Shl,
    Shr,
    Sar,

    Mov,

    LoadTupleElement,
    LoadEnumElement,
    LoadEnumVariant,
    LoadStructField,

    LoadField,
    StoreField,

    LoadGlobal,
    StoreGlobal,

    LoadConst,

    PushRegister,

    ConstTrue,
    ConstFalse,
    ConstUInt8,
    ConstChar,
    ConstInt32,
    ConstInt64,
    ConstFloat32,
    ConstFloat64,
    ConstString,

    TestIdentity,
    TestEq,
    TestNe,
    TestGt,
    TestGe,
    TestLt,
    TestLe,

    // Backward jump
    JumpLoop,
    LoopStart,

    // Forward jumps
    Jump,
    JumpIfFalse,
    JumpIfTrue,
    Switch,

    InvokeDirect,
    InvokeVirtual,
    InvokeStatic,
    InvokeLambda,
    InvokeGenericStatic,
    InvokeGenericDirect,

    NewObject,
    NewObjectInitialized,
    NewArray,
    NewTuple,
    NewEnum,
    NewStruct,
    NewTraitObject,
    NewLambda,

    ArrayLength,

    LoadArray,
    StoreArray,

    LoadTraitObjectValue,

    Ret,
}

impl BytecodeOpcode {
    pub fn is_loop_start(self) -> bool {
        match self {
            BytecodeOpcode::LoopStart => true,
            _ => false,
        }
    }

    pub fn is_new_enum(self) -> bool {
        match self {
            BytecodeOpcode::NewEnum => true,
            _ => false,
        }
    }

    pub fn is_new_struct(self) -> bool {
        match self {
            BytecodeOpcode::NewStruct => true,
            _ => false,
        }
    }

    pub fn is_new_tuple(self) -> bool {
        match self {
            BytecodeOpcode::NewTuple => true,
            _ => false,
        }
    }

    pub fn is_push_register(self) -> bool {
        match self {
            BytecodeOpcode::PushRegister => true,
            _ => false,
        }
    }

    pub fn is_new_object_initialized(self) -> bool {
        match self {
            BytecodeOpcode::NewObjectInitialized => true,
            _ => false,
        }
    }

    pub fn is_new_lambda(self) -> bool {
        match self {
            BytecodeOpcode::NewLambda => true,
            _ => false,
        }
    }

    pub fn is_any_invoke(self) -> bool {
        match self {
            BytecodeOpcode::InvokeDirect
            | BytecodeOpcode::InvokeGenericDirect
            | BytecodeOpcode::InvokeGenericStatic
            | BytecodeOpcode::InvokeStatic
            | BytecodeOpcode::InvokeVirtual
            | BytecodeOpcode::InvokeLambda => true,
            _ => false,
        }
    }

    pub fn needs_location(&self) -> bool {
        match *self {
            BytecodeOpcode::Div
            | BytecodeOpcode::Mod
            | BytecodeOpcode::LoadField
            | BytecodeOpcode::StoreField
            | BytecodeOpcode::InvokeDirect
            | BytecodeOpcode::InvokeVirtual
            | BytecodeOpcode::InvokeStatic
            | BytecodeOpcode::InvokeLambda
            | BytecodeOpcode::InvokeGenericStatic
            | BytecodeOpcode::InvokeGenericDirect
            | BytecodeOpcode::NewObject
            | BytecodeOpcode::NewObjectInitialized
            | BytecodeOpcode::NewArray
            | BytecodeOpcode::NewEnum
            | BytecodeOpcode::NewTuple
            | BytecodeOpcode::NewStruct
            | BytecodeOpcode::NewTraitObject
            | BytecodeOpcode::NewLambda
            | BytecodeOpcode::ArrayLength
            | BytecodeOpcode::LoadArray
            | BytecodeOpcode::StoreArray
            | BytecodeOpcode::LoadEnumElement
            | BytecodeOpcode::LoadEnumVariant
            | BytecodeOpcode::LoadGlobal
            | BytecodeOpcode::Add
            | BytecodeOpcode::Sub
            | BytecodeOpcode::Mul
            | BytecodeOpcode::Neg => true,
            _ => false,
        }
    }
}

pub enum BytecodeInstruction {
    Add {
        dest: Register,
        lhs: Register,
        rhs: Register,
    },

    Sub {
        dest: Register,
        lhs: Register,
        rhs: Register,
    },

    Neg {
        dest: Register,
        src: Register,
    },

    Mul {
        dest: Register,
        lhs: Register,
        rhs: Register,
    },

    Div {
        dest: Register,
        lhs: Register,
        rhs: Register,
    },

    Mod {
        dest: Register,
        lhs: Register,
        rhs: Register,
    },

    And {
        dest: Register,
        lhs: Register,
        rhs: Register,
    },

    Or {
        dest: Register,
        lhs: Register,
        rhs: Register,
    },
    Xor {
        dest: Register,
        lhs: Register,
        rhs: Register,
    },

    Not {
        dest: Register,
        src: Register,
    },

    Shl {
        dest: Register,
        lhs: Register,
        rhs: Register,
    },
    Shr {
        dest: Register,
        lhs: Register,
        rhs: Register,
    },
    Sar {
        dest: Register,
        lhs: Register,
        rhs: Register,
    },

    Mov {
        dest: Register,
        src: Register,
    },

    LoadTupleElement {
        dest: Register,
        src: Register,
        idx: ConstPoolIdx,
    },
    LoadEnumElement {
        dest: Register,
        src: Register,
        idx: ConstPoolIdx,
    },
    LoadEnumVariant {
        dest: Register,
        src: Register,
        idx: ConstPoolIdx,
    },
    LoadStructField {
        dest: Register,
        obj: Register,
        field: ConstPoolIdx,
    },

    LoadField {
        dest: Register,
        obj: Register,
        field: ConstPoolIdx,
    },
    StoreField {
        src: Register,
        obj: Register,
        field: ConstPoolIdx,
    },

    LoadGlobal {
        dest: Register,
        global_id: GlobalId,
    },
    StoreGlobal {
        src: Register,
        global_id: GlobalId,
    },

    LoadConst {
        dest: Register,
        const_id: ConstId,
    },

    PushRegister {
        src: Register,
    },

    ConstTrue {
        dest: Register,
    },
    ConstFalse {
        dest: Register,
    },
    ConstUInt8 {
        dest: Register,
        value: u8,
    },
    ConstChar {
        dest: Register,
        idx: ConstPoolIdx,
    },
    ConstInt32 {
        dest: Register,
        idx: ConstPoolIdx,
    },
    ConstInt64 {
        dest: Register,
        idx: ConstPoolIdx,
    },
    ConstFloat32 {
        dest: Register,
        idx: ConstPoolIdx,
    },
    ConstFloat64 {
        dest: Register,
        idx: ConstPoolIdx,
    },
    ConstString {
        dest: Register,
        idx: ConstPoolIdx,
    },

    TestIdentity {
        dest: Register,
        lhs: Register,
        rhs: Register,
    },

    TestEq {
        dest: Register,
        lhs: Register,
        rhs: Register,
    },
    TestNe {
        dest: Register,
        lhs: Register,
        rhs: Register,
    },
    TestGt {
        dest: Register,
        lhs: Register,
        rhs: Register,
    },
    TestGe {
        dest: Register,
        lhs: Register,
        rhs: Register,
    },
    TestLt {
        dest: Register,
        lhs: Register,
        rhs: Register,
    },
    TestLe {
        dest: Register,
        lhs: Register,
        rhs: Register,
    },

    // Backward jump
    JumpLoop {
        offset: u32,
    },
    LoopStart,

    // Forward jumps
    Jump {
        offset: u32,
    },
    JumpIfFalse {
        opnd: Register,
        offset: u32,
    },
    JumpIfTrue {
        opnd: Register,
        offset: u32,
    },
    Switch {
        opnd: Register,
        idx: ConstPoolIdx,
    },

    InvokeDirect {
        dest: Register,
        fct: ConstPoolIdx,
    },

    InvokeVirtual {
        dest: Register,
        fct: ConstPoolIdx,
    },

    InvokeStatic {
        dest: Register,
        fct: ConstPoolIdx,
    },

    InvokeLambda {
        dest: Register,
        idx: ConstPoolIdx,
    },

    InvokeGenericStatic {
        dest: Register,
        fct: ConstPoolIdx,
    },

    InvokeGenericDirect {
        dest: Register,
        fct: ConstPoolIdx,
    },

    NewObject {
        dest: Register,
        cls: ConstPoolIdx,
    },
    NewObjectInitialized {
        dest: Register,
        cls: ConstPoolIdx,
    },
    NewArray {
        dest: Register,
        length: Register,
        idx: ConstPoolIdx,
    },
    NewTuple {
        dest: Register,
        idx: ConstPoolIdx,
    },
    NewEnum {
        dest: Register,
        idx: ConstPoolIdx,
    },
    NewStruct {
        dest: Register,
        idx: ConstPoolIdx,
    },
    NewTraitObject {
        dest: Register,
        src: Register,
        idx: ConstPoolIdx,
    },
    NewLambda {
        dest: Register,
        idx: ConstPoolIdx,
    },

    ArrayLength {
        dest: Register,
        arr: Register,
    },

    LoadArray {
        dest: Register,
        arr: Register,
        idx: Register,
    },
    StoreArray {
        src: Register,
        arr: Register,
        idx: Register,
    },

    LoadTraitObjectValue {
        dest: Register,
        object: Register,
    },

    Ret {
        opnd: Register,
    },
}

#[derive(Copy, Clone, PartialEq, Debug, Eq, Hash)]
pub struct Register(pub usize);

impl Register {
    pub fn zero() -> Register {
        Register(0)
    }

    pub fn to_usize(&self) -> usize {
        self.0
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Decode, Encode)]
pub struct Location {
    line: u32,
    column: u32,
}

impl Location {
    pub fn new(line: u32, column: u32) -> Location {
        Location { line, column }
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn column(&self) -> u32 {
        self.column
    }
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Clone, Debug, Decode, Encode)]
pub struct BytecodeFunction {
    code: Vec<u8>,
    registers: Vec<BytecodeType>,
    const_pool: Vec<ConstPoolEntry>,
    arguments: u32,
    locations: Vec<(BytecodeOffset, Location)>,
    return_ty: BytecodeType,
}

impl BytecodeFunction {
    pub fn new(
        code: Vec<u8>,
        const_pool: Vec<ConstPoolEntry>,
        registers: Vec<BytecodeType>,
        arguments: u32,
        locations: Vec<(BytecodeOffset, Location)>,
        return_ty: BytecodeType,
    ) -> BytecodeFunction {
        BytecodeFunction {
            code,
            const_pool,
            registers,
            arguments,
            locations,
            return_ty,
        }
    }
    pub fn code(&self) -> &[u8] {
        &self.code
    }

    pub fn registers(&self) -> &[BytecodeType] {
        &self.registers
    }

    pub fn locations(&self) -> &[(BytecodeOffset, Location)] {
        &self.locations
    }

    pub fn register_type(&self, register: Register) -> BytecodeType {
        self.registers
            .get(register.0)
            .expect("register not found")
            .clone()
    }

    pub fn arguments(&self) -> u32 {
        self.arguments
    }

    pub fn return_type(&self) -> &BytecodeType {
        &self.return_ty
    }

    pub fn const_pool_entries(&self) -> &[ConstPoolEntry] {
        &self.const_pool
    }

    pub fn const_pool(&self, idx: ConstPoolIdx) -> &ConstPoolEntry {
        &self.const_pool[idx.0 as usize]
    }

    pub fn offset_location(&self, offset: u32) -> Location {
        let index = self
            .locations
            .binary_search_by_key(&BytecodeOffset(offset), |&(o, _)| o);
        let index = match index {
            Err(index) => index - 1,
            Ok(index) => index,
        };
        self.locations[index].1
    }

    pub fn read_opcode(&self, offset: BytecodeOffset) -> BytecodeOpcode {
        BytecodeReader::read_opcode_at(&self.code, offset.to_usize())
    }
}

#[derive(Copy, Clone, PartialEq, Eq, IntoPrimitive)]
#[repr(u8)]
pub enum ConstPoolOpcode {
    String,
    Float32,
    Float64,
    Int32,
    Int64,
    Char,
    Class,
    Field,
    Fct,
    TraitObjectMethod,
    Generic,
    GenericSelf,
    GenericNew,
    Enum,
    EnumVariant,
    EnumElement,
    Struct,
    StructField,
    TraitObject,
    TupleElement,
    Tuple,
    Lambda,
    JumpTable,
}

#[derive(Clone, Debug, PartialEq, Decode, Encode)]
pub enum ConstPoolEntry {
    String(String),
    Float32(f32),
    Float64(f64),
    Int32(i32),
    Int64(i64),
    Char(char),
    Class(ClassId, BytecodeTypeArray),
    Field(ClassId, BytecodeTypeArray, u32),
    Fct(FunctionId, BytecodeTypeArray),
    TraitObjectMethod(BytecodeType, FunctionId),
    Generic(u32, FunctionId, BytecodeTypeArray, BytecodeTypeArray),
    GenericSelf(FunctionId, BytecodeTypeArray, BytecodeTypeArray),
    GenericNew {
        object_type: BytecodeType,
        trait_ty: BytecodeTraitType,
        fct_id: FunctionId,
        fct_type_params: BytecodeTypeArray,
    },
    Enum(EnumId, BytecodeTypeArray),
    EnumVariant(EnumId, BytecodeTypeArray, u32),
    EnumElement(EnumId, BytecodeTypeArray, u32, u32),
    Struct(StructId, BytecodeTypeArray),
    StructField(StructId, BytecodeTypeArray, u32),
    TraitObject {
        trait_ty: BytecodeType,
        actual_object_ty: BytecodeType,
    },
    TupleElement(BytecodeType, u32),
    Tuple(BytecodeTypeArray),
    Lambda(BytecodeTypeArray, BytecodeType),
    JumpTable {
        targets: Vec<u32>,
        default_target: u32,
    },
}

impl ConstPoolEntry {
    pub fn to_string(&self) -> Option<&str> {
        match self {
            ConstPoolEntry::String(value) => Some(value),
            _ => None,
        }
    }

    pub fn to_float32(&self) -> Option<f32> {
        match self {
            ConstPoolEntry::Float32(value) => Some(*value),
            _ => None,
        }
    }

    pub fn to_float64(&self) -> Option<f64> {
        match self {
            ConstPoolEntry::Float64(value) => Some(*value),
            _ => None,
        }
    }

    pub fn to_int32(&self) -> Option<i32> {
        match self {
            ConstPoolEntry::Int32(value) => Some(*value),
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

    pub fn is_fct(&self) -> bool {
        match self {
            ConstPoolEntry::Fct(_, _) => true,
            _ => false,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ConstPoolIdx(pub u32);
