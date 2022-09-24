use std::fmt;

use crate::bytecode::{BytecodeReader, BytecodeType};
use crate::language::sem_analysis::{
    ClassDefinitionId, EnumDefinitionId, FctDefinitionId, FieldId, GlobalDefinitionId,
    StructDefinitionFieldId, StructDefinitionId, TraitDefinitionId, TypeParamId,
};
use crate::language::ty::{SourceType, SourceTypeArray};
use crate::utils::enumeration;
use crate::vm::ClassInstanceId;
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
    Trait,
    Lambda,
}

enumeration!(BytecodeOpcode {
    Wide,

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
    JumpConst,
    JumpIfFalse,
    JumpIfFalseConst,
    JumpIfTrue,
    JumpIfTrueConst,

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

    StringLength,
    LoadStringUInt8,

    Ret
});

fn opcode_size(width: OperandWidth) -> u32 {
    match width {
        OperandWidth::Normal => 1,
        OperandWidth::Wide => 2,
    }
}

fn operand_size(width: OperandWidth) -> u32 {
    match width {
        OperandWidth::Normal => 1,
        OperandWidth::Wide => 4,
    }
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

    pub fn size(self, width: OperandWidth) -> u32 {
        match self {
            BytecodeOpcode::Wide => unreachable!(),

            BytecodeOpcode::LoopStart => opcode_size(width),

            BytecodeOpcode::PushRegister
            | BytecodeOpcode::ConstTrue
            | BytecodeOpcode::ConstFalse
            | BytecodeOpcode::Ret
            | BytecodeOpcode::JumpConst
            | BytecodeOpcode::Jump
            | BytecodeOpcode::JumpLoop => opcode_size(width) + 1 * operand_size(width),

            BytecodeOpcode::Neg
            | BytecodeOpcode::Not
            | BytecodeOpcode::Mov
            | BytecodeOpcode::LoadGlobal
            | BytecodeOpcode::StoreGlobal
            | BytecodeOpcode::ConstChar
            | BytecodeOpcode::ConstInt32
            | BytecodeOpcode::ConstInt64
            | BytecodeOpcode::ConstFloat32
            | BytecodeOpcode::ConstFloat64
            | BytecodeOpcode::ConstString
            | BytecodeOpcode::ArrayLength
            | BytecodeOpcode::NewObject
            | BytecodeOpcode::NewTuple
            | BytecodeOpcode::NewEnum
            | BytecodeOpcode::NewStruct
            | BytecodeOpcode::InvokeGenericDirect
            | BytecodeOpcode::InvokeGenericStatic
            | BytecodeOpcode::InvokeStatic
            | BytecodeOpcode::InvokeDirect
            | BytecodeOpcode::InvokeVirtual
            | BytecodeOpcode::JumpIfTrueConst
            | BytecodeOpcode::JumpIfTrue
            | BytecodeOpcode::JumpIfFalseConst
            | BytecodeOpcode::JumpIfFalse => opcode_size(width) + 2 * operand_size(width),

            BytecodeOpcode::Add
            | BytecodeOpcode::Sub
            | BytecodeOpcode::Mul
            | BytecodeOpcode::Div
            | BytecodeOpcode::Mod
            | BytecodeOpcode::And
            | BytecodeOpcode::Or
            | BytecodeOpcode::Xor
            | BytecodeOpcode::Shl
            | BytecodeOpcode::Shr
            | BytecodeOpcode::Sar
            | BytecodeOpcode::LoadEnumVariant
            | BytecodeOpcode::LoadStructField
            | BytecodeOpcode::LoadField
            | BytecodeOpcode::StoreField
            | BytecodeOpcode::TestEq
            | BytecodeOpcode::TestNe
            | BytecodeOpcode::TestGt
            | BytecodeOpcode::TestGe
            | BytecodeOpcode::TestLt
            | BytecodeOpcode::TestLe
            | BytecodeOpcode::LoadArray
            | BytecodeOpcode::StoreArray
            | BytecodeOpcode::NewArray
            | BytecodeOpcode::NewTraitObject => opcode_size(width) + 3 * operand_size(width),

            BytecodeOpcode::LoadTupleElement | BytecodeOpcode::LoadEnumElement => {
                opcode_size(width) + 4 * operand_size(width)
            }

            BytecodeOpcode::ConstUInt8 => opcode_size(width) + operand_size(width) + 1,

            _ => unreachable!(),
        }
    }

    pub fn needs_position(&self) -> bool {
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
            | BytecodeOpcode::StringLength
            | BytecodeOpcode::LoadStringUInt8
            | BytecodeOpcode::LoadEnumElement
            | BytecodeOpcode::LoadEnumVariant
            | BytecodeOpcode::Add
            | BytecodeOpcode::Sub
            | BytecodeOpcode::Mul => true,
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
        global_id: GlobalDefinitionId,
    },
    StoreGlobal {
        src: Register,
        global_id: GlobalDefinitionId,
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
    JumpConst {
        idx: ConstPoolIdx,
    },
    JumpIfFalse {
        opnd: Register,
        offset: u32,
    },
    JumpIfFalseConst {
        opnd: Register,
        idx: ConstPoolIdx,
    },
    JumpIfTrue {
        opnd: Register,
        offset: u32,
    },
    JumpIfTrueConst {
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
        cls: ConstPoolIdx,
        length: Register,
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
        idx: ConstPoolIdx,
        src: Register,
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

    StringLength {
        dest: Register,
        str: Register,
    },
    LoadStringUInt8 {
        dest: Register,
        str: Register,
        idx: Register,
    },

    Ret {
        opnd: Register,
    },
}

#[derive(Copy, Clone, Debug)]
pub enum OperandWidth {
    Normal,
    Wide,
}

impl OperandWidth {
    pub fn size(self) -> usize {
        match self {
            OperandWidth::Normal => 1,
            OperandWidth::Wide => 4,
        }
    }

    pub fn needs_bytecode(self) -> bool {
        match self {
            OperandWidth::Normal => false,
            OperandWidth::Wide => true,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug, Eq, Hash)]
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
    arguments: u32,
    positions: Vec<(u32, Position)>,
    params: Vec<BytecodeType>,
    return_type: Option<BytecodeType>,
}

impl BytecodeFunction {
    pub fn new(
        code: Vec<u8>,
        const_pool: Vec<ConstPoolEntry>,
        registers: Vec<BytecodeType>,
        arguments: u32,
        positions: Vec<(u32, Position)>,
        params: Vec<BytecodeType>,
        return_type: Option<BytecodeType>,
    ) -> BytecodeFunction {
        BytecodeFunction {
            code,
            const_pool,
            registers,
            arguments,
            positions,
            params,
            return_type,
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
        self.registers
            .get(register.0)
            .expect("register not found")
            .clone()
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

    pub fn read_opcode(&self, offset: BytecodeOffset) -> BytecodeOpcode {
        BytecodeReader::read_opcode_at(&self.code, offset.to_usize())
    }
}

enumeration!(ConstPoolOpcode {
    String,
    Float32,
    Float64,
    Int32,
    Int64,
    Char,
    Fct,
    Class,
    Enum,
    EnumVariant,
    EnumElement,
    Struct,
    StructField,
    Trait,
    Field,
    FieldFixed,
    Generic,
    TupleElement,
    Tuple,
    Lambda
});

#[derive(Debug, PartialEq)]
pub enum ConstPoolEntry {
    String(String),
    Float32(f32),
    Float64(f64),
    Int32(i32),
    Int64(i64),
    Char(char),
    Class(ClassDefinitionId, SourceTypeArray),
    Field(ClassDefinitionId, SourceTypeArray, FieldId),
    FieldFixed(ClassInstanceId, FieldId),
    Fct(FctDefinitionId, SourceTypeArray),
    Generic(TypeParamId, FctDefinitionId, SourceTypeArray),
    Enum(EnumDefinitionId, SourceTypeArray),
    EnumVariant(EnumDefinitionId, SourceTypeArray, usize),
    EnumElement(EnumDefinitionId, SourceTypeArray, usize, usize),
    Struct(StructDefinitionId, SourceTypeArray),
    StructField(StructDefinitionId, SourceTypeArray, StructDefinitionFieldId),
    Trait(TraitDefinitionId, SourceTypeArray, SourceType),
    TupleElement(SourceType, usize),
    Tuple(SourceTypeArray),
    Lambda(SourceTypeArray, SourceType),
}

impl ConstPoolEntry {
    pub fn to_string(&self) -> Option<&str> {
        match self {
            ConstPoolEntry::String(ref value) => Some(value),
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

enumeration!(SourceTypeOpcode {
    // couldn't determine type because of error
    Error,

    // Allow any type here, used for type inference
    Any,

    // type with only one value: ()
    Unit,

    // primitives
    Bool,
    Char,
    UInt8,
    Int32,
    Int64,
    Float32,
    Float64,

    // pointer to object, only used internally
    Ptr,

    // self type
    This,

    // some class
    Class,

    // some struct
    Struct,

    // some tuple
    Tuple,

    // some trait object
    Trait,

    // some module
    Module,

    // some type variable
    TypeParam,

    // some lambda
    Lambda,

    // some enum
    Enum
});

#[rustfmt::skip]
enumeration!(InstructionSet {
    X64,
    Arm64
});
