use bincode::{Decode, Encode};
use std::fmt;

use crate::opcode as opc;
use crate::{
    BytecodeReader, BytecodeTraitType, BytecodeType, BytecodeTypeArray, ClassId, ConstId, EnumId,
    FunctionId, GlobalId, StructId,
};

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

#[derive(Copy, Clone, PartialEq, Eq)]
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
    LoadEnumElement,
    LoadEnumVariant,
    LoadField,
    StoreField,
    LoadGlobal,
    StoreGlobal,
    LoadConst,
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
    JumpLoop,
    LoopStart,
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
    NewObjectUninitialized,
    NewObject,
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
    GetFieldAddress,
    StoreAddress,
    LoadAddress,
    GetFieldRef,
    StoreRef,
    LoadRef,
    Ret,
}

impl From<BytecodeOpcode> for u8 {
    fn from(value: BytecodeOpcode) -> u8 {
        match value {
            BytecodeOpcode::Add => opc::BYTECODE_OPCODE_ADD,
            BytecodeOpcode::Sub => opc::BYTECODE_OPCODE_SUB,
            BytecodeOpcode::Neg => opc::BYTECODE_OPCODE_NEG,
            BytecodeOpcode::Mul => opc::BYTECODE_OPCODE_MUL,
            BytecodeOpcode::Div => opc::BYTECODE_OPCODE_DIV,
            BytecodeOpcode::Mod => opc::BYTECODE_OPCODE_MOD,
            BytecodeOpcode::And => opc::BYTECODE_OPCODE_AND,
            BytecodeOpcode::Or => opc::BYTECODE_OPCODE_OR,
            BytecodeOpcode::Xor => opc::BYTECODE_OPCODE_XOR,
            BytecodeOpcode::Not => opc::BYTECODE_OPCODE_NOT,
            BytecodeOpcode::Shl => opc::BYTECODE_OPCODE_SHL,
            BytecodeOpcode::Shr => opc::BYTECODE_OPCODE_SHR,
            BytecodeOpcode::Sar => opc::BYTECODE_OPCODE_SAR,
            BytecodeOpcode::Mov => opc::BYTECODE_OPCODE_MOV,
            BytecodeOpcode::LoadEnumElement => opc::BYTECODE_OPCODE_LOAD_ENUM_ELEMENT,
            BytecodeOpcode::LoadEnumVariant => opc::BYTECODE_OPCODE_LOAD_ENUM_VARIANT,
            BytecodeOpcode::LoadField => opc::BYTECODE_OPCODE_LOAD_FIELD,
            BytecodeOpcode::StoreField => opc::BYTECODE_OPCODE_STORE_FIELD,
            BytecodeOpcode::LoadGlobal => opc::BYTECODE_OPCODE_LOAD_GLOBAL,
            BytecodeOpcode::StoreGlobal => opc::BYTECODE_OPCODE_STORE_GLOBAL,
            BytecodeOpcode::LoadConst => opc::BYTECODE_OPCODE_LOAD_CONST,
            BytecodeOpcode::ConstTrue => opc::BYTECODE_OPCODE_CONST_TRUE,
            BytecodeOpcode::ConstFalse => opc::BYTECODE_OPCODE_CONST_FALSE,
            BytecodeOpcode::ConstUInt8 => opc::BYTECODE_OPCODE_CONST_UINT8,
            BytecodeOpcode::ConstChar => opc::BYTECODE_OPCODE_CONST_CHAR,
            BytecodeOpcode::ConstInt32 => opc::BYTECODE_OPCODE_CONST_INT32,
            BytecodeOpcode::ConstInt64 => opc::BYTECODE_OPCODE_CONST_INT64,
            BytecodeOpcode::ConstFloat32 => opc::BYTECODE_OPCODE_CONST_FLOAT32,
            BytecodeOpcode::ConstFloat64 => opc::BYTECODE_OPCODE_CONST_FLOAT64,
            BytecodeOpcode::ConstString => opc::BYTECODE_OPCODE_CONST_STRING,
            BytecodeOpcode::TestIdentity => opc::BYTECODE_OPCODE_TEST_IDENTITY,
            BytecodeOpcode::TestEq => opc::BYTECODE_OPCODE_TEST_EQ,
            BytecodeOpcode::TestNe => opc::BYTECODE_OPCODE_TEST_NE,
            BytecodeOpcode::TestGt => opc::BYTECODE_OPCODE_TEST_GT,
            BytecodeOpcode::TestGe => opc::BYTECODE_OPCODE_TEST_GE,
            BytecodeOpcode::TestLt => opc::BYTECODE_OPCODE_TEST_LT,
            BytecodeOpcode::TestLe => opc::BYTECODE_OPCODE_TEST_LE,
            BytecodeOpcode::JumpLoop => opc::BYTECODE_OPCODE_JUMP_LOOP,
            BytecodeOpcode::LoopStart => opc::BYTECODE_OPCODE_LOOP_START,
            BytecodeOpcode::Jump => opc::BYTECODE_OPCODE_JUMP,
            BytecodeOpcode::JumpIfFalse => opc::BYTECODE_OPCODE_JUMP_IF_FALSE,
            BytecodeOpcode::JumpIfTrue => opc::BYTECODE_OPCODE_JUMP_IF_TRUE,
            BytecodeOpcode::Switch => opc::BYTECODE_OPCODE_SWITCH,
            BytecodeOpcode::InvokeDirect => opc::BYTECODE_OPCODE_INVOKE_DIRECT,
            BytecodeOpcode::InvokeVirtual => opc::BYTECODE_OPCODE_INVOKE_VIRTUAL,
            BytecodeOpcode::InvokeStatic => opc::BYTECODE_OPCODE_INVOKE_STATIC,
            BytecodeOpcode::InvokeLambda => opc::BYTECODE_OPCODE_INVOKE_LAMBDA,
            BytecodeOpcode::InvokeGenericStatic => opc::BYTECODE_OPCODE_INVOKE_GENERIC_STATIC,
            BytecodeOpcode::InvokeGenericDirect => opc::BYTECODE_OPCODE_INVOKE_GENERIC_DIRECT,
            BytecodeOpcode::NewObjectUninitialized => opc::BYTECODE_OPCODE_NEW_OBJECT_UNINITIALIZED,
            BytecodeOpcode::NewObject => opc::BYTECODE_OPCODE_NEW_OBJECT,
            BytecodeOpcode::NewArray => opc::BYTECODE_OPCODE_NEW_ARRAY,
            BytecodeOpcode::NewTuple => opc::BYTECODE_OPCODE_NEW_TUPLE,
            BytecodeOpcode::NewEnum => opc::BYTECODE_OPCODE_NEW_ENUM,
            BytecodeOpcode::NewStruct => opc::BYTECODE_OPCODE_NEW_STRUCT,
            BytecodeOpcode::NewTraitObject => opc::BYTECODE_OPCODE_NEW_TRAIT_OBJECT,
            BytecodeOpcode::NewLambda => opc::BYTECODE_OPCODE_NEW_LAMBDA,
            BytecodeOpcode::ArrayLength => opc::BYTECODE_OPCODE_ARRAY_LENGTH,
            BytecodeOpcode::LoadArray => opc::BYTECODE_OPCODE_LOAD_ARRAY,
            BytecodeOpcode::StoreArray => opc::BYTECODE_OPCODE_STORE_ARRAY,
            BytecodeOpcode::LoadTraitObjectValue => opc::BYTECODE_OPCODE_LOAD_TRAIT_OBJECT_VALUE,
            BytecodeOpcode::GetFieldAddress => opc::BYTECODE_OPCODE_GET_FIELD_ADDRESS,
            BytecodeOpcode::StoreAddress => opc::BYTECODE_OPCODE_STORE_ADDRESS,
            BytecodeOpcode::LoadAddress => opc::BYTECODE_OPCODE_LOAD_ADDRESS,
            BytecodeOpcode::GetFieldRef => opc::BYTECODE_OPCODE_GET_FIELD_REF,
            BytecodeOpcode::StoreRef => opc::BYTECODE_OPCODE_STORE_REF,
            BytecodeOpcode::LoadRef => opc::BYTECODE_OPCODE_LOAD_REF,
            BytecodeOpcode::Ret => opc::BYTECODE_OPCODE_RET,
        }
    }
}

impl TryFrom<u8> for BytecodeOpcode {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            opc::BYTECODE_OPCODE_ADD => Ok(BytecodeOpcode::Add),
            opc::BYTECODE_OPCODE_SUB => Ok(BytecodeOpcode::Sub),
            opc::BYTECODE_OPCODE_NEG => Ok(BytecodeOpcode::Neg),
            opc::BYTECODE_OPCODE_MUL => Ok(BytecodeOpcode::Mul),
            opc::BYTECODE_OPCODE_DIV => Ok(BytecodeOpcode::Div),
            opc::BYTECODE_OPCODE_MOD => Ok(BytecodeOpcode::Mod),
            opc::BYTECODE_OPCODE_AND => Ok(BytecodeOpcode::And),
            opc::BYTECODE_OPCODE_OR => Ok(BytecodeOpcode::Or),
            opc::BYTECODE_OPCODE_XOR => Ok(BytecodeOpcode::Xor),
            opc::BYTECODE_OPCODE_NOT => Ok(BytecodeOpcode::Not),
            opc::BYTECODE_OPCODE_SHL => Ok(BytecodeOpcode::Shl),
            opc::BYTECODE_OPCODE_SHR => Ok(BytecodeOpcode::Shr),
            opc::BYTECODE_OPCODE_SAR => Ok(BytecodeOpcode::Sar),
            opc::BYTECODE_OPCODE_MOV => Ok(BytecodeOpcode::Mov),
            opc::BYTECODE_OPCODE_LOAD_ENUM_ELEMENT => Ok(BytecodeOpcode::LoadEnumElement),
            opc::BYTECODE_OPCODE_LOAD_ENUM_VARIANT => Ok(BytecodeOpcode::LoadEnumVariant),
            opc::BYTECODE_OPCODE_LOAD_FIELD => Ok(BytecodeOpcode::LoadField),
            opc::BYTECODE_OPCODE_STORE_FIELD => Ok(BytecodeOpcode::StoreField),
            opc::BYTECODE_OPCODE_LOAD_GLOBAL => Ok(BytecodeOpcode::LoadGlobal),
            opc::BYTECODE_OPCODE_STORE_GLOBAL => Ok(BytecodeOpcode::StoreGlobal),
            opc::BYTECODE_OPCODE_LOAD_CONST => Ok(BytecodeOpcode::LoadConst),
            opc::BYTECODE_OPCODE_CONST_TRUE => Ok(BytecodeOpcode::ConstTrue),
            opc::BYTECODE_OPCODE_CONST_FALSE => Ok(BytecodeOpcode::ConstFalse),
            opc::BYTECODE_OPCODE_CONST_UINT8 => Ok(BytecodeOpcode::ConstUInt8),
            opc::BYTECODE_OPCODE_CONST_CHAR => Ok(BytecodeOpcode::ConstChar),
            opc::BYTECODE_OPCODE_CONST_INT32 => Ok(BytecodeOpcode::ConstInt32),
            opc::BYTECODE_OPCODE_CONST_INT64 => Ok(BytecodeOpcode::ConstInt64),
            opc::BYTECODE_OPCODE_CONST_FLOAT32 => Ok(BytecodeOpcode::ConstFloat32),
            opc::BYTECODE_OPCODE_CONST_FLOAT64 => Ok(BytecodeOpcode::ConstFloat64),
            opc::BYTECODE_OPCODE_CONST_STRING => Ok(BytecodeOpcode::ConstString),
            opc::BYTECODE_OPCODE_TEST_IDENTITY => Ok(BytecodeOpcode::TestIdentity),
            opc::BYTECODE_OPCODE_TEST_EQ => Ok(BytecodeOpcode::TestEq),
            opc::BYTECODE_OPCODE_TEST_NE => Ok(BytecodeOpcode::TestNe),
            opc::BYTECODE_OPCODE_TEST_GT => Ok(BytecodeOpcode::TestGt),
            opc::BYTECODE_OPCODE_TEST_GE => Ok(BytecodeOpcode::TestGe),
            opc::BYTECODE_OPCODE_TEST_LT => Ok(BytecodeOpcode::TestLt),
            opc::BYTECODE_OPCODE_TEST_LE => Ok(BytecodeOpcode::TestLe),
            opc::BYTECODE_OPCODE_JUMP_LOOP => Ok(BytecodeOpcode::JumpLoop),
            opc::BYTECODE_OPCODE_LOOP_START => Ok(BytecodeOpcode::LoopStart),
            opc::BYTECODE_OPCODE_JUMP => Ok(BytecodeOpcode::Jump),
            opc::BYTECODE_OPCODE_JUMP_IF_FALSE => Ok(BytecodeOpcode::JumpIfFalse),
            opc::BYTECODE_OPCODE_JUMP_IF_TRUE => Ok(BytecodeOpcode::JumpIfTrue),
            opc::BYTECODE_OPCODE_SWITCH => Ok(BytecodeOpcode::Switch),
            opc::BYTECODE_OPCODE_INVOKE_DIRECT => Ok(BytecodeOpcode::InvokeDirect),
            opc::BYTECODE_OPCODE_INVOKE_VIRTUAL => Ok(BytecodeOpcode::InvokeVirtual),
            opc::BYTECODE_OPCODE_INVOKE_STATIC => Ok(BytecodeOpcode::InvokeStatic),
            opc::BYTECODE_OPCODE_INVOKE_LAMBDA => Ok(BytecodeOpcode::InvokeLambda),
            opc::BYTECODE_OPCODE_INVOKE_GENERIC_STATIC => Ok(BytecodeOpcode::InvokeGenericStatic),
            opc::BYTECODE_OPCODE_INVOKE_GENERIC_DIRECT => Ok(BytecodeOpcode::InvokeGenericDirect),
            opc::BYTECODE_OPCODE_NEW_OBJECT_UNINITIALIZED => {
                Ok(BytecodeOpcode::NewObjectUninitialized)
            }
            opc::BYTECODE_OPCODE_NEW_OBJECT => Ok(BytecodeOpcode::NewObject),
            opc::BYTECODE_OPCODE_NEW_ARRAY => Ok(BytecodeOpcode::NewArray),
            opc::BYTECODE_OPCODE_NEW_TUPLE => Ok(BytecodeOpcode::NewTuple),
            opc::BYTECODE_OPCODE_NEW_ENUM => Ok(BytecodeOpcode::NewEnum),
            opc::BYTECODE_OPCODE_NEW_STRUCT => Ok(BytecodeOpcode::NewStruct),
            opc::BYTECODE_OPCODE_NEW_TRAIT_OBJECT => Ok(BytecodeOpcode::NewTraitObject),
            opc::BYTECODE_OPCODE_NEW_LAMBDA => Ok(BytecodeOpcode::NewLambda),
            opc::BYTECODE_OPCODE_ARRAY_LENGTH => Ok(BytecodeOpcode::ArrayLength),
            opc::BYTECODE_OPCODE_LOAD_ARRAY => Ok(BytecodeOpcode::LoadArray),
            opc::BYTECODE_OPCODE_STORE_ARRAY => Ok(BytecodeOpcode::StoreArray),
            opc::BYTECODE_OPCODE_LOAD_TRAIT_OBJECT_VALUE => {
                Ok(BytecodeOpcode::LoadTraitObjectValue)
            }
            opc::BYTECODE_OPCODE_GET_FIELD_ADDRESS => Ok(BytecodeOpcode::GetFieldAddress),
            opc::BYTECODE_OPCODE_STORE_ADDRESS => Ok(BytecodeOpcode::StoreAddress),
            opc::BYTECODE_OPCODE_LOAD_ADDRESS => Ok(BytecodeOpcode::LoadAddress),
            opc::BYTECODE_OPCODE_GET_FIELD_REF => Ok(BytecodeOpcode::GetFieldRef),
            opc::BYTECODE_OPCODE_STORE_REF => Ok(BytecodeOpcode::StoreRef),
            opc::BYTECODE_OPCODE_LOAD_REF => Ok(BytecodeOpcode::LoadRef),
            opc::BYTECODE_OPCODE_RET => Ok(BytecodeOpcode::Ret),
            _ => Err(()),
        }
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

    pub fn is_new_object(self) -> bool {
        match self {
            BytecodeOpcode::NewObject => true,
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
            | BytecodeOpcode::InvokeDirect
            | BytecodeOpcode::InvokeVirtual
            | BytecodeOpcode::InvokeStatic
            | BytecodeOpcode::InvokeLambda
            | BytecodeOpcode::InvokeGenericStatic
            | BytecodeOpcode::InvokeGenericDirect
            | BytecodeOpcode::NewObjectUninitialized
            | BytecodeOpcode::NewObject
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
            | BytecodeOpcode::GetFieldAddress
            | BytecodeOpcode::StoreAddress
            | BytecodeOpcode::GetFieldRef
            | BytecodeOpcode::StoreRef
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
        arguments: Vec<Register>,
    },

    InvokeVirtual {
        dest: Register,
        fct: ConstPoolIdx,
        arguments: Vec<Register>,
    },

    InvokeStatic {
        dest: Register,
        fct: ConstPoolIdx,
        arguments: Vec<Register>,
    },

    InvokeLambda {
        dest: Register,
        idx: ConstPoolIdx,
        arguments: Vec<Register>,
    },

    InvokeGenericStatic {
        dest: Register,
        fct: ConstPoolIdx,
        arguments: Vec<Register>,
    },

    InvokeGenericDirect {
        dest: Register,
        fct: ConstPoolIdx,
        arguments: Vec<Register>,
    },

    NewObjectUninitialized {
        dest: Register,
        cls: ConstPoolIdx,
    },
    NewObject {
        dest: Register,
        cls: ConstPoolIdx,
        arguments: Vec<Register>,
    },
    NewArray {
        dest: Register,
        length: Register,
        idx: ConstPoolIdx,
    },
    NewTuple {
        dest: Register,
        idx: ConstPoolIdx,
        arguments: Vec<Register>,
    },
    NewEnum {
        dest: Register,
        idx: ConstPoolIdx,
        arguments: Vec<Register>,
    },
    NewStruct {
        dest: Register,
        idx: ConstPoolIdx,
        arguments: Vec<Register>,
    },
    NewTraitObject {
        dest: Register,
        src: Register,
        idx: ConstPoolIdx,
    },
    NewLambda {
        dest: Register,
        idx: ConstPoolIdx,
        arguments: Vec<Register>,
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

    GetFieldAddress {
        dest: Register,
        obj: Register,
        field: ConstPoolIdx,
    },
    StoreAddress {
        src: Register,
        address: Register,
    },
    LoadAddress {
        dest: Register,
        address: Register,
    },

    GetFieldRef {
        dest: Register,
        obj: Register,
        field: ConstPoolIdx,
    },
    StoreRef {
        src: Register,
        reference: Register,
    },
    LoadRef {
        dest: Register,
        reference: Register,
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
            Err(0) => 0,
            Err(index) => index - 1,
            Ok(index) => index,
        };
        self.locations
            .get(index)
            .map(|(_, loc)| *loc)
            .unwrap_or(Location::new(1, 1))
    }

    pub fn read_opcode(&self, offset: BytecodeOffset) -> BytecodeOpcode {
        BytecodeReader::read_opcode_at(&self.code, offset.to_usize())
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
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

impl From<ConstPoolOpcode> for u8 {
    fn from(value: ConstPoolOpcode) -> u8 {
        match value {
            ConstPoolOpcode::String => opc::CONST_POOL_OPCODE_STRING,
            ConstPoolOpcode::Float32 => opc::CONST_POOL_OPCODE_FLOAT32,
            ConstPoolOpcode::Float64 => opc::CONST_POOL_OPCODE_FLOAT64,
            ConstPoolOpcode::Int32 => opc::CONST_POOL_OPCODE_INT32,
            ConstPoolOpcode::Int64 => opc::CONST_POOL_OPCODE_INT64,
            ConstPoolOpcode::Char => opc::CONST_POOL_OPCODE_CHAR,
            ConstPoolOpcode::Class => opc::CONST_POOL_OPCODE_CLASS,
            ConstPoolOpcode::Field => opc::CONST_POOL_OPCODE_FIELD,
            ConstPoolOpcode::Fct => opc::CONST_POOL_OPCODE_FCT,
            ConstPoolOpcode::TraitObjectMethod => opc::CONST_POOL_OPCODE_TRAIT_OBJECT_METHOD,
            ConstPoolOpcode::Generic => opc::CONST_POOL_OPCODE_GENERIC,
            ConstPoolOpcode::Enum => opc::CONST_POOL_OPCODE_ENUM,
            ConstPoolOpcode::EnumVariant => opc::CONST_POOL_OPCODE_ENUM_VARIANT,
            ConstPoolOpcode::EnumElement => opc::CONST_POOL_OPCODE_ENUM_ELEMENT,
            ConstPoolOpcode::Struct => opc::CONST_POOL_OPCODE_STRUCT,
            ConstPoolOpcode::StructField => opc::CONST_POOL_OPCODE_STRUCT_FIELD,
            ConstPoolOpcode::TraitObject => opc::CONST_POOL_OPCODE_TRAIT_OBJECT,
            ConstPoolOpcode::TupleElement => opc::CONST_POOL_OPCODE_TUPLE_ELEMENT,
            ConstPoolOpcode::Tuple => opc::CONST_POOL_OPCODE_TUPLE,
            ConstPoolOpcode::Lambda => opc::CONST_POOL_OPCODE_LAMBDA,
            ConstPoolOpcode::JumpTable => opc::CONST_POOL_OPCODE_JUMP_TABLE,
        }
    }
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
    Generic {
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

impl std::fmt::Display for ConstPoolIdx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}", self.0)
    }
}
