use std::fmt;

use crate::bytecode::read_opcode_and_width;
use crate::mem::ptr_width;
use crate::semck::specialize::{specialize_enum_id_params, specialize_struct_id_params};
use crate::ty::{MachineMode, SourceType, SourceTypeArray};
use crate::utils::enumeration;
use crate::vm::{
    get_vm, ClassDefId, ClassId, EnumId, EnumLayout, FctId, FieldId, StructDefinitionFieldId,
    StructId, TraitId, TupleId, TypeParamId, VM,
};
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
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum BytecodeType {
    Bool,
    UInt8,
    Char,
    Int32,
    Int64,
    Float32,
    Float64,
    Ptr,
    Tuple(TupleId),
    TypeParam(u32),
    Enum(EnumId, SourceTypeArray),
    Struct(StructId, SourceTypeArray),
}

impl BytecodeType {
    pub fn size(&self, vm: &VM) -> i32 {
        match self {
            BytecodeType::Bool => 1,
            BytecodeType::UInt8 => 1,
            BytecodeType::Char => 4,
            BytecodeType::Int32 => 4,
            BytecodeType::Int64 => 8,
            BytecodeType::Float32 => 4,
            BytecodeType::Float64 => 8,
            BytecodeType::Ptr => ptr_width(),
            BytecodeType::Tuple(tuple_id) => {
                let vm = get_vm();
                vm.tuples.lock().get_tuple(*tuple_id).size()
            }
            BytecodeType::TypeParam(_) => unreachable!(),
            BytecodeType::Enum(enum_id, type_params) => {
                let edef_id = specialize_enum_id_params(vm, *enum_id, type_params.clone());
                let edef = vm.enum_defs.idx(edef_id);

                match edef.layout {
                    EnumLayout::Int => 4,
                    EnumLayout::Ptr | EnumLayout::Tagged => ptr_width(),
                }
            }
            BytecodeType::Struct(struct_id, type_params) => {
                let sdef_id = specialize_struct_id_params(vm, *struct_id, type_params.clone());
                let sdef = vm.struct_defs.idx(sdef_id);

                sdef.size
            }
        }
    }

    pub fn kind(&self) -> BytecodeTypeKind {
        match self {
            BytecodeType::Bool => BytecodeTypeKind::Bool,
            BytecodeType::UInt8 => BytecodeTypeKind::UInt8,
            BytecodeType::Char => BytecodeTypeKind::Char,
            BytecodeType::Int32 => BytecodeTypeKind::Int32,
            BytecodeType::Int64 => BytecodeTypeKind::Int64,
            BytecodeType::Float32 => BytecodeTypeKind::Float32,
            BytecodeType::Float64 => BytecodeTypeKind::Float64,
            BytecodeType::Ptr => BytecodeTypeKind::Ptr,
            BytecodeType::Tuple(_) => BytecodeTypeKind::Tuple,
            BytecodeType::TypeParam(_) => BytecodeTypeKind::TypeParam,
            BytecodeType::Enum(_, _) => BytecodeTypeKind::Enum,
            BytecodeType::Struct(_, _) => BytecodeTypeKind::Struct,
        }
    }

    pub fn mode(&self, vm: &VM) -> MachineMode {
        match self {
            BytecodeType::Bool => MachineMode::Int8,
            BytecodeType::UInt8 => MachineMode::Int8,
            BytecodeType::Char => MachineMode::Int32,
            BytecodeType::Int32 => MachineMode::Int32,
            BytecodeType::Int64 => MachineMode::Int64,
            BytecodeType::Float32 => MachineMode::Float32,
            BytecodeType::Float64 => MachineMode::Float64,
            BytecodeType::Ptr => MachineMode::Ptr,
            BytecodeType::Tuple(_) => unreachable!(),
            BytecodeType::TypeParam(_) => unreachable!(),
            BytecodeType::Enum(enum_id, type_params) => {
                let edef_id = specialize_enum_id_params(vm, *enum_id, type_params.clone());
                let edef = vm.enum_defs.idx(edef_id);

                match edef.layout {
                    EnumLayout::Int => MachineMode::Int32,
                    EnumLayout::Ptr | EnumLayout::Tagged => MachineMode::Ptr,
                }
            }
            BytecodeType::Struct(_struct_id, _type_params) => unreachable!(),
        }
    }

    pub fn is_any_float(&self) -> bool {
        match self {
            BytecodeType::Float32 | BytecodeType::Float64 => true,
            _ => false,
        }
    }

    pub fn is_ptr(&self) -> bool {
        match self {
            BytecodeType::Ptr => true,
            _ => false,
        }
    }

    pub fn is_type_param(&self) -> bool {
        match self {
            BytecodeType::TypeParam(_) => true,
            _ => false,
        }
    }

    pub fn tuple_id(&self) -> Option<TupleId> {
        match *self {
            BytecodeType::Tuple(tuple_id) => Some(tuple_id),
            _ => None,
        }
    }

    pub fn from_ty(vm: &VM, ty: SourceType) -> BytecodeType {
        match ty {
            SourceType::Bool => BytecodeType::Bool,
            SourceType::UInt8 => BytecodeType::UInt8,
            SourceType::Char => BytecodeType::Char,
            SourceType::Int32 => BytecodeType::Int32,
            SourceType::Int64 => BytecodeType::Int64,
            SourceType::Float32 => BytecodeType::Float32,
            SourceType::Float64 => BytecodeType::Float64,
            SourceType::Class(_, _) => BytecodeType::Ptr,
            SourceType::Trait(_, _) => BytecodeType::Ptr,
            SourceType::Enum(id, list_id) => {
                let xenum = vm.enums[id].read();

                for variant in &xenum.variants {
                    if !variant.types.is_empty() {
                        let type_params = vm.source_type_arrays.lock().get(list_id);
                        return BytecodeType::Enum(id, type_params);
                    }
                }

                BytecodeType::Int32
            }
            SourceType::Struct(id, list_id) => {
                let type_params = vm.source_type_arrays.lock().get(list_id);
                BytecodeType::Struct(id, type_params)
            }
            SourceType::Tuple(tuple_id) => BytecodeType::Tuple(tuple_id),
            SourceType::TypeParam(idx) => BytecodeType::TypeParam(idx.to_usize() as u32),
            _ => panic!("BuiltinType {:?} cannot converted to BytecodeType", ty),
        }
    }
}

// Keep in sync with dora-boots/bytecode.dora
enumeration!(BytecodeOpcode {
    Wide,

    AddInt32,
    AddInt64,
    AddFloat32,
    AddFloat64,

    SubInt32,
    SubInt64,
    SubFloat32,
    SubFloat64,

    NegInt32,
    NegInt64,
    NegFloat32,
    NegFloat64,

    MulInt32,
    MulInt64,
    MulFloat32,
    MulFloat64,

    DivInt32,
    DivInt64,
    DivFloat32,
    DivFloat64,

    ModInt32,
    ModInt64,

    AndInt32,
    AndInt64,

    OrInt32,
    OrInt64,

    XorInt32,
    XorInt64,

    NotBool,
    NotInt32,
    NotInt64,

    ShlInt32,
    ShrInt32,
    SarInt32,

    ShlInt64,
    ShrInt64,
    SarInt64,

    RolInt32,
    RorInt32,

    RolInt64,
    RorInt64,

    ExtendUInt8ToChar,
    ExtendUInt8ToInt32,
    ExtendUInt8ToInt64,
    ExtendInt32ToInt64,
    ExtendCharToInt64,
    CastCharToInt32,
    CastInt32ToUInt8,
    CastInt32ToChar,
    CastInt64ToUInt8,
    CastInt64ToChar,
    CastInt64ToInt32,

    InstanceOf,
    CheckedCast,

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
    ConstZeroUInt8,
    ConstZeroChar,
    ConstZeroInt32,
    ConstZeroInt64,
    ConstZeroFloat32,
    ConstZeroFloat64,
    ConstUInt8,
    ConstChar,
    ConstInt32,
    ConstInt64,
    ConstFloat32,
    ConstFloat64,
    ConstString,

    TestIdentity,

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

    TestEqFloat32,
    TestNeFloat32,
    TestGtFloat32,
    TestGeFloat32,
    TestLtFloat32,
    TestLeFloat32,

    TestEqFloat64,
    TestNeFloat64,
    TestGtFloat64,
    TestGeFloat64,
    TestLtFloat64,
    TestLeFloat64,

    Assert,

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

    InvokeDirectVoid,
    InvokeDirect,

    InvokeVirtualVoid,
    InvokeVirtual,

    InvokeStaticVoid,
    InvokeStatic,

    InvokeGenericStaticVoid,
    InvokeGenericStatic,

    InvokeGenericDirectVoid,
    InvokeGenericDirect,

    NewObject,
    NewArray,
    NewTuple,
    NewEnum,
    NewStruct,
    NewTraitObject,

    NilCheck,

    ArrayLength,
    ArrayBoundCheck,

    LoadArray,
    StoreArray,

    RetVoid,
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

    pub fn is_any_invoke(self) -> bool {
        match self {
            BytecodeOpcode::InvokeDirectVoid
            | BytecodeOpcode::InvokeDirect
            | BytecodeOpcode::InvokeGenericDirect
            | BytecodeOpcode::InvokeGenericDirectVoid
            | BytecodeOpcode::InvokeGenericStatic
            | BytecodeOpcode::InvokeGenericStaticVoid
            | BytecodeOpcode::InvokeStatic
            | BytecodeOpcode::InvokeStaticVoid
            | BytecodeOpcode::InvokeVirtual
            | BytecodeOpcode::InvokeVirtualVoid => true,
            _ => false,
        }
    }

    pub fn size(self, width: OperandWidth) -> u32 {
        match self {
            BytecodeOpcode::Wide => unreachable!(),

            BytecodeOpcode::RetVoid | BytecodeOpcode::LoopStart => opcode_size(width),

            BytecodeOpcode::PushRegister
            | BytecodeOpcode::ConstTrue
            | BytecodeOpcode::ConstFalse
            | BytecodeOpcode::ConstZeroUInt8
            | BytecodeOpcode::ConstZeroChar
            | BytecodeOpcode::ConstZeroInt32
            | BytecodeOpcode::ConstZeroInt64
            | BytecodeOpcode::ConstZeroFloat32
            | BytecodeOpcode::ConstZeroFloat64
            | BytecodeOpcode::Assert
            | BytecodeOpcode::Ret
            | BytecodeOpcode::NilCheck
            | BytecodeOpcode::InvokeGenericDirectVoid
            | BytecodeOpcode::InvokeGenericStaticVoid
            | BytecodeOpcode::InvokeStaticVoid
            | BytecodeOpcode::InvokeDirectVoid
            | BytecodeOpcode::InvokeVirtualVoid
            | BytecodeOpcode::JumpConst
            | BytecodeOpcode::Jump
            | BytecodeOpcode::JumpLoop => opcode_size(width) + 1 * operand_size(width),

            BytecodeOpcode::NegInt32
            | BytecodeOpcode::NegInt64
            | BytecodeOpcode::NegFloat32
            | BytecodeOpcode::NegFloat64
            | BytecodeOpcode::NotBool
            | BytecodeOpcode::NotInt32
            | BytecodeOpcode::NotInt64
            | BytecodeOpcode::ExtendUInt8ToChar
            | BytecodeOpcode::ExtendUInt8ToInt32
            | BytecodeOpcode::ExtendUInt8ToInt64
            | BytecodeOpcode::ExtendInt32ToInt64
            | BytecodeOpcode::ExtendCharToInt64
            | BytecodeOpcode::CastCharToInt32
            | BytecodeOpcode::CastInt32ToUInt8
            | BytecodeOpcode::CastInt32ToChar
            | BytecodeOpcode::CastInt64ToUInt8
            | BytecodeOpcode::CastInt64ToChar
            | BytecodeOpcode::CastInt64ToInt32
            | BytecodeOpcode::CheckedCast
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
            | BytecodeOpcode::ArrayBoundCheck
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

            BytecodeOpcode::AddInt32
            | BytecodeOpcode::AddInt64
            | BytecodeOpcode::AddFloat32
            | BytecodeOpcode::AddFloat64
            | BytecodeOpcode::SubInt32
            | BytecodeOpcode::SubInt64
            | BytecodeOpcode::SubFloat32
            | BytecodeOpcode::SubFloat64
            | BytecodeOpcode::MulInt32
            | BytecodeOpcode::MulInt64
            | BytecodeOpcode::MulFloat32
            | BytecodeOpcode::MulFloat64
            | BytecodeOpcode::DivInt32
            | BytecodeOpcode::DivInt64
            | BytecodeOpcode::DivFloat32
            | BytecodeOpcode::DivFloat64
            | BytecodeOpcode::ModInt32
            | BytecodeOpcode::ModInt64
            | BytecodeOpcode::AndInt32
            | BytecodeOpcode::AndInt64
            | BytecodeOpcode::OrInt32
            | BytecodeOpcode::OrInt64
            | BytecodeOpcode::XorInt32
            | BytecodeOpcode::XorInt64
            | BytecodeOpcode::ShlInt32
            | BytecodeOpcode::ShrInt32
            | BytecodeOpcode::SarInt32
            | BytecodeOpcode::ShlInt64
            | BytecodeOpcode::ShrInt64
            | BytecodeOpcode::SarInt64
            | BytecodeOpcode::RolInt32
            | BytecodeOpcode::RorInt32
            | BytecodeOpcode::RolInt64
            | BytecodeOpcode::RorInt64
            | BytecodeOpcode::InstanceOf
            | BytecodeOpcode::LoadEnumVariant
            | BytecodeOpcode::LoadStructField
            | BytecodeOpcode::LoadField
            | BytecodeOpcode::StoreField
            | BytecodeOpcode::TestIdentity
            | BytecodeOpcode::TestEqBool
            | BytecodeOpcode::TestNeBool
            | BytecodeOpcode::TestEqUInt8
            | BytecodeOpcode::TestNeUInt8
            | BytecodeOpcode::TestGtUInt8
            | BytecodeOpcode::TestGeUInt8
            | BytecodeOpcode::TestLtUInt8
            | BytecodeOpcode::TestLeUInt8
            | BytecodeOpcode::TestEqChar
            | BytecodeOpcode::TestNeChar
            | BytecodeOpcode::TestGtChar
            | BytecodeOpcode::TestGeChar
            | BytecodeOpcode::TestLtChar
            | BytecodeOpcode::TestLeChar
            | BytecodeOpcode::TestEqEnum
            | BytecodeOpcode::TestNeEnum
            | BytecodeOpcode::TestEqInt32
            | BytecodeOpcode::TestNeInt32
            | BytecodeOpcode::TestGtInt32
            | BytecodeOpcode::TestGeInt32
            | BytecodeOpcode::TestLtInt32
            | BytecodeOpcode::TestLeInt32
            | BytecodeOpcode::TestEqInt64
            | BytecodeOpcode::TestNeInt64
            | BytecodeOpcode::TestGtInt64
            | BytecodeOpcode::TestGeInt64
            | BytecodeOpcode::TestLtInt64
            | BytecodeOpcode::TestLeInt64
            | BytecodeOpcode::TestEqFloat32
            | BytecodeOpcode::TestNeFloat32
            | BytecodeOpcode::TestGtFloat32
            | BytecodeOpcode::TestGeFloat32
            | BytecodeOpcode::TestLtFloat32
            | BytecodeOpcode::TestLeFloat32
            | BytecodeOpcode::TestEqFloat64
            | BytecodeOpcode::TestNeFloat64
            | BytecodeOpcode::TestGtFloat64
            | BytecodeOpcode::TestGeFloat64
            | BytecodeOpcode::TestLtFloat64
            | BytecodeOpcode::TestLeFloat64
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
            BytecodeOpcode::DivInt32
            | BytecodeOpcode::DivInt64
            | BytecodeOpcode::ModInt32
            | BytecodeOpcode::ModInt64
            | BytecodeOpcode::CheckedCast
            | BytecodeOpcode::LoadField
            | BytecodeOpcode::StoreField
            | BytecodeOpcode::InvokeDirectVoid
            | BytecodeOpcode::InvokeDirect
            | BytecodeOpcode::InvokeVirtualVoid
            | BytecodeOpcode::InvokeVirtual
            | BytecodeOpcode::InvokeStaticVoid
            | BytecodeOpcode::InvokeStatic
            | BytecodeOpcode::InvokeGenericStaticVoid
            | BytecodeOpcode::InvokeGenericStatic
            | BytecodeOpcode::InvokeGenericDirectVoid
            | BytecodeOpcode::InvokeGenericDirect
            | BytecodeOpcode::NewObject
            | BytecodeOpcode::NewArray
            | BytecodeOpcode::NewEnum
            | BytecodeOpcode::NewTuple
            | BytecodeOpcode::NewStruct
            | BytecodeOpcode::NewTraitObject
            | BytecodeOpcode::NilCheck
            | BytecodeOpcode::ArrayLength
            | BytecodeOpcode::ArrayBoundCheck
            | BytecodeOpcode::LoadArray
            | BytecodeOpcode::StoreArray
            | BytecodeOpcode::LoadEnumElement
            | BytecodeOpcode::LoadEnumVariant
            | BytecodeOpcode::Assert
            | BytecodeOpcode::AddInt32
            | BytecodeOpcode::AddInt64
            | BytecodeOpcode::SubInt32
            | BytecodeOpcode::SubInt64
            | BytecodeOpcode::MulInt32
            | BytecodeOpcode::MulInt64 => true,
            _ => false,
        }
    }
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
}

impl BytecodeFunction {
    pub fn new(
        code: Vec<u8>,
        const_pool: Vec<ConstPoolEntry>,
        registers: Vec<BytecodeType>,
        arguments: u32,
        positions: Vec<(u32, Position)>,
    ) -> BytecodeFunction {
        BytecodeFunction {
            code,
            const_pool,
            registers,
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
        read_opcode_and_width(&self.code, offset.to_usize()).0
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
    Struct,
    StructField,
    Trait,
    Field,
    FieldFixed,
    Generic
});

#[derive(Debug, PartialEq)]
pub enum ConstPoolEntry {
    String(String),
    Float32(f32),
    Float64(f64),
    Int32(i32),
    Int64(i64),
    Char(char),
    Class(ClassId, SourceTypeArray),
    Field(ClassId, SourceTypeArray, FieldId),
    FieldFixed(ClassDefId, FieldId),
    Fct(FctId, SourceTypeArray),
    Generic(TypeParamId, FctId, SourceTypeArray),
    Enum(EnumId, SourceTypeArray),
    EnumVariant(EnumId, SourceTypeArray, usize),
    Struct(StructId, SourceTypeArray),
    StructField(StructId, SourceTypeArray, StructDefinitionFieldId),
    Trait(TraitId, SourceTypeArray, SourceType),
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
