use std::fmt;

use crate::mem::ptr_width;
use crate::semck::specialize::specialize_enum_id_params;
use crate::ty::{MachineMode, SourceType, TypeList, TypeListId};
use crate::vm::{get_vm, ClassId, EnumId, EnumLayout, FctId, FieldId, TupleId, VM};
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
    Enum(EnumId, TypeList),
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
                let edef = edef.read();

                match edef.layout {
                    EnumLayout::Int => 4,
                    EnumLayout::Ptr | EnumLayout::Tagged => ptr_width(),
                }
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
            BytecodeType::TypeParam(_) => unreachable!(),
            BytecodeType::Enum(_, _) => BytecodeTypeKind::Enum,
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
                let edef = edef.read();

                match edef.layout {
                    EnumLayout::Int => MachineMode::Int32,
                    EnumLayout::Ptr | EnumLayout::Tagged => MachineMode::Ptr,
                }
            }
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
            SourceType::Enum(id, list_id) => {
                let xenum = vm.enums[id].read();

                for variant in &xenum.variants {
                    if !variant.types.is_empty() {
                        let type_params = vm.lists.lock().get(list_id);
                        return BytecodeType::Enum(id, type_params);
                    }
                }

                BytecodeType::Int32
            }
            SourceType::Tuple(tuple_id) => BytecodeType::Tuple(tuple_id),
            SourceType::TypeParam(idx) => BytecodeType::TypeParam(idx.to_usize() as u32),
            _ => panic!("BuiltinType {:?} cannot converted to BytecodeType", ty),
        }
    }
}

// Keep in sync with dora-boots/bytecode.dora

#[derive(Copy, Clone, PartialEq, Eq, Debug, FromPrimitive, ToPrimitive)]
pub enum BytecodeOpcode {
    Wide,

    Push,
    Pop,

    AddInt32Stack,

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

    MovBool,
    MovUInt8,
    MovChar,
    MovInt32,
    MovInt64,
    MovFloat32,
    MovFloat64,
    MovPtr,
    MovTuple,
    MovGeneric,
    MovEnum,

    LoadTupleElement,
    LoadEnumElement,
    LoadEnumVariant,

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

    NilCheck,

    ArrayLength,
    ArrayBoundCheck,

    LoadArrayBool,
    LoadArrayUInt8,
    LoadArrayChar,
    LoadArrayInt32,
    LoadArrayInt64,
    LoadArrayFloat32,
    LoadArrayFloat64,
    LoadArrayPtr,
    LoadArrayTuple,
    LoadArrayGeneric,
    LoadArrayEnum,

    StoreArrayBool,
    StoreArrayUInt8,
    StoreArrayChar,
    StoreArrayInt32,
    StoreArrayInt64,
    StoreArrayFloat32,
    StoreArrayFloat64,
    StoreArrayPtr,
    StoreArrayTuple,
    StoreArrayGeneric,
    StoreArrayEnum,

    RetVoid,
    Ret,
}

impl BytecodeOpcode {
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
            | BytecodeOpcode::NilCheck
            | BytecodeOpcode::ArrayLength
            | BytecodeOpcode::ArrayBoundCheck
            | BytecodeOpcode::LoadArrayBool
            | BytecodeOpcode::LoadArrayUInt8
            | BytecodeOpcode::LoadArrayChar
            | BytecodeOpcode::LoadArrayInt32
            | BytecodeOpcode::LoadArrayInt64
            | BytecodeOpcode::LoadArrayFloat32
            | BytecodeOpcode::LoadArrayFloat64
            | BytecodeOpcode::LoadArrayPtr
            | BytecodeOpcode::LoadArrayTuple
            | BytecodeOpcode::LoadArrayGeneric
            | BytecodeOpcode::LoadArrayEnum
            | BytecodeOpcode::StoreArrayBool
            | BytecodeOpcode::StoreArrayUInt8
            | BytecodeOpcode::StoreArrayChar
            | BytecodeOpcode::StoreArrayInt32
            | BytecodeOpcode::StoreArrayInt64
            | BytecodeOpcode::StoreArrayFloat32
            | BytecodeOpcode::StoreArrayFloat64
            | BytecodeOpcode::StoreArrayPtr
            | BytecodeOpcode::StoreArrayTuple
            | BytecodeOpcode::StoreArrayGeneric
            | BytecodeOpcode::StoreArrayEnum
            | BytecodeOpcode::LoadEnumElement
            | BytecodeOpcode::LoadEnumVariant
            | BytecodeOpcode::Assert => true,
            _ => false,
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
}

#[derive(FromPrimitive, ToPrimitive)]
pub enum ConstPoolOpcode {
    String,
    Float32,
    Float64,
    Int32,
    Int64,
    Char,
}

#[derive(Debug, PartialEq)]
pub enum ConstPoolEntry {
    String(String),
    Float32(f32),
    Float64(f64),
    Int32(i32),
    Int64(i64),
    Char(char),
    Class(ClassId, TypeList),
    Field(ClassId, TypeList, FieldId),
    Fct(FctId, TypeList),
    Generic(TypeListId, FctId, TypeList),
    Enum(EnumId, TypeList),
    EnumVariant(EnumId, TypeList, usize),
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
