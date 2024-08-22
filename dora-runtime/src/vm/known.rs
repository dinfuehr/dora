use parking_lot::Mutex;

use std::cell::OnceCell;
use std::collections::HashMap;

use crate::gc::Address;
use crate::vm::ClassInstanceId;
use dora_bytecode::{ClassId, FunctionId, Intrinsic as BytecodeIntrinsic, TraitId};

#[derive(Debug)]
pub struct KnownElements {
    pub byte_array_class_instance_id: Mutex<Option<ClassInstanceId>>,
    pub int_array_class_instance_id: Mutex<Option<ClassInstanceId>>,
    pub string_class_instance_id: Mutex<Option<ClassInstanceId>>,
    pub ste_class_instance_id: Mutex<Option<ClassInstanceId>>,

    pub filler_word_class_instance_id: Option<ClassInstanceId>,
    pub filler_array_class_instance_id: Option<ClassInstanceId>,
    pub free_space_class_instance_id: Option<ClassInstanceId>,
    pub code_class_instance_id: Option<ClassInstanceId>,

    pub filler_word_class_address: Address,
    pub filler_array_class_address: Address,
    pub free_space_class_address: Address,

    pub zero_trait_id: Option<TraitId>,
    pub array_class_id: Option<ClassId>,
    pub string_class_id: Option<ClassId>,
    pub thread_class_id: Option<ClassId>,
    pub boots_compile_fct_id: Option<FunctionId>,
    pub boots_compile_fct_address: OnceCell<Address>,
    pub boots_test_addresses: OnceCell<HashMap<FunctionId, Address>>,
    pub unreachable_fct_id: Option<FunctionId>,
    pub fatal_error_fct_id: Option<FunctionId>,
}

impl KnownElements {
    pub fn new() -> KnownElements {
        KnownElements {
            byte_array_class_instance_id: Mutex::new(None),
            int_array_class_instance_id: Mutex::new(None),
            string_class_instance_id: Mutex::new(None),
            ste_class_instance_id: Mutex::new(None),

            filler_word_class_instance_id: None,
            filler_array_class_instance_id: None,
            free_space_class_instance_id: None,
            code_class_instance_id: None,

            free_space_class_address: Address::null(),
            filler_word_class_address: Address::null(),
            filler_array_class_address: Address::null(),

            zero_trait_id: None,
            array_class_id: None,
            string_class_id: None,
            thread_class_id: None,
            boots_compile_fct_id: None,
            boots_compile_fct_address: OnceCell::new(),
            boots_test_addresses: OnceCell::new(),
            unreachable_fct_id: None,
            fatal_error_fct_id: None,
        }
    }

    pub fn filler_word_class_instance(&self) -> ClassInstanceId {
        self.filler_word_class_instance_id.expect("uninitialized")
    }

    pub fn filler_word_class_address(&self) -> Address {
        self.filler_word_class_address
    }

    pub fn filler_array_class_instance(&self) -> ClassInstanceId {
        self.filler_array_class_instance_id.expect("uninitialized")
    }

    pub fn filler_array_class_address(&self) -> Address {
        self.filler_array_class_address
    }

    pub fn free_space_class_instance(&self) -> ClassInstanceId {
        self.free_space_class_instance_id.expect("uninitialized")
    }

    pub fn free_space_class_address(&self) -> Address {
        self.free_space_class_address
    }

    pub fn code_class_instance(&self) -> ClassInstanceId {
        self.code_class_instance_id.expect("uninitialized")
    }

    pub fn zero_trait_id(&self) -> TraitId {
        self.zero_trait_id.expect("uninitialized")
    }

    pub fn array_class_id(&self) -> ClassId {
        self.array_class_id.expect("uninitialized")
    }

    pub fn string_class_id(&self) -> ClassId {
        self.string_class_id.expect("uninitialized")
    }

    pub fn thread_class_id(&self) -> ClassId {
        self.thread_class_id.expect("uninitialized")
    }

    pub fn boots_compile_fct_id(&self) -> FunctionId {
        self.boots_compile_fct_id.expect("uninitialized")
    }

    pub fn boots_compile_fct_address(&self) -> Address {
        self.boots_compile_fct_address
            .get()
            .cloned()
            .expect("uninitialized")
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Intrinsic {
    ArrayNewOfSize,
    ArrayWithValues,
    ArrayLen,
    ArrayGet,
    ArraySet,

    Unreachable,
    FatalError,
    UnsafeKillRefs,

    Assert,
    Debug,

    StrLen,
    StrGet,

    BoolEq,
    BoolNot,
    BoolToInt32,
    BoolToInt64,

    UInt8Eq,
    UInt8Cmp,
    UInt8ToChar,
    UInt8ToInt32,
    UInt8ToInt64,

    CharEq,
    CharCmp,
    CharToInt32,
    CharToInt64,

    Int32ToUInt8,
    Int32ToCharUnchecked,
    Int32ToInt64,
    Int32ToFloat32,
    Int32ToFloat64,
    ReinterpretInt32AsFloat32,

    EnumEq,
    EnumNe,

    Int32Eq,
    Int32Cmp,

    Int32Add,
    Int32AddUnchecked,
    Int32Sub,
    Int32SubUnchecked,
    Int32Mul,
    Int32MulUnchecked,
    Int32Div,
    Int32Mod,

    Int32Or,
    Int32And,
    Int32Xor,

    Int32Shl,
    Int32Sar,
    Int32Shr,

    Int32RotateLeft,
    Int32RotateRight,

    Int32Not,
    Int32Neg,
    Int32NegUnchecked,

    Int32CountZeroBits,
    Int32CountOneBits,
    Int32CountZeroBitsLeading,
    Int32CountOneBitsLeading,
    Int32CountZeroBitsTrailing,
    Int32CountOneBitsTrailing,

    Int64ToInt32,
    Int64ToCharUnchecked,
    Int64ToUInt8,
    Int64ToFloat32,
    Int64ToFloat64,
    ReinterpretInt64AsFloat64,

    Int64Eq,
    Int64Cmp,

    Int64Add,
    Int64AddUnchecked,
    Int64Sub,
    Int64SubUnchecked,
    Int64Mul,
    Int64MulUnchecked,
    Int64Div,
    Int64Mod,

    Int64Or,
    Int64And,
    Int64Xor,

    Int64Shl,
    Int64Sar,
    Int64Shr,

    Int64RotateLeft,
    Int64RotateRight,

    Int64Not,
    Int64Neg,
    Int64NegUnchecked,

    Int64CountZeroBits,
    Int64CountOneBits,
    Int64CountZeroBitsLeading,
    Int64CountOneBitsLeading,
    Int64CountZeroBitsTrailing,
    Int64CountOneBitsTrailing,

    Float32ToInt32,
    Float32ToInt64,
    PromoteFloat32ToFloat64,
    ReinterpretFloat32AsInt32,

    Float32Eq,
    Float32Cmp,

    Float32Add,
    Float32Sub,
    Float32Mul,
    Float32Div,

    Float32Neg,
    Float32Abs,
    Float32IsNan,

    Float32RoundToZero,
    Float32RoundUp,
    Float32RoundDown,
    Float32RoundHalfEven,

    Float32Sqrt,

    Float64ToInt32,
    Float64ToInt64,
    DemoteFloat64ToFloat32,
    ReinterpretFloat64AsInt64,

    Float64Eq,
    Float64Cmp,

    Float64Add,
    Float64Sub,
    Float64Mul,
    Float64Div,

    Float64Neg,
    Float64Abs,
    Float64IsNan,

    Float64RoundToZero,
    Float64RoundUp,
    Float64RoundDown,
    Float64RoundHalfEven,

    Float64Sqrt,

    OptionGetOrPanic,
    OptionIsNone,
    OptionIsSome,

    AtomicInt32Get,
    AtomicInt32Set,
    AtomicInt32Exchange,
    AtomicInt32CompareExchange,
    AtomicInt32FetchAdd,

    AtomicInt64Get,
    AtomicInt64Set,
    AtomicInt64Exchange,
    AtomicInt64CompareExchange,
    AtomicInt64FetchAdd,

    ThreadCurrent,
}

impl Intrinsic {
    pub fn from_bytecode(original: BytecodeIntrinsic) -> Option<Intrinsic> {
        match original {
            BytecodeIntrinsic::ArrayNewOfSize => None,
            BytecodeIntrinsic::ArrayWithValues => None,
            BytecodeIntrinsic::ArrayLen => None,
            BytecodeIntrinsic::ArrayGet => None,
            BytecodeIntrinsic::ArraySet => None,

            BytecodeIntrinsic::Unreachable => unreachable!(),
            BytecodeIntrinsic::UnsafeKillRefs => unreachable!(),

            BytecodeIntrinsic::Assert => unreachable!(),
            BytecodeIntrinsic::Debug => unreachable!(),

            BytecodeIntrinsic::StrLen => None,
            BytecodeIntrinsic::StrGet => None,

            BytecodeIntrinsic::BoolEq => None,
            BytecodeIntrinsic::BoolNot => None,
            BytecodeIntrinsic::BoolToInt32 => None,
            BytecodeIntrinsic::BoolToInt64 => None,

            BytecodeIntrinsic::UInt8Eq => None,
            BytecodeIntrinsic::UInt8Cmp => None,
            BytecodeIntrinsic::UInt8ToChar => None,
            BytecodeIntrinsic::UInt8ToInt32 => None,
            BytecodeIntrinsic::UInt8ToInt64 => None,

            BytecodeIntrinsic::CharEq => None,
            BytecodeIntrinsic::CharCmp => None,
            BytecodeIntrinsic::CharToInt32 => None,
            BytecodeIntrinsic::CharToInt64 => None,

            BytecodeIntrinsic::Int32ToUInt8 => None,
            BytecodeIntrinsic::Int32ToChar => None,
            BytecodeIntrinsic::Int32ToInt64 => None,
            BytecodeIntrinsic::Int32ToFloat32 => None,
            BytecodeIntrinsic::Int32ToFloat64 => None,
            BytecodeIntrinsic::ReinterpretInt32AsFloat32 => None,

            BytecodeIntrinsic::Int32Eq => None,
            BytecodeIntrinsic::Int32Cmp => None,

            BytecodeIntrinsic::Int32Add => None,
            BytecodeIntrinsic::Int32AddUnchecked => None,
            BytecodeIntrinsic::Int32Sub => None,
            BytecodeIntrinsic::Int32SubUnchecked => None,
            BytecodeIntrinsic::Int32Mul => None,
            BytecodeIntrinsic::Int32MulUnchecked => None,
            BytecodeIntrinsic::Int32Div => None,
            BytecodeIntrinsic::Int32Mod => None,

            BytecodeIntrinsic::Int32Or => None,
            BytecodeIntrinsic::Int32And => None,
            BytecodeIntrinsic::Int32Xor => None,

            BytecodeIntrinsic::Int32Shl => None,
            BytecodeIntrinsic::Int32Sar => None,
            BytecodeIntrinsic::Int32Shr => None,

            BytecodeIntrinsic::Int32RotateLeft => None,
            BytecodeIntrinsic::Int32RotateRight => None,

            BytecodeIntrinsic::Int32Not => None,
            BytecodeIntrinsic::Int32Neg => None,
            BytecodeIntrinsic::Int32NegUnchecked => None,

            BytecodeIntrinsic::Int32CountZeroBits => None,
            BytecodeIntrinsic::Int32CountOneBits => None,
            BytecodeIntrinsic::Int32CountZeroBitsLeading => None,
            BytecodeIntrinsic::Int32CountOneBitsLeading => None,
            BytecodeIntrinsic::Int32CountZeroBitsTrailing => None,
            BytecodeIntrinsic::Int32CountOneBitsTrailing => None,

            BytecodeIntrinsic::Int64ToInt32 => None,
            BytecodeIntrinsic::Int64ToChar => None,
            BytecodeIntrinsic::Int64ToUInt8 => None,
            BytecodeIntrinsic::Int64ToFloat32 => None,
            BytecodeIntrinsic::Int64ToFloat64 => None,
            BytecodeIntrinsic::ReinterpretInt64AsFloat64 => None,

            BytecodeIntrinsic::Int64Eq => None,
            BytecodeIntrinsic::Int64Cmp => None,

            BytecodeIntrinsic::Int64Add => None,
            BytecodeIntrinsic::Int64AddUnchecked => None,
            BytecodeIntrinsic::Int64Sub => None,
            BytecodeIntrinsic::Int64SubUnchecked => None,
            BytecodeIntrinsic::Int64Mul => None,
            BytecodeIntrinsic::Int64MulUnchecked => None,
            BytecodeIntrinsic::Int64Div => None,
            BytecodeIntrinsic::Int64Mod => None,

            BytecodeIntrinsic::Int64Or => None,
            BytecodeIntrinsic::Int64And => None,
            BytecodeIntrinsic::Int64Xor => None,

            BytecodeIntrinsic::Int64Shl => None,
            BytecodeIntrinsic::Int64Sar => None,
            BytecodeIntrinsic::Int64Shr => None,

            BytecodeIntrinsic::Int64RotateLeft => None,
            BytecodeIntrinsic::Int64RotateRight => None,

            BytecodeIntrinsic::Int64Not => None,
            BytecodeIntrinsic::Int64Neg => None,
            BytecodeIntrinsic::Int64NegUnchecked => None,

            BytecodeIntrinsic::Int64CountZeroBits => None,
            BytecodeIntrinsic::Int64CountOneBits => None,
            BytecodeIntrinsic::Int64CountZeroBitsLeading => None,
            BytecodeIntrinsic::Int64CountOneBitsLeading => None,
            BytecodeIntrinsic::Int64CountZeroBitsTrailing => None,
            BytecodeIntrinsic::Int64CountOneBitsTrailing => None,

            BytecodeIntrinsic::EnumEq => unreachable!(),
            BytecodeIntrinsic::EnumNe => unreachable!(),

            BytecodeIntrinsic::Float32ToInt32 => None,
            BytecodeIntrinsic::Float32ToInt64 => None,
            BytecodeIntrinsic::PromoteFloat32ToFloat64 => None,
            BytecodeIntrinsic::ReinterpretFloat32AsInt32 => None,

            BytecodeIntrinsic::Float32Eq => None,
            BytecodeIntrinsic::Float32Cmp => None,

            BytecodeIntrinsic::Float32Add => None,
            BytecodeIntrinsic::Float32Sub => None,
            BytecodeIntrinsic::Float32Mul => None,
            BytecodeIntrinsic::Float32Div => None,

            BytecodeIntrinsic::Float32Neg => None,
            BytecodeIntrinsic::Float32Abs => None,
            BytecodeIntrinsic::Float32IsNan => None,

            BytecodeIntrinsic::Float32RoundToZero => None,
            BytecodeIntrinsic::Float32RoundUp => None,
            BytecodeIntrinsic::Float32RoundDown => None,
            BytecodeIntrinsic::Float32RoundHalfEven => None,

            BytecodeIntrinsic::Float32Sqrt => None,

            BytecodeIntrinsic::Float64ToInt32 => None,
            BytecodeIntrinsic::Float64ToInt64 => None,
            BytecodeIntrinsic::DemoteFloat64ToFloat32 => None,
            BytecodeIntrinsic::ReinterpretFloat64AsInt64 => None,

            BytecodeIntrinsic::Float64Eq => None,
            BytecodeIntrinsic::Float64Cmp => None,

            BytecodeIntrinsic::Float64Add => None,
            BytecodeIntrinsic::Float64Sub => None,
            BytecodeIntrinsic::Float64Mul => None,
            BytecodeIntrinsic::Float64Div => None,

            BytecodeIntrinsic::Float64Neg => None,
            BytecodeIntrinsic::Float64Abs => None,
            BytecodeIntrinsic::Float64IsNan => None,

            BytecodeIntrinsic::Float64RoundToZero => None,
            BytecodeIntrinsic::Float64RoundUp => None,
            BytecodeIntrinsic::Float64RoundDown => None,
            BytecodeIntrinsic::Float64RoundHalfEven => None,

            BytecodeIntrinsic::Float64Sqrt => None,

            BytecodeIntrinsic::OptionGetOrPanic => None,
            BytecodeIntrinsic::OptionIsNone => None,
            BytecodeIntrinsic::OptionIsSome => None,

            BytecodeIntrinsic::AtomicInt32Get => None,
            BytecodeIntrinsic::AtomicInt32Set => None,
            BytecodeIntrinsic::AtomicInt32Exchange => None,
            BytecodeIntrinsic::AtomicInt32CompareExchange => None,
            BytecodeIntrinsic::AtomicInt32FetchAdd => None,

            BytecodeIntrinsic::AtomicInt64Get => None,
            BytecodeIntrinsic::AtomicInt64Set => None,
            BytecodeIntrinsic::AtomicInt64Exchange => None,
            BytecodeIntrinsic::AtomicInt64CompareExchange => None,
            BytecodeIntrinsic::AtomicInt64FetchAdd => None,

            BytecodeIntrinsic::ThreadCurrent => None,
        }
    }
}
