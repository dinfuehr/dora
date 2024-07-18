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
    Int32ToChar,
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
    Int64ToChar,
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

            BytecodeIntrinsic::StrLen => Some(Intrinsic::StrLen),
            BytecodeIntrinsic::StrGet => Some(Intrinsic::StrGet),

            BytecodeIntrinsic::BoolEq => Some(Intrinsic::BoolEq),
            BytecodeIntrinsic::BoolNot => Some(Intrinsic::BoolNot),
            BytecodeIntrinsic::BoolToInt32 => Some(Intrinsic::BoolToInt32),
            BytecodeIntrinsic::BoolToInt64 => Some(Intrinsic::BoolToInt64),

            BytecodeIntrinsic::UInt8Eq => None,
            BytecodeIntrinsic::UInt8Cmp => None,
            BytecodeIntrinsic::UInt8ToChar => Some(Intrinsic::UInt8ToChar),
            BytecodeIntrinsic::UInt8ToInt32 => Some(Intrinsic::UInt8ToInt32),
            BytecodeIntrinsic::UInt8ToInt64 => Some(Intrinsic::UInt8ToInt64),

            BytecodeIntrinsic::CharEq => Some(Intrinsic::CharEq),
            BytecodeIntrinsic::CharCmp => Some(Intrinsic::CharCmp),
            BytecodeIntrinsic::CharToInt32 => Some(Intrinsic::CharToInt32),
            BytecodeIntrinsic::CharToInt64 => Some(Intrinsic::CharToInt64),

            BytecodeIntrinsic::Int32ToUInt8 => Some(Intrinsic::Int32ToUInt8),
            BytecodeIntrinsic::Int32ToChar => Some(Intrinsic::Int32ToChar),
            BytecodeIntrinsic::Int32ToInt64 => Some(Intrinsic::Int32ToInt64),
            BytecodeIntrinsic::Int32ToFloat32 => Some(Intrinsic::Int32ToFloat32),
            BytecodeIntrinsic::Int32ToFloat64 => Some(Intrinsic::Int32ToFloat64),
            BytecodeIntrinsic::ReinterpretInt32AsFloat32 => {
                Some(Intrinsic::ReinterpretInt32AsFloat32)
            }

            BytecodeIntrinsic::EnumEq => Some(Intrinsic::EnumEq),
            BytecodeIntrinsic::EnumNe => Some(Intrinsic::EnumNe),

            BytecodeIntrinsic::Int32Eq => Some(Intrinsic::Int32Eq),
            BytecodeIntrinsic::Int32Cmp => Some(Intrinsic::Int32Cmp),

            BytecodeIntrinsic::Int32Add => Some(Intrinsic::Int32Add),
            BytecodeIntrinsic::Int32AddUnchecked => Some(Intrinsic::Int32AddUnchecked),
            BytecodeIntrinsic::Int32Sub => Some(Intrinsic::Int32Sub),
            BytecodeIntrinsic::Int32SubUnchecked => Some(Intrinsic::Int32SubUnchecked),
            BytecodeIntrinsic::Int32Mul => Some(Intrinsic::Int32Mul),
            BytecodeIntrinsic::Int32MulUnchecked => Some(Intrinsic::Int32MulUnchecked),
            BytecodeIntrinsic::Int32Div => Some(Intrinsic::Int32Div),
            BytecodeIntrinsic::Int32Mod => Some(Intrinsic::Int32Mod),

            BytecodeIntrinsic::Int32Or => Some(Intrinsic::Int32Or),
            BytecodeIntrinsic::Int32And => Some(Intrinsic::Int32And),
            BytecodeIntrinsic::Int32Xor => Some(Intrinsic::Int32Xor),

            BytecodeIntrinsic::Int32Shl => Some(Intrinsic::Int32Shl),
            BytecodeIntrinsic::Int32Sar => Some(Intrinsic::Int32Sar),
            BytecodeIntrinsic::Int32Shr => Some(Intrinsic::Int32Shr),

            BytecodeIntrinsic::Int32RotateLeft => Some(Intrinsic::Int32RotateLeft),
            BytecodeIntrinsic::Int32RotateRight => Some(Intrinsic::Int32RotateRight),

            BytecodeIntrinsic::Int32Not => Some(Intrinsic::Int32Not),
            BytecodeIntrinsic::Int32Neg => Some(Intrinsic::Int32Neg),
            BytecodeIntrinsic::Int32NegUnchecked => Some(Intrinsic::Int32NegUnchecked),

            BytecodeIntrinsic::Int32CountZeroBits => Some(Intrinsic::Int32CountZeroBits),
            BytecodeIntrinsic::Int32CountOneBits => Some(Intrinsic::Int32CountOneBits),
            BytecodeIntrinsic::Int32CountZeroBitsLeading => {
                Some(Intrinsic::Int32CountZeroBitsLeading)
            }
            BytecodeIntrinsic::Int32CountOneBitsLeading => {
                Some(Intrinsic::Int32CountOneBitsLeading)
            }
            BytecodeIntrinsic::Int32CountZeroBitsTrailing => {
                Some(Intrinsic::Int32CountZeroBitsTrailing)
            }
            BytecodeIntrinsic::Int32CountOneBitsTrailing => {
                Some(Intrinsic::Int32CountOneBitsTrailing)
            }

            BytecodeIntrinsic::Int64ToInt32 => Some(Intrinsic::Int64ToInt32),
            BytecodeIntrinsic::Int64ToChar => Some(Intrinsic::Int64ToChar),
            BytecodeIntrinsic::Int64ToUInt8 => Some(Intrinsic::Int64ToUInt8),
            BytecodeIntrinsic::Int64ToFloat32 => Some(Intrinsic::Int64ToFloat32),
            BytecodeIntrinsic::Int64ToFloat64 => Some(Intrinsic::Int64ToFloat64),
            BytecodeIntrinsic::ReinterpretInt64AsFloat64 => {
                Some(Intrinsic::ReinterpretInt64AsFloat64)
            }

            BytecodeIntrinsic::Int64Eq => Some(Intrinsic::Int64Eq),
            BytecodeIntrinsic::Int64Cmp => Some(Intrinsic::Int64Cmp),

            BytecodeIntrinsic::Int64Add => Some(Intrinsic::Int64Add),
            BytecodeIntrinsic::Int64AddUnchecked => Some(Intrinsic::Int64AddUnchecked),
            BytecodeIntrinsic::Int64Sub => Some(Intrinsic::Int64Sub),
            BytecodeIntrinsic::Int64SubUnchecked => Some(Intrinsic::Int64SubUnchecked),
            BytecodeIntrinsic::Int64Mul => Some(Intrinsic::Int64Mul),
            BytecodeIntrinsic::Int64MulUnchecked => Some(Intrinsic::Int64MulUnchecked),
            BytecodeIntrinsic::Int64Div => Some(Intrinsic::Int64Div),
            BytecodeIntrinsic::Int64Mod => Some(Intrinsic::Int64Mod),

            BytecodeIntrinsic::Int64Or => Some(Intrinsic::Int64Or),
            BytecodeIntrinsic::Int64And => Some(Intrinsic::Int64And),
            BytecodeIntrinsic::Int64Xor => Some(Intrinsic::Int64Xor),

            BytecodeIntrinsic::Int64Shl => Some(Intrinsic::Int64Shl),
            BytecodeIntrinsic::Int64Sar => Some(Intrinsic::Int64Sar),
            BytecodeIntrinsic::Int64Shr => Some(Intrinsic::Int64Shr),

            BytecodeIntrinsic::Int64RotateLeft => Some(Intrinsic::Int64RotateLeft),
            BytecodeIntrinsic::Int64RotateRight => Some(Intrinsic::Int64RotateRight),

            BytecodeIntrinsic::Int64Not => Some(Intrinsic::Int64Not),
            BytecodeIntrinsic::Int64Neg => Some(Intrinsic::Int64Neg),
            BytecodeIntrinsic::Int64NegUnchecked => Some(Intrinsic::Int64NegUnchecked),

            BytecodeIntrinsic::Int64CountZeroBits => Some(Intrinsic::Int64CountZeroBits),
            BytecodeIntrinsic::Int64CountOneBits => Some(Intrinsic::Int64CountOneBits),
            BytecodeIntrinsic::Int64CountZeroBitsLeading => {
                Some(Intrinsic::Int64CountZeroBitsLeading)
            }
            BytecodeIntrinsic::Int64CountOneBitsLeading => {
                Some(Intrinsic::Int64CountOneBitsLeading)
            }
            BytecodeIntrinsic::Int64CountZeroBitsTrailing => {
                Some(Intrinsic::Int64CountZeroBitsTrailing)
            }
            BytecodeIntrinsic::Int64CountOneBitsTrailing => {
                Some(Intrinsic::Int64CountOneBitsTrailing)
            }

            BytecodeIntrinsic::Float32ToInt32 => Some(Intrinsic::Float32ToInt32),
            BytecodeIntrinsic::Float32ToInt64 => Some(Intrinsic::Float32ToInt64),
            BytecodeIntrinsic::PromoteFloat32ToFloat64 => Some(Intrinsic::PromoteFloat32ToFloat64),
            BytecodeIntrinsic::ReinterpretFloat32AsInt32 => {
                Some(Intrinsic::ReinterpretFloat32AsInt32)
            }

            BytecodeIntrinsic::Float32Eq => Some(Intrinsic::Float32Eq),
            BytecodeIntrinsic::Float32Cmp => Some(Intrinsic::Float32Cmp),

            BytecodeIntrinsic::Float32Add => Some(Intrinsic::Float32Add),
            BytecodeIntrinsic::Float32Sub => Some(Intrinsic::Float32Sub),
            BytecodeIntrinsic::Float32Mul => Some(Intrinsic::Float32Mul),
            BytecodeIntrinsic::Float32Div => Some(Intrinsic::Float32Div),

            BytecodeIntrinsic::Float32Neg => Some(Intrinsic::Float32Neg),
            BytecodeIntrinsic::Float32Abs => Some(Intrinsic::Float32Abs),
            BytecodeIntrinsic::Float32IsNan => Some(Intrinsic::Float32IsNan),

            BytecodeIntrinsic::Float32RoundToZero => Some(Intrinsic::Float32RoundToZero),
            BytecodeIntrinsic::Float32RoundUp => Some(Intrinsic::Float32RoundUp),
            BytecodeIntrinsic::Float32RoundDown => Some(Intrinsic::Float32RoundDown),
            BytecodeIntrinsic::Float32RoundHalfEven => Some(Intrinsic::Float32RoundHalfEven),

            BytecodeIntrinsic::Float32Sqrt => Some(Intrinsic::Float32Sqrt),

            BytecodeIntrinsic::Float64ToInt32 => Some(Intrinsic::Float64ToInt32),
            BytecodeIntrinsic::Float64ToInt64 => Some(Intrinsic::Float64ToInt64),
            BytecodeIntrinsic::DemoteFloat64ToFloat32 => Some(Intrinsic::DemoteFloat64ToFloat32),
            BytecodeIntrinsic::ReinterpretFloat64AsInt64 => {
                Some(Intrinsic::ReinterpretFloat64AsInt64)
            }

            BytecodeIntrinsic::Float64Eq => Some(Intrinsic::Float64Eq),
            BytecodeIntrinsic::Float64Cmp => Some(Intrinsic::Float64Cmp),

            BytecodeIntrinsic::Float64Add => Some(Intrinsic::Float64Add),
            BytecodeIntrinsic::Float64Sub => Some(Intrinsic::Float64Sub),
            BytecodeIntrinsic::Float64Mul => Some(Intrinsic::Float64Mul),
            BytecodeIntrinsic::Float64Div => Some(Intrinsic::Float64Div),

            BytecodeIntrinsic::Float64Neg => Some(Intrinsic::Float64Neg),
            BytecodeIntrinsic::Float64Abs => Some(Intrinsic::Float64Abs),
            BytecodeIntrinsic::Float64IsNan => Some(Intrinsic::Float64IsNan),

            BytecodeIntrinsic::Float64RoundToZero => Some(Intrinsic::Float64RoundToZero),
            BytecodeIntrinsic::Float64RoundUp => Some(Intrinsic::Float64RoundUp),
            BytecodeIntrinsic::Float64RoundDown => Some(Intrinsic::Float64RoundDown),
            BytecodeIntrinsic::Float64RoundHalfEven => Some(Intrinsic::Float64RoundHalfEven),

            BytecodeIntrinsic::Float64Sqrt => Some(Intrinsic::Float64Sqrt),

            BytecodeIntrinsic::OptionGetOrPanic => Some(Intrinsic::OptionGetOrPanic),
            BytecodeIntrinsic::OptionIsNone => Some(Intrinsic::OptionIsNone),
            BytecodeIntrinsic::OptionIsSome => Some(Intrinsic::OptionIsSome),

            BytecodeIntrinsic::AtomicInt32Get => Some(Intrinsic::AtomicInt32Get),
            BytecodeIntrinsic::AtomicInt32Set => Some(Intrinsic::AtomicInt32Set),
            BytecodeIntrinsic::AtomicInt32Exchange => Some(Intrinsic::AtomicInt32Exchange),
            BytecodeIntrinsic::AtomicInt32CompareExchange => {
                Some(Intrinsic::AtomicInt32CompareExchange)
            }
            BytecodeIntrinsic::AtomicInt32FetchAdd => Some(Intrinsic::AtomicInt32FetchAdd),

            BytecodeIntrinsic::AtomicInt64Get => Some(Intrinsic::AtomicInt64Get),
            BytecodeIntrinsic::AtomicInt64Set => Some(Intrinsic::AtomicInt64Set),
            BytecodeIntrinsic::AtomicInt64Exchange => Some(Intrinsic::AtomicInt64Exchange),
            BytecodeIntrinsic::AtomicInt64CompareExchange => {
                Some(Intrinsic::AtomicInt64CompareExchange)
            }
            BytecodeIntrinsic::AtomicInt64FetchAdd => Some(Intrinsic::AtomicInt64FetchAdd),

            BytecodeIntrinsic::ThreadCurrent => Some(Intrinsic::ThreadCurrent),
        }
    }
}
