use parking_lot::Mutex;

use std::cell::OnceCell;
use std::collections::HashMap;

use crate::gc::Address;
use crate::vm::ClassInstanceId;
use dora_bytecode::{ClassId, FunctionId, TraitId};

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
