use crate::{BytecodeFunction, BytecodeType, Location};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct PackageId(pub u32);

#[derive(Debug)]
pub struct PackageData {
    pub name: String,
    pub root_module_id: ModuleId,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ModuleId(pub u32);

#[derive(Debug)]
pub struct ModuleData {
    pub name: String,
    pub parent_id: Option<ModuleId>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionId(pub u32);

#[derive(Debug)]
pub struct FunctionData {
    pub name: String,
    pub file_id: SourceFileId,
    pub loc: Location,
    pub kind: FunctionKind,
    pub package_id: PackageId,
    pub module_id: ModuleId,
    pub type_params: TypeParamData,
    pub source_file_id: Option<SourceFileId>,
    pub params: Vec<BytecodeType>,
    pub return_type: BytecodeType,
    pub native: Option<NativeFunction>,
    pub internal: Option<InternalFunction>,
    pub intrinsic: Option<Intrinsic>,
    pub vtable_index: Option<u32>,
    pub is_test: bool,
    pub is_optimize_immediately: bool,
    pub is_variadic: bool,
    pub bytecode: Option<BytecodeFunction>,
}

#[derive(Debug)]
pub enum FunctionKind {
    Impl(ImplId),
    Lambda(FunctionId),
    Trait(TraitId),
    Method,
    Function,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct GlobalId(pub u32);

#[derive(Debug)]
pub struct GlobalData {
    pub module_id: ModuleId,
    pub ty: BytecodeType,
    pub mutable: bool,
    pub name: String,
    pub initializer: Option<FunctionId>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ClassId(pub u32);

#[derive(Debug)]
pub struct ClassData {
    pub module_id: ModuleId,
    pub name: String,
    pub type_params: TypeParamData,
    pub layout: ClassLayout,
    pub fields: Vec<ClassField>,
    pub internal: Option<InternalClass>,
}

#[derive(Debug)]
pub enum ClassLayout {
    Regular,
    Array,
    String,
}

impl ClassLayout {
    pub fn is_regular(&self) -> bool {
        match self {
            ClassLayout::Regular => true,
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool {
        match self {
            ClassLayout::Array => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            ClassLayout::String => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct ClassField {
    pub ty: BytecodeType,
    pub name: String,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructId(pub u32);

#[derive(Debug)]
pub struct StructData {
    pub module_id: ModuleId,
    pub name: String,
    pub type_params: TypeParamData,
    pub fields: Vec<StructField>,
}

#[derive(Debug)]
pub struct StructField {
    pub ty: BytecodeType,
    pub name: String,
}

#[derive(Debug)]
pub struct TypeParamData {
    pub names: Vec<String>,
    pub bounds: Vec<TypeParamBound>,
}

#[derive(Debug)]
pub struct TypeParamBound {
    pub ty: BytecodeType,
    pub trait_ty: BytecodeType,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct EnumId(pub u32);

#[derive(Debug)]
pub struct EnumData {
    pub module_id: ModuleId,
    pub name: String,
    pub type_params: TypeParamData,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug)]
pub struct EnumVariant {
    pub name: String,
    pub arguments: Vec<BytecodeType>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TraitId(pub u32);

#[derive(Debug)]
pub struct TraitData {
    pub module_id: ModuleId,
    pub name: String,
    pub type_params: TypeParamData,
    pub methods: Vec<FunctionId>,
}

#[derive(Copy, Clone, Debug)]
pub struct SourceFileId(pub u32);

#[derive(Debug)]
pub struct SourceFileData {
    pub path: String,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ImplId(pub u32);

#[derive(Debug)]
pub struct ImplData {
    pub module_id: ModuleId,
    pub type_params: TypeParamData,
    pub trait_ty: BytecodeType,
    pub extended_ty: BytecodeType,
    pub methods: Vec<FunctionId>,
}

#[derive(Debug)]
pub struct Program {
    pub packages: Vec<PackageData>,
    pub modules: Vec<ModuleData>,
    pub functions: Vec<FunctionData>,
    pub globals: Vec<GlobalData>,
    pub classes: Vec<ClassData>,
    pub structs: Vec<StructData>,
    pub enums: Vec<EnumData>,
    pub traits: Vec<TraitData>,
    pub impls: Vec<ImplData>,
    pub source_files: Vec<SourceFileData>,
    pub stdlib_package_id: PackageId,
    pub program_package_id: PackageId,
    pub boots_package_id: Option<PackageId>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum InternalClass {
    Array,
    String,
    Thread,
    StacktraceElement,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum InternalFunction {
    StacktraceRetrieve,
    BootsCompile,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum NativeFunction {
    FatalError,
    Abort,
    Exit,
    Print,
    PrintLn,
    Argc,
    Argv,
    ForceCollect,
    Timestamp,
    ForceMinorCollect,
    Sleep,
    UInt8ToString,
    CharToString,
    Int32ToString,
    Int64ToString,
    StringCompareTo,
    StringToInt32Success,
    StringToInt64Success,
    StringToFloat32Success,
    StringToFloat64Success,
    StringToInt32OrZero,
    StringToInt64OrZero,
    StringToFloat32OrZero,
    StringToFloat64OrZero,
    StringPlus,
    Float32ToString,
    Float64ToString,
    StringFromBytesPart,
    StringFromStringPart,
    RetrieveStacktrace,
    GetStackTraceElement,
    SpawnThread,
    ThreadJoin,
    MutexWait,
    MutexNotify,
    ConditionEnqueue,
    ConditionBlock,
    ConditionWakupOne,
    ConditionWakupAll,
    ReadFileAsString,
    ReadFileAsBytes,
    WriteFileAsString,
    WriteFileAsBytes,
    SocketConnect,
    SocketClose,
    SocketWrite,
    SocketRead,
    SocketBind,
    SocketAccept,
    StringClone,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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
    StrSet,

    BoolEq,
    BoolNot,
    BoolToInt32,
    BoolToInt64,

    ByteEq,
    ByteCmp,
    ByteToChar,
    ByteToInt32,
    ByteToInt64,

    CharEq,
    CharCmp,
    CharToInt32,
    CharToInt64,

    Int32ToInt32,

    Int32ToByte,
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
    Int32Plus,

    Int32CountZeroBits,
    Int32CountOneBits,
    Int32CountZeroBitsLeading,
    Int32CountOneBitsLeading,
    Int32CountZeroBitsTrailing,
    Int32CountOneBitsTrailing,

    Int64ToInt32,
    Int64ToChar,
    Int64ToByte,
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
    Int64Plus,

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

    Float32Plus,
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

    Float64Plus,
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
