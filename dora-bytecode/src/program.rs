use crate::{BytecodeFunction, BytecodeType, Location};
use bincode::{Decode, Encode};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Decode, Encode)]
pub struct PackageId(pub u32);

#[derive(Debug, Decode, Encode)]
pub struct PackageData {
    pub name: String,
    pub root_module_id: ModuleId,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Decode, Encode)]
pub struct ModuleId(pub u32);

#[derive(Debug, Decode, Encode)]
pub struct ModuleData {
    pub name: String,
    pub parent_id: Option<ModuleId>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Decode, Encode)]
pub struct FunctionId(pub u32);

#[derive(Debug, Decode, Encode)]
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
    pub is_internal: bool,
    pub intrinsic: Option<Intrinsic>,
    pub vtable_index: Option<u32>,
    pub is_test: bool,
    pub is_optimize_immediately: bool,
    pub is_variadic: bool,
    pub is_force_inline: bool,
    pub is_never_inline: bool,
    pub bytecode: Option<BytecodeFunction>,
}

#[derive(Debug, Decode, Encode)]
pub enum FunctionKind {
    Impl(ImplId),
    Lambda,
    Trait(TraitId),
    Extension(ExtensionId),
    Function,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Decode, Encode)]
pub struct GlobalId(pub u32);

#[derive(Debug, Decode, Encode)]
pub struct GlobalData {
    pub module_id: ModuleId,
    pub ty: BytecodeType,
    pub mutable: bool,
    pub name: String,
    pub initial_value: Option<FunctionId>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Decode, Encode)]
pub struct ClassId(pub u32);

#[derive(Debug, Decode, Encode)]
pub struct ClassData {
    pub module_id: ModuleId,
    pub name: String,
    pub type_params: TypeParamData,
    pub fields: Vec<ClassField>,
}

#[derive(Debug, Decode, Encode)]
pub struct ClassField {
    pub ty: BytecodeType,
    pub name: String,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Decode, Encode)]
pub struct StructId(pub u32);

#[derive(Debug, Decode, Encode)]
pub struct StructData {
    pub module_id: ModuleId,
    pub name: String,
    pub type_params: TypeParamData,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Decode, Encode)]
pub struct StructField {
    pub ty: BytecodeType,
    pub name: String,
}

#[derive(Debug, Decode, Encode)]
pub struct TypeParamData {
    pub names: Vec<String>,
    pub bounds: Vec<TypeParamBound>,
}

#[derive(Debug, Decode, Encode)]
pub struct TypeParamBound {
    pub ty: BytecodeType,
    pub trait_ty: BytecodeType,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Decode, Encode)]
pub struct EnumId(pub u32);

#[derive(Debug, Decode, Encode)]
pub struct EnumData {
    pub module_id: ModuleId,
    pub name: String,
    pub type_params: TypeParamData,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Decode, Encode)]
pub struct EnumVariant {
    pub name: String,
    pub arguments: Vec<BytecodeType>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Decode, Encode)]
pub struct TraitId(pub u32);

#[derive(Debug, Decode, Encode)]
pub struct TraitData {
    pub module_id: ModuleId,
    pub name: String,
    pub type_params: TypeParamData,
    pub methods: Vec<FunctionId>,
}

#[derive(Copy, Clone, Debug, Decode, Encode)]
pub struct SourceFileId(pub u32);

#[derive(Debug, Decode, Encode)]
pub struct SourceFileData {
    pub path: String,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Decode, Encode)]
pub struct ExtensionId(pub u32);

#[derive(Debug, Decode, Encode)]
pub struct ExtensionData {
    pub module_id: ModuleId,
    pub type_params: TypeParamData,
    pub extended_ty: BytecodeType,
    pub methods: Vec<FunctionId>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Decode, Encode)]
pub struct ImplId(pub u32);

#[derive(Debug, Decode, Encode)]
pub struct ImplData {
    pub module_id: ModuleId,
    pub type_params: TypeParamData,
    pub trait_ty: BytecodeType,
    pub extended_ty: BytecodeType,
    pub methods: Vec<FunctionId>,
    pub trait_method_map: Vec<(FunctionId, FunctionId)>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Decode, Encode)]
pub struct AliasId(pub u32);

#[derive(Debug, Decode, Encode)]
pub struct AliasData {
    pub name: String,
    pub ty: Option<BytecodeType>,
}

#[derive(Debug, Decode, Encode)]
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
    pub extensions: Vec<ExtensionData>,
    pub aliases: Vec<AliasData>,
    pub source_files: Vec<SourceFileData>,
    pub stdlib_package_id: PackageId,
    pub program_package_id: PackageId,
    pub boots_package_id: Option<PackageId>,
    pub main_fct_id: Option<FunctionId>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Decode, Encode)]
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
