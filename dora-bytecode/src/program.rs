use crate::BytecodeType;

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
    pub type_params: TypeParamData,
    pub source_file_id: Option<SourceFileId>,
    pub params: Vec<BytecodeType>,
    pub return_type: BytecodeType,
    pub native_function: Option<NativeFunction>,
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
    pub source_files: Vec<SourceFileData>,
    pub stdlib_package_id: PackageId,
    pub program_package_id: PackageId,
    pub boots_package_id: Option<PackageId>,
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
