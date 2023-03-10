use crate::bytecode::BytecodeType;

#[derive(Copy, Clone, Debug)]
pub struct PackageId(pub u32);

#[derive(Debug)]
pub struct PackageData {
    pub name: String,
    pub root_module_id: ModuleId,
}

#[derive(Debug, Copy, Clone)]
pub struct ModuleId(pub u32);

#[derive(Debug)]
pub struct ModuleData {
    pub name: String,
}

#[derive(Copy, Clone, Debug)]
pub struct FunctionId(pub u32);

#[derive(Debug)]
pub struct FunctionData {
    pub name: String,
    pub type_params: TypeParamData,
    pub source_file_id: Option<SourceFileId>,
}

#[derive(Copy, Clone, Debug)]
pub struct GlobalId(pub u32);

#[derive(Debug)]
pub struct GlobalData {
    pub module_id: ModuleId,
    pub ty: BytecodeType,
    pub mutable: bool,
    pub name: String,
    pub initializer: Option<FunctionId>,
}

#[derive(Copy, Clone, Debug)]
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

#[derive(Copy, Clone, Debug)]
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

#[derive(Copy, Clone, Debug)]
pub struct EnumId(pub u32);

#[derive(Debug)]
pub struct EnumData {
    pub module_id: ModuleId,
    pub name: String,
}

#[derive(Copy, Clone, Debug)]
pub struct TraitId(pub u32);

#[derive(Debug)]
pub struct TraitData {
    pub module_id: ModuleId,
    pub name: String,
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
