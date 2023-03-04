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

pub struct Program {
    pub packages: Vec<PackageData>,
    pub modules: Vec<ModuleData>,
    pub functions: Vec<FunctionData>,
    pub globals: Vec<GlobalData>,
    pub stdlib_package_id: PackageId,
    pub program_package_id: PackageId,
    pub boots_package_id: Option<PackageId>,
}
