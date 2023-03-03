#[derive(Copy, Clone)]
pub struct PackageId(pub u32);

pub struct PackageData {
    pub name: String,
    pub root_module_id: ModuleId,
}

#[derive(Copy, Clone)]
pub struct ModuleId(pub u32);

pub struct ModuleData {
    pub name: String,
}

#[derive(Copy, Clone)]
pub struct FunctionId(pub u32);

pub struct FunctionData {
    pub name: String,
}

pub struct Program {
    pub packages: Vec<PackageData>,
    pub modules: Vec<ModuleData>,
    pub functions: Vec<FunctionData>,
    pub stdlib_package_id: PackageId,
    pub program_package_id: PackageId,
    pub boots_package_id: Option<PackageId>,
}
