use crate::bytecode::program::FunctionData;
use crate::bytecode::{
    FunctionId, GlobalData, ModuleData, ModuleId, PackageData, PackageId, Program,
};
use crate::language::SemAnalysis;

use crate::language::generator::bty_from_ty;
use crate::language::sem_analysis::PackageName;

use super::sem_analysis::{FctDefinitionId, ModuleDefinitionId, PackageDefinitionId};

pub fn emit_program(sa: &SemAnalysis) -> Program {
    let boots_package_id = if let Some(package_id) = sa.boots_package_id {
        Some(convert_package_id(package_id))
    } else {
        None
    };

    Program {
        packages: create_packages(sa),
        modules: create_modules(sa),
        functions: create_functions(sa),
        globals: create_globals(sa),
        stdlib_package_id: convert_package_id(sa.stdlib_package_id()),
        program_package_id: convert_package_id(sa.program_package_id()),
        boots_package_id,
    }
}

fn create_packages(sa: &SemAnalysis) -> Vec<PackageData> {
    let mut result = Vec::new();

    for pkg in sa.packages.iter() {
        let pkg = pkg.read();

        let name = match pkg.name {
            PackageName::Boots => "boots".into(),
            PackageName::Stdlib => "stdlib".into(),
            PackageName::Program => "program".into(),
            PackageName::External(name) => sa.interner.str(name).to_string(),
        };

        result.push(PackageData {
            name,
            root_module_id: convert_module_id(pkg.top_level_module_id()),
        })
    }

    result
}

fn create_modules(sa: &SemAnalysis) -> Vec<ModuleData> {
    let mut result = Vec::new();

    for module in sa.modules.iter() {
        let module = module.read();

        let name = if let Some(name) = module.name {
            sa.interner.str(name).to_string()
        } else {
            "<root>".into()
        };

        result.push(ModuleData { name })
    }

    result
}

fn create_functions(sa: &SemAnalysis) -> Vec<FunctionData> {
    let mut result = Vec::new();

    for fct in sa.fcts.iter() {
        let fct = fct.read();
        let name = sa.interner.str(fct.name).to_string();
        result.push(FunctionData { name })
    }

    result
}

fn create_globals(sa: &SemAnalysis) -> Vec<GlobalData> {
    let mut result = Vec::new();

    for global in sa.globals.iter() {
        let global = global.read();
        let name = sa.interner.str(global.name).to_string();

        result.push(GlobalData {
            module_id: convert_module_id(global.module_id),
            ty: bty_from_ty(global.ty.clone()),
            mutable: global.mutable,
            name,
            initializer: global.initializer.map(|t| convert_function_id(t)),
        })
    }

    result
}

fn convert_package_id(id: PackageDefinitionId) -> PackageId {
    PackageId(id.to_usize().try_into().expect("failure"))
}

fn convert_module_id(id: ModuleDefinitionId) -> ModuleId {
    ModuleId(id.to_usize().try_into().expect("failure"))
}

fn convert_function_id(id: FctDefinitionId) -> FunctionId {
    FunctionId(id.to_usize().try_into().expect("failure"))
}
