use crate::bytecode::program::FunctionData;
use crate::bytecode::{ModuleData, ModuleId, PackageData, PackageId, Program};
use crate::language::SemAnalysis;

use super::sem_analysis::PackageName;

pub fn emit_program(sa: &SemAnalysis) -> Program {
    let boots_package_id = if let Some(package_id) = sa.boots_package_id {
        Some(PackageId(package_id.to_usize() as u32))
    } else {
        None
    };

    Program {
        packages: create_packages(sa),
        modules: create_modules(sa),
        functions: create_functions(sa),
        stdlib_package_id: PackageId(sa.stdlib_package_id().to_usize() as u32),
        program_package_id: PackageId(sa.program_package_id().to_usize() as u32),
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
            root_module_id: ModuleId(pkg.top_level_module_id().to_usize() as u32),
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
