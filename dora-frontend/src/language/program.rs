use crate::language::SemAnalysis;
use dora_bytecode::program::{ClassLayout, ImplData};
use dora_bytecode::{
    ClassData, ClassField, EnumData, EnumVariant, FunctionData, FunctionId, GlobalData, ModuleData,
    ModuleId, PackageData, PackageId, Program, SourceFileData, SourceFileId, StructData,
    StructField, TraitData, TypeParamBound, TypeParamData,
};

use crate::language::generator::bty_from_ty;

use crate::language::sem_analysis as sa;
use crate::language::sem_analysis::{
    ClassDefinition, FctDefinitionId, ModuleDefinitionId, PackageDefinitionId, PackageName,
    StructDefinition, TypeParamDefinition,
};

pub fn emit_program(sa: &SemAnalysis) -> Program {
    Program {
        packages: create_packages(sa),
        modules: create_modules(sa),
        functions: create_functions(sa),
        globals: create_globals(sa),
        classes: create_classes(sa),
        structs: create_structs(sa),
        enums: create_enums(sa),
        traits: create_traits(sa),
        impls: create_impls(sa),
        source_files: create_source_files(sa),
        stdlib_package_id: convert_package_id(sa.stdlib_package_id()),
        program_package_id: convert_package_id(sa.program_package_id()),
        boots_package_id: sa.boots_package_id.map(|p| convert_package_id(p)),
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

        result.push(ModuleData {
            name,
            parent_id: module.parent_module_id.map(|id| convert_module_id(id)),
        })
    }

    result
}

fn create_impls(sa: &SemAnalysis) -> Vec<ImplData> {
    let mut result = Vec::new();

    for impl_ in sa.impls.iter() {
        let impl_ = impl_.read();

        result.push(ImplData {
            module_id: convert_module_id(impl_.module_id),
            type_params: create_type_params(sa, impl_.type_params()),
            trait_ty: bty_from_ty(impl_.trait_ty.clone()),
            extended_ty: bty_from_ty(impl_.extended_ty.clone()),
        });
    }

    result
}

fn create_functions(sa: &SemAnalysis) -> Vec<FunctionData> {
    let mut result = Vec::new();

    for fct in sa.fcts.iter() {
        let fct = fct.read();
        let name = sa.interner.str(fct.name).to_string();
        result.push(FunctionData {
            name,
            package_id: convert_package_id(fct.package_id),
            type_params: create_type_params(sa, &fct.type_params),
            source_file_id: Some(convert_source_file_id(fct.file_id)),
            params: fct
                .params_with_self()
                .iter()
                .map(|ty| bty_from_ty(ty.clone()))
                .collect(),
            return_type: fct.return_type_bty(),
            native_function: fct.native_function.clone(),
        })
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

fn create_classes(sa: &SemAnalysis) -> Vec<ClassData> {
    let mut result = Vec::new();

    for class in sa.classes.iter() {
        let class = class.read();
        let name = sa.interner.str(class.name).to_string();

        result.push(ClassData {
            module_id: convert_module_id(class.module_id),
            name,
            type_params: create_type_params(sa, class.type_params()),
            layout: create_class_layout(&*class),
            fields: create_class_fields(sa, &*class),
        })
    }

    result
}

fn create_class_layout(class: &ClassDefinition) -> ClassLayout {
    if class.is_array {
        ClassLayout::Array
    } else if class.is_str {
        ClassLayout::String
    } else {
        ClassLayout::Regular
    }
}

fn create_class_fields(sa: &SemAnalysis, class: &ClassDefinition) -> Vec<ClassField> {
    class
        .fields
        .iter()
        .map(|f| ClassField {
            ty: bty_from_ty(f.ty.clone()),
            name: sa.interner.str(f.name).to_string(),
        })
        .collect()
}

fn create_structs(sa: &SemAnalysis) -> Vec<StructData> {
    let mut result = Vec::new();

    for struct_ in sa.structs.iter() {
        let struct_ = struct_.read();
        let name = sa.interner.str(struct_.name).to_string();

        result.push(StructData {
            module_id: convert_module_id(struct_.module_id),
            name,
            type_params: create_type_params(sa, struct_.type_params()),
            fields: create_struct_fields(sa, &*struct_),
        })
    }

    result
}

fn create_type_params(sa: &SemAnalysis, type_params: &TypeParamDefinition) -> TypeParamData {
    let names = type_params
        .names()
        .map(|(_, name)| sa.interner.str(name).to_string())
        .collect();

    let bounds = type_params
        .bounds()
        .iter()
        .map(|b| TypeParamBound {
            ty: bty_from_ty(b.ty()),
            trait_ty: bty_from_ty(b.trait_ty()),
        })
        .collect();

    TypeParamData { names, bounds }
}

fn create_struct_fields(sa: &SemAnalysis, struct_: &StructDefinition) -> Vec<StructField> {
    struct_
        .fields
        .iter()
        .map(|f| StructField {
            ty: bty_from_ty(f.ty.clone()),
            name: sa.interner.str(f.name).to_string(),
        })
        .collect()
}

fn create_enums(sa: &SemAnalysis) -> Vec<EnumData> {
    let mut result = Vec::new();

    for enum_ in sa.enums.iter() {
        let enum_ = enum_.read();
        let name = sa.interner.str(enum_.name).to_string();

        result.push(EnumData {
            module_id: convert_module_id(enum_.module_id),
            name,
            type_params: create_type_params(sa, enum_.type_params()),
            variants: create_enum_variants(sa, &*enum_),
        })
    }

    result
}

fn create_enum_variants(sa: &SemAnalysis, enum_: &sa::EnumDefinition) -> Vec<EnumVariant> {
    let mut result = Vec::new();

    for variant in &enum_.variants {
        let arguments = variant
            .types
            .iter()
            .map(|ty| bty_from_ty(ty.clone()))
            .collect();
        result.push(EnumVariant {
            name: sa.interner.str(variant.name).to_string(),
            arguments,
        })
    }

    result
}

fn create_traits(sa: &SemAnalysis) -> Vec<TraitData> {
    let mut result = Vec::new();

    for trait_ in sa.traits.iter() {
        let trait_ = trait_.read();
        let name = sa.interner.str(trait_.name).to_string();

        result.push(TraitData {
            module_id: convert_module_id(trait_.module_id),
            name,
            type_params: create_type_params(sa, &trait_.type_params()),
            methods: trait_
                .methods
                .iter()
                .map(|f| convert_function_id(*f))
                .collect(),
        })
    }

    result
}

fn create_source_files(sa: &SemAnalysis) -> Vec<SourceFileData> {
    let mut result = Vec::new();

    for file in sa.source_files.iter() {
        result.push(SourceFileData {
            path: file.path.to_string_lossy().to_string(),
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

fn convert_source_file_id(id: sa::SourceFileId) -> SourceFileId {
    SourceFileId(id.to_usize().try_into().expect("failure"))
}
