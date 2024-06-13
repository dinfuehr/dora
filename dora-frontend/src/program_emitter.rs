use std::collections::HashMap;

use crate::Sema;
use dora_bytecode::program::{AliasData, ImplData};
use dora_bytecode::{
    ClassData, ClassField, EnumData, EnumVariant, ExtensionData, ExtensionId, FunctionData,
    FunctionId, FunctionKind, GlobalData, ImplId, ModuleData, ModuleId, PackageData, PackageId,
    Program, SourceFileData, SourceFileId, StructData, StructField, TraitData, TraitId,
    TypeParamBound, TypeParamData,
};

use crate::generator::bty_from_ty;

use crate::sema::{self as sa, ExtensionDefinitionId, GlobalDefinition, GlobalDefinitionId};
use crate::sema::{
    ClassDefinition, FctDefinitionId, FctParent, ModuleDefinitionId, PackageDefinitionId,
    PackageName, StructDefinition, TypeParamDefinition,
};

use super::sema::{ImplDefinitionId, TraitDefinitionId};

struct Emitter {
    global_initializer: HashMap<GlobalDefinitionId, FunctionId>,
}

pub fn emit_program(sa: Sema) -> Program {
    let mut emitter = Emitter {
        global_initializer: HashMap::new(),
    };

    Program {
        packages: create_packages(&sa),
        modules: create_modules(&sa),
        functions: create_functions(&sa, &mut emitter),
        globals: create_globals(&sa, &emitter),
        classes: create_classes(&sa),
        structs: create_structs(&sa),
        enums: create_enums(&sa),
        traits: create_traits(&sa),
        extensions: create_extensions(&sa),
        impls: create_impls(&sa),
        aliases: create_aliases(&sa),
        source_files: create_source_files(&sa),
        stdlib_package_id: convert_package_id(sa.stdlib_package_id()),
        program_package_id: convert_package_id(sa.program_package_id()),
        boots_package_id: sa.boots_package_id.map(|p| convert_package_id(p)),
        main_fct_id: find_main_fct_id(&sa),
    }
}

fn create_packages(sa: &Sema) -> Vec<PackageData> {
    let mut result = Vec::new();

    for (_id, pkg) in sa.packages.iter() {
        let name = match pkg.name {
            PackageName::Boots => "boots".into(),
            PackageName::Stdlib => "stdlib".into(),
            PackageName::Program => "program".into(),
            PackageName::External(ref name) => name.clone(),
        };

        result.push(PackageData {
            name,
            root_module_id: convert_module_id(pkg.top_level_module_id()),
        })
    }

    result
}

fn create_modules(sa: &Sema) -> Vec<ModuleData> {
    let mut result = Vec::new();

    for (_id, module) in sa.modules.iter() {
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

fn create_extensions(sa: &Sema) -> Vec<ExtensionData> {
    let mut result = Vec::new();

    for (_id, extension) in sa.extensions.iter() {
        let mut methods = Vec::new();

        // The methods array for impl should have the exact same order as for the trait.
        for method_id in extension.methods() {
            methods.push(convert_function_id(*method_id));
        }

        result.push(ExtensionData {
            module_id: convert_module_id(extension.module_id),
            type_params: create_type_params(sa, extension.type_params()),
            extended_ty: bty_from_ty(extension.ty().clone()),
            methods,
        });
    }

    result
}

fn create_impls(sa: &Sema) -> Vec<ImplData> {
    let mut result = Vec::new();

    for (_id, impl_) in sa.impls.iter() {
        let mut methods = Vec::new();

        let trait_id = impl_.trait_id();
        let trait_ = sa.trait_(trait_id);

        // The methods array for impl should have the exact same order as for the trait.
        for method_id in trait_.methods() {
            let target_method_id = *impl_
                .trait_method_map()
                .get(&method_id)
                .expect("missing impl for trait methdo");

            methods.push(convert_function_id(target_method_id));
        }

        let mut trait_method_map = Vec::new();

        for (trait_method_id, impl_method_id) in impl_.trait_method_map() {
            trait_method_map.push((
                convert_function_id(*trait_method_id),
                convert_function_id(*impl_method_id),
            ))
        }

        result.push(ImplData {
            module_id: convert_module_id(impl_.module_id),
            type_params: create_type_params(sa, impl_.type_params()),
            trait_ty: bty_from_ty(impl_.trait_ty()),
            extended_ty: bty_from_ty(impl_.extended_ty()),
            methods,
            trait_method_map,
        });
    }

    result
}

fn create_aliases(sa: &Sema) -> Vec<AliasData> {
    let mut result = Vec::new();

    for (_id, alias) in sa.aliases.iter() {
        result.push(AliasData {
            name: sa.interner.str(alias.name).to_string(),
            ty: None,
        })
    }

    result
}

fn create_functions(sa: &Sema, e: &mut Emitter) -> Vec<FunctionData> {
    let mut result = Vec::new();

    for (_id, fct) in sa.fcts.iter() {
        let name = sa.interner.str(fct.name).to_string();

        let kind = match fct.parent {
            FctParent::Extension(extension_id) => {
                FunctionKind::Extension(convert_extension_id(extension_id))
            }
            FctParent::Function => FunctionKind::Lambda,
            FctParent::Impl(impl_id) => FunctionKind::Impl(convert_impl_id(impl_id)),
            FctParent::Trait(trait_id) => FunctionKind::Trait(convert_trait_id(trait_id)),
            FctParent::None => FunctionKind::Function,
        };

        result.push(FunctionData {
            name,
            loc: sa.compute_loc(fct.file_id, fct.span),
            kind,
            file_id: convert_source_file_id(fct.file_id),
            package_id: convert_package_id(fct.package_id),
            module_id: convert_module_id(fct.module_id),
            type_params: create_type_params(sa, fct.type_params()),
            source_file_id: Some(convert_source_file_id(fct.file_id)),
            params: fct
                .params_with_self()
                .iter()
                .map(|ty| bty_from_ty(ty.clone()))
                .collect(),
            return_type: fct.return_type_bty(),
            native: fct.native_function.get().cloned(),
            intrinsic: fct.intrinsic.get().cloned(),
            is_test: fct.is_test,
            vtable_index: fct.vtable_index.get().cloned(),
            is_optimize_immediately: fct.is_optimize_immediately,
            is_variadic: fct.is_variadic.get(),
            bytecode: fct.bytecode.get().cloned(),
        })
    }

    for (_id, global) in sa.globals.iter() {
        if !global.has_initial_value() {
            continue;
        }

        let fct_id = FunctionId(result.len().try_into().expect("overflow"));
        let name = sa.interner.str(global.name).to_string();

        result.push(FunctionData {
            name,
            loc: sa.compute_loc(global.file_id, global.span),
            kind: FunctionKind::Function,
            file_id: convert_source_file_id(global.file_id),
            package_id: convert_package_id(global.package_id),
            module_id: convert_module_id(global.module_id),
            type_params: create_type_params(sa, &TypeParamDefinition::new()),
            source_file_id: Some(convert_source_file_id(global.file_id)),
            params: Vec::new(),
            return_type: bty_from_ty(global.ty()),
            native: None,
            intrinsic: None,
            is_test: false,
            vtable_index: None,
            is_optimize_immediately: false,
            is_variadic: false,
            bytecode: Some(global.bytecode().clone()),
        });

        e.global_initializer.insert(global.id(), fct_id);
    }

    result
}

fn create_globals(sa: &Sema, e: &Emitter) -> Vec<GlobalData> {
    let mut result = Vec::new();

    for (_id, global) in sa.globals.iter() {
        let name = sa.interner.str(global.name).to_string();

        result.push(GlobalData {
            module_id: convert_module_id(global.module_id),
            ty: bty_from_ty(global.ty()),
            mutable: global.mutable,
            name,
            initial_value: global_initializer_function_id(sa, &*global, e),
        })
    }

    result
}

fn global_initializer_function_id(
    _sa: &Sema,
    global: &GlobalDefinition,
    e: &Emitter,
) -> Option<FunctionId> {
    Some(
        e.global_initializer
            .get(&global.id())
            .expect("missing initializer")
            .to_owned(),
    )
}

fn create_classes(sa: &Sema) -> Vec<ClassData> {
    let mut result = Vec::new();

    for (_class_id, class) in sa.classes.iter() {
        let name = sa.interner.str(class.name).to_string();

        result.push(ClassData {
            module_id: convert_module_id(class.module_id),
            name,
            type_params: create_type_params(sa, class.type_params()),
            fields: create_class_fields(sa, &*class),
        })
    }

    result
}

fn create_class_fields(sa: &Sema, class: &ClassDefinition) -> Vec<ClassField> {
    class
        .fields
        .iter()
        .map(|f| ClassField {
            ty: bty_from_ty(f.ty()),
            name: sa.interner.str(f.name).to_string(),
        })
        .collect()
}

fn create_structs(sa: &Sema) -> Vec<StructData> {
    let mut result = Vec::new();

    for (_struct_id, struct_) in sa.structs.iter() {
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

fn create_type_params(sa: &Sema, type_params: &TypeParamDefinition) -> TypeParamData {
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

fn create_struct_fields(sa: &Sema, struct_: &StructDefinition) -> Vec<StructField> {
    struct_
        .fields
        .iter()
        .map(|f| StructField {
            ty: bty_from_ty(f.ty()),
            name: sa.interner.str(f.name).to_string(),
        })
        .collect()
}

fn create_enums(sa: &Sema) -> Vec<EnumData> {
    let mut result = Vec::new();

    for (_id, enum_) in sa.enums.iter() {
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

fn create_enum_variants(sa: &Sema, enum_: &sa::EnumDefinition) -> Vec<EnumVariant> {
    let mut result = Vec::new();

    for variant in enum_.variants() {
        let arguments = variant
            .types()
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

fn create_traits(sa: &Sema) -> Vec<TraitData> {
    let mut result = Vec::new();

    for (_id, trait_) in sa.traits.iter() {
        let name = sa.interner.str(trait_.name).to_string();

        result.push(TraitData {
            module_id: convert_module_id(trait_.module_id),
            name,
            type_params: create_type_params(sa, &trait_.type_params()),
            methods: trait_
                .methods()
                .iter()
                .map(|f| convert_function_id(*f))
                .collect(),
        })
    }

    result
}

fn create_source_files(sa: &Sema) -> Vec<SourceFileData> {
    let mut result = Vec::new();

    for (_id, file) in sa.source_files.iter() {
        result.push(SourceFileData {
            path: file.path.to_string_lossy().to_string(),
        })
    }

    result
}

fn find_main_fct_id(sa: &Sema) -> Option<FunctionId> {
    let name = sa.interner.intern("main");
    let table = sa.module_table(sa.program_module_id());
    let sym = table.get_sym(name);

    if sym.is_none() {
        return None;
    }

    let sym = sym.unwrap();

    if !sym.kind().is_fct() {
        return None;
    }

    let fct_id = sym.kind().to_fct().unwrap();

    let fct = sa.fct(fct_id);
    let ret = fct.return_type();

    if (!ret.is_unit() && !ret.is_int32())
        || !fct.params_without_self().is_empty()
        || !fct.type_params().is_empty()
    {
        None
    } else {
        Some(convert_function_id(fct.id()))
    }
}

fn convert_package_id(id: PackageDefinitionId) -> PackageId {
    PackageId(id.index().try_into().expect("failure"))
}

fn convert_module_id(id: ModuleDefinitionId) -> ModuleId {
    ModuleId(id.index().try_into().expect("failure"))
}

fn convert_function_id(id: FctDefinitionId) -> FunctionId {
    FunctionId(id.index().try_into().expect("failure"))
}

fn convert_source_file_id(id: sa::SourceFileId) -> SourceFileId {
    SourceFileId(id.index().try_into().expect("failure"))
}

fn convert_impl_id(id: ImplDefinitionId) -> ImplId {
    ImplId(id.index().try_into().expect("failure"))
}

fn convert_extension_id(id: ExtensionDefinitionId) -> ExtensionId {
    ExtensionId(id.index().try_into().expect("failure"))
}

fn convert_trait_id(id: TraitDefinitionId) -> TraitId {
    TraitId(id.index().try_into().expect("failure"))
}
