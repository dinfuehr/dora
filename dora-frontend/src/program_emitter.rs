use std::collections::HashMap;

use crate::Sema;
use dora_bytecode::program::{AliasData, ImplData};
use dora_bytecode::{
    AliasId, ClassData, ClassField, EnumData, EnumVariant, ExtensionData, ExtensionId,
    FunctionData, FunctionId, FunctionKind, GlobalData, ImplId, ModuleData, ModuleId, PackageData,
    PackageId, Program, SourceFileData, SourceFileId, StructData, StructField, TraitData, TraitId,
    TypeParamBound, TypeParamData,
};

use crate::generator::{bty_from_ty, convert_trait_type};

use crate::sema::{
    self, AliasDefinitionId, ClassDefinition, Element, EnumDefinition, FctDefinitionId, FctParent,
    ModuleDefinitionId, PackageDefinitionId, PackageName, StructDefinition, TypeParamDefinition,
};
use crate::sema::{
    ExtensionDefinitionId, GlobalDefinition, GlobalDefinitionId, ImplDefinitionId,
    TraitDefinitionId,
};

pub fn emit_program(sa: Sema) -> Program {
    let mut emitter = Emitter {
        global_initializer: HashMap::new(),
        map_functions: HashMap::new(),
        functions: Vec::new(),
        globals: Vec::new(),
        packages: Vec::new(),
        modules: Vec::new(),
    };

    emitter.create_packages(&sa);
    emitter.create_modules(&sa);
    emitter.create_functions(&sa);
    emitter.create_globals(&sa);

    Program {
        packages: emitter.packages,
        modules: emitter.modules,
        functions: emitter.functions,
        globals: emitter.globals,
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

struct Emitter {
    global_initializer: HashMap<GlobalDefinitionId, FunctionId>,
    map_functions: HashMap<FctDefinitionId, FunctionId>,
    functions: Vec<FunctionData>,
    globals: Vec<GlobalData>,
    packages: Vec<PackageData>,
    modules: Vec<ModuleData>,
}

impl Emitter {
    fn create_packages(&mut self, sa: &Sema) {
        for (_id, pkg) in sa.packages.iter() {
            let name = match pkg.name {
                PackageName::Boots => "boots".into(),
                PackageName::Std => "std".into(),
                PackageName::Program => "program".into(),
                PackageName::External(ref name) => name.clone(),
            };

            self.packages.push(PackageData {
                name,
                root_module_id: convert_module_id(pkg.top_level_module_id()),
            });
        }
    }

    fn create_modules(&mut self, sa: &Sema) {
        for (_id, module) in sa.modules.iter() {
            let name = if let Some(name) = module.name {
                sa.interner.str(name).to_string()
            } else {
                "<root>".into()
            };

            self.modules.push(ModuleData {
                name,
                parent_id: module.parent_module_id.map(|id| convert_module_id(id)),
            })
        }
    }

    fn create_functions(&mut self, sa: &Sema) {
        for (id, fct) in sa.fcts.iter() {
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

            let function_id = FunctionId(self.functions.len().try_into().expect("overflow"));
            self.functions.push(FunctionData {
                name,
                loc: sa.compute_loc(fct.file_id, fct.span),
                kind,
                file_id: convert_source_file_id(fct.file_id),
                package_id: convert_package_id(fct.package_id),
                module_id: convert_module_id(fct.module_id),
                type_params: create_type_params(sa, fct.type_param_definition()),
                source_file_id: Some(convert_source_file_id(fct.file_id)),
                params: fct
                    .params_with_self()
                    .iter()
                    .map(|p| bty_from_ty(p.ty()))
                    .collect(),
                return_type: fct.return_type_bty(),
                is_internal: fct.is_internal,
                is_test: fct.is_test,
                is_optimize_immediately: fct.is_optimize_immediately,
                is_variadic: fct.params.is_variadic(),
                is_force_inline: fct.is_force_inline,
                is_never_inline: fct.is_never_inline,
                is_trait_object_ignore: fct.is_trait_object_ignore,
                bytecode: fct.bytecode.get().cloned(),
                trait_method_impl: fct
                    .trait_method_impl
                    .get()
                    .cloned()
                    .map(|id| convert_function_id(id)),
            });

            self.map_functions.insert(id, function_id);
        }

        for (_id, global) in sa.globals.iter() {
            if !global.has_initial_value() {
                continue;
            }

            let fct_id = FunctionId(self.functions.len().try_into().expect("overflow"));
            let name = sa.interner.str(global.name).to_string();

            self.functions.push(FunctionData {
                name,
                loc: sa.compute_loc(global.file_id, global.span),
                kind: FunctionKind::Function,
                file_id: convert_source_file_id(global.file_id),
                package_id: convert_package_id(global.package_id),
                module_id: convert_module_id(global.module_id),
                type_params: create_type_params(sa, &TypeParamDefinition::empty()),
                source_file_id: Some(convert_source_file_id(global.file_id)),
                params: Vec::new(),
                return_type: bty_from_ty(global.ty()),
                is_internal: false,
                is_test: false,
                is_optimize_immediately: false,
                is_variadic: false,
                is_force_inline: false,
                is_never_inline: false,
                is_trait_object_ignore: false,
                bytecode: Some(global.bytecode().clone()),
                trait_method_impl: None,
            });

            self.global_initializer.insert(global.id(), fct_id);
        }
    }

    fn create_globals(&mut self, sa: &Sema) {
        for (_id, global) in sa.globals.iter() {
            let name = sa.interner.str(global.name).to_string();

            self.globals.push(GlobalData {
                module_id: convert_module_id(global.module_id),
                ty: bty_from_ty(global.ty()),
                mutable: global.mutable,
                name,
                initial_value: global_initializer_function_id(sa, &*global, self),
            })
        }
    }
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
            type_params: create_type_params(sa, extension.type_param_definition()),
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

        let trait_ty = impl_.trait_ty().expect("trait expected");
        let trait_ = sa.trait_(trait_ty.trait_id);

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

        let mut trait_alias_map = Vec::new();

        for (trait_alias_id, impl_alias_id) in impl_.trait_alias_map() {
            trait_alias_map.push((
                convert_alias_id(*trait_alias_id),
                convert_alias_id(*impl_alias_id),
            ));
        }

        result.push(ImplData {
            module_id: convert_module_id(impl_.module_id),
            type_params: create_type_params(sa, impl_.type_param_definition()),
            trait_ty: convert_trait_type(&trait_ty),
            extended_ty: bty_from_ty(impl_.extended_ty()),
            methods,
            trait_method_map,
            trait_alias_map,
        });
    }

    result
}

fn create_aliases(sa: &Sema) -> Vec<AliasData> {
    let mut result = Vec::new();

    for (_id, alias) in sa.aliases.iter() {
        result.push(AliasData {
            name: sa.interner.str(alias.name).to_string(),
            ty: alias.parsed_ty().map(|pty| bty_from_ty(pty.ty())),
            idx_in_trait: alias.idx_in_trait,
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
            type_params: create_type_params(sa, class.type_param_definition()),
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
            name: f.name.map(|n| sa.interner.str(n).to_string()),
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
            type_params: create_type_params(sa, struct_.type_param_definition()),
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
        .map(|b| TypeParamBound {
            ty: bty_from_ty(b.ty()),
            trait_ty: convert_trait_type(b.trait_ty().as_ref().expect("missing trait type")),
        })
        .collect();

    let container_count = type_params.container_type_params();

    TypeParamData {
        names,
        container_count,
        bounds,
    }
}

fn create_struct_fields(sa: &Sema, struct_: &StructDefinition) -> Vec<StructField> {
    struct_
        .fields
        .iter()
        .map(|f| StructField {
            ty: bty_from_ty(f.ty()),
            name: f.name.map(|n| sa.interner.str(n).to_string()),
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
            type_params: create_type_params(sa, enum_.type_param_definition()),
            variants: create_enum_variants(sa, &*enum_),
        })
    }

    result
}

fn create_enum_variants(sa: &Sema, enum_: &EnumDefinition) -> Vec<EnumVariant> {
    let mut result = Vec::new();

    for variant in enum_.variants() {
        let arguments = variant
            .fields
            .iter()
            .map(|f| bty_from_ty(f.parsed_type.ty()))
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

        let methods = trait_
            .methods()
            .iter()
            .map(|f| convert_function_id(*f))
            .collect();

        let virtual_methods = trait_
            .methods()
            .iter()
            .filter(|f| {
                let method = sa.fct(**f);
                !method.is_trait_object_ignore
            })
            .map(|f| convert_function_id(*f))
            .collect();

        result.push(TraitData {
            module_id: convert_module_id(trait_.module_id),
            name,
            type_params: create_type_params(sa, &trait_.type_param_definition()),
            methods,
            virtual_methods,
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
        || !fct.type_param_definition().is_empty()
    {
        None
    } else {
        Some(convert_function_id(fct.id()))
    }
}

fn convert_alias_id(id: AliasDefinitionId) -> AliasId {
    AliasId(id.index().try_into().expect("failure"))
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

fn convert_source_file_id(id: sema::SourceFileId) -> SourceFileId {
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
