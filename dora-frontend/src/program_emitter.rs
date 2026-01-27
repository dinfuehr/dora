use std::collections::HashMap;

use crate::Sema;
use crate::sym::SymbolKind;
use dora_bytecode::program::{AliasData, ImplData};
use dora_bytecode::{
    AliasId, BytecodeTraitType, BytecodeType, BytecodeTypeArray, ClassData, ClassField, ClassId,
    ConstData, ConstId, EnumData, EnumId, EnumVariant, ExtensionData, ExtensionId, FunctionData,
    FunctionId, FunctionKind, GlobalData, GlobalId, ImplId, ModuleData, ModuleElementId, ModuleId,
    PackageData, PackageId, Program, SourceFileData, SourceFileId, StructData, StructField,
    StructId, TraitData, TraitId, TypeParamBound, TypeParamData,
};

use crate::generator::{generate_fct, generate_global_initializer};

use crate::sema::{
    self, AliasDefinitionId, ClassDefinition, ClassDefinitionId, ConstDefinitionId, Element,
    EnumDefinition, EnumDefinitionId, ExtensionDefinitionId, FctDefinitionId, FctParent,
    GlobalDefinition, GlobalDefinitionId, ImplDefinitionId, ModuleDefinitionId,
    PackageDefinitionId, PackageName, StructDefinition, StructDefinitionId, TraitDefinitionId,
    TypeParamDefinition,
};
use crate::{SourceType, SourceTypeArray, TraitType};

pub fn emit_program(sa: Sema) -> Program {
    let mut emitter = Emitter::new();

    emitter.create_id_mappings(&sa);

    emitter.create_packages(&sa);
    emitter.create_modules(&sa);
    emitter.create_classes(&sa);
    emitter.create_structs(&sa);
    emitter.create_functions(&sa);
    emitter.create_globals(&sa);
    emitter.create_consts(&sa);
    emitter.create_enums(&sa);
    emitter.create_traits(&sa);
    emitter.create_source_files(&sa);
    emitter.create_extensions(&sa);
    emitter.create_impls(&sa);
    emitter.create_aliases(&sa);

    let stdlib_package_id = emitter.convert_package_id(sa.stdlib_package_id());
    let program_package_id = emitter.convert_package_id(sa.program_package_id());
    let boots_package_id = sa.boots_package_id.map(|p| emitter.convert_package_id(p));
    let main_fct_id = emitter.find_main_fct_id(&sa);

    Program {
        packages: emitter.packages,
        modules: emitter.modules,
        functions: emitter.functions,
        globals: emitter.globals,
        consts: emitter.consts,
        classes: emitter.classes,
        structs: emitter.structs,
        enums: emitter.enums,
        traits: emitter.traits,
        extensions: emitter.extensions,
        impls: emitter.impls,
        aliases: emitter.aliases,
        source_files: emitter.source_files,
        stdlib_package_id,
        program_package_id,
        boots_package_id,
        main_fct_id,
    }
}

pub struct Emitter {
    global_initializer: HashMap<GlobalDefinitionId, FunctionId>,
    map_packages: HashMap<PackageDefinitionId, PackageId>,
    map_modules: HashMap<ModuleDefinitionId, ModuleId>,
    map_functions: HashMap<FctDefinitionId, FunctionId>,
    map_globals: HashMap<GlobalDefinitionId, GlobalId>,
    map_consts: HashMap<ConstDefinitionId, ConstId>,
    map_classes: HashMap<ClassDefinitionId, ClassId>,
    map_structs: HashMap<StructDefinitionId, StructId>,
    map_enums: HashMap<EnumDefinitionId, EnumId>,
    map_traits: HashMap<TraitDefinitionId, TraitId>,
    map_source_files: HashMap<sema::SourceFileId, SourceFileId>,
    map_extensions: HashMap<ExtensionDefinitionId, ExtensionId>,
    map_impls: HashMap<ImplDefinitionId, ImplId>,
    map_aliases: HashMap<AliasDefinitionId, AliasId>,
    functions: Vec<FunctionData>,
    globals: Vec<GlobalData>,
    consts: Vec<ConstData>,
    packages: Vec<PackageData>,
    modules: Vec<ModuleData>,
    classes: Vec<ClassData>,
    structs: Vec<StructData>,
    enums: Vec<EnumData>,
    traits: Vec<TraitData>,
    source_files: Vec<SourceFileData>,
    extensions: Vec<ExtensionData>,
    impls: Vec<ImplData>,
    aliases: Vec<AliasData>,
}

impl Emitter {
    pub fn new() -> Emitter {
        Emitter {
            global_initializer: HashMap::new(),
            map_packages: HashMap::new(),
            map_modules: HashMap::new(),
            map_functions: HashMap::new(),
            map_globals: HashMap::new(),
            map_consts: HashMap::new(),
            map_classes: HashMap::new(),
            map_structs: HashMap::new(),
            map_enums: HashMap::new(),
            map_traits: HashMap::new(),
            map_source_files: HashMap::new(),
            map_extensions: HashMap::new(),
            map_impls: HashMap::new(),
            map_aliases: HashMap::new(),
            functions: Vec::new(),
            globals: Vec::new(),
            consts: Vec::new(),
            packages: Vec::new(),
            modules: Vec::new(),
            classes: Vec::new(),
            structs: Vec::new(),
            enums: Vec::new(),
            traits: Vec::new(),
            source_files: Vec::new(),
            extensions: Vec::new(),
            impls: Vec::new(),
            aliases: Vec::new(),
        }
    }

    fn create_id_mappings(&mut self, sa: &Sema) {
        // Add offset to all IDs and dummy elements to catch raw index conversions.
        const DUMMY_COUNT: usize = 10;

        for (id, _) in sa.packages.iter() {
            let package_id = (id.index() + DUMMY_COUNT).into();
            self.map_packages.insert(id, package_id);
        }

        for (id, _) in sa.modules.iter() {
            let module_id = (id.index() + DUMMY_COUNT).into();
            self.map_modules.insert(id, module_id);
        }

        for (id, _) in sa.fcts.iter() {
            let function_id = (id.index() + DUMMY_COUNT).into();
            self.map_functions.insert(id, function_id);
        }

        for (id, _) in sa.globals.iter() {
            let global_id = (id.index() + DUMMY_COUNT).into();
            self.map_globals.insert(id, global_id);
        }

        for (id, _) in sa.consts.iter() {
            let const_id = (id.index() + DUMMY_COUNT).into();
            self.map_consts.insert(id, const_id);
        }

        for (id, _) in sa.classes.iter() {
            let class_id = (id.index() + DUMMY_COUNT).into();
            self.map_classes.insert(id, class_id);
        }

        for (id, _) in sa.structs.iter() {
            let struct_id = (id.index() + DUMMY_COUNT).into();
            self.map_structs.insert(id, struct_id);
        }

        for (id, _) in sa.enums.iter() {
            let enum_id = (id.index() + DUMMY_COUNT).into();
            self.map_enums.insert(id, enum_id);
        }

        for (id, _) in sa.traits.iter() {
            let trait_id = (id.index() + DUMMY_COUNT).into();
            self.map_traits.insert(id, trait_id);
        }

        for (id, _) in sa.source_files.iter() {
            let source_file_id = (id.index() + DUMMY_COUNT).into();
            self.map_source_files.insert(id, source_file_id);
        }

        for (id, _) in sa.extensions.iter() {
            let extension_id = (id.index() + DUMMY_COUNT).into();
            self.map_extensions.insert(id, extension_id);
        }

        for (id, _) in sa.impls.iter() {
            let impl_id = (id.index() + DUMMY_COUNT).into();
            self.map_impls.insert(id, impl_id);
        }

        for (id, _) in sa.aliases.iter() {
            let alias_id = (id.index() + DUMMY_COUNT).into();
            self.map_aliases.insert(id, alias_id);
        }

        // Add dummy elements at the beginning of each table.
        self.add_dummy_elements(DUMMY_COUNT);
    }

    fn add_dummy_elements(&mut self, count: usize) {
        for _ in 0..count {
            self.packages.push(PackageData {
                name: "<dummy>".into(),
                root_module_id: 0usize.into(),
            });
        }

        for _ in 0..count {
            self.modules.push(ModuleData {
                name: "<dummy>".into(),
                parent_id: None,
                items: Vec::new(),
            });
        }

        for _ in 0..count {
            self.functions.push(FunctionData {
                name: "<dummy>".into(),
                loc: dora_bytecode::Location::new(0, 0),
                kind: FunctionKind::Function,
                file_id: 0usize.into(),
                package_id: 0usize.into(),
                module_id: 0usize.into(),
                type_params: TypeParamData {
                    names: Vec::new(),
                    container_count: 0,
                    bounds: Vec::new(),
                },
                source_file_id: None,
                params: Vec::new(),
                return_type: BytecodeType::Unit,
                is_internal: false,
                is_test: false,
                is_optimize_immediately: false,
                is_variadic: false,
                is_force_inline: false,
                is_never_inline: false,
                is_trait_object_ignore: false,
                bytecode: None,
                trait_method_impl: None,
            });
        }

        for _ in 0..count {
            self.globals.push(GlobalData {
                module_id: 0usize.into(),
                ty: BytecodeType::Unit,
                mutable: false,
                name: "<dummy>".into(),
                initial_value: None,
            });
        }

        for _ in 0..count {
            self.consts.push(ConstData {
                module_id: 0usize.into(),
                ty: BytecodeType::Unit,
                name: "<dummy>".into(),
                value: dora_bytecode::ConstValue::None,
            });
        }

        for _ in 0..count {
            self.classes.push(ClassData {
                module_id: 0usize.into(),
                name: "<dummy>".into(),
                type_params: TypeParamData {
                    names: Vec::new(),
                    container_count: 0,
                    bounds: Vec::new(),
                },
                fields: Vec::new(),
            });
        }

        for _ in 0..count {
            self.structs.push(StructData {
                module_id: 0usize.into(),
                name: "<dummy>".into(),
                type_params: TypeParamData {
                    names: Vec::new(),
                    container_count: 0,
                    bounds: Vec::new(),
                },
                fields: Vec::new(),
            });
        }

        for _ in 0..count {
            self.enums.push(EnumData {
                module_id: 0usize.into(),
                name: "<dummy>".into(),
                type_params: TypeParamData {
                    names: Vec::new(),
                    container_count: 0,
                    bounds: Vec::new(),
                },
                variants: Vec::new(),
            });
        }

        for _ in 0..count {
            self.traits.push(TraitData {
                module_id: 0usize.into(),
                name: "<dummy>".into(),
                type_params: TypeParamData {
                    names: Vec::new(),
                    container_count: 0,
                    bounds: Vec::new(),
                },
                methods: Vec::new(),
                virtual_methods: Vec::new(),
            });
        }

        for _ in 0..count {
            self.source_files.push(SourceFileData {
                path: "<dummy>".into(),
            });
        }

        for _ in 0..count {
            self.extensions.push(ExtensionData {
                module_id: 0usize.into(),
                type_params: TypeParamData {
                    names: Vec::new(),
                    container_count: 0,
                    bounds: Vec::new(),
                },
                extended_ty: BytecodeType::Unit,
                methods: Vec::new(),
            });
        }

        for _ in 0..count {
            self.impls.push(ImplData {
                module_id: 0usize.into(),
                type_params: TypeParamData {
                    names: Vec::new(),
                    container_count: 0,
                    bounds: Vec::new(),
                },
                trait_ty: BytecodeTraitType {
                    trait_id: 0usize.into(),
                    type_params: BytecodeTypeArray::empty(),
                    bindings: Vec::new(),
                },
                extended_ty: BytecodeType::Unit,
                methods: Vec::new(),
                trait_method_map: Vec::new(),
                trait_alias_map: Vec::new(),
            });
        }

        for _ in 0..count {
            self.aliases.push(AliasData {
                name: "<dummy>".into(),
                ty: None,
                idx_in_trait: None,
            });
        }
    }

    fn create_packages(&mut self, sa: &Sema) {
        for (_id, pkg) in sa.packages.iter() {
            let name = match pkg.name {
                PackageName::Boots => "boots".into(),
                PackageName::Std => "std".into(),
                PackageName::Program => "program".into(),
                PackageName::External(ref name) => name.clone(),
            };

            let root_module_id = self.convert_module_id(pkg.top_level_module_id());
            self.packages.push(PackageData {
                name,
                root_module_id,
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

            let mut items = Vec::new();

            for (name, sym) in &module.table().table {
                let name = sa.interner.str(*name).to_string();

                let item = match sym.kind() {
                    SymbolKind::Class(class_id) => {
                        ModuleElementId::Class(self.convert_class_id(*class_id))
                    }
                    SymbolKind::Enum(enum_id) => {
                        ModuleElementId::Enum(self.convert_enum_id(*enum_id))
                    }
                    SymbolKind::Struct(struct_id) => {
                        ModuleElementId::Struct(self.convert_struct_id(*struct_id))
                    }
                    SymbolKind::Trait(trait_id) => {
                        ModuleElementId::Trait(self.convert_trait_id(*trait_id))
                    }
                    SymbolKind::Module(module_id) => {
                        ModuleElementId::Module(self.convert_module_id(*module_id))
                    }
                    SymbolKind::Fct(fct_id) => {
                        ModuleElementId::Function(self.convert_function_id(*fct_id))
                    }
                    SymbolKind::Global(global_id) => {
                        ModuleElementId::Global(self.convert_global_id(*global_id))
                    }
                    SymbolKind::Const(const_id) => {
                        ModuleElementId::Const(self.convert_const_id(*const_id))
                    }
                    SymbolKind::EnumVariant(enum_id, variant_id) => {
                        ModuleElementId::EnumVariant(self.convert_enum_id(*enum_id), *variant_id)
                    }
                    SymbolKind::Alias(alias_id) => {
                        ModuleElementId::Alias(self.convert_alias_id(*alias_id))
                    }
                    _ => {
                        println!("sym = {:?}", sym.kind());
                        unreachable!()
                    }
                };

                items.push((name, item));
            }

            items.sort_by(|(k1, _), (k2, _)| k1.cmp(k2));

            let parent_id = module.parent_module_id.map(|id| self.convert_module_id(id));

            self.modules.push(ModuleData {
                name,
                parent_id,
                items,
            })
        }
    }

    fn create_classes(&mut self, sa: &Sema) {
        for (_class_id, class) in sa.classes.iter() {
            let name = sa.interner.str(class.name).to_string();
            let fields = self.create_class_fields(sa, &*class);

            let module_id = self.convert_module_id(class.module_id);
            let type_params = self.create_type_params(sa, class.type_param_definition());
            self.classes.push(ClassData {
                module_id,
                name,
                type_params,
                fields,
            })
        }
    }

    fn create_type_params(
        &mut self,
        sa: &Sema,
        type_params: &TypeParamDefinition,
    ) -> TypeParamData {
        let names = type_params
            .names()
            .map(|(_, name)| sa.interner.str(name).to_string())
            .collect();

        let mut bounds = Vec::new();
        for b in type_params.bounds() {
            let ty = self.convert_ty(b.ty());
            let trait_ty =
                self.convert_trait_ty(b.trait_ty().as_ref().expect("missing trait type"));
            bounds.push(TypeParamBound { ty, trait_ty });
        }

        let container_count = type_params.container_type_params();

        TypeParamData {
            names,
            container_count,
            bounds,
        }
    }

    fn create_class_fields(&mut self, sa: &Sema, class: &ClassDefinition) -> Vec<ClassField> {
        class
            .field_ids()
            .iter()
            .map(|&field_id| {
                let field = sa.field(field_id);
                ClassField {
                    ty: self.convert_ty(field.ty()),
                    name: field.name.map(|n| sa.interner.str(n).to_string()),
                }
            })
            .collect()
    }

    fn create_structs(&mut self, sa: &Sema) {
        for (_struct_id, struct_) in sa.structs.iter() {
            let name = sa.interner.str(struct_.name).to_string();
            let fields = self.create_struct_fields(sa, struct_);
            let module_id = self.convert_module_id(struct_.module_id);
            let type_params = self.create_type_params(sa, struct_.type_param_definition());

            self.structs.push(StructData {
                module_id,
                name,
                type_params,
                fields,
            })
        }
    }

    fn create_struct_fields(&mut self, sa: &Sema, struct_: &StructDefinition) -> Vec<StructField> {
        struct_
            .field_ids()
            .iter()
            .map(|&field_id| {
                let field = sa.field(field_id);
                StructField {
                    ty: self.convert_ty(field.ty()),
                    name: field.name.map(|n| sa.interner.str(n).to_string()),
                }
            })
            .collect()
    }

    fn create_enums(&mut self, sa: &Sema) {
        for (_id, enum_) in sa.enums.iter() {
            let name = sa.interner.str(enum_.name).to_string();
            let variants = self.create_enum_variants(sa, &*enum_);
            let module_id = self.convert_module_id(enum_.module_id);
            let type_params = self.create_type_params(sa, enum_.type_param_definition());

            self.enums.push(EnumData {
                module_id,
                name,
                type_params,
                variants,
            })
        }
    }

    fn create_enum_variants(&mut self, sa: &Sema, enum_: &EnumDefinition) -> Vec<EnumVariant> {
        let mut result = Vec::new();

        for &variant_id in enum_.variant_ids() {
            let variant = sa.variant(variant_id);
            let arguments = variant
                .field_ids()
                .iter()
                .map(|&field_id| {
                    let field = sa.field(field_id);
                    self.convert_ty(field.ty())
                })
                .collect();
            result.push(EnumVariant {
                name: sa.interner.str(variant.name).to_string(),
                arguments,
            })
        }

        result
    }

    fn create_functions(&mut self, sa: &Sema) {
        // First pass: Create all function entries without bytecode.
        for (_id, fct) in sa.fcts.iter() {
            let name = sa.interner.str(fct.name).to_string();

            let kind = match fct.parent {
                FctParent::Extension(extension_id) => {
                    FunctionKind::Extension(self.convert_extension_id(extension_id))
                }
                FctParent::Function => FunctionKind::Lambda,
                FctParent::Impl(impl_id) => FunctionKind::Impl(self.convert_impl_id(impl_id)),
                FctParent::Trait(trait_id) => FunctionKind::Trait(self.convert_trait_id(trait_id)),
                FctParent::None => FunctionKind::Function,
            };

            let file_id = self.convert_source_file_id(fct.file_id);
            let package_id = self.convert_package_id(fct.package_id);
            let module_id = self.convert_module_id(fct.module_id);
            let type_params = self.create_type_params(sa, fct.type_param_definition());
            let params: Vec<_> = fct
                .params_with_self()
                .iter()
                .map(|p| self.convert_ty(p.ty()))
                .collect();
            let return_type = self.convert_ty(fct.return_type());
            let trait_method_impl = fct
                .trait_method_impl
                .get()
                .cloned()
                .map(|id| self.convert_function_id(id));

            self.functions.push(FunctionData {
                name,
                loc: sa.compute_loc(fct.file_id, fct.span),
                kind,
                file_id,
                package_id,
                module_id,
                type_params,
                source_file_id: Some(file_id),
                params,
                return_type,
                is_internal: fct.is_internal,
                is_test: fct.is_test,
                is_optimize_immediately: fct.is_optimize_immediately,
                is_variadic: fct.params.is_variadic(),
                is_force_inline: fct.is_force_inline,
                is_never_inline: fct.is_never_inline,
                is_trait_object_ignore: fct.is_trait_object_ignore,
                bytecode: None,
                trait_method_impl,
            });
        }

        // Second pass: Generate bytecode for all functions.
        for (id, fct) in sa.fcts.iter() {
            if fct.has_body(sa) {
                let analysis = fct.analysis();
                let bc_fct = generate_fct(sa, self, &*fct, analysis);
                let function_id = self.convert_function_id(id);
                self.functions[function_id.index()].bytecode = Some(bc_fct);
            }
        }

        for (_id, global) in sa.globals.iter() {
            if !global.has_initial_value() {
                continue;
            }

            let fct_id = self.functions.len().into();
            let name = sa.interner.str(global.name).to_string();

            let file_id = self.convert_source_file_id(global.file_id);
            let package_id = self.convert_package_id(global.package_id);
            let module_id = self.convert_module_id(global.module_id);
            let type_params = self.create_type_params(sa, &TypeParamDefinition::empty());
            let return_type = self.convert_ty(global.ty());

            let analysis = global.analysis();
            let bc_fct = generate_global_initializer(sa, self, global, analysis);

            self.functions.push(FunctionData {
                name,
                loc: sa.compute_loc(global.file_id, global.span),
                kind: FunctionKind::Function,
                file_id,
                package_id,
                module_id,
                type_params,
                source_file_id: Some(file_id),
                params: Vec::new(),
                return_type,
                is_internal: false,
                is_test: false,
                is_optimize_immediately: false,
                is_variadic: false,
                is_force_inline: false,
                is_never_inline: false,
                is_trait_object_ignore: false,
                bytecode: Some(bc_fct),
                trait_method_impl: None,
            });

            self.global_initializer.insert(global.id(), fct_id);
        }
    }

    fn create_globals(&mut self, sa: &Sema) {
        for (_id, global) in sa.globals.iter() {
            let name = sa.interner.str(global.name).to_string();
            let module_id = self.convert_module_id(global.module_id);
            let ty = self.convert_ty(global.ty());
            let initial_value = self.global_initializer_function_id(&*global);

            self.globals.push(GlobalData {
                module_id,
                ty,
                mutable: global.mutable,
                name,
                initial_value,
            })
        }
    }

    fn create_consts(&mut self, sa: &Sema) {
        for (_id, const_) in sa.consts.iter() {
            let name = sa.interner.str(const_.name).to_string();
            let module_id = self.convert_module_id(const_.module_id);
            let ty = self.convert_ty(const_.ty());

            self.consts.push(ConstData {
                module_id,
                ty,
                name,
                value: const_.value().clone(),
            })
        }
    }

    fn global_initializer_function_id(&self, global: &GlobalDefinition) -> Option<FunctionId> {
        Some(
            self.global_initializer
                .get(&global.id())
                .expect("missing initializer")
                .to_owned(),
        )
    }

    fn create_traits(&mut self, sa: &Sema) {
        for (_id, trait_) in sa.traits.iter() {
            let name = sa.interner.str(trait_.name).to_string();

            let methods = trait_
                .methods()
                .iter()
                .map(|f| self.convert_function_id(*f))
                .collect();

            let virtual_methods = trait_
                .methods()
                .iter()
                .filter(|f| {
                    let method = sa.fct(**f);
                    !method.is_trait_object_ignore
                })
                .map(|f| self.convert_function_id(*f))
                .collect();

            let module_id = self.convert_module_id(trait_.module_id);
            let type_params = self.create_type_params(sa, &trait_.type_param_definition());

            self.traits.push(TraitData {
                module_id,
                name,
                type_params,
                methods,
                virtual_methods,
            })
        }
    }

    fn create_source_files(&mut self, sa: &Sema) {
        for (_id, file) in sa.source_files.iter() {
            self.source_files.push(SourceFileData {
                path: file.path.to_string_lossy().to_string(),
            })
        }
    }

    fn create_extensions(&mut self, sa: &Sema) {
        for (_id, extension) in sa.extensions.iter() {
            let mut methods = Vec::new();

            // The methods array for impl should have the exact same order as for the trait.
            for method_id in extension.methods() {
                methods.push(self.convert_function_id(*method_id));
            }

            let module_id = self.convert_module_id(extension.module_id);
            let type_params = self.create_type_params(sa, extension.type_param_definition());
            let extended_ty = self.convert_ty(extension.ty().clone());

            self.extensions.push(ExtensionData {
                module_id,
                type_params,
                extended_ty,
                methods,
            });
        }
    }

    fn create_impls(&mut self, sa: &Sema) {
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

                methods.push(self.convert_function_id(target_method_id));
            }

            let mut trait_method_map = Vec::new();

            for (trait_method_id, impl_method_id) in impl_.trait_method_map() {
                trait_method_map.push((
                    self.convert_function_id(*trait_method_id),
                    self.convert_function_id(*impl_method_id),
                ))
            }

            let mut trait_alias_map = Vec::new();

            for (trait_alias_id, impl_alias_id) in impl_.trait_alias_map() {
                trait_alias_map.push((
                    self.convert_alias_id(*trait_alias_id),
                    self.convert_alias_id(*impl_alias_id),
                ));
            }

            let module_id = self.convert_module_id(impl_.module_id);
            let type_params = self.create_type_params(sa, impl_.type_param_definition());
            let bc_trait_ty = self.convert_trait_ty(&trait_ty);
            let extended_ty = self.convert_ty(impl_.extended_ty());

            self.impls.push(ImplData {
                module_id,
                type_params,
                trait_ty: bc_trait_ty,
                extended_ty,
                methods,
                trait_method_map,
                trait_alias_map,
            });
        }
    }

    fn create_aliases(&mut self, sa: &Sema) {
        for (_id, alias) in sa.aliases.iter() {
            let ty = alias.parsed_ty().map(|pty| self.convert_ty(pty.ty()));
            self.aliases.push(AliasData {
                name: sa.interner.str(alias.name).to_string(),
                ty,
                idx_in_trait: alias.idx_in_trait,
            })
        }
    }

    fn find_main_fct_id(&mut self, sa: &Sema) -> Option<FunctionId> {
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
            Some(self.convert_function_id(fct.id()))
        }
    }

    pub fn convert_ty(&mut self, ty: SourceType) -> BytecodeType {
        match ty {
            SourceType::Unit => BytecodeType::Unit,
            SourceType::Bool => BytecodeType::Bool,
            SourceType::UInt8 => BytecodeType::UInt8,
            SourceType::Char => BytecodeType::Char,
            SourceType::Int32 => BytecodeType::Int32,
            SourceType::Int64 => BytecodeType::Int64,
            SourceType::Float32 => BytecodeType::Float32,
            SourceType::Float64 => BytecodeType::Float64,
            SourceType::Class(class_id, type_params) => BytecodeType::Class(
                self.convert_class_id(class_id),
                self.convert_tya(&type_params),
            ),
            SourceType::TraitObject(trait_id, type_params, bindings) => BytecodeType::TraitObject(
                self.convert_trait_id(trait_id),
                self.convert_tya(&type_params),
                self.convert_tya(&bindings),
            ),
            SourceType::Enum(enum_id, type_params) => BytecodeType::Enum(
                self.convert_enum_id(enum_id),
                self.convert_tya(&type_params),
            ),
            SourceType::Struct(struct_id, type_params) => BytecodeType::Struct(
                self.convert_struct_id(struct_id),
                self.convert_tya(&type_params),
            ),
            SourceType::Tuple(subtypes) => BytecodeType::Tuple(self.convert_tya(&subtypes)),
            SourceType::TypeParam(idx) => BytecodeType::TypeParam(idx.index() as u32),
            SourceType::Lambda(params, return_type) => BytecodeType::Lambda(
                self.convert_tya(&params),
                Box::new(self.convert_ty(*return_type)),
            ),
            SourceType::Ptr => BytecodeType::Ptr,
            SourceType::This => BytecodeType::This,
            SourceType::Alias(id, type_params) => {
                assert!(type_params.is_empty());
                BytecodeType::TypeAlias(self.convert_alias_id(id))
            }
            SourceType::Assoc { trait_ty, assoc_id } => BytecodeType::Assoc {
                trait_ty: self.convert_trait_ty(&trait_ty),
                assoc_id: self.convert_alias_id(assoc_id),
            },
            SourceType::GenericAssoc {
                tp_id,
                trait_ty,
                assoc_id,
            } => BytecodeType::GenericAssoc {
                type_param_id: tp_id.index().try_into().expect("overflow"),
                trait_ty: self.convert_trait_ty(&trait_ty),
                assoc_id: self.convert_alias_id(assoc_id),
            },
            _ => panic!("SourceType {:?} cannot be converted to BytecodeType", ty),
        }
    }

    pub fn convert_ty_reg(&mut self, ty: SourceType) -> BytecodeType {
        match ty {
            SourceType::Class(..) | SourceType::Lambda(..) => BytecodeType::Ptr,
            _ => self.convert_ty(ty),
        }
    }

    pub fn convert_tya(&mut self, ty: &SourceTypeArray) -> BytecodeTypeArray {
        let mut bytecode_subtypes = Vec::with_capacity(ty.len());
        for subtype in ty.iter() {
            bytecode_subtypes.push(self.convert_ty(subtype));
        }
        BytecodeTypeArray::new(bytecode_subtypes)
    }

    pub fn convert_trait_ty(&mut self, trait_ty: &TraitType) -> BytecodeTraitType {
        let trait_id = self.convert_trait_id(trait_ty.trait_id);
        let type_params = self.convert_tya(&trait_ty.type_params);
        let bindings = trait_ty
            .bindings
            .iter()
            .map(|(alias_id, ty)| {
                (
                    self.convert_alias_id(*alias_id),
                    self.convert_ty(ty.clone()),
                )
            })
            .collect::<Vec<_>>();
        BytecodeTraitType {
            trait_id,
            type_params,
            bindings,
        }
    }

    fn convert_package_id(&mut self, id: PackageDefinitionId) -> PackageId {
        self.map_packages
            .get(&id)
            .cloned()
            .expect("PackageDefinitionId not found in map")
    }

    fn convert_alias_id(&mut self, id: AliasDefinitionId) -> AliasId {
        self.map_aliases
            .get(&id)
            .cloned()
            .expect("AliasDefinitionId not found in map")
    }

    fn convert_module_id(&mut self, id: ModuleDefinitionId) -> ModuleId {
        self.map_modules
            .get(&id)
            .cloned()
            .expect("ModuleDefinitionId not found in map")
    }

    pub fn convert_function_id(&mut self, id: FctDefinitionId) -> FunctionId {
        self.map_functions
            .get(&id)
            .cloned()
            .expect("FctDefinitionId not found in map")
    }

    pub fn convert_global_id(&mut self, id: GlobalDefinitionId) -> GlobalId {
        self.map_globals
            .get(&id)
            .cloned()
            .expect("GlobalDefinitionId not found in map")
    }

    pub fn convert_const_id(&mut self, id: ConstDefinitionId) -> ConstId {
        self.map_consts
            .get(&id)
            .cloned()
            .expect("ConstDefinitionId not found in map")
    }

    pub fn convert_class_id(&mut self, id: ClassDefinitionId) -> ClassId {
        self.map_classes
            .get(&id)
            .cloned()
            .expect("ClassDefinitionId not found in map")
    }

    pub fn convert_struct_id(&mut self, id: StructDefinitionId) -> StructId {
        self.map_structs
            .get(&id)
            .cloned()
            .expect("StructDefinitionId not found in map")
    }

    pub fn convert_enum_id(&mut self, id: EnumDefinitionId) -> EnumId {
        self.map_enums
            .get(&id)
            .cloned()
            .expect("EnumDefinitionId not found in map")
    }

    fn convert_source_file_id(&mut self, id: sema::SourceFileId) -> SourceFileId {
        self.map_source_files
            .get(&id)
            .cloned()
            .expect("SourceFileId not found in map")
    }

    fn convert_impl_id(&mut self, id: ImplDefinitionId) -> ImplId {
        self.map_impls
            .get(&id)
            .cloned()
            .expect("ImplDefinitionId not found in map")
    }

    fn convert_extension_id(&mut self, id: ExtensionDefinitionId) -> ExtensionId {
        self.map_extensions
            .get(&id)
            .cloned()
            .expect("ExtensionDefinitionId not found in map")
    }

    fn convert_trait_id(&mut self, id: TraitDefinitionId) -> TraitId {
        self.map_traits
            .get(&id)
            .cloned()
            .expect("TraitDefinitionId not found in map")
    }
}
