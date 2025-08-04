use std::collections::HashMap;

use crate::Sema;
use dora_bytecode::program::{AliasData, ImplData};
use dora_bytecode::{
    AliasId, BytecodeTraitType, BytecodeType, BytecodeTypeArray, ClassData, ClassField, ClassId,
    EnumData, EnumId, EnumVariant, ExtensionData, ExtensionId, FunctionData, FunctionId,
    FunctionKind, GlobalData, ImplId, ModuleData, ModuleId, PackageData, PackageId, Program,
    SourceFileData, SourceFileId, StructData, StructField, StructId, TraitData, TraitId,
    TypeParamBound, TypeParamData,
};

use crate::generator::{generate_fct, generate_global_initializer};

use crate::sema::{
    self, AliasDefinitionId, ClassDefinition, Element, EnumDefinition, ExtensionDefinitionId,
    FctDefinitionId, FctParent, GlobalDefinition, GlobalDefinitionId, ImplDefinitionId,
    ModuleDefinitionId, PackageDefinitionId, PackageName, StructDefinition, TraitDefinitionId,
    TypeParamDefinition,
};
use crate::{SourceType, SourceTypeArray, TraitType};

pub fn emit_program(sa: Sema) -> Program {
    let mut emitter = Emitter::new();

    emitter.create_packages(&sa);
    emitter.create_modules(&sa);
    emitter.create_classes(&sa);
    emitter.create_structs(&sa);
    emitter.create_functions(&sa);
    emitter.create_globals(&sa);
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
    map_functions: HashMap<FctDefinitionId, FunctionId>,
    functions: Vec<FunctionData>,
    globals: Vec<GlobalData>,
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
            map_functions: HashMap::new(),
            functions: Vec::new(),
            globals: Vec::new(),
            packages: Vec::new(),
            modules: Vec::new(),
            classes: Vec::new(),
            structs: Vec::new(),
            enums: Default::default(),
            traits: Default::default(),
            source_files: Default::default(),
            extensions: Default::default(),
            impls: Default::default(),
            aliases: Default::default(),
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

            self.packages.push(PackageData {
                name,
                root_module_id: self.convert_module_id(pkg.top_level_module_id()),
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
                parent_id: module.parent_module_id.map(|id| self.convert_module_id(id)),
            })
        }
    }

    fn create_classes(&mut self, sa: &Sema) {
        for (_class_id, class) in sa.classes.iter() {
            let name = sa.interner.str(class.name).to_string();
            let fields = self.create_class_fields(sa, &*class);

            self.classes.push(ClassData {
                module_id: self.convert_module_id(class.module_id),
                name,
                type_params: self.create_type_params(sa, class.type_param_definition()),
                fields,
            })
        }
    }

    fn create_type_params(&self, sa: &Sema, type_params: &TypeParamDefinition) -> TypeParamData {
        let names = type_params
            .names()
            .map(|(_, name)| sa.interner.str(name).to_string())
            .collect();

        let bounds = type_params
            .bounds()
            .map(|b| TypeParamBound {
                ty: self.convert_ty(b.ty()),
                trait_ty: self.convert_trait_ty(b.trait_ty().as_ref().expect("missing trait type")),
            })
            .collect();

        let container_count = type_params.container_type_params();

        TypeParamData {
            names,
            container_count,
            bounds,
        }
    }

    fn create_class_fields(&mut self, sa: &Sema, class: &ClassDefinition) -> Vec<ClassField> {
        class
            .fields
            .iter()
            .map(|f| ClassField {
                ty: self.convert_ty(f.ty()),
                name: f.name.map(|n| sa.interner.str(n).to_string()),
            })
            .collect()
    }

    fn create_structs(&mut self, sa: &Sema) {
        for (_struct_id, struct_) in sa.structs.iter() {
            let name = sa.interner.str(struct_.name).to_string();
            let fields = self.create_struct_fields(sa, struct_);

            self.structs.push(StructData {
                module_id: self.convert_module_id(struct_.module_id),
                name,
                type_params: self.create_type_params(sa, struct_.type_param_definition()),
                fields,
            })
        }
    }

    fn create_struct_fields(&mut self, sa: &Sema, struct_: &StructDefinition) -> Vec<StructField> {
        struct_
            .fields
            .iter()
            .map(|f| StructField {
                ty: self.convert_ty(f.ty()),
                name: f.name.map(|n| sa.interner.str(n).to_string()),
            })
            .collect()
    }

    fn create_enums(&mut self, sa: &Sema) {
        for (_id, enum_) in sa.enums.iter() {
            let name = sa.interner.str(enum_.name).to_string();
            let variants = self.create_enum_variants(sa, &*enum_);

            self.enums.push(EnumData {
                module_id: self.convert_module_id(enum_.module_id),
                name,
                type_params: self.create_type_params(sa, enum_.type_param_definition()),
                variants,
            })
        }
    }

    fn create_enum_variants(&mut self, sa: &Sema, enum_: &EnumDefinition) -> Vec<EnumVariant> {
        let mut result = Vec::new();

        for variant in enum_.variants() {
            let arguments = variant
                .fields
                .iter()
                .map(|f| self.convert_ty(f.parsed_type.ty()))
                .collect();
            result.push(EnumVariant {
                name: sa.interner.str(variant.name).to_string(),
                arguments,
            })
        }

        result
    }

    fn create_functions(&mut self, sa: &Sema) {
        for (id, fct) in sa.fcts.iter() {
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

            let bc_fct = if fct.has_body() {
                let analysis = fct.analysis();
                Some(generate_fct(sa, self, &*fct, analysis))
            } else {
                None
            };

            let function_id = FunctionId(self.functions.len().try_into().expect("overflow"));
            self.functions.push(FunctionData {
                name,
                loc: sa.compute_loc(fct.file_id, fct.span),
                kind,
                file_id: self.convert_source_file_id(fct.file_id),
                package_id: self.convert_package_id(fct.package_id),
                module_id: self.convert_module_id(fct.module_id),
                type_params: self.create_type_params(sa, fct.type_param_definition()),
                source_file_id: Some(self.convert_source_file_id(fct.file_id)),
                params: fct
                    .params_with_self()
                    .iter()
                    .map(|p| self.convert_ty(p.ty()))
                    .collect(),
                return_type: self.convert_ty(fct.return_type()),
                is_internal: fct.is_internal,
                is_test: fct.is_test,
                is_optimize_immediately: fct.is_optimize_immediately,
                is_variadic: fct.params.is_variadic(),
                is_force_inline: fct.is_force_inline,
                is_never_inline: fct.is_never_inline,
                is_trait_object_ignore: fct.is_trait_object_ignore,
                bytecode: bc_fct,
                trait_method_impl: fct
                    .trait_method_impl
                    .get()
                    .cloned()
                    .map(|id| self.convert_function_id(id)),
            });

            self.map_functions.insert(id, function_id);
        }

        for (_id, global) in sa.globals.iter() {
            if !global.has_initial_value() {
                continue;
            }

            let fct_id = FunctionId(self.functions.len().try_into().expect("overflow"));
            let name = sa.interner.str(global.name).to_string();

            let analysis = global.analysis();
            let bc_fct = generate_global_initializer(sa, self, global, analysis);

            self.functions.push(FunctionData {
                name,
                loc: sa.compute_loc(global.file_id, global.span),
                kind: FunctionKind::Function,
                file_id: self.convert_source_file_id(global.file_id),
                package_id: self.convert_package_id(global.package_id),
                module_id: self.convert_module_id(global.module_id),
                type_params: self.create_type_params(sa, &TypeParamDefinition::empty()),
                source_file_id: Some(self.convert_source_file_id(global.file_id)),
                params: Vec::new(),
                return_type: self.convert_ty(global.ty()),
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

            self.globals.push(GlobalData {
                module_id: self.convert_module_id(global.module_id),
                ty: self.convert_ty(global.ty()),
                mutable: global.mutable,
                name,
                initial_value: self.global_initializer_function_id(&*global),
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

            self.traits.push(TraitData {
                module_id: self.convert_module_id(trait_.module_id),
                name,
                type_params: self.create_type_params(sa, &trait_.type_param_definition()),
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

            self.extensions.push(ExtensionData {
                module_id: self.convert_module_id(extension.module_id),
                type_params: self.create_type_params(sa, extension.type_param_definition()),
                extended_ty: self.convert_ty(extension.ty().clone()),
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

            self.impls.push(ImplData {
                module_id: self.convert_module_id(impl_.module_id),
                type_params: self.create_type_params(sa, impl_.type_param_definition()),
                trait_ty: self.convert_trait_ty(&trait_ty),
                extended_ty: self.convert_ty(impl_.extended_ty()),
                methods,
                trait_method_map,
                trait_alias_map,
            });
        }
    }

    fn create_aliases(&mut self, sa: &Sema) {
        for (_id, alias) in sa.aliases.iter() {
            self.aliases.push(AliasData {
                name: sa.interner.str(alias.name).to_string(),
                ty: alias.parsed_ty().map(|pty| self.convert_ty(pty.ty())),
                idx_in_trait: alias.idx_in_trait,
            })
        }
    }

    fn find_main_fct_id(&self, sa: &Sema) -> Option<FunctionId> {
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

    pub fn convert_ty(&self, ty: SourceType) -> BytecodeType {
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
                ClassId(class_id.index().try_into().expect("overflow")),
                self.convert_tya(&type_params),
            ),
            SourceType::TraitObject(trait_id, type_params, bindings) => BytecodeType::TraitObject(
                TraitId(trait_id.index().try_into().expect("overflow")),
                self.convert_tya(&type_params),
                self.convert_tya(&bindings),
            ),
            SourceType::Enum(enum_id, type_params) => BytecodeType::Enum(
                EnumId(enum_id.index().try_into().expect("overflow")),
                self.convert_tya(&type_params),
            ),
            SourceType::Struct(struct_id, type_params) => BytecodeType::Struct(
                StructId(struct_id.index().try_into().expect("overflow")),
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
                BytecodeType::TypeAlias(AliasId(id.index().try_into().expect("overflow")))
            }
            SourceType::Assoc { trait_ty, assoc_id } => BytecodeType::Assoc {
                trait_ty: self.convert_trait_ty(&trait_ty),
                assoc_id: AliasId(assoc_id.index().try_into().expect("overflow")),
            },
            SourceType::GenericAssoc {
                tp_id,
                trait_ty,
                assoc_id,
            } => BytecodeType::GenericAssoc {
                type_param_id: tp_id.index().try_into().expect("overflow"),
                trait_ty: self.convert_trait_ty(&trait_ty),
                assoc_id: AliasId(assoc_id.index().try_into().expect("overflow")),
            },
            _ => panic!("SourceType {:?} cannot be converted to BytecodeType", ty),
        }
    }

    pub fn convert_tya(&self, ty: &SourceTypeArray) -> BytecodeTypeArray {
        let mut bytecode_subtypes = Vec::with_capacity(ty.len());
        for subtype in ty.iter() {
            bytecode_subtypes.push(self.convert_ty(subtype));
        }
        BytecodeTypeArray::new(bytecode_subtypes)
    }

    pub fn convert_trait_ty(&self, trait_ty: &TraitType) -> BytecodeTraitType {
        BytecodeTraitType {
            trait_id: self.convert_trait_id(trait_ty.trait_id),
            type_params: self.convert_tya(&trait_ty.type_params),
            bindings: trait_ty
                .bindings
                .iter()
                .map(|(alias_id, ty)| {
                    (
                        AliasId(alias_id.index().try_into().expect("overflow")),
                        self.convert_ty(ty.clone()),
                    )
                })
                .collect::<Vec<_>>(),
        }
    }

    fn convert_package_id(&self, id: PackageDefinitionId) -> PackageId {
        PackageId(id.index().try_into().expect("failure"))
    }

    fn convert_alias_id(&self, id: AliasDefinitionId) -> AliasId {
        AliasId(id.index().try_into().expect("failure"))
    }

    fn convert_module_id(&self, id: ModuleDefinitionId) -> ModuleId {
        ModuleId(id.index().try_into().expect("failure"))
    }

    fn convert_function_id(&self, id: FctDefinitionId) -> FunctionId {
        FunctionId(id.index().try_into().expect("failure"))
    }

    fn convert_source_file_id(&self, id: sema::SourceFileId) -> SourceFileId {
        SourceFileId(id.index().try_into().expect("failure"))
    }

    fn convert_impl_id(&self, id: ImplDefinitionId) -> ImplId {
        ImplId(id.index().try_into().expect("failure"))
    }

    fn convert_extension_id(&self, id: ExtensionDefinitionId) -> ExtensionId {
        ExtensionId(id.index().try_into().expect("failure"))
    }

    fn convert_trait_id(&self, id: TraitDefinitionId) -> TraitId {
        TraitId(id.index().try_into().expect("failure"))
    }
}
