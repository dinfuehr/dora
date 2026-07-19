use std::collections::HashMap;

use crate::Sema;
use crate::sym::SymbolKind;
use dora_bytecode::program::{AliasData, ImplData};
use dora_bytecode::{
    AliasId, BytecodeTraitType, BytecodeType, BytecodeTypeArray, ClassData, ClassField, ClassId,
    ConstData, ConstId, EnumData, EnumId, EnumVariant, EnumVariantField, ExtensionData,
    ExtensionId, FunctionData, FunctionId, FunctionKind, GlobalData, GlobalId, ImplId, ModuleData,
    ModuleElementId, ModuleId, PackageData, PackageId, Program, SourceFileData, SourceFileId,
    StructData, StructField, StructId, TraitData, TraitId, TypeParamBound, TypeParamData,
    verify_program,
};

use crate::generator::{generate_fct, generate_global_initializer};

use crate::sema::{
    self, AliasDefinitionId, ClassDefinition, ClassDefinitionId, ConstDefinitionId, Element,
    EnumDefinition, EnumDefinitionId, ExtensionDefinitionId, FctDefinitionId, FctParent,
    GlobalDefinition, GlobalDefinitionId, ImplDefinitionId, ModuleDefinitionId,
    PackageDefinitionId, PackageName, StructDefinition, StructDefinitionId, TraitDefinitionId,
    TypeParamDefinition, TypeParamDefinitionId,
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

    let stdlib_package_id = emitter.convert_package_id(&sa, sa.stdlib_package_id());
    let program_package_id = emitter.convert_package_id(&sa, sa.program_package_id());
    let main_fct_id = emitter.find_main_fct_id(&sa);

    let program = Program {
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
        main_fct_id,
    };

    verify_program(&program);

    program
}

pub struct Emitter {
    // Semantic types carry arena IDs. Bytecode types still use indices local to
    // the element's type-parameter definition.
    type_param_definition: Option<TypeParamDefinitionId>,
    // Generated closures and context classes in trait bodies carry `Self` as a
    // final bytecode-only type parameter. Frontend type parameters stay unchanged.
    hidden_self_type_param: Option<u32>,
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
            type_param_definition: None,
            hidden_self_type_param: None,
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
                is_public: true,
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
                    container_bound_count: 0,
                    bounds: Vec::new(),
                },
                source_file_id: None,
                params: Vec::new(),
                return_type: BytecodeType::Unit,
                is_public: false,
                is_static: false,
                is_mutating: false,
                is_internal: false,
                is_native: false,
                is_test: false,
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
                is_public: false,
                name: "<dummy>".into(),
                initial_value: None,
            });
        }

        for _ in 0..count {
            self.consts.push(ConstData {
                module_id: 0usize.into(),
                ty: BytecodeType::Unit,
                name: "<dummy>".into(),
                is_public: false,
                value: dora_bytecode::ConstValue::None,
            });
        }

        for _ in 0..count {
            self.classes.push(ClassData {
                package_id: 0usize.into(),
                module_id: 0usize.into(),
                name: "<dummy>".into(),
                type_params: TypeParamData {
                    names: Vec::new(),
                    container_count: 0,
                    container_bound_count: 0,
                    bounds: Vec::new(),
                },
                is_public: false,
                is_internal: false,
                fields: Vec::new(),
            });
        }

        for _ in 0..count {
            self.structs.push(StructData {
                package_id: 0usize.into(),
                module_id: 0usize.into(),
                name: "<dummy>".into(),
                type_params: TypeParamData {
                    names: Vec::new(),
                    container_count: 0,
                    container_bound_count: 0,
                    bounds: Vec::new(),
                },
                is_public: false,
                is_internal: false,
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
                    container_bound_count: 0,
                    bounds: Vec::new(),
                },
                is_public: false,
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
                    container_bound_count: 0,
                    bounds: Vec::new(),
                },
                is_public: false,
                is_trait_object: false,
                aliases: Vec::new(),
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
                    container_bound_count: 0,
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
                    container_bound_count: 0,
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
                module_id: 0usize.into(),
                name: "<dummy>".into(),
                type_params: TypeParamData {
                    names: Vec::new(),
                    container_count: 0,
                    container_bound_count: 0,
                    bounds: Vec::new(),
                },
                is_public: false,
                ty: None,
                idx_in_trait: None,
            });
        }
    }

    fn create_packages(&mut self, sa: &Sema) {
        for (_id, pkg) in sa.packages.iter() {
            let name = match pkg.name {
                PackageName::Std => "std".into(),
                PackageName::Program => "program".into(),
                PackageName::External(ref name) => name.clone(),
            };

            let root_module_id = self.convert_module_id(sa, pkg.top_level_module_id());
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
                        ModuleElementId::Class(self.convert_class_id(sa, *class_id))
                    }
                    SymbolKind::Enum(enum_id) => {
                        ModuleElementId::Enum(self.convert_enum_id(sa, *enum_id))
                    }
                    SymbolKind::Struct(struct_id) => {
                        ModuleElementId::Struct(self.convert_struct_id(sa, *struct_id))
                    }
                    SymbolKind::Trait(trait_id) => {
                        ModuleElementId::Trait(self.convert_trait_id(sa, *trait_id))
                    }
                    SymbolKind::Module(module_id) => {
                        ModuleElementId::Module(self.convert_module_id(sa, *module_id))
                    }
                    SymbolKind::Fct(fct_id) => {
                        ModuleElementId::Function(self.convert_function_id(sa, *fct_id))
                    }
                    SymbolKind::Global(global_id) => {
                        ModuleElementId::Global(self.convert_global_id(sa, *global_id))
                    }
                    SymbolKind::Const(const_id) => {
                        ModuleElementId::Const(self.convert_const_id(sa, *const_id))
                    }
                    SymbolKind::EnumVariant(enum_id, variant_id) => ModuleElementId::EnumVariant(
                        self.convert_enum_id(sa, *enum_id),
                        *variant_id,
                    ),
                    SymbolKind::Alias(alias_id) => {
                        ModuleElementId::Alias(self.convert_alias_id(sa, *alias_id))
                    }
                    _ => {
                        println!("sym = {:?}", sym.kind());
                        unreachable!()
                    }
                };

                items.push((name, item));
            }

            items.sort_by(|(k1, _), (k2, _)| k1.cmp(k2));

            let parent_id = module
                .parent_module_id
                .map(|id| self.convert_module_id(sa, id));

            self.modules.push(ModuleData {
                name,
                parent_id,
                items,
                is_public: module.visibility.is_public(),
            })
        }
    }

    fn create_classes(&mut self, sa: &Sema) {
        for (_class_id, class) in sa.classes.iter() {
            self.enter_type_param_definition(class.type_param_definition_id());
            assert!(self.hidden_self_type_param.is_none());
            if class.needs_self_type_param {
                self.hidden_self_type_param = Some(
                    class
                        .type_param_definition(sa)
                        .type_param_count()
                        .try_into()
                        .expect("type parameter overflow"),
                );
            }

            let name = sa.interner.str(class.name).to_string();
            let fields = self.create_class_fields(sa, &*class);

            let package_id = self.convert_package_id(sa, class.package_id);
            let module_id = self.convert_module_id(sa, class.module_id);
            let type_params = self.create_type_params(sa, class.type_param_definition(sa));
            self.classes.push(ClassData {
                package_id,
                module_id,
                name,
                type_params,
                is_public: class.visibility.is_public(),
                is_internal: class.is_internal,
                fields,
            });
            self.hidden_self_type_param = None;
            self.leave_type_param_definition();
        }
    }

    fn enter_type_param_definition(&mut self, id: TypeParamDefinitionId) {
        assert!(self.type_param_definition.replace(id).is_none());
    }

    fn leave_type_param_definition(&mut self) {
        assert!(self.type_param_definition.take().is_some());
    }

    fn create_type_params(
        &mut self,
        sa: &Sema,
        type_params: &TypeParamDefinition,
    ) -> TypeParamData {
        let mut names: Vec<_> = type_params
            .names(sa)
            .map(|(_, name)| sa.interner.str(name).to_string())
            .collect();

        if let Some(self_type_param) = self.hidden_self_type_param {
            assert_eq!(self_type_param as usize, names.len());
            names.push("$Self".into());
        }

        let mut bounds = Vec::new();
        for b in type_params.bounds(sa) {
            let ty = self.convert_ty(sa, b.ty());
            let trait_ty =
                self.convert_trait_ty(sa, b.trait_ty().as_ref().expect("missing trait type"));
            bounds.push(TypeParamBound { ty, trait_ty });
        }

        let container_count = type_params.container_type_params();
        let container_bound_count = type_params.container_bounds();

        TypeParamData {
            names,
            container_count,
            container_bound_count,
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
                    ty: self.convert_ty(sa, field.ty()),
                    name: field.name.map(|n| sa.interner.str(n).to_string()),
                    is_public: field.visibility.is_public(),
                    mutable: field.mutable,
                }
            })
            .collect()
    }

    fn create_structs(&mut self, sa: &Sema) {
        for (_struct_id, struct_) in sa.structs.iter() {
            self.enter_type_param_definition(struct_.type_param_definition_id());
            let name = sa.interner.str(struct_.name).to_string();
            let fields = self.create_struct_fields(sa, struct_);
            let package_id = self.convert_package_id(sa, struct_.package_id);
            let module_id = self.convert_module_id(sa, struct_.module_id);
            let type_params = self.create_type_params(sa, struct_.type_param_definition(sa));

            self.structs.push(StructData {
                package_id,
                module_id,
                name,
                type_params,
                is_public: struct_.visibility.is_public(),
                is_internal: struct_.is_internal,
                fields,
            });
            self.leave_type_param_definition();
        }
    }

    fn create_struct_fields(&mut self, sa: &Sema, struct_: &StructDefinition) -> Vec<StructField> {
        struct_
            .field_ids()
            .iter()
            .map(|&field_id| {
                let field = sa.field(field_id);
                StructField {
                    ty: self.convert_ty(sa, field.ty()),
                    name: field.name.map(|n| sa.interner.str(n).to_string()),
                    is_public: field.visibility.is_public(),
                    mutable: field.mutable,
                }
            })
            .collect()
    }

    fn create_enums(&mut self, sa: &Sema) {
        for (_id, enum_) in sa.enums.iter() {
            self.enter_type_param_definition(enum_.type_param_definition_id());
            let name = sa.interner.str(enum_.name).to_string();
            let variants = self.create_enum_variants(sa, &*enum_);
            let module_id = self.convert_module_id(sa, enum_.module_id);
            let type_params = self.create_type_params(sa, enum_.type_param_definition(sa));

            self.enums.push(EnumData {
                module_id,
                name,
                type_params,
                is_public: enum_.visibility.is_public(),
                variants,
            });
            self.leave_type_param_definition();
        }
    }

    fn create_enum_variants(&mut self, sa: &Sema, enum_: &EnumDefinition) -> Vec<EnumVariant> {
        let mut result = Vec::new();

        for &variant_id in enum_.variant_ids() {
            let variant = sa.variant(variant_id);
            let fields = variant
                .field_ids()
                .iter()
                .map(|&field_id| {
                    let field = sa.field(field_id);
                    EnumVariantField {
                        ty: self.convert_ty(sa, field.ty()),
                        name: field.name.map(|n| sa.interner.str(n).to_string()),
                    }
                })
                .collect();
            result.push(EnumVariant {
                name: sa.interner.str(variant.name).to_string(),
                fields,
            })
        }

        result
    }

    fn create_functions(&mut self, sa: &Sema) {
        // First pass: Create all function entries without bytecode.
        for (_id, fct) in sa.fcts.iter() {
            self.enter_type_param_definition(fct.type_param_definition_id());
            assert!(self.hidden_self_type_param.is_none());
            if fct.needs_self_type_param(sa) {
                self.hidden_self_type_param = Some(
                    fct.type_param_definition(sa)
                        .type_param_count()
                        .try_into()
                        .expect("type parameter overflow"),
                );
            }

            let name = sa.interner.str(fct.name).to_string();

            let kind = match fct.parent {
                FctParent::Extension(extension_id) => {
                    FunctionKind::Extension(self.convert_extension_id(sa, extension_id))
                }
                FctParent::Function => FunctionKind::Lambda,
                FctParent::Impl(impl_id) => FunctionKind::Impl(self.convert_impl_id(sa, impl_id)),
                FctParent::Trait(trait_id) => {
                    FunctionKind::Trait(self.convert_trait_id(sa, trait_id))
                }
                FctParent::None => FunctionKind::Function,
            };

            let file_id = self.convert_source_file_id(sa, fct.file_id);
            let package_id = self.convert_package_id(sa, fct.package_id);
            let module_id = self.convert_module_id(sa, fct.module_id);
            let type_params = self.create_type_params(sa, fct.type_param_definition(sa));
            let params: Vec<_> = fct
                .params_with_self()
                .iter()
                .map(|p| self.convert_ty(sa, p.ty()))
                .collect();
            let return_type = self.convert_ty(sa, fct.return_type());
            let trait_method_impl = fct
                .trait_method_impl
                .get()
                .cloned()
                .map(|id| self.convert_function_id(sa, id));

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
                is_public: fct.visibility.is_public(),
                is_static: fct.is_static,
                is_mutating: fct.is_mutating,
                is_internal: fct.is_internal,
                is_native: fct.is_native,
                is_test: fct.is_test,
                is_variadic: fct.params.is_variadic(),
                is_force_inline: fct.is_force_inline,
                is_never_inline: fct.is_never_inline,
                is_trait_object_ignore: fct.is_trait_object_ignore,
                bytecode: None,
                trait_method_impl,
            });
            self.hidden_self_type_param = None;
            self.leave_type_param_definition();
        }

        // Second pass: Generate bytecode for all functions.
        for (id, fct) in sa.fcts.iter() {
            if fct.has_body(sa) {
                self.enter_type_param_definition(fct.type_param_definition_id());
                assert!(self.hidden_self_type_param.is_none());
                if fct.needs_self_type_param(sa) {
                    self.hidden_self_type_param = Some(
                        fct.type_param_definition(sa)
                            .type_param_count()
                            .try_into()
                            .expect("type parameter overflow"),
                    );
                }

                let analysis = fct.analysis();
                let bc_fct = generate_fct(sa, self, &*fct, analysis);
                let function_id = self.convert_function_id(sa, id);
                self.functions[function_id.index()].bytecode = Some(bc_fct);
                self.hidden_self_type_param = None;
                self.leave_type_param_definition();
            }
        }

        for (_id, global) in sa.globals.iter() {
            if !global.has_initial_value() {
                continue;
            }

            self.enter_type_param_definition(global.type_param_definition_id());

            let fct_id = self.functions.len().into();
            let name = sa.interner.str(global.name).to_string();

            let file_id = self.convert_source_file_id(sa, global.file_id);
            let package_id = self.convert_package_id(sa, global.package_id);
            let module_id = self.convert_module_id(sa, global.module_id);
            let type_params = self.create_type_params(sa, global.type_param_definition(sa));
            let return_type = self.convert_ty(sa, global.ty());

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
                is_public: false,
                is_static: false,
                is_mutating: false,
                is_internal: false,
                is_native: false,
                is_test: false,
                is_variadic: false,
                is_force_inline: false,
                is_never_inline: false,
                is_trait_object_ignore: false,
                bytecode: Some(bc_fct),
                trait_method_impl: None,
            });

            self.global_initializer.insert(global.id(), fct_id);
            self.leave_type_param_definition();
        }
    }

    fn create_globals(&mut self, sa: &Sema) {
        for (_id, global) in sa.globals.iter() {
            self.enter_type_param_definition(global.type_param_definition_id());
            let name = sa.interner.str(global.name).to_string();
            let module_id = self.convert_module_id(sa, global.module_id);
            let ty = self.convert_ty(sa, global.ty());
            let initial_value = self.global_initializer_function_id(&*global);

            self.globals.push(GlobalData {
                module_id,
                ty,
                mutable: global.mutable,
                is_public: global.visibility.is_public(),
                name,
                initial_value,
            });
            self.leave_type_param_definition();
        }
    }

    fn create_consts(&mut self, sa: &Sema) {
        for (_id, const_) in sa.consts.iter() {
            self.enter_type_param_definition(const_.type_param_definition_id());
            let name = sa.interner.str(const_.name).to_string();
            let module_id = self.convert_module_id(sa, const_.module_id);
            let ty = self.convert_ty(sa, const_.ty());

            self.consts.push(ConstData {
                module_id,
                ty,
                name,
                is_public: const_.visibility.is_public(),
                value: const_.value().clone(),
            });
            self.leave_type_param_definition();
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
            self.enter_type_param_definition(trait_.type_param_definition_id());
            let name = sa.interner.str(trait_.name).to_string();

            let methods = trait_
                .methods()
                .iter()
                .map(|f| self.convert_function_id(sa, *f))
                .collect();

            let virtual_methods = trait_
                .methods()
                .iter()
                .filter(|f| {
                    let method = sa.fct(**f);
                    !method.is_trait_object_ignore
                })
                .map(|f| self.convert_function_id(sa, *f))
                .collect();

            let module_id = self.convert_module_id(sa, trait_.module_id);
            let type_params = self.create_type_params(sa, trait_.type_param_definition(sa));
            let aliases = trait_
                .aliases()
                .iter()
                .map(|alias_id| self.convert_alias_id(sa, *alias_id))
                .collect();

            self.traits.push(TraitData {
                module_id,
                name,
                type_params,
                is_public: trait_.visibility.is_public(),
                is_trait_object: trait_.is_trait_object,
                aliases,
                methods,
                virtual_methods,
            });
            self.leave_type_param_definition();
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
            self.enter_type_param_definition(extension.type_param_definition_id());
            let mut methods = Vec::new();

            // The methods array for impl should have the exact same order as for the trait.
            for method_id in extension.methods() {
                methods.push(self.convert_function_id(sa, *method_id));
            }

            let module_id = self.convert_module_id(sa, extension.module_id);
            let type_params = self.create_type_params(sa, extension.type_param_definition(sa));
            let extended_ty = self.convert_ty(sa, extension.ty().clone());

            self.extensions.push(ExtensionData {
                module_id,
                type_params,
                extended_ty,
                methods,
            });
            self.leave_type_param_definition();
        }
    }

    fn create_impls(&mut self, sa: &Sema) {
        for (_id, impl_) in sa.impls.iter() {
            self.enter_type_param_definition(impl_.type_param_definition_id());
            let mut methods = Vec::new();

            let trait_ty = impl_.trait_ty().expect("trait expected");
            let trait_ = sa.trait_(trait_ty.trait_id);

            // The methods array for impl should have the exact same order as for the trait.
            for method_id in trait_.methods() {
                let target_method_id = *impl_
                    .trait_method_map()
                    .get(&method_id)
                    .expect("missing impl for trait methdo");

                methods.push(self.convert_function_id(sa, target_method_id));
            }

            let mut trait_method_map = Vec::new();

            for trait_method_id in trait_.methods() {
                let impl_method_id = impl_
                    .trait_method_map()
                    .get(trait_method_id)
                    .expect("missing impl for trait method");
                trait_method_map.push((
                    self.convert_function_id(sa, *trait_method_id),
                    self.convert_function_id(sa, *impl_method_id),
                ))
            }

            let mut trait_alias_map = Vec::new();

            for trait_alias_id in trait_.aliases() {
                let impl_alias_id = impl_
                    .trait_alias_map()
                    .get(trait_alias_id)
                    .expect("missing impl for trait alias");
                trait_alias_map.push((
                    self.convert_alias_id(sa, *trait_alias_id),
                    self.convert_alias_id(sa, *impl_alias_id),
                ));
            }

            let module_id = self.convert_module_id(sa, impl_.module_id);
            let type_params = self.create_type_params(sa, impl_.type_param_definition(sa));
            let bc_trait_ty = self.convert_trait_ty(sa, &trait_ty);
            let extended_ty = self.convert_ty(sa, impl_.extended_ty());

            self.impls.push(ImplData {
                module_id,
                type_params,
                trait_ty: bc_trait_ty,
                extended_ty,
                methods,
                trait_method_map,
                trait_alias_map,
            });
            self.leave_type_param_definition();
        }
    }

    fn create_aliases(&mut self, sa: &Sema) {
        for (_id, alias) in sa.aliases.iter() {
            self.enter_type_param_definition(alias.type_param_definition_id());
            let ty = alias.parsed_ty().map(|pty| self.convert_ty(sa, pty.ty()));
            let module_id = self.convert_module_id(sa, alias.module_id);
            let type_params = self.create_type_params(sa, alias.type_param_definition(sa));
            self.aliases.push(AliasData {
                module_id,
                name: sa.interner.str(alias.name).to_string(),
                type_params,
                is_public: alias.visibility.is_public(),
                ty,
                idx_in_trait: alias.idx_in_trait,
            });
            self.leave_type_param_definition();
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
            || !fct.type_param_definition(sa).is_empty()
        {
            None
        } else {
            Some(self.convert_function_id(sa, fct.id()))
        }
    }

    pub fn convert_ty(&mut self, sa: &Sema, ty: SourceType) -> BytecodeType {
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
                self.convert_class_id(sa, class_id),
                self.convert_tya(sa, &type_params),
            ),
            SourceType::TraitObject(trait_id, type_params, bindings) => BytecodeType::TraitObject(
                self.convert_trait_id(sa, trait_id),
                self.convert_tya(sa, &type_params),
                self.convert_tya(sa, &bindings),
            ),
            SourceType::Enum(enum_id, type_params) => BytecodeType::Enum(
                self.convert_enum_id(sa, enum_id),
                self.convert_tya(sa, &type_params),
            ),
            SourceType::Struct(struct_id, type_params) => BytecodeType::Struct(
                self.convert_struct_id(sa, struct_id),
                self.convert_tya(sa, &type_params),
            ),
            SourceType::Tuple(subtypes) => BytecodeType::Tuple(self.convert_tya(sa, &subtypes)),
            SourceType::TypeParam(id) => {
                let definition_id = self
                    .type_param_definition
                    .expect("type parameter definition missing during bytecode emission");
                let idx = sa
                    .type_param_definition(definition_id)
                    .type_param_idx(sa, id)
                    .unwrap_or_else(|| {
                        panic!(
                            "type parameter {:?} ({}) missing from definition {:?} during bytecode emission",
                            id,
                            sa.interner.str(sa.type_param(id).name()),
                            definition_id,
                        )
                    });
                BytecodeType::TypeParam(idx.index() as u32)
            }
            SourceType::Lambda(params, return_type, is_variadic) => BytecodeType::Lambda(
                self.convert_tya(sa, &params),
                Box::new(self.convert_ty(sa, *return_type)),
                is_variadic,
            ),
            SourceType::Ptr => BytecodeType::Ptr,
            SourceType::This => self
                .hidden_self_type_param
                .map(BytecodeType::TypeParam)
                .unwrap_or(BytecodeType::This),
            SourceType::Ref(inner) => {
                BytecodeType::Ref(Box::new(self.convert_ty(sa, inner.as_ref().clone())))
            }
            SourceType::Alias(id, type_params) => {
                assert!(type_params.is_empty());
                BytecodeType::TypeAlias(self.convert_alias_id(sa, id))
            }
            SourceType::Assoc { trait_ty, assoc_id } => BytecodeType::Assoc {
                ty: Box::new(self.convert_ty(sa, SourceType::This)),
                trait_ty: self.convert_trait_ty(sa, &trait_ty),
                assoc_id: self.convert_alias_id(sa, assoc_id),
            },
            SourceType::GenericAssoc {
                ty,
                trait_ty,
                assoc_id,
            } => BytecodeType::Assoc {
                ty: Box::new(self.convert_ty(sa, ty.as_ref().clone())),
                trait_ty: self.convert_trait_ty(sa, &trait_ty),
                assoc_id: self.convert_alias_id(sa, assoc_id),
            },
            _ => panic!("SourceType {:?} cannot be converted to BytecodeType", ty),
        }
    }

    pub fn convert_tya(&mut self, _sa: &Sema, ty: &SourceTypeArray) -> BytecodeTypeArray {
        let mut bytecode_subtypes = Vec::with_capacity(ty.len());
        for subtype in ty.iter() {
            bytecode_subtypes.push(self.convert_ty(_sa, subtype));
        }
        BytecodeTypeArray::new(bytecode_subtypes)
    }

    pub fn convert_trait_ty(&mut self, _sa: &Sema, trait_ty: &TraitType) -> BytecodeTraitType {
        let trait_id = self.convert_trait_id(_sa, trait_ty.trait_id);
        let type_params = self.convert_tya(_sa, &trait_ty.type_params);
        let bindings = trait_ty
            .bindings
            .iter()
            .map(|(alias_id, ty)| {
                (
                    self.convert_alias_id(_sa, *alias_id),
                    self.convert_ty(_sa, ty.clone()),
                )
            })
            .collect::<Vec<_>>();
        BytecodeTraitType {
            trait_id,
            type_params,
            bindings,
        }
    }

    fn convert_package_id(&mut self, _sa: &Sema, id: PackageDefinitionId) -> PackageId {
        self.map_packages
            .get(&id)
            .cloned()
            .expect("PackageDefinitionId not found in map")
    }

    fn convert_alias_id(&mut self, _sa: &Sema, id: AliasDefinitionId) -> AliasId {
        self.map_aliases
            .get(&id)
            .cloned()
            .expect("AliasDefinitionId not found in map")
    }

    fn convert_module_id(&mut self, _sa: &Sema, id: ModuleDefinitionId) -> ModuleId {
        self.map_modules
            .get(&id)
            .cloned()
            .expect("ModuleDefinitionId not found in map")
    }

    pub fn convert_function_id(&mut self, _sa: &Sema, id: FctDefinitionId) -> FunctionId {
        self.map_functions
            .get(&id)
            .cloned()
            .expect("FctDefinitionId not found in map")
    }

    pub fn convert_global_id(&mut self, _sa: &Sema, id: GlobalDefinitionId) -> GlobalId {
        self.map_globals
            .get(&id)
            .cloned()
            .expect("GlobalDefinitionId not found in map")
    }

    pub fn convert_const_id(&mut self, _sa: &Sema, id: ConstDefinitionId) -> ConstId {
        self.map_consts
            .get(&id)
            .cloned()
            .expect("ConstDefinitionId not found in map")
    }

    pub fn convert_class_id(&mut self, _sa: &Sema, id: ClassDefinitionId) -> ClassId {
        self.map_classes
            .get(&id)
            .cloned()
            .expect("ClassDefinitionId not found in map")
    }

    pub fn convert_struct_id(&mut self, _sa: &Sema, id: StructDefinitionId) -> StructId {
        self.map_structs
            .get(&id)
            .cloned()
            .expect("StructDefinitionId not found in map")
    }

    pub fn convert_enum_id(&mut self, _sa: &Sema, id: EnumDefinitionId) -> EnumId {
        self.map_enums
            .get(&id)
            .cloned()
            .expect("EnumDefinitionId not found in map")
    }

    fn convert_source_file_id(&mut self, _sa: &Sema, id: sema::SourceFileId) -> SourceFileId {
        self.map_source_files
            .get(&id)
            .cloned()
            .expect("SourceFileId not found in map")
    }

    fn convert_impl_id(&mut self, _sa: &Sema, id: ImplDefinitionId) -> ImplId {
        self.map_impls
            .get(&id)
            .cloned()
            .expect("ImplDefinitionId not found in map")
    }

    fn convert_extension_id(&mut self, _sa: &Sema, id: ExtensionDefinitionId) -> ExtensionId {
        self.map_extensions
            .get(&id)
            .cloned()
            .expect("ExtensionDefinitionId not found in map")
    }

    fn convert_trait_id(&mut self, _sa: &Sema, id: TraitDefinitionId) -> TraitId {
        self.map_traits
            .get(&id)
            .cloned()
            .expect("TraitDefinitionId not found in map")
    }
}
