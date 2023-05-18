use std::collections::HashMap;

use crate::Sema;
use dora_bytecode::program::{ClassLayout, ImplData, InternalClass, InternalFunction};
use dora_bytecode::{
    BytecodeType, ClassData, ClassField, EnumData, EnumVariant, FunctionData, FunctionId,
    FunctionKind, GlobalData, ImplId, ModuleData, ModuleId, PackageData, PackageId, Program,
    SourceFileData, SourceFileId, StructData, StructField, TraitData, TraitId, TypeParamBound,
    TypeParamData,
};

use crate::generator::bty_from_ty;

use crate::sema::{self as sa, GlobalDefinition, GlobalDefinitionId};
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
        impls: create_impls(&sa),
        source_files: create_source_files(&sa),
        stdlib_package_id: convert_package_id(sa.stdlib_package_id()),
        program_package_id: convert_package_id(sa.program_package_id()),
        boots_package_id: sa.boots_package_id.map(|p| convert_package_id(p)),
        main_fct_id: find_main_fct_id(&sa),
    }
}

fn create_packages(sa: &Sema) -> Vec<PackageData> {
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

fn create_modules(sa: &Sema) -> Vec<ModuleData> {
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

fn create_impls(sa: &Sema) -> Vec<ImplData> {
    let mut result = Vec::new();

    for impl_ in sa.impls.iter() {
        let impl_ = impl_.read();

        let mut methods = Vec::new();

        let trait_id = impl_.trait_id();
        let trait_ = sa.traits.idx(trait_id);
        let trait_ = trait_.read();

        // The methods array for impl should have the exact same order as for the trait.
        for method_id in &trait_.methods {
            let target_method_id = *impl_
                .impl_for
                .get(&method_id)
                .expect("missing impl for trait methdo");

            methods.push(convert_function_id(target_method_id));
        }

        result.push(ImplData {
            module_id: convert_module_id(impl_.module_id),
            type_params: create_type_params(sa, impl_.type_params()),
            trait_ty: bty_from_ty(impl_.trait_ty.clone()),
            extended_ty: bty_from_ty(impl_.extended_ty.clone()),
            methods,
        });
    }

    result
}

fn create_functions(sa: &Sema, e: &mut Emitter) -> Vec<FunctionData> {
    let mut result = Vec::new();

    for fct in sa.fcts.iter() {
        let fct = fct.read();
        let name = sa.interner.str(fct.name).to_string();

        let internal_function = if Some(fct.id()) == sa.known.functions.compile {
            Some(InternalFunction::BootsCompile)
        } else if fct.id() == sa.known.functions.stacktrace_retrieve() {
            Some(InternalFunction::StacktraceRetrieve)
        } else {
            None
        };

        let kind = match fct.parent {
            FctParent::Extension(..) => FunctionKind::Method,
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
            type_params: create_type_params(sa, &fct.type_params),
            source_file_id: Some(convert_source_file_id(fct.file_id)),
            params: fct
                .params_with_self()
                .iter()
                .map(|ty| bty_from_ty(ty.clone()))
                .collect(),
            return_type: fct.return_type_bty(),
            native: fct.native_function.clone(),
            intrinsic: fct.intrinsic,
            internal: internal_function,
            is_test: fct.is_test,
            vtable_index: fct.vtable_index,
            is_optimize_immediately: fct.is_optimize_immediately,
            is_variadic: fct.is_variadic,
            bytecode: fct.bytecode.clone(),
        })
    }

    for global in sa.globals.iter() {
        let global = global.read();

        if global.ast.initial_value.is_none() {
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
            return_type: BytecodeType::Unit,
            native: None,
            intrinsic: None,
            internal: None,
            is_test: false,
            vtable_index: None,
            is_optimize_immediately: false,
            is_variadic: false,
            bytecode: Some(global.bytecode.as_ref().expect("missing bytecode").clone()),
        });

        e.global_initializer.insert(global.id(), fct_id);
    }

    result
}

fn create_globals(sa: &Sema, e: &Emitter) -> Vec<GlobalData> {
    let mut result = Vec::new();

    for global in sa.globals.iter() {
        let global = global.read();
        let name = sa.interner.str(global.name).to_string();

        result.push(GlobalData {
            module_id: convert_module_id(global.module_id),
            ty: bty_from_ty(global.ty.clone()),
            mutable: global.mutable,
            name,
            initializer: global_initializer_function_id(sa, &*global, e),
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

    for class in sa.classes.iter() {
        let class = class.read();
        let name = sa.interner.str(class.name).to_string();

        let internal_class = if class.is_array {
            Some(InternalClass::Array)
        } else if class.is_str {
            Some(InternalClass::String)
        } else if class.id() == sa.known.classes.thread() {
            Some(InternalClass::Thread)
        } else if class.id() == sa.known.classes.stacktrace_element() {
            Some(InternalClass::StacktraceElement)
        } else {
            None
        };

        result.push(ClassData {
            module_id: convert_module_id(class.module_id),
            name,
            type_params: create_type_params(sa, class.type_params()),
            layout: create_class_layout(&*class),
            fields: create_class_fields(sa, &*class),
            internal: internal_class,
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

fn create_class_fields(sa: &Sema, class: &ClassDefinition) -> Vec<ClassField> {
    class
        .fields
        .iter()
        .map(|f| ClassField {
            ty: bty_from_ty(f.ty.clone()),
            name: sa.interner.str(f.name).to_string(),
        })
        .collect()
}

fn create_structs(sa: &Sema) -> Vec<StructData> {
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
            ty: bty_from_ty(f.ty.clone()),
            name: sa.interner.str(f.name).to_string(),
        })
        .collect()
}

fn create_enums(sa: &Sema) -> Vec<EnumData> {
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

fn create_enum_variants(sa: &Sema, enum_: &sa::EnumDefinition) -> Vec<EnumVariant> {
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

fn create_traits(sa: &Sema) -> Vec<TraitData> {
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

fn create_source_files(sa: &Sema) -> Vec<SourceFileData> {
    let mut result = Vec::new();

    for file in sa.source_files.iter() {
        result.push(SourceFileData {
            path: file.path.to_string_lossy().to_string(),
        })
    }

    result
}

fn find_main_fct_id(sa: &Sema) -> Option<FunctionId> {
    let name = sa.interner.intern("main");
    let fctid = if let Some(id) = sa.module_table(sa.program_module_id()).read().get_fct(name) {
        id
    } else {
        return None;
    };

    let fct = sa.fcts.idx(fctid);
    let fct = fct.read();
    let ret = fct.return_type.clone();

    if (!ret.is_unit() && !ret.is_int32())
        || !fct.params_without_self().is_empty()
        || !fct.type_params.is_empty()
    {
        None
    } else {
        Some(convert_function_id(fct.id()))
    }
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

fn convert_impl_id(id: ImplDefinitionId) -> ImplId {
    ImplId(id.to_usize().try_into().expect("failure"))
}

fn convert_trait_id(id: TraitDefinitionId) -> TraitId {
    TraitId(id.to_usize().try_into().expect("failure"))
}
