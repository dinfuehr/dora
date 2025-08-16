use std::collections::HashMap;

use crate::boots::BOOTS_FUNCTIONS;
use crate::gc::Address;
use crate::stdlib::STDLIB_FUNCTIONS;
use crate::stdlib::io::IO_FUNCTIONS;
use crate::vm::{BytecodeType, Intrinsic, VM};
use dora_bytecode::{
    ClassId, ExtensionId, FunctionId, FunctionKind, ImplId, ModuleId, ModuleItem, PackageId,
    Program, StructId, TraitId, display_fct,
};

#[derive(Clone)]
pub enum FctImplementation {
    Intrinsic(Intrinsic),
    Native(*const u8),
}

pub fn lookup(vm: &mut VM) {
    let module_items = compute_module_items(&vm.program);

    for (path, implementation) in STDLIB_FUNCTIONS {
        apply_fct(vm, &module_items, path, implementation.clone());
    }

    for (path, implementation) in IO_FUNCTIONS {
        apply_fct(vm, &module_items, path, implementation.clone());
    }

    if vm.has_boots() {
        for (path, implementation) in BOOTS_FUNCTIONS {
            apply_fct(vm, &module_items, path, implementation.clone());
        }
    }

    lookup_known_classes(vm, &module_items);
    lookup_known_functions(vm, &module_items);
    lookup_known_traits(vm, &module_items);

    for (fct_id, fct) in vm.program.functions.iter().enumerate() {
        let fct_id = FunctionId(fct_id as u32);

        if fct.is_internal
            && vm.native_methods.get(fct_id).is_none()
            && vm.intrinsics.get(&fct_id).is_none()
        {
            panic!(
                "unknown internal function {}",
                display_fct(&vm.program, fct_id)
            );
        }
    }
}

fn get_primitive_ty(path: &str) -> Option<BytecodeType> {
    match path {
        "std::primitives::Bool" => Some(BytecodeType::Bool),
        "std::primitives::UInt8" => Some(BytecodeType::UInt8),
        "std::primitives::Char" => Some(BytecodeType::Char),
        "std::primitives::Int32" => Some(BytecodeType::Int32),
        "std::primitives::Int64" => Some(BytecodeType::Int64),
        "std::primitives::Float32" => Some(BytecodeType::Float32),
        "std::primitives::Float64" => Some(BytecodeType::Float64),
        _ => None,
    }
}

fn apply_fct(
    vm: &mut VM,
    module_items: &ModuleItemMap,
    path: &str,
    implementation: FctImplementation,
) {
    let fct_id = if path.contains("#") {
        let parts = path.split("#").collect::<Vec<_>>();
        assert_eq!(parts.len(), 2);
        let path = parts[0];
        let method_name = parts[1];

        if path.contains(" for ") {
            let parts = path.split(" for ").collect::<Vec<_>>();
            assert_eq!(parts.len(), 2);

            let trait_path = parts[0];
            let extended_ty_path = parts[1];

            let trait_id = resolve_path(&vm.program, module_items, trait_path)
                .trait_id()
                .expect("trait expected");

            let impl_id = if let Some(extended_ty) = get_primitive_ty(extended_ty_path) {
                lookup_impl_for_ty(vm, trait_id, extended_ty.clone()).expect("missing impl")
            } else {
                let extended_ty = resolve_path(&vm.program, module_items, extended_ty_path);
                lookup_impl_for_item(vm, trait_id, extended_ty).expect("missing impl")
            };

            lookup_fct_by_impl_id_and_name(vm, impl_id, method_name).expect("missing method")
        } else {
            let extension_id = if let Some(extended_ty) = get_primitive_ty(path) {
                lookup_extension_for_ty(vm, extended_ty)
            } else {
                let extended_ty = resolve_path(&vm.program, module_items, path);
                lookup_extension_for_item(vm, extended_ty)
            }
            .expect("impl block not found");
            lookup_fct_by_extension_id_and_name(vm, extension_id, method_name)
                .expect("missing method")
        }
    } else {
        resolve_path(&vm.program, &module_items, path)
            .function_id()
            .expect("function expected")
    };

    let existed = match implementation {
        FctImplementation::Intrinsic(intrinsic) => {
            vm.intrinsics.insert(fct_id, intrinsic).is_some()
        }
        FctImplementation::Native(ptr) => vm
            .native_methods
            .insert(fct_id, Address::from_ptr(ptr))
            .is_some(),
    };

    if existed {
        panic!(
            "function {} was already initialized",
            display_fct(&vm.program, fct_id)
        );
    }
}

fn lookup_fct_by_extension_id_and_name(
    vm: &VM,
    extension_id: ExtensionId,
    name: &str,
) -> Option<FunctionId> {
    for (id, fct) in vm.program.functions.iter().enumerate() {
        match fct.kind {
            FunctionKind::Extension(ext_id) if ext_id == extension_id && fct.name == name => {
                return Some(FunctionId(id.try_into().expect("overflow")));
            }

            _ => {}
        }
    }
    None
}

fn lookup_fct_by_impl_id_and_name(vm: &VM, impl_id: ImplId, name: &str) -> Option<FunctionId> {
    for (id, fct) in vm.program.functions.iter().enumerate() {
        match fct.kind {
            FunctionKind::Impl(fct_impl_id) if fct_impl_id == impl_id && fct.name == name => {
                return Some(FunctionId(id.try_into().expect("overflow")));
            }

            _ => {}
        }
    }
    None
}

fn lookup_extension_for_item(vm: &VM, extended_ty: ModuleItem) -> Option<ExtensionId> {
    for (id, extension) in vm.program.extensions.iter().enumerate() {
        match &extension.extended_ty {
            BytecodeType::Class(ext_class_id, ..)
                if extended_ty == ModuleItem::Class(*ext_class_id) =>
            {
                return Some(ExtensionId(id.try_into().expect("overflow")));
            }

            BytecodeType::Enum(ext_enum_id, ..)
                if extended_ty == ModuleItem::Enum(*ext_enum_id) =>
            {
                return Some(ExtensionId(id.try_into().expect("overflow")));
            }

            _ => {}
        }
    }

    None
}

fn lookup_extension_for_ty(vm: &VM, extended_ty: BytecodeType) -> Option<ExtensionId> {
    for (id, extension) in vm.program.extensions.iter().enumerate() {
        if extension.extended_ty == extended_ty {
            return Some(ExtensionId(id.try_into().expect("overflow")));
        }
    }

    None
}

fn lookup_impl_for_ty(vm: &VM, trait_id: TraitId, extended_ty: BytecodeType) -> Option<ImplId> {
    for (id, impl_) in vm.program.impls.iter().enumerate() {
        if impl_.trait_ty.trait_id != trait_id {
            continue;
        }

        if impl_.extended_ty == extended_ty {
            return Some(ImplId(id.try_into().expect("overflow")));
        }
    }

    None
}

fn lookup_impl_for_item(vm: &VM, trait_id: TraitId, extended_ty: ModuleItem) -> Option<ImplId> {
    for (id, impl_) in vm.program.impls.iter().enumerate() {
        if impl_.trait_ty.trait_id != trait_id {
            continue;
        }

        match impl_.extended_ty {
            BytecodeType::Class(class_id, ..) if extended_ty == ModuleItem::Class(class_id) => {
                return Some(ImplId(id.try_into().expect("overflow")));
            }

            _ => {}
        }
    }

    None
}

fn resolve_path(program: &Program, module_items: &ModuleItemMap, path: &str) -> ModuleItem {
    let mut components = path.split("::");

    let package_name = components.next().expect("missing package name");
    let package_id = match lookup_package(program, package_name) {
        Some(package_id) => package_id,
        None => {
            panic!("unknown package {} in path {}", package_name, path);
        }
    };

    let package = &program.packages[package_id.0 as usize];
    let mut item = ModuleItem::Module(package.root_module_id);

    while let Some(component) = components.next() {
        let module_id = item.module_id().expect("module expected");
        item = match module_items.get(module_id, component) {
            Some(item) => item,
            None => {
                panic!("unknown module {} in path {}", component, path);
            }
        };
    }

    item
}

fn lookup_package(program: &Program, name: &str) -> Option<PackageId> {
    for (id, package) in program.packages.iter().enumerate() {
        if package.name == name {
            return Some(PackageId(id.try_into().expect("overflow")));
        }
    }

    None
}

fn lookup_known_classes(vm: &mut VM, module_items: &ModuleItemMap) {
    vm.known.array_class_id = Some(
        resolve_path(&vm.program, module_items, "std::collections::Array")
            .class_id()
            .expect("class expected"),
    );
    vm.known.string_class_id = Some(
        resolve_path(&vm.program, module_items, "std::string::String")
            .class_id()
            .expect("class expected"),
    );
    vm.known.thread_class_id = Some(
        resolve_path(&vm.program, module_items, "std::thread::Thread")
            .class_id()
            .expect("class expected"),
    );
}

fn lookup_known_functions(vm: &mut VM, module_items: &ModuleItemMap) {
    if vm.has_boots() {
        vm.known.boots_compile_fct_id = Some(
            resolve_path(&vm.program, module_items, "boots::interface::compile")
                .function_id()
                .expect("function expected"),
        );
    }

    vm.known.unreachable_fct_id = Some(
        resolve_path(&vm.program, module_items, "std::unreachable")
            .function_id()
            .expect("function expected"),
    );

    vm.known.fatal_error_fct_id = Some(
        resolve_path(&vm.program, module_items, "std::fatalError")
            .function_id()
            .expect("function expected"),
    );
}

fn lookup_known_traits(vm: &mut VM, module_items: &ModuleItemMap) {
    vm.known.zero_trait_id = Some(
        resolve_path(&vm.program, module_items, "std::traits::Zero")
            .trait_id()
            .expect("trait expected"),
    );
}

fn compute_module_items(program: &Program) -> ModuleItemMap {
    let mut map = ModuleItemMap::new();

    for (id, module) in program.modules.iter().enumerate() {
        let id = ModuleId(id.try_into().expect("overflow"));

        for (name, item) in &module.items {
            map.insert(id, name, item.clone());
        }
    }

    map
}

struct ModuleItemMap {
    data: HashMap<ModuleId, HashMap<String, ModuleItem>>,
}

impl ModuleItemMap {
    fn new() -> ModuleItemMap {
        ModuleItemMap {
            data: HashMap::new(),
        }
    }

    fn insert(&mut self, id: ModuleId, name: &str, item: ModuleItem) {
        let entry = self.data.entry(id).or_default();
        assert!(entry.insert(name.to_string(), item).is_none());
    }

    fn get(&self, id: ModuleId, name: &str) -> Option<ModuleItem> {
        self.data.get(&id).and_then(|m| m.get(name)).cloned()
    }
}

trait ModuleItemExt {
    fn module_id(&self) -> Option<ModuleId>;
    fn function_id(&self) -> Option<FunctionId>;
    fn class_id(&self) -> Option<ClassId>;
    fn struct_id(&self) -> Option<StructId>;
    fn trait_id(&self) -> Option<TraitId>;
}

impl ModuleItemExt for ModuleItem {
    fn module_id(&self) -> Option<ModuleId> {
        match self {
            ModuleItem::Module(id) => Some(*id),
            _ => None,
        }
    }

    fn function_id(&self) -> Option<FunctionId> {
        match self {
            ModuleItem::Function(id) => Some(*id),
            _ => None,
        }
    }

    fn class_id(&self) -> Option<ClassId> {
        match self {
            ModuleItem::Class(id) => Some(*id),
            _ => None,
        }
    }

    fn struct_id(&self) -> Option<StructId> {
        match self {
            ModuleItem::Struct(id) => Some(*id),
            _ => None,
        }
    }

    fn trait_id(&self) -> Option<TraitId> {
        match self {
            ModuleItem::Trait(id) => Some(*id),
            _ => None,
        }
    }
}
