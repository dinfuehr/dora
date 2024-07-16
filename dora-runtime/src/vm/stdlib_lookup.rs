use std::collections::HashMap;

use crate::boots::BOOTS_NATIVE_FUNCTIONS;
use crate::gc::Address;
use crate::stdlib::io::IO_NATIVE_FUNCTIONS;
use crate::stdlib::{
    STDLIB_NATIVE_FUNCTIONS, STDLIB_NATIVE_IMPL_METHODS, STDLIB_NATIVE_METHODS,
    STDLIB_NATIVE_PRIMITIVE_IMPL_METHODS,
};
use crate::vm::{display_fct, display_ty, BytecodeType, Intrinsic, VM};
use dora_bytecode::{
    ClassId, EnumId, ExtensionId, FunctionId, FunctionKind, ImplId, ModuleId, PackageId, Program,
    StructId, TraitId,
};

pub fn lookup(vm: &mut VM) {
    let module_items = compute_module_items(&vm.program);

    for (path, ptr) in STDLIB_NATIVE_FUNCTIONS {
        native_fct(vm, &module_items, path, *ptr);
    }

    for (path, method_name, ptr) in STDLIB_NATIVE_METHODS {
        native_method(vm, &module_items, path, method_name, *ptr);
    }

    for (trait_path, extended_ty, method_name, ptr) in STDLIB_NATIVE_PRIMITIVE_IMPL_METHODS {
        native_impl_method_ty(
            vm,
            &module_items,
            trait_path,
            extended_ty.clone(),
            method_name,
            *ptr,
        );
    }

    for (trait_path, extended_ty, method_name, ptr) in STDLIB_NATIVE_IMPL_METHODS {
        native_impl_method(
            vm,
            &module_items,
            trait_path,
            extended_ty,
            method_name,
            *ptr,
        );
    }

    for (path, ptr) in IO_NATIVE_FUNCTIONS {
        native_fct(vm, &module_items, path, *ptr);
    }

    if vm.program.boots_package_id.is_some() {
        for (path, ptr) in BOOTS_NATIVE_FUNCTIONS {
            native_fct(vm, &module_items, path, *ptr);
        }
    }

    lookup_known_classes(vm, &module_items);
    lookup_known_functions(vm, &module_items);

    for (fct_id, fct) in vm.program.functions.iter().enumerate() {
        let fct_id = FunctionId(fct_id as u32);

        if let Some(intrinsic) = fct.intrinsic {
            let intrinsic = Intrinsic::from_bytecode(intrinsic);
            let old = vm.intrinsics.insert(fct_id, intrinsic);
            assert!(old.is_none());
        }

        if fct.is_internal
            && vm.native_methods.get(fct_id).is_none()
            && vm.intrinsics.get(&fct_id).is_none()
        {
            panic!("unknown internal function {}", display_fct(vm, fct_id));
        }
    }
}

fn native_fct(vm: &mut VM, module_items: &ModuleItemMap, full_path: &str, ptr: *const u8) {
    let fct_id = resolve_path(vm, module_items, full_path)
        .function_id()
        .expect("function expected");

    let old = vm.native_methods.insert(fct_id, Address::from_ptr(ptr));

    if old.is_some() {
        panic!("function {} was already initialized", full_path);
    }

    assert!(old.is_none());
}

fn native_method(
    vm: &mut VM,
    module_items: &ModuleItemMap,
    full_path: &str,
    method_name: &str,
    ptr: *const u8,
) {
    let extended_ty = resolve_path(vm, module_items, full_path);
    let extension_id = lookup_extension_for_item(vm, extended_ty).expect("class not found");
    let fct_id =
        lookup_fct_by_extension_id_and_name(vm, extension_id, method_name).expect("missing method");

    let old = vm.native_methods.insert(fct_id, Address::from_ptr(ptr));

    if old.is_some() {
        panic!("function {} was already initialized", full_path);
    }

    assert!(old.is_none());
}

fn native_impl_method_ty(
    vm: &mut VM,
    module_items: &ModuleItemMap,
    trait_path: &str,
    extended_ty: BytecodeType,
    method_name: &str,
    ptr: *const u8,
) {
    let trait_id = resolve_path(vm, module_items, trait_path)
        .trait_id()
        .expect("trait expected");
    let impl_id = lookup_impl_for_ty(vm, trait_id, extended_ty.clone()).expect("missing impl");
    let fct_id = lookup_fct_by_impl_id_and_name(vm, impl_id, method_name).expect("missing method");

    let old = vm.native_methods.insert(fct_id, Address::from_ptr(ptr));

    if old.is_some() {
        panic!(
            "function {} in `impl {} for {}` was already initialized",
            method_name,
            trait_path,
            display_ty(vm, &extended_ty)
        );
    }

    assert!(old.is_none());
}

fn native_impl_method(
    vm: &mut VM,
    module_items: &ModuleItemMap,
    trait_path: &str,
    extended_ty_path: &str,
    method_name: &str,
    ptr: *const u8,
) {
    let trait_id = resolve_path(vm, module_items, trait_path)
        .trait_id()
        .expect("trait expected");
    let extended_ty = resolve_path(vm, module_items, extended_ty_path);
    let impl_id = lookup_impl_for_item(vm, trait_id, extended_ty).expect("missing impl");
    let fct_id = lookup_fct_by_impl_id_and_name(vm, impl_id, method_name).expect("missing method");

    let old = vm.native_methods.insert(fct_id, Address::from_ptr(ptr));

    if old.is_some() {
        panic!(
            "function {} in `impl {} for {:?}` was already initialized",
            method_name, trait_path, extended_ty
        );
    }

    assert!(old.is_none());
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

            _ => {}
        }
    }

    None
}

fn lookup_impl_for_ty(vm: &VM, trait_id: TraitId, extended_ty: BytecodeType) -> Option<ImplId> {
    for (id, impl_) in vm.program.impls.iter().enumerate() {
        match &impl_.trait_ty {
            BytecodeType::Trait(impl_trait_id, ..) if *impl_trait_id == trait_id => {}
            _ => continue,
        }

        if impl_.extended_ty == extended_ty {
            return Some(ImplId(id.try_into().expect("overflow")));
        }
    }

    None
}

fn lookup_impl_for_item(vm: &VM, trait_id: TraitId, extended_ty: ModuleItem) -> Option<ImplId> {
    for (id, impl_) in vm.program.impls.iter().enumerate() {
        match &impl_.trait_ty {
            BytecodeType::Trait(impl_trait_id, ..) if *impl_trait_id == trait_id => {}
            _ => continue,
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

fn resolve_path(vm: &VM, module_items: &ModuleItemMap, path: &str) -> ModuleItem {
    let mut components = path.split("::");

    let package_name = components.next().expect("missing package name");
    let package_id = match lookup_package(vm, package_name) {
        Some(package_id) => package_id,
        None => {
            panic!("unknown package {} in path {}", package_name, path);
        }
    };

    let package = &vm.program.packages[package_id.0 as usize];
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

fn lookup_package(vm: &VM, name: &str) -> Option<PackageId> {
    for (id, package) in vm.program.packages.iter().enumerate() {
        if package.name == name {
            return Some(PackageId(id.try_into().expect("overflow")));
        }
    }

    None
}

fn lookup_known_classes(vm: &mut VM, module_items: &ModuleItemMap) {
    vm.known.array_class_id = Some(
        resolve_path(vm, module_items, "stdlib::collections::Array")
            .class_id()
            .expect("class expected"),
    );
    vm.known.string_class_id = Some(
        resolve_path(vm, module_items, "stdlib::string::String")
            .class_id()
            .expect("class expected"),
    );
    vm.known.thread_class_id = Some(
        resolve_path(vm, module_items, "stdlib::thread::Thread")
            .class_id()
            .expect("class expected"),
    );
}

fn lookup_known_functions(vm: &mut VM, module_items: &ModuleItemMap) {
    if vm.program.boots_package_id.is_some() {
        vm.known.boots_compile_fct_id = Some(
            resolve_path(vm, module_items, "boots::interface::compile")
                .function_id()
                .expect("function expected"),
        );
    }

    vm.known.unreachable_fct_id = Some(
        resolve_path(vm, module_items, "stdlib::unreachable")
            .function_id()
            .expect("function expected"),
    );
}

fn compute_module_items(program: &Program) -> ModuleItemMap {
    let mut map = ModuleItemMap::new();

    for (id, module) in program.modules.iter().enumerate() {
        let id = ModuleId(id.try_into().expect("overflow"));
        if let Some(parent_id) = module.parent_id {
            map.insert(parent_id, &module.name, ModuleItem::Module(id));
        }
    }

    for (id, trait_) in program.traits.iter().enumerate() {
        let id = TraitId(id.try_into().expect("overflow"));
        map.insert(trait_.module_id, &trait_.name, ModuleItem::Trait(id));
    }

    for (id, class) in program.classes.iter().enumerate() {
        let id = ClassId(id.try_into().expect("overflow"));
        map.insert(class.module_id, &class.name, ModuleItem::Class(id));
    }

    for (id, struct_) in program.structs.iter().enumerate() {
        let id = StructId(id.try_into().expect("overflow"));
        map.insert(struct_.module_id, &struct_.name, ModuleItem::Struct(id));
    }

    for (id, enum_) in program.enums.iter().enumerate() {
        let id = EnumId(id.try_into().expect("overflow"));
        map.insert(enum_.module_id, &enum_.name, ModuleItem::Enum(id));
    }

    for (id, function) in program.functions.iter().enumerate() {
        match function.kind {
            FunctionKind::Function => {
                let id = FunctionId(id.try_into().expect("overflow"));
                map.insert(function.module_id, &function.name, ModuleItem::Function(id));
            }

            _ => {}
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

#[derive(PartialEq, Debug, Copy, Clone)]
enum ModuleItem {
    Class(ClassId),
    Struct(StructId),
    Enum(EnumId),
    Trait(TraitId),
    Module(ModuleId),
    Function(FunctionId),
}

impl ModuleItem {
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
