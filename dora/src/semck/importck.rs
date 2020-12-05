use parking_lot::RwLock;

use crate::error::msg::SemError;
use crate::semck::report_type_shadow;
use crate::sym::{NestedSymTable, SymTable, TypeSym};
use crate::vm::{
    class_accessible_from, const_accessible_from, enum_accessible_from, fct_accessible_from,
    global_accessible_from, module_accessible_from, namespace_accessible_from, namespace_package,
    struct_accessible_from, trait_accessible_from, EnumId, ImportData, NamespaceId, VM,
};

use dora_parser::ast::ImportContext;
use dora_parser::interner::Name;

pub fn check<'a>(vm: &VM) {
    for import in &vm.imports {
        check_import(vm, import);
    }
}

fn check_import(vm: &VM, import: &ImportData) {
    let table = vm.namespace_table(import.namespace_id);

    let namespace_id = match import.ast.context {
        ImportContext::This => import.namespace_id,
        ImportContext::Package => namespace_package(vm, import.namespace_id),
        ImportContext::Super => {
            let namespace = &vm.namespaces[import.namespace_id.to_usize()];
            if let Some(namespace_id) = namespace.parent_namespace_id {
                namespace_id
            } else {
                vm.diag.lock().report(
                    import.file_id.into(),
                    import.ast.pos,
                    SemError::NoSuperNamespace,
                );
                return;
            }
        }
    };

    let symtable = NestedSymTable::new(vm, namespace_id);

    let element_name = import.ast.element_name;
    let target_name = import.ast.target_name.unwrap_or(element_name);

    if import.ast.path.is_empty() {
        import_namespace(vm, import, &table, namespace_id, element_name, target_name);
    } else {
        let sym_type = match read_path(vm, import, &symtable) {
            Ok(sym_type) => sym_type,
            Err(()) => {
                return;
            }
        };

        match sym_type {
            Some(TypeSym::Namespace(namespace_id)) => {
                if !namespace_accessible_from(vm, namespace_id, import.namespace_id) {
                    let namespace = &vm.namespaces[namespace_id.to_usize()];
                    let msg = SemError::NotAccessible(namespace.name(vm));
                    vm.diag.lock().report(import.file_id, import.ast.pos, msg);
                    return;
                }

                import_namespace(vm, import, &table, namespace_id, element_name, target_name)
            }

            Some(TypeSym::Enum(enum_id)) => {
                import_enum(vm, import, &table, enum_id, element_name, target_name)
            }

            _ => {
                vm.diag.lock().report(
                    import.file_id.into(),
                    import.ast.pos,
                    SemError::ExpectedPath,
                );
            }
        }
    }
}

fn read_path(
    vm: &VM,
    import: &ImportData,
    symtable: &NestedSymTable,
) -> Result<Option<TypeSym>, ()> {
    if !import.ast.path.is_empty() {
        let path = &import.ast.path;
        let first_name = path.first().cloned().unwrap();

        let mut sym_type = symtable.get(first_name);

        for &name in &path[1..] {
            match sym_type {
                Some(TypeSym::Namespace(namespace_id)) => {
                    let namespace = &vm.namespaces[namespace_id.to_usize()];
                    let symtable = namespace.table.read();

                    if !namespace_accessible_from(vm, namespace_id, import.namespace_id) {
                        let namespace = &vm.namespaces[namespace_id.to_usize()];
                        let msg = SemError::NotAccessible(namespace.name(vm));
                        vm.diag.lock().report(import.file_id, import.ast.pos, msg);
                        return Err(());
                    }

                    sym_type = symtable.get(name);
                }

                None => {
                    let msg = SemError::ExpectedPath;
                    vm.diag.lock().report(import.file_id, import.ast.pos, msg);
                    return Err(());
                }

                _ => {
                    let msg = SemError::ExpectedNamespace;
                    vm.diag.lock().report(import.file_id, import.ast.pos, msg);
                    return Err(());
                }
            }
        }

        Ok(sym_type)
    } else {
        let msg = SemError::ExpectedPath;
        vm.diag.lock().report(import.file_id, import.ast.pos, msg);
        Err(())
    }
}

fn import_namespace(
    vm: &VM,
    import: &ImportData,
    table: &RwLock<SymTable>,
    namespace_id: NamespaceId,
    element_name: Name,
    target_name: Name,
) {
    let namespace = &vm.namespaces[namespace_id.to_usize()];
    let sym_type = namespace.table.read().get(element_name);

    match sym_type {
        Some(TypeSym::Fct(fct_id)) => {
            if !fct_accessible_from(vm, fct_id, import.namespace_id) {
                let fct = &vm.fcts.idx(fct_id);
                let fct = fct.read();
                let msg = SemError::NotAccessible(fct.name(vm));
                vm.diag.lock().report(import.file_id, import.ast.pos, msg);
            }

            let new_sym = TypeSym::Fct(fct_id);
            if let Some(old_sym) = table.write().insert(target_name, new_sym) {
                report_type_shadow(vm, target_name, import.file_id, import.ast.pos, old_sym);
            }
        }

        Some(TypeSym::Namespace(namespace_id)) => {
            if !namespace_accessible_from(vm, namespace_id, import.namespace_id) {
                let namespace = &vm.namespaces[namespace_id.to_usize()];
                let msg = SemError::NotAccessible(namespace.name(vm));
                vm.diag.lock().report(import.file_id, import.ast.pos, msg);
            }

            let new_sym = TypeSym::Namespace(namespace_id);
            if let Some(old_sym) = table.write().insert(target_name, new_sym) {
                report_type_shadow(vm, target_name, import.file_id, import.ast.pos, old_sym);
            }
        }

        Some(TypeSym::Global(global_id)) => {
            if !global_accessible_from(vm, global_id, import.namespace_id) {
                let global = &vm.globals.idx(global_id);
                let global = global.read();
                let msg = SemError::NotAccessible(global.name(vm));
                vm.diag.lock().report(import.file_id, import.ast.pos, msg);
            }

            let new_sym = TypeSym::Global(global_id);
            if let Some(old_sym) = table.write().insert(target_name, new_sym) {
                report_type_shadow(vm, target_name, import.file_id, import.ast.pos, old_sym);
            }
        }

        Some(TypeSym::Const(const_id)) => {
            if !const_accessible_from(vm, const_id, import.namespace_id) {
                let xconst = &vm.consts.idx(const_id);
                let xconst = xconst.read();
                let msg = SemError::NotAccessible(xconst.name(vm));
                vm.diag.lock().report(import.file_id, import.ast.pos, msg);
            }

            let new_sym = TypeSym::Const(const_id);
            if let Some(old_sym) = table.write().insert(target_name, new_sym) {
                report_type_shadow(vm, target_name, import.file_id, import.ast.pos, old_sym);
            }
        }

        Some(TypeSym::Class(cls_id)) => {
            if !class_accessible_from(vm, cls_id, import.namespace_id) {
                let cls = &vm.classes.idx(cls_id);
                let cls = cls.read();
                let msg = SemError::NotAccessible(cls.name(vm));
                vm.diag.lock().report(import.file_id, import.ast.pos, msg);
            }

            let new_sym = TypeSym::Class(cls_id);
            let old_sym = table.write().insert(target_name, new_sym);
            if let Some(old_sym) = old_sym {
                report_type_shadow(vm, target_name, import.file_id, import.ast.pos, old_sym);
            }
        }

        Some(TypeSym::Enum(enum_id)) => {
            if !enum_accessible_from(vm, enum_id, import.namespace_id) {
                let xenum = vm.enums[enum_id].read();
                let msg = SemError::NotAccessible(xenum.name(vm));
                vm.diag.lock().report(import.file_id, import.ast.pos, msg);
            }

            let new_sym = TypeSym::Enum(enum_id);
            if let Some(old_sym) = table.write().insert(target_name, new_sym) {
                report_type_shadow(vm, target_name, import.file_id, import.ast.pos, old_sym);
            }
        }

        Some(TypeSym::Struct(struct_id)) => {
            if !struct_accessible_from(vm, struct_id, import.namespace_id) {
                let xstruct = vm.structs.idx(struct_id);
                let xstruct = xstruct.read();
                let msg = SemError::NotAccessible(xstruct.name(vm));
                vm.diag.lock().report(import.file_id, import.ast.pos, msg);
            }

            let new_sym = TypeSym::Struct(struct_id);
            let old_sym = table.write().insert(target_name, new_sym);
            if let Some(old_sym) = old_sym {
                report_type_shadow(vm, target_name, import.file_id, import.ast.pos, old_sym);
            }
        }

        Some(TypeSym::Module(module_id)) => {
            if !module_accessible_from(vm, module_id, import.namespace_id) {
                let module = vm.modules.idx(module_id);
                let module = module.read();
                let msg = SemError::NotAccessible(module.name(vm));
                vm.diag.lock().report(import.file_id, import.ast.pos, msg);
            }

            let new_sym = TypeSym::Module(module_id);
            if let Some(old_sym) = table.write().insert(target_name, new_sym) {
                report_type_shadow(vm, target_name, import.file_id, import.ast.pos, old_sym);
            }
        }

        Some(TypeSym::Trait(trait_id)) => {
            if !trait_accessible_from(vm, trait_id, import.namespace_id) {
                let xtrait = vm.traits[trait_id].read();
                let msg = SemError::NotAccessible(xtrait.name(vm));
                vm.diag.lock().report(import.file_id, import.ast.pos, msg);
            }

            let new_sym = TypeSym::Trait(trait_id);
            let old_sym = table.write().insert(target_name, new_sym);
            if let Some(old_sym) = old_sym {
                report_type_shadow(vm, target_name, import.file_id, import.ast.pos, old_sym);
            }
        }

        None => {
            let name = vm.interner.str(element_name).to_string();
            let namespace_name = namespace.name(vm);
            vm.diag.lock().report(
                import.file_id.into(),
                import.ast.pos,
                SemError::UnknownIdentifierInNamespace(namespace_name, name),
            );
        }

        _ => {
            vm.diag.lock().report(
                import.file_id.into(),
                import.ast.pos,
                SemError::ExpectedPath,
            );
        }
    }
}

fn import_enum(
    vm: &VM,
    import: &ImportData,
    table: &RwLock<SymTable>,
    enum_id: EnumId,
    element_name: Name,
    target_name: Name,
) {
    let xenum = vm.enums[enum_id].read();

    if !enum_accessible_from(vm, enum_id, import.namespace_id) {
        let msg = SemError::NotAccessible(xenum.name(vm));
        vm.diag.lock().report(import.file_id, import.ast.pos, msg);
    }

    if let Some(&variant_id) = xenum.name_to_value.get(&element_name) {
        let sym = TypeSym::EnumValue(enum_id, variant_id as usize);
        if let Some(sym) = table.write().insert(target_name, sym) {
            report_type_shadow(vm, target_name, import.file_id.into(), import.ast.pos, sym);
        }
    } else {
        let name = vm.interner.str(element_name).to_string();
        vm.diag.lock().report(
            import.file_id.into(),
            import.ast.pos,
            SemError::UnknownEnumValue(name),
        );
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::SemError;
    use crate::semck::tests::*;

    #[test]
    fn check_initializer() {
        ok("let a: Int32 = 0;");
        ok("let a: Int32 = 0; var b: Int32 = a + 1;");
        err(
            "var a: Int32 = foo;",
            pos(1, 16),
            SemError::UnknownIdentifier("foo".into()),
        );
    }

    #[test]
    fn check_type() {
        err(
            "var x: Foo = 0;",
            pos(1, 8),
            SemError::UnknownIdentifier("Foo".into()),
        );
    }

    #[test]
    fn import_namespace() {
        err(
            "
            import foo::bar::Foo;
            namespace foo {
                namespace bar {
                    class Foo
                }
            }
        ",
            pos(2, 13),
            SemError::NotAccessible("foo::bar".into()),
        );
    }

    #[test]
    fn import_class() {
        err(
            "
            import foo::bar::Foo;
            namespace foo {
                @pub namespace bar {
                    class Foo
                }
            }
        ",
            pos(2, 13),
            SemError::NotAccessible("foo::bar::Foo".into()),
        );
    }

    #[test]
    fn import_fct() {
        err(
            "
            import foo::bar;
            namespace foo {
                fun bar() {}
            }
        ",
            pos(2, 13),
            SemError::NotAccessible("foo::bar".into()),
        );
    }

    #[test]
    fn import_global() {
        err(
            "
            import foo::bar;
            namespace foo {
                var bar: Int32 = 12;
            }
        ",
            pos(2, 13),
            SemError::NotAccessible("foo::bar".into()),
        );
    }

    #[test]
    fn import_const() {
        err(
            "
            import foo::bar;
            namespace foo {
                const bar: Int32 = 12;
            }
        ",
            pos(2, 13),
            SemError::NotAccessible("foo::bar".into()),
        );
    }

    #[test]
    fn import_enum() {
        err(
            "
            import foo::Bar;
            namespace foo {
                enum Bar { A, B, C }
            }
        ",
            pos(2, 13),
            SemError::NotAccessible("foo::Bar".into()),
        );
    }

    #[test]
    fn import_enum_value() {
        err(
            "
            import foo::Bar::A;
            namespace foo {
                enum Bar { A, B, C }
            }
        ",
            pos(2, 13),
            SemError::NotAccessible("foo::Bar".into()),
        );
    }

    #[test]
    fn import_trait() {
        err(
            "
            import foo::Bar;
            namespace foo {
                trait Bar {}
            }
        ",
            pos(2, 13),
            SemError::NotAccessible("foo::Bar".into()),
        );
    }

    #[test]
    fn import_struct() {
        ok("
            import foo::Bar;
            namespace foo {
                @pub struct Bar { f: Int32 }
            }
        ");

        err(
            "
            import foo::Bar;
            namespace foo {
                struct Bar { f: Int32 }
            }
        ",
            pos(2, 13),
            SemError::NotAccessible("foo::Bar".into()),
        );
    }
}
