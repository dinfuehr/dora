use parking_lot::RwLock;

use crate::error::msg::SemError;
use crate::semck::{report_term_shadow, report_type_shadow};
use crate::sym::{NestedSymTable, SymTable, TermSym, TypeSym};
use crate::vm::{
    class_accessible_from, fct_accessible_from, global_accessible_from, namespace_accessible_from,
    namespace_package, EnumId, ImportData, NamespaceId, VM,
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
        let (sym_term, sym_type) = match read_path(vm, import, &symtable) {
            Ok((sym_term, sym_type)) => (sym_term, sym_type),
            Err(()) => {
                return;
            }
        };

        match (sym_term.clone(), sym_type.clone()) {
            (Some(TermSym::Namespace(namespace_id)), _) => {
                if !namespace_accessible_from(vm, namespace_id, import.namespace_id) {
                    let namespace = &vm.namespaces[namespace_id.to_usize()];
                    let msg = SemError::NotAccessible(namespace.name(vm));
                    vm.diag.lock().report(import.file_id, import.ast.pos, msg);
                    return;
                }

                import_namespace(vm, import, &table, namespace_id, element_name, target_name)
            }

            (_, Some(TypeSym::Enum(enum_id))) => {
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
) -> Result<(Option<TermSym>, Option<TypeSym>), ()> {
    if !import.ast.path.is_empty() {
        let path = &import.ast.path;
        let first_name = path.first().cloned().unwrap();

        let mut sym_term = symtable.get_term(first_name);
        let mut sym_type = symtable.get_type(first_name);

        for &name in &path[1..] {
            match (sym_term, sym_type) {
                (Some(TermSym::Namespace(namespace_id)), _) => {
                    let namespace = &vm.namespaces[namespace_id.to_usize()];
                    let symtable = namespace.table.read();

                    if !namespace_accessible_from(vm, namespace_id, import.namespace_id) {
                        let namespace = &vm.namespaces[namespace_id.to_usize()];
                        let msg = SemError::NotAccessible(namespace.name(vm));
                        vm.diag.lock().report(import.file_id, import.ast.pos, msg);
                        return Err(());
                    }

                    sym_term = symtable.get_term(name);
                    sym_type = symtable.get_type(name);
                }

                (None, None) => {
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

        Ok((sym_term, sym_type))
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
    let sym_term = namespace.table.read().get_term(element_name);
    let sym_type = namespace.table.read().get_type(element_name);

    match (sym_term, sym_type) {
        (Some(TermSym::Fct(fct_id)), _) => {
            if !fct_accessible_from(vm, fct_id, import.namespace_id) {
                let fct = &vm.fcts.idx(fct_id);
                let fct = fct.read();
                let msg = SemError::NotAccessible(fct.name(vm));
                vm.diag.lock().report(import.file_id, import.ast.pos, msg);
            }

            let new_sym = TermSym::Fct(fct_id);
            if let Some(old_sym) = table.write().insert_term(target_name, new_sym) {
                report_term_shadow(vm, target_name, import.file_id, import.ast.pos, old_sym);
            }
        }

        (Some(TermSym::Namespace(namespace_id)), _) => {
            if !namespace_accessible_from(vm, namespace_id, import.namespace_id) {
                let namespace = &vm.namespaces[namespace_id.to_usize()];
                let msg = SemError::NotAccessible(namespace.name(vm));
                vm.diag.lock().report(import.file_id, import.ast.pos, msg);
            }

            let new_sym = TermSym::Namespace(namespace_id);
            if let Some(old_sym) = table.write().insert_term(target_name, new_sym) {
                report_term_shadow(vm, target_name, import.file_id, import.ast.pos, old_sym);
            }
        }

        (Some(TermSym::Global(global_id)), _) => {
            if !global_accessible_from(vm, global_id, import.namespace_id) {
                let global = &vm.globals.idx(global_id);
                let global = global.read();
                let msg = SemError::NotAccessible(global.name(vm));
                vm.diag.lock().report(import.file_id, import.ast.pos, msg);
            }

            let new_sym = TermSym::Global(global_id);
            if let Some(old_sym) = table.write().insert_term(target_name, new_sym) {
                report_term_shadow(vm, target_name, import.file_id, import.ast.pos, old_sym);
            }
        }

        (Some(TermSym::Const(const_id)), _) => {
            let new_sym = TermSym::Const(const_id);
            if let Some(old_sym) = table.write().insert_term(target_name, new_sym) {
                report_term_shadow(vm, target_name, import.file_id, import.ast.pos, old_sym);
            }
        }

        (Some(sym_term), Some(TypeSym::Class(cls_id))) => {
            if !class_accessible_from(vm, cls_id, import.namespace_id) {
                let cls = &vm.classes.idx(cls_id);
                let cls = cls.read();
                let msg = SemError::NotAccessible(cls.name(vm));
                vm.diag.lock().report(import.file_id, import.ast.pos, msg);
            }

            let new_sym = TypeSym::Class(cls_id);
            let old_sym = table.write().insert_type(target_name, new_sym);
            if let Some(old_sym) = old_sym {
                report_type_shadow(vm, target_name, import.file_id, import.ast.pos, old_sym);
            } else {
                let result = table.write().insert_term(target_name, sym_term);
                assert!(result.is_none());
            }
        }

        (Some(TermSym::Module(module_id)), None) => {
            let new_sym = TermSym::Module(module_id);
            if let Some(old_sym) = table.write().insert_term(target_name, new_sym) {
                report_term_shadow(vm, target_name, import.file_id, import.ast.pos, old_sym);
            }
        }

        (None, Some(TypeSym::Trait(trait_id))) => {
            let new_sym = TypeSym::Trait(trait_id);
            let old_sym = table.write().insert_type(target_name, new_sym);
            if let Some(old_sym) = old_sym {
                report_type_shadow(vm, target_name, import.file_id, import.ast.pos, old_sym);
            }
        }

        (None, None) => {
            let name = vm.interner.str(element_name).to_string();
            let namespace_name = namespace.name(vm);
            vm.diag.lock().report(
                import.file_id.into(),
                import.ast.pos,
                SemError::UnknownIdentifierInNamespace(namespace_name, name),
            );
        }

        (_, _) => {
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

    if let Some(&variant_id) = xenum.name_to_value.get(&element_name) {
        let sym = TermSym::EnumValue(enum_id, variant_id as usize);
        if let Some(sym) = table.write().insert_term(target_name, sym) {
            report_term_shadow(vm, target_name, import.file_id.into(), import.ast.pos, sym);
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
}
