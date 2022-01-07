use parking_lot::RwLock;

use crate::language::access::{
    class_accessible_from, enum_accessible_from, fct_accessible_from, global_accessible_from,
    struct_accessible_from,
};
use crate::language::error::msg::SemError;
use crate::language::report_sym_shadow;
use crate::language::sym::{NestedSymTable, Sym, SymTable};
use crate::vm::{
    const_accessible_from, module_accessible_from, namespace_accessible_from, namespace_package,
    trait_accessible_from, EnumDefinitionId, ImportData, NamespaceId, SemAnalysis,
};

use dora_parser::ast::ImportContext;
use dora_parser::interner::Name;

pub fn check<'a>(sa: &SemAnalysis) {
    for import in &sa.imports {
        check_import(sa, import);
    }
}

fn check_import(sa: &SemAnalysis, import: &ImportData) {
    let table = sa.namespace_table(import.namespace_id);

    let namespace_id = match import.ast.context {
        ImportContext::This => import.namespace_id,
        ImportContext::Package => namespace_package(sa, import.namespace_id),
        ImportContext::Super => {
            let namespace = &sa.namespaces[import.namespace_id.to_usize()];
            if let Some(namespace_id) = namespace.parent_namespace_id {
                namespace_id
            } else {
                sa.diag.lock().report(
                    import.file_id.into(),
                    import.ast.pos,
                    SemError::NoSuperNamespace,
                );
                return;
            }
        }
    };

    let symtable = NestedSymTable::new(sa, namespace_id);

    let element_name = import.ast.element_name;
    let target_name = import.ast.target_name.unwrap_or(element_name);

    if import.ast.path.is_empty() {
        import_namespace(sa, import, &table, namespace_id, element_name, target_name);
    } else {
        let sym = match read_path(sa, import, &symtable) {
            Ok(sym) => sym,
            Err(()) => {
                return;
            }
        };

        match sym {
            Some(Sym::Namespace(namespace_id)) => {
                if !namespace_accessible_from(sa, namespace_id, import.namespace_id) {
                    let namespace = &sa.namespaces[namespace_id.to_usize()];
                    let msg = SemError::NotAccessible(namespace.name(sa));
                    sa.diag.lock().report(import.file_id, import.ast.pos, msg);
                    return;
                }

                import_namespace(sa, import, &table, namespace_id, element_name, target_name)
            }

            Some(Sym::Enum(enum_id)) => {
                import_enum(sa, import, &table, enum_id, element_name, target_name)
            }

            _ => {
                sa.diag.lock().report(
                    import.file_id.into(),
                    import.ast.pos,
                    SemError::ExpectedPath,
                );
            }
        }
    }
}

fn read_path(
    sa: &SemAnalysis,
    import: &ImportData,
    symtable: &NestedSymTable,
) -> Result<Option<Sym>, ()> {
    if !import.ast.path.is_empty() {
        let path = &import.ast.path;
        let first_name = path.first().cloned().unwrap();

        let mut sym = symtable.get(first_name);

        for &name in &path[1..] {
            match sym {
                Some(Sym::Namespace(namespace_id)) => {
                    let namespace = &sa.namespaces[namespace_id.to_usize()];
                    let symtable = namespace.table.read();

                    if !namespace_accessible_from(sa, namespace_id, import.namespace_id) {
                        let namespace = &sa.namespaces[namespace_id.to_usize()];
                        let msg = SemError::NotAccessible(namespace.name(sa));
                        sa.diag.lock().report(import.file_id, import.ast.pos, msg);
                        return Err(());
                    }

                    sym = symtable.get(name);
                }

                None => {
                    let msg = SemError::ExpectedPath;
                    sa.diag.lock().report(import.file_id, import.ast.pos, msg);
                    return Err(());
                }

                _ => {
                    let msg = SemError::ExpectedNamespace;
                    sa.diag.lock().report(import.file_id, import.ast.pos, msg);
                    return Err(());
                }
            }
        }

        Ok(sym)
    } else {
        let msg = SemError::ExpectedPath;
        sa.diag.lock().report(import.file_id, import.ast.pos, msg);
        Err(())
    }
}

fn import_namespace(
    sa: &SemAnalysis,
    import: &ImportData,
    table: &RwLock<SymTable>,
    namespace_id: NamespaceId,
    element_name: Name,
    target_name: Name,
) {
    let namespace = &sa.namespaces[namespace_id.to_usize()];
    let sym = namespace.table.read().get(element_name);

    match sym {
        Some(Sym::Fct(fct_id)) => {
            if !fct_accessible_from(sa, fct_id, import.namespace_id) {
                let fct = &sa.fcts.idx(fct_id);
                let fct = fct.read();
                let msg = SemError::NotAccessible(fct.name(sa));
                sa.diag.lock().report(import.file_id, import.ast.pos, msg);
            }

            let new_sym = Sym::Fct(fct_id);
            if let Some(old_sym) = table.write().insert(target_name, new_sym) {
                report_sym_shadow(sa, target_name, import.file_id, import.ast.pos, old_sym);
            }
        }

        Some(Sym::Namespace(namespace_id)) => {
            if !namespace_accessible_from(sa, namespace_id, import.namespace_id) {
                let namespace = &sa.namespaces[namespace_id.to_usize()];
                let msg = SemError::NotAccessible(namespace.name(sa));
                sa.diag.lock().report(import.file_id, import.ast.pos, msg);
            }

            let new_sym = Sym::Namespace(namespace_id);
            if let Some(old_sym) = table.write().insert(target_name, new_sym) {
                report_sym_shadow(sa, target_name, import.file_id, import.ast.pos, old_sym);
            }
        }

        Some(Sym::Global(global_id)) => {
            if !global_accessible_from(sa, global_id, import.namespace_id) {
                let global = &sa.globals.idx(global_id);
                let global = global.read();
                let msg = SemError::NotAccessible(global.name(sa));
                sa.diag.lock().report(import.file_id, import.ast.pos, msg);
            }

            let new_sym = Sym::Global(global_id);
            if let Some(old_sym) = table.write().insert(target_name, new_sym) {
                report_sym_shadow(sa, target_name, import.file_id, import.ast.pos, old_sym);
            }
        }

        Some(Sym::Const(const_id)) => {
            if !const_accessible_from(sa, const_id, import.namespace_id) {
                let xconst = &sa.consts.idx(const_id);
                let xconst = xconst.read();
                let msg = SemError::NotAccessible(xconst.name(sa));
                sa.diag.lock().report(import.file_id, import.ast.pos, msg);
            }

            let new_sym = Sym::Const(const_id);
            if let Some(old_sym) = table.write().insert(target_name, new_sym) {
                report_sym_shadow(sa, target_name, import.file_id, import.ast.pos, old_sym);
            }
        }

        Some(Sym::Class(cls_id)) => {
            if !class_accessible_from(sa, cls_id, import.namespace_id) {
                let cls = &sa.classes.idx(cls_id);
                let cls = cls.read();
                let msg = SemError::NotAccessible(cls.name(sa));
                sa.diag.lock().report(import.file_id, import.ast.pos, msg);
            }

            let new_sym = Sym::Class(cls_id);
            let old_sym = table.write().insert(target_name, new_sym);
            if let Some(old_sym) = old_sym {
                report_sym_shadow(sa, target_name, import.file_id, import.ast.pos, old_sym);
            }
        }

        Some(Sym::Enum(enum_id)) => {
            if !enum_accessible_from(sa, enum_id, import.namespace_id) {
                let xenum = sa.enums[enum_id].read();
                let msg = SemError::NotAccessible(xenum.name(sa));
                sa.diag.lock().report(import.file_id, import.ast.pos, msg);
            }

            let new_sym = Sym::Enum(enum_id);
            if let Some(old_sym) = table.write().insert(target_name, new_sym) {
                report_sym_shadow(sa, target_name, import.file_id, import.ast.pos, old_sym);
            }
        }

        Some(Sym::Struct(struct_id)) => {
            if !struct_accessible_from(sa, struct_id, import.namespace_id) {
                let xstruct = sa.structs.idx(struct_id);
                let xstruct = xstruct.read();
                let msg = SemError::NotAccessible(xstruct.name(sa));
                sa.diag.lock().report(import.file_id, import.ast.pos, msg);
            }

            let new_sym = Sym::Struct(struct_id);
            let old_sym = table.write().insert(target_name, new_sym);
            if let Some(old_sym) = old_sym {
                report_sym_shadow(sa, target_name, import.file_id, import.ast.pos, old_sym);
            }
        }

        Some(Sym::Module(module_id)) => {
            if !module_accessible_from(sa, module_id, import.namespace_id) {
                let module = sa.modules.idx(module_id);
                let module = module.read();
                let msg = SemError::NotAccessible(module.name(sa));
                sa.diag.lock().report(import.file_id, import.ast.pos, msg);
            }

            let new_sym = Sym::Module(module_id);
            if let Some(old_sym) = table.write().insert(target_name, new_sym) {
                report_sym_shadow(sa, target_name, import.file_id, import.ast.pos, old_sym);
            }
        }

        Some(Sym::Trait(trait_id)) => {
            if !trait_accessible_from(sa, trait_id, import.namespace_id) {
                let xtrait = sa.traits[trait_id].read();
                let msg = SemError::NotAccessible(xtrait.name(sa));
                sa.diag.lock().report(import.file_id, import.ast.pos, msg);
            }

            let new_sym = Sym::Trait(trait_id);
            let old_sym = table.write().insert(target_name, new_sym);
            if let Some(old_sym) = old_sym {
                report_sym_shadow(sa, target_name, import.file_id, import.ast.pos, old_sym);
            }
        }

        None => {
            let name = sa.interner.str(element_name).to_string();
            let namespace_name = namespace.name(sa);
            sa.diag.lock().report(
                import.file_id.into(),
                import.ast.pos,
                SemError::UnknownIdentifierInNamespace(namespace_name, name),
            );
        }

        _ => {
            sa.diag.lock().report(
                import.file_id.into(),
                import.ast.pos,
                SemError::ExpectedPath,
            );
        }
    }
}

fn import_enum(
    sa: &SemAnalysis,
    import: &ImportData,
    table: &RwLock<SymTable>,
    enum_id: EnumDefinitionId,
    element_name: Name,
    target_name: Name,
) {
    let xenum = sa.enums[enum_id].read();

    if !enum_accessible_from(sa, enum_id, import.namespace_id) {
        let msg = SemError::NotAccessible(xenum.name(sa));
        sa.diag.lock().report(import.file_id, import.ast.pos, msg);
    }

    if let Some(&variant_id) = xenum.name_to_value.get(&element_name) {
        let sym = Sym::EnumValue(enum_id, variant_id as usize);
        if let Some(sym) = table.write().insert(target_name, sym) {
            report_sym_shadow(sa, target_name, import.file_id.into(), import.ast.pos, sym);
        }
    } else {
        let name = sa.interner.str(element_name).to_string();
        sa.diag.lock().report(
            import.file_id.into(),
            import.ast.pos,
            SemError::UnknownEnumValue(name),
        );
    }
}

#[cfg(test)]
mod tests {
    use crate::language::error::msg::SemError;
    use crate::language::tests::*;

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
