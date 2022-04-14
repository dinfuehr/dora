use parking_lot::RwLock;

use crate::language::access::{
    class_accessible_from, const_accessible_from, enum_accessible_from, fct_accessible_from,
    global_accessible_from, namespace_accessible_from, struct_accessible_from,
    trait_accessible_from,
};
use crate::language::error::msg::SemError;
use crate::language::report_sym_shadow;
use crate::language::sem_analysis::{
    module_package, EnumDefinitionId, ModuleDefinitionId, UseDefinition,
};
use crate::language::sym::{NestedSymTable, Sym, SymTable};
use crate::vm::SemAnalysis;

use dora_parser::ast::UseContext;
use dora_parser::interner::Name;

pub fn check<'a>(sa: &SemAnalysis) {
    for use_def in &sa.uses {
        check_use(sa, use_def);
    }
}

fn check_use(sa: &SemAnalysis, use_def: &UseDefinition) {
    let table = sa.namespace_table(use_def.module_id);

    let namespace_id = match use_def.ast.context {
        UseContext::This => use_def.module_id,
        UseContext::Package => module_package(sa, use_def.module_id),
        UseContext::Super => {
            let namespace = &sa.modules[use_def.module_id].read();
            if let Some(namespace_id) = namespace.parent_module_id {
                namespace_id
            } else {
                sa.diag.lock().report(
                    use_def.file_id.into(),
                    use_def.ast.pos,
                    SemError::NoSuperNamespace,
                );
                return;
            }
        }
    };

    let symtable = NestedSymTable::new(sa, namespace_id);

    let element_name = use_def.ast.element_name;
    let target_name = use_def.ast.target_name.unwrap_or(element_name);

    if use_def.ast.path.is_empty() {
        use_namespace(sa, use_def, &table, namespace_id, element_name, target_name);
    } else {
        let sym = match read_path(sa, use_def, &symtable) {
            Ok(sym) => sym,
            Err(()) => {
                return;
            }
        };

        match sym {
            Some(Sym::Namespace(namespace_id)) => {
                if !namespace_accessible_from(sa, namespace_id, use_def.module_id) {
                    let namespace = &sa.modules[namespace_id].read();
                    let msg = SemError::NotAccessible(namespace.name(sa));
                    sa.diag.lock().report(use_def.file_id, use_def.ast.pos, msg);
                    return;
                }

                use_namespace(sa, use_def, &table, namespace_id, element_name, target_name)
            }

            Some(Sym::Enum(enum_id)) => {
                use_enum(sa, use_def, &table, enum_id, element_name, target_name)
            }

            _ => {
                sa.diag.lock().report(
                    use_def.file_id.into(),
                    use_def.ast.pos,
                    SemError::ExpectedPath,
                );
            }
        }
    }
}

fn read_path(
    sa: &SemAnalysis,
    use_def: &UseDefinition,
    symtable: &NestedSymTable,
) -> Result<Option<Sym>, ()> {
    if !use_def.ast.path.is_empty() {
        let path = &use_def.ast.path;
        let first_name = path.first().cloned().unwrap();

        let mut sym = symtable.get(first_name);

        for &name in &path[1..] {
            match sym {
                Some(Sym::Namespace(namespace_id)) => {
                    let namespace = &sa.modules[namespace_id].read();
                    let symtable = namespace.table.read();

                    if !namespace_accessible_from(sa, namespace_id, use_def.module_id) {
                        let namespace = &sa.modules[namespace_id].read();
                        let msg = SemError::NotAccessible(namespace.name(sa));
                        sa.diag.lock().report(use_def.file_id, use_def.ast.pos, msg);
                        return Err(());
                    }

                    sym = symtable.get(name);
                }

                None => {
                    let msg = SemError::ExpectedPath;
                    sa.diag.lock().report(use_def.file_id, use_def.ast.pos, msg);
                    return Err(());
                }

                _ => {
                    let msg = SemError::ExpectedNamespace;
                    sa.diag.lock().report(use_def.file_id, use_def.ast.pos, msg);
                    return Err(());
                }
            }
        }

        Ok(sym)
    } else {
        let msg = SemError::ExpectedPath;
        sa.diag.lock().report(use_def.file_id, use_def.ast.pos, msg);
        Err(())
    }
}

fn use_namespace(
    sa: &SemAnalysis,
    use_def: &UseDefinition,
    table: &RwLock<SymTable>,
    namespace_id: ModuleDefinitionId,
    element_name: Name,
    target_name: Name,
) {
    let namespace = &sa.modules[namespace_id].read();
    let sym = namespace.table.read().get(element_name);

    match sym {
        Some(Sym::Fct(fct_id)) => {
            if !fct_accessible_from(sa, fct_id, use_def.module_id) {
                let fct = &sa.fcts.idx(fct_id);
                let fct = fct.read();
                let msg = SemError::NotAccessible(fct.name(sa));
                sa.diag.lock().report(use_def.file_id, use_def.ast.pos, msg);
            }

            let new_sym = Sym::Fct(fct_id);
            if let Some(old_sym) = table.write().insert(target_name, new_sym) {
                report_sym_shadow(sa, target_name, use_def.file_id, use_def.ast.pos, old_sym);
            }
        }

        Some(Sym::Namespace(namespace_id)) => {
            if !namespace_accessible_from(sa, namespace_id, use_def.module_id) {
                let namespace = &sa.modules[namespace_id].read();
                let msg = SemError::NotAccessible(namespace.name(sa));
                sa.diag.lock().report(use_def.file_id, use_def.ast.pos, msg);
            }

            let new_sym = Sym::Namespace(namespace_id);
            if let Some(old_sym) = table.write().insert(target_name, new_sym) {
                report_sym_shadow(sa, target_name, use_def.file_id, use_def.ast.pos, old_sym);
            }
        }

        Some(Sym::Global(global_id)) => {
            if !global_accessible_from(sa, global_id, use_def.module_id) {
                let global = &sa.globals.idx(global_id);
                let global = global.read();
                let msg = SemError::NotAccessible(global.name(sa));
                sa.diag.lock().report(use_def.file_id, use_def.ast.pos, msg);
            }

            let new_sym = Sym::Global(global_id);
            if let Some(old_sym) = table.write().insert(target_name, new_sym) {
                report_sym_shadow(sa, target_name, use_def.file_id, use_def.ast.pos, old_sym);
            }
        }

        Some(Sym::Const(const_id)) => {
            if !const_accessible_from(sa, const_id, use_def.module_id) {
                let xconst = &sa.consts.idx(const_id);
                let xconst = xconst.read();
                let msg = SemError::NotAccessible(xconst.name(sa));
                sa.diag.lock().report(use_def.file_id, use_def.ast.pos, msg);
            }

            let new_sym = Sym::Const(const_id);
            if let Some(old_sym) = table.write().insert(target_name, new_sym) {
                report_sym_shadow(sa, target_name, use_def.file_id, use_def.ast.pos, old_sym);
            }
        }

        Some(Sym::Class(cls_id)) => {
            if !class_accessible_from(sa, cls_id, use_def.module_id) {
                let cls = &sa.classes.idx(cls_id);
                let cls = cls.read();
                let msg = SemError::NotAccessible(cls.name(sa));
                sa.diag.lock().report(use_def.file_id, use_def.ast.pos, msg);
            }

            let new_sym = Sym::Class(cls_id);
            let old_sym = table.write().insert(target_name, new_sym);
            if let Some(old_sym) = old_sym {
                report_sym_shadow(sa, target_name, use_def.file_id, use_def.ast.pos, old_sym);
            }
        }

        Some(Sym::Enum(enum_id)) => {
            if !enum_accessible_from(sa, enum_id, use_def.module_id) {
                let xenum = sa.enums[enum_id].read();
                let msg = SemError::NotAccessible(xenum.name(sa));
                sa.diag.lock().report(use_def.file_id, use_def.ast.pos, msg);
            }

            let new_sym = Sym::Enum(enum_id);
            if let Some(old_sym) = table.write().insert(target_name, new_sym) {
                report_sym_shadow(sa, target_name, use_def.file_id, use_def.ast.pos, old_sym);
            }
        }

        Some(Sym::Struct(struct_id)) => {
            if !struct_accessible_from(sa, struct_id, use_def.module_id) {
                let xstruct = sa.structs.idx(struct_id);
                let xstruct = xstruct.read();
                let msg = SemError::NotAccessible(xstruct.name(sa));
                sa.diag.lock().report(use_def.file_id, use_def.ast.pos, msg);
            }

            let new_sym = Sym::Struct(struct_id);
            let old_sym = table.write().insert(target_name, new_sym);
            if let Some(old_sym) = old_sym {
                report_sym_shadow(sa, target_name, use_def.file_id, use_def.ast.pos, old_sym);
            }
        }

        Some(Sym::Trait(trait_id)) => {
            if !trait_accessible_from(sa, trait_id, use_def.module_id) {
                let xtrait = sa.traits[trait_id].read();
                let msg = SemError::NotAccessible(xtrait.name(sa));
                sa.diag.lock().report(use_def.file_id, use_def.ast.pos, msg);
            }

            let new_sym = Sym::Trait(trait_id);
            let old_sym = table.write().insert(target_name, new_sym);
            if let Some(old_sym) = old_sym {
                report_sym_shadow(sa, target_name, use_def.file_id, use_def.ast.pos, old_sym);
            }
        }

        None => {
            let name = sa.interner.str(element_name).to_string();
            let namespace_name = namespace.name(sa);
            sa.diag.lock().report(
                use_def.file_id.into(),
                use_def.ast.pos,
                SemError::UnknownIdentifierInNamespace(namespace_name, name),
            );
        }

        _ => {
            sa.diag.lock().report(
                use_def.file_id.into(),
                use_def.ast.pos,
                SemError::ExpectedPath,
            );
        }
    }
}

fn use_enum(
    sa: &SemAnalysis,
    use_def: &UseDefinition,
    table: &RwLock<SymTable>,
    enum_id: EnumDefinitionId,
    element_name: Name,
    target_name: Name,
) {
    let xenum = sa.enums[enum_id].read();

    if !enum_accessible_from(sa, enum_id, use_def.module_id) {
        let msg = SemError::NotAccessible(xenum.name(sa));
        sa.diag.lock().report(use_def.file_id, use_def.ast.pos, msg);
    }

    if let Some(&variant_id) = xenum.name_to_value.get(&element_name) {
        let sym = Sym::EnumValue(enum_id, variant_id as usize);
        if let Some(sym) = table.write().insert(target_name, sym) {
            report_sym_shadow(
                sa,
                target_name,
                use_def.file_id.into(),
                use_def.ast.pos,
                sym,
            );
        }
    } else {
        let name = sa.interner.str(element_name).to_string();
        sa.diag.lock().report(
            use_def.file_id.into(),
            use_def.ast.pos,
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
        ok("let a: Int32 = 0I;");
        ok("let a: Int32 = 0I; var b: Int32 = a + 1I;");
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
    fn use_namespace() {
        err(
            "
            use foo::bar::Foo;
            mod foo {
                mod bar {
                    class Foo
                }
            }
        ",
            pos(2, 13),
            SemError::NotAccessible("foo::bar".into()),
        );
    }

    #[test]
    fn use_class() {
        err(
            "
            use foo::bar::Foo;
            mod foo {
                @pub mod bar {
                    class Foo
                }
            }
        ",
            pos(2, 13),
            SemError::NotAccessible("foo::bar::Foo".into()),
        );
    }

    #[test]
    fn use_fct() {
        err(
            "
            use foo::bar;
            mod foo {
                fn bar() {}
            }
        ",
            pos(2, 13),
            SemError::NotAccessible("foo::bar".into()),
        );
    }

    #[test]
    fn use_global() {
        err(
            "
            use foo::bar;
            mod foo {
                var bar: Int32 = 12;
            }
        ",
            pos(2, 13),
            SemError::NotAccessible("foo::bar".into()),
        );
    }

    #[test]
    fn use_const() {
        err(
            "
            use foo::bar;
            mod foo {
                const bar: Int32 = 12;
            }
        ",
            pos(2, 13),
            SemError::NotAccessible("foo::bar".into()),
        );
    }

    #[test]
    fn use_enum() {
        err(
            "
            use foo::Bar;
            mod foo {
                enum Bar { A, B, C }
            }
        ",
            pos(2, 13),
            SemError::NotAccessible("foo::Bar".into()),
        );
    }

    #[test]
    fn use_enum_value() {
        err(
            "
            use foo::Bar::A;
            mod foo {
                enum Bar { A, B, C }
            }
        ",
            pos(2, 13),
            SemError::NotAccessible("foo::Bar".into()),
        );
    }

    #[test]
    fn use_trait() {
        err(
            "
            use foo::Bar;
            mod foo {
                trait Bar {}
            }
        ",
            pos(2, 13),
            SemError::NotAccessible("foo::Bar".into()),
        );
    }

    #[test]
    fn use_struct() {
        ok("
            use foo::Bar;
            mod foo {
                @pub struct Bar { f: Int32 }
            }
        ");

        err(
            "
            use foo::Bar;
            mod foo {
                struct Bar { f: Int32 }
            }
        ",
            pos(2, 13),
            SemError::NotAccessible("foo::Bar".into()),
        );
    }
}
