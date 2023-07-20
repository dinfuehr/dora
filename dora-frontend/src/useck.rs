use std::collections::HashSet;

use crate::access::sym_accessible_from;
use crate::error::msg::ErrorMessage;
use crate::report_sym_shadow_span;
use crate::sema::{module_package, ModuleDefinitionId, Sema};
use crate::sym::{ModuleSymTable, Sym};

use dora_parser::ast::{self, Ident, NodeId, UseAtom, UsePathComponentValue, UsePathDescriptor};
use dora_parser::Span;

use super::sema::SourceFileId;

pub fn check<'a>(sa: &Sema) {
    let mut all_resolved = HashSet::<(SourceFileId, NodeId)>::new();
    let mut more_work = true;

    while more_work {
        let mut resolved = false;
        let mut cancel = false;

        for use_elem in &sa.uses {
            let result = check_use(
                sa,
                &use_elem.ast,
                use_elem.module_id,
                use_elem.file_id,
                None,
                true,
                &mut all_resolved,
                &mut resolved,
            );

            match result {
                Ok(()) | Err(UseError::Unresolved) => {}
                Err(UseError::Fatal) => cancel = true,
            }
        }

        if cancel {
            return;
        }

        more_work = resolved;
    }

    for use_elem in &sa.uses {
        let _ = check_use(
            sa,
            &use_elem.ast,
            use_elem.module_id,
            use_elem.file_id,
            None,
            false,
            &mut all_resolved,
            &mut false,
        );
    }
}

enum UseError {
    Unresolved,
    Fatal,
}

fn check_use(
    sa: &Sema,
    use_declaration: &ast::UsePath,
    use_module_id: ModuleDefinitionId,
    use_file_id: SourceFileId,
    previous_sym: Option<Sym>,
    ignore_errors: bool,
    all_resolved: &mut HashSet<(SourceFileId, NodeId)>,
    resolved: &mut bool,
) -> Result<(), UseError> {
    if all_resolved.contains(&(use_file_id, use_declaration.id)) {
        return Ok(());
    }

    let (start_idx, mut previous_sym) = initial_module(
        sa,
        use_declaration,
        use_module_id,
        use_file_id,
        previous_sym,
    )?;

    for (idx, component) in use_declaration.path.iter().enumerate().skip(start_idx) {
        if !previous_sym.is_enum() && !previous_sym.is_module() {
            let msg = ErrorMessage::ExpectedPath;
            sa.report(use_file_id, use_declaration.path[idx - 1].span, msg);
            return Err(UseError::Fatal);
        }

        previous_sym = process_component(
            sa,
            use_module_id,
            use_file_id,
            previous_sym,
            component,
            ignore_errors,
        )?;
    }

    match &use_declaration.target {
        UsePathDescriptor::Default => {
            let last_component = use_declaration.path.last().expect("no component");

            let name = match last_component.value {
                UsePathComponentValue::Name(ref name) => name.clone(),
                UsePathComponentValue::Package
                | UsePathComponentValue::Super
                | UsePathComponentValue::This
                | UsePathComponentValue::Error => {
                    sa.report(use_file_id, last_component.span, ErrorMessage::ExpectedPath);
                    return Err(UseError::Fatal);
                }
            };

            assert!(all_resolved.insert((use_file_id, use_declaration.id)));
            *resolved = true;

            define_use_target(
                sa,
                use_file_id,
                last_component.span,
                use_module_id,
                name,
                previous_sym,
            )?;
        }
        UsePathDescriptor::As(target) => {
            let last_component = use_declaration.path.last().expect("no component");

            if let Some(ident) = &target.name {
                assert!(all_resolved.insert((use_file_id, use_declaration.id)));
                *resolved = true;

                define_use_target(
                    sa,
                    use_file_id,
                    last_component.span,
                    use_module_id,
                    ident.clone(),
                    previous_sym,
                )?;
            }
        }
        UsePathDescriptor::Group(ref group) => {
            if group.targets.is_empty() {
                sa.report(use_file_id, group.span, ErrorMessage::ExpectedPath);
                return Err(UseError::Fatal);
            }

            for nested_use in &group.targets {
                // Ignore errors as an error in `foo::{a, b, c}`
                // for `a` does not affect `b` or `c`.
                let _ = check_use(
                    sa,
                    nested_use,
                    use_module_id,
                    use_file_id,
                    Some(previous_sym.clone()),
                    ignore_errors,
                    all_resolved,
                    resolved,
                );
            }
        }

        UsePathDescriptor::Error => {}
    }

    Ok(())
}

fn initial_module(
    sa: &Sema,
    use_declaration: &ast::UsePath,
    use_module_id: ModuleDefinitionId,
    use_file_id: SourceFileId,
    previous_sym: Option<Sym>,
) -> Result<(usize, Sym), UseError> {
    if let Some(namespace) = previous_sym {
        return Ok((0, namespace));
    }

    if let Some(first_component) = use_declaration.path.first() {
        match first_component.value {
            UsePathComponentValue::This => Ok((1, Sym::Module(use_module_id))),
            UsePathComponentValue::Package => {
                Ok((1, Sym::Module(module_package(sa, use_module_id))))
            }
            UsePathComponentValue::Super => {
                let module = &sa.modules[use_module_id].read();
                if let Some(module_id) = module.parent_module_id {
                    Ok((1, Sym::Module(module_id)))
                } else {
                    sa.report(
                        use_file_id.into(),
                        first_component.span,
                        ErrorMessage::NoSuperModule,
                    );
                    Err(UseError::Fatal)
                }
            }
            UsePathComponentValue::Name(ref ident) => {
                if let Some(package_id) = sa.package_names.get(&ident.name_as_string).cloned() {
                    Ok((
                        1,
                        Sym::Module(sa.packages[package_id].top_level_module_id()),
                    ))
                } else {
                    Ok((0, Sym::Module(use_module_id)))
                }
            }
            UsePathComponentValue::Error => Err(UseError::Fatal),
        }
    } else {
        Err(UseError::Fatal)
    }
}

fn process_component(
    sa: &Sema,
    use_module_id: ModuleDefinitionId,
    use_file_id: SourceFileId,
    previous_sym: Sym,
    component: &UseAtom,
    ignore_errors: bool,
) -> Result<Sym, UseError> {
    let component_name = match component.value {
        UsePathComponentValue::Name(ref name) => name.clone(),
        UsePathComponentValue::Package
        | UsePathComponentValue::Super
        | UsePathComponentValue::This
        | UsePathComponentValue::Error => {
            sa.report(use_file_id, component.span, ErrorMessage::ExpectedPath);
            return Err(UseError::Fatal);
        }
    };

    match previous_sym {
        Sym::Module(module_id) => {
            let symtable = ModuleSymTable::new(sa, module_id);
            let current_sym = symtable.get_string(sa, &component_name.name_as_string);

            if let Some(current_sym) = current_sym {
                if sym_accessible_from(sa, current_sym.clone(), use_module_id) {
                    Ok(current_sym)
                } else {
                    let module = &sa.modules[module_id].read();
                    let name = component_name.name_as_string.clone();
                    let msg = ErrorMessage::NotAccessibleInModule(module.name(sa), name);
                    sa.report(use_file_id, component.span, msg);
                    Err(UseError::Fatal)
                }
            } else if ignore_errors {
                Err(UseError::Unresolved)
            } else {
                let module = sa.modules.idx(module_id);
                let module = module.read();
                let name = component_name.name_as_string.clone();
                let module_name = module.name(sa);
                sa.report(
                    use_file_id,
                    component.span,
                    ErrorMessage::UnknownIdentifierInModule(module_name, name),
                );
                Err(UseError::Unresolved)
            }
        }

        Sym::Enum(enum_id) => {
            let enum_ = sa.enums[enum_id].read();
            let name = sa.interner.intern(&component_name.name_as_string);

            if let Some(&variant_idx) = enum_.name_to_value.get(&name) {
                Ok(Sym::EnumVariant(enum_id, variant_idx))
            } else {
                let name = component_name.name_as_string.clone();
                sa.report(
                    use_file_id,
                    component.span,
                    ErrorMessage::UnknownEnumVariant(name),
                );
                Err(UseError::Fatal)
            }
        }

        _ => unreachable!(),
    }
}

fn define_use_target(
    sa: &Sema,
    use_file_id: SourceFileId,
    use_span: Span,
    module_id: ModuleDefinitionId,
    ident: Ident,
    sym: Sym,
) -> Result<(), UseError> {
    let module = sa.modules.idx(module_id);
    let module = module.read();

    let table = module.table.clone();
    let mut table = table.write();

    let name = sa.interner.intern(&ident.name_as_string);

    if let Some(old_sym) = table.insert(name, sym) {
        report_sym_shadow_span(sa, name, use_file_id, use_span, old_sym);
        Err(UseError::Fatal)
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::ErrorMessage;
    use crate::tests::*;

    #[test]
    fn check_initializer() {
        ok("let a: Int32 = 0i32;");
        ok("let a: Int32 = 0i32; let mut b: Int32 = a + 1i32;");
        err(
            "let mut a: Int32 = foo;",
            (1, 20),
            ErrorMessage::UnknownIdentifier("foo".into()),
        );
    }

    #[test]
    fn check_type() {
        err(
            "let mut x: Foo = 0;",
            (1, 12),
            ErrorMessage::UnknownIdentifier("Foo".into()),
        );
    }

    #[test]
    fn use_module() {
        err(
            "
            use foo::bar::Foo;
            mod foo {
                mod bar {
                    class Foo
                }
            }
        ",
            (2, 22),
            ErrorMessage::NotAccessibleInModule("foo".into(), "bar".into()),
        );
    }

    #[test]
    fn use_class() {
        err(
            "
            use foo::bar::Foo;
            mod foo {
                pub mod bar {
                    class Foo
                }
            }
        ",
            (2, 27),
            ErrorMessage::NotAccessibleInModule("foo::bar".into(), "Foo".into()),
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
            (2, 22),
            ErrorMessage::NotAccessibleInModule("foo".into(), "bar".into()),
        );
    }

    #[test]
    fn use_global() {
        err(
            "
            use foo::bar;
            mod foo {
                let mut bar: Int32 = 12;
            }
        ",
            (2, 22),
            ErrorMessage::NotAccessibleInModule("foo".into(), "bar".into()),
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
            (2, 22),
            ErrorMessage::NotAccessibleInModule("foo".into(), "bar".into()),
        );
    }

    #[test]
    fn use_enum() {
        err(
            "
            use foo::Bar;
            pub mod foo {
                enum Bar { A, B, C }
            }
        ",
            (2, 22),
            ErrorMessage::NotAccessibleInModule("foo".into(), "Bar".into()),
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
            (2, 22),
            ErrorMessage::NotAccessibleInModule("foo".into(), "Bar".into()),
        );

        ok("
            use foo::Bar::{A, B, C};
            mod foo {
                pub enum Bar { A, B, C }
            }
        ");
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
            (2, 22),
            ErrorMessage::NotAccessibleInModule("foo".into(), "Bar".into()),
        );
    }

    #[test]
    fn use_struct() {
        ok("
            use foo::Bar;
            mod foo {
                pub struct Bar { f: Int32 }
            }
        ");

        err(
            "
            use foo::Bar;
            mod foo {
                struct Bar { f: Int32 }
            }
        ",
            (2, 22),
            ErrorMessage::NotAccessibleInModule("foo".into(), "Bar".into()),
        );
    }

    #[test]
    fn use_public() {
        ok("
            pub use foo::Bar;
            pub mod foo {
                pub enum Bar { A, B, C }
            }
        ");
    }

    #[test]
    fn use_keyword_only() {
        err("use self;", (1, 5), ErrorMessage::ExpectedPath);
        err("use package;", (1, 5), ErrorMessage::ExpectedPath);
        err(
            "mod foo { use super; }",
            (1, 15),
            ErrorMessage::ExpectedPath,
        );
    }

    #[test]
    fn use_keyword_in_path() {
        err(
            "use foo::bar::self; mod foo { pub mod bar {} }",
            (1, 15),
            ErrorMessage::ExpectedPath,
        );
        err(
            "use foo::bar::super; mod foo { pub mod bar {} }",
            (1, 15),
            ErrorMessage::ExpectedPath,
        );
        err(
            "use foo::bar::package; mod foo { pub mod bar {} }",
            (1, 15),
            ErrorMessage::ExpectedPath,
        );
    }

    #[test]
    fn no_use_targets() {
        err(
            "use foo::bar:: {}; mod foo { pub mod bar {} }",
            (1, 16),
            ErrorMessage::ExpectedPath,
        );
    }

    #[test]
    fn use_zig_zag() {
        ok("
            pub use foo::f1 as f2;
            pub use foo::f3 as f4;

            mod foo {
                pub use super::f2 as f3;
                pub use super::f4 as f5;

                pub fn f1() {}
            }
        ");
    }

    #[test]
    fn use_cyclic() {
        errors(
            "
            pub use foo::f1 as f2;

            mod foo {
                pub use super::f2 as f1;
            }
        ",
            &[
                (
                    (2, 26),
                    ErrorMessage::UnknownIdentifierInModule("foo".into(), "f1".into()),
                ),
                (
                    (5, 32),
                    ErrorMessage::UnknownIdentifierInModule("".into(), "f2".into()),
                ),
            ],
        );
    }

    #[test]
    fn use_group() {
        errors(
            "
            use foo::{a, b};
            mod foo {}
        ",
            &[
                (
                    (2, 23),
                    ErrorMessage::UnknownIdentifierInModule("foo".into(), "a".into()),
                ),
                (
                    (2, 26),
                    ErrorMessage::UnknownIdentifierInModule("foo".into(), "b".into()),
                ),
            ],
        )
    }
}
