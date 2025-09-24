use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::access::{sym_accessible_from, use_accessible_from};
use crate::error::msg::ErrorMessage;
use crate::report_sym_shadow_span;
use crate::sema::{module_package, ModuleDefinitionId, Sema, Visibility};
use crate::sym::{SymTable, SymbolKind};

use dora_parser::ast::{self, UseAtom, UsePathComponentValue, UsePathDescriptor};
use dora_parser::Span;

use super::sema::SourceFileId;

pub fn check<'a>(sa: &Sema, mut module_symtables: HashMap<ModuleDefinitionId, SymTable>) {
    let mut processed_uses = HashSet::<(SourceFileId, ast::AstId)>::new();

    while {
        let mut did_resolve_symbol = false;

        for (_id, use_definition) in &sa.uses {
            let _ = check_use(
                sa,
                &mut module_symtables,
                use_definition.ast_id,
                use_definition.module_id,
                use_definition.file_id,
                use_definition.visibility,
                None,
                true,
                &mut processed_uses,
                &mut did_resolve_symbol,
            );
        }

        did_resolve_symbol
    } {}

    for (_id, use_definition) in &sa.uses {
        let _ = check_use(
            sa,
            &mut module_symtables,
            use_definition.ast_id,
            use_definition.module_id,
            use_definition.file_id,
            use_definition.visibility,
            None,
            false,
            &mut processed_uses,
            &mut false,
        );
    }

    for (module_id, table) in module_symtables {
        assert!(sa.module(module_id).table.set(Rc::new(table)).is_ok());
    }
}

fn check_use(
    sa: &Sema,
    module_symtables: &mut HashMap<ModuleDefinitionId, SymTable>,
    use_path_id: ast::AstId,
    use_module_id: ModuleDefinitionId,
    use_file_id: SourceFileId,
    use_visibility: Visibility,
    previous_sym: Option<SymbolKind>,
    ignore_unknown_symbols: bool,
    processed_uses: &mut HashSet<(SourceFileId, ast::AstId)>,
    did_resolve_symbol: &mut bool,
) -> Result<(), ()> {
    if processed_uses.contains(&(use_file_id, use_path_id)) {
        return Ok(());
    }

    let use_path = sa
        .node(use_file_id, use_path_id)
        .to_use_path()
        .expect("use path expected");

    let (start_idx, mut previous_sym) = initial_module(
        sa,
        use_path_id,
        use_module_id,
        use_file_id,
        previous_sym,
        processed_uses,
    )?;

    for (idx, component) in use_path.path.iter().enumerate().skip(start_idx) {
        if !previous_sym.is_enum() && !previous_sym.is_module() {
            let msg = ErrorMessage::ExpectedPath;
            sa.report(use_file_id, use_path.path[idx - 1].span, msg);
            assert!(processed_uses.insert((use_file_id, use_path_id)));
            return Err(());
        }

        previous_sym = process_component(
            sa,
            module_symtables,
            use_path_id,
            use_module_id,
            use_file_id,
            previous_sym,
            component,
            processed_uses,
            ignore_unknown_symbols,
        )?;
    }

    match &use_path.target {
        UsePathDescriptor::Default => {
            let last_component = use_path.path.last().expect("no component");
            assert!(processed_uses.insert((use_file_id, use_path_id)));

            let name = match last_component.value {
                UsePathComponentValue::Name(ref name) => name.clone(),
                UsePathComponentValue::Package
                | UsePathComponentValue::Super
                | UsePathComponentValue::This
                | UsePathComponentValue::Error => {
                    sa.report(use_file_id, last_component.span, ErrorMessage::ExpectedPath);
                    return Err(());
                }
            };

            *did_resolve_symbol = true;

            define_use_target(
                sa,
                module_symtables,
                use_file_id,
                last_component.span,
                use_visibility,
                use_module_id,
                name,
                previous_sym,
            )?;
        }
        UsePathDescriptor::As(target_id) => {
            let target = sa
                .node(use_file_id, *target_id)
                .to_use_target_name()
                .expect("use target expected");
            let last_component = use_path.path.last().expect("no component");
            assert!(processed_uses.insert((use_file_id, use_path_id)));

            if let Some(ident) = target.name {
                *did_resolve_symbol = true;

                define_use_target(
                    sa,
                    module_symtables,
                    use_file_id,
                    last_component.span,
                    use_visibility,
                    use_module_id,
                    ident.clone(),
                    previous_sym,
                )?;
            }
        }
        UsePathDescriptor::Group(use_group_id) => {
            let group = sa
                .node(use_file_id, *use_group_id)
                .to_use_group()
                .expect("use group expected");

            if group.targets.is_empty() {
                sa.report(use_file_id, group.span, ErrorMessage::ExpectedPath);
                assert!(processed_uses.insert((use_file_id, use_path_id)));
                return Err(());
            }

            for &nested_use in &group.targets {
                // Ignore errors as an error in `foo::{a, b, c}`
                // for `a` does not affect `b` or `c`.
                let _ = check_use(
                    sa,
                    module_symtables,
                    nested_use,
                    use_module_id,
                    use_file_id,
                    use_visibility,
                    Some(previous_sym.clone()),
                    ignore_unknown_symbols,
                    processed_uses,
                    did_resolve_symbol,
                );
            }
        }

        UsePathDescriptor::Error => {
            assert!(processed_uses.insert((use_file_id, use_path_id)));
        }
    }

    Ok(())
}

fn initial_module(
    sa: &Sema,
    use_path_id: ast::AstId,
    use_module_id: ModuleDefinitionId,
    use_file_id: SourceFileId,
    previous_sym: Option<SymbolKind>,
    processed_uses: &mut HashSet<(SourceFileId, ast::AstId)>,
) -> Result<(usize, SymbolKind), ()> {
    if let Some(namespace) = previous_sym {
        return Ok((0, namespace));
    }

    let use_path = sa
        .node(use_file_id, use_path_id)
        .to_use_path()
        .expect("use path expected");

    if let Some(first_component) = use_path.path.first() {
        match first_component.value {
            UsePathComponentValue::This => Ok((1, SymbolKind::Module(use_module_id))),
            UsePathComponentValue::Package => {
                Ok((1, SymbolKind::Module(module_package(sa, use_module_id))))
            }
            UsePathComponentValue::Super => {
                let module = sa.module(use_module_id);
                if let Some(module_id) = module.parent_module_id {
                    Ok((1, SymbolKind::Module(module_id)))
                } else {
                    sa.report(
                        use_file_id.into(),
                        first_component.span,
                        ErrorMessage::NoSuperModule,
                    );
                    assert!(processed_uses.insert((use_file_id, use_path_id)));
                    Err(())
                }
            }
            UsePathComponentValue::Name(ident_id) => {
                let ident = sa
                    .node(use_file_id, ident_id)
                    .to_ident()
                    .expect("ident expected");

                if let Some(package_id) = sa.package_names.get(&ident.name).cloned() {
                    Ok((
                        1,
                        SymbolKind::Module(sa.packages[package_id].top_level_module_id()),
                    ))
                } else {
                    Ok((0, SymbolKind::Module(use_module_id)))
                }
            }
            UsePathComponentValue::Error => Err(()),
        }
    } else {
        Err(())
    }
}

fn process_component(
    sa: &Sema,
    module_symtables: &mut HashMap<ModuleDefinitionId, SymTable>,
    use_path_id: ast::AstId,
    use_module_id: ModuleDefinitionId,
    use_file_id: SourceFileId,
    previous_sym: SymbolKind,
    component: &UseAtom,
    processed_uses: &mut HashSet<(SourceFileId, ast::AstId)>,
    ignore_unknown_symbols: bool,
) -> Result<SymbolKind, ()> {
    let component_name_id = match component.value {
        UsePathComponentValue::Name(name) => name,
        UsePathComponentValue::Package
        | UsePathComponentValue::Super
        | UsePathComponentValue::This
        | UsePathComponentValue::Error => {
            sa.report(use_file_id, component.span, ErrorMessage::ExpectedPath);
            assert!(processed_uses.insert((use_file_id, use_path_id)));
            return Err(());
        }
    };

    match previous_sym {
        SymbolKind::Module(module_id) => {
            let symtable = module_symtables.get(&module_id).expect("missing symtable");

            let component_name = sa
                .node(use_file_id, component_name_id)
                .to_ident()
                .expect("ident expected");
            let name = sa.interner.intern(&component_name.name);

            let current_sym = symtable.get_sym(name);

            if let Some(current_sym) = current_sym {
                if let Some(visibility) = current_sym.visibility() {
                    if !use_accessible_from(sa, module_id, visibility.to_owned(), use_module_id) {
                        let msg = ErrorMessage::UseNotAccessible;
                        sa.report(use_file_id, component.span, msg);
                        assert!(processed_uses.insert((use_file_id, use_path_id)));
                        return Err(());
                    }
                }

                if sym_accessible_from(sa, current_sym.kind().to_owned(), use_module_id) {
                    Ok(current_sym.kind().to_owned())
                } else {
                    let module = sa.module(module_id);
                    let name = component_name.name.clone();
                    let msg = ErrorMessage::NotAccessibleInModule(module.name(sa), name);
                    assert!(processed_uses.insert((use_file_id, use_path_id)));
                    sa.report(use_file_id, component.span, msg);
                    Err(())
                }
            } else if ignore_unknown_symbols {
                Err(())
            } else {
                let module = sa.module(module_id);
                let name = component_name.name.clone();
                let module_name = module.name(sa);
                sa.report(
                    use_file_id,
                    component.span,
                    ErrorMessage::UnknownIdentifierInModule(module_name, name),
                );
                Err(())
            }
        }

        SymbolKind::Enum(enum_id) => {
            let enum_ = sa.enum_(enum_id);

            let component_name = sa
                .node(use_file_id, component_name_id)
                .to_ident()
                .expect("ident expected");

            let name = sa.interner.intern(&component_name.name);

            if let Some(&variant_idx) = enum_.name_to_value().get(&name) {
                Ok(SymbolKind::EnumVariant(enum_id, variant_idx))
            } else {
                let name = component_name.name.clone();
                sa.report(
                    use_file_id,
                    component.span,
                    ErrorMessage::UnknownEnumVariant(name),
                );
                Err(())
            }
        }

        _ => unreachable!(),
    }
}

fn define_use_target(
    sa: &Sema,
    module_symtables: &mut HashMap<ModuleDefinitionId, SymTable>,
    use_file_id: SourceFileId,
    use_span: Span,
    visibility: Visibility,
    module_id: ModuleDefinitionId,
    ident_id: ast::AstId,
    sym: SymbolKind,
) -> Result<(), ()> {
    let module_symtable = module_symtables
        .get_mut(&module_id)
        .expect("missing tabble");
    let component_name = sa
        .node(use_file_id, ident_id)
        .to_ident()
        .expect("ident expected");
    let name = sa.interner.intern(&component_name.name);

    if let Some(old_sym) = module_symtable.insert_use(name, visibility, sym) {
        report_sym_shadow_span(sa, name, use_file_id, use_span, old_sym);
        Err(())
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
        );
    }
}
