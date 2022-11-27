use std::collections::HashSet;

use crate::language::access::sym_accessible_from;
use crate::language::error::msg::ErrorMessage;
use crate::language::report_sym_shadow;
use crate::language::sem_analysis::{module_package, ModuleDefinitionId, SemAnalysis};
use crate::language::sym::{ModuleSymTable, Sym};

use dora_parser::ast::{
    self, NodeId, UsePathComponent, UsePathComponentValue, UseTargetDescriptor,
};
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use super::sem_analysis::SourceFileId;

pub fn check<'a>(sa: &SemAnalysis) {
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
    sa: &SemAnalysis,
    use_declaration: &ast::Use,
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

    for (idx, component) in use_declaration
        .common_path
        .iter()
        .enumerate()
        .skip(start_idx)
    {
        if !previous_sym.is_enum() && !previous_sym.is_module() {
            let msg = ErrorMessage::ExpectedPath;
            let pos = use_declaration.common_path[idx - 1].pos;
            sa.diag.lock().report(use_file_id, pos, msg);
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
        UseTargetDescriptor::Default => {
            let last_component = use_declaration.common_path.last().expect("no component");

            let name = match last_component.value {
                UsePathComponentValue::Name(name) => name,
                UsePathComponentValue::Package
                | UsePathComponentValue::Super
                | UsePathComponentValue::This => {
                    sa.diag.lock().report(
                        use_file_id,
                        last_component.pos,
                        ErrorMessage::ExpectedPath,
                    );
                    return Err(UseError::Fatal);
                }
            };

            assert!(all_resolved.insert((use_file_id, use_declaration.id)));
            *resolved = true;

            define_use_target(
                sa,
                use_file_id,
                last_component.pos,
                use_module_id,
                name,
                previous_sym,
            )?;
        }
        UseTargetDescriptor::As(target) => {
            let last_component = use_declaration.common_path.last().expect("no component");

            let name = target.name.expect("target expected");

            assert!(all_resolved.insert((use_file_id, use_declaration.id)));
            *resolved = true;

            define_use_target(
                sa,
                use_file_id,
                last_component.pos,
                use_module_id,
                name,
                previous_sym,
            )?;
        }
        UseTargetDescriptor::Group(ref group) => {
            if group.targets.is_empty() {
                sa.diag
                    .lock()
                    .report(use_file_id, group.pos, ErrorMessage::ExpectedPath);
                return Err(UseError::Fatal);
            }

            for nested_use in &group.targets {
                check_use(
                    sa,
                    nested_use,
                    use_module_id,
                    use_file_id,
                    Some(previous_sym.clone()),
                    ignore_errors,
                    all_resolved,
                    resolved,
                )?;
            }
        }
    }

    Ok(())
}

fn initial_module(
    sa: &SemAnalysis,
    use_declaration: &ast::Use,
    use_module_id: ModuleDefinitionId,
    use_file_id: SourceFileId,
    previous_sym: Option<Sym>,
) -> Result<(usize, Sym), UseError> {
    if let Some(namespace) = previous_sym {
        return Ok((0, namespace));
    }

    if let Some(first_component) = use_declaration.common_path.first() {
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
                    sa.diag.lock().report(
                        use_file_id.into(),
                        first_component.pos,
                        ErrorMessage::NoSuperModule,
                    );
                    Err(UseError::Fatal)
                }
            }
            UsePathComponentValue::Name(name) => {
                if let Some(package_id) = sa.package_names.get(&name).cloned() {
                    let package = sa.packages.idx(package_id);
                    let package = package.read();

                    Ok((1, Sym::Module(package.top_level_module_id())))
                } else {
                    Ok((0, Sym::Module(use_module_id)))
                }
            }
        }
    } else {
        Err(UseError::Fatal)
    }
}

fn process_component(
    sa: &SemAnalysis,
    use_module_id: ModuleDefinitionId,
    use_file_id: SourceFileId,
    previous_sym: Sym,
    component: &UsePathComponent,
    ignore_errors: bool,
) -> Result<Sym, UseError> {
    let component_name = match component.value {
        UsePathComponentValue::Name(name) => name,
        UsePathComponentValue::Package
        | UsePathComponentValue::Super
        | UsePathComponentValue::This => {
            sa.diag
                .lock()
                .report(use_file_id, component.pos, ErrorMessage::ExpectedPath);
            return Err(UseError::Fatal);
        }
    };

    match previous_sym {
        Sym::Module(module_id) => {
            let symtable = ModuleSymTable::new(sa, module_id);
            let current_sym = symtable.get(component_name);

            if let Some(current_sym) = current_sym {
                if sym_accessible_from(sa, current_sym.clone(), use_module_id) {
                    Ok(current_sym)
                } else {
                    let module = &sa.modules[module_id].read();
                    let name = sa.interner.str(component_name).to_string();
                    let msg = ErrorMessage::NotAccessibleInModule(module.name(sa), name);
                    sa.diag.lock().report(use_file_id, component.pos, msg);
                    Err(UseError::Fatal)
                }
            } else if ignore_errors {
                Err(UseError::Unresolved)
            } else {
                let module = sa.modules.idx(module_id);
                let module = module.read();
                let name = sa.interner.str(component_name).to_string();
                let module_name = module.name(sa);
                sa.diag.lock().report(
                    use_file_id,
                    component.pos,
                    ErrorMessage::UnknownIdentifierInModule(module_name, name),
                );
                Err(UseError::Unresolved)
            }
        }

        Sym::Enum(enum_id) => {
            let enum_ = sa.enums[enum_id].read();

            if let Some(&variant_idx) = enum_.name_to_value.get(&component_name) {
                Ok(Sym::EnumVariant(enum_id, variant_idx as usize))
            } else {
                let name = sa.interner.str(component_name).to_string();
                sa.diag.lock().report(
                    use_file_id,
                    component.pos,
                    ErrorMessage::UnknownEnumVariant(name),
                );
                Err(UseError::Fatal)
            }
        }

        _ => unreachable!(),
    }
}

fn define_use_target(
    sa: &SemAnalysis,
    use_file_id: SourceFileId,
    use_pos: Position,
    module_id: ModuleDefinitionId,
    name: Name,
    sym: Sym,
) -> Result<(), UseError> {
    let module = sa.modules.idx(module_id);
    let module = module.read();

    let table = module.table.clone();
    let mut table = table.write();

    if let Some(old_sym) = table.insert(name, sym) {
        report_sym_shadow(sa, name, use_file_id, use_pos, old_sym);
        Err(UseError::Fatal)
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::language::error::msg::ErrorMessage;
    use crate::language::tests::*;

    #[test]
    fn check_initializer() {
        ok("let a: Int32 = 0i32;");
        ok("let a: Int32 = 0i32; var b: Int32 = a + 1i32;");
        err(
            "var a: Int32 = foo;",
            pos(1, 16),
            ErrorMessage::UnknownIdentifier("foo".into()),
        );
    }

    #[test]
    fn check_type() {
        err(
            "var x: Foo = 0;",
            pos(1, 8),
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
            pos(2, 22),
            ErrorMessage::NotAccessibleInModule("foo".into(), "bar".into()),
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
            pos(2, 27),
            ErrorMessage::NotAccessibleInModule("foo::bar".into(), "Foo".into()),
        );
    }

    #[test]
    fn use_fct() {
        err(
            "
            use foo::bar;
            mod foo {
                fun bar(): Unit {}
            }
        ",
            pos(2, 22),
            ErrorMessage::NotAccessibleInModule("foo".into(), "bar".into()),
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
            pos(2, 22),
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
            pos(2, 22),
            ErrorMessage::NotAccessibleInModule("foo".into(), "bar".into()),
        );
    }

    #[test]
    fn use_enum() {
        err(
            "
            use foo::Bar;
            @pub mod foo {
                enum Bar { A, B, C }
            }
        ",
            pos(2, 22),
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
            pos(2, 22),
            ErrorMessage::NotAccessibleInModule("foo".into(), "Bar".into()),
        );

        ok("
            use foo::Bar::{A, B, C};
            mod foo {
                @pub enum Bar { A, B, C }
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
            pos(2, 22),
            ErrorMessage::NotAccessibleInModule("foo".into(), "Bar".into()),
        );
    }

    #[test]
    fn use_value() {
        ok("
            use foo::Bar;
            mod foo {
                @pub value Bar { f: Int32 }
            }
        ");

        err(
            "
            use foo::Bar;
            mod foo {
                value Bar { f: Int32 }
            }
        ",
            pos(2, 22),
            ErrorMessage::NotAccessibleInModule("foo".into(), "Bar".into()),
        );
    }

    #[test]
    fn use_public() {
        ok("
            @pub use foo::Bar;
            @pub mod foo {
                @pub enum Bar { A, B, C }
            }
        ");
    }

    #[test]
    fn use_keyword_only() {
        err("use self;", pos(1, 5), ErrorMessage::ExpectedPath);
        err("use package;", pos(1, 5), ErrorMessage::ExpectedPath);
        err(
            "mod foo { use super; }",
            pos(1, 15),
            ErrorMessage::ExpectedPath,
        );
    }

    #[test]
    fn use_keyword_in_path() {
        err(
            "use foo::bar::self; mod foo { @pub mod bar {} }",
            pos(1, 15),
            ErrorMessage::ExpectedPath,
        );
        err(
            "use foo::bar::super; mod foo { @pub mod bar {} }",
            pos(1, 15),
            ErrorMessage::ExpectedPath,
        );
        err(
            "use foo::bar::package; mod foo { @pub mod bar {} }",
            pos(1, 15),
            ErrorMessage::ExpectedPath,
        );
    }

    #[test]
    fn no_use_targets() {
        err(
            "use foo::bar:: {}; mod foo { @pub mod bar {} }",
            pos(1, 16),
            ErrorMessage::ExpectedPath,
        );
    }

    #[test]
    fn use_zig_zag() {
        ok("
            @pub use foo::f1 as f2;
            @pub use foo::f3 as f4;

            mod foo {
                @pub use super::f2 as f3;
                @pub use super::f4 as f5;

                @pub fun f1(): Unit {}
            }
        ");
    }

    #[test]
    fn use_cyclic() {
        errors(
            "
            @pub use foo::f1 as f2;

            mod foo {
                @pub use super::f2 as f1;
            }
        ",
            &[
                (
                    pos(2, 27),
                    ErrorMessage::UnknownIdentifierInModule("foo".into(), "f1".into()),
                ),
                (
                    pos(5, 33),
                    ErrorMessage::UnknownIdentifierInModule("".into(), "f2".into()),
                ),
            ],
        );
    }
}
