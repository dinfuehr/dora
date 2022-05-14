use crate::language::access::sym_accessible_from;
use crate::language::error::msg::SemError;
use crate::language::report_sym_shadow;
use crate::language::sem_analysis::{module_package, ModuleDefinitionId, SemAnalysis};
use crate::language::sym::{NestedSymTable, Sym};

use dora_parser::ast::{self, UsePathComponent, UsePathComponentValue, UseTargetDescriptor};
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

use super::sem_analysis::SourceFileId;

pub fn check<'a>(sa: &SemAnalysis) {
    for use_elem in &sa.uses {
        let _ = check_use(
            sa,
            &use_elem.ast,
            use_elem.module_id,
            use_elem.file_id,
            None,
        );
    }
}

fn check_use(
    sa: &SemAnalysis,
    use_declaration: &ast::Use,
    use_module_id: ModuleDefinitionId,
    use_file_id: SourceFileId,
    namespace: Option<Sym>,
) -> Result<(), ()> {
    let (start_idx, mut previous_sym) =
        initial_module(sa, use_declaration, use_module_id, use_file_id, namespace)?;

    for (idx, component) in use_declaration
        .common_path
        .iter()
        .enumerate()
        .skip(start_idx)
    {
        if !previous_sym.is_enum() && !previous_sym.is_module() {
            let msg = SemError::ExpectedPath;
            let pos = use_declaration.common_path[idx - 1].pos;
            sa.diag.lock().report(use_file_id, pos, msg);
            return Err(());
        }

        previous_sym = process_component(sa, use_module_id, use_file_id, previous_sym, component)?;
    }

    match &use_declaration.target {
        UseTargetDescriptor::Default => {
            let last_component = use_declaration.common_path.last().expect("no component");

            let name = match last_component.value {
                UsePathComponentValue::Name(name) => name,
                _ => unreachable!(),
            };

            define_use_target(
                sa,
                use_file_id,
                last_component.pos,
                use_module_id,
                name,
                previous_sym,
            )
        }
        UseTargetDescriptor::As(target) => {
            let last_component = use_declaration.common_path.last().expect("no component");

            let name = target.name.expect("target expected");

            define_use_target(
                sa,
                use_file_id,
                last_component.pos,
                use_module_id,
                name,
                previous_sym,
            )
        }
        UseTargetDescriptor::Group(ref targets) => {
            for nested_use in targets {
                check_use(
                    sa,
                    nested_use,
                    use_module_id,
                    use_file_id,
                    Some(previous_sym.clone()),
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
    namespace: Option<Sym>,
) -> Result<(usize, Sym), ()> {
    if let Some(namespace) = namespace {
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
                        SemError::NoSuperModule,
                    );
                    Err(())
                }
            }
            UsePathComponentValue::Name(_) => Ok((0, Sym::Module(use_module_id))),
        }
    } else {
        Err(())
    }
}

fn process_component(
    sa: &SemAnalysis,
    use_module_id: ModuleDefinitionId,
    use_file_id: SourceFileId,
    previous_sym: Sym,
    component: &UsePathComponent,
) -> Result<Sym, ()> {
    let component_name = match component.value {
        UsePathComponentValue::Name(name) => name,
        _ => unreachable!(),
    };

    match previous_sym {
        Sym::Module(module_id) => {
            let symtable = NestedSymTable::new(sa, module_id);
            let current_sym = symtable.get(component_name);

            if let Some(current_sym) = current_sym {
                if sym_accessible_from(sa, current_sym.clone(), use_module_id) {
                    Ok(current_sym)
                } else {
                    let module = &sa.modules[module_id].read();
                    let name = sa.interner.str(component_name).to_string();
                    let msg = SemError::NotAccessibleInModule(module.name(sa), name);
                    sa.diag.lock().report(use_file_id, component.pos, msg);
                    Err(())
                }
            } else {
                let module = sa.modules.idx(module_id);
                let module = module.read();
                let name = sa.interner.str(component_name).to_string();
                let module_name = module.name(sa);
                sa.diag.lock().report(
                    use_file_id,
                    component.pos,
                    SemError::UnknownIdentifierInModule(module_name, name),
                );
                Err(())
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
                    SemError::UnknownEnumVariant(name),
                );
                Err(())
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
) {
    let module = sa.modules.idx(module_id);
    let module = module.read();

    let table = module.table.clone();
    let mut table = table.write();

    if let Some(old_sym) = table.insert(name, sym) {
        report_sym_shadow(sa, name, use_file_id, use_pos, old_sym);
    }
}

#[cfg(test)]
mod tests {
    use crate::language::error::msg::SemError;
    use crate::language::tests::*;

    #[test]
    fn check_initializer() {
        ok("let a: Int32 = 0i32;");
        ok("let a: Int32 = 0i32; var b: Int32 = a + 1i32;");
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
            SemError::NotAccessibleInModule("foo".into(), "bar".into()),
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
            SemError::NotAccessibleInModule("foo::bar".into(), "Foo".into()),
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
            pos(2, 22),
            SemError::NotAccessibleInModule("foo".into(), "bar".into()),
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
            SemError::NotAccessibleInModule("foo".into(), "bar".into()),
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
            SemError::NotAccessibleInModule("foo".into(), "bar".into()),
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
            SemError::NotAccessibleInModule("foo".into(), "Bar".into()),
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
            SemError::NotAccessibleInModule("foo".into(), "Bar".into()),
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
            SemError::NotAccessibleInModule("foo".into(), "Bar".into()),
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
            pos(2, 22),
            SemError::NotAccessibleInModule("foo".into(), "Bar".into()),
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
    #[ignore]
    fn use_keyword_only() {
        err("use self;", pos(1, 1), SemError::ExpectedPath);
        err("use super;", pos(1, 1), SemError::ExpectedPath);
        err("use package;", pos(1, 1), SemError::ExpectedPath);
    }

    #[test]
    #[ignore]
    fn use_keyword_in_middle_of_path() {
        err(
            "use foo::bar::self; mod foo { @pub mod bar {} }",
            pos(1, 1),
            SemError::ExpectedPath,
        );
        err(
            "use foo::bar::super; mod foo { @pub mod bar {} }",
            pos(1, 1),
            SemError::ExpectedPath,
        );
        err(
            "use foo::bar::package; mod foo { @pub mod bar {} }",
            pos(1, 1),
            SemError::ExpectedPath,
        );
    }
}
