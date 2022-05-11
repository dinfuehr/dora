use crate::language::access::{module_accessible_from, sym_accessible_from};
use crate::language::error::msg::SemError;
use crate::language::report_sym_shadow;
use crate::language::sem_analysis::{module_package, ModuleDefinitionId, SemAnalysis};
use crate::language::sym::{NestedSymTable, Sym};

use dora_parser::ast::{
    self, UsePathComponent, UsePathComponentValue, UseTargetDescriptor, UseTargetName,
};
use dora_parser::lexer::position::Position;

use super::sem_analysis::SourceFileId;

pub fn check<'a>(sa: &SemAnalysis) {
    let mut all_uses = Vec::new();

    for use_def in &sa.uses {
        create_imports(
            use_def.module_id,
            use_def.file_id,
            &use_def.ast,
            &mut all_uses,
            &[],
        );
    }

    for use_decl in all_uses {
        check_use(sa, &use_decl);
    }
}

fn check_use_new(
    _sa: &SemAnalysis,
    _se_declaration: &ast::Use,
    _use_module_id: ModuleDefinitionId,
    _se_file_id: SourceFileId,
) {
    unimplemented!()
}

fn create_imports(
    module_id: ModuleDefinitionId,
    file_id: SourceFileId,
    use_declaration: &ast::Use,
    all_uses: &mut Vec<Import>,
    outer_path: &[UsePathComponent],
) {
    let mut path = outer_path.to_vec();
    path.extend_from_slice(&use_declaration.common_path);

    match use_declaration.target {
        UseTargetDescriptor::Default => {
            let last_component = use_declaration.common_path.last().expect("no component");

            all_uses.push(Import {
                path,
                target: None,
                module_id,
                file_id,
                pos: last_component.pos,
            })
        }

        UseTargetDescriptor::As(ref target_rename) => all_uses.push(Import {
            path,
            target: Some(target_rename.clone()),
            module_id,
            file_id,
            pos: target_rename.pos,
        }),

        UseTargetDescriptor::Group(ref group) => {
            for use_declaration in group {
                create_imports(module_id, file_id, use_declaration, all_uses, &path);
            }
        }
    }
}

struct Import {
    path: Vec<UsePathComponent>,
    target: Option<UseTargetName>,
    module_id: ModuleDefinitionId,
    file_id: SourceFileId,
    pos: Position,
}

fn check_use(sa: &SemAnalysis, use_decl: &Import) {
    let first_component = use_decl
        .path
        .first()
        .expect("there should always be at least one component");

    let (start_idx, module_id) = match first_component.value {
        UsePathComponentValue::This => (1, use_decl.module_id),
        UsePathComponentValue::Package => (1, module_package(sa, use_decl.module_id)),
        UsePathComponentValue::Super => {
            let module = &sa.modules[use_decl.module_id].read();
            if let Some(module_id) = module.parent_module_id {
                (1, module_id)
            } else {
                sa.diag.lock().report(
                    use_decl.file_id.into(),
                    first_component.pos,
                    SemError::NoSuperModule,
                );
                return;
            }
        }
        UsePathComponentValue::Name(_) => (0, use_decl.module_id),
    };

    let sym = match read_path(sa, use_decl, start_idx, module_id) {
        Ok(sym) => sym,
        Err(()) => {
            return;
        }
    };

    let module = sa.modules.idx(use_decl.module_id);
    let module = module.read();

    let table = module.table.clone();
    let mut table = table.write();

    let target_name = match use_decl.target {
        Some(ref target) => target.name.expect("name expected"),
        None => match use_decl.path.last().expect("path expected").value {
            UsePathComponentValue::Name(name) => name,
            _ => unreachable!(),
        },
    };

    if let Some(old_sym) = table.insert(target_name, sym) {
        report_sym_shadow(sa, target_name, use_decl.file_id, use_decl.pos, old_sym);
    }
}

fn read_path(
    sa: &SemAnalysis,
    use_decl: &Import,
    start_idx: usize,
    module_id: ModuleDefinitionId,
) -> Result<Sym, ()> {
    let mut previous_sym = Sym::Module(module_id);
    debug_assert!(module_accessible_from(sa, module_id, use_decl.module_id));

    for (idx, component) in use_decl.path.iter().enumerate().skip(start_idx) {
        if !previous_sym.is_enum() && !previous_sym.is_module() {
            let msg = SemError::ExpectedPath;
            let pos = use_decl.path[idx - 1].pos;
            sa.diag.lock().report(use_decl.file_id, pos, msg);
            return Err(());
        }

        previous_sym = read_path_component(sa, use_decl, previous_sym, component)?;
    }

    Ok(previous_sym)
}

fn read_path_component(
    sa: &SemAnalysis,
    use_decl: &Import,
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
                if sym_accessible_from(sa, current_sym.clone(), use_decl.module_id) {
                    Ok(current_sym)
                } else {
                    let module = &sa.modules[module_id].read();
                    let name = sa.interner.str(component_name).to_string();
                    let msg = SemError::NotAccessibleInModule(module.name(sa), name);
                    sa.diag.lock().report(use_decl.file_id, component.pos, msg);
                    Err(())
                }
            } else {
                let module = sa.modules.idx(module_id);
                let module = module.read();
                let name = sa.interner.str(component_name).to_string();
                let module_name = module.name(sa);
                sa.diag.lock().report(
                    use_decl.file_id,
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
                    use_decl.file_id.into(),
                    component.pos,
                    SemError::UnknownEnumVariant(name),
                );
                Err(())
            }
        }

        _ => unreachable!(),
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
