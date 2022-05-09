use crate::language::access::{module_accessible_from, sym_accessible_from};
use crate::language::error::msg::SemError;
use crate::language::report_sym_shadow;
use crate::language::sem_analysis::{module_package, ModuleDefinitionId, SemAnalysis};
use crate::language::sym::{NestedSymTable, Sym};

use dora_parser::ast::{UseContext, UsePathComponent};
use dora_parser::lexer::position::Position;

use super::sem_analysis::SourceFileId;

pub fn check<'a>(sa: &SemAnalysis) {
    let mut all_uses = Vec::new();

    for use_def in &sa.uses {
        for mapping in &use_def.ast.mappings {
            let mut path = use_def.ast.path.clone();
            path.push(mapping.element_name.clone());
            all_uses.push(Import {
                context: use_def.ast.context.clone(),
                path,
                target: mapping
                    .target_name
                    .clone()
                    .unwrap_or(mapping.element_name.clone()),
                module_id: use_def.module_id,
                file_id: use_def.file_id,
                pos: mapping.pos,
            });
        }
    }

    for use_decl in all_uses {
        check_use(sa, &use_decl);
    }
}

struct Import {
    context: UseContext,
    path: Vec<UsePathComponent>,
    target: UsePathComponent,
    module_id: ModuleDefinitionId,
    file_id: SourceFileId,
    pos: Position,
}

fn check_use(sa: &SemAnalysis, use_decl: &Import) {
    let module_id = match use_decl.context {
        UseContext::This => use_decl.module_id,
        UseContext::Package => module_package(sa, use_decl.module_id),
        UseContext::Super => {
            let module = &sa.modules[use_decl.module_id].read();
            if let Some(module_id) = module.parent_module_id {
                module_id
            } else {
                sa.diag.lock().report(
                    use_decl.file_id.into(),
                    use_decl.pos,
                    SemError::NoSuperModule,
                );
                return;
            }
        }
    };

    let sym = match read_path(sa, use_decl, module_id) {
        Ok(sym) => sym,
        Err(()) => {
            return;
        }
    };

    let module = sa.modules.idx(use_decl.module_id);
    let module = module.read();

    let table = module.table.clone();
    let mut table = table.write();

    if let Some(old_sym) = table.insert(use_decl.target.name, sym) {
        report_sym_shadow(
            sa,
            use_decl.target.name,
            use_decl.file_id,
            use_decl.pos,
            old_sym,
        );
    }
}

fn read_path(
    sa: &SemAnalysis,
    use_decl: &Import,
    module_id: ModuleDefinitionId,
) -> Result<Sym, ()> {
    let mut previous_sym = Sym::Module(module_id);
    debug_assert!(module_accessible_from(sa, module_id, use_decl.module_id));

    for (idx, component) in use_decl.path.iter().enumerate() {
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
    match previous_sym {
        Sym::Module(module_id) => {
            let symtable = NestedSymTable::new(sa, module_id);
            let current_sym = symtable.get(component.name);

            if let Some(current_sym) = current_sym {
                if sym_accessible_from(sa, current_sym.clone(), use_decl.module_id) {
                    Ok(current_sym)
                } else {
                    let module = &sa.modules[module_id].read();
                    let name = sa.interner.str(component.name).to_string();
                    let msg = SemError::NotAccessibleInModule(module.name(sa), name);
                    sa.diag.lock().report(use_decl.file_id, component.pos, msg);
                    Err(())
                }
            } else {
                let module = sa.modules.idx(module_id);
                let module = module.read();
                let name = sa.interner.str(component.name).to_string();
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

            if let Some(&variant_idx) = enum_.name_to_value.get(&component.name) {
                Ok(Sym::EnumVariant(enum_id, variant_idx as usize))
            } else {
                let name = sa.interner.str(component.name).to_string();
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
}
