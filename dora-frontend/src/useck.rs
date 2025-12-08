use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use crate::access::{sym_accessible_from, use_accessible_from};
use crate::error::msg::ErrorMessage;
use crate::report_sym_shadow_span;
use crate::sema::{ModuleDefinitionId, Sema, Visibility, module_package};
use crate::sym::{SymTable, SymbolKind};

use dora_parser::ast::{self, SyntaxNodeBase};
use dora_parser::{Span, TokenKind};

use super::sema::SourceFileId;

pub fn check<'a>(sa: &Sema, mut module_symtables: HashMap<ModuleDefinitionId, SymTable>) {
    let mut processed_uses = HashSet::<(SourceFileId, ast::SyntaxNodePtr)>::new();

    while {
        let mut did_resolve_symbol = false;

        for (_id, use_definition) in &sa.uses {
            let mut checker = UseChecker {
                sa,
                module_symtables: &mut module_symtables,
                processed_uses: &mut processed_uses,
                module_id: use_definition.module_id,
                file_id: use_definition.file_id,
                visibility: use_definition.visibility,
                ignore_unknown_symbols: true,
                did_resolve_symbol: &mut did_resolve_symbol,
            };

            let _ = checker.check_use(use_definition.ast(sa).path(), None);
        }

        did_resolve_symbol
    } {}

    for (_id, use_definition) in &sa.uses {
        let mut checker = UseChecker {
            sa,
            module_symtables: &mut module_symtables,
            processed_uses: &mut processed_uses,
            module_id: use_definition.module_id,
            file_id: use_definition.file_id,
            visibility: use_definition.visibility,
            ignore_unknown_symbols: false,
            did_resolve_symbol: &mut false,
        };

        let _ = checker.check_use(use_definition.ast(sa).path(), None);
    }

    for (module_id, table) in module_symtables {
        assert!(sa.module(module_id).table.set(Rc::new(table)).is_ok());
    }
}

struct UseChecker<'a> {
    sa: &'a Sema,
    module_symtables: &'a mut HashMap<ModuleDefinitionId, SymTable>,
    processed_uses: &'a mut HashSet<(SourceFileId, ast::SyntaxNodePtr)>,
    did_resolve_symbol: &'a mut bool,
    module_id: ModuleDefinitionId,
    file_id: SourceFileId,
    visibility: Visibility,
    ignore_unknown_symbols: bool,
}

impl<'a> UseChecker<'a> {
    fn check_use(
        &mut self,
        use_path: ast::AstUsePath,
        previous_sym: Option<SymbolKind>,
    ) -> Result<(), ()> {
        if self
            .processed_uses
            .contains(&(self.file_id, use_path.as_ptr()))
        {
            return Ok(());
        }

        let (start_idx, mut previous_sym) = if let Some(previous_sym) = previous_sym {
            (0, previous_sym)
        } else {
            self.initial_module(&use_path)?
        };

        assert!(previous_sym.is_module() || previous_sym.is_enum());
        let mut previous_span = Span::new(1, 1);

        for component in use_path.path().skip(start_idx) {
            previous_sym =
                self.process_component(&use_path, previous_sym, previous_span, &component)?;
            previous_span = component.span();
        }

        let target = use_path.target();

        match target {
            ast::AstUseTarget::UseAtom(component) => {
                let sym =
                    self.process_component(&use_path, previous_sym, previous_span, &component)?;

                assert!(
                    self.processed_uses
                        .insert((self.file_id, use_path.as_ptr()))
                );
                *self.did_resolve_symbol = true;

                let name = component.to_name().expect("ident expected");

                self.define_use_target(component.span(), name, sym)?;
            }
            ast::AstUseTarget::UseAs(use_as) => {
                let original_name = use_as.original_name();

                let sym =
                    self.process_component(&use_path, previous_sym, previous_span, &original_name)?;

                assert!(
                    self.processed_uses
                        .insert((self.file_id, use_path.as_ptr()))
                );

                if let Some(ident) = use_as.target_name() {
                    *self.did_resolve_symbol = true;
                    self.define_use_target(original_name.span(), ident, sym)?;
                }
            }
            ast::AstUseTarget::UseGroup(group) => {
                if group.targets().count() == 0 {
                    self.sa
                        .report(self.file_id, group.span(), ErrorMessage::ExpectedPath);
                    assert!(
                        self.processed_uses
                            .insert((self.file_id, use_path.as_ptr()))
                    );
                    return Err(());
                }

                for nested_use in group.targets() {
                    // Ignore errors as an error in `foo::{a, b, c}`
                    // for `a` does not affect `b` or `c`.
                    let _ = self.check_use(nested_use, Some(previous_sym.clone()));
                }
            }

            ast::AstUseTarget::Error(..) => {
                assert!(
                    self.processed_uses
                        .insert((self.file_id, use_path.as_ptr()))
                );
            }
        }

        Ok(())
    }

    fn initial_module(&mut self, use_path: &ast::AstUsePath) -> Result<(usize, SymbolKind), ()> {
        if let Some(first_component) = use_path.path().next().or_else(|| match use_path.target() {
            ast::AstUseTarget::UseAtom(atom) => Some(atom),
            _ => None,
        }) {
            match first_component.kind() {
                TokenKind::SELF_KW => Ok((1, SymbolKind::Module(self.module_id))),
                TokenKind::PACKAGE_KW => Ok((
                    1,
                    SymbolKind::Module(module_package(self.sa, self.module_id)),
                )),
                TokenKind::SUPER_KW => {
                    let module = self.sa.module(self.module_id);
                    if let Some(module_id) = module.parent_module_id {
                        Ok((1, SymbolKind::Module(module_id)))
                    } else {
                        self.sa.report(
                            self.file_id.into(),
                            first_component.span(),
                            ErrorMessage::NoSuperModule,
                        );
                        assert!(
                            self.processed_uses
                                .insert((self.file_id, use_path.as_ptr()))
                        );
                        Err(())
                    }
                }
                TokenKind::NAME => {
                    let ident = first_component.to_name().expect("ident expected");

                    if let Some(package_id) =
                        self.sa.package_names.get(ident.token().text()).cloned()
                    {
                        Ok((
                            1,
                            SymbolKind::Module(self.sa.packages[package_id].top_level_module_id()),
                        ))
                    } else {
                        Ok((0, SymbolKind::Module(self.module_id)))
                    }
                }
                _ => unreachable!(),
            }
        } else {
            Ok((0, SymbolKind::Module(self.module_id)))
        }
    }

    fn process_component(
        &mut self,
        use_path: &ast::AstUsePath,
        previous_sym: SymbolKind,
        previous_span: Span,
        component: &ast::AstUseAtom,
    ) -> Result<SymbolKind, ()> {
        if !previous_sym.is_enum() && !previous_sym.is_module() {
            let msg = ErrorMessage::ExpectedPath;
            self.sa.report(self.file_id, previous_span, msg);
            assert!(
                self.processed_uses
                    .insert((self.file_id, use_path.as_ptr()))
            );
            return Err(());
        }

        let component_name = if let Some(ident) = component.to_name() {
            ident
        } else {
            self.sa
                .report(self.file_id, component.span(), ErrorMessage::ExpectedPath);
            assert!(
                self.processed_uses
                    .insert((self.file_id, use_path.as_ptr()))
            );
            return Err(());
        };

        let name = self.sa.interner.intern(component_name.token().text());

        match previous_sym {
            SymbolKind::Module(module_id) => {
                let symtable = self
                    .module_symtables
                    .get(&module_id)
                    .expect("missing symtable");

                let current_sym = symtable.get_sym(name);

                if let Some(current_sym) = current_sym {
                    if let Some(visibility) = current_sym.visibility() {
                        if !use_accessible_from(
                            self.sa,
                            module_id,
                            visibility.to_owned(),
                            self.module_id,
                        ) {
                            let msg = ErrorMessage::UseNotAccessible;
                            self.sa.report(self.file_id, component.span(), msg);
                            assert!(
                                self.processed_uses
                                    .insert((self.file_id, use_path.as_ptr()))
                            );
                            return Err(());
                        }
                    }

                    if sym_accessible_from(self.sa, current_sym.kind().to_owned(), self.module_id) {
                        Ok(current_sym.kind().to_owned())
                    } else {
                        let module = self.sa.module(module_id);
                        let name = component_name.token_as_string();
                        let msg = ErrorMessage::NotAccessibleInModule(module.name(self.sa), name);
                        assert!(
                            self.processed_uses
                                .insert((self.file_id, use_path.as_ptr()))
                        );
                        self.sa.report(self.file_id, component.span(), msg);
                        Err(())
                    }
                } else if self.ignore_unknown_symbols {
                    Err(())
                } else {
                    let module = self.sa.module(module_id);
                    let name = component_name.token_as_string();
                    let module_name = module.name(self.sa);
                    self.sa.report(
                        self.file_id,
                        component.span(),
                        ErrorMessage::UnknownIdentifierInModule(module_name, name),
                    );
                    Err(())
                }
            }

            SymbolKind::Enum(enum_id) => {
                let enum_ = self.sa.enum_(enum_id);

                if let Some(&variant_idx) = enum_.name_to_value().get(&name) {
                    Ok(SymbolKind::EnumVariant(enum_id, variant_idx))
                } else {
                    let name = component_name.token_as_string();
                    self.sa.report(
                        self.file_id,
                        component.span(),
                        ErrorMessage::UnknownEnumVariant(name),
                    );
                    Err(())
                }
            }

            _ => {
                println!("previous_sym = {:?}", previous_sym);
                unreachable!()
            }
        }
    }

    fn define_use_target(
        &mut self,
        use_span: Span,
        ident: ast::AstName,
        sym: SymbolKind,
    ) -> Result<(), ()> {
        let module_symtable = self
            .module_symtables
            .get_mut(&self.module_id)
            .expect("missing tabble");
        let name = self.sa.interner.intern(ident.token().text());

        if let Some(old_sym) = module_symtable.insert_use(name, self.visibility, sym) {
            report_sym_shadow_span(self.sa, name, self.file_id, use_span, old_sym);
            Err(())
        } else {
            Ok(())
        }
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

        err("use self as foo;", (1, 5), ErrorMessage::ExpectedPath);
        err("use package as foo;", (1, 5), ErrorMessage::ExpectedPath);
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
