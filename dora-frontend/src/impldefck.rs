use std::collections::{HashMap, HashSet};

use crate::extensiondefck::check_for_unconstrained_type_params;
use crate::sema::{
    params_match, AliasDefinitionId, FctDefinition, FctDefinitionId, ImplDefinition,
    ImplDefinitionId, Sema, SourceFileId, TraitDefinition,
};
use crate::{
    read_type_context, AllowSelf, ErrorMessage, ModuleSymTable, SourceType, SymbolKind,
    TypeParamContext,
};

use dora_parser::ast;

pub fn check_definition(sa: &Sema) {
    for (id, impl_) in sa.impls.iter() {
        let mut implck = ImplCheck {
            sa,
            file_id: impl_.file_id,
            impl_id: id,
            sym: ModuleSymTable::new(sa, impl_.module_id),
            ast: &impl_.ast,
        };

        implck.check();
    }
}

struct ImplCheck<'x> {
    sa: &'x Sema,
    file_id: SourceFileId,
    impl_id: ImplDefinitionId,
    sym: ModuleSymTable,
    ast: &'x ast::Impl,
}

impl<'x> ImplCheck<'x> {
    fn check(&mut self) {
        assert!(self.ast.trait_type.is_some());

        self.sym.push_level();

        {
            let impl_ = &self.sa.impls[self.impl_id];

            for (id, name) in impl_.type_params().names() {
                self.sym.insert(name, SymbolKind::TypeParam(id));
            }
        }

        let impl_ = &self.sa.impls[self.impl_id];

        let ast_trait_type = self.ast.trait_type.as_ref().unwrap();

        let trait_ty = if let Some(trait_ty) = read_type_context(
            self.sa,
            &self.sym,
            self.file_id.into(),
            ast_trait_type,
            TypeParamContext::Impl(&*impl_),
            AllowSelf::No,
        ) {
            match trait_ty {
                SourceType::Trait(trait_id, type_params) => {
                    SourceType::Trait(trait_id, type_params)
                }

                _ => {
                    self.sa
                        .report(self.file_id, self.ast.span, ErrorMessage::ExpectedTrait);
                    SourceType::Error
                }
            }
        } else {
            SourceType::Error
        };

        assert!(impl_.trait_ty.set(trait_ty).is_ok());

        let extended_ty = if let Some(class_ty) = read_type_context(
            self.sa,
            &self.sym,
            self.file_id.into(),
            &self.ast.extended_type,
            TypeParamContext::Impl(&*impl_),
            AllowSelf::No,
        ) {
            if class_ty.is_cls()
                || class_ty.is_struct()
                || class_ty.is_enum()
                || class_ty.is_primitive()
                || class_ty.is_tuple_or_unit()
            {
                check_for_unconstrained_type_params(
                    self.sa,
                    class_ty.clone(),
                    impl_.type_params(),
                    self.file_id,
                    self.ast.span,
                );

                class_ty
            } else {
                self.sa.report(
                    self.file_id,
                    self.ast.extended_type.span(),
                    ErrorMessage::ExpectedImplTraitType,
                );

                SourceType::Error
            }
        } else {
            SourceType::Error
        };

        assert!(impl_.extended_ty.set(extended_ty).is_ok());

        self.sym.pop_level();
    }
}

pub fn check_body(sa: &Sema) {
    for (_id, impl_) in sa.impls.iter() {
        check_impl_body(sa, impl_);
    }
}

fn check_impl_body(sa: &Sema, impl_: &ImplDefinition) {
    let trait_ = &sa.traits[impl_.trait_id()];

    check_impl_methods(sa, impl_, trait_);
    check_impl_types(sa, impl_, trait_);
}

fn check_impl_methods(sa: &Sema, impl_: &ImplDefinition, trait_: &TraitDefinition) {
    let mut remaining_trait_methods: HashSet<FctDefinitionId> =
        trait_.methods().iter().cloned().collect();
    let mut trait_method_map = HashMap::new();

    for &impl_method_id in impl_.methods() {
        let impl_method = &sa.fcts[impl_method_id];

        if impl_method.ast.block.is_none() && !impl_method.is_internal {
            sa.report(
                impl_method.file_id.into(),
                impl_method.span,
                ErrorMessage::MissingFctBody,
            );
        }

        if let Some(trait_method_id) = trait_.get_method(impl_method.name, impl_method.is_static) {
            if let Some(existing_id) = trait_method_map.insert(trait_method_id, impl_method_id) {
                let existing_fct = &sa.fcts[existing_id];
                let method_name = sa.interner.str(existing_fct.name).to_string();

                sa.report(
                    impl_method.file_id,
                    impl_method.ast.span,
                    ErrorMessage::AliasExists(method_name, existing_fct.span),
                );
            }

            remaining_trait_methods.remove(&trait_method_id);

            check_impl_method(sa, impl_, impl_method, trait_method_id);
        } else {
            sa.report(
                impl_.file_id,
                impl_method.span,
                ErrorMessage::ElementNotInTrait,
            )
        }
    }

    if !remaining_trait_methods.is_empty() {
        report_missing_methods(sa, impl_, trait_, remaining_trait_methods);
    }

    assert!(impl_.trait_method_map.set(trait_method_map).is_ok());
}

fn check_impl_method(
    sa: &Sema,
    impl_: &ImplDefinition,
    impl_method: &FctDefinition,
    trait_method_id: FctDefinitionId,
) {
    let trait_method = &sa.fcts[trait_method_id];

    let params_match = params_match(
        Some(impl_.extended_ty().clone()),
        trait_method.params_without_self(),
        impl_method.params_without_self(),
    );

    let return_type_valid = impl_method.return_type()
        == if trait_method.return_type().is_self() {
            impl_.extended_ty()
        } else {
            trait_method.return_type()
        };

    if !return_type_valid || !params_match {
        let msg = ErrorMessage::ImplMethodTypeMismatch;
        sa.report(impl_.file_id, impl_method.span, msg);
    }
}

fn report_missing_methods(
    sa: &Sema,
    impl_: &ImplDefinition,
    _trait: &TraitDefinition,
    missing_methods: HashSet<FctDefinitionId>,
) {
    for method_id in missing_methods {
        let method = &sa.fcts[method_id];
        let mtd_name = sa.interner.str(method.name).to_string();

        sa.report(
            impl_.file_id,
            impl_.span,
            ErrorMessage::ElementNotInImpl(mtd_name),
        )
    }
}

fn check_impl_types(sa: &Sema, impl_: &ImplDefinition, trait_: &TraitDefinition) {
    let mut remaining_aliases: HashSet<AliasDefinitionId> =
        trait_.aliases().iter().cloned().collect();
    let mut trait_alias_map = HashMap::new();

    for &impl_alias_id in impl_.aliases() {
        let impl_alias = &sa.aliases[impl_alias_id];

        if let Some(&trait_alias_id) = trait_.alias_names().get(&impl_alias.name) {
            if let Some(existing_id) = trait_alias_map.insert(trait_alias_id, trait_alias_id) {
                let existing_alias = &sa.aliases[existing_id];
                let method_name = sa.interner.str(existing_alias.name).to_string();

                sa.report(
                    impl_alias.file_id,
                    impl_alias.node.span,
                    ErrorMessage::AliasExists(method_name, existing_alias.node.span),
                );
            }

            remaining_aliases.remove(&trait_alias_id);
        } else {
            sa.report(
                impl_.file_id,
                impl_alias.node.span,
                ErrorMessage::ElementNotInTrait,
            )
        }
    }

    assert!(impl_.trait_alias_map.set(trait_alias_map).is_ok());
}

#[cfg(test)]
mod tests {
    use crate::error::msg::ErrorMessage;
    use crate::tests::*;
    use dora_parser::Span;

    #[test]
    fn impl_method_without_body() {
        err(
            "
            trait Foo {
                fn foo(): Int32;
            }
            class Bar
            impl Foo for Bar { fn foo(): Int32; }",
            (6, 32),
            ErrorMessage::MissingFctBody,
        );
    }

    #[test]
    fn impl_method_defined_twice() {
        err(
            "
            trait Foo {
                fn foo(): Int32;
            }
            class Bar
            impl Foo for Bar {
                fn foo(): Int32 { return 0; }
                fn foo(): Int32 { return 1; }
            }",
            (8, 17),
            ErrorMessage::AliasExists("foo".into(), Span::new(141, 29)),
        );
    }

    #[test]
    fn impl_for_unknown_trait() {
        err(
            "class A impl Foo for A {}",
            (1, 14),
            ErrorMessage::UnknownIdentifier("Foo".into()),
        );
    }

    #[test]
    fn impl_for_unknown_class() {
        err(
            "trait Foo {} impl Foo for A {}",
            (1, 27),
            ErrorMessage::UnknownIdentifier("A".into()),
        );

        err(
            "trait Foo {} trait A {} impl Foo for A {}",
            (1, 38),
            ErrorMessage::ExpectedImplTraitType,
        );
    }

    #[test]
    fn impl_definitions() {
        ok("trait Foo {} class A {} impl Foo for A {}");
        ok("trait Foo { fn toBool(): Bool; }
            class A {}
            impl Foo for A { fn toBool(): Bool { return false; } }");
    }

    #[test]
    fn impl_struct() {
        ok("
            trait Foo {}
            struct A(x: Int32)
            impl Foo for A {}
        ");
        ok("
            trait Foo {}
            struct A[T](x: Int32)
            impl Foo for A[Int32] {}
            impl Foo for A[Float32] {}
        ");
        ok("
            trait Foo {}
            struct A[T](x: Int32)
            impl[T] Foo for A[T] {}
        ");
    }

    #[test]
    fn impl_enum() {
        ok("
            trait Foo {}
            enum A { B, C }
            impl Foo for A {}
        ");
        ok("
            trait Foo {}
            enum A[T] { A, B }
            impl Foo for A[Int32] {}
            impl Foo for A[Float32] {}
        ");
        ok("
            trait Foo {}
            enum A[T] { A, B }
            impl[T] Foo for A[T] {}
        ");
    }

    #[test]
    fn impl_class_type_params() {
        ok("trait MyTrait {} class Foo[T] impl MyTrait for Foo[String] {}");
    }

    #[test]
    fn impl_unconstrained_type_param() {
        err(
            "
            struct MyFoo[T]
            trait MyTrait {}
            impl[T] MyTrait for MyFoo[Int32] {}
        ",
            (4, 13),
            ErrorMessage::UnconstrainedTypeParam("T".into()),
        );
    }

    #[test]
    fn impl_mod() {
        err(
            "
            mod foo { trait MyTrait {} }
            class Foo
            impl foo::MyTrait for Foo {}",
            (4, 18),
            ErrorMessage::NotAccessible("foo::MyTrait".into()),
        );

        err(
            "
            mod foo { class Foo }
            trait MyTrait {}
            impl MyTrait for foo::Foo {}",
            (4, 30),
            ErrorMessage::NotAccessible("foo::Foo".into()),
        );
    }

    #[test]
    #[ignore]
    fn impl_trait_with_type_params() {
        ok("
            trait MyEquals[T] { fn equals(val: T): Bool; }
            class Foo(x: Int64)
            impl MyEquals[Foo] for Foo {
                fn equals(val: Foo): Bool {
                    self.x == val.x
                }
            }
        ")
    }

    #[test]
    fn method_not_in_trait() {
        err(
            "
            trait Foo {}
            class A
            impl Foo for A {
                fn bar() {}
            }",
            (5, 17),
            ErrorMessage::ElementNotInTrait,
        );
    }

    #[test]
    fn alias_not_in_trait() {
        err(
            "
            trait Foo {}
            class A
            impl Foo for A {
                type X = Int64;
            }",
            (5, 17),
            ErrorMessage::ElementNotInTrait,
        );
    }

    #[test]
    fn alias_in_impl_multiple_times() {
        err(
            "
            trait Foo {
                type X;
            }
            class A
            impl Foo for A {
                type X = Int64;
                type X = Bool;
            }",
            (8, 17),
            ErrorMessage::AliasExists("X".into(), Span::new(41, 7)),
        );
    }

    #[test]
    fn method_missing_in_impl() {
        err(
            "
            trait Foo {
                fn bar();
            }
            class A
            impl Foo for A {}",
            (6, 13),
            ErrorMessage::ElementNotInImpl("bar".into()),
        );
    }

    #[test]
    fn method_returning_self() {
        ok("trait Foo {
                fn foo(): Self;
            }

            class A

            impl Foo for A {
                fn foo(): A { return A(); }
            }");
    }

    #[test]
    fn static_method_not_in_trait() {
        err(
            "
            trait Foo {}
            class A
            impl Foo for A {
                static fn bar() {}
            }",
            (5, 24),
            ErrorMessage::ElementNotInTrait,
        );
    }

    #[test]
    fn static_method_missing_in_impl() {
        err(
            "
            trait Foo {
                static fn bar();
            }
            class A
            impl Foo for A {}",
            (6, 13),
            ErrorMessage::ElementNotInImpl("bar".into()),
        );
    }

    #[test]
    fn method_return_type_check() {
        err(
            "trait X {
                fn m(): Bool;
                fn n(): Bool;
              }

              class CX

              impl X for CX {
                fn m(): Int32 { 0 }
                fn n(): Bool { true }
              }",
            (9, 17),
            ErrorMessage::ImplMethodTypeMismatch,
        );
    }

    #[test]
    fn method_params_type_check() {
        err(
            "trait X {
                fn f(a: Int64, b: Int64): Bool;
              }

              class CX

              impl X for CX {
                fn f(a: Int64, b: Int32): Bool { true }
              }",
            (8, 17),
            ErrorMessage::ImplMethodTypeMismatch,
        );
    }
}
