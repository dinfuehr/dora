use std::collections::{HashMap, HashSet};

use crate::extensiondefck::check_for_unconstrained_type_params;
use crate::sema::{
    params_match, FctDefinition, FctDefinitionId, ImplDefinition, ImplDefinitionId, Sema,
    SourceFileId, TraitDefinition,
};
use crate::{
    read_type_context, AllowSelf, ErrorMessage, ModuleSymTable, Name, SourceType, SymbolKind,
    TypeParamContext,
};

use dora_parser::ast;

pub fn check_definition(sa: &Sema) {
    for (_id, impl_) in sa.impls.iter() {
        let (impl_id, file_id, module_id, ast) = {
            (
                impl_.id(),
                impl_.file_id,
                impl_.module_id,
                impl_.ast.clone(),
            )
        };

        let mut implck = ImplCheck {
            sa,
            impl_id,
            file_id,
            sym: ModuleSymTable::new(sa, module_id),
            ast: &ast,
            instance_names: HashMap::new(),
            static_names: HashMap::new(),
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
    instance_names: HashMap<Name, FctDefinitionId>,
    static_names: HashMap<Name, FctDefinitionId>,
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

        for &method_id in impl_.methods() {
            self.visit_method(method_id);
        }
    }

    fn visit_method(&mut self, method_id: FctDefinitionId) {
        let method = &self.sa.fcts[method_id];

        if method.ast.block.is_none() && !method.is_internal {
            self.sa.report(
                self.file_id.into(),
                method.span,
                ErrorMessage::MissingFctBody,
            );
        }

        let table = if method.is_static {
            &mut self.static_names
        } else {
            &mut self.instance_names
        };

        if let Some(&existing_id) = table.get(&method.name) {
            let existing_fct = &self.sa.fcts[existing_id];
            let method_name = self.sa.interner.str(method.name).to_string();

            self.sa.report(
                method.file_id,
                method.ast.span,
                ErrorMessage::MethodExists(method_name, existing_fct.span),
            );
        } else {
            assert!(table.insert(method.name, method_id).is_none());
        }
    }
}

pub fn check_body(sa: &Sema) {
    for (_id, impl_) in sa.impls.iter() {
        check_impl_body(sa, impl_);
    }
}

fn check_impl_body(sa: &Sema, impl_: &ImplDefinition) {
    let trait_ = &sa.traits[impl_.trait_id()];

    let mut remaining_trait_methods: HashSet<FctDefinitionId> =
        trait_.methods().iter().cloned().collect();
    let mut trait_to_impl_method_map = HashMap::new();

    for &impl_method_id in impl_.methods() {
        let impl_method = &sa.fcts[impl_method_id];

        if let Some(trait_method_id) = trait_.get_method(impl_method.name, impl_method.is_static) {
            trait_to_impl_method_map.insert(trait_method_id, impl_method_id);
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

    let mut missing_methods = HashSet::new();

    for method_id in remaining_trait_methods {
        let method = &sa.fcts[method_id];

        if method.has_body() {
            // method has a default implementation, use that one
            trait_to_impl_method_map.insert(method_id, method_id);
        } else {
            missing_methods.insert(method_id);
        }
    }

    if !missing_methods.is_empty() {
        report_missing_methods(sa, impl_, trait_, missing_methods);
    }

    assert!(impl_.trait_method_map.set(trait_to_impl_method_map).is_ok());
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
            ErrorMessage::MethodExists("foo".into(), Span::new(141, 29)),
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

    #[test]
    fn impl_method_with_default_body() {
        ok("
            trait Foo {
                fn foo(): Int32 { 1 }
            }
            class Bar {}
            impl Foo for Bar {}");
    }
}
