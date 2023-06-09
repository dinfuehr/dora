use crate::error::msg::ErrorMessage;
use crate::extensiondefck::check_for_unconstrained_type_params;
use crate::sema::{FctDefinitionId, ImplDefinitionId, Sema, SourceFileId};
use crate::sym::{ModuleSymTable, Sym};
use crate::ty::SourceType;
use crate::{read_type_context, AllowSelf, TypeParamContext};

use dora_parser::ast;

use super::sema::ImplDefinition;

pub fn check(sa: &Sema) {
    for impl_ in sa.impls.iter() {
        let (impl_id, file_id, module_id, ast) = {
            let impl_ = impl_.read();

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
            let impl_ = self.sa.impls.idx(self.impl_id);
            let impl_ = impl_.read();

            for (id, name) in impl_.type_params().names() {
                self.sym.insert(name, Sym::TypeParam(id));
            }
        }

        let mut impl_ = self.sa.impls[self.impl_id].write();

        let ast_trait_type = self.ast.trait_type.as_ref().unwrap();

        if let Some(trait_ty) = read_type_context(
            self.sa,
            &self.sym,
            self.file_id.into(),
            ast_trait_type,
            TypeParamContext::Impl(&*impl_),
            AllowSelf::No,
        ) {
            match trait_ty {
                SourceType::Trait(trait_id, type_params) => {
                    impl_.trait_ty = SourceType::Trait(trait_id, type_params);
                }

                _ => {
                    self.sa
                        .report(self.file_id, self.ast.span, ErrorMessage::ExpectedTrait);
                }
            }
        }

        if let Some(class_ty) = read_type_context(
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
            {
                impl_.extended_ty = class_ty.clone();

                check_for_unconstrained_type_params(
                    self.sa,
                    class_ty.clone(),
                    impl_.type_params(),
                    self.file_id,
                    self.ast.span,
                );
            } else {
                self.sa.report(
                    self.file_id,
                    self.ast.extended_type.span(),
                    ErrorMessage::ClassEnumStructExpected,
                );
            }
        }

        self.sym.pop_level();

        let methods = impl_.methods.clone();

        for method_id in methods {
            self.visit_method(&mut *impl_, method_id);
        }
    }

    fn visit_method(&mut self, impl_: &mut ImplDefinition, fct_id: FctDefinitionId) {
        let method = self.sa.fcts.idx(fct_id);
        let method = method.read();

        if method.ast.block.is_none() && !method.is_internal {
            self.sa.report(
                self.file_id.into(),
                method.span,
                ErrorMessage::MissingFctBody,
            );
        }

        let table = if method.is_static {
            &mut impl_.static_names
        } else {
            &mut impl_.instance_names
        };

        if !table.contains_key(&method.name) {
            table.insert(method.name, fct_id);
        }
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
            ErrorMessage::ClassEnumStructExpected,
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
}
