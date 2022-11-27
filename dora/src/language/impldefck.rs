use crate::language::error::msg::ErrorMessage;
use crate::language::extensiondefck::check_for_unconstrained_type_params;
use crate::language::sem_analysis::{
    FctDefinitionId, ImplDefinitionId, ModuleDefinitionId, SemAnalysis, SourceFileId,
};
use crate::language::sym::{ModuleSymTable, Sym};
use crate::language::ty::SourceType;
use crate::language::{self, AllowSelf, TypeParamContext};

use dora_parser::ast;

use super::sem_analysis::ImplDefinition;

pub fn check(sa: &SemAnalysis) {
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
            module_id,
            sym: ModuleSymTable::new(sa, module_id),
            ast: &ast,
        };

        implck.check();
    }
}

struct ImplCheck<'x> {
    sa: &'x SemAnalysis,
    file_id: SourceFileId,
    impl_id: ImplDefinitionId,
    module_id: ModuleDefinitionId,
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

        if let Some(trait_ty) = language::read_type(
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
                    self.sa.diag.lock().report(
                        self.file_id,
                        self.ast.pos,
                        ErrorMessage::ExpectedTrait,
                    );
                }
            }
        }

        if let Some(class_ty) = language::read_type(
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
                    self.ast.pos,
                );
            } else {
                self.sa.diag.lock().report(
                    self.file_id,
                    self.ast.extended_type.pos(),
                    ErrorMessage::ClassEnumValueExpected,
                );
            }
        }

        if impl_.trait_ty.is_trait() && !impl_.extended_ty.is_error() {
            match impl_.extended_ty {
                SourceType::Enum(enum_id, _) => {
                    let enum_ = &self.sa.enums[enum_id];
                    let mut enum_ = enum_.write();
                    enum_.impls.push(impl_.id());
                }

                SourceType::Bool
                | SourceType::UInt8
                | SourceType::Char
                | SourceType::Int32
                | SourceType::Int64
                | SourceType::Float32
                | SourceType::Float64 => {
                    let struct_id = impl_
                        .extended_ty
                        .primitive_struct_id(self.sa)
                        .expect("primitive expected");
                    let struct_ = self.sa.values.idx(struct_id);
                    let mut struct_ = struct_.write();
                    struct_.impls.push(impl_.id());
                }

                SourceType::Value(struct_id, _) => {
                    let struct_ = self.sa.values.idx(struct_id);
                    let mut struct_ = struct_.write();
                    struct_.impls.push(impl_.id());
                }

                SourceType::Class(cls_id, _) => {
                    let cls = self.sa.classes.idx(cls_id);
                    let mut cls = cls.write();
                    cls.impls.push(impl_.id());
                }

                _ => unreachable!(),
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

        if method.ast.block.is_none() && !method.internal {
            self.sa.diag.lock().report(
                self.file_id.into(),
                method.pos,
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
    use crate::language::error::msg::ErrorMessage;
    use crate::language::tests::*;

    #[test]
    fn impl_method_without_body() {
        err(
            "
            trait Foo {
                fun foo(): Int32;
            }
            class Bar
            impl Foo for Bar { fun foo(): Int32; }",
            pos(6, 32),
            ErrorMessage::MissingFctBody,
        );
    }

    #[test]
    fn impl_method_defined_twice() {
        err(
            "
            trait Foo {
                fun foo(): Int32;
            }
            class Bar
            impl Foo for Bar {
                fun foo(): Int32 { return 0; }
                fun foo(): Int32 { return 1; }
            }",
            pos(8, 17),
            ErrorMessage::MethodExists("foo".into(), pos(7, 17)),
        );
    }

    #[test]
    fn impl_for_unknown_trait() {
        err(
            "class A impl Foo for A {}",
            pos(1, 14),
            ErrorMessage::UnknownIdentifier("Foo".into()),
        );
    }

    #[test]
    fn impl_for_unknown_class() {
        err(
            "trait Foo {} impl Foo for A {}",
            pos(1, 27),
            ErrorMessage::UnknownIdentifier("A".into()),
        );

        err(
            "trait Foo {} trait A {} impl Foo for A {}",
            pos(1, 38),
            ErrorMessage::ClassEnumValueExpected,
        );
    }

    #[test]
    fn impl_definitions() {
        ok("trait Foo {} class A {} impl Foo for A {}");
        ok("trait Foo { fun toBool(): Bool; }
            class A {}
            impl Foo for A { fun toBool(): Bool { return false; } }");
    }

    #[test]
    fn impl_value() {
        ok("
            trait Foo {}
            value A(x: Int32)
            impl Foo for A {}
        ");
        ok("
            trait Foo {}
            value A[T](x: Int32)
            impl Foo for A[Int32] {}
            impl Foo for A[Float32] {}
        ");
        ok("
            trait Foo {}
            value A[T](x: Int32)
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
            value MyFoo[T]
            trait MyTrait {}
            impl[T] MyTrait for MyFoo[Int32] {}
        ",
            pos(4, 13),
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
            pos(4, 18),
            ErrorMessage::NotAccessible("foo::MyTrait".into()),
        );

        err(
            "
            mod foo { class Foo }
            trait MyTrait {}
            impl MyTrait for foo::Foo {}",
            pos(4, 30),
            ErrorMessage::NotAccessible("foo::Foo".into()),
        );
    }

    #[test]
    #[ignore]
    fn impl_trait_with_type_params() {
        ok("
            trait MyEquals[T] { fun equals(val: T): Bool; }
            class Foo(x: Int64)
            impl MyEquals[Foo] for Foo {
                fun equals(val: Foo): Bool {
                    self.x == val.x
                }
            }
        ")
    }
}
