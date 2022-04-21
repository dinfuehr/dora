use crate::language::error::msg::SemError;
use crate::language::extensiondefck::check_for_unconstrained_type_params;
use crate::language::sem_analysis::{
    FctDefinitionId, ImplDefinitionId, ModuleDefinitionId, SemAnalysis, SourceFileId,
};
use crate::language::sym::NestedSymTable;
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
            sym: NestedSymTable::new(sa, module_id),
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
    sym: NestedSymTable<'x>,
    ast: &'x ast::Impl,
}

impl<'x> ImplCheck<'x> {
    fn check(&mut self) {
        assert!(self.ast.trait_type.is_some());

        self.sym.push_level();

        if let Some(ref type_params) = self.ast.type_params {
            self.check_type_params(type_params);
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
                SourceType::Trait(trait_id, _) => {
                    impl_.trait_id = Some(trait_id);
                }

                _ => {
                    self.sa
                        .diag
                        .lock()
                        .report(self.file_id, self.ast.pos, SemError::ExpectedTrait);
                }
            }
        }

        if let Some(class_ty) = language::read_type(
            self.sa,
            &self.sym,
            self.file_id.into(),
            &self.ast.class_type,
            TypeParamContext::Impl(&*impl_),
            AllowSelf::No,
        ) {
            if class_ty.is_cls()
                || class_ty.is_struct()
                || class_ty.is_enum()
                || class_ty.is_primitive()
            {
                impl_.ty = class_ty.clone();

                check_for_unconstrained_type_params(
                    self.sa,
                    class_ty.clone(),
                    &impl_.type_params,
                    self.file_id,
                    self.ast.pos,
                );
            } else {
                self.sa.diag.lock().report(
                    self.file_id,
                    self.ast.class_type.pos(),
                    SemError::ClassEnumStructExpected,
                );
            }
        }

        if impl_.trait_id.is_some() && !impl_.ty.is_error() {
            match impl_.ty {
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
                        .ty
                        .primitive_struct_id(self.sa)
                        .expect("primitive expected");
                    let xstruct = self.sa.structs.idx(struct_id);
                    let mut xstruct = xstruct.write();
                    xstruct.impls.push(impl_.id());
                }

                SourceType::Struct(struct_id, _) => {
                    let xstruct = self.sa.structs.idx(struct_id);
                    let mut xstruct = xstruct.write();
                    xstruct.impls.push(impl_.id());
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

    fn check_type_params(&mut self, ast_type_params: &[ast::TypeParam]) {
        let impl_ = &self.sa.impls[self.impl_id];
        let mut impl_ = impl_.write();

        language::check_type_params(
            self.sa,
            ast_type_params,
            &mut impl_.type_params,
            &mut self.sym,
            self.file_id,
            self.ast.pos,
        );
    }

    fn visit_method(&mut self, impl_: &mut ImplDefinition, fct_id: FctDefinitionId) {
        let method = self.sa.fcts.idx(fct_id);
        let method = method.read();

        if method.ast.block.is_none() && !method.internal {
            self.sa
                .diag
                .lock()
                .report(self.file_id.into(), method.pos, SemError::MissingFctBody);
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
    use crate::language::error::msg::SemError;
    use crate::language::tests::*;

    #[test]
    fn impl_method_without_body() {
        err(
            "
            trait Foo {
                fn foo(): Int32;
            }
            class Bar {}
            impl Foo for Bar { fn foo(): Int32; }",
            pos(6, 32),
            SemError::MissingFctBody,
        );
    }

    #[test]
    fn impl_method_defined_twice() {
        err(
            "
            trait Foo {
                fn foo(): Int32;
            }
            class Bar {}
            impl Foo for Bar {
                fn foo(): Int32 { return 0; }
                fn foo(): Int32 { return 1; }
            }",
            pos(8, 17),
            SemError::MethodExists("foo".into(), pos(7, 17)),
        );
    }

    #[test]
    fn impl_for_unknown_trait() {
        err(
            "class A {} impl Foo for A {}",
            pos(1, 17),
            SemError::UnknownIdentifier("Foo".into()),
        );
    }

    #[test]
    fn impl_for_unknown_class() {
        err(
            "trait Foo {} impl Foo for A {}",
            pos(1, 27),
            SemError::UnknownIdentifier("A".into()),
        );

        err(
            "trait Foo {} trait A {} impl Foo for A {}",
            pos(1, 38),
            SemError::ClassEnumStructExpected,
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
            pos(4, 13),
            SemError::UnconstrainedTypeParam("T".into()),
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
            SemError::NotAccessible("foo::MyTrait".into()),
        );

        err(
            "
            mod foo { class Foo }
            trait MyTrait {}
            impl MyTrait for foo::Foo {}",
            pos(4, 30),
            SemError::NotAccessible("foo::Foo".into()),
        );
    }
}
