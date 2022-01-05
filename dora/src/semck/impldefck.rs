use std::sync::Arc;

use crate::semck::error::msg::SemError;
use crate::semck::extensiondefck::check_for_unconstrained_type_params;
use crate::semck::{self, AllowSelf, TypeParamContext};
use crate::sym::NestedSymTable;
use crate::ty::SourceType;
use crate::vm::{
    AnnotationDefinition, FctDefinition, FctDefinitionId, FctParent, FileId, ImplId, NamespaceId,
    SemAnalysis,
};

use dora_parser::ast;

pub fn check(sa: &SemAnalysis) {
    for ximpl in sa.impls.iter() {
        let (impl_id, file_id, namespace_id, ast) = {
            let ximpl = ximpl.read();

            (
                ximpl.id,
                ximpl.file_id,
                ximpl.namespace_id,
                ximpl.ast.clone(),
            )
        };

        let mut implck = ImplCheck {
            sa,
            impl_id,
            file_id,
            namespace_id,
            sym: NestedSymTable::new(sa, namespace_id),
            ast: &ast,
        };

        implck.check();
    }
}

struct ImplCheck<'x> {
    sa: &'x SemAnalysis,
    file_id: FileId,
    impl_id: ImplId,
    namespace_id: NamespaceId,
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

        let mut ximpl = self.sa.impls[self.impl_id].write();

        let ast_trait_type = self.ast.trait_type.as_ref().unwrap();

        if let Some(trait_ty) = semck::read_type(
            self.sa,
            &self.sym,
            self.file_id.into(),
            ast_trait_type,
            TypeParamContext::Impl(&*ximpl),
            AllowSelf::No,
        ) {
            match trait_ty {
                SourceType::Trait(trait_id, _) => {
                    ximpl.trait_id = Some(trait_id);
                }

                _ => {
                    self.sa
                        .diag
                        .lock()
                        .report(self.file_id, self.ast.pos, SemError::ExpectedTrait);
                }
            }
        }

        if let Some(class_ty) = semck::read_type(
            self.sa,
            &self.sym,
            self.file_id.into(),
            &self.ast.class_type,
            TypeParamContext::Impl(&*ximpl),
            AllowSelf::No,
        ) {
            if class_ty.is_cls()
                || class_ty.is_struct()
                || class_ty.is_enum()
                || class_ty.is_primitive()
            {
                ximpl.ty = class_ty.clone();

                check_for_unconstrained_type_params(
                    self.sa,
                    class_ty.clone(),
                    &ximpl.type_params,
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

        if ximpl.trait_id.is_some() && !ximpl.ty.is_error() {
            match ximpl.ty {
                SourceType::Enum(enum_id, _) => {
                    let xenum = &self.sa.enums[enum_id];
                    let mut xenum = xenum.write();
                    xenum.impls.push(ximpl.id);
                }

                SourceType::Bool
                | SourceType::UInt8
                | SourceType::Char
                | SourceType::Int32
                | SourceType::Int64
                | SourceType::Float32
                | SourceType::Float64 => {
                    let struct_id = ximpl
                        .ty
                        .primitive_struct_id(self.sa)
                        .expect("primitive expected");
                    let xstruct = self.sa.structs.idx(struct_id);
                    let mut xstruct = xstruct.write();
                    xstruct.impls.push(ximpl.id);
                }

                SourceType::Struct(struct_id, _) => {
                    let xstruct = self.sa.structs.idx(struct_id);
                    let mut xstruct = xstruct.write();
                    xstruct.impls.push(ximpl.id);
                }

                SourceType::Class(cls_id, _) => {
                    let cls = self.sa.classes.idx(cls_id);
                    let mut cls = cls.write();
                    cls.impls.push(ximpl.id);
                }

                _ => unreachable!(),
            }
        }

        self.sym.pop_level();

        for method in &self.ast.methods {
            let method_id = self.visit_method(method);
            ximpl.methods.push(method_id);

            let table = if AnnotationDefinition::is_static(&method.annotation_usages, self.sa) {
                &mut ximpl.static_names
            } else {
                &mut ximpl.instance_names
            };

            if !table.contains_key(&method.name) {
                table.insert(method.name, method_id);
            }
        }
    }

    fn check_type_params(&mut self, ast_type_params: &[ast::TypeParam]) {
        let ximpl = &self.sa.impls[self.impl_id];
        let mut ximpl = ximpl.write();

        semck::check_type_params(
            self.sa,
            ast_type_params,
            &mut ximpl.type_params,
            &mut self.sym,
            self.file_id,
            self.ast.pos,
        );
    }

    fn visit_method(&mut self, method: &Arc<ast::Function>) -> FctDefinitionId {
        let internal = AnnotationDefinition::is_internal(&method.annotation_usages, self.sa);
        if method.block.is_none() && !internal {
            self.sa
                .diag
                .lock()
                .report(self.file_id.into(), method.pos, SemError::MissingFctBody);
        }

        let parent = FctParent::Impl(self.impl_id);

        let fct = FctDefinition::new(
            self.sa,
            self.file_id.into(),
            self.namespace_id,
            method,
            parent,
        );
        self.sa.add_fct(fct)
    }
}

#[cfg(test)]
mod tests {
    use crate::semck::error::msg::SemError;
    use crate::semck::tests::*;

    #[test]
    fn impl_method_without_body() {
        err(
            "
            trait Foo {
                fun foo(): Int32;
            }
            class Bar {}
            impl Foo for Bar { fun foo(): Int32; }",
            pos(6, 32),
            SemError::MissingFctBody,
        );
    }

    #[test]
    fn impl_method_defined_twice() {
        err(
            "
            trait Foo {
                fun foo(): Int32;
            }
            class Bar {}
            impl Foo for Bar {
                fun foo(): Int32 { return 0; }
                fun foo(): Int32 { return 1; }
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
        ok("trait Foo { fun toBool(): Bool; }
            class A {}
            impl Foo for A { fun toBool(): Bool { return false; } }");
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
    fn impl_namespace() {
        err(
            "
            namespace foo { trait MyTrait {} }
            class Foo
            impl foo::MyTrait for Foo {}",
            pos(4, 18),
            SemError::NotAccessible("foo::MyTrait".into()),
        );

        err(
            "
            namespace foo { class Foo }
            trait MyTrait {}
            impl MyTrait for foo::Foo {}",
            pos(4, 30),
            SemError::NotAccessible("foo::Foo".into()),
        );
    }
}
