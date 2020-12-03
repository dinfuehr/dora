use std::sync::Arc;

use crate::error::msg::SemError;
use crate::semck::extensiondefck::check_for_unconstrained_type_params;
use crate::semck::{self, TypeParamContext};
use crate::sym::NestedSymTable;
use crate::ty::SourceType;
use crate::vm::{Fct, FctId, FctParent, FileId, ImplId, NamespaceId, VM};

use dora_parser::ast;

pub fn check(vm: &VM) {
    for ximpl in vm.impls.iter() {
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
            vm,
            impl_id,
            file_id,
            namespace_id,
            sym: NestedSymTable::new(vm, namespace_id),
            ast: &ast,
        };

        implck.check();
    }
}

struct ImplCheck<'x> {
    vm: &'x VM,
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

        let mut ximpl = self.vm.impls[self.impl_id].write();

        let ast_trait_type = self.ast.trait_type.as_ref().unwrap();

        if let Some(trait_ty) = semck::read_type(
            self.vm,
            &self.sym,
            self.file_id.into(),
            ast_trait_type,
            TypeParamContext::Impl(&*ximpl),
        ) {
            match trait_ty {
                SourceType::TraitObject(trait_id) => {
                    ximpl.trait_id = Some(trait_id);
                }

                _ => {
                    self.vm
                        .diag
                        .lock()
                        .report(self.file_id, self.ast.pos, SemError::ExpectedTrait);
                }
            }
        }

        if let Some(class_ty) = semck::read_type(
            self.vm,
            &self.sym,
            self.file_id.into(),
            &self.ast.class_type,
            TypeParamContext::Impl(&*ximpl),
        ) {
            if class_ty.cls_id(self.vm).is_some() || class_ty.is_struct() || class_ty.is_enum() {
                ximpl.ty = class_ty.clone();

                check_for_unconstrained_type_params(
                    self.vm,
                    class_ty.clone(),
                    &ximpl.type_params,
                    self.file_id,
                    self.ast.pos,
                );
            } else {
                self.vm.diag.lock().report(
                    self.file_id,
                    self.ast.class_type.pos(),
                    SemError::ClassEnumStructExpected,
                );
            }
        }

        if ximpl.trait_id.is_some() && !ximpl.ty.is_error() {
            match ximpl.ty {
                SourceType::Enum(enum_id, _) => {
                    let xenum = &self.vm.enums[enum_id];
                    let mut xenum = xenum.write();
                    xenum.impls.push(ximpl.id);
                }

                SourceType::Struct(struct_id, _) => {
                    let xstruct = self.vm.structs.idx(struct_id);
                    let mut xstruct = xstruct.write();
                    xstruct.impls.push(ximpl.id);
                }

                _ => {
                    let cls = self.vm.classes.idx(ximpl.cls_id(self.vm));
                    let mut cls = cls.write();
                    cls.impls.push(ximpl.id);
                }
            }
        }

        self.sym.pop_level();

        for method in &self.ast.methods {
            let method_id = self.visit_method(method);
            ximpl.methods.push(method_id);

            let table = if method.is_static {
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
        let ximpl = &self.vm.impls[self.impl_id];
        let mut ximpl = ximpl.write();

        semck::check_type_params(
            self.vm,
            ast_type_params,
            &mut ximpl.type_params,
            &mut self.sym,
            self.file_id,
            self.ast.pos,
        );
    }

    fn visit_method(&mut self, method: &Arc<ast::Function>) -> FctId {
        if method.block.is_none() && !method.internal {
            self.vm
                .diag
                .lock()
                .report(self.file_id.into(), method.pos, SemError::MissingFctBody);
        }

        let parent = FctParent::Impl(self.impl_id);

        let fct = Fct::new(self.file_id.into(), self.namespace_id, method, parent);
        self.vm.add_fct(fct)
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::SemError;
    use crate::semck::tests::*;

    #[test]
    fn impl_method_without_body() {
        err(
            "
            trait Foo {
                fun foo(): Int32;
            }
            class Bar {}
            impl Foo for Bar { fun foo(): Int32;}",
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
