use std::sync::Arc;

use crate::error::msg::SemError;
use crate::semck;
use crate::ty::SourceType;
use crate::vm::{Fct, FctParent, FileId, ImplId, NamespaceId, VM};

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
    ast: &'x ast::Impl,
}

impl<'x> ImplCheck<'x> {
    fn check(&mut self) {
        assert!(self.ast.trait_type.is_some());

        for method in &self.ast.methods {
            self.visit_method(method);
        }

        let mut ximpl = self.vm.impls[self.impl_id].write();

        if self.ast.type_params.is_some() {
            // We don't support type parameters for impl-blocks yet.
            self.vm
                .diag
                .lock()
                .report(self.file_id, self.ast.pos, SemError::Unimplemented);
            return;
        }

        if let Some(ref expr_trait_type) = self.ast.trait_type {
            if let Some(trait_ty) = semck::read_type_namespace(
                self.vm,
                self.file_id.into(),
                self.namespace_id,
                expr_trait_type,
            ) {
                match trait_ty {
                    SourceType::TraitObject(trait_id) => {
                        ximpl.trait_id = Some(trait_id);
                    }

                    _ => {
                        self.vm.diag.lock().report(
                            self.file_id,
                            self.ast.pos,
                            SemError::ExpectedTrait,
                        );
                    }
                }
            }
        } else {
            // We don't support extension blocks yet.
            self.vm
                .diag
                .lock()
                .report(self.file_id, self.ast.pos, SemError::Unimplemented);
            return;
        }

        if let Some(class_ty) = semck::read_type_namespace(
            self.vm,
            self.file_id.into(),
            self.namespace_id,
            &self.ast.class_type,
        ) {
            if class_ty.cls_id(self.vm).is_some() {
                ximpl.class_ty = class_ty;
            } else {
                self.vm.diag.lock().report(
                    self.file_id,
                    self.ast.class_type.pos(),
                    SemError::ClassExpected,
                );
            }
        }

        if ximpl.trait_id.is_some() && !ximpl.class_ty.is_error() {
            let cls = self.vm.classes.idx(ximpl.cls_id(self.vm));
            let mut cls = cls.write();
            cls.traits.push(ximpl.trait_id());
            cls.impls.push(ximpl.id);
        }
    }

    fn visit_method(&mut self, method: &Arc<ast::Function>) {
        if method.block.is_none() && !method.internal {
            self.vm
                .diag
                .lock()
                .report(self.file_id.into(), method.pos, SemError::MissingFctBody);
        }

        let parent = FctParent::Impl(self.impl_id);

        let fct = Fct::new(self.file_id.into(), self.namespace_id, method, parent);
        let fctid = self.vm.add_fct(fct);

        let mut ximpl = self.vm.impls[self.impl_id].write();
        ximpl.methods.push(fctid);
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
            SemError::ClassExpected,
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
    fn impl_class_type_params() {
        ok("trait MyTrait {} class Foo[T] impl MyTrait for Foo[String] {}");
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
