use std::collections::HashSet;
use std::sync::Arc;

use crate::error::msg::SemError;
use crate::semck;
use crate::sym::{NestedSymTable, TypeSym};
use crate::ty::SourceType;
use crate::vm::{EnumId, ExtensionId, Fct, FctParent, FileId, NamespaceId, VM};

use dora_parser::ast;

pub fn check(vm: &VM) {
    for extension in &vm.extensions {
        let (extension_id, file_id, namespace_id, ast) = {
            let extension = extension.read();

            (
                extension.id,
                extension.file_id,
                extension.namespace_id,
                extension.ast.clone(),
            )
        };

        let mut extck = ExtensionCheck {
            vm,
            extension_id,
            sym: NestedSymTable::new(vm, namespace_id),
            namespace_id,
            file_id,
            ast: &ast,
            extension_ty: SourceType::Error,
        };

        extck.check();
    }
}

struct ExtensionCheck<'x> {
    vm: &'x VM,
    file_id: FileId,
    namespace_id: NamespaceId,
    sym: NestedSymTable<'x>,
    extension_id: ExtensionId,
    extension_ty: SourceType,
    ast: &'x ast::Impl,
}

impl<'x> ExtensionCheck<'x> {
    fn check(&mut self) {
        assert!(self.ast.trait_type.is_none());

        self.sym.push_level();

        if let Some(ref type_params) = self.ast.type_params {
            self.check_type_params(type_params);
        }

        if let Some(extension_ty) = semck::read_type_table(
            self.vm,
            &self.sym,
            self.file_id.into(),
            &self.ast.class_type,
        ) {
            self.extension_ty = extension_ty.clone();

            match extension_ty {
                SourceType::Enum(enum_id, _) => {
                    let mut xenum = self.vm.enums[enum_id].write();
                    xenum.extensions.push(self.extension_id);
                }

                _ => {
                    let cls_id = extension_ty.cls_id(self.vm).unwrap();
                    let cls = self.vm.classes.idx(cls_id);
                    let mut cls = cls.write();
                    cls.extensions.push(self.extension_id);
                }
            }

            let mut extension = self.vm.extensions[self.extension_id].write();
            extension.ty = extension_ty;
        }

        for method in &self.ast.methods {
            self.visit_method(method);
        }

        self.sym.pop_level();
    }

    fn visit_method(&mut self, f: &Arc<ast::Function>) {
        if f.block.is_none() && !f.internal {
            self.vm
                .diag
                .lock()
                .report(self.file_id.into(), f.pos, SemError::MissingFctBody);
        }

        let fct = Fct::new(
            self.file_id,
            self.namespace_id,
            f,
            FctParent::Extension(self.extension_id),
        );

        let fct_id = self.vm.add_fct(fct);

        if self.extension_ty.is_error() {
            return;
        }

        let success = match self.extension_ty {
            SourceType::Enum(enum_id, _) => self.check_in_enum(&f, enum_id),
            _ => self.check_in_class(&f),
        };

        if !success {
            return;
        }

        let mut extension = self.vm.extensions[self.extension_id].write();
        extension.methods.push(fct_id);

        let table = if f.is_static {
            &mut extension.static_names
        } else {
            &mut extension.instance_names
        };

        if !table.contains_key(&f.name) {
            table.insert(f.name, fct_id);
        }
    }

    fn check_type_params(&mut self, type_params: &[ast::TypeParam]) {
        let extension = &self.vm.extensions[self.extension_id];
        let extension = extension.read();

        if type_params.len() > 0 {
            let mut names = HashSet::new();
            let mut type_param_id = 0;

            for type_param in type_params {
                if !names.insert(type_param.name) {
                    let name = self.vm.interner.str(type_param.name).to_string();
                    let msg = SemError::TypeParamNameNotUnique(name);
                    self.vm
                        .diag
                        .lock()
                        .report(extension.file_id, type_param.pos, msg);
                }

                assert!(type_param.bounds.is_empty());

                let sym = TypeSym::TypeParam(type_param_id.into());
                self.sym.insert_type(type_param.name, sym);
                type_param_id += 1;
            }
        } else {
            let msg = SemError::TypeParamsExpected;
            self.vm
                .diag
                .lock()
                .report(extension.file_id, self.ast.pos, msg);
        }
    }

    fn check_in_enum(&self, f: &ast::Function, enum_id: EnumId) -> bool {
        let xenum = self.vm.enums[enum_id].read();

        for &extension_id in &xenum.extensions {
            if !self.check_extension(f, extension_id) {
                return false;
            }
        }

        true
    }

    fn check_in_class(&self, f: &ast::Function) -> bool {
        let cls_id = self.extension_ty.cls_id(self.vm).unwrap();
        let cls = self.vm.classes.idx(cls_id);
        let cls = cls.read();

        for &method in &cls.methods {
            let method = self.vm.fcts.idx(method);
            let method = method.read();

            if method.name == f.name && method.is_static == f.is_static {
                let method_name = self.vm.interner.str(method.name).to_string();
                let msg = SemError::MethodExists(method_name, method.pos);
                self.vm.diag.lock().report(self.file_id.into(), f.pos, msg);
                return false;
            }
        }

        for &extension_id in &cls.extensions {
            if !self.check_extension(f, extension_id) {
                return false;
            }
        }

        true
    }

    fn check_extension(&self, f: &ast::Function, extension_id: ExtensionId) -> bool {
        let extension = self.vm.extensions[extension_id].read();

        if extension.ty.type_params(self.vm) != self.extension_ty.type_params(self.vm) {
            return true;
        }

        let table = if f.is_static {
            &extension.static_names
        } else {
            &extension.instance_names
        };

        if let Some(&method_id) = table.get(&f.name) {
            let method = self.vm.fcts.idx(method_id);
            let method = method.read();
            let method_name = self.vm.interner.str(method.name).to_string();
            let msg = SemError::MethodExists(method_name, method.pos);
            self.vm.diag.lock().report(self.file_id.into(), f.pos, msg);
            false
        } else {
            true
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::SemError;
    use crate::semck::tests::*;

    #[test]
    fn extension_empty() {
        ok("class A impl A {}");
        ok("class A impl A {} impl A {}");
        err(
            "class A impl A[String] {}",
            pos(1, 14),
            SemError::WrongNumberTypeParams(0, 1),
        );

        ok("class A[T] impl A[Int32] {} impl A[String] {}");
        err(
            "class A[T: std::Zero] impl A[Int32] {} impl A[String] {}",
            pos(1, 45),
            SemError::TraitBoundNotSatisfied("String".into(), "Zero".into()),
        );
    }

    #[test]
    fn extension_method() {
        ok("class A impl A { fun foo() {} fun bar() {} }");
        err(
            "class A impl A { fun foo() {} fun foo() {} }",
            pos(1, 31),
            SemError::MethodExists("foo".into(), pos(1, 18)),
        );
    }

    #[test]
    fn extension_defined_twice() {
        err(
            "class A { fun foo() {} }
            impl A { fun foo() {} }",
            pos(2, 22),
            SemError::MethodExists("foo".into(), pos(1, 11)),
        );

        err(
            "class A
            impl A { fun foo() {} }
            impl A { fun foo() {} }",
            pos(3, 22),
            SemError::MethodExists("foo".into(), pos(2, 22)),
        );
    }

    #[test]
    fn extension_defined_twice_with_type_params_in_class() {
        err(
            "class Foo[T]
            impl Foo[Int32] { fun foo() {} }
            impl Foo[Int32] { fun foo() {} }",
            pos(3, 31),
            SemError::MethodExists("foo".into(), pos(2, 31)),
        );

        ok("class Foo[T]
            impl Foo[Int32] { fun foo() {} }
            impl Foo[Int64] { fun foo() {} }");
    }

    #[test]
    fn extension_with_illegal_type_param_in_class() {
        err(
            "trait MyTrait {}
            class Foo[T: MyTrait]
            impl Foo[String] {}
        ",
            pos(3, 18),
            SemError::TraitBoundNotSatisfied("String".into(), "MyTrait".into()),
        );
    }

    #[test]
    fn extension_enum() {
        ok("enum MyEnum { A, B } impl MyEnum {}");
        ok("enum MyEnum { A, B } impl MyEnum {} impl MyEnum {}");
        ok("enum MyEnum { A, B } impl MyEnum { fun foo() {} fun bar() {} }");

        err(
            "enum MyEnum { A, B } impl MyEnum { fun foo() {} fun foo() {} }",
            pos(1, 49),
            SemError::MethodExists("foo".into(), pos(1, 36)),
        );
    }

    #[test]
    fn extension_with_type_param() {
        ok("
            enum MyFoo[T] { A(T), B }
            impl[T] MyFoo[T] {
                fun test(x: T) {}
            }
            fun test(x: MyFoo[Int32]) { x.test(1); }
        ");
    }
}
