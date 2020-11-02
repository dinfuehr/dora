use parking_lot::RwLock;
use std::collections::HashSet;
use std::sync::Arc;

use crate::error::msg::SemError;
use crate::semck;
use crate::sym::{SymTables, TypeSym};
use crate::ty::SourceType;
use crate::vm::{EnumId, ExtensionId, Fct, FctId, FctKind, FctParent, FctSrc, NodeMap, VM};

use dora_parser::ast;
use dora_parser::ast::visit::{self, Visitor};

pub fn check(vm: &VM, map_extension_defs: &NodeMap<ExtensionId>) {
    let mut clsck = ExtensionCheck {
        vm,
        extension_id: None,
        map_extension_defs,
        sym: SymTables::global(vm),

        file_id: 0,
        extension_ty: SourceType::Error,
    };

    clsck.check();
}

struct ExtensionCheck<'x> {
    vm: &'x VM,
    map_extension_defs: &'x NodeMap<ExtensionId>,
    file_id: u32,
    sym: SymTables<'x>,

    extension_id: Option<ExtensionId>,
    extension_ty: SourceType,
}

impl<'x> ExtensionCheck<'x> {
    fn check(&mut self) {
        let files = self.vm.files.clone();
        let files = files.read();

        for file in files.iter() {
            self.visit_file(file);
        }
    }

    fn visit_extension(&mut self, i: &Arc<ast::Impl>) {
        assert!(i.trait_type.is_none());
        self.extension_id = Some(*self.map_extension_defs.get(i.id).unwrap());

        self.sym.push_level();

        if let Some(ref type_params) = i.type_params {
            self.check_type_params(i, self.extension_id.unwrap(), type_params);
        }

        if let Some(extension_ty) =
            semck::read_type_table(self.vm, &self.sym, self.file_id.into(), &i.class_type)
        {
            self.extension_ty = extension_ty.clone();

            match extension_ty {
                SourceType::Enum(enum_id, _) => {
                    let mut xenum = self.vm.enums[enum_id].write();
                    xenum.extensions.push(self.extension_id.unwrap());
                }

                _ => {
                    let cls_id = extension_ty.cls_id(self.vm).unwrap();
                    let cls = self.vm.classes.idx(cls_id);
                    let mut cls = cls.write();
                    cls.extensions.push(self.extension_id.unwrap());
                }
            }

            let mut extension = self.vm.extensions[self.extension_id.unwrap()].write();
            extension.ty = extension_ty;
        }

        visit::walk_impl(self, i);

        self.extension_id = None;
        self.sym.pop_level();
    }

    fn check_type_params(
        &mut self,
        ximpl: &ast::Impl,
        extension_id: ExtensionId,
        type_params: &[ast::TypeParam],
    ) {
        let extension = &self.vm.extensions[extension_id];
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
                .report(extension.file_id, ximpl.pos, msg);
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

impl<'x> Visitor for ExtensionCheck<'x> {
    fn visit_file(&mut self, f: &ast::File) {
        visit::walk_file(self, f);
        self.file_id += 1;
    }

    fn visit_impl(&mut self, i: &Arc<ast::Impl>) {
        if i.trait_type.is_none() {
            self.visit_extension(i);
        }
    }

    fn visit_method(&mut self, f: &Arc<ast::Function>) {
        if self.extension_id.is_none() {
            return;
        }

        let extension_id = self.extension_id.unwrap();

        if f.block.is_none() && !f.internal {
            self.vm
                .diag
                .lock()
                .report(self.file_id.into(), f.pos, SemError::MissingFctBody);
        }

        let kind = if f.block.is_some() {
            FctKind::Source(RwLock::new(FctSrc::new()))
        } else {
            FctKind::Definition
        };

        let parent = FctParent::Extension(extension_id);

        let fct = Fct {
            id: FctId(0),
            ast: f.clone(),
            pos: f.pos,
            name: f.name,
            namespace_id: None,
            param_types: Vec::new(),
            return_type: SourceType::Unit,
            parent: parent,
            has_override: f.has_override,
            has_open: f.has_open,
            has_final: f.has_final,
            has_optimize_immediately: f.has_optimize_immediately,
            is_pub: f.is_pub,
            is_static: f.is_static,
            is_abstract: false,
            is_test: f.is_test,
            use_cannon: f.use_cannon,
            internal: f.internal,
            internal_resolved: false,
            overrides: None,
            is_constructor: false,
            vtable_index: None,
            initialized: false,
            impl_for: None,
            file_id: self.file_id.into(),
            variadic_arguments: false,

            type_params: Vec::new(),
            kind,
            bytecode: None,
            intrinsic: None,
        };

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

        let mut extension = self.vm.extensions[extension_id].write();
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
            "class A[T: Zero] impl A[Int32] {} impl A[String] {}",
            pos(1, 40),
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
