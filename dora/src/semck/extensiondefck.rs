use std::sync::Arc;

use crate::error::msg::SemError;
use crate::semck::{self, read_type, AllowSelf, TypeParamContext};
use crate::sym::NestedSymTable;
use crate::ty::SourceType;
use crate::vm::{
    EnumId, ExtensionId, Fct, FctParent, FileId, NamespaceId, StructId, TypeParam, VM,
};

use dora_parser::ast;
use dora_parser::lexer::position::Position;
use fixedbitset::FixedBitSet;

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

        if let Some(extension_ty) = read_type(
            self.vm,
            &self.sym,
            self.file_id.into(),
            &self.ast.class_type,
            TypeParamContext::Extension(self.extension_id),
            AllowSelf::No,
        ) {
            self.extension_ty = extension_ty.clone();

            match extension_ty {
                SourceType::Enum(enum_id, _) => {
                    let mut xenum = self.vm.enums[enum_id].write();
                    xenum.extensions.push(self.extension_id);
                }

                SourceType::Bool
                | SourceType::UInt8
                | SourceType::Char
                | SourceType::Int32
                | SourceType::Int64
                | SourceType::Float32
                | SourceType::Float64 => {
                    let struct_id = extension_ty
                        .primitive_struct_id(self.vm)
                        .expect("primitive expected");
                    let xstruct = self.vm.structs.idx(struct_id);
                    let mut xstruct = xstruct.write();

                    xstruct.extensions.push(self.extension_id);
                }

                SourceType::Struct(struct_id, _) => {
                    let xstruct = self.vm.structs.idx(struct_id);
                    let mut xstruct = xstruct.write();

                    xstruct.extensions.push(self.extension_id);
                }

                _ => {
                    let cls_id = extension_ty.cls_id().unwrap();
                    let cls = self.vm.classes.idx(cls_id);
                    let mut cls = cls.write();
                    cls.extensions.push(self.extension_id);
                }
            }

            let mut extension = self.vm.extensions[self.extension_id].write();

            check_for_unconstrained_type_params(
                self.vm,
                extension_ty.clone(),
                &extension.type_params,
                self.file_id,
                self.ast.pos,
            );

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
            SourceType::Bool
            | SourceType::UInt8
            | SourceType::Char
            | SourceType::Int32
            | SourceType::Int64
            | SourceType::Float32
            | SourceType::Float64 => {
                let struct_id = self
                    .extension_ty
                    .primitive_struct_id(self.vm)
                    .expect("primitive expected");
                self.check_in_struct(&f, struct_id)
            }
            SourceType::Struct(struct_id, _) => self.check_in_struct(&f, struct_id),
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

    fn check_type_params(&mut self, ast_type_params: &[ast::TypeParam]) {
        let extension = &self.vm.extensions[self.extension_id];
        let mut extension = extension.write();

        semck::check_type_params(
            self.vm,
            ast_type_params,
            &mut extension.type_params,
            &mut self.sym,
            self.file_id,
            self.ast.pos,
        );
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

    fn check_in_struct(&self, f: &ast::Function, struct_id: StructId) -> bool {
        let xstruct = self.vm.structs.idx(struct_id);
        let xstruct = xstruct.read();

        for &extension_id in &xstruct.extensions {
            if !self.check_extension(f, extension_id) {
                return false;
            }
        }

        true
    }

    fn check_in_class(&self, f: &ast::Function) -> bool {
        let cls_id = self.extension_ty.cls_id().unwrap();
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

pub fn check_for_unconstrained_type_params(
    vm: &VM,
    ty: SourceType,
    type_params_defs: &[TypeParam],
    file_id: FileId,
    pos: Position,
) {
    let mut bitset = FixedBitSet::with_capacity(type_params_defs.len());

    discover_type_params(vm, ty, &mut bitset);

    bitset.toggle_range(..);

    for idx in bitset.ones() {
        let type_param_def = &type_params_defs[idx];
        let tp_name = vm.interner.str(type_param_def.name).to_string();
        vm.diag
            .lock()
            .report(file_id, pos, SemError::UnconstrainedTypeParam(tp_name));
    }
}

fn discover_type_params(vm: &VM, ty: SourceType, used_type_params: &mut FixedBitSet) {
    match ty {
        SourceType::Error
        | SourceType::Unit
        | SourceType::This
        | SourceType::Any
        | SourceType::Bool
        | SourceType::UInt8
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Module(_)
        | SourceType::Ptr
        | SourceType::Trait(_, _) => {}
        SourceType::Class(_, list_id)
        | SourceType::Enum(_, list_id)
        | SourceType::Struct(_, list_id) => {
            let params = vm.source_type_arrays.lock().get(list_id);

            for param in params.iter() {
                discover_type_params(vm, param, used_type_params);
            }
        }
        SourceType::Tuple(tuple_id) => {
            let subtypes = vm.tuples.lock().get(tuple_id);

            for subtype in subtypes.iter() {
                discover_type_params(vm, subtype.clone(), used_type_params);
            }
        }
        SourceType::Lambda(_) => unimplemented!(),
        SourceType::TypeParam(tp_id) => {
            used_type_params.insert(tp_id.to_usize());
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
            SemError::TypeNotImplementingTrait("String".into(), "Zero".into()),
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
            SemError::TypeNotImplementingTrait("String".into(), "MyTrait".into()),
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

    #[test]
    fn extension_unconstrained_type_param() {
        err(
            "
            struct MyFoo[T]
            impl[T] MyFoo[Int32] {}
        ",
            pos(3, 13),
            SemError::UnconstrainedTypeParam("T".into()),
        );

        err(
            "
            struct MyFoo[T]
            impl[A, B] MyFoo[(A, A)] {}
        ",
            pos(3, 13),
            SemError::UnconstrainedTypeParam("B".into()),
        );
    }

    #[test]
    fn extension_struct() {
        ok("
            struct Foo { f1: Int32, f2: Int32 }
            impl Foo {
                fun sum(): Int32 {
                    self.f1 + self.f2
                }
            }
            fun test(x: Foo): Int32 { x.sum() }
        ");
    }

    #[test]
    fn extension_struct_type_params() {
        ok("
            struct Foo[T](value: T)
            trait MyTrait { fun bar(): Int32; }
            impl[X: MyTrait] Foo[X] {
                fun getmyhash(): Int32 {
                    self.value.bar()
                }
            }
        ");
    }

    #[test]
    fn extension_namespace() {
        err(
            "
            impl foo::MyFoo { fun bar() {} }
            namespace foo { class MyFoo }
        ",
            pos(2, 18),
            SemError::NotAccessible("foo::MyFoo".into()),
        );

        ok("
            impl foo::MyFoo { fun bar() {} }
            namespace foo { @pub class MyFoo }
        ");
    }
}
