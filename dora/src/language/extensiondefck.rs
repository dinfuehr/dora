use crate::language::error::msg::SemError;
use crate::language::sem_analysis::{
    EnumDefinitionId, ExtensionDefinitionId, FctDefinitionId, ModuleDefinitionId, SemAnalysis,
    SourceFileId, StructDefinitionId, TypeParam,
};
use crate::language::sym::NestedSymTable;
use crate::language::ty::SourceType;
use crate::language::{self, read_type, AllowSelf, TypeParamContext};

use dora_parser::ast;
use dora_parser::lexer::position::Position;
use fixedbitset::FixedBitSet;

pub fn check(sa: &SemAnalysis) {
    for extension in sa.extensions.iter() {
        let (extension_id, file_id, module_id, ast) = {
            let extension = extension.read();

            (
                extension.id(),
                extension.file_id,
                extension.module_id,
                extension.ast.clone(),
            )
        };

        let mut extck = ExtensionCheck {
            sa,
            extension_id,
            sym: NestedSymTable::new(sa, module_id),
            module_id,
            file_id,
            ast: &ast,
            extension_ty: SourceType::Error,
        };

        extck.check();
    }
}

struct ExtensionCheck<'x> {
    sa: &'x SemAnalysis,
    file_id: SourceFileId,
    module_id: ModuleDefinitionId,
    sym: NestedSymTable,
    extension_id: ExtensionDefinitionId,
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
            self.sa,
            &self.sym,
            self.file_id.into(),
            &self.ast.extended_type,
            TypeParamContext::Extension(self.extension_id),
            AllowSelf::No,
        ) {
            self.extension_ty = extension_ty.clone();

            match extension_ty {
                SourceType::Enum(enum_id, _) => {
                    let mut enum_ = self.sa.enums[enum_id].write();
                    enum_.extensions.push(self.extension_id);
                }

                SourceType::Bool
                | SourceType::UInt8
                | SourceType::Char
                | SourceType::Int32
                | SourceType::Int64
                | SourceType::Float32
                | SourceType::Float64 => {
                    let struct_id = extension_ty
                        .primitive_struct_id(self.sa)
                        .expect("primitive expected");
                    let xstruct = self.sa.structs.idx(struct_id);
                    let mut xstruct = xstruct.write();

                    xstruct.extensions.push(self.extension_id);
                }

                SourceType::Struct(struct_id, _) => {
                    let xstruct = self.sa.structs.idx(struct_id);
                    let mut xstruct = xstruct.write();

                    xstruct.extensions.push(self.extension_id);
                }

                _ => {
                    let cls_id = extension_ty.cls_id().unwrap();
                    let cls = self.sa.classes.idx(cls_id);
                    let mut cls = cls.write();
                    cls.extensions.push(self.extension_id);
                }
            }

            let mut extension = self.sa.extensions[self.extension_id].write();

            check_for_unconstrained_type_params(
                self.sa,
                extension_ty.clone(),
                &extension.type_params,
                self.file_id,
                self.ast.pos,
            );

            extension.ty = extension_ty;
        }

        let methods = self
            .sa
            .extensions
            .idx(self.extension_id)
            .read()
            .methods
            .clone();

        for method_id in methods {
            self.visit_method(method_id);
        }

        self.sym.pop_level();
    }

    fn visit_method(&mut self, fct_id: FctDefinitionId) {
        let fct = self.sa.fcts.idx(fct_id);
        let fct = fct.read();

        if fct.ast.block.is_none() && !fct.internal {
            self.sa
                .diag
                .lock()
                .report(self.file_id.into(), fct.pos, SemError::MissingFctBody);
        }

        if self.extension_ty.is_error() {
            return;
        }

        let success = match self.extension_ty {
            SourceType::Enum(enum_id, _) => self.check_in_enum(&fct.ast, enum_id),
            SourceType::Bool
            | SourceType::UInt8
            | SourceType::Char
            | SourceType::Int32
            | SourceType::Int64
            | SourceType::Float32
            | SourceType::Float64 => {
                let struct_id = self
                    .extension_ty
                    .primitive_struct_id(self.sa)
                    .expect("primitive expected");
                self.check_in_struct(&fct.ast, struct_id)
            }
            SourceType::Struct(struct_id, _) => self.check_in_struct(&fct.ast, struct_id),
            _ => self.check_in_class(&fct.ast),
        };

        if !success {
            return;
        }

        let extension = self.sa.extensions.idx(self.extension_id);
        let mut extension = extension.write();

        let table = if fct.is_static {
            &mut extension.static_names
        } else {
            &mut extension.instance_names
        };

        if !table.contains_key(&fct.name) {
            table.insert(fct.name, fct_id);
        }
    }

    fn check_type_params(&mut self, ast_type_params: &[ast::TypeParam]) {
        let extension = &self.sa.extensions[self.extension_id];
        let mut extension = extension.write();

        language::check_type_params(
            self.sa,
            ast_type_params,
            &mut extension.type_params,
            &mut self.sym,
            self.file_id,
            self.ast.pos,
        );
    }

    fn check_in_enum(&self, f: &ast::Function, enum_id: EnumDefinitionId) -> bool {
        let enum_ = self.sa.enums[enum_id].read();

        for &extension_id in &enum_.extensions {
            if !self.check_extension(f, extension_id) {
                return false;
            }
        }

        true
    }

    fn check_in_struct(&self, f: &ast::Function, struct_id: StructDefinitionId) -> bool {
        let xstruct = self.sa.structs.idx(struct_id);
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
        let cls = self.sa.classes.idx(cls_id);
        let cls = cls.read();

        for &extension_id in &cls.extensions {
            if !self.check_extension(f, extension_id) {
                return false;
            }
        }

        true
    }

    fn check_extension(&self, f: &ast::Function, extension_id: ExtensionDefinitionId) -> bool {
        let extension = self.sa.extensions[extension_id].read();

        if extension.ty.type_params() != self.extension_ty.type_params() {
            return true;
        }

        let table = if f.is_static {
            &extension.static_names
        } else {
            &extension.instance_names
        };

        if let Some(&method_id) = table.get(&f.name) {
            let method = self.sa.fcts.idx(method_id);
            let method = method.read();
            let method_name = self.sa.interner.str(method.name).to_string();
            let msg = SemError::MethodExists(method_name, method.pos);
            self.sa.diag.lock().report(self.file_id.into(), f.pos, msg);
            false
        } else {
            true
        }
    }
}

pub fn check_for_unconstrained_type_params(
    sa: &SemAnalysis,
    ty: SourceType,
    type_params_defs: &[TypeParam],
    file_id: SourceFileId,
    pos: Position,
) {
    let mut bitset = FixedBitSet::with_capacity(type_params_defs.len());

    discover_type_params(sa, ty, &mut bitset);

    bitset.toggle_range(..);

    for idx in bitset.ones() {
        let type_param_def = &type_params_defs[idx];
        let tp_name = sa.interner.str(type_param_def.name).to_string();
        sa.diag
            .lock()
            .report(file_id, pos, SemError::UnconstrainedTypeParam(tp_name));
    }
}

fn discover_type_params(sa: &SemAnalysis, ty: SourceType, used_type_params: &mut FixedBitSet) {
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
        | SourceType::Ptr
        | SourceType::Trait(_, _) => {}
        SourceType::Class(_, params)
        | SourceType::Enum(_, params)
        | SourceType::Struct(_, params) => {
            for param in params.iter() {
                discover_type_params(sa, param, used_type_params);
            }
        }
        SourceType::Tuple(subtypes) => {
            for subtype in subtypes.iter() {
                discover_type_params(sa, subtype.clone(), used_type_params);
            }
        }
        SourceType::Lambda(_, _) => unimplemented!(),
        SourceType::TypeParam(tp_id) => {
            used_type_params.insert(tp_id.to_usize());
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::language::error::msg::SemError;
    use crate::language::tests::*;

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
        ok("class A impl A { fn foo() {} fn bar() {} }");
        err(
            "class A impl A { fn foo() {} fn foo() {} }",
            pos(1, 30),
            SemError::MethodExists("foo".into(), pos(1, 18)),
        );
    }

    #[test]
    fn extension_defined_twice() {
        err(
            "class A
            impl A { fn foo() {} }
            impl A { fn foo() {} }",
            pos(3, 22),
            SemError::MethodExists("foo".into(), pos(2, 22)),
        );
    }

    #[test]
    fn extension_defined_twice_with_type_params_in_class() {
        err(
            "class Foo[T]
            impl Foo[Int32] { fn foo() {} }
            impl Foo[Int32] { fn foo() {} }",
            pos(3, 31),
            SemError::MethodExists("foo".into(), pos(2, 31)),
        );

        ok("class Foo[T]
            impl Foo[Int32] { fn foo() {} }
            impl Foo[Int64] { fn foo() {} }");
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
        ok("enum MyEnum { A, B } impl MyEnum { fn foo() {} fn bar() {} }");

        err(
            "enum MyEnum { A, B } impl MyEnum { fn foo() {} fn foo() {} }",
            pos(1, 48),
            SemError::MethodExists("foo".into(), pos(1, 36)),
        );
    }

    #[test]
    fn extension_with_type_param() {
        ok("
            enum MyFoo[T] { A(T), B }
            impl[T] MyFoo[T] {
                fn test(x: T) {}
            }
            fn test(x: MyFoo[Int32]) { x.test(1i32); }
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
                fn sum(): Int32 {
                    self.f1 + self.f2
                }
            }
            fn test(x: Foo): Int32 { x.sum() }
        ");
    }

    #[test]
    fn extension_struct_type_params() {
        ok("
            struct Foo[T](value: T)
            trait MyTrait { fn bar(): Int32; }
            impl[X: MyTrait] Foo[X] {
                fn getmyhash(): Int32 {
                    self.value.bar()
                }
            }
        ");
    }

    #[test]
    fn extension_mod() {
        err(
            "
            impl foo::MyFoo { fn bar() {} }
            mod foo { class MyFoo }
        ",
            pos(2, 18),
            SemError::NotAccessible("foo::MyFoo".into()),
        );

        ok("
            impl foo::MyFoo { fn bar() {} }
            mod foo { @pub class MyFoo }
        ");
    }
}
