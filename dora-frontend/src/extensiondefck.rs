use crate::error::msg::ErrorMessage;
use crate::sema::{
    EnumDefinitionId, ExtensionDefinitionId, FctDefinitionId, Sema, SourceFileId,
    StructDefinitionId, TypeParamDefinition, TypeParamId,
};
use crate::sym::{ModuleSymTable, SymbolKind};
use crate::ty::SourceType;
use crate::{read_type_context, AllowSelf, TypeParamContext};

use dora_parser::ast;
use dora_parser::Span;
use fixedbitset::FixedBitSet;

pub fn check(sa: &Sema) {
    for (_id, extension) in sa.extensions.iter() {
        let (extension_id, file_id, module_id, ast) = {
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
            sym: ModuleSymTable::new(sa, module_id),
            file_id,
            ast: &ast,
            extension_ty: SourceType::Error,
        };

        extck.check();
    }
}

struct ExtensionCheck<'x> {
    sa: &'x Sema,
    file_id: SourceFileId,
    sym: ModuleSymTable,
    extension_id: ExtensionDefinitionId,
    extension_ty: SourceType,
    ast: &'x ast::Impl,
}

impl<'x> ExtensionCheck<'x> {
    fn check(&mut self) {
        assert!(self.ast.trait_type.is_none());

        self.sym.push_level();

        {
            let extension = &self.sa.extensions[self.extension_id];

            for (id, name) in extension.type_params().names() {
                self.sym.insert(name, SymbolKind::TypeParam(id));
            }
        }

        if let Some(extension_ty) = read_type_context(
            self.sa,
            &self.sym,
            self.file_id.into(),
            &self.ast.extended_type,
            TypeParamContext::Extension(self.extension_id),
            AllowSelf::No,
        ) {
            self.extension_ty = extension_ty.clone();

            match extension_ty {
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
                    let struct_ = &self.sa.structs[struct_id];
                    struct_.extensions.borrow_mut().push(self.extension_id);
                }

                SourceType::Struct(struct_id, _) => {
                    let struct_ = &self.sa.structs[struct_id];
                    struct_.extensions.borrow_mut().push(self.extension_id);
                }

                SourceType::Enum(enum_id, _) => {
                    let enum_ = &self.sa.enums[enum_id];
                    enum_.extensions.borrow_mut().push(self.extension_id);
                }

                SourceType::Class(cls_id, _) => {
                    let cls = &self.sa.classes[cls_id];
                    cls.extensions.borrow_mut().push(self.extension_id);
                }

                SourceType::Trait(..) => {
                    unimplemented!();
                }

                SourceType::Tuple(..)
                | SourceType::Unit
                | SourceType::TypeParam(..)
                | SourceType::Lambda(..) => {
                    let extension = &self.sa.extensions[self.extension_id];

                    self.sa.report(
                        self.file_id.into(),
                        extension.span,
                        ErrorMessage::ExpectedImplType,
                    );
                }

                SourceType::Error | SourceType::Any | SourceType::This | SourceType::Ptr => {
                    unreachable!()
                }
            }

            let extension = &self.sa.extensions[self.extension_id];

            check_for_unconstrained_type_params(
                self.sa,
                extension_ty.clone(),
                extension.type_params(),
                self.file_id,
                self.ast.span,
            );

            assert!(extension.ty.set(extension_ty).is_ok());
        }

        for &method_id in self.sa.extensions[self.extension_id]
            .methods
            .get()
            .expect("missing method")
        {
            self.visit_method(method_id);
        }

        self.sym.pop_level();
    }

    fn visit_method(&mut self, fct_id: FctDefinitionId) {
        let fct = &self.sa.fcts[fct_id];

        if fct.ast.block.is_none() && !fct.is_internal {
            self.sa
                .report(self.file_id.into(), fct.span, ErrorMessage::MissingFctBody);
        }

        if self.extension_ty.is_error() {
            return;
        }

        let success = match self.extension_ty {
            SourceType::Enum(enum_id, _) => self.check_in_enum(&fct.ast, fct.is_static, enum_id),
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
                self.check_in_struct(&fct.ast, fct.is_static, struct_id)
            }
            SourceType::Struct(struct_id, _) => {
                self.check_in_struct(&fct.ast, fct.is_static, struct_id)
            }
            _ => self.check_in_class(&fct.ast, fct.is_static),
        };

        if !success {
            return;
        }

        let extension = &self.sa.extensions[self.extension_id];

        let table = if fct.is_static {
            &extension.static_names
        } else {
            &extension.instance_names
        };

        let mut table = table.borrow_mut();

        if !table.contains_key(&fct.name) {
            table.insert(fct.name, fct_id);
        }
    }

    fn check_in_enum(&self, f: &ast::Function, is_static: bool, enum_id: EnumDefinitionId) -> bool {
        let enum_ = &self.sa.enums[enum_id];
        let extensions = enum_.extensions.borrow();

        for &extension_id in extensions.iter() {
            if !self.check_extension(f, is_static, extension_id) {
                return false;
            }
        }

        true
    }

    fn check_in_struct(
        &self,
        f: &ast::Function,
        is_static: bool,
        struct_id: StructDefinitionId,
    ) -> bool {
        let struct_ = &self.sa.structs[struct_id];
        let extensions = struct_.extensions.borrow();

        for &extension_id in extensions.iter() {
            if !self.check_extension(f, is_static, extension_id) {
                return false;
            }
        }

        true
    }

    fn check_in_class(&self, f: &ast::Function, is_static: bool) -> bool {
        let cls_id = self.extension_ty.cls_id().unwrap();
        let cls = &self.sa.classes[cls_id];
        let extensions = cls.extensions.borrow();

        for &extension_id in extensions.iter() {
            if !self.check_extension(f, is_static, extension_id) {
                return false;
            }
        }

        true
    }

    fn check_extension(
        &self,
        f: &ast::Function,
        is_static: bool,
        extension_id: ExtensionDefinitionId,
    ) -> bool {
        let extension = &self.sa.extensions[extension_id];

        if extension.ty().type_params() != self.extension_ty.type_params() {
            return true;
        }

        let table = if is_static {
            &extension.static_names
        } else {
            &extension.instance_names
        };

        let name = self
            .sa
            .interner
            .intern(&f.name.as_ref().expect("missing name").name_as_string);

        if let Some(&method_id) = table.borrow().get(&name) {
            let method = &self.sa.fcts[method_id];
            let method_name = self.sa.interner.str(method.name).to_string();
            let msg = ErrorMessage::MethodExists(method_name, method.span);
            self.sa.report(self.file_id.into(), f.span, msg);
            false
        } else {
            true
        }
    }
}

pub fn check_for_unconstrained_type_params(
    sa: &Sema,
    ty: SourceType,
    type_params_defs: &TypeParamDefinition,
    file_id: SourceFileId,
    span: Span,
) {
    let mut bitset = FixedBitSet::with_capacity(type_params_defs.len());

    discover_type_params(sa, ty, &mut bitset);

    bitset.toggle_range(..);

    for idx in bitset.ones() {
        let type_param_def = type_params_defs.name(TypeParamId(idx));
        let tp_name = sa.interner.str(type_param_def).to_string();
        sa.report(file_id, span, ErrorMessage::UnconstrainedTypeParam(tp_name));
    }
}

fn discover_type_params(sa: &Sema, ty: SourceType, used_type_params: &mut FixedBitSet) {
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
        SourceType::Lambda(params, return_type) => {
            for param in params.iter() {
                discover_type_params(sa, param, used_type_params);
            }

            discover_type_params(sa, *return_type, used_type_params);
        }
        SourceType::TypeParam(tp_id) => {
            used_type_params.insert(tp_id.to_usize());
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::ErrorMessage;
    use crate::tests::*;
    use dora_parser::Span;

    #[test]
    fn extension_empty() {
        ok("class A impl A {}");
        ok("class A impl A {} impl A {}");
        err(
            "class A impl A[String] {}",
            (1, 14),
            ErrorMessage::WrongNumberTypeParams(0, 1),
        );

        ok("class A[T] impl A[Int32] {} impl A[String] {}");
        err(
            "class A[T: std::Zero] impl A[Int32] {} impl A[String] {}",
            (1, 45),
            ErrorMessage::TypeNotImplementingTrait("String".into(), "Zero".into()),
        );
    }

    #[test]
    fn extension_method() {
        ok("class A impl A { fn foo() {} fn bar() {} }");
        err(
            "class A impl A { fn foo() {} fn foo() {} }",
            (1, 30),
            ErrorMessage::MethodExists("foo".into(), Span::new(17, 11)),
        );
    }

    #[test]
    fn extension_defined_twice() {
        err(
            "class A
            impl A { fn foo() {} }
            impl A { fn foo() {} }",
            (3, 22),
            ErrorMessage::MethodExists("foo".into(), Span::new(29, 11)),
        );
    }

    #[test]
    fn extension_defined_twice_with_type_params_in_class() {
        err(
            "class Foo[T]
            impl Foo[Int32] { fn foo() {} }
            impl Foo[Int32] { fn foo() {} }",
            (3, 31),
            ErrorMessage::MethodExists("foo".into(), Span::new(43, 11)),
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
            (3, 18),
            ErrorMessage::TypeNotImplementingTrait("String".into(), "MyTrait".into()),
        );
    }

    #[test]
    fn extension_enum() {
        ok("enum MyEnum { A, B } impl MyEnum {}");
        ok("enum MyEnum { A, B } impl MyEnum {} impl MyEnum {}");
        ok("enum MyEnum { A, B } impl MyEnum { fn foo() {} fn bar() {} }");

        err(
            "enum MyEnum { A, B } impl MyEnum { fn foo() {} fn foo() {} }",
            (1, 48),
            ErrorMessage::MethodExists("foo".into(), Span::new(35, 11)),
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
            (3, 13),
            ErrorMessage::UnconstrainedTypeParam("T".into()),
        );

        err(
            "
            struct MyFoo[T]
            impl[A, B] MyFoo[(A, A)] {}
        ",
            (3, 13),
            ErrorMessage::UnconstrainedTypeParam("B".into()),
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
            (2, 18),
            ErrorMessage::NotAccessible("foo::MyFoo".into()),
        );

        ok("
            impl foo::MyFoo { fn bar() {} }
            mod foo { pub class MyFoo }
        ");
    }

    #[test]
    fn extension_for_trait() {
        err(
            "
            impl (Int64, Int64) {}
        ",
            (2, 13),
            ErrorMessage::ExpectedImplType,
        );
    }

    #[test]
    fn extension_for_unit() {
        err(
            "
            impl () {}
        ",
            (2, 13),
            ErrorMessage::ExpectedImplType,
        );
    }

    #[test]
    fn extension_for_lambda() {
        err(
            "
            impl (Int64, Int64): Bool {}
        ",
            (2, 13),
            ErrorMessage::ExpectedImplType,
        );
    }

    #[test]
    fn extension_for_type_param() {
        err(
            "
            impl[T] T {}
        ",
            (2, 13),
            ErrorMessage::ExpectedImplType,
        );
    }
}
