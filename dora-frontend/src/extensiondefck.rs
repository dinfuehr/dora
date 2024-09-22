use std::collections::HashMap;

use crate::sema::{
    extension_matches_ty, ExtensionDefinition, FctDefinitionId, PackageDefinitionId, Sema,
    SourceFileId, TypeParamDefinition, TypeParamId,
};
use crate::{expand_type, AllowSelf, ErrorMessage, ModuleSymTable, Name, SourceType, SymbolKind};

use dora_parser::Span;
use fixedbitset::FixedBitSet;

pub fn check(sa: &Sema) {
    let mut maybe_duplicate_names: HashMap<Name, Vec<FctDefinitionId>> = HashMap::new();

    for (_id, extension) in sa.extensions.iter() {
        let mut extck = ExtensionCheck {
            sa,
            extension,
            sym: ModuleSymTable::new(sa, extension.module_id),
            maybe_duplicate_names: &mut maybe_duplicate_names,
        };

        extck.check();
    }

    for (name, fcts) in maybe_duplicate_names {
        if fcts.len() < 2 {
            continue;
        }

        for idx in 0..fcts.len() {
            let fct_id = fcts[idx];
            let fct = sa.fct(fct_id);
            let extension_id = fct.parent.extension_id().expect("extension expected");
            let extension = sa.extension(extension_id);

            for cmp_idx in idx + 1..fcts.len() {
                let cmp_fct_id = fcts[cmp_idx];
                let cmp_fct = sa.fct(cmp_fct_id);
                let cmp_extension_id = cmp_fct.parent.extension_id().expect("extension expected");
                let cmp_extension = sa.extension(cmp_extension_id);

                if extension_matches_ty(
                    sa,
                    extension.ty().clone(),
                    extension.type_params(),
                    cmp_extension.ty().clone(),
                    cmp_extension.type_params(),
                )
                .is_some()
                {
                    let method_name = sa.interner.str(name).to_string();
                    let msg = ErrorMessage::AliasExists(method_name, fct.span);
                    sa.report(extension.file_id.into(), cmp_fct.span, msg);
                }
            }
        }
    }
}

pub fn package_for_type(sa: &Sema, ty: SourceType) -> Option<PackageDefinitionId> {
    match ty {
        SourceType::Error
        | SourceType::Unit
        | SourceType::Lambda(..)
        | SourceType::Tuple(..)
        | SourceType::TypeParam(..) => None,
        SourceType::Any | SourceType::Ptr | SourceType::This => unreachable!(),
        SourceType::Bool
        | SourceType::UInt8
        | SourceType::Char
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Int32
        | SourceType::Int64 => Some(sa.stdlib_package_id()),
        SourceType::Class(id, ..) => Some(sa.class(id).package_id),
        SourceType::Struct(id, ..) => Some(sa.struct_(id).package_id),
        SourceType::Enum(id, ..) => Some(sa.enum_(id).package_id),
        SourceType::Trait(id, ..) => Some(sa.trait_(id).package_id),
        SourceType::TypeAlias(id, ..) => Some(sa.alias(id).package_id),
    }
}

struct ExtensionCheck<'x> {
    sa: &'x Sema,
    sym: ModuleSymTable,
    extension: &'x ExtensionDefinition,
    maybe_duplicate_names: &'x mut HashMap<Name, Vec<FctDefinitionId>>,
}

impl<'x> ExtensionCheck<'x> {
    fn check(&mut self) {
        assert!(self.extension.ast.trait_type.is_none());

        self.sym.push_level();

        for (id, name) in self.extension.type_params().names() {
            self.sym.insert(name, SymbolKind::TypeParam(id));
        }

        let extension_ty = expand_type(
            self.sa,
            &self.sym,
            self.extension.file_id.into(),
            &self.extension.ast.extended_type,
            self.extension.type_params(),
            AllowSelf::No,
        );

        match extension_ty {
            SourceType::TypeParam(..) => {
                let msg = ErrorMessage::ExpectedExtensionType;
                self.sa.report(
                    self.extension.file_id.into(),
                    self.extension.ast.extended_type.span(),
                    msg,
                );
            }
            SourceType::TypeAlias(..) => unimplemented!(),
            SourceType::Any | SourceType::Ptr | SourceType::This => {
                unreachable!()
            }
            SourceType::Error
            | SourceType::Bool
            | SourceType::UInt8
            | SourceType::Char
            | SourceType::Float32
            | SourceType::Float64
            | SourceType::Int32
            | SourceType::Int64
            | SourceType::Class(..)
            | SourceType::Struct(..)
            | SourceType::Enum(..)
            | SourceType::Trait(..)
            | SourceType::Unit
            | SourceType::Lambda(..)
            | SourceType::Tuple(..) => {}
        }

        let extension_ty_package_id = package_for_type(self.sa, extension_ty.clone());

        if let Some(extension_ty_package_id) = extension_ty_package_id {
            if extension_ty_package_id != self.extension.package_id {
                let msg = ErrorMessage::ExtendingTypeDifferentPackage;
                self.sa.report(
                    self.extension.file_id.into(),
                    self.extension.ast.extended_type.span(),
                    msg,
                );
            }
        }

        check_for_unconstrained_type_params(
            self.sa,
            extension_ty.clone(),
            self.extension.type_params(),
            self.extension.file_id,
            self.extension.ast.extended_type.span(),
        );

        assert!(self.extension.ty.set(extension_ty).is_ok());

        for &method_id in self.extension.methods() {
            self.visit_method(method_id);
        }

        self.sym.pop_level();
    }

    fn visit_method(&mut self, fct_id: FctDefinitionId) {
        let fct = self.sa.fct(fct_id);

        if self.extension.ty().is_error() {
            return;
        }

        self.maybe_duplicate_names
            .entry(fct.name)
            .and_modify(|v| v.push(fct_id))
            .or_insert_with(|| vec![fct_id]);

        let table = if fct.is_static {
            &self.extension.static_names
        } else {
            &self.extension.instance_names
        };

        let mut table = table.borrow_mut();

        if !table.contains_key(&fct.name) {
            table.insert(fct.name, fct_id);
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
        SourceType::TypeAlias(alias_id) => {
            discover_type_params(sa, sa.alias(alias_id).ty(), used_type_params);
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
            ErrorMessage::AliasExists("foo".into(), Span::new(17, 11)),
        );
    }

    #[test]
    fn extension_defined_twice() {
        err(
            "class A
            impl A { fn foo() {} }
            impl A { fn foo() {} }",
            (3, 22),
            ErrorMessage::AliasExists("foo".into(), Span::new(29, 11)),
        );
    }

    #[test]
    fn extension_defined_twice_with_type_params_in_class() {
        err(
            "class Foo[T]
            impl Foo[Int32] { fn foo() {} }
            impl Foo[Int32] { fn foo() {} }",
            (3, 31),
            ErrorMessage::AliasExists("foo".into(), Span::new(43, 11)),
        );

        ok("class Foo[T]
            impl Foo[Int32] { fn foo() {} }
            impl Foo[Int64] { fn foo() {} }");

        err(
            "class Foo[T]
            impl[T] Foo[T] { fn foo() {} }
            impl[T] Foo[T] { fn foo() {} }",
            (3, 30),
            ErrorMessage::AliasExists("foo".into(), Span::new(42, 11)),
        );

        // err(
        //     "class Foo[T]
        //     trait TraitA {}
        //     trait TraitB {}
        //     impl[T: TraitA] Foo[T] { fn foo() {} }
        //     impl[T: TraitB] Foo[T] { fn foo() {} }",
        //     (1, 1),
        //     ErrorMessage::Unimplemented,
        // );
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
            ErrorMessage::AliasExists("foo".into(), Span::new(35, 11)),
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
            (3, 21),
            ErrorMessage::UnconstrainedTypeParam("T".into()),
        );

        err(
            "
            struct MyFoo[T]
            impl[A, B] MyFoo[(A, A)] {}
        ",
            (3, 24),
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
    fn extension_for_alias() {
        ok("
            class X
            type Y = X;
            impl Y {
                fn testme() {}
            }
            fn f(x: X) { x.testme(); }
        ");
    }

    #[test]
    fn use_self_in_extension() {
        ok("
            class X

            impl X {
                fn take(x: Self) {}
            }

            fn f(x: X, y: X) {
                x.take(y);
            }
        ");
    }

    #[test]
    fn extension_tuple() {
        ok("
            impl (Int64, Int64) {
                fn f(): Int64 {
                    self.0
                }
            }

            fn f(x: (Int64, Int64)) {
                x.f();
            }
        ")
    }

    #[test]
    fn extension_lambda() {
        ok("
            impl (Int64, Int64): Bool {
                fn f(): Bool {
                    self(1, 2)
                }
            }

            fn f(x: (Int64, Int64): Bool): Bool {
                x.f()
            }
        ")
    }

    #[test]
    fn extension_trait() {
        ok("
            trait TraitA {
                fn foo(): Int64;
            }

            impl TraitA {
                fn plus1(): Int64 {
                    self.foo() + 1
                }
            }

            fn f(x: TraitA): Int64 {
                x.plus1()
            }
        ")
    }

    #[test]
    fn extension_type_param() {
        err(
            "
            impl[T] T {
                fn foo(): Int64 { 0 }
            }
        ",
            (2, 21),
            ErrorMessage::ExpectedExtensionType,
        )
    }
}
