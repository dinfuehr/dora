use std::collections::HashSet;

use crate::error::msg::SemError;
use crate::semck::{self, AllowSelf, TypeParamContext};
use crate::sym::{NestedSymTable, Sym};
use crate::ty::SourceType;
use crate::utils::iter_some;
use crate::vm::{self, Fct, FctId, FctParent, TypeParamId, VM};

pub fn check(vm: &VM) {
    for fct in vm.fcts.iter() {
        let mut fct = fct.write();
        let ast = fct.ast.clone();

        // check modifiers for function
        check_abstract(vm, &*fct);
        check_static(vm, &*fct);

        let mut sym_table = NestedSymTable::new(vm, fct.namespace_id);
        sym_table.push_level();

        match fct.parent {
            FctParent::Class(owner_class) => {
                let cls = vm.classes.idx(owner_class);
                let cls = cls.read();

                for (type_param_id, param) in cls.type_params.iter().enumerate() {
                    let sym = Sym::TypeParam(TypeParamId(type_param_id));
                    sym_table.insert(param.name, sym);
                    fct.type_params.push(param.clone());
                }

                if fct.has_self() {
                    fct.param_types.push(cls.ty());
                }
            }

            FctParent::Impl(impl_id) => {
                let ximpl = vm.impls[impl_id].read();

                for (type_param_id, param) in ximpl.type_params.iter().enumerate() {
                    let sym = Sym::TypeParam(TypeParamId(type_param_id));
                    sym_table.insert(param.name, sym);
                    fct.type_params.push(param.clone());
                }

                if fct.has_self() {
                    fct.param_types.push(ximpl.ty.clone());
                }
            }

            FctParent::Extension(extension_id) => {
                let extension = vm.extensions[extension_id].read();

                for (type_param_id, param) in extension.type_params.iter().enumerate() {
                    let sym = Sym::TypeParam(TypeParamId(type_param_id));
                    sym_table.insert(param.name, sym);
                    fct.type_params.push(param.clone());
                }

                if fct.has_self() {
                    fct.param_types.push(extension.ty.clone());
                }
            }

            FctParent::Module(_) => {}

            FctParent::Trait(trait_id) => {
                let xtrait = vm.traits[trait_id].read();

                for (type_param_id, param) in xtrait.type_params.iter().enumerate() {
                    let sym = Sym::TypeParam(TypeParamId(type_param_id));
                    sym_table.insert(param.name, sym);
                    fct.type_params.push(param.clone());
                }

                if fct.has_self() {
                    fct.param_types.push(SourceType::This);
                }
            }

            FctParent::None => {}
        }

        let container_type_params = fct.type_params.len();
        fct.container_type_params = container_type_params;

        if let Some(ref type_params) = ast.type_params {
            if type_params.len() > 0 {
                let mut names = HashSet::new();

                for (type_param_id, type_param) in type_params.iter().enumerate() {
                    if !names.insert(type_param.name) {
                        let name = vm.interner.str(type_param.name).to_string();
                        let msg = SemError::TypeParamNameNotUnique(name);
                        vm.diag.lock().report(fct.file_id, type_param.pos, msg);
                    }

                    fct.type_params.push(vm::TypeParam::new(type_param.name));

                    for bound in &type_param.bounds {
                        let ty = semck::read_type(
                            vm,
                            &sym_table,
                            fct.file_id,
                            bound,
                            TypeParamContext::Fct(&*fct),
                            AllowSelf::No,
                        );

                        match ty {
                            Some(SourceType::Trait(trait_id, _)) => {
                                if !fct.type_params[container_type_params + type_param_id]
                                    .trait_bounds
                                    .insert(trait_id)
                                {
                                    let msg = SemError::DuplicateTraitBound;
                                    vm.diag.lock().report(fct.file_id, type_param.pos, msg);
                                }
                            }

                            None => {
                                // unknown type, error is already thrown
                            }

                            _ => {
                                let msg = SemError::BoundExpected;
                                vm.diag.lock().report(fct.file_id, bound.pos(), msg);
                            }
                        }
                    }

                    let sym = Sym::TypeParam(TypeParamId(container_type_params + type_param_id));
                    sym_table.insert(type_param.name, sym);
                }
            } else {
                let msg = SemError::TypeParamsExpected;
                vm.diag.lock().report(fct.file_id, fct.pos, msg);
            }
        }

        for p in iter_some(&ast.params) {
            if fct.variadic_arguments {
                vm.diag
                    .lock()
                    .report(fct.file_id, p.pos, SemError::VariadicParameterNeedsToBeLast);
            }

            let ty = semck::read_type(
                vm,
                &sym_table,
                fct.file_id,
                &p.data_type,
                TypeParamContext::Fct(&*fct),
                if fct.in_trait() {
                    AllowSelf::Yes
                } else {
                    AllowSelf::No
                },
            )
            .unwrap_or(SourceType::Error);

            fct.param_types.push(ty);

            if p.variadic {
                fct.variadic_arguments = true;
            }
        }

        if let Some(ret) = ast.return_type.as_ref() {
            let ty = semck::read_type(
                vm,
                &sym_table,
                fct.file_id,
                ret,
                TypeParamContext::Fct(&*fct),
                if fct.in_trait() {
                    AllowSelf::Yes
                } else {
                    AllowSelf::No
                },
            )
            .unwrap_or(SourceType::Error);

            fct.return_type = ty;
        } else {
            fct.return_type = SourceType::Unit;
        }

        fct.initialized = true;

        match fct.parent {
            FctParent::Class(clsid) => {
                let cls = vm.classes.idx(clsid);
                let cls = cls.read();
                check_against_methods(vm, &*fct, &cls.methods);
            }

            FctParent::Trait(traitid) => {
                let xtrait = vm.traits[traitid].read();
                check_against_methods(vm, &*fct, &xtrait.methods);
            }

            FctParent::Impl(implid) => {
                let ximpl = vm.impls[implid].read();
                check_against_methods(vm, &*fct, &ximpl.methods);
            }

            FctParent::Module(modid) => {
                let module = vm.modules.idx(modid);
                let module = module.read();
                check_against_methods(vm, &*fct, &module.methods);
            }

            _ => {}
        }
    }
}

fn check_abstract(vm: &VM, fct: &Fct) {
    if !fct.is_abstract {
        return;
    }

    let cls_id = fct.cls_id();
    let cls = vm.classes.idx(cls_id);
    let cls = cls.read();

    if fct.has_body() {
        let msg = SemError::AbstractMethodWithImplementation;
        vm.diag.lock().report(fct.file_id, fct.pos, msg);
    }

    if !cls.is_abstract {
        let msg = SemError::AbstractMethodNotInAbstractClass;
        vm.diag.lock().report(fct.file_id, fct.pos, msg);
    }
}

fn check_static(vm: &VM, fct: &Fct) {
    if !fct.is_static {
        return;
    }

    // static isn't allowed with these modifiers
    if fct.is_abstract || fct.has_open || fct.has_override || fct.has_final {
        let modifier = if fct.is_abstract {
            "abstract"
        } else if fct.has_open {
            "open"
        } else if fct.has_override {
            "override"
        } else {
            "final"
        };

        let msg = SemError::ModifierNotAllowedForStaticMethod(modifier.into());
        vm.diag.lock().report(fct.file_id, fct.pos, msg);
    }
}

fn check_against_methods(vm: &VM, fct: &Fct, methods: &[FctId]) {
    for &method in methods {
        if method == fct.id {
            continue;
        }

        let method = vm.fcts.idx(method);
        let method = method.read();

        if method.initialized && method.name == fct.name && method.is_static == fct.is_static {
            let method_name = vm.interner.str(method.name).to_string();

            let msg = SemError::MethodExists(method_name, method.pos);
            vm.diag.lock().report(fct.file_id, fct.ast.pos, msg);
            return;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::error::msg::SemError;
    use crate::semck::tests::*;

    #[test]
    fn self_param() {
        err(
            "fun foo(x: Self) {}",
            pos(1, 12),
            SemError::SelfTypeUnavailable,
        );
    }

    #[test]
    fn self_return_type() {
        err(
            "fun foo(): Self {}",
            pos(1, 12),
            SemError::SelfTypeUnavailable,
        );
    }

    #[test]
    fn allow_same_method_as_static_and_non_static() {
        err(
            "class Foo {
                @static fun foo() {}
                fun foo() {}
            }",
            pos(3, 17),
            SemError::MethodExists("foo".into(), pos(2, 25)),
        );
    }

    #[test]
    fn fct_with_type_params() {
        ok("fun f[T]() {}");
        ok("fun f[X, Y]() {}");
        err(
            "fun f[T, T]() {}",
            pos(1, 10),
            SemError::TypeParamNameNotUnique("T".into()),
        );
        err("fun f[]() {}", pos(1, 1), SemError::TypeParamsExpected);
    }

    #[test]
    fn fct_with_type_param_in_annotation() {
        ok("fun f[T](val: T) {}");
    }

    #[test]
    fn abstract_method_in_non_abstract_class() {
        err(
            "class A { @abstract fun foo(); }",
            pos(1, 21),
            SemError::AbstractMethodNotInAbstractClass,
        );
    }

    #[test]
    fn abstract_method_with_implementation() {
        err(
            "@abstract class A { @abstract fun foo() {} }",
            pos(1, 31),
            SemError::AbstractMethodWithImplementation,
        );
    }

    #[test]
    fn abstract_static_method() {
        err(
            "@abstract class A { @static @abstract fun foo(); }",
            pos(1, 39),
            SemError::ModifierNotAllowedForStaticMethod("abstract".into()),
        );
    }

    #[test]
    fn open_static_method() {
        err(
            "@abstract class A { @static @open fun foo() {} }",
            pos(1, 35),
            SemError::ModifierNotAllowedForStaticMethod("open".into()),
        );
    }

    #[test]
    fn override_static_method() {
        err(
            "@abstract class A { @static @override fun foo() {} }",
            pos(1, 39),
            SemError::ModifierNotAllowedForStaticMethod("override".into()),
        );
    }

    #[test]
    fn final_static_method() {
        err(
            "@abstract class A { @final @static fun foo() {} }",
            pos(1, 36),
            SemError::ModifierNotAllowedForStaticMethod("final".into()),
        );
    }

    #[test]
    fn lambdas() {
        ok("fun f() { || {}; }");
        ok("fun f() { |a: Int32| {}; }");
        ok("fun f() { || -> Int32 { return 2; }; }");

        err(
            "fun f() { || -> Foo { }; }",
            pos(1, 17),
            SemError::UnknownIdentifier("Foo".into()),
        );
        err(
            "fun f() { |a: Foo| { }; }",
            pos(1, 15),
            SemError::UnknownIdentifier("Foo".into()),
        );
    }

    #[test]
    fn generic_bounds() {
        err(
            "fun f[T: Foo]() {}",
            pos(1, 10),
            SemError::UnknownIdentifier("Foo".into()),
        );
        err(
            "class Foo fun f[T: Foo]() {}",
            pos(1, 20),
            SemError::BoundExpected,
        );
        ok("trait Foo {} fun f[T: Foo]() {}");

        err(
            "trait Foo {}
            fun f[T: Foo + Foo]() {  }",
            pos(2, 19),
            SemError::DuplicateTraitBound,
        );
    }

    #[test]
    fn check_previous_defined_type_params() {
        // Type params need to be cleaned up such that the following code is an error:
        err(
            "fun f(a: T) {}",
            pos(1, 10),
            SemError::UnknownIdentifier("T".into()),
        );
    }
}
