use std::collections::HashSet;

use crate::semck;
use crate::sym::Sym;
use crate::ty::BuiltinType;
use crate::vm::{self, Fct, FctId, FctParent, FctSrc, VM};
use dora_parser::ast::visit::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::*;
use dora_parser::error::msg::Msg;

pub fn check<'a, 'ast>(vm: &VM<'ast>) {
    debug_assert!(vm.sym.lock().levels() == 1);

    for fct in vm.fcts.iter() {
        let mut fct = fct.write();
        let ast = fct.ast;

        // check modifiers for function
        check_abstract(vm, &*fct);
        check_static(vm, &*fct);

        if !(fct.is_src() || fct.kind.is_definition()) {
            continue;
        }

        vm.sym.lock().push_level();

        match fct.parent {
            FctParent::Class(owner_class) => {
                let cls = vm.classes.idx(owner_class);
                let cls = cls.read();
                let mut type_param_id = 0;

                for param in &cls.type_params {
                    let sym = Sym::SymClassTypeParam(cls.id, type_param_id.into());
                    vm.sym.lock().insert(param.name, sym);
                    type_param_id += 1;
                }

                if fct.has_self() {
                    fct.param_types.push(cls.ty);
                }
            }

            FctParent::Impl(impl_id) => {
                let ximpl = vm.impls[impl_id].read();
                let cls = vm.classes.idx(ximpl.cls_id());
                let cls = cls.read();

                if fct.has_self() {
                    fct.param_types.push(cls.ty);
                }
            }

            FctParent::Trait(_) => {
                if fct.has_self() {
                    fct.param_types.push(BuiltinType::This);
                }
            }

            FctParent::None => {}
        }

        if let Some(ref type_params) = ast.type_params {
            if type_params.len() > 0 {
                let mut names = HashSet::new();
                let mut type_param_id = 0;

                for type_param in type_params {
                    if !names.insert(type_param.name) {
                        let name = vm.interner.str(type_param.name).to_string();
                        let msg = Msg::TypeParamNameNotUnique(name);
                        vm.diag.lock().report_without_path(type_param.pos, msg);
                    }

                    fct.type_params.push(vm::TypeParam::new(type_param.name));

                    for bound in &type_param.bounds {
                        let ty = semck::read_type(vm, bound);

                        match ty {
                            Some(BuiltinType::Class(cls_id, _)) => {
                                if let None = fct.type_params[type_param_id].class_bound {
                                    fct.type_params[type_param_id].class_bound = Some(cls_id);
                                } else {
                                    let msg = Msg::MultipleClassBounds;
                                    vm.diag.lock().report_without_path(type_param.pos, msg);
                                }
                            }

                            Some(BuiltinType::Trait(trait_id)) => {
                                if !fct.type_params[type_param_id].trait_bounds.insert(trait_id) {
                                    let msg = Msg::DuplicateTraitBound;
                                    vm.diag.lock().report_without_path(type_param.pos, msg);
                                }
                            }

                            None => {
                                // unknown type, error is already thrown
                            }

                            _ => {
                                let msg = Msg::BoundExpected;
                                vm.diag.lock().report_without_path(bound.pos(), msg);
                            }
                        }
                    }

                    let sym = Sym::SymFctTypeParam(fct.id, type_param_id.into());
                    vm.sym.lock().insert(type_param.name, sym);
                    type_param_id += 1;
                }
            } else {
                let msg = Msg::TypeParamsExpected;
                vm.diag.lock().report_without_path(fct.pos, msg);
            }
        }

        for p in &ast.params {
            let ty = semck::read_type(vm, &p.data_type).unwrap_or(BuiltinType::Unit);

            if ty == BuiltinType::This && !fct.in_trait() {
                vm.diag
                    .lock()
                    .report_without_path(p.data_type.pos(), Msg::SelfTypeUnavailable);
            }

            fct.param_types.push(ty);

            if fct.is_src() {
                let src = fct.src();
                let mut src = src.write();

                let var = *src.map_vars.get(p.id).unwrap();
                src.vars[var].ty = ty;
            }
        }

        if let Some(ret) = ast.return_type.as_ref() {
            let ty = semck::read_type(vm, ret).unwrap_or(BuiltinType::Unit);

            if ty == BuiltinType::This && !fct.in_trait() {
                vm.diag
                    .lock()
                    .report_without_path(ret.pos(), Msg::SelfTypeUnavailable);
            }

            fct.return_type = ty;
        }

        fct.initialized = true;

        match fct.parent {
            FctParent::Class(clsid) => {
                let cls = vm.classes.idx(clsid);
                let cls = cls.read();
                check_against_methods(vm, cls.ty, &*fct, &cls.methods);
            }

            FctParent::Trait(traitid) => {
                let xtrait = vm.traits[traitid].read();
                let ty = BuiltinType::Trait(traitid);
                check_against_methods(vm, ty, &*fct, &xtrait.methods);
            }

            FctParent::Impl(implid) => {
                let ximpl = vm.impls[implid].read();
                let ty = BuiltinType::Trait(ximpl.trait_id());
                check_against_methods(vm, ty, &*fct, &ximpl.methods);
            }

            _ => {}
        }

        if !fct.is_src() {
            vm.sym.lock().pop_level();
            continue;
        }

        let src = fct.src();
        let mut src = src.write();

        let mut defck = FctDefCheck {
            vm: vm,
            src: &mut src,
            ast: ast,
            current_type: BuiltinType::Unit,
        };

        defck.check();

        vm.sym.lock().pop_level();
    }

    debug_assert!(vm.sym.lock().levels() == 1);
}

fn check_abstract<'ast>(vm: &VM<'ast>, fct: &Fct<'ast>) {
    if !fct.is_abstract {
        return;
    }

    let cls_id = fct.cls_id();
    let cls = vm.classes.idx(cls_id);
    let cls = cls.read();

    if !fct.kind.is_definition() {
        let msg = Msg::AbstractMethodWithImplementation;
        vm.diag.lock().report_without_path(fct.pos, msg);
    }

    if !cls.is_abstract {
        let msg = Msg::AbstractMethodNotInAbstractClass;
        vm.diag.lock().report_without_path(fct.pos, msg);
    }
}

fn check_static<'ast>(vm: &VM<'ast>, fct: &Fct<'ast>) {
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

        let msg = Msg::ModifierNotAllowedForStaticMethod(modifier.into());
        vm.diag.lock().report_without_path(fct.pos, msg);
    }
}

fn check_against_methods(vm: &VM, ty: BuiltinType, fct: &Fct, methods: &[FctId]) {
    for &method in methods {
        if method == fct.id {
            continue;
        }

        let method = vm.fcts.idx(method);
        let method = method.read();

        if method.initialized && method.name == fct.name && method.is_static == fct.is_static {
            let cls_name = ty.name(vm);
            let method_name = vm.interner.str(method.name).to_string();

            let msg = Msg::MethodExists(cls_name, method_name, method.pos);
            vm.diag.lock().report_without_path(fct.ast.pos, msg);
            return;
        }
    }
}

struct FctDefCheck<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    src: &'a mut FctSrc,
    ast: &'ast Function,
    current_type: BuiltinType,
}

impl<'a, 'ast> FctDefCheck<'a, 'ast> {
    fn check(&mut self) {
        self.visit_fct(self.ast);
    }
}

impl<'a, 'ast> Visitor<'ast> for FctDefCheck<'a, 'ast> {
    fn visit_fct(&mut self, f: &'ast Function) {
        self.visit_stmt(f.block());
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtVar(ref var) => {
                if let Some(ref data_type) = var.data_type {
                    self.visit_type(data_type);

                    let varid = *self.src.map_vars.get(var.id).unwrap();
                    self.src.vars[varid].ty = self.current_type;
                }

                if let Some(ref expr) = var.expr {
                    visit::walk_expr(self, expr);
                }
            }

            StmtDo(ref r#try) => {
                visit::walk_stmt(self, s);

                for catch in &r#try.catch_blocks {
                    let ty = self.src.ty(catch.data_type.id());

                    let var = *self.src.map_vars.get(catch.id).unwrap();
                    self.src.vars[var].ty = ty;

                    if !ty.reference_type() {
                        let ty = ty.name(self.vm);
                        self.vm.diag.lock().report_without_path(
                            catch.data_type.pos(),
                            Msg::ReferenceTypeExpected(ty),
                        );
                    }
                }

                if r#try.catch_blocks.is_empty() && r#try.finally_block.is_none() {
                    self.vm
                        .diag
                        .lock()
                        .report_without_path(r#try.pos, Msg::CatchOrFinallyExpected);
                }
            }

            _ => visit::walk_stmt(self, s),
        }
    }

    fn visit_type(&mut self, t: &'ast Type) {
        self.current_type = semck::read_type(self.vm, t).unwrap_or(BuiltinType::Unit);
        self.src.set_ty(t.id(), self.current_type);
    }
}

#[cfg(test)]
mod tests {
    use crate::semck::tests::*;
    use dora_parser::error::msg::Msg;

    #[test]
    fn self_param() {
        err("fun foo(x: Self) {}", pos(1, 12), Msg::SelfTypeUnavailable);
    }

    #[test]
    fn self_return_type() {
        err("fun foo() -> Self {}", pos(1, 14), Msg::SelfTypeUnavailable);
    }

    #[test]
    fn allow_same_method_as_static_and_non_static() {
        ok("class Foo {
                @static fun foo() {}
                fun foo() {}
            }");
    }

    #[test]
    fn fct_with_type_params() {
        ok("fun f[T]() {}");
        ok("fun f[X, Y]() {}");
        err(
            "fun f[T, T]() {}",
            pos(1, 10),
            Msg::TypeParamNameNotUnique("T".into()),
        );
        err("fun f[]() {}", pos(1, 1), Msg::TypeParamsExpected);
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
            Msg::AbstractMethodNotInAbstractClass,
        );
    }

    #[test]
    fn abstract_method_with_implementation() {
        err(
            "@abstract class A { @abstract fun foo() {} }",
            pos(1, 31),
            Msg::AbstractMethodWithImplementation,
        );
    }

    #[test]
    fn abstract_static_method() {
        err(
            "@abstract class A { @static @abstract fun foo(); }",
            pos(1, 39),
            Msg::ModifierNotAllowedForStaticMethod("abstract".into()),
        );
    }

    #[test]
    fn open_static_method() {
        err(
            "@abstract class A { @static @open fun foo() {} }",
            pos(1, 35),
            Msg::ModifierNotAllowedForStaticMethod("open".into()),
        );
    }

    #[test]
    fn override_static_method() {
        err(
            "@abstract class A { @static @override fun foo() {} }",
            pos(1, 39),
            Msg::ModifierNotAllowedForStaticMethod("override".into()),
        );
    }

    #[test]
    fn final_static_method() {
        err(
            "@abstract class A { @final @static fun foo() {} }",
            pos(1, 36),
            Msg::ModifierNotAllowedForStaticMethod("final".into()),
        );
    }

    #[test]
    fn lambdas() {
        ok("fun f() { || {}; }");
        ok("fun f() { |a: Int| {}; }");
        ok("fun f() { || -> Int { return 2; }; }");

        err(
            "fun f() { || -> Foo { }; }",
            pos(1, 17),
            Msg::UnknownType("Foo".into()),
        );
        err(
            "fun f() { |a: Foo| { }; }",
            pos(1, 15),
            Msg::UnknownType("Foo".into()),
        );
    }

    #[test]
    fn generic_bounds() {
        err(
            "fun f[T: Foo]() {}",
            pos(1, 10),
            Msg::UnknownType("Foo".into()),
        );
        ok("class Foo fun f[T: Foo]() {}");
        ok("trait Foo {} fun f[T: Foo]() {}");

        err(
            "class A class B
            fun f[T: A + B]() {  }",
            pos(2, 19),
            Msg::MultipleClassBounds,
        );
        err(
            "trait Foo {}
            fun f[T: Foo + Foo]() {  }",
            pos(2, 19),
            Msg::DuplicateTraitBound,
        );
    }

    #[test]
    fn check_previous_defined_type_params() {
        // defaultValue<T>() defines T and needs to be cleaned up again,
        // such that this fct definition is reported as an error
        err("fun f(a: T) {}", pos(1, 10), Msg::UnknownType("T".into()));
    }
}
