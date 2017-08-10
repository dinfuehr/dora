use std::collections::HashSet;

use dora_parser::ast::*;
use dora_parser::ast::Stmt::*;
use dora_parser::ast::visit::*;
use ctxt::{SemContext, Fct, FctId, FctParent, FctSrc};
use dora_parser::error::msg::Msg;
use semck;
use sym::Sym;
use ty::BuiltinType;

pub fn check<'a, 'ast>(ctxt: &SemContext<'ast>) {
    for fct in ctxt.fcts.iter() {
        let mut fct = fct.borrow_mut();
        let ast = fct.ast;

        // check modifiers for function
        check_abstract(ctxt, &*fct);
        check_static(ctxt, &*fct);

        if !(fct.is_src() || fct.kind.is_definition()) {
            continue;
        }

        ctxt.sym.borrow_mut().push_level();

        match fct.parent {
            FctParent::Class(owner_class) => {
                let cls = ctxt.classes[owner_class].borrow();
                let mut type_param_id = 0;

                for param in &cls.type_params {
                    let sym = Sym::SymClassTypeParam(cls.id, type_param_id.into());
                    ctxt.sym.borrow_mut().insert(param.name, sym);
                    type_param_id += 1;
                }

                if fct.has_self() {
                    fct.param_types.push(cls.ty);
                }
            }

            FctParent::Impl(impl_id) => {
                let ximpl = ctxt.impls[impl_id].borrow();
                let cls = ctxt.classes[ximpl.cls_id()].borrow();

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
                        let name = ctxt.interner.str(type_param.name).to_string();
                        let msg = Msg::TypeParamNameNotUnique(name);
                        ctxt.diag.borrow_mut().report(type_param.pos, msg);
                    }

                    fct.type_params.push(type_param.name);

                    let sym = Sym::SymFctTypeParam(fct.id, type_param_id.into());
                    ctxt.sym.borrow_mut().insert(type_param.name, sym);
                    type_param_id += 1;
                }

            } else {
                let msg = Msg::TypeParamsExpected;
                ctxt.diag.borrow_mut().report(fct.pos, msg);
            }
        }

        for p in &ast.params {
            let ty = semck::read_type(ctxt, &p.data_type).unwrap_or(BuiltinType::Unit);

            if ty == BuiltinType::This && !fct.in_trait() {
                ctxt.diag
                    .borrow_mut()
                    .report(p.data_type.pos(), Msg::SelfTypeUnavailable);
            }

            fct.param_types.push(ty);

            if fct.is_src() {
                let src = fct.src();
                let mut src = src.borrow_mut();

                let var = *src.map_vars.get(p.id).unwrap();
                src.vars[var].ty = ty;
            }
        }

        if let Some(ret) = ast.return_type.as_ref() {
            let ty = semck::read_type(ctxt, ret).unwrap_or(BuiltinType::Unit);

            if ty == BuiltinType::This && !fct.in_trait() {
                ctxt.diag
                    .borrow_mut()
                    .report(ret.pos(), Msg::SelfTypeUnavailable);
            }

            fct.return_type = ty;
        }

        fct.initialized = true;

        match fct.parent {
            FctParent::Class(clsid) => {
                let cls = ctxt.classes[clsid].borrow();
                check_against_methods(ctxt, cls.ty, &*fct, &cls.methods);
            }

            FctParent::Trait(traitid) => {
                let xtrait = ctxt.traits[traitid].borrow();
                let ty = BuiltinType::Trait(traitid);
                check_against_methods(ctxt, ty, &*fct, &xtrait.methods);
            }

            FctParent::Impl(implid) => {
                let ximpl = ctxt.impls[implid].borrow();
                let ty = BuiltinType::Trait(ximpl.trait_id());
                check_against_methods(ctxt, ty, &*fct, &ximpl.methods);
            }

            _ => {}
        }

        if !fct.is_src() {
            continue;
        }

        let src = fct.src();
        let mut src = src.borrow_mut();

        let mut defck = FctDefCheck {
            ctxt: ctxt,
            src: &mut src,
            ast: ast,
            current_type: BuiltinType::Unit,
        };

        defck.check();

        ctxt.sym.borrow_mut().pop_level();
    }
}

fn check_abstract<'ast>(ctxt: &SemContext<'ast>, fct: &Fct<'ast>) {
    if !fct.is_abstract {
        return;
    }

    let cls_id = fct.cls_id();
    let cls = ctxt.classes[cls_id].borrow();

    if !fct.kind.is_definition() {
        let msg = Msg::AbstractMethodWithImplementation;
        ctxt.diag.borrow_mut().report(fct.pos, msg);
    }

    if !cls.is_abstract {
        let msg = Msg::AbstractMethodNotInAbstractClass;
        ctxt.diag.borrow_mut().report(fct.pos, msg);
    }
}

fn check_static<'ast>(ctxt: &SemContext<'ast>, fct: &Fct<'ast>) {
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
        ctxt.diag.borrow_mut().report(fct.pos, msg);
    }
}

fn check_against_methods(ctxt: &SemContext, ty: BuiltinType, fct: &Fct, methods: &[FctId]) {
    for &method in methods {
        if method == fct.id {
            continue;
        }

        let method = ctxt.fcts[method].borrow();

        if method.initialized && method.name == fct.name && method.is_static == fct.is_static {
            let cls_name = ty.name(ctxt);
            let method_name = ctxt.interner.str(method.name).to_string();

            let msg = Msg::MethodExists(cls_name, method_name, method.pos);
            ctxt.diag.borrow_mut().report(fct.ast.pos, msg);
            return;
        }
    }
}

struct FctDefCheck<'a, 'ast: 'a> {
    ctxt: &'a SemContext<'ast>,
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

            StmtDo(ref try) => {
                visit::walk_stmt(self, s);

                for catch in &try.catch_blocks {
                    let ty = self.src.ty(catch.data_type.id());

                    let var = *self.src.map_vars.get(catch.id).unwrap();
                    self.src.vars[var].ty = ty;

                    if !ty.reference_type() {
                        let ty = ty.name(self.ctxt);
                        self.ctxt
                            .diag
                            .borrow_mut()
                            .report(catch.data_type.pos(), Msg::ReferenceTypeExpected(ty));
                    }
                }

                if try.catch_blocks.is_empty() && try.finally_block.is_none() {
                    self.ctxt
                        .diag
                        .borrow_mut()
                        .report(try.pos, Msg::CatchOrFinallyExpected);
                }
            }

            _ => visit::walk_stmt(self, s),
        }
    }

    fn visit_type(&mut self, t: &'ast Type) {
        self.current_type = semck::read_type(self.ctxt, t).unwrap_or(BuiltinType::Unit);
        self.src.set_ty(t.id(), self.current_type);
    }
}

#[cfg(test)]
mod tests {
    use dora_parser::error::msg::Msg;
    use semck::tests::*;

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
                static fun foo() {}
                fun foo() {}
            }");
    }

    #[test]
    fn fct_with_type_params() {
        ok("fun f<T>() {}");
        ok("fun f<X, Y>() {}");
        err("fun f<T, T>() {}",
            pos(1, 10),
            Msg::TypeParamNameNotUnique("T".into()));
        err("fun f<>() {}", pos(1, 1), Msg::TypeParamsExpected);
    }

    #[test]
    fn fct_with_type_param_in_annotation() {
        ok("fun f<T>(val: T) {}");
    }

    #[test]
    fn abstract_method_in_non_abstract_class() {
        err("class A { abstract fun foo(); }",
            pos(1, 20),
            Msg::AbstractMethodNotInAbstractClass);
    }

    #[test]
    fn abstract_method_with_implementation() {
        err("abstract class A { abstract fun foo() {} }",
            pos(1, 29),
            Msg::AbstractMethodWithImplementation);
    }

    #[test]
    fn abstract_static_method() {
        err("abstract class A { static abstract fun foo(); }",
            pos(1, 36),
            Msg::ModifierNotAllowedForStaticMethod("abstract".into()));
    }

    #[test]
    fn open_static_method() {
        err("abstract class A { static open fun foo() {} }",
            pos(1, 32),
            Msg::ModifierNotAllowedForStaticMethod("open".into()));
    }

    #[test]
    fn override_static_method() {
        err("abstract class A { static override fun foo() {} }",
            pos(1, 36),
            Msg::ModifierNotAllowedForStaticMethod("override".into()));
    }

    #[test]
    fn final_static_method() {
        err("abstract class A { final static fun foo() {} }",
            pos(1, 33),
            Msg::ModifierNotAllowedForStaticMethod("final".into()));
    }

    #[test]
    fn lambdas() {
        ok("fun f() { || {}; }");
        ok("fun f() { |a: int| {}; }");
        ok("fun f() { || -> int { return 2; }; }");

        err("fun f() { || -> Foo { }; }",
            pos(1, 17),
            Msg::UnknownType("Foo".into()));
        err("fun f() { |a: Foo| { }; }",
            pos(1, 15),
            Msg::UnknownType("Foo".into()));
    }
}
