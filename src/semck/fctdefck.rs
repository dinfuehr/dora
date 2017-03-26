use ast::*;
use ast::Stmt::*;
use ast::visit::*;
use ctxt::{Context, Fct, FctId, FctParent, FctSrc, GlobalId, NodeMap};
use error::msg::Msg;
use semck;
use sym::Sym;
use ty::BuiltinType;

pub fn check<'a, 'ast>(ctxt: &Context<'ast>, map_global_defs: &NodeMap<GlobalId>) {
    for fct in ctxt.fcts.iter() {
        let mut fct = fct.borrow_mut();
        let ast = fct.ast;

        if !(fct.is_src() || fct.kind.is_definition()) {
            continue;
        }

        ctxt.sym.borrow_mut().push_level();

        match fct.parent {
            FctParent::Class(owner_class) => {
                let cls = ctxt.classes[owner_class].borrow();

                if fct.has_self() {
                    fct.param_types.push(cls.ty);
                }

                let mut type_param_id = 0;

                for &name in &cls.type_params {
                    let sym = Sym::SymTypeParam(type_param_id.into());
                    ctxt.sym.borrow_mut().insert(name, sym);
                    type_param_id += 1;
                }
            }

            FctParent::Impl(impl_id) => {
                let ximpl = ctxt.impls[impl_id].borrow();
                let cls = ctxt.classes[ximpl.cls_id()].borrow();
                fct.param_types.push(cls.ty);
            }

            FctParent::Trait(_) => {
                fct.param_types.push(BuiltinType::This);
            }

            _ => {}
        }

        for p in &ast.params {
            let ty = semck::read_type(ctxt, &p.data_type).unwrap_or(BuiltinType::Unit);

            if ty == BuiltinType::This && !fct.in_trait() {
                ctxt.diag.borrow_mut().report(p.data_type.pos(), Msg::SelfTypeUnavailable);
            }

            fct.param_types.push(ty);

            if fct.is_src() {
                let src = fct.src();
                let mut src = src.lock().unwrap();

                let var = *src.map_vars.get(p.id).unwrap();
                src.vars[var].ty = ty;
            }
        }

        if let Some(ret) = ast.return_type.as_ref() {
            let ty = semck::read_type(ctxt, ret).unwrap_or(BuiltinType::Unit);

            if ty == BuiltinType::This && !fct.in_trait() {
                ctxt.diag.borrow_mut().report(ret.pos(), Msg::SelfTypeUnavailable);
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
        let mut src = src.lock().unwrap();

        let mut defck = FctDefCheck {
            ctxt: ctxt,
            src: &mut src,
            ast: ast,
            current_type: BuiltinType::Unit,
            map_global_defs: map_global_defs,
        };

        defck.check();

        ctxt.sym.borrow_mut().pop_level();
    }
}

fn check_against_methods(ctxt: &Context, ty: BuiltinType, fct: &Fct, methods: &[FctId]) {
    for &method in methods {
        if method == fct.id {
            continue;
        }

        let method = ctxt.fcts[method].borrow();

        if method.initialized && method.name == fct.name &&
           method.params_with_self() == fct.params_with_self() {
            let cls_name = ty.name(ctxt);
            let param_names = method.params_without_self()
                .iter()
                .map(|a| a.name(ctxt))
                .collect::<Vec<String>>();
            let method_name = ctxt.interner.str(method.name).to_string();

            let msg = Msg::MethodExists(cls_name, method_name, param_names, method.pos);
            ctxt.diag.borrow_mut().report(fct.ast.pos, msg);
            return;
        }
    }
}

struct FctDefCheck<'a, 'ast: 'a> {
    ctxt: &'a Context<'ast>,
    src: &'a mut FctSrc,
    ast: &'ast Function,
    current_type: BuiltinType,
    map_global_defs: &'a NodeMap<GlobalId>,
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

    fn visit_global(&mut self, g: &'ast Global) {
        let global_id = *self.map_global_defs.get(g.id).unwrap();

        self.visit_type(&g.data_type);
        self.ctxt.globals[global_id].borrow_mut().ty = self.current_type;
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtVar(ref var) => {
                if let Some(ref data_type) = var.data_type {
                    self.visit_type(data_type);

                    let varid = *self.src
                                     .map_vars
                                     .get(var.id)
                                     .unwrap();
                    self.src.vars[varid].ty = self.current_type;
                }

                if let Some(ref expr) = var.expr {
                    visit::walk_expr(self, expr);
                }
            }

            StmtDo(ref try) => {
                for catch in &try.catch_blocks {
                    self.visit_type(&catch.data_type);
                    self.src.set_ty(catch.id, self.current_type);

                    let var = *self.src
                                   .map_vars
                                   .get(catch.id)
                                   .unwrap();
                    self.src.vars[var].ty = self.current_type;

                    if !self.current_type.reference_type() {
                        let ty = self.current_type.name(self.ctxt);
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

                visit::walk_stmt(self, s);
            }

            _ => visit::walk_stmt(self, s),
        }
    }

    fn visit_type(&mut self, t: &'ast Type) {
        self.current_type = semck::read_type(self.ctxt, t).unwrap_or(BuiltinType::Unit);
    }
}

#[cfg(test)]
mod tests {
    use error::msg::Msg;
    use semck::tests::*;

    #[test]
    fn self_param() {
        err("fun foo(x: Self) {}", pos(1, 12), Msg::SelfTypeUnavailable);
    }

    #[test]
    fn self_return_type() {
        err("fun foo() -> Self {}", pos(1, 14), Msg::SelfTypeUnavailable);
    }
}
