use std::cell::{Ref, RefMut};

use ast::*;
use ast::Stmt::*;
use ast::visit::*;
use ctxt::{Context, Fct, FctId, FctKind, FctSrc};
use error::msg::Msg;
use semck;
use ty::BuiltinType;

pub fn check<'a, 'ast>(ctxt: &mut Context<'ast>) {
    let last = ctxt.fcts.len();

    for id in 0..last {
        let id = FctId(id);

        let src = {
            let fct = ctxt.fct_by_id(id);
            if !fct.is_src() { continue; }

            fct.src()
        };

        let mut src = src.lock().unwrap();

        let ast = src.ast;

        let mut defck = FctDefCheck {
            ctxt: ctxt,
            fct_id: id,
            src: &mut src,
            ast: ast,
            current_type: BuiltinType::Unit,
        };

        defck.check();
    }
}

struct FctDefCheck<'a, 'ast: 'a> {
    ctxt: &'a mut Context<'ast>,
    fct_id: FctId,
    src: &'a mut FctSrc<'ast>,
    ast: &'ast Function,
    current_type: BuiltinType,
}

impl<'a, 'ast> FctDefCheck<'a, 'ast> {
    fn fct(&self) -> &Fct<'ast> {
        &self.ctxt.fcts[self.fct_id]
    }

    fn fct_mut(&mut self) -> &mut Fct<'ast> {
        &mut self.ctxt.fcts[self.fct_id]
    }

    fn check(&mut self) {
        self.visit_fct(self.ast);

        if self.ctxt.diag.borrow().has_errors() {
            return;
        }

        self.fct_mut().initialized = true;

        if let Some(clsid) = self.fct().owner_class {
            let cls = self.ctxt.cls_by_id(clsid);

            for &method in &cls.methods {
                if method == self.fct().id { continue; }
                let method = self.ctxt.fct_by_id(method);

                if method.initialized
                   && method.name == self.fct().name
                   && method.params_types == self.fct().params_types {
                    let cls_name = BuiltinType::Class(clsid).name(self.ctxt);
                    let param_names = method.params_types[1..].iter()
                        .map(|a| a.name(self.ctxt)).collect::<Vec<String>>();
                    let method_name = self.ctxt.interner.str(method.name).to_string();

                    let msg = Msg::MethodExists(
                        cls_name, method_name,
                        param_names, method.kind.src().ast.pos);
                    self.ctxt.diag.borrow_mut().report(self.ast.pos, msg);
                    return;
                }
            }
        }
    }
}

impl<'a, 'ast> Visitor<'ast> for FctDefCheck<'a, 'ast> {
    fn visit_fct(&mut self, f: &'ast Function) {
        for p in &f.params {
            self.visit_param(p);
        }

        if let Some(ref ty) = f.return_type {
            self.visit_type(ty);
            self.fct_mut().return_type = self.current_type;
        }

        self.visit_stmt(&f.block);
    }

    fn visit_param(&mut self, p: &'ast Param) {
        if p.idx == 0 && self.fct().owner_class.is_some() && !self.fct().is_ctor() {
            if !p.data_type.is_self() {
                self.ctxt.diag.borrow_mut().report(p.pos, Msg::SelfNeeded);
                return;
            }

            let cls_id = self.fct().owner_class.unwrap();
            self.current_type = BuiltinType::Class(cls_id);

        } else {
            if p.data_type.is_self() {
                self.ctxt.diag.borrow_mut().report(p.pos, Msg::InvalidUseOfSelf);
                return;
            }

            self.visit_type(&p.data_type);
        }

        let ty = self.current_type;
        self.fct_mut().params_types.push(ty);
        self.src.vars[p.var()].ty = ty;
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        match *s {
            StmtVar(ref var) => {
                if let Some(ref data_type) = var.data_type {
                    self.visit_type(data_type);

                    let varid = var.var();
                    self.src.vars[varid].ty = self.current_type;
                }

                if let Some(ref expr) = var.expr {
                    visit::walk_expr(self, expr);
                }

            }

            StmtTry(ref try) => {
                for catch in &try.catch_blocks {
                    self.visit_type(&catch.data_type);
                    catch.set_ty(self.current_type);
                    self.src.vars[catch.var()].ty = self.current_type;

                    if !self.current_type.reference_type() {
                        let ty = self.current_type.name(self.ctxt);
                        self.ctxt.diag.borrow_mut().report(catch.data_type.pos(),
                            Msg::ReferenceTypeExpected(ty));
                    }
                }

                if try.catch_blocks.is_empty() && try.finally_block.is_none() {
                    self.ctxt.diag.borrow_mut().report(try.pos, Msg::CatchOrFinallyExpected);
                }

                visit::walk_stmt(self, s);
            }

            _ => visit::walk_stmt(self, s)
        }
    }

    fn visit_type(&mut self, t: &'ast Type) {
        self.current_type = semck::read_type(self.ctxt, t);
    }
}
