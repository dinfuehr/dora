use ast::*;
use ast::Stmt::*;
use ast::visit::*;
use ctxt::{Context, Fct};
use error::msg::Msg;
use semck;
use ty::BuiltinType;

pub fn check<'a, 'ast>(ctxt: &Context<'ast>) {
    for fct in ctxt.fcts.iter() {
        let mut fct = fct.lock().unwrap();

        if fct.kind.is_src() {
            let ast = fct.ast();
            let mut defck = FctDefCheck {
                ctxt: ctxt,
                fct: &mut fct,
                ast: ast,
                current_type: BuiltinType::Unit,
            };

            defck.check();
        }
    }
}

struct FctDefCheck<'a, 'ast: 'a> {
    ctxt: &'a Context<'ast>,
    fct: &'a mut Fct<'ast>,
    ast: &'ast Function,
    current_type: BuiltinType,
}

impl<'a, 'ast> FctDefCheck<'a, 'ast> {
    fn check(&mut self) {
        self.visit_fct(self.ast);

        if self.ctxt.diag.borrow().has_errors() {
            return;
        }

        self.fct.initialized = true;

        if let Some(clsid) = self.fct.owner_class {
            let cls = self.ctxt.cls_by_id(clsid);

            for method in &cls.methods {
                if *method == self.fct.id { continue; }

                let method = self.ctxt.fcts[method.0].clone();
                let method = &mut method.lock().unwrap();

                if method.initialized
                   && method.name == self.fct.name
                   && method.params_types == self.fct.params_types {

                    let msg = Msg::MethodExists(
                        BuiltinType::Class(clsid), method.name,
                        method.params_types[1..].to_vec(), method.src().ast.pos);
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
            self.fct.return_type = self.current_type;
        }

        self.visit_stmt(&f.block);
    }

    fn visit_param(&mut self, p: &'ast Param) {
        if p.idx == 0 && self.fct.owner_class.is_some() && !self.fct.ctor {
            if !p.data_type.is_self() {
                self.ctxt.diag.borrow_mut().report(p.pos, Msg::SelfNeeded);
                return;
            }

            let cls_id = self.fct.owner_class.unwrap();
            self.current_type = BuiltinType::Class(cls_id);

        } else {
            if p.data_type.is_self() {
                self.ctxt.diag.borrow_mut().report(p.pos, Msg::InvalidUseOfSelf);
                return;
            }

            self.visit_type(&p.data_type);
        }

        self.fct.params_types.push(self.current_type);

        let var = self.fct.var_by_node_id_mut(p.id);
        var.data_type = self.current_type;
    }

    fn visit_stmt(&mut self, s: &'ast Stmt) {
        if let StmtLet(ref var) = *s {
            if let Some(ref data_type) = var.data_type {
                self.visit_type(data_type);

                let var = self.fct.var_by_node_id_mut(var.id);
                var.data_type = self.current_type;
            }

            if let Some(ref expr) = var.expr {
                visit::walk_expr(self, expr);
            }

        } else {
            visit::walk_stmt(self, s);
        }
    }

    fn visit_type(&mut self, t: &'ast Type) {
        self.current_type = semck::read_type(self.ctxt, t);
    }
}
