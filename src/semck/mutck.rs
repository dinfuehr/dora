use ctxt::{Context, Fct, IdentType, VarId};
use error::msg::Msg;

use ast::*;
use ast::Expr::*;
use ast::visit::*;
use lexer::position::Position;

pub fn check<'ast>(ctxt: &Context<'ast>) {
    for fct in ctxt.fcts.iter() {
        let mut fct = fct.lock().unwrap();

        if fct.kind.is_src() {
            let ast = fct.ast();
            let mut mutck = MutCheck {
                ctxt: ctxt,
                fct: &mut fct,
                ast: ast,
            };

            mutck.check();
        }
    }
}

struct MutCheck<'a, 'ast: 'a> {
    ctxt: &'a Context<'ast>,
    fct: &'a mut Fct<'ast>,
    ast: &'ast Function,
}

impl<'a, 'ast> MutCheck<'a, 'ast> {
    fn check(&mut self) {
        self.visit_fct(self.ast);
    }

    fn check_expr_assign(&mut self, e: &'ast ExprAssignType) {
        if e.lhs.is_ident() {
            self.check_expr_ident(e.lhs.to_ident().unwrap());
        }
    }

    fn check_expr_ident(&mut self, e: &'ast ExprIdentType) {
        let ident_type = *self.fct.src().defs.get(&e.id).unwrap();

        match ident_type {
            IdentType::Var(varid) => self.check_var_mutable(e.pos, varid),
            _ => {}
        }
    }

    fn check_var_mutable(&mut self, pos: Position, var_id: VarId) {
        let var = &self.fct.src().vars[var_id.0];

        if !var.mutable {
            self.ctxt.diag.borrow_mut().report(pos, Msg::VarNotMutable(var.name));
        }
    }
}

impl<'a, 'ast> Visitor<'ast> for MutCheck<'a, 'ast> {
    fn visit_expr(&mut self, e: &'ast Expr) {
        match *e {
            ExprAssign(ref expr) => self.check_expr_assign(expr),
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use error::msg::Msg;
    use interner::Name;
    use semck::tests::*;

    #[test]
    fn test_mutable_var() {
        ok("fn f() { let mut a = 1; }");
        ok("fn f() { let mut a = 1; a = 2; }");
    }

    #[test]
    fn test_non_mutable_var() {
        err("fn f() { let a = 1; a = 2; }", pos(1, 21), Msg::VarNotMutable(Name(1)));
    }

    #[test]
    fn test_param() {
        ok("fn f(mut a: int) { a = 1; }");
        err("fn f(a: int) { a = 1; }", pos(1, 16), Msg::VarNotMutable(Name(1)));
    }
}
