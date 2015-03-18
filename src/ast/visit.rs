use std::default::Default;

use ast::Expr;
use ast::ExprType::*;
use ast::Function;
use ast::Statement;
use ast::StatementType::*;

trait Visitor : Sized {
    type Returns : Default;

    fn visit_fct(&mut self, fct: &mut Function) -> Self::Returns {
        self.visit_stmt(&mut *fct.block)
    }

    fn visit_expr(&mut self, e: &mut Expr) -> Self::Returns {
        walk_expr(self, e)
    }

    fn visit_stmt(&mut self, s: &mut Statement) -> Self::Returns {
        walk_stmt(self, s)
    }
}

fn walk_stmt<V: Visitor>(v: &mut V, s: &mut Statement) -> V::Returns {
    match s.stmt {
        Var(_, _, ref mut expr) => {
            if let Some(ref mut e) = *expr {
                v.visit_expr(e);
            }
        }

        While(ref mut cond, ref mut block) => {
            v.visit_expr(cond);
            v.visit_stmt(block);
        }

        Loop(ref mut block) => {
            v.visit_stmt(block);
        }

        If(ref mut cond, ref mut tblock, ref mut eblock) => {
            v.visit_expr(cond);
            v.visit_stmt(tblock);

            if let Some(ref mut b) = *eblock {
                v.visit_stmt(b);
            }
        }

        ExprStmt(ref mut e) => {
            v.visit_expr(e);
        }

        Block(ref mut stmts) => {
            for stmt in stmts {
                v.visit_stmt(stmt);
            }
        }

        Return(ref mut expr) => {
            if let Some(ref mut e) = *expr {
                v.visit_expr(e);
            }
        }

        Break => { }
        Continue => { }
        Nop => { }
    }

    Default::default()
}

fn walk_expr<V: Visitor>(v: &mut V, e: &mut Expr) -> V::Returns {
    match e.expr {
        Un(_, ref mut op) => {
            v.visit_expr(op);
        }

        Bin(_, ref mut left, ref mut right) => {
            v.visit_expr(left);
            v.visit_expr(right);
        }

        Assign(ref mut left, ref mut right) => {
            v.visit_expr(left);
            v.visit_expr(right);
        }

        LitInt(_) => {}
        LitStr(_) => {}
        LitTrue => {}
        LitFalse => {}
        Ident(_) => {}
    }

    Default::default()
}

#[cfg(test)]
mod tests {
    #[test]
    fn var() {
        // TODO
    }
}
