use std::default::Default;

use ast::Expr;
use ast::ExprType::*;
use ast::Function;
use ast::Statement;
use ast::StatementType::*;

pub trait Visitor<'a> : Sized {
    type Returns : Default;

    fn visit_fct(&mut self, fct: &'a Function) -> Self::Returns {
        self.visit_stmt(&fct.block)
    }

    fn visit_expr(&mut self, e: &Expr) -> Self::Returns {
        walk_expr(self, e)
    }

    fn visit_stmt(&mut self, s: &Statement) -> Self::Returns {
        walk_stmt(self, s)
    }
}

pub fn walk_stmt<'v, V: Visitor<'v>>(v: &mut V, s: &Statement) -> V::Returns {
    match s.stmt {
        Var(_, _, ref expr) => {
            if let Some(ref e) = *expr {
                v.visit_expr(e);
            }
        }

        While(ref cond, ref block) => {
            v.visit_expr(cond);
            v.visit_stmt(block);
        }

        Loop(ref block) => {
            v.visit_stmt(block);
        }

        If(ref cond, ref tblock, ref eblock) => {
            v.visit_expr(cond);
            v.visit_stmt(tblock);

            if let Some(ref b) = *eblock {
                v.visit_stmt(b);
            }
        }

        ExprStmt(ref e) => {
            v.visit_expr(e);
        }

        Block(ref stmts) => {
            for stmt in stmts {
                v.visit_stmt(stmt);
            }
        }

        Return(ref expr) => {
            if let Some(ref e) = *expr {
                v.visit_expr(e);
            }
        }

        Break => { }
        Continue => { }
        Nop => { }
    }

    Default::default()
}

pub fn walk_expr<'v, V: Visitor<'v>>(v: &mut V, e: &Expr) -> V::Returns {
    match e.expr {
        Un(_, ref op) => {
            v.visit_expr(op);
        }

        Bin(_, ref left, ref right) => {
            v.visit_expr(left);
            v.visit_expr(right);
        }

        Assign(ref left, ref right) => {
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

