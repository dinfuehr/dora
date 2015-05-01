use std::default::Default;

use ast::Expr;
use ast::ExprType::*;
use ast::Function;
use ast::Stmt;
use ast::StmtType::*;

pub trait Visitor<'a> : Sized {
    type Returns : Default;

    fn visit_expr(&mut self, e: &Expr) -> Self::Returns {
        walk_expr(self, e)
    }

    fn visit_stmt(&mut self, s: &Stmt) -> Self::Returns {
        walk_stmt(self, s)
    }
}

pub fn walk_stmt<'v, V: Visitor<'v>>(v: &mut V, s: &Stmt) -> V::Returns {
    match s.node {
        StmtVar(_, _, ref expr) => {
            if let Some(ref e) = *expr {
                v.visit_expr(e);
            }
        }

        StmtWhile(ref cond, ref block) => {
            v.visit_expr(cond);
            v.visit_stmt(block);
        }

        StmtLoop(ref block) => {
            v.visit_stmt(block);
        }

        StmtIf(ref cond, ref tblock, ref eblock) => {
            v.visit_expr(cond);
            v.visit_stmt(tblock);

            if let Some(ref b) = *eblock {
                v.visit_stmt(b);
            }
        }

        StmtExpr(ref e) => {
            v.visit_expr(e);
        }

        StmtBlock(ref stmts) => {
            for stmt in stmts {
                v.visit_stmt(stmt);
            }
        }

        StmtReturn(ref expr) => {
            if let Some(ref e) = *expr {
                v.visit_expr(e);
            }
        }

        StmtBreak => { }
        StmtContinue => { }
    }

    Default::default()
}

pub fn walk_expr<'v, V: Visitor<'v>>(v: &mut V, e: &Expr) -> V::Returns {
    match e.node {
        ExprUn(_, ref op) => {
            v.visit_expr(op);
        }

        ExprBin(_, ref left, ref right) => {
            v.visit_expr(left);
            v.visit_expr(right);
        }

        ExprAssign(ref left, ref right) => {
            v.visit_expr(left);
            v.visit_expr(right);
        }

        ExprLitInt(_) => {}
        ExprLitStr(_) => {}
        ExprLitBool(_) => {}
        ExprIdent(_) => {}
    }

    Default::default()
}

