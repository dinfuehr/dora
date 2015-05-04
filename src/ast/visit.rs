use std::default::Default;

use ast::Expr;
use ast::ExprType::*;
use ast::Stmt;
use ast::StmtType::*;

use error::ParseError;

pub trait Visitor : Sized {
    type Returns : Default;

    fn visit_expr(&mut self, e: &Expr) -> Result<Self::Returns, ParseError> {
        walk_expr(self, e)
    }

    fn visit_stmt(&mut self, s: &Stmt) -> Result<Self::Returns, ParseError> {
        walk_stmt(self, s)
    }
}

pub fn walk_stmt<V: Visitor>(v: &mut V, s: &Stmt) -> Result<V::Returns, ParseError> {
    match s.node {
        StmtVar(_, _, ref expr) => {
            if let Some(ref e) = *expr {
                try!(v.visit_expr(e));
            }
        }

        StmtWhile(ref cond, ref block) => {
            try!(v.visit_expr(cond));
            try!(v.visit_stmt(block));
        }

        StmtLoop(ref block) => {
            try!(v.visit_stmt(block));
        }

        StmtIf(ref cond, ref tblock, ref eblock) => {
            try!(v.visit_expr(cond));
            try!(v.visit_stmt(tblock));

            if let Some(ref b) = *eblock {
                try!(v.visit_stmt(b));
            }
        }

        StmtExpr(ref e) => {
            try!(v.visit_expr(e));
        }

        StmtBlock(ref stmts) => {
            for stmt in stmts {
                try!(v.visit_stmt(stmt));
            }
        }

        StmtReturn(ref expr) => {
            if let Some(ref e) = *expr {
                try!(v.visit_expr(e));
            }
        }

        StmtBreak => { }
        StmtContinue => { }
    }

    Ok(Default::default())
}

pub fn walk_expr<V: Visitor>(v: &mut V, e: &Expr) -> Result<V::Returns, ParseError> {
    match e.node {
        ExprUn(_, ref op) => {
            try!(v.visit_expr(op));
        }

        ExprBin(_, ref left, ref right) => {
            try!(v.visit_expr(left));
            try!(v.visit_expr(right));
        }

        ExprAssign(ref left, ref right) => {
            try!(v.visit_expr(left));
            try!(v.visit_expr(right));
        }

        ExprLitInt(_) => {}
        ExprLitStr(_) => {}
        ExprLitBool(_) => {}
        ExprIdent(_) => {}
    }

    Ok(Default::default())
}

