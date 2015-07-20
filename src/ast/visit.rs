use std::default::Default;

use ast::Ast;
use ast::Elem::*;
use ast::Expr;
use ast::ExprType::*;
use ast::Function;
use ast::Stmt;
use ast::Stmt::*;

use error::ParseError;

pub trait Visitor<'v> : Sized {
    type Returns : Default;

    fn visit_ast(&mut self, a: &'v Ast) -> Result<Self::Returns, ParseError> {
        walk_ast(self, a)
    }

    fn visit_fct(&mut self, a: &'v Function) -> Result<Self::Returns, ParseError> {
        walk_stmt(self, &a.block)
    }

    fn visit_stmt(&mut self, s: &'v Stmt) -> Result<Self::Returns, ParseError> {
        walk_stmt(self, s)
    }

    fn visit_expr(&mut self, e: &'v Expr) -> Result<Self::Returns, ParseError> {
        walk_expr(self, e)
    }
}

pub fn walk_ast<'v, V: Visitor<'v>>(v: &mut V, a: &'v Ast) -> Result<V::Returns, ParseError> {
    for e in &a.elements {
        try!(match *e {
            ElemFunction(ref f) => v.visit_fct(f),
            _ => Ok(Default::default())
        });
    }

    Ok(Default::default())
}

pub fn walk_fct<'v, V: Visitor<'v>>(v: &mut V, f: &'v Function) -> Result<V::Returns, ParseError> {
    v.visit_stmt(&f.block)
}

pub fn walk_stmt<'v, V: Visitor<'v>>(v: &mut V, s: &'v Stmt) -> Result<V::Returns, ParseError> {
    match *s {
        StmtVar(ref value) => {
            if let Some(ref e) = value.expr {
                try!(v.visit_expr(e));
            }
        }

        StmtWhile(ref value) => {
            try!(v.visit_expr(&value.cond));
            try!(v.visit_stmt(&value.block));
        }

        StmtLoop(ref value) => {
            try!(v.visit_stmt(&value.block));
        }

        StmtIf(ref value) => {
            try!(v.visit_expr(&value.cond));
            try!(v.visit_stmt(&value.then_block));

            if let Some(ref b) = value.else_block {
                try!(v.visit_stmt(b));
            }
        }

        StmtExpr(ref value) => {
            try!(v.visit_expr(&value.expr));
        }

        StmtBlock(ref value) => {
            for stmt in &value.stmts {
                try!(v.visit_stmt(stmt));
            }
        }

        StmtReturn(ref value) => {
            if let Some(ref e) = value.expr {
                try!(v.visit_expr(e));
            }
        }

        StmtBreak(_) => { }
        StmtContinue(_) => { }
    }

    Ok(Default::default())
}

pub fn walk_expr<'v, V: Visitor<'v>>(v: &mut V, e: &'v Expr) -> Result<V::Returns, ParseError> {
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

