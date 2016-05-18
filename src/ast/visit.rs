use ast::*;
use ast::Elem::*;
use ast::Expr::*;
use ast::Stmt::*;
use ast::Type::*;

pub trait Visitor<'v> : Sized {
    fn visit_ast(&mut self, a: &'v Ast) {
        walk_ast(self, a);
    }

    fn visit_class(&mut self, c: &'v Class) {
        walk_class(self, c);
    }

    fn visit_ctor(&mut self, m: &'v Function) {
        walk_fct(self, m);
    }

    fn visit_method(&mut self, m: &'v Function) {
        walk_fct(self, m);
    }

    fn visit_prop(&mut self, p: &'v Prop) {
        walk_prop(self, p);
    }

    fn visit_fct(&mut self, f: &'v Function) {
        walk_fct(self, f);
    }

    fn visit_param(&mut self, p: &'v Param) {
        walk_param(self, p);
    }

    fn visit_type(&mut self, t: &'v Type) {
        walk_type(self, t);
    }

    fn visit_stmt(&mut self, s: &'v Stmt) {
        walk_stmt(self, s);
    }

    fn visit_expr_top(&mut self, e: &'v Expr) {
        self.visit_expr(e);
    }

    fn visit_expr(&mut self, e: &'v Expr) {
        walk_expr(self, e);
    }
}

pub fn walk_ast<'v, V: Visitor<'v>>(v: &mut V, a: &'v Ast) {
    for e in &a.elements {
        match *e {
            ElemFunction(ref f) => v.visit_fct(f),
            ElemClass(ref c) => v.visit_class(c),
        }
    }
}

pub fn walk_class<'v, V: Visitor<'v>>(v: &mut V, c: &'v Class) {
    for p in &c.props {
        v.visit_prop(p);
    }

    if let Some(ref ctor) = c.ctor {
        v.visit_ctor(ctor);
    }

    for m in &c.methods {
        v.visit_method(m);
    }
}

pub fn walk_prop<'v, V: Visitor<'v>>(v: &mut V, p: &'v Prop) {
    v.visit_type(&p.data_type);
}

pub fn walk_fct<'v, V: Visitor<'v>>(v: &mut V, f: &'v Function) {
    for p in &f.params {
        v.visit_param(p);
    }

    if let Some(ref ty) = f.return_type {
        v.visit_type(ty);
    }

    v.visit_stmt(&f.block);
}

pub fn walk_param<'v, V: Visitor<'v>>(v: &mut V, p: &'v Param) {
    v.visit_type(&p.data_type);
}

pub fn walk_type<'v, V: Visitor<'v>>(v: &mut V, t: &'v Type) {
    match *t {
        TypeSelf => { },
        TypeBasic(_) => { },
        TypeTuple(ref tuple) => {
            for ty in &tuple.subtypes {
                v.visit_type(ty);
            }
        }

        TypePtr(ref ptr) => {
            v.visit_type(&ptr.subtype);
        }

        TypeArray(ref array) => {
            v.visit_type(&array.subtype);
        }
    }
}

pub fn walk_stmt<'v, V: Visitor<'v>>(v: &mut V, s: &'v Stmt) {
    match *s {
        StmtLet(ref value) => {
            if let Some(ref ty) = value.data_type {
                v.visit_type(ty);
            }

            if let Some(ref e) = value.expr {
                v.visit_expr_top(e);
            }
        }

        StmtWhile(ref value) => {
            v.visit_expr_top(&value.cond);
            v.visit_stmt(&value.block);
        }

        StmtLoop(ref value) => {
            v.visit_stmt(&value.block);
        }

        StmtIf(ref value) => {
            v.visit_expr_top(&value.cond);
            v.visit_stmt(&value.then_block);

            if let Some(ref b) = value.else_block {
                v.visit_stmt(b);
            }
        }

        StmtExpr(ref value) => {
            v.visit_expr_top(&value.expr);
        }

        StmtBlock(ref value) => {
            for stmt in &value.stmts {
                v.visit_stmt(stmt);
            }
        }

        StmtReturn(ref value) => {
            if let Some(ref e) = value.expr {
                v.visit_expr_top(e);
            }
        }

        StmtThrow(ref value) => {
            v.visit_expr_top(&value.expr);
        }

        StmtTry(ref value) => {
            v.visit_stmt(&value.try_block);

            for catch in &value.catch_blocks {
                v.visit_type(&catch.data_type);
                v.visit_stmt(&catch.block);
            }

            if let Some(ref finally_block) = value.finally_block {
                v.visit_stmt(finally_block);
            }
        }

        StmtBreak(_) => { }
        StmtContinue(_) => { }
    }
}

pub fn walk_expr<'v, V: Visitor<'v>>(v: &mut V, e: &'v Expr) {
    match *e {
        ExprUn(ref value) => {
            v.visit_expr(&value.opnd);
        }

        ExprBin(ref value) => {
            v.visit_expr(&value.lhs);
            v.visit_expr(&value.rhs);
        }

        ExprArray(ref value) => {
            v.visit_expr(&value.object);
            v.visit_expr(&value.index);
        }

        ExprAssign(ref value) => {
            v.visit_expr(&value.lhs);
            v.visit_expr(&value.rhs);
        }

        ExprCall(ref call) => {
            for arg in &call.args {
                v.visit_expr(arg);
            }
        }

        ExprProp(ref value) => {
            v.visit_expr(&value.object);
        }

        ExprSelf(_) => {}
        ExprLitInt(_) => {}
        ExprLitStr(_) => {}
        ExprLitBool(_) => {}
        ExprIdent(_) => {}
        ExprNil(_) => {}
    }
}
