use crate::ast::*;

use crate::ast::Expr::*;
use crate::ast::Stmt::*;
use crate::ast::Type::*;

pub trait Visitor<'v>: Sized {
    fn visit_ast(&mut self, a: &'v Ast) {
        walk_ast(self, a);
    }

    fn visit_file(&mut self, a: &'v File) {
        walk_file(self, a);
    }

    fn visit_global(&mut self, g: &'v Global) {
        walk_global(self, g);
    }

    fn visit_trait(&mut self, t: &'v Trait) {
        walk_trait(self, t);
    }

    fn visit_impl(&mut self, i: &'v Impl) {
        walk_impl(self, i);
    }

    fn visit_class(&mut self, c: &'v Class) {
        walk_class(self, c);
    }

    fn visit_struct(&mut self, s: &'v Struct) {
        walk_struct(self, s);
    }

    fn visit_const(&mut self, c: &'v Const) {
        walk_const(self, c);
    }

    fn visit_struct_field(&mut self, f: &'v StructField) {
        walk_struct_field(self, f);
    }

    fn visit_ctor(&mut self, m: &'v Function) {
        walk_fct(self, m);
    }

    fn visit_method(&mut self, m: &'v Function) {
        walk_fct(self, m);
    }

    fn visit_field(&mut self, p: &'v Field) {
        walk_field(self, p);
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
    for f in &a.files {
        v.visit_file(f);
    }
}

pub fn walk_file<'v, V: Visitor<'v>>(v: &mut V, f: &'v File) {
    for e in &f.elements {
        match *e {
            ElemFunction(ref f) => v.visit_fct(f),
            ElemClass(ref c) => v.visit_class(c),
            ElemStruct(ref s) => v.visit_struct(s),
            ElemTrait(ref t) => v.visit_trait(t),
            ElemImpl(ref i) => v.visit_impl(i),
            ElemGlobal(ref g) => v.visit_global(g),
            ElemConst(ref c) => v.visit_const(c),
        }
    }
}

pub fn walk_global<'v, V: Visitor<'v>>(v: &mut V, g: &'v Global) {
    v.visit_type(&g.data_type);

    if let Some(ref expr) = g.expr {
        v.visit_expr_top(expr);
    }
}

pub fn walk_trait<'v, V: Visitor<'v>>(v: &mut V, t: &'v Trait) {
    for m in &t.methods {
        v.visit_method(m);
    }
}

pub fn walk_impl<'v, V: Visitor<'v>>(v: &mut V, i: &'v Impl) {
    for m in &i.methods {
        v.visit_method(m);
    }
}

pub fn walk_class<'v, V: Visitor<'v>>(v: &mut V, c: &'v Class) {
    for f in &c.fields {
        v.visit_field(f);
    }

    if let Some(ctor) = &c.constructor {
        v.visit_ctor(ctor);
    }

    for m in &c.methods {
        v.visit_method(m);
    }
}

pub fn walk_const<'v, V: Visitor<'v>>(v: &mut V, c: &'v Const) {
    v.visit_type(&c.data_type);
    v.visit_expr_top(&c.expr);
}

pub fn walk_struct<'v, V: Visitor<'v>>(v: &mut V, s: &'v Struct) {
    for f in &s.fields {
        v.visit_struct_field(f);
    }
}

pub fn walk_struct_field<'v, V: Visitor<'v>>(v: &mut V, f: &'v StructField) {
    v.visit_type(&f.data_type);
}

pub fn walk_field<'v, V: Visitor<'v>>(v: &mut V, f: &'v Field) {
    v.visit_type(&f.data_type);
}

pub fn walk_fct<'v, V: Visitor<'v>>(v: &mut V, f: &'v Function) {
    for p in &f.params {
        v.visit_param(p);
    }

    if let Some(ref ty) = f.return_type {
        v.visit_type(ty);
    }

    if let Some(ref block) = f.block {
        v.visit_stmt(block);
    }
}

pub fn walk_param<'v, V: Visitor<'v>>(v: &mut V, p: &'v Param) {
    v.visit_type(&p.data_type);
}

pub fn walk_type<'v, V: Visitor<'v>>(v: &mut V, t: &'v Type) {
    match *t {
        TypeSelf(_) => {}
        TypeBasic(_) => {}
        TypeTuple(ref tuple) => {
            for ty in &tuple.subtypes {
                v.visit_type(ty);
            }
        }

        TypeLambda(ref fct) => {
            for ty in &fct.params {
                v.visit_type(ty);
            }

            v.visit_type(&fct.ret);
        }
    }
}

pub fn walk_stmt<'v, V: Visitor<'v>>(v: &mut V, s: &'v Stmt) {
    match *s {
        StmtVar(ref value) => {
            if let Some(ref ty) = value.data_type {
                v.visit_type(ty);
            }

            if let Some(ref e) = value.expr {
                v.visit_expr_top(e);
            }
        }

        StmtFor(ref value) => {
            v.visit_expr_top(&value.expr);
            v.visit_stmt(&value.block);
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

        StmtDefer(ref value) => {
            v.visit_expr_top(&value.expr);
        }

        StmtDo(ref value) => {
            v.visit_stmt(&value.do_block);

            for catch in &value.catch_blocks {
                v.visit_type(&catch.data_type);
                v.visit_stmt(&catch.block);
            }

            if let Some(ref finally_block) = value.finally_block {
                v.visit_stmt(&finally_block.block);
            }
        }

        StmtSpawn(ref value) => {
            v.visit_expr_top(&value.expr);
        }

        StmtBreak(_) => {}
        StmtContinue(_) => {}
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

        ExprAssign(ref value) => {
            v.visit_expr(&value.lhs);
            v.visit_expr(&value.rhs);
        }

        ExprCall(ref call) => {
            v.visit_expr(&call.callee);

            for arg in &call.args {
                v.visit_expr(arg);
            }
        }

        ExprTypeParam(ref expr) => {
            v.visit_expr(&expr.callee);

            for arg in &expr.args {
                v.visit_type(arg);
            }
        }

        ExprPath(ref path) => {
            v.visit_expr(&path.lhs);
            v.visit_expr(&path.rhs);
        }

        ExprDelegation(ref call) => {
            for arg in &call.args {
                v.visit_expr(arg);
            }
        }

        ExprDot(ref value) => {
            v.visit_expr(&value.object);
        }

        ExprConv(ref value) => {
            v.visit_expr(&value.object);
            v.visit_type(&value.data_type);
        }

        ExprTry(ref value) => {
            v.visit_expr(&value.expr);
        }

        ExprLambda(ref value) => {
            for param in &value.params {
                v.visit_type(&param.data_type);
            }

            if let Some(ref ret) = value.ret {
                v.visit_type(ret);
            }

            v.visit_stmt(&value.block);
        }

        ExprTemplate(ref value) => {
            for part in &value.parts {
                v.visit_expr(part);
            }
        }

        ExprSuper(_) => {}
        ExprSelf(_) => {}
        ExprLitChar(_) => {}
        ExprLitInt(_) => {}
        ExprLitFloat(_) => {}
        ExprLitStr(_) => {}
        ExprLitBool(_) => {}
        ExprIdent(_) => {}
        ExprNil(_) => {}
    }
}
