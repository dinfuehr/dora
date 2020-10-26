use crate::ast::*;

use crate::ast::Expr::*;
use crate::ast::Stmt::*;
use crate::ast::Type::*;

pub trait Visitor: Sized {
    fn visit_ast(&mut self, a: &Ast) {
        walk_ast(self, a);
    }

    fn visit_file(&mut self, a: &File) {
        walk_file(self, a);
    }

    fn visit_global(&mut self, g: &Global) {
        walk_global(self, g);
    }

    fn visit_trait(&mut self, t: &Trait) {
        walk_trait(self, t);
    }

    fn visit_impl(&mut self, i: &Impl) {
        walk_impl(self, i);
    }

    fn visit_class(&mut self, c: &Arc<Class>) {
        walk_class(self, c);
    }

    fn visit_module(&mut self, m: &Module) {
        walk_module(self, m);
    }

    fn visit_struct(&mut self, s: &Struct) {
        walk_struct(self, s);
    }

    fn visit_annotation(&mut self, a: &Annotation) {
        walk_annotation(self, a);
    }

    fn visit_const(&mut self, c: &Const) {
        walk_const(self, c);
    }

    fn visit_enum(&mut self, e: &Enum) {
        walk_enum(self, e);
    }

    fn visit_alias(&mut self, e: &Alias) {
        walk_alias(self, e);
    }

    fn visit_namespace(&mut self, e: &Namespace) {
        walk_namespace(self, e);
    }

    fn visit_struct_field(&mut self, f: &StructField) {
        walk_struct_field(self, f);
    }

    fn visit_ctor(&mut self, m: Arc<Function>) {
        walk_fct(self, &m);
    }

    fn visit_method(&mut self, m: Arc<Function>) {
        walk_fct(self, &m);
    }

    fn visit_field(&mut self, p: &Field) {
        walk_field(self, p);
    }

    fn visit_fct(&mut self, f: Arc<Function>) {
        walk_fct(self, &f);
    }

    fn visit_param(&mut self, p: &Param) {
        walk_param(self, p);
    }

    fn visit_type(&mut self, t: &Type) {
        walk_type(self, t);
    }

    fn visit_stmt(&mut self, s: &Stmt) {
        walk_stmt(self, s);
    }

    fn visit_expr(&mut self, e: &Expr) {
        walk_expr(self, e);
    }
}

pub fn walk_ast<V: Visitor>(v: &mut V, a: &Ast) {
    for f in &a.files {
        v.visit_file(f);
    }
}

pub fn walk_file<V: Visitor>(v: &mut V, f: &File) {
    for e in &f.elements {
        walk_elem(v, e);
    }
}

pub fn walk_elem<V: Visitor>(v: &mut V, e: &Elem) {
    match e {
        ElemFunction(f) => v.visit_fct(f.clone()),
        ElemClass(ref c) => v.visit_class(c),
        ElemStruct(ref s) => v.visit_struct(s),
        ElemTrait(ref t) => v.visit_trait(t),
        ElemImpl(ref i) => v.visit_impl(i),
        ElemModule(ref m) => v.visit_module(m),
        ElemAnnotation(ref a) => v.visit_annotation(a),
        ElemGlobal(ref g) => v.visit_global(g),
        ElemConst(ref c) => v.visit_const(c),
        ElemEnum(ref e) => v.visit_enum(e),
        ElemAlias(ref e) => v.visit_alias(e),
        ElemNamespace(ref e) => v.visit_namespace(e),
    }
}

pub fn walk_global<V: Visitor>(v: &mut V, g: &Global) {
    v.visit_type(&g.data_type);

    if let Some(ref initializer) = g.initializer {
        v.visit_fct(initializer.clone());
    }
}

pub fn walk_trait<V: Visitor>(v: &mut V, t: &Trait) {
    for m in &t.methods {
        v.visit_method(m.clone());
    }
}

pub fn walk_impl<V: Visitor>(v: &mut V, i: &Impl) {
    for m in &i.methods {
        v.visit_method(m.clone());
    }
}

pub fn walk_class<V: Visitor>(v: &mut V, c: &Arc<Class>) {
    for f in &c.fields {
        v.visit_field(f);
    }

    if let Some(ctor) = &c.constructor {
        v.visit_ctor(ctor.clone());
    }

    for m in &c.methods {
        v.visit_method(m.clone());
    }
}

pub fn walk_module<V: Visitor>(v: &mut V, m: &Module) {
    for f in &m.fields {
        v.visit_field(f);
    }

    if let Some(ctor) = &m.constructor {
        v.visit_ctor(ctor.clone());
    }

    for m in &m.methods {
        v.visit_method(m.clone());
    }
}

pub fn walk_annotation<V: Visitor>(_v: &mut V, _a: &Annotation) {}

pub fn walk_const<V: Visitor>(v: &mut V, c: &Const) {
    v.visit_type(&c.data_type);
    v.visit_expr(&c.expr);
}

pub fn walk_enum<V: Visitor>(_v: &mut V, _e: &Enum) {
    // nothing to do
}

pub fn walk_alias<V: Visitor>(v: &mut V, a: &Alias) {
    v.visit_type(&a.ty);
}

pub fn walk_namespace<V: Visitor>(v: &mut V, namespace: &Namespace) {
    for e in &namespace.elements {
        walk_elem(v, e);
    }
}

pub fn walk_struct<V: Visitor>(v: &mut V, s: &Struct) {
    for f in &s.fields {
        v.visit_struct_field(f);
    }
}

pub fn walk_struct_field<V: Visitor>(v: &mut V, f: &StructField) {
    v.visit_type(&f.data_type);
}

pub fn walk_field<V: Visitor>(v: &mut V, f: &Field) {
    v.visit_type(&f.data_type);
}

pub fn walk_fct<V: Visitor>(v: &mut V, f: &Function) {
    for p in &f.params {
        v.visit_param(p);
    }

    if let Some(ref ty) = f.return_type {
        v.visit_type(ty);
    }

    if let Some(ref block) = f.block {
        for stmt in &block.stmts {
            v.visit_stmt(stmt);
        }

        if let Some(ref value) = block.expr {
            v.visit_expr(value);
        }
    }
}

pub fn walk_param<V: Visitor>(v: &mut V, p: &Param) {
    v.visit_type(&p.data_type);
}

pub fn walk_type<V: Visitor>(v: &mut V, t: &Type) {
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

pub fn walk_stmt<V: Visitor>(v: &mut V, s: &Stmt) {
    match *s {
        StmtLet(ref value) => {
            if let Some(ref ty) = value.data_type {
                v.visit_type(ty);
            }

            if let Some(ref e) = value.expr {
                v.visit_expr(e);
            }
        }

        StmtFor(ref value) => {
            v.visit_expr(&value.expr);
            v.visit_stmt(&value.block);
        }

        StmtWhile(ref value) => {
            v.visit_expr(&value.cond);
            v.visit_stmt(&value.block);
        }

        StmtExpr(ref value) => {
            v.visit_expr(&value.expr);
        }

        StmtReturn(ref value) => {
            if let Some(ref e) = value.expr {
                v.visit_expr(e);
            }
        }

        StmtBreak(_) => {}
        StmtContinue(_) => {}
    }
}

pub fn walk_expr<V: Visitor>(v: &mut V, e: &Expr) {
    match *e {
        ExprUn(ref value) => {
            v.visit_expr(&value.opnd);
        }

        ExprBin(ref value) => {
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
            v.visit_expr(&value.lhs);
            v.visit_expr(&value.rhs);
        }

        ExprConv(ref value) => {
            v.visit_expr(&value.object);
            v.visit_type(&value.data_type);
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

        ExprBlock(ref value) => {
            for stmt in &value.stmts {
                v.visit_stmt(stmt);
            }

            if let Some(ref expr) = value.expr {
                v.visit_expr(expr);
            }
        }

        ExprTemplate(ref value) => {
            for part in &value.parts {
                v.visit_expr(part);
            }
        }

        ExprIf(ref value) => {
            v.visit_expr(&value.cond);
            v.visit_expr(&value.then_block);

            if let Some(ref b) = value.else_block {
                v.visit_expr(b);
            }
        }

        ExprTuple(ref value) => {
            for expr in &value.values {
                v.visit_expr(expr);
            }
        }

        ExprParen(ref value) => {
            v.visit_expr(&value.expr);
        }

        ExprMatch(ref value) => {
            v.visit_expr(&value.expr);
        }

        ExprSuper(_) => {}
        ExprSelf(_) => {}
        ExprLitChar(_) => {}
        ExprLitInt(_) => {}
        ExprLitFloat(_) => {}
        ExprLitStr(_) => {}
        ExprLitBool(_) => {}
        ExprIdent(_) => {}
    }
}
