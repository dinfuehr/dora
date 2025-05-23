use crate::ast::*;

pub trait Visitor: Sized {
    fn visit_file(&mut self, a: &File) {
        walk_file(self, a);
    }

    fn visit_extern(&mut self, stmt: &Arc<ExternPackage>) {
        walk_extern(self, stmt);
    }

    fn visit_global(&mut self, g: &Arc<Global>) {
        walk_global(self, g);
    }

    fn visit_trait(&mut self, t: &Arc<Trait>) {
        walk_trait(self, t);
    }

    fn visit_impl(&mut self, i: &Arc<Impl>) {
        walk_impl(self, i);
    }

    fn visit_class(&mut self, c: &Arc<Class>) {
        walk_class(self, c);
    }

    fn visit_struct(&mut self, s: &Arc<Struct>) {
        walk_struct(self, s);
    }

    fn visit_const(&mut self, c: &Arc<Const>) {
        walk_const(self, c);
    }

    fn visit_enum(&mut self, e: &Arc<Enum>) {
        walk_enum(self, e);
    }

    fn visit_module(&mut self, e: &Arc<Module>) {
        walk_module(self, e);
    }

    fn visit_use(&mut self, i: &Arc<Use>) {
        walk_use(self, i);
    }

    fn visit_ctor(&mut self, m: &Arc<Function>) {
        walk_fct(self, &m);
    }

    fn visit_method(&mut self, m: &Arc<Function>) {
        walk_fct(self, &m);
    }

    fn visit_field(&mut self, p: &Arc<Field>) {
        walk_field(self, p);
    }

    fn visit_fct(&mut self, f: &Arc<Function>) {
        walk_fct(self, &f);
    }

    fn visit_param(&mut self, p: &Param) {
        walk_param(self, p);
    }

    fn visit_type(&mut self, t: &TypeData) {
        walk_type(self, t);
    }

    fn visit_stmt(&mut self, s: &StmtData) {
        walk_stmt(self, s);
    }

    fn visit_expr(&mut self, e: &ExprData) {
        walk_expr(self, e);
    }

    fn visit_type_alias(&mut self, e: &Arc<Alias>) {
        walk_type_alias(self, e);
    }
}

pub fn walk_file<V: Visitor>(v: &mut V, f: &File) {
    for e in &f.elements {
        walk_elem(v, e);
    }
}

pub fn walk_elem<V: Visitor>(v: &mut V, e: &ElemData) {
    match e {
        ElemData::Function(f) => v.visit_fct(f),
        ElemData::Class(ref c) => v.visit_class(c),
        ElemData::Struct(ref s) => v.visit_struct(s),
        ElemData::Trait(ref t) => v.visit_trait(t),
        ElemData::Impl(ref i) => v.visit_impl(i),
        ElemData::Global(ref g) => v.visit_global(g),
        ElemData::Const(ref c) => v.visit_const(c),
        ElemData::Enum(ref e) => v.visit_enum(e),
        ElemData::Module(ref e) => v.visit_module(e),
        ElemData::Use(ref i) => v.visit_use(i),
        ElemData::Extern(ref stmt) => v.visit_extern(stmt),
        ElemData::Alias(ref node) => v.visit_type_alias(node),
        ElemData::Error { .. } => {}
    }
}

pub fn walk_global<V: Visitor>(v: &mut V, g: &Global) {
    v.visit_type(&g.data_type);

    if let Some(ref initial_value) = g.initial_value {
        v.visit_expr(initial_value);
    }
}

pub fn walk_trait<V: Visitor>(v: &mut V, t: &Arc<Trait>) {
    for m in &t.methods {
        walk_elem(v, m);
    }
}

pub fn walk_impl<V: Visitor>(v: &mut V, i: &Arc<Impl>) {
    for m in &i.methods {
        walk_elem(v, m);
    }
}

pub fn walk_class<V: Visitor>(v: &mut V, c: &Arc<Class>) {
    for f in &c.fields {
        v.visit_field(f);
    }
}

pub fn walk_const<V: Visitor>(v: &mut V, c: &Arc<Const>) {
    v.visit_type(&c.data_type);
    v.visit_expr(&c.expr);
}

pub fn walk_enum<V: Visitor>(_v: &mut V, _e: &Arc<Enum>) {
    // nothing to do
}

pub fn walk_module<V: Visitor>(v: &mut V, node: &Arc<Module>) {
    if let Some(ref elements) = node.elements {
        for e in elements {
            walk_elem(v, e);
        }
    }
}

pub fn walk_use<V: Visitor>(_v: &mut V, _use: &Arc<Use>) {
    // nothing to do
}

pub fn walk_extern<V: Visitor>(_v: &mut V, _use: &Arc<ExternPackage>) {
    // nothing to do
}

pub fn walk_type_alias<V: Visitor>(_v: &mut V, _node: &Arc<Alias>) {
    // nothing to do
}

pub fn walk_struct<V: Visitor>(v: &mut V, s: &Struct) {
    for f in &s.fields {
        v.visit_field(f);
    }
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
        v.visit_expr(block);
    }
}

pub fn walk_param<V: Visitor>(v: &mut V, p: &Param) {
    v.visit_type(&p.data_type);
}

pub fn walk_type<V: Visitor>(v: &mut V, t: &TypeData) {
    match *t {
        TypeData::Regular(_) => {}
        TypeData::Tuple(ref tuple) => {
            for ty in &tuple.subtypes {
                v.visit_type(ty);
            }
        }

        TypeData::Lambda(ref fct) => {
            for ty in &fct.params {
                v.visit_type(ty);
            }

            if let Some(ref ret) = fct.ret {
                v.visit_type(&ret);
            }
        }

        TypeData::QualifiedPath(ref qualified_path) => {
            v.visit_type(&qualified_path.ty);
            v.visit_type(&qualified_path.trait_ty);
        }

        TypeData::Error { .. } => {}
    }
}

pub fn walk_stmt<V: Visitor>(v: &mut V, s: &StmtData) {
    match *s {
        StmtData::Let(ref value) => {
            if let Some(ref ty) = value.data_type {
                v.visit_type(ty);
            }

            if let Some(ref e) = value.expr {
                v.visit_expr(e);
            }
        }

        StmtData::Expr(ref value) => {
            v.visit_expr(&value.expr);
        }
    }
}

pub fn walk_expr<V: Visitor>(v: &mut V, e: &ExprData) {
    match *e {
        ExprData::Un(ref value) => {
            v.visit_expr(&value.opnd);
        }

        ExprData::Bin(ref value) => {
            v.visit_expr(&value.lhs);
            v.visit_expr(&value.rhs);
        }

        ExprData::Call(ref call) => {
            v.visit_expr(&call.callee);

            for arg in &call.args {
                v.visit_expr(&arg.expr);
            }
        }

        ExprData::TypeParam(ref expr) => {
            v.visit_expr(&expr.callee);

            for arg in &expr.args {
                v.visit_type(arg);
            }
        }

        ExprData::Path(ref path) => {
            v.visit_expr(&path.lhs);
            v.visit_expr(&path.rhs);
        }

        ExprData::Dot(ref value) => {
            v.visit_expr(&value.lhs);
            v.visit_expr(&value.rhs);
        }

        ExprData::Conv(ref value) => {
            v.visit_expr(&value.object);
            v.visit_type(&value.data_type);
        }

        ExprData::Is(ref value) => {
            v.visit_expr(&value.value);
        }

        ExprData::Lambda(ref fct) => v.visit_fct(fct),

        ExprData::Block(ref value) => {
            for stmt in &value.stmts {
                v.visit_stmt(stmt);
            }

            if let Some(ref expr) = value.expr {
                v.visit_expr(expr);
            }
        }

        ExprData::Template(ref value) => {
            for part in &value.parts {
                v.visit_expr(part);
            }
        }

        ExprData::If(ref value) => {
            v.visit_expr(&value.cond);
            v.visit_expr(&value.then_block);

            if let Some(ref b) = value.else_block {
                v.visit_expr(b);
            }
        }

        ExprData::For(ref value) => {
            v.visit_expr(&value.expr);
            v.visit_expr(&value.block);
        }

        ExprData::While(ref value) => {
            v.visit_expr(&value.cond);
            v.visit_expr(&value.block);
        }

        ExprData::Tuple(ref value) => {
            for expr in &value.values {
                v.visit_expr(expr);
            }
        }

        ExprData::Paren(ref value) => {
            v.visit_expr(&value.expr);
        }

        ExprData::Match(ref value) => {
            v.visit_expr(&value.expr);
            for arm in &value.arms {
                if let Some(ref cond) = arm.cond {
                    v.visit_expr(cond);
                }
                v.visit_expr(&arm.value);
            }
        }

        ExprData::Return(ref value) => {
            if let Some(ref e) = value.expr {
                v.visit_expr(e);
            }
        }

        ExprData::Break(_) => {}
        ExprData::Continue(_) => {}

        ExprData::This(_) => {}
        ExprData::LitChar(_) => {}
        ExprData::LitInt(_) => {}
        ExprData::LitFloat(_) => {}
        ExprData::LitStr(_) => {}
        ExprData::LitBool(_) => {}
        ExprData::Ident(_) => {}
        ExprData::Error { .. } => {}
    }
}
