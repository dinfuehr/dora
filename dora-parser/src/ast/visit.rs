use crate::ast::*;

pub trait Visitor: Sized {
    fn visit_file(&mut self, a: &File) {
        walk_file(self, a);
    }

    fn visit_extern(&mut self, f: &File, stmt: &Arc<ExternPackage>) {
        walk_extern(self, f, stmt);
    }

    fn visit_global(&mut self, f: &File, g: &Arc<Global>) {
        walk_global(self, f, g);
    }

    fn visit_trait(&mut self, f: &File, t: &Arc<Trait>) {
        walk_trait(self, f, t);
    }

    fn visit_impl(&mut self, f: &File, i: &Arc<Impl>) {
        walk_impl(self, f, i);
    }

    fn visit_class(&mut self, f: &File, c: &Arc<Class>) {
        walk_class(self, f, c);
    }

    fn visit_struct(&mut self, f: &File, s: &Arc<Struct>) {
        walk_struct(self, f, s);
    }

    fn visit_const(&mut self, f: &File, c: &Arc<Const>) {
        walk_const(self, f, c);
    }

    fn visit_enum(&mut self, f: &File, e: &Arc<Enum>) {
        walk_enum(self, f, e);
    }

    fn visit_module(&mut self, f: &File, e: &Arc<Module>) {
        walk_module(self, f, e);
    }

    fn visit_use(&mut self, f: &File, i: &Arc<Use>) {
        walk_use(self, f, i);
    }

    fn visit_ctor(&mut self, f: &File, m: &Arc<Function>) {
        walk_fct(self, f, &m);
    }

    fn visit_method(&mut self, f: &File, m: &Arc<Function>) {
        walk_fct(self, f, &m);
    }

    fn visit_field(&mut self, f: &File, p: &Arc<Field>) {
        walk_field(self, f, p);
    }

    fn visit_fct(&mut self, f: &File, fct: &Arc<Function>) {
        walk_fct(self, f, &fct);
    }

    fn visit_param(&mut self, f: &File, p: &Param) {
        walk_param(self, f, p);
    }

    fn visit_type(&mut self, f: &File, t: &TypeData) {
        walk_type(self, f, t);
    }

    fn visit_stmt(&mut self, f: &File, s: &StmtData) {
        walk_stmt(self, f, s);
    }

    fn visit_expr(&mut self, f: &File, e: &ExprData) {
        walk_expr(self, f, e);
    }

    fn visit_type_alias(&mut self, f: &File, e: &Arc<Alias>) {
        walk_type_alias(self, f, e);
    }
}

pub fn walk_file<V: Visitor>(v: &mut V, f: &File) {
    for &element_id in &f.elements {
        let e = f.node(element_id);
        walk_elem(v, f, e);
    }
}

pub fn walk_elem<V: Visitor>(v: &mut V, f: &File, e: &ElemData) {
    match e {
        ElemData::Function(fct) => v.visit_fct(f, fct),
        ElemData::Class(ref c) => v.visit_class(f, c),
        ElemData::Struct(ref s) => v.visit_struct(f, s),
        ElemData::Trait(ref t) => v.visit_trait(f, t),
        ElemData::Impl(ref i) => v.visit_impl(f, i),
        ElemData::Global(ref g) => v.visit_global(f, g),
        ElemData::Const(ref c) => v.visit_const(f, c),
        ElemData::Enum(ref e) => v.visit_enum(f, e),
        ElemData::Module(ref e) => v.visit_module(f, e),
        ElemData::Use(ref i) => v.visit_use(f, i),
        ElemData::Extern(ref stmt) => v.visit_extern(f, stmt),
        ElemData::Alias(ref node) => v.visit_type_alias(f, node),
        ElemData::Error { .. } => {}
    }
}

pub fn walk_global<V: Visitor>(v: &mut V, f: &File, g: &Global) {
    v.visit_type(f, &g.data_type);

    if let Some(ref initial_value) = g.initial_value {
        v.visit_expr(f, initial_value);
    }
}

pub fn walk_trait<V: Visitor>(v: &mut V, f: &File, t: &Arc<Trait>) {
    for &elem_id in &t.methods {
        let elem = f.node(elem_id);
        walk_elem(v, f, elem);
    }
}

pub fn walk_impl<V: Visitor>(v: &mut V, f: &File, i: &Arc<Impl>) {
    for &elem_id in &i.methods {
        let elem = f.node(elem_id);
        walk_elem(v, f, elem);
    }
}

pub fn walk_class<V: Visitor>(v: &mut V, f: &File, c: &Arc<Class>) {
    for field in &c.fields {
        v.visit_field(f, field);
    }
}

pub fn walk_const<V: Visitor>(v: &mut V, f: &File, c: &Arc<Const>) {
    v.visit_type(f, &c.data_type);
    v.visit_expr(f, &c.expr);
}

pub fn walk_enum<V: Visitor>(_v: &mut V, _f: &File, _e: &Arc<Enum>) {
    // nothing to do
}

pub fn walk_module<V: Visitor>(v: &mut V, f: &File, node: &Arc<Module>) {
    if let Some(ref elements) = node.elements {
        for &element_id in elements {
            let element = f.node(element_id);
            walk_elem(v, f, element);
        }
    }
}

pub fn walk_use<V: Visitor>(_v: &mut V, _f: &File, _use: &Arc<Use>) {
    // nothing to do
}

pub fn walk_extern<V: Visitor>(_v: &mut V, _f: &File, _use: &Arc<ExternPackage>) {
    // nothing to do
}

pub fn walk_type_alias<V: Visitor>(_v: &mut V, _f: &File, _node: &Arc<Alias>) {
    // nothing to do
}

pub fn walk_struct<V: Visitor>(v: &mut V, f: &File, s: &Struct) {
    for field in &s.fields {
        v.visit_field(f, field);
    }
}

pub fn walk_field<V: Visitor>(v: &mut V, f: &File, field: &Field) {
    v.visit_type(f, &field.data_type);
}

pub fn walk_fct<V: Visitor>(v: &mut V, f: &File, fct: &Function) {
    for p in &fct.params {
        v.visit_param(f, p);
    }

    if let Some(ref ty) = fct.return_type {
        v.visit_type(f, ty);
    }

    if let Some(ref block) = fct.block {
        v.visit_expr(f, block);
    }
}

pub fn walk_param<V: Visitor>(v: &mut V, f: &File, p: &Param) {
    v.visit_type(f, &p.data_type);
}

pub fn walk_type<V: Visitor>(v: &mut V, f: &File, t: &TypeData) {
    match *t {
        TypeData::Regular(_) => {}
        TypeData::Tuple(ref tuple) => {
            for ty in &tuple.subtypes {
                v.visit_type(f, ty);
            }
        }

        TypeData::Lambda(ref fct) => {
            for ty in &fct.params {
                v.visit_type(f, ty);
            }

            if let Some(ref ret) = fct.ret {
                v.visit_type(f, &ret);
            }
        }

        TypeData::QualifiedPath(ref qualified_path) => {
            v.visit_type(f, &qualified_path.ty);
            v.visit_type(f, &qualified_path.trait_ty);
        }

        TypeData::Error { .. } => {}
    }
}

pub fn walk_stmt<V: Visitor>(v: &mut V, f: &File, s: &StmtData) {
    match *s {
        StmtData::Let(ref value) => {
            if let Some(ref ty) = value.data_type {
                v.visit_type(f, ty);
            }

            if let Some(ref e) = value.expr {
                v.visit_expr(f, e);
            }
        }

        StmtData::Expr(ref value) => {
            v.visit_expr(f, &value.expr);
        }
    }
}

pub fn walk_expr<V: Visitor>(v: &mut V, f: &File, e: &ExprData) {
    match *e {
        ExprData::Un(ref value) => {
            v.visit_expr(f, &value.opnd);
        }

        ExprData::Bin(ref value) => {
            v.visit_expr(f, &value.lhs);
            v.visit_expr(f, &value.rhs);
        }

        ExprData::Call(ref call) => {
            v.visit_expr(f, &call.callee);

            for arg in &call.args {
                v.visit_expr(f, &arg.expr);
            }
        }

        ExprData::TypeParam(ref expr) => {
            v.visit_expr(f, &expr.callee);

            for arg in &expr.args {
                v.visit_type(f, arg);
            }
        }

        ExprData::Path(ref path) => {
            v.visit_expr(f, &path.lhs);
            v.visit_expr(f, &path.rhs);
        }

        ExprData::Dot(ref value) => {
            v.visit_expr(f, &value.lhs);
            v.visit_expr(f, &value.rhs);
        }

        ExprData::Conv(ref value) => {
            v.visit_expr(f, &value.object);
            v.visit_type(f, &value.data_type);
        }

        ExprData::Is(ref value) => {
            v.visit_expr(f, &value.value);
        }

        ExprData::Lambda(ref fct) => v.visit_fct(f, fct),

        ExprData::Block(ref value) => {
            for stmt in &value.stmts {
                v.visit_stmt(f, stmt);
            }

            if let Some(ref expr) = value.expr {
                v.visit_expr(f, expr);
            }
        }

        ExprData::Template(ref value) => {
            for part in &value.parts {
                v.visit_expr(f, part);
            }
        }

        ExprData::If(ref value) => {
            v.visit_expr(f, &value.cond);
            v.visit_expr(f, &value.then_block);

            if let Some(ref b) = value.else_block {
                v.visit_expr(f, b);
            }
        }

        ExprData::For(ref value) => {
            v.visit_expr(f, &value.expr);
            v.visit_expr(f, &value.block);
        }

        ExprData::While(ref value) => {
            v.visit_expr(f, &value.cond);
            v.visit_expr(f, &value.block);
        }

        ExprData::Tuple(ref value) => {
            for expr in &value.values {
                v.visit_expr(f, expr);
            }
        }

        ExprData::Paren(ref value) => {
            v.visit_expr(f, &value.expr);
        }

        ExprData::Match(ref value) => {
            v.visit_expr(f, &value.expr);
            for arm in &value.arms {
                if let Some(ref cond) = arm.cond {
                    v.visit_expr(f, cond);
                }
                v.visit_expr(f, &arm.value);
            }
        }

        ExprData::Return(ref value) => {
            if let Some(ref e) = value.expr {
                v.visit_expr(f, e);
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
