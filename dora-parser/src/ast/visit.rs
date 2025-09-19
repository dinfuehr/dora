use crate::ast::*;

pub trait Visitor: Sized {
    fn visit_file(&mut self, a: &File) {
        walk_file(self, a);
    }

    fn visit_extern(&mut self, f: &File, id: AstId, stmt: &ExternPackage) {
        walk_extern(self, f, id, stmt);
    }

    fn visit_global(&mut self, f: &File, id: AstId, g: &Global) {
        walk_global(self, f, id, g);
    }

    fn visit_trait(&mut self, f: &File, id: AstId, t: &Trait) {
        walk_trait(self, f, id, t);
    }

    fn visit_impl(&mut self, f: &File, id: AstId, i: &Impl) {
        walk_impl(self, f, id, i);
    }

    fn visit_class(&mut self, f: &File, id: AstId, c: &Class) {
        walk_class(self, f, id, c);
    }

    fn visit_struct(&mut self, f: &File, id: AstId, s: &Struct) {
        walk_struct(self, f, id, s);
    }

    fn visit_const(&mut self, f: &File, id: AstId, c: &Const) {
        walk_const(self, f, id, c);
    }

    fn visit_enum(&mut self, f: &File, id: AstId, e: &Enum) {
        walk_enum(self, f, id, e);
    }

    fn visit_module(&mut self, f: &File, id: AstId, e: &Module) {
        walk_module(self, f, id, e);
    }

    fn visit_use(&mut self, f: &File, id: AstId, i: &Use) {
        walk_use(self, f, id, i);
    }

    fn visit_method(&mut self, f: &File, id: AstId, m: &Function) {
        walk_fct(self, f, id, &m);
    }

    fn visit_field(&mut self, f: &File, p: &Field) {
        walk_field(self, f, p);
    }

    fn visit_fct(&mut self, f: &File, id: AstId, fct: &Function) {
        walk_fct(self, f, id, &fct);
    }

    fn visit_param(&mut self, f: &File, p: &Param) {
        walk_param(self, f, p);
    }

    fn visit_expr(&mut self, f: &File, e: &ExprData) {
        walk_expr(self, f, e);
    }

    fn visit_type_alias(&mut self, f: &File, id: AstId, e: &Alias) {
        walk_type_alias(self, f, id, e);
    }

    fn visit_regular_type(&mut self, f: &File, id: AstId, e: &TypeRegularType) {
        walk_regular_type(self, f, id, e);
    }

    fn visit_tuple_type(&mut self, f: &File, id: AstId, e: &TypeTupleType) {
        walk_tuple_type(self, f, id, e);
    }

    fn visit_lambda_type(&mut self, f: &File, id: AstId, e: &TypeLambdaType) {
        walk_lambda_type(self, f, id, e);
    }

    fn visit_qualified_path_type(&mut self, f: &File, id: AstId, e: &TypeQualifiedPathType) {
        walk_qualified_path_type(self, f, id, e);
    }

    fn visit_let_stmt(&mut self, f: &File, id: AstId, e: &StmtLetType) {
        walk_let_stmt(self, f, id, e);
    }

    fn visit_expr_stmt(&mut self, f: &File, id: AstId, e: &StmtExprType) {
        walk_expr_stmt(self, f, id, e);
    }
}

pub fn walk_file<V: Visitor>(v: &mut V, f: &File) {
    for &element_id in &f.elements {
        let e = f.node(element_id);
        dispatch_ast(v, f, element_id, e);
    }
}

pub fn dispatch_ast<V: Visitor>(v: &mut V, f: &File, id: AstId, e: &Ast) {
    match e {
        Ast::Function(fct) => v.visit_fct(f, id, fct),
        Ast::Class(ref c) => v.visit_class(f, id, c),
        Ast::Struct(ref s) => v.visit_struct(f, id, s),
        Ast::Trait(ref t) => v.visit_trait(f, id, t),
        Ast::Impl(ref i) => v.visit_impl(f, id, i),
        Ast::Global(ref g) => v.visit_global(f, id, g),
        Ast::Const(ref c) => v.visit_const(f, id, c),
        Ast::Enum(ref e) => v.visit_enum(f, id, e),
        Ast::Module(ref e) => v.visit_module(f, id, e),
        Ast::Use(ref i) => v.visit_use(f, id, i),
        Ast::Extern(ref stmt) => v.visit_extern(f, id, stmt),
        Ast::Alias(ref node) => v.visit_type_alias(f, id, node),
        Ast::LambdaType(ref node) => v.visit_lambda_type(f, id, node),
        Ast::RegularType(ref node) => v.visit_regular_type(f, id, node),
        Ast::QualifiedPathType(ref node) => v.visit_qualified_path_type(f, id, node),
        Ast::TupleType(ref node) => v.visit_tuple_type(f, id, node),
        Ast::LetStmt(ref node) => v.visit_let_stmt(f, id, node),
        Ast::ExprStmt(ref node) => v.visit_expr_stmt(f, id, node),
        Ast::Error { .. } => {}
    }
}

pub fn walk_global<V: Visitor>(v: &mut V, f: &File, _id: AstId, g: &Global) {
    dispatch_ast(v, f, g.data_type, f.node(g.data_type));

    if let Some(ref initial_value) = g.initial_value {
        v.visit_expr(f, initial_value);
    }
}

pub fn walk_trait<V: Visitor>(v: &mut V, f: &File, _id: AstId, t: &Trait) {
    for &elem_id in &t.methods {
        let elem = f.node(elem_id);
        dispatch_ast(v, f, elem_id, elem);
    }
}

pub fn walk_impl<V: Visitor>(v: &mut V, f: &File, _id: AstId, i: &Impl) {
    for &elem_id in &i.methods {
        let elem = f.node(elem_id);
        dispatch_ast(v, f, elem_id, elem);
    }
}

pub fn walk_class<V: Visitor>(v: &mut V, f: &File, _id: AstId, c: &Class) {
    for field in &c.fields {
        v.visit_field(f, field);
    }
}

pub fn walk_const<V: Visitor>(v: &mut V, f: &File, _id: AstId, c: &Const) {
    dispatch_ast(v, f, c.data_type, f.node(c.data_type));
    v.visit_expr(f, &c.expr);
}

pub fn walk_enum<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _e: &Enum) {
    // nothing to do
}

pub fn walk_module<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &Module) {
    if let Some(ref elements) = node.elements {
        for &element_id in elements {
            let element = f.node(element_id);
            dispatch_ast(v, f, element_id, element);
        }
    }
}

pub fn walk_use<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _use: &Use) {
    // nothing to do
}

pub fn walk_extern<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _use: &ExternPackage) {
    // nothing to do
}

pub fn walk_type_alias<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &Alias) {
    // nothing to do
}

pub fn walk_struct<V: Visitor>(v: &mut V, f: &File, _id: AstId, s: &Struct) {
    for field in &s.fields {
        v.visit_field(f, field);
    }
}

pub fn walk_field<V: Visitor>(v: &mut V, f: &File, field: &Field) {
    dispatch_ast(v, f, field.data_type, f.node(field.data_type));
}

pub fn walk_fct<V: Visitor>(v: &mut V, f: &File, _id: AstId, fct: &Function) {
    for p in &fct.params {
        v.visit_param(f, p);
    }

    if let Some(ret_id) = fct.return_type {
        dispatch_ast(v, f, ret_id, f.node(ret_id));
    }

    if let Some(ref block) = fct.block {
        v.visit_expr(f, block);
    }
}

pub fn walk_param<V: Visitor>(v: &mut V, f: &File, p: &Param) {
    dispatch_ast(v, f, p.data_type, f.node(p.data_type));
}

pub fn walk_regular_type<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _t: &TypeRegularType) {}

pub fn walk_tuple_type<V: Visitor>(v: &mut V, f: &File, _id: AstId, t: &TypeTupleType) {
    for &ty_id in &t.subtypes {
        dispatch_ast(v, f, ty_id, f.node(ty_id));
    }
}

pub fn walk_lambda_type<V: Visitor>(v: &mut V, f: &File, _id: AstId, t: &TypeLambdaType) {
    for &ty_id in &t.params {
        dispatch_ast(v, f, ty_id, f.node(ty_id));
    }

    if let Some(ret_id) = t.ret {
        dispatch_ast(v, f, ret_id, f.node(ret_id));
    }
}

pub fn walk_qualified_path_type<V: Visitor>(
    v: &mut V,
    f: &File,
    _id: AstId,
    t: &TypeQualifiedPathType,
) {
    dispatch_ast(v, f, t.ty, f.node(t.ty));
    dispatch_ast(v, f, t.trait_ty, f.node(t.trait_ty));
}

pub fn walk_let_stmt<V: Visitor>(v: &mut V, f: &File, _id: AstId, s: &StmtLetType) {
    if let Some(ty) = s.data_type {
        dispatch_ast(v, f, ty, f.node(ty));
    }

    if let Some(ref e) = s.expr {
        v.visit_expr(f, e);
    }
}

pub fn walk_expr_stmt<V: Visitor>(v: &mut V, f: &File, _id: AstId, s: &StmtExprType) {
    v.visit_expr(f, &s.expr);
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

            for &arg_id in &expr.args {
                dispatch_ast(v, f, arg_id, f.node(arg_id));
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
            dispatch_ast(v, f, value.data_type, f.node(value.data_type));
        }

        ExprData::Is(ref value) => {
            v.visit_expr(f, &value.value);
        }

        ExprData::Lambda(ref value) => {
            let fct = f.node(value.fct_id).to_function().expect("fct expected");
            v.visit_fct(f, value.fct_id, fct)
        }

        ExprData::Block(ref value) => {
            for &stmt_id in &value.stmts {
                dispatch_ast(v, f, stmt_id, f.node(stmt_id));
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
