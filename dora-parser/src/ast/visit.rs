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

    fn visit_field(&mut self, f: &File, id: AstId, node: &Field) {
        walk_field(self, f, id, node);
    }

    fn visit_fct(&mut self, f: &File, id: AstId, fct: &Function) {
        walk_fct(self, f, id, &fct);
    }

    fn visit_param(&mut self, f: &File, p: &Param) {
        walk_param(self, f, p);
    }

    fn visit_type_alias(&mut self, f: &File, id: AstId, e: &Alias) {
        walk_type_alias(self, f, id, e);
    }

    fn visit_argument(&mut self, f: &File, id: AstId, node: &Argument) {
        walk_argument(self, f, id, node);
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

    fn visit_un(&mut self, f: &File, id: AstId, e: &ExprUnType) {
        walk_un(self, f, id, e);
    }

    fn visit_bin(&mut self, f: &File, id: AstId, e: &ExprBinType) {
        walk_bin(self, f, id, e);
    }

    fn visit_call(&mut self, f: &File, id: AstId, e: &ExprCallType) {
        walk_call(self, f, id, e);
    }

    fn visit_type_param(&mut self, f: &File, id: AstId, e: &ExprTypeParamType) {
        walk_type_param(self, f, id, e);
    }

    fn visit_path(&mut self, f: &File, id: AstId, e: &ExprPathType) {
        walk_path(self, f, id, e);
    }

    fn visit_dot(&mut self, f: &File, id: AstId, e: &ExprDotType) {
        walk_dot(self, f, id, e);
    }

    fn visit_conv(&mut self, f: &File, id: AstId, e: &ExprConvType) {
        walk_conv(self, f, id, e);
    }

    fn visit_is(&mut self, f: &File, id: AstId, e: &ExprIsType) {
        walk_is(self, f, id, e);
    }

    fn visit_lambda(&mut self, f: &File, id: AstId, e: &ExprLambdaType) {
        walk_lambda(self, f, id, e);
    }

    fn visit_block(&mut self, f: &File, id: AstId, e: &ExprBlockType) {
        walk_block(self, f, id, e);
    }

    fn visit_template(&mut self, f: &File, id: AstId, e: &ExprTemplateType) {
        walk_template(self, f, id, e);
    }

    fn visit_if(&mut self, f: &File, id: AstId, e: &ExprIfType) {
        walk_if(self, f, id, e);
    }

    fn visit_for(&mut self, f: &File, id: AstId, e: &ExprForType) {
        walk_for(self, f, id, e);
    }

    fn visit_while(&mut self, f: &File, id: AstId, e: &ExprWhileType) {
        walk_while(self, f, id, e);
    }

    fn visit_tuple(&mut self, f: &File, id: AstId, e: &ExprTupleType) {
        walk_tuple(self, f, id, e);
    }

    fn visit_paren(&mut self, f: &File, id: AstId, e: &ExprParenType) {
        walk_paren(self, f, id, e);
    }

    fn visit_match(&mut self, f: &File, id: AstId, e: &ExprMatchType) {
        walk_match(self, f, id, e);
    }

    fn visit_return(&mut self, f: &File, id: AstId, e: &ExprReturnType) {
        walk_return(self, f, id, e);
    }

    fn visit_break(&mut self, f: &File, id: AstId, e: &ExprBreakType) {
        walk_break(self, f, id, e);
    }

    fn visit_continue(&mut self, f: &File, id: AstId, e: &ExprContinueType) {
        walk_continue(self, f, id, e);
    }

    fn visit_this(&mut self, f: &File, id: AstId, e: &ExprSelfType) {
        walk_this(self, f, id, e);
    }

    fn visit_lit_char(&mut self, f: &File, id: AstId, e: &ExprLitCharType) {
        walk_lit_char(self, f, id, e);
    }

    fn visit_lit_int(&mut self, f: &File, id: AstId, e: &ExprLitIntType) {
        walk_lit_int(self, f, id, e);
    }

    fn visit_lit_float(&mut self, f: &File, id: AstId, e: &ExprLitFloatType) {
        walk_lit_float(self, f, id, e);
    }

    fn visit_lit_str(&mut self, f: &File, id: AstId, e: &ExprLitStrType) {
        walk_lit_str(self, f, id, e);
    }

    fn visit_lit_bool(&mut self, f: &File, id: AstId, e: &ExprLitBoolType) {
        walk_lit_bool(self, f, id, e);
    }

    fn visit_ident(&mut self, f: &File, id: AstId, e: &ExprIdentType) {
        walk_ident(self, f, id, e);
    }

    fn visit_error(&mut self, f: &File, id: AstId, e: &Error) {
        walk_error(self, f, id, e);
    }
}

pub fn walk_file<V: Visitor>(v: &mut V, f: &File) {
    for &element_id in &f.elements {
        let e = f.node(element_id);
        dispatch_ast(v, f, element_id, e);
    }
}

pub fn dispatch_ast_id<V: Visitor>(v: &mut V, f: &File, id: AstId) {
    let node = f.node(id);
    dispatch_ast(v, f, id, node);
}

pub fn dispatch_ast<V: Visitor>(v: &mut V, f: &File, id: AstId, e: &Ast) {
    match e {
        Ast::Function(fct) => v.visit_fct(f, id, fct),
        Ast::Class(ref c) => v.visit_class(f, id, c),
        Ast::Struct(ref s) => v.visit_struct(f, id, s),
        Ast::Field(ref node) => v.visit_field(f, id, node),
        Ast::Trait(ref t) => v.visit_trait(f, id, t),
        Ast::Impl(ref i) => v.visit_impl(f, id, i),
        Ast::Global(ref g) => v.visit_global(f, id, g),
        Ast::Const(ref c) => v.visit_const(f, id, c),
        Ast::Enum(ref e) => v.visit_enum(f, id, e),
        Ast::Module(ref e) => v.visit_module(f, id, e),
        Ast::Use(ref i) => v.visit_use(f, id, i),
        Ast::Argument(ref node) => v.visit_argument(f, id, node),
        Ast::Extern(ref stmt) => v.visit_extern(f, id, stmt),
        Ast::Alias(ref node) => v.visit_type_alias(f, id, node),
        Ast::LambdaType(ref node) => v.visit_lambda_type(f, id, node),
        Ast::RegularType(ref node) => v.visit_regular_type(f, id, node),
        Ast::QualifiedPathType(ref node) => v.visit_qualified_path_type(f, id, node),
        Ast::TupleType(ref node) => v.visit_tuple_type(f, id, node),
        Ast::LetStmt(ref node) => v.visit_let_stmt(f, id, node),
        Ast::ExprStmt(ref node) => v.visit_expr_stmt(f, id, node),
        Ast::Un(ref node) => v.visit_un(f, id, node),
        Ast::Bin(ref node) => v.visit_bin(f, id, node),
        Ast::Call(ref node) => v.visit_call(f, id, node),
        Ast::TypeParam(ref node) => v.visit_type_param(f, id, node),
        Ast::Path(ref node) => v.visit_path(f, id, node),
        Ast::Dot(ref node) => v.visit_dot(f, id, node),
        Ast::Conv(ref node) => v.visit_conv(f, id, node),
        Ast::Is(ref node) => v.visit_is(f, id, node),
        Ast::Lambda(ref node) => v.visit_lambda(f, id, node),
        Ast::Block(ref node) => v.visit_block(f, id, node),
        Ast::Template(ref node) => v.visit_template(f, id, node),
        Ast::If(ref node) => v.visit_if(f, id, node),
        Ast::For(ref node) => v.visit_for(f, id, node),
        Ast::While(ref node) => v.visit_while(f, id, node),
        Ast::Tuple(ref node) => v.visit_tuple(f, id, node),
        Ast::Paren(ref node) => v.visit_paren(f, id, node),
        Ast::Match(ref node) => v.visit_match(f, id, node),
        Ast::Return(ref node) => v.visit_return(f, id, node),
        Ast::Break(ref node) => v.visit_break(f, id, node),
        Ast::Continue(ref node) => v.visit_continue(f, id, node),
        Ast::This(ref node) => v.visit_this(f, id, node),
        Ast::LitChar(ref node) => v.visit_lit_char(f, id, node),
        Ast::LitInt(ref node) => v.visit_lit_int(f, id, node),
        Ast::LitFloat(ref node) => v.visit_lit_float(f, id, node),
        Ast::LitStr(ref node) => v.visit_lit_str(f, id, node),
        Ast::LitBool(ref node) => v.visit_lit_bool(f, id, node),
        Ast::Ident(ref node) => v.visit_ident(f, id, node),
        Ast::Error(ref node) => v.visit_error(f, id, node),
    }
}

pub fn walk_global<V: Visitor>(v: &mut V, f: &File, _id: AstId, g: &Global) {
    dispatch_ast(v, f, g.data_type, f.node(g.data_type));

    if let Some(initial_value) = g.initial_value {
        dispatch_ast_id(v, f, initial_value);
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
    for &field_id in &c.fields {
        dispatch_ast_id(v, f, field_id);
    }
}

pub fn walk_const<V: Visitor>(v: &mut V, f: &File, _id: AstId, c: &Const) {
    dispatch_ast(v, f, c.data_type, f.node(c.data_type));
    dispatch_ast_id(v, f, c.expr);
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
    for &field_id in &s.fields {
        dispatch_ast_id(v, f, field_id);
    }
}

pub fn walk_field<V: Visitor>(v: &mut V, f: &File, _id: AstId, field: &Field) {
    dispatch_ast(v, f, field.data_type, f.node(field.data_type));
}

pub fn walk_fct<V: Visitor>(v: &mut V, f: &File, _id: AstId, fct: &Function) {
    for p in &fct.params {
        v.visit_param(f, p);
    }

    if let Some(ret_id) = fct.return_type {
        dispatch_ast(v, f, ret_id, f.node(ret_id));
    }

    if let Some(block) = fct.block {
        dispatch_ast_id(v, f, block);
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

    if let Some(e) = s.expr {
        dispatch_ast_id(v, f, e);
    }
}

pub fn walk_expr_stmt<V: Visitor>(v: &mut V, f: &File, _id: AstId, s: &StmtExprType) {
    dispatch_ast_id(v, f, s.expr);
}

pub fn walk_un<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &ExprUnType) {
    dispatch_ast_id(v, f, node.opnd);
}

pub fn walk_bin<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &ExprBinType) {
    dispatch_ast_id(v, f, node.lhs);
    dispatch_ast_id(v, f, node.rhs);
}

pub fn walk_call<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &ExprCallType) {
    dispatch_ast_id(v, f, node.callee);
    for &arg_id in &node.args {
        dispatch_ast_id(v, f, arg_id);
    }
}

pub fn walk_argument<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &Argument) {
    dispatch_ast_id(v, f, node.expr);
}

pub fn walk_type_param<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &ExprTypeParamType) {
    dispatch_ast_id(v, f, node.callee);
    for &arg in &node.args {
        dispatch_ast_id(v, f, arg);
    }
}

pub fn walk_path<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &ExprPathType) {
    dispatch_ast_id(v, f, node.lhs);
    dispatch_ast_id(v, f, node.rhs);
}

pub fn walk_dot<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &ExprDotType) {
    dispatch_ast_id(v, f, node.lhs);
    dispatch_ast_id(v, f, node.rhs);
}

pub fn walk_conv<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &ExprConvType) {
    dispatch_ast_id(v, f, node.object);
    dispatch_ast_id(v, f, node.data_type);
}

pub fn walk_is<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &ExprIsType) {
    dispatch_ast_id(v, f, node.value);
}

pub fn walk_lambda<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &ExprLambdaType) {
    dispatch_ast_id(v, f, node.fct_id);
}

pub fn walk_block<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &ExprBlockType) {
    for &stmt_id in &node.stmts {
        dispatch_ast_id(v, f, stmt_id);
    }

    if let Some(expr) = node.expr {
        dispatch_ast_id(v, f, expr);
    }
}

pub fn walk_template<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &ExprTemplateType) {
    for &part_id in &node.parts {
        dispatch_ast_id(v, f, part_id);
    }
}

pub fn walk_if<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &ExprIfType) {
    dispatch_ast_id(v, f, node.cond);
    dispatch_ast_id(v, f, node.then_block);

    if let Some(else_block) = node.else_block {
        dispatch_ast_id(v, f, else_block);
    }
}

pub fn walk_for<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &ExprForType) {
    dispatch_ast_id(v, f, node.expr);
    dispatch_ast_id(v, f, node.block);
}

pub fn walk_while<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &ExprWhileType) {
    dispatch_ast_id(v, f, node.cond);
    dispatch_ast_id(v, f, node.block);
}

pub fn walk_tuple<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &ExprTupleType) {
    for &value_id in &node.values {
        dispatch_ast_id(v, f, value_id);
    }
}

pub fn walk_paren<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &ExprParenType) {
    dispatch_ast_id(v, f, node.expr);
}

pub fn walk_match<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &ExprMatchType) {
    dispatch_ast_id(v, f, node.expr);

    for arm in &node.arms {
        if let Some(cond) = arm.cond {
            dispatch_ast_id(v, f, cond);
        }
        dispatch_ast_id(v, f, arm.value);
    }
}

pub fn walk_return<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &ExprReturnType) {
    if let Some(expr) = node.expr {
        dispatch_ast_id(v, f, expr);
    }
}

pub fn walk_break<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &ExprBreakType) {
    // Nothing to do.
}

pub fn walk_continue<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &ExprContinueType) {
    // Nothing to do.
}

pub fn walk_this<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &ExprSelfType) {
    // Nothing to do.
}

pub fn walk_lit_char<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &ExprLitCharType) {
    // Nothing to do.
}

pub fn walk_lit_int<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &ExprLitIntType) {
    // Nothing to do.
}

pub fn walk_lit_float<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &ExprLitFloatType) {
    // Nothing to do.
}

pub fn walk_lit_str<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &ExprLitStrType) {
    // Nothing to do.
}

pub fn walk_lit_bool<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &ExprLitBoolType) {
    // Nothing to do.
}

pub fn walk_ident<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &ExprIdentType) {
    // Nothing to do.
}

pub fn walk_error<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &Error) {
    // Nothing to do.
}
