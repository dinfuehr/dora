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

    fn visit_where_item(&mut self, f: &File, id: AstId, node: &WhereClause) {
        walk_where_clause(self, f, id, node);
    }

    fn visit_where_clause_item(&mut self, f: &File, id: AstId, node: &WhereClauseItem) {
        walk_where_clause_item(self, f, id, node);
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

    fn visit_use_path(&mut self, f: &File, id: AstId, i: &UsePath) {
        walk_use_path(self, f, id, i);
    }

    fn visit_use_group(&mut self, f: &File, id: AstId, i: &UseGroup) {
        walk_use_group(self, f, id, i);
    }

    fn visit_use_target_name(&mut self, f: &File, id: AstId, node: &UseTargetName) {
        walk_use_target_name(self, f, id, node);
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

    fn visit_param(&mut self, f: &File, id: AstId, p: &Param) {
        walk_param(self, f, id, p);
    }

    fn visit_type_alias(&mut self, f: &File, id: AstId, e: &Alias) {
        walk_type_alias(self, f, id, e);
    }

    fn visit_argument(&mut self, f: &File, id: AstId, node: &Argument) {
        walk_argument(self, f, id, node);
    }

    fn visit_regular_type(&mut self, f: &File, id: AstId, e: &RegularType) {
        walk_regular_type(self, f, id, e);
    }

    fn visit_type_argument(&mut self, f: &File, id: AstId, node: &TypeArgument) {
        walk_type_argument(self, f, id, node);
    }

    fn visit_type_param_list(&mut self, f: &File, id: AstId, node: &TypeParamList) {
        walk_type_param_list(self, f, id, node);
    }

    fn visit_type_param(&mut self, f: &File, id: AstId, node: &TypeParam) {
        walk_type_param(self, f, id, node);
    }

    fn visit_tuple_type(&mut self, f: &File, id: AstId, e: &TupleType) {
        walk_tuple_type(self, f, id, e);
    }

    fn visit_lambda_type(&mut self, f: &File, id: AstId, e: &LambdaType) {
        walk_lambda_type(self, f, id, e);
    }

    fn visit_qualified_path_type(&mut self, f: &File, id: AstId, e: &QualifiedPathType) {
        walk_qualified_path_type(self, f, id, e);
    }

    fn visit_let_stmt(&mut self, f: &File, id: AstId, e: &Let) {
        walk_let_stmt(self, f, id, e);
    }

    fn visit_expr_stmt(&mut self, f: &File, id: AstId, e: &ExprStmt) {
        walk_expr_stmt(self, f, id, e);
    }

    fn visit_un(&mut self, f: &File, id: AstId, e: &Un) {
        walk_un(self, f, id, e);
    }

    fn visit_bin(&mut self, f: &File, id: AstId, e: &Bin) {
        walk_bin(self, f, id, e);
    }

    fn visit_call(&mut self, f: &File, id: AstId, e: &Call) {
        walk_call(self, f, id, e);
    }

    fn visit_typed_expr(&mut self, f: &File, id: AstId, e: &TypedExpr) {
        walk_typed_expr(self, f, id, e);
    }

    fn visit_path(&mut self, f: &File, id: AstId, e: &Path) {
        walk_path(self, f, id, e);
    }

    fn visit_path_data(&mut self, f: &File, id: AstId, node: &PathData) {
        walk_path_data(self, f, id, node);
    }

    fn visit_dot(&mut self, f: &File, id: AstId, e: &Dot) {
        walk_dot(self, f, id, e);
    }

    fn visit_conv(&mut self, f: &File, id: AstId, e: &Conv) {
        walk_conv(self, f, id, e);
    }

    fn visit_is(&mut self, f: &File, id: AstId, e: &Is) {
        walk_is(self, f, id, e);
    }

    fn visit_lambda(&mut self, f: &File, id: AstId, e: &Lambda) {
        walk_lambda(self, f, id, e);
    }

    fn visit_block(&mut self, f: &File, id: AstId, e: &Block) {
        walk_block(self, f, id, e);
    }

    fn visit_template(&mut self, f: &File, id: AstId, e: &Template) {
        walk_template(self, f, id, e);
    }

    fn visit_if(&mut self, f: &File, id: AstId, e: &If) {
        walk_if(self, f, id, e);
    }

    fn visit_for(&mut self, f: &File, id: AstId, e: &For) {
        walk_for(self, f, id, e);
    }

    fn visit_while(&mut self, f: &File, id: AstId, e: &While) {
        walk_while(self, f, id, e);
    }

    fn visit_tuple(&mut self, f: &File, id: AstId, e: &Tuple) {
        walk_tuple(self, f, id, e);
    }

    fn visit_paren(&mut self, f: &File, id: AstId, e: &Paren) {
        walk_paren(self, f, id, e);
    }

    fn visit_match(&mut self, f: &File, id: AstId, e: &Match) {
        walk_match(self, f, id, e);
    }

    fn visit_match_arm(&mut self, f: &File, id: AstId, node: &Arm) {
        walk_match_arm(self, f, id, node);
    }

    fn visit_return(&mut self, f: &File, id: AstId, e: &Return) {
        walk_return(self, f, id, e);
    }

    fn visit_break(&mut self, f: &File, id: AstId, e: &Break) {
        walk_break(self, f, id, e);
    }

    fn visit_continue(&mut self, f: &File, id: AstId, e: &Continue) {
        walk_continue(self, f, id, e);
    }

    fn visit_this(&mut self, f: &File, id: AstId, e: &This) {
        walk_this(self, f, id, e);
    }

    fn visit_upcase_this(&mut self, f: &File, id: AstId, node: &UpcaseThis) {
        walk_upcase_this(self, f, id, node);
    }

    fn visit_lit_char(&mut self, f: &File, id: AstId, e: &LitChar) {
        walk_lit_char(self, f, id, e);
    }

    fn visit_lit_int(&mut self, f: &File, id: AstId, e: &LitInt) {
        walk_lit_int(self, f, id, e);
    }

    fn visit_lit_float(&mut self, f: &File, id: AstId, e: &LitFloat) {
        walk_lit_float(self, f, id, e);
    }

    fn visit_lit_str(&mut self, f: &File, id: AstId, e: &LitStr) {
        walk_lit_str(self, f, id, e);
    }

    fn visit_lit_bool(&mut self, f: &File, id: AstId, e: &LitBool) {
        walk_lit_bool(self, f, id, e);
    }

    fn visit_ident(&mut self, f: &File, id: AstId, e: &Ident) {
        walk_ident(self, f, id, e);
    }

    fn visit_underscore(&mut self, f: &File, id: AstId, node: &Underscore) {
        walk_underscore(self, f, id, node);
    }

    fn visit_lit_pattern(&mut self, f: &File, id: AstId, node: &LitPattern) {
        walk_lit_pattern(self, f, id, node);
    }

    fn visit_ident_pattern(&mut self, f: &File, id: AstId, node: &IdentPattern) {
        walk_ident_pattern(self, f, id, node);
    }

    fn visit_tuple_pattern(&mut self, f: &File, id: AstId, node: &TuplePattern) {
        walk_tuple_pattern(self, f, id, node);
    }

    fn visit_constructor_pattern(&mut self, f: &File, id: AstId, node: &CtorPattern) {
        walk_constructor_pattern(self, f, id, node);
    }

    fn visit_constructor_field(&mut self, f: &File, id: AstId, node: &CtorField) {
        walk_constructor_field(self, f, id, node);
    }

    fn visit_rest(&mut self, f: &File, id: AstId, node: &Rest) {
        walk_rest(self, f, id, node);
    }

    fn visit_alt(&mut self, f: &File, id: AstId, node: &Alt) {
        walk_alt(self, f, id, node);
    }

    fn visit_modifier_list(&mut self, f: &File, id: AstId, node: &ModifierList) {
        walk_modifier_list(self, f, id, node);
    }

    fn visit_modifier(&mut self, f: &File, id: AstId, node: &Modifier) {
        walk_modifier(self, f, id, node);
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
        Ast::Class(node) => v.visit_class(f, id, node),
        Ast::Struct(node) => v.visit_struct(f, id, node),
        Ast::WhereClause(node) => v.visit_where_item(f, id, node),
        Ast::WhereClauseItem(node) => v.visit_where_clause_item(f, id, node),
        Ast::Field(node) => v.visit_field(f, id, node),
        Ast::Trait(node) => v.visit_trait(f, id, node),
        Ast::Impl(node) => v.visit_impl(f, id, node),
        Ast::Global(node) => v.visit_global(f, id, node),
        Ast::Const(node) => v.visit_const(f, id, node),
        Ast::Enum(node) => v.visit_enum(f, id, node),
        Ast::Module(node) => v.visit_module(f, id, node),
        Ast::Use(node) => v.visit_use(f, id, node),
        Ast::UsePath(node) => v.visit_use_path(f, id, node),
        Ast::UseGroup(node) => v.visit_use_group(f, id, node),
        Ast::UseTargetName(node) => v.visit_use_target_name(f, id, node),
        Ast::Argument(node) => v.visit_argument(f, id, node),
        Ast::Param(node) => v.visit_param(f, id, node),
        Ast::Extern(node) => v.visit_extern(f, id, node),
        Ast::Alias(node) => v.visit_type_alias(f, id, node),
        Ast::LambdaType(node) => v.visit_lambda_type(f, id, node),
        Ast::RegularType(node) => v.visit_regular_type(f, id, node),
        Ast::QualifiedPathType(node) => v.visit_qualified_path_type(f, id, node),
        Ast::TupleType(node) => v.visit_tuple_type(f, id, node),
        Ast::Let(node) => v.visit_let_stmt(f, id, node),
        Ast::ExprStmt(node) => v.visit_expr_stmt(f, id, node),
        Ast::Un(node) => v.visit_un(f, id, node),
        Ast::Bin(node) => v.visit_bin(f, id, node),
        Ast::Call(node) => v.visit_call(f, id, node),
        Ast::TypedExpr(node) => v.visit_typed_expr(f, id, node),
        Ast::Path(node) => v.visit_path(f, id, node),
        Ast::PathData(node) => v.visit_path_data(f, id, node),
        Ast::Dot(node) => v.visit_dot(f, id, node),
        Ast::Conv(node) => v.visit_conv(f, id, node),
        Ast::Is(node) => v.visit_is(f, id, node),
        Ast::Lambda(node) => v.visit_lambda(f, id, node),
        Ast::Block(node) => v.visit_block(f, id, node),
        Ast::Template(node) => v.visit_template(f, id, node),
        Ast::If(node) => v.visit_if(f, id, node),
        Ast::For(node) => v.visit_for(f, id, node),
        Ast::While(node) => v.visit_while(f, id, node),
        Ast::Tuple(node) => v.visit_tuple(f, id, node),
        Ast::Paren(node) => v.visit_paren(f, id, node),
        Ast::Match(node) => v.visit_match(f, id, node),
        Ast::MatchArm(node) => v.visit_match_arm(f, id, node),
        Ast::Return(node) => v.visit_return(f, id, node),
        Ast::Break(node) => v.visit_break(f, id, node),
        Ast::Continue(node) => v.visit_continue(f, id, node),
        Ast::This(node) => v.visit_this(f, id, node),
        Ast::UpcaseThis(node) => v.visit_upcase_this(f, id, node),
        Ast::LitChar(node) => v.visit_lit_char(f, id, node),
        Ast::LitInt(node) => v.visit_lit_int(f, id, node),
        Ast::LitFloat(node) => v.visit_lit_float(f, id, node),
        Ast::LitStr(node) => v.visit_lit_str(f, id, node),
        Ast::LitBool(node) => v.visit_lit_bool(f, id, node),
        Ast::Ident(node) => v.visit_ident(f, id, node),
        Ast::TypeArgument(node) => v.visit_type_argument(f, id, node),
        Ast::TypeParamList(node) => v.visit_type_param_list(f, id, node),
        Ast::TypeParam(node) => v.visit_type_param(f, id, node),
        Ast::Underscore(node) => v.visit_underscore(f, id, node),
        Ast::LitPattern(node) => v.visit_lit_pattern(f, id, node),
        Ast::IdentPattern(node) => v.visit_ident_pattern(f, id, node),
        Ast::TuplePattern(node) => v.visit_tuple_pattern(f, id, node),
        Ast::CtorPattern(node) => v.visit_constructor_pattern(f, id, node),
        Ast::CtorField(node) => v.visit_constructor_field(f, id, node),
        Ast::Rest(node) => v.visit_rest(f, id, node),
        Ast::Alt(node) => v.visit_alt(f, id, node),
        Ast::ModifierList(node) => v.visit_modifier_list(f, id, node),
        Ast::Modifier(node) => v.visit_modifier(f, id, node),
        Ast::Error(node) => v.visit_error(f, id, node),
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

pub fn walk_use<V: Visitor>(v: &mut V, f: &File, _id: AstId, use_: &Use) {
    dispatch_ast_id(v, f, use_.path);
}

pub fn walk_use_path<V: Visitor>(v: &mut V, f: &File, _id: AstId, use_path: &UsePath) {
    match use_path.target {
        UsePathDescriptor::As(..) => {}
        UsePathDescriptor::Default => {}
        UsePathDescriptor::Error => {}
        UsePathDescriptor::Group(use_group_id) => dispatch_ast_id(v, f, use_group_id),
    }
}

pub fn walk_use_group<V: Visitor>(v: &mut V, f: &File, _id: AstId, use_group: &UseGroup) {
    for &target_id in &use_group.targets {
        dispatch_ast_id(v, f, target_id);
    }
}

pub fn walk_use_target_name<V: Visitor>(
    v: &mut V,
    f: &File,
    _id: AstId,
    use_target_name: &UseTargetName,
) {
    if let Some(name_id) = use_target_name.name {
        dispatch_ast_id(v, f, name_id);
    }
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

pub fn walk_where_clause<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &WhereClause) {
    for &where_clause_id in &node.clauses {
        dispatch_ast_id(v, f, where_clause_id);
    }
}

pub fn walk_where_clause_item<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &WhereClauseItem) {
    dispatch_ast_id(v, f, node.ty);
    for &bound_id in &node.bounds {
        dispatch_ast_id(v, f, bound_id);
    }
}

pub fn walk_field<V: Visitor>(v: &mut V, f: &File, _id: AstId, field: &Field) {
    dispatch_ast(v, f, field.data_type, f.node(field.data_type));
}

pub fn walk_fct<V: Visitor>(v: &mut V, f: &File, _id: AstId, fct: &Function) {
    for &param_id in &fct.params {
        dispatch_ast_id(v, f, param_id);
    }

    if let Some(ret_id) = fct.return_type {
        dispatch_ast(v, f, ret_id, f.node(ret_id));
    }

    if let Some(block) = fct.block {
        dispatch_ast_id(v, f, block);
    }
}

pub fn walk_param<V: Visitor>(v: &mut V, f: &File, _id: AstId, p: &Param) {
    dispatch_ast(v, f, p.data_type, f.node(p.data_type));
}

pub fn walk_regular_type<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _t: &RegularType) {}

pub fn walk_type_argument<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &TypeArgument) {
    if let Some(name_id) = node.name {
        dispatch_ast_id(v, f, name_id);
    }
    dispatch_ast_id(v, f, node.ty);
}

pub fn walk_type_param_list<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &TypeParamList) {
    for &param_id in &node.params {
        dispatch_ast_id(v, f, param_id);
    }
}

pub fn walk_type_param<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &TypeParam) {
    if let Some(name_id) = node.name {
        dispatch_ast_id(v, f, name_id);
    }

    for &bound_id in &node.bounds {
        dispatch_ast_id(v, f, bound_id);
    }
}

pub fn walk_tuple_type<V: Visitor>(v: &mut V, f: &File, _id: AstId, t: &TupleType) {
    for &ty_id in &t.subtypes {
        dispatch_ast(v, f, ty_id, f.node(ty_id));
    }
}

pub fn walk_lambda_type<V: Visitor>(v: &mut V, f: &File, _id: AstId, t: &LambdaType) {
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
    t: &QualifiedPathType,
) {
    dispatch_ast(v, f, t.ty, f.node(t.ty));
    dispatch_ast(v, f, t.trait_ty, f.node(t.trait_ty));
}

pub fn walk_let_stmt<V: Visitor>(v: &mut V, f: &File, _id: AstId, s: &Let) {
    if let Some(ty) = s.data_type {
        dispatch_ast(v, f, ty, f.node(ty));
    }

    if let Some(e) = s.expr {
        dispatch_ast_id(v, f, e);
    }
}

pub fn walk_expr_stmt<V: Visitor>(v: &mut V, f: &File, _id: AstId, s: &ExprStmt) {
    dispatch_ast_id(v, f, s.expr);
}

pub fn walk_un<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &Un) {
    dispatch_ast_id(v, f, node.opnd);
}

pub fn walk_bin<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &Bin) {
    dispatch_ast_id(v, f, node.lhs);
    dispatch_ast_id(v, f, node.rhs);
}

pub fn walk_call<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &Call) {
    dispatch_ast_id(v, f, node.callee);
    for &arg_id in &node.args {
        dispatch_ast_id(v, f, arg_id);
    }
}

pub fn walk_argument<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &Argument) {
    dispatch_ast_id(v, f, node.expr);
}

pub fn walk_typed_expr<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &TypedExpr) {
    dispatch_ast_id(v, f, node.callee);
    for &arg in &node.args {
        dispatch_ast_id(v, f, arg);
    }
}

pub fn walk_path<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &Path) {
    dispatch_ast_id(v, f, node.lhs);
    dispatch_ast_id(v, f, node.rhs);
}

pub fn walk_path_data<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &PathData) {
    for &segment_id in &node.segments {
        dispatch_ast_id(v, f, segment_id);
    }
}

pub fn walk_dot<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &Dot) {
    dispatch_ast_id(v, f, node.lhs);
    dispatch_ast_id(v, f, node.rhs);
}

pub fn walk_conv<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &Conv) {
    dispatch_ast_id(v, f, node.object);
    dispatch_ast_id(v, f, node.data_type);
}

pub fn walk_is<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &Is) {
    dispatch_ast_id(v, f, node.value);
}

pub fn walk_lambda<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &Lambda) {
    dispatch_ast_id(v, f, node.fct_id);
}

pub fn walk_block<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &Block) {
    for &stmt_id in &node.stmts {
        dispatch_ast_id(v, f, stmt_id);
    }

    if let Some(expr) = node.expr {
        dispatch_ast_id(v, f, expr);
    }
}

pub fn walk_template<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &Template) {
    for &part_id in &node.parts {
        dispatch_ast_id(v, f, part_id);
    }
}

pub fn walk_if<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &If) {
    dispatch_ast_id(v, f, node.cond);
    dispatch_ast_id(v, f, node.then_block);

    if let Some(else_block) = node.else_block {
        dispatch_ast_id(v, f, else_block);
    }
}

pub fn walk_for<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &For) {
    dispatch_ast_id(v, f, node.expr);
    dispatch_ast_id(v, f, node.block);
}

pub fn walk_while<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &While) {
    dispatch_ast_id(v, f, node.cond);
    dispatch_ast_id(v, f, node.block);
}

pub fn walk_tuple<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &Tuple) {
    for &value_id in &node.values {
        dispatch_ast_id(v, f, value_id);
    }
}

pub fn walk_paren<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &Paren) {
    dispatch_ast_id(v, f, node.expr);
}

pub fn walk_match<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &Match) {
    dispatch_ast_id(v, f, node.expr);

    for &arm_id in &node.arms {
        dispatch_ast_id(v, f, arm_id);
    }
}

pub fn walk_match_arm<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &Arm) {
    if let Some(cond_id) = node.cond {
        dispatch_ast_id(v, f, cond_id);
    }

    dispatch_ast_id(v, f, node.value);
}

pub fn walk_return<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &Return) {
    if let Some(expr) = node.expr {
        dispatch_ast_id(v, f, expr);
    }
}

pub fn walk_break<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &Break) {
    // Nothing to do.
}

pub fn walk_continue<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &Continue) {
    // Nothing to do.
}

pub fn walk_this<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &This) {
    // Nothing to do.
}

pub fn walk_upcase_this<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &UpcaseThis) {
    // Nothing to do.
}

pub fn walk_lit_char<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &LitChar) {
    // Nothing to do.
}

pub fn walk_lit_int<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &LitInt) {
    // Nothing to do.
}

pub fn walk_lit_float<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &LitFloat) {
    // Nothing to do.
}

pub fn walk_lit_str<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &LitStr) {
    // Nothing to do.
}

pub fn walk_lit_bool<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &LitBool) {
    // Nothing to do.
}

pub fn walk_ident<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &Ident) {
    // Nothing to do.
}

pub fn walk_error<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &Error) {
    // Nothing to do.
}

pub fn walk_underscore<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &Underscore) {
    // Nothing to do.
}

pub fn walk_lit_pattern<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &LitPattern) {
    unimplemented!()
}

pub fn walk_ident_pattern<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &IdentPattern) {
    unimplemented!()
}

pub fn walk_tuple_pattern<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &TuplePattern) {
    unimplemented!()
}

pub fn walk_rest<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &Rest) {
    unimplemented!()
}

pub fn walk_constructor_pattern<V: Visitor>(
    _v: &mut V,
    _f: &File,
    _id: AstId,
    _node: &CtorPattern,
) {
    unimplemented!()
}

pub fn walk_constructor_field<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &CtorField) {
    unimplemented!()
}

pub fn walk_alt<V: Visitor>(_v: &mut V, _f: &File, _id: AstId, _node: &Alt) {
    unimplemented!()
}

pub fn walk_modifier_list<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &ModifierList) {
    for &arm_id in &node.modifiers {
        dispatch_ast_id(v, f, arm_id);
    }
}

pub fn walk_modifier<V: Visitor>(v: &mut V, f: &File, _id: AstId, node: &Modifier) {
    if let Some(expr) = node.ident {
        dispatch_ast_id(v, f, expr);
    }
}
