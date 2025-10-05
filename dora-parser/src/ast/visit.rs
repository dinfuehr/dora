use crate::ast::*;

macro_rules! for_each_visit {
    ($macro:ident) => {
        $macro!(visit_alt, walk_alt, Alt);
        $macro!(visit_argument, walk_argument, Argument);
        $macro!(visit_bin, walk_bin, Bin);
        $macro!(visit_block, walk_block, Block);
        $macro!(visit_break, walk_break, Break);
        $macro!(visit_call, walk_call, Call);
        $macro!(visit_class, walk_class, Class);
        $macro!(visit_const, walk_const, Const);
        $macro!(visit_constructor_field, walk_constructor_field, CtorField);
        $macro!(
            visit_constructor_pattern,
            walk_constructor_pattern,
            CtorPattern
        );
        $macro!(visit_continue, walk_continue, Continue);
        $macro!(visit_conv, walk_conv, Conv);
        $macro!(visit_dot, walk_dot, Dot);
        $macro!(visit_enum, walk_enum, Enum);
        $macro!(visit_error, walk_error, Error);
        $macro!(visit_expr_stmt, walk_expr_stmt, ExprStmt);
        $macro!(visit_extern, walk_extern, Extern);
        $macro!(visit_fct, walk_fct, Function);
        $macro!(visit_field, walk_field, Field);
        $macro!(visit_for, walk_for, For);
        $macro!(visit_global, walk_global, Global);
        $macro!(visit_ident, walk_ident, Ident);
        $macro!(visit_ident_pattern, walk_ident_pattern, IdentPattern);
        $macro!(visit_if, walk_if, If);
        $macro!(visit_impl, walk_impl, Impl);
        $macro!(visit_is, walk_is, Is);
        $macro!(visit_lambda, walk_lambda, Lambda);
        $macro!(visit_lambda_type, walk_lambda_type, LambdaType);
        $macro!(visit_let_stmt, walk_let_stmt, Let);
        $macro!(visit_lit_bool, walk_lit_bool, LitBool);
        $macro!(visit_lit_char, walk_lit_char, LitChar);
        $macro!(visit_lit_float, walk_lit_float, LitFloat);
        $macro!(visit_lit_int, walk_lit_int, LitInt);
        $macro!(visit_lit_pattern, walk_lit_pattern, LitPattern);
        $macro!(visit_lit_str, walk_lit_str, LitStr);
        $macro!(visit_match, walk_match, Match);
        $macro!(visit_match_arm, walk_match_arm, MatchArm);
        $macro!(visit_modifier, walk_modifier, Modifier);
        $macro!(visit_modifier_list, walk_modifier_list, ModifierList);
        $macro!(visit_module, walk_module, Module);
        $macro!(visit_param, walk_param, Param);
        $macro!(visit_paren, walk_paren, Paren);
        $macro!(visit_path, walk_path, Path);
        $macro!(visit_path_data, walk_path_data, PathData);
        $macro!(
            visit_qualified_path_type,
            walk_qualified_path_type,
            QualifiedPathType
        );
        $macro!(visit_regular_type, walk_regular_type, RegularType);
        $macro!(visit_rest, walk_rest, Rest);
        $macro!(visit_return, walk_return, Return);
        $macro!(visit_root, walk_root, Root);
        $macro!(visit_struct, walk_struct, Struct);
        $macro!(visit_template, walk_template, Template);
        $macro!(visit_this, walk_this, This);
        $macro!(visit_trait, walk_trait, Trait);
        $macro!(visit_tuple, walk_tuple, Tuple);
        $macro!(visit_tuple_pattern, walk_tuple_pattern, TuplePattern);
        $macro!(visit_tuple_type, walk_tuple_type, TupleType);
        $macro!(visit_type_alias, walk_type_alias, Alias);
        $macro!(visit_type_argument, walk_type_argument, TypeArgument);
        $macro!(visit_type_param, walk_type_param, TypeParam);
        $macro!(visit_type_param_list, walk_type_param_list, TypeParamList);
        $macro!(visit_typed_expr, walk_typed_expr, TypedExpr);
        $macro!(visit_un, walk_un, Un);
        $macro!(visit_underscore, walk_underscore, Underscore);
        $macro!(visit_upcase_this, walk_upcase_this, UpcaseThis);
        $macro!(visit_use, walk_use, Use);
        $macro!(visit_use_group, walk_use_group, UseGroup);
        $macro!(visit_use_path, walk_use_path, UsePath);
        $macro!(visit_use_target_name, walk_use_target_name, UseTargetName);
        $macro!(
            visit_where_clause_item,
            walk_where_clause_item,
            WhereClauseItem
        );
        $macro!(visit_where_item, walk_where_clause, WhereClause);
        $macro!(visit_while, walk_while, While);
    };
}

macro_rules! generate_trait_method {
    ($method:ident, $walk_fn:ident, $ty:ident) => {
        fn $method(&mut self, file: &File, id: AstId, _node: &$ty) {
            walk_children(self, file.node2(id));
        }
    };
}

pub trait Visitor: Sized {
    for_each_visit!(generate_trait_method);
}

pub fn visit_node<V: Visitor>(v: &mut V, node: AstNode) {
    let file = &node.file;
    let id = node.id();

    match node.raw_node() {
        Ast::Function(n) => v.visit_fct(file, id, n),
        Ast::Alias(n) => v.visit_type_alias(file, id, n),
        Ast::Alt(n) => v.visit_alt(file, id, n),
        Ast::Argument(n) => v.visit_argument(file, id, n),
        Ast::Bin(n) => v.visit_bin(file, id, n),
        Ast::Block(n) => v.visit_block(file, id, n),
        Ast::Break(n) => v.visit_break(file, id, n),
        Ast::Call(n) => v.visit_call(file, id, n),
        Ast::Class(n) => v.visit_class(file, id, n),
        Ast::Const(n) => v.visit_const(file, id, n),
        Ast::Continue(n) => v.visit_continue(file, id, n),
        Ast::Conv(n) => v.visit_conv(file, id, n),
        Ast::CtorField(n) => v.visit_constructor_field(file, id, n),
        Ast::CtorPattern(n) => v.visit_constructor_pattern(file, id, n),
        Ast::Dot(n) => v.visit_dot(file, id, n),
        Ast::Enum(n) => v.visit_enum(file, id, n),
        Ast::Error(n) => v.visit_error(file, id, n),
        Ast::ExprStmt(n) => v.visit_expr_stmt(file, id, n),
        Ast::Extern(n) => v.visit_extern(file, id, n),
        Ast::Field(n) => v.visit_field(file, id, n),
        Ast::For(n) => v.visit_for(file, id, n),
        Ast::Global(n) => v.visit_global(file, id, n),
        Ast::Ident(n) => v.visit_ident(file, id, n),
        Ast::IdentPattern(n) => v.visit_ident_pattern(file, id, n),
        Ast::If(n) => v.visit_if(file, id, n),
        Ast::Impl(n) => v.visit_impl(file, id, n),
        Ast::Is(n) => v.visit_is(file, id, n),
        Ast::Lambda(n) => v.visit_lambda(file, id, n),
        Ast::LambdaType(n) => v.visit_lambda_type(file, id, n),
        Ast::Let(n) => v.visit_let_stmt(file, id, n),
        Ast::LitBool(n) => v.visit_lit_bool(file, id, n),
        Ast::LitChar(n) => v.visit_lit_char(file, id, n),
        Ast::LitFloat(n) => v.visit_lit_float(file, id, n),
        Ast::LitInt(n) => v.visit_lit_int(file, id, n),
        Ast::LitPattern(n) => v.visit_lit_pattern(file, id, n),
        Ast::LitStr(n) => v.visit_lit_str(file, id, n),
        Ast::Match(n) => v.visit_match(file, id, n),
        Ast::MatchArm(n) => v.visit_match_arm(file, id, n),
        Ast::Modifier(n) => v.visit_modifier(file, id, n),
        Ast::ModifierList(n) => v.visit_modifier_list(file, id, n),
        Ast::Module(n) => v.visit_module(file, id, n),
        Ast::Param(n) => v.visit_param(file, id, n),
        Ast::Paren(n) => v.visit_paren(file, id, n),
        Ast::Path(n) => v.visit_path(file, id, n),
        Ast::PathData(n) => v.visit_path_data(file, id, n),
        Ast::QualifiedPathType(n) => v.visit_qualified_path_type(file, id, n),
        Ast::RegularType(n) => v.visit_regular_type(file, id, n),
        Ast::Rest(n) => v.visit_rest(file, id, n),
        Ast::Return(n) => v.visit_return(file, id, n),
        Ast::Root(n) => v.visit_root(file, id, n),
        Ast::Struct(n) => v.visit_struct(file, id, n),
        Ast::Template(n) => v.visit_template(file, id, n),
        Ast::This(n) => v.visit_this(file, id, n),
        Ast::Trait(n) => v.visit_trait(file, id, n),
        Ast::Tuple(n) => v.visit_tuple(file, id, n),
        Ast::TuplePattern(n) => v.visit_tuple_pattern(file, id, n),
        Ast::TupleType(n) => v.visit_tuple_type(file, id, n),
        Ast::TypeArgument(n) => v.visit_type_argument(file, id, n),
        Ast::TypedExpr(n) => v.visit_typed_expr(file, id, n),
        Ast::TypeParamList(n) => v.visit_type_param_list(file, id, n),
        Ast::TypeParam(n) => v.visit_type_param(file, id, n),
        Ast::Un(n) => v.visit_un(file, id, n),
        Ast::Underscore(n) => v.visit_underscore(file, id, n),
        Ast::UpcaseThis(n) => v.visit_upcase_this(file, id, n),
        Ast::Use(n) => v.visit_use(file, id, n),
        Ast::UseGroup(n) => v.visit_use_group(file, id, n),
        Ast::UsePath(n) => v.visit_use_path(file, id, n),
        Ast::UseTargetName(n) => v.visit_use_target_name(file, id, n),
        Ast::WhereClause(n) => v.visit_where_item(file, id, n),
        Ast::WhereClauseItem(n) => v.visit_where_clause_item(file, id, n),
        Ast::While(n) => v.visit_while(file, id, n),
    }
}

pub fn walk_children<V: Visitor, N: AstNodeBase>(v: &mut V, node: N) {
    for child in node.children() {
        visit_node(v, child);
    }
}
