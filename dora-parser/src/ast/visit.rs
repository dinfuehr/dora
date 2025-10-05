use crate::ast::*;

macro_rules! for_each_visit {
    ($macro:ident) => {
        $macro!(visit_alt, Alt, AstAlt);
        $macro!(visit_argument, Argument, AstArgument);
        $macro!(visit_bin, Bin, AstBin);
        $macro!(visit_block, Block, AstBlock);
        $macro!(visit_break, Break, AstBreak);
        $macro!(visit_call, Call, AstCall);
        $macro!(visit_class, Class, AstClass);
        $macro!(visit_const, Const, AstConst);
        $macro!(visit_constructor_field, CtorField, AstCtorField);
        $macro!(visit_constructor_pattern, CtorPattern, AstCtorPattern);
        $macro!(visit_continue, Continue, AstContinue);
        $macro!(visit_conv, Conv, AstConv);
        $macro!(visit_dot, Dot, AstDot);
        $macro!(visit_enum, Enum, AstEnum);
        $macro!(visit_error, Error, AstError);
        $macro!(visit_expr_stmt, ExprStmt, AstExprStmt);
        $macro!(visit_extern, Extern, AstExtern);
        $macro!(visit_function, Function, AstFunction);
        $macro!(visit_field, Field, AstField);
        $macro!(visit_for, For, AstFor);
        $macro!(visit_global, Global, AstGlobal);
        $macro!(visit_ident, Ident, AstIdent);
        $macro!(visit_ident_pattern, IdentPattern, AstIdentPattern);
        $macro!(visit_if, If, AstIf);
        $macro!(visit_impl, Impl, AstImpl);
        $macro!(visit_is, Is, AstIs);
        $macro!(visit_lambda, Lambda, AstLambda);
        $macro!(visit_lambda_type, LambdaType, AstLambdaType);
        $macro!(visit_let_stmt, Let, AstLet);
        $macro!(visit_lit_bool, LitBool, AstLitBool);
        $macro!(visit_lit_char, LitChar, AstLitChar);
        $macro!(visit_lit_float, LitFloat, AstLitFloat);
        $macro!(visit_lit_int, LitInt, AstLitInt);
        $macro!(visit_lit_pattern, LitPattern, AstLitPattern);
        $macro!(visit_lit_str, LitStr, AstLitStr);
        $macro!(visit_match, Match, AstMatch);
        $macro!(visit_match_arm, MatchArm, AstMatchArm);
        $macro!(visit_modifier, Modifier, AstModifier);
        $macro!(visit_modifier_list, ModifierList, AstModifierList);
        $macro!(visit_module, Module, AstModule);
        $macro!(visit_param, Param, AstParam);
        $macro!(visit_paren, Paren, AstParen);
        $macro!(visit_path, Path, AstPath);
        $macro!(visit_path_data, PathData, AstPathData);
        $macro!(
            visit_qualified_path_type,
            QualifiedPathType,
            AstQualifiedPathType
        );
        $macro!(visit_regular_type, RegularType, AstRegularType);
        $macro!(visit_rest, Rest, AstRest);
        $macro!(visit_return, Return, AstReturn);
        $macro!(visit_root, Root, AstRoot);
        $macro!(visit_struct, Struct, AstStruct);
        $macro!(visit_template, Template, AstTemplate);
        $macro!(visit_this, This, AstThis);
        $macro!(visit_trait, Trait, AstTrait);
        $macro!(visit_tuple, Tuple, AstTuple);
        $macro!(visit_tuple_pattern, TuplePattern, AstTuplePattern);
        $macro!(visit_tuple_type, TupleType, AstTupleType);
        $macro!(visit_alias, Alias, AstAlias);
        $macro!(visit_type_argument, TypeArgument, AstTypeArgument);
        $macro!(visit_type_param, TypeParam, AstTypeParam);
        $macro!(visit_type_param_list, TypeParamList, AstTypeParamList);
        $macro!(visit_typed_expr, TypedExpr, AstTypedExpr);
        $macro!(visit_un, Un, AstUn);
        $macro!(visit_underscore, Underscore, AstUnderscore);
        $macro!(visit_upcase_this, UpcaseThis, AstUpcaseThis);
        $macro!(visit_use, Use, AstUse);
        $macro!(visit_use_group, UseGroup, AstUseGroup);
        $macro!(visit_use_path, UsePath, AstUsePath);
        $macro!(visit_use_target_name, UseTargetName, AstUseTargetName);
        $macro!(visit_where_clause_item, WhereClauseItem, AstWhereClauseItem);
        $macro!(visit_where_item, WhereClause, AstWhereClause);
        $macro!(visit_while, While, AstWhile);
    };
}

macro_rules! generate_trait_method {
    ($method:ident, $ty:ident, $ast_node_ty:ident) => {
        fn $method(&mut self, file: &File, id: AstId, _node: &$ty, _ast_node: $ast_node_ty) {
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
        Ast::Function(n) => v.visit_function(file, id, n, node.clone().as_function()),
        Ast::Alias(n) => v.visit_alias(file, id, n, node.clone().as_alias()),
        Ast::Alt(n) => v.visit_alt(file, id, n, node.clone().as_alt()),
        Ast::Argument(n) => v.visit_argument(file, id, n, node.clone().as_argument()),
        Ast::Bin(n) => v.visit_bin(file, id, n, node.clone().as_bin()),
        Ast::Block(n) => v.visit_block(file, id, n, node.clone().as_block()),
        Ast::Break(n) => v.visit_break(file, id, n, node.clone().as_break()),
        Ast::Call(n) => v.visit_call(file, id, n, node.clone().as_call()),
        Ast::Class(n) => v.visit_class(file, id, n, node.clone().as_class()),
        Ast::Const(n) => v.visit_const(file, id, n, node.clone().as_const()),
        Ast::Continue(n) => v.visit_continue(file, id, n, node.clone().as_continue()),
        Ast::Conv(n) => v.visit_conv(file, id, n, node.clone().as_conv()),
        Ast::CtorField(n) => v.visit_constructor_field(file, id, n, node.clone().as_ctor_field()),
        Ast::CtorPattern(n) => {
            v.visit_constructor_pattern(file, id, n, node.clone().as_ctor_pattern())
        }
        Ast::Dot(n) => v.visit_dot(file, id, n, node.clone().as_dot()),
        Ast::Enum(n) => v.visit_enum(file, id, n, node.clone().as_enum()),
        Ast::Error(n) => v.visit_error(file, id, n, node.clone().as_error()),
        Ast::ExprStmt(n) => v.visit_expr_stmt(file, id, n, node.clone().as_expr_stmt()),
        Ast::Extern(n) => v.visit_extern(file, id, n, node.clone().as_extern()),
        Ast::Field(n) => v.visit_field(file, id, n, node.clone().as_field()),
        Ast::For(n) => v.visit_for(file, id, n, node.clone().as_for()),
        Ast::Global(n) => v.visit_global(file, id, n, node.clone().as_global()),
        Ast::Ident(n) => v.visit_ident(file, id, n, node.clone().as_ident()),
        Ast::IdentPattern(n) => v.visit_ident_pattern(file, id, n, node.clone().as_ident_pattern()),
        Ast::If(n) => v.visit_if(file, id, n, node.clone().as_if()),
        Ast::Impl(n) => v.visit_impl(file, id, n, node.clone().as_impl()),
        Ast::Is(n) => v.visit_is(file, id, n, node.clone().as_is()),
        Ast::Lambda(n) => v.visit_lambda(file, id, n, node.clone().as_lambda()),
        Ast::LambdaType(n) => v.visit_lambda_type(file, id, n, node.clone().as_lambda_type()),
        Ast::Let(n) => v.visit_let_stmt(file, id, n, node.clone().as_let()),
        Ast::LitBool(n) => v.visit_lit_bool(file, id, n, node.clone().as_lit_bool()),
        Ast::LitChar(n) => v.visit_lit_char(file, id, n, node.clone().as_lit_char()),
        Ast::LitFloat(n) => v.visit_lit_float(file, id, n, node.clone().as_lit_float()),
        Ast::LitInt(n) => v.visit_lit_int(file, id, n, node.clone().as_lit_int()),
        Ast::LitPattern(n) => v.visit_lit_pattern(file, id, n, node.clone().as_lit_pattern()),
        Ast::LitStr(n) => v.visit_lit_str(file, id, n, node.clone().as_lit_str()),
        Ast::Match(n) => v.visit_match(file, id, n, node.clone().as_match()),
        Ast::MatchArm(n) => v.visit_match_arm(file, id, n, node.clone().as_match_arm()),
        Ast::Modifier(n) => v.visit_modifier(file, id, n, node.clone().as_modifier()),
        Ast::ModifierList(n) => v.visit_modifier_list(file, id, n, node.clone().as_modifier_list()),
        Ast::Module(n) => v.visit_module(file, id, n, node.clone().as_module()),
        Ast::Param(n) => v.visit_param(file, id, n, node.clone().as_param()),
        Ast::Paren(n) => v.visit_paren(file, id, n, node.clone().as_paren()),
        Ast::Path(n) => v.visit_path(file, id, n, node.clone().as_path()),
        Ast::PathData(n) => v.visit_path_data(file, id, n, node.clone().as_path_data()),
        Ast::QualifiedPathType(n) => {
            v.visit_qualified_path_type(file, id, n, node.clone().as_qualified_path_type())
        }
        Ast::RegularType(n) => v.visit_regular_type(file, id, n, node.clone().as_regular_type()),
        Ast::Rest(n) => v.visit_rest(file, id, n, node.clone().as_rest()),
        Ast::Return(n) => v.visit_return(file, id, n, node.clone().as_return()),
        Ast::Root(n) => v.visit_root(file, id, n, node.clone().as_root()),
        Ast::Struct(n) => v.visit_struct(file, id, n, node.clone().as_struct()),
        Ast::Template(n) => v.visit_template(file, id, n, node.clone().as_template()),
        Ast::This(n) => v.visit_this(file, id, n, node.clone().as_this()),
        Ast::Trait(n) => v.visit_trait(file, id, n, node.clone().as_trait()),
        Ast::Tuple(n) => v.visit_tuple(file, id, n, node.clone().as_tuple()),
        Ast::TuplePattern(n) => v.visit_tuple_pattern(file, id, n, node.clone().as_tuple_pattern()),
        Ast::TupleType(n) => v.visit_tuple_type(file, id, n, node.clone().as_tuple_type()),
        Ast::TypeArgument(n) => v.visit_type_argument(file, id, n, node.clone().as_type_argument()),
        Ast::TypedExpr(n) => v.visit_typed_expr(file, id, n, node.clone().as_typed_expr()),
        Ast::TypeParamList(n) => {
            v.visit_type_param_list(file, id, n, node.clone().as_type_param_list())
        }
        Ast::TypeParam(n) => v.visit_type_param(file, id, n, node.clone().as_type_param()),
        Ast::Un(n) => v.visit_un(file, id, n, node.clone().as_un()),
        Ast::Underscore(n) => v.visit_underscore(file, id, n, node.clone().as_underscore()),
        Ast::UpcaseThis(n) => v.visit_upcase_this(file, id, n, node.clone().as_upcase_this()),
        Ast::Use(n) => v.visit_use(file, id, n, node.clone().as_use()),
        Ast::UseGroup(n) => v.visit_use_group(file, id, n, node.clone().as_use_group()),
        Ast::UsePath(n) => v.visit_use_path(file, id, n, node.clone().as_use_path()),
        Ast::UseTargetName(n) => {
            v.visit_use_target_name(file, id, n, node.clone().as_use_target_name())
        }
        Ast::WhereClause(n) => v.visit_where_item(file, id, n, node.clone().as_where_clause()),
        Ast::WhereClauseItem(n) => {
            v.visit_where_clause_item(file, id, n, node.clone().as_where_clause_item())
        }
        Ast::While(n) => v.visit_while(file, id, n, node.clone().as_while()),
    }
}

pub fn walk_children<V: Visitor, N: AstNodeBase>(v: &mut V, node: N) {
    for child in node.children() {
        visit_node(v, child);
    }
}
