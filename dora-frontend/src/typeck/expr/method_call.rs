use dora_parser::Span;
use dora_parser::ast::{self, SyntaxNodeBase};

use crate::sema::{ExprId, MethodCallExpr};
use crate::typeck::{CallArguments, TypeCheck, check_expr_id, check_expr_opt};
use crate::{SourceType, SourceTypeArray};

use crate::typeck::call::check_expr_call_method;

pub(crate) fn check_expr_method_call(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &MethodCallExpr,
    _expected_ty: SourceType,
) -> SourceType {
    let object_type = check_expr_id(ck, sema_expr.object, SourceType::Any);
    let method_name = ck.sa.interner.str(sema_expr.name).to_string();

    let type_params: SourceTypeArray = SourceTypeArray::with(
        sema_expr
            .type_params
            .iter()
            .map(|&type_ref_id| ck.read_type_id(type_ref_id))
            .collect(),
    );

    let expr = ck.syntax_by_id::<ast::AstMethodCallExpr>(expr_id);

    let arguments = create_method_call_arguments(ck, expr_id);

    // Compute span from object to method name (for error reporting on field access)
    let object_span = ck.expr_span(sema_expr.object);
    let name_token = expr.name();
    let name_span = name_token.span();
    let callee_span = Span::new(object_span.start(), name_span.end() - object_span.start());

    // Load AST nodes for check_expr_call_method (still uses AST)
    let call_expr: ast::AstExpr = expr.clone().into();
    let object_expr = ck.syntax_by_id::<ast::AstExpr>(sema_expr.object);
    check_expr_call_method(
        ck,
        call_expr,
        object_expr,
        callee_span,
        object_type,
        method_name,
        type_params,
        arguments,
    )
}

pub(crate) fn create_method_call_arguments(ck: &mut TypeCheck, expr_id: ExprId) -> CallArguments {
    let node = ck.syntax_by_id::<ast::AstMethodCallExpr>(expr_id);
    let args = node.arg_list();

    let mut arguments = CallArguments {
        arguments: Vec::new(),
        span: node.span(),
    };

    for arg in args.items() {
        let ty = check_expr_opt(ck, arg.expr(), SourceType::Any);
        ck.body.set_ty(arg.id(), ty);

        arguments.arguments.push(arg);
    }

    arguments
}
