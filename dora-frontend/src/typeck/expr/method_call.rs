use dora_parser::Span;
use dora_parser::ast::{self, SyntaxNodeBase};

use crate::sema::{ExprId, MethodCallExpr};
use crate::typeck::{CallArguments, TypeCheck, check_expr, check_expr_opt};
use crate::{SourceType, SourceTypeArray};

use crate::typeck::call::check_expr_call_method;

pub(crate) fn check_expr_method_call(
    ck: &mut TypeCheck,
    _expr_id: ExprId,
    expr: ast::AstMethodCallExpr,
    _sema_expr: &MethodCallExpr,
    _expected_ty: SourceType,
) -> SourceType {
    let object_type = check_expr(ck, expr.object(), SourceType::Any);
    let name_token = expr.name();
    let method_name = name_token.text().to_string();

    let type_params: SourceTypeArray = if let Some(type_arg_list) = expr.type_argument_list() {
        SourceTypeArray::with(
            type_arg_list
                .items()
                .map(|arg| ck.read_type_opt(arg.ty()))
                .collect(),
        )
    } else {
        SourceTypeArray::empty()
    };

    let arguments = create_method_call_arguments(ck, &expr);

    // Compute span from object to method name (for error reporting on field access)
    let object_span = expr.object().span();
    let name_span = name_token.span();
    let callee_span = Span::new(object_span.start(), name_span.end() - object_span.start());

    let call_expr: ast::AstExpr = expr.clone().into();
    check_expr_call_method(
        ck,
        call_expr,
        expr.object(),
        callee_span,
        object_type,
        method_name,
        type_params,
        arguments,
    )
}

pub(crate) fn create_method_call_arguments(
    ck: &mut TypeCheck,
    node: &ast::AstMethodCallExpr,
) -> CallArguments {
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
