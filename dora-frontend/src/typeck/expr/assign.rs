use std::sync::Arc;

use dora_parser::Span;
use dora_parser::TokenKind;
use dora_parser::ast::{self, SyntaxNodeBase};

use super::bin::OpTraitInfo;
use super::field::check_expr_field_named;
use super::{check_expr, create_call_arguments, create_method_call_arguments};
use crate::access::{class_field_accessible_from, struct_field_accessible_from};
use crate::args;
use crate::error::diagnostics::{
    ASSIGN_FIELD, ASSIGN_TYPE, BIN_OP_TYPE, EXPECTED_MODULE, IMMUTABLE_FIELD,
    INDEX_GET_AND_INDEX_SET_DO_NOT_MATCH, INDEX_GET_NOT_IMPLEMENTED, INDEX_SET_NOT_IMPLEMENTED,
    LET_REASSIGNED, LVALUE_EXPECTED, NOT_ACCESSIBLE, UNKNOWN_FIELD, UNKNOWN_IDENTIFIER,
    WRONG_TYPE_FOR_ARGUMENT,
};
use crate::replace_type;
use crate::sema::{
    ArrayAssignment, AssignExpr, CallType, Expr, ExprId, FieldIndex, IdentType, TraitDefinitionId,
    find_field_in_class, find_impl, implements_trait,
};
use crate::ty::TraitType;
use crate::typeck::TypeCheck;
use crate::{SourceType, SourceTypeArray, SymbolKind, ty::error as ty_error};

pub(super) fn check_expr_assign(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &AssignExpr,
) -> SourceType {
    let lhs_expr = ck.expr(sema_expr.lhs);

    match lhs_expr {
        Expr::Call(..) => check_expr_assign_call(ck, expr_id, sema_expr),
        Expr::MethodCall(..) => check_expr_assign_method_call(ck, expr_id, sema_expr),
        Expr::Field(..) => check_expr_assign_field(ck, expr_id, sema_expr),
        Expr::Path(..) => check_expr_assign_ident(ck, expr_id, sema_expr),
        _ => {
            ck.report(ck.expr_span(expr_id), &LVALUE_EXPECTED, args![]);
        }
    }

    ck.body.set_ty(expr_id, SourceType::Unit);
    SourceType::Unit
}

fn check_expr_assign_ident(ck: &mut TypeCheck, expr_id: ExprId, sema_expr: &AssignExpr) {
    let lhs_id = sema_expr.lhs;
    let name_expr = ck.expr(lhs_id).as_path();
    let path = &name_expr.path;

    // Single segment: simple identifier assignment
    if path.len() == 1 {
        let interned_name = path[0].name;
        let sym = ck.symtable.get(interned_name);

        let lhs_type = match sym {
            Some(SymbolKind::Var(var_id)) => {
                if !ck.vars.get_var(var_id).mutable {
                    ck.report(ck.expr_span(expr_id), &LET_REASSIGNED, args![]);
                }

                // Variable may have to be context-allocated.
                let ident = ck.maybe_allocate_in_context(var_id);
                ck.body.insert_ident(lhs_id, ident);

                ck.vars.get_var(var_id).ty.clone()
            }

            Some(SymbolKind::Global(global_id)) => {
                let global_var = ck.sa.global(global_id);

                if !global_var.mutable {
                    ck.report(ck.expr_span(expr_id), &LET_REASSIGNED, args![]);
                }

                ck.body.insert_ident(lhs_id, IdentType::Global(global_id));
                global_var.ty()
            }

            None => {
                ck.report(
                    ck.expr_span(lhs_id),
                    &UNKNOWN_IDENTIFIER,
                    args![ck.path_name(path)],
                );

                return;
            }

            _ => {
                ck.report(ck.expr_span(lhs_id), &LVALUE_EXPECTED, args![]);

                return;
            }
        };

        let rhs_type = check_expr(ck, sema_expr.rhs, lhs_type.clone());
        check_assign_type(ck, expr_id, sema_expr.op, lhs_type, rhs_type);
        return;
    }

    // Multi-segment path: resolve through modules
    let first_name = path[0].name;
    let mut sym = ck.symtable.get(first_name);

    // Resolve intermediate segments
    for segment in &path[1..path.len() - 1] {
        match sym {
            Some(SymbolKind::Module(module_id)) => {
                let module = ck.sa.module(module_id);
                let symtable = module.table();
                sym = symtable.get(segment.name);
            }
            _ => {
                ck.report(ck.expr_span(lhs_id), &EXPECTED_MODULE, args![]);
                return;
            }
        }
    }

    // Handle the last segment - must resolve to a global
    let last_name = path[path.len() - 1].name;

    let lhs_type = match sym {
        Some(SymbolKind::Module(module_id)) => {
            let module = ck.sa.module(module_id);
            let symtable = module.table();
            let final_sym = symtable.get(last_name);

            match final_sym {
                Some(SymbolKind::Global(global_id)) => {
                    let global = ck.sa.global(global_id);
                    if !global.mutable {
                        ck.report(ck.expr_span(expr_id), &LET_REASSIGNED, args![]);
                    }
                    ck.body.insert_ident(lhs_id, IdentType::Global(global_id));
                    global.ty()
                }
                _ => {
                    ck.report(ck.expr_span(lhs_id), &LVALUE_EXPECTED, args![]);
                    return;
                }
            }
        }
        _ => {
            ck.report(ck.expr_span(lhs_id), &LVALUE_EXPECTED, args![]);
            return;
        }
    };

    let rhs_type = check_expr(ck, sema_expr.rhs, lhs_type.clone());
    check_assign_type(ck, expr_id, sema_expr.op, lhs_type, rhs_type);
}

fn check_assign_type(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    op: ast::AssignOp,
    lhs_type: SourceType,
    rhs_type: SourceType,
) -> OpTraitInfo {
    ck.body.set_ty(expr_id, SourceType::Unit);

    match op {
        ast::AssignOp::AddAssign => check_expr_assign_trait(
            ck,
            expr_id,
            op,
            ck.sa.known.traits.add(),
            "add",
            lhs_type,
            rhs_type,
        ),

        ast::AssignOp::SubAssign => check_expr_assign_trait(
            ck,
            expr_id,
            op,
            ck.sa.known.traits.sub(),
            "sub",
            lhs_type,
            rhs_type,
        ),

        ast::AssignOp::MulAssign => check_expr_assign_trait(
            ck,
            expr_id,
            op,
            ck.sa.known.traits.mul(),
            "mul",
            lhs_type,
            rhs_type,
        ),

        ast::AssignOp::DivAssign => check_expr_assign_trait(
            ck,
            expr_id,
            op,
            ck.sa.known.traits.div(),
            "div",
            lhs_type,
            rhs_type,
        ),

        ast::AssignOp::ModAssign => check_expr_assign_trait(
            ck,
            expr_id,
            op,
            ck.sa.known.traits.mod_(),
            "modulo",
            lhs_type,
            rhs_type,
        ),

        ast::AssignOp::BitOrAssign => check_expr_assign_trait(
            ck,
            expr_id,
            op,
            ck.sa.known.traits.bit_or(),
            "bitor",
            lhs_type,
            rhs_type,
        ),

        ast::AssignOp::BitAndAssign => check_expr_assign_trait(
            ck,
            expr_id,
            op,
            ck.sa.known.traits.bit_and(),
            "bitand",
            lhs_type,
            rhs_type,
        ),

        ast::AssignOp::BitXorAssign => check_expr_assign_trait(
            ck,
            expr_id,
            op,
            ck.sa.known.traits.bit_xor(),
            "bitxor",
            lhs_type,
            rhs_type,
        ),

        ast::AssignOp::ShiftLAssign => check_expr_assign_trait(
            ck,
            expr_id,
            op,
            ck.sa.known.traits.shl(),
            "shl",
            lhs_type,
            rhs_type,
        ),

        ast::AssignOp::LogicalShiftRAssign => check_expr_assign_trait(
            ck,
            expr_id,
            op,
            ck.sa.known.traits.shr(),
            "shr",
            lhs_type,
            rhs_type,
        ),

        ast::AssignOp::ArithShiftRAssign => check_expr_assign_trait(
            ck,
            expr_id,
            op,
            ck.sa.known.traits.sar(),
            "sar",
            lhs_type,
            rhs_type,
        ),

        ast::AssignOp::Assign => {
            if !lhs_type.is_error()
                && !rhs_type.is_error()
                && !lhs_type.allows(ck.sa, rhs_type.clone())
            {
                let lhs_type = ck.ty_name(&lhs_type);
                let rhs_type = ck.ty_name(&rhs_type);

                ck.report(
                    ck.expr_span(expr_id),
                    &ASSIGN_TYPE,
                    args![lhs_type, rhs_type],
                );
            }

            OpTraitInfo {
                rhs_type: ty_error(),
                return_type: ty_error(),
            }
        }
    }
}

fn check_expr_assign_trait(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    op: ast::AssignOp,
    trait_id: TraitDefinitionId,
    trait_method_name: &str,
    lhs_type: SourceType,
    rhs_type: SourceType,
) -> OpTraitInfo {
    let trait_ty = TraitType::from_trait_id(trait_id);

    let impl_match = find_impl(
        ck.sa,
        ck.element,
        lhs_type.clone(),
        &ck.type_param_definition,
        trait_ty.clone(),
    );
    let trait_method_name = ck.sa.interner.intern(trait_method_name);

    if let Some(impl_match) = impl_match {
        let type_params = impl_match.bindings;

        let trait_ = &ck.sa.trait_(trait_id);
        let trait_method_id = trait_
            .get_method(trait_method_name, false)
            .expect("missing method");
        let method_id = ck
            .sa
            .impl_(impl_match.id)
            .get_method_for_trait_method_id(trait_method_id);

        if let Some(method_id) = method_id {
            let call_type = CallType::Method(lhs_type.clone(), method_id, type_params.clone());
            ck.body
                .insert_or_replace_call_type(expr_id, Arc::new(call_type));

            let method = ck.sa.fct(method_id);
            let params = method.params_without_self();

            assert_eq!(params.len(), 1);

            let param = params[0].ty();
            let param = replace_type(ck.sa, param, Some(&type_params), None);

            if !param.allows(ck.sa, rhs_type.clone())
                && !lhs_type.is_error()
                && !rhs_type.is_error()
            {
                let lhs_type = ck.ty_name(&lhs_type);
                let rhs_type = ck.ty_name(&rhs_type);
                ck.report(
                    ck.expr_span(expr_id),
                    &BIN_OP_TYPE,
                    args![op.as_str().to_string(), lhs_type, rhs_type],
                );
            }

            let return_type = method.return_type();
            ck.body.set_ty(expr_id, return_type.clone());

            OpTraitInfo {
                rhs_type,
                return_type,
            }
        } else {
            ck.body.set_ty(expr_id, ty_error());
            OpTraitInfo {
                rhs_type: ty_error(),
                return_type: ty_error(),
            }
        }
    } else if lhs_type.is_type_param()
        && implements_trait(ck.sa, lhs_type.clone(), ck.element, trait_ty)
    {
        let trait_ = ck.sa.trait_(trait_id);

        let method_id = trait_
            .get_method(trait_method_name, false)
            .expect("method not found");

        let method = ck.sa.fct(method_id);
        let params = method.params_without_self();

        let call_type = CallType::GenericMethod(
            lhs_type.type_param_id().expect("type param expected"),
            trait_id,
            method_id,
            SourceTypeArray::empty(),
            SourceTypeArray::empty(),
        );
        ck.body
            .insert_or_replace_call_type(expr_id, Arc::new(call_type));

        let param = params[0].ty();
        let param = replace_type(
            ck.sa,
            param,
            Some(&SourceTypeArray::empty()),
            Some(lhs_type.clone()),
        );

        if !param.allows(ck.sa, rhs_type.clone()) && !lhs_type.is_error() && !rhs_type.is_error() {
            let lhs_type = ck.ty_name(&lhs_type);
            let rhs_type = ck.ty_name(&rhs_type);
            ck.report(
                ck.expr_span(expr_id),
                &BIN_OP_TYPE,
                args![op.as_str().to_string(), lhs_type, rhs_type],
            );
        }

        let return_type = method.return_type();
        ck.body.set_ty(expr_id, return_type.clone());

        OpTraitInfo {
            rhs_type,
            return_type,
        }
    } else {
        if !lhs_type.is_error() && !rhs_type.is_error() {
            let lhs_type_name = ck.ty_name(&lhs_type);
            let rhs_type = ck.ty_name(&rhs_type);
            ck.report(
                ck.expr_span(expr_id),
                &BIN_OP_TYPE,
                args![op.as_str().to_string(), lhs_type_name, rhs_type],
            );
        }
        ck.body.set_ty(expr_id, ty_error());
        OpTraitInfo {
            rhs_type: ty_error(),
            return_type: ty_error(),
        }
    }
}

fn check_expr_assign_call(ck: &mut TypeCheck, expr_id: ExprId, sema_expr: &AssignExpr) {
    let lhs_id = sema_expr.lhs;
    let call_expr = ck.expr(lhs_id).as_call();
    let object_type = check_expr(ck, call_expr.callee, SourceType::Any);

    let args = create_call_arguments(ck, lhs_id, call_expr);

    let value_type = check_expr(ck, sema_expr.rhs, SourceType::Any);
    ck.body.set_ty(sema_expr.rhs, value_type.clone());

    let mut array_assignment = ArrayAssignment::new();
    let index_type;
    let item_type;
    let rhs_type;

    if sema_expr.op == ast::AssignOp::Assign {
        (index_type, item_type) = check_index_trait_on_ty(
            ck,
            expr_id,
            &mut array_assignment,
            object_type.clone(),
            false,
        );
        rhs_type = item_type.clone();
    } else {
        let (index_get_index, index_get_item) = check_index_trait_on_ty(
            ck,
            expr_id,
            &mut array_assignment,
            object_type.clone(),
            true,
        );

        let (index_set_index, index_set_item) = check_index_trait_on_ty(
            ck,
            expr_id,
            &mut array_assignment,
            object_type.clone(),
            false,
        );

        if (index_get_index != index_set_index
            && !index_get_index.is_error()
            && !index_set_index.is_error())
            || (index_get_item != index_set_item
                && !index_get_item.is_error()
                && !index_set_item.is_error())
        {
            ck.report(
                ck.expr_span(call_expr.callee),
                &INDEX_GET_AND_INDEX_SET_DO_NOT_MATCH,
                args![],
            );
        }

        index_type = index_get_index;
        item_type = index_get_item.clone();

        let op_trait_info = check_assign_type(
            ck,
            expr_id,
            sema_expr.op,
            index_get_item,
            value_type.clone(),
        );
        rhs_type = op_trait_info.rhs_type;
    }

    let arg_index_type = args
        .arguments
        .get(0)
        .map(|arg| ck.ty(arg.id()))
        .unwrap_or(ty_error());

    if !index_type.allows(ck.sa, arg_index_type.clone()) && !index_type.is_error() {
        let arg = &args.arguments[0];

        let exp = ck.ty_name(&index_type);
        let got = ck.ty_name(&arg_index_type);

        ck.report(arg.span(), &WRONG_TYPE_FOR_ARGUMENT, args![exp, got]);
    }

    if !rhs_type.allows(ck.sa, value_type.clone()) && !rhs_type.is_error() && !value_type.is_error()
    {
        let exp = ck.ty_name(&rhs_type);
        let got = ck.ty_name(&value_type);

        ck.report(
            ck.expr_span(sema_expr.rhs),
            &WRONG_TYPE_FOR_ARGUMENT,
            args![exp, got],
        );
    }

    for arg in &args.arguments {
        if let Some(name_ident) = arg.name() {
            ck.report(
                name_ident.span(),
                &crate::error::diagnostics::UNEXPECTED_NAMED_ARGUMENT,
                args![],
            );
        }
    }

    if args.arguments.len() > 1 {
        for arg in &args.arguments[1..] {
            ck.report(
                arg.span(),
                &crate::error::diagnostics::SUPERFLUOUS_ARGUMENT,
                args![],
            );
        }
    }

    ck.body.set_ty(expr_id, SourceType::Unit);
    array_assignment.item_ty = Some(item_type);
    ck.body.insert_array_assignment(expr_id, array_assignment);
}

fn check_expr_assign_method_call(ck: &mut TypeCheck, expr_id: ExprId, sema_expr: &AssignExpr) {
    let lhs_id = sema_expr.lhs;
    let method_call_expr = ck.expr(lhs_id).as_method_call();
    let object_type = check_expr(ck, method_call_expr.object, SourceType::Any);

    let name = method_call_expr.name;

    // Load AST to compute span from object to method name (for error reporting)
    let ast_call = ck.syntax_by_id::<ast::AstMethodCallExpr>(lhs_id);
    let object_span = ast_call.object().span();
    let name_span = ast_call.name().span();
    let error_span = Span::new(object_span.start(), name_span.end() - object_span.start());

    let field_type = check_expr_field_named(ck, lhs_id, error_span, object_type, name);

    let args = create_method_call_arguments(ck, lhs_id, method_call_expr);

    let value_type = check_expr(ck, sema_expr.rhs, SourceType::Any);
    ck.body.set_ty(sema_expr.rhs, value_type.clone());

    let mut array_assignment = ArrayAssignment::new();
    let index_type;
    let item_type;
    let rhs_type;

    if sema_expr.op == ast::AssignOp::Assign {
        (index_type, item_type) = check_index_trait_on_ty(
            ck,
            expr_id,
            &mut array_assignment,
            field_type.clone(),
            false,
        );
        rhs_type = item_type.clone();
    } else {
        let (index_get_index, index_get_item) =
            check_index_trait_on_ty(ck, expr_id, &mut array_assignment, field_type.clone(), true);

        let (index_set_index, index_set_item) = check_index_trait_on_ty(
            ck,
            expr_id,
            &mut array_assignment,
            field_type.clone(),
            false,
        );

        if (index_get_index != index_set_index
            && !index_get_index.is_error()
            && !index_set_index.is_error())
            || (index_get_item != index_set_item
                && !index_get_item.is_error()
                && !index_set_item.is_error())
        {
            ck.report(
                ck.expr_span(expr_id),
                &INDEX_GET_AND_INDEX_SET_DO_NOT_MATCH,
                args![],
            );
        }

        index_type = index_get_index;
        item_type = index_get_item.clone();

        let op_trait_info = check_assign_type(
            ck,
            expr_id,
            sema_expr.op,
            index_get_item,
            value_type.clone(),
        );
        rhs_type = op_trait_info.rhs_type;
    }

    let arg_index_type = args
        .arguments
        .get(0)
        .map(|arg| ck.ty(arg.id()))
        .unwrap_or(ty_error());

    if !index_type.allows(ck.sa, arg_index_type.clone()) && !index_type.is_error() {
        let arg = &args.arguments[0];

        let exp = ck.ty_name(&index_type);
        let got = ck.ty_name(&arg_index_type);

        ck.report(arg.span(), &WRONG_TYPE_FOR_ARGUMENT, args![exp, got]);
    }

    if !rhs_type.allows(ck.sa, value_type.clone()) && !rhs_type.is_error() && !value_type.is_error()
    {
        let exp = ck.ty_name(&rhs_type);
        let got = ck.ty_name(&value_type);

        ck.report(
            ck.expr_span(sema_expr.rhs),
            &WRONG_TYPE_FOR_ARGUMENT,
            args![exp, got],
        );
    }

    for arg in &args.arguments {
        if let Some(name_ident) = arg.name() {
            ck.report(
                name_ident.span(),
                &crate::error::diagnostics::UNEXPECTED_NAMED_ARGUMENT,
                args![],
            );
        }
    }

    if args.arguments.len() > 1 {
        for arg in &args.arguments[1..] {
            ck.report(
                arg.span(),
                &crate::error::diagnostics::SUPERFLUOUS_ARGUMENT,
                args![],
            );
        }
    }

    ck.body.set_ty(expr_id, SourceType::Unit);
    array_assignment.item_ty = Some(item_type);
    ck.body.insert_array_assignment(expr_id, array_assignment);
}

fn check_index_trait_on_ty(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    array_assignment: &mut ArrayAssignment,
    expr_type: SourceType,
    is_get: bool,
) -> (SourceType, SourceType) {
    let trait_id;
    let method_name;

    if is_get {
        trait_id = ck.sa.known.traits.index_get();
        method_name = "get";
    } else {
        trait_id = ck.sa.known.traits.index_set();
        method_name = "set";
    };

    let trait_ty = TraitType::from_trait_id(trait_id);

    let impl_match = find_impl(
        ck.sa,
        ck.element,
        expr_type.clone(),
        &ck.type_param_definition,
        trait_ty.clone(),
    );

    if let Some(impl_match) = impl_match {
        let trait_method_name = ck.sa.interner.intern(method_name);
        let trait_ = ck.sa.trait_(trait_id);
        let trait_method_id = trait_
            .get_method(trait_method_name, false)
            .expect("missing method");
        let index_name = ck.sa.interner.intern("Index");
        let trait_index_type_alias_id =
            trait_.alias_names().get(&index_name).expect("missing Item");
        let item_name = ck.sa.interner.intern("Item");
        let trait_item_type_alias_id = trait_.alias_names().get(&item_name).expect("missing Item");
        let method_id = ck
            .sa
            .impl_(impl_match.id)
            .get_method_for_trait_method_id(trait_method_id)
            .expect("method not found");

        let impl_ = ck.sa.impl_(impl_match.id);

        let impl_index_type_alias_id = impl_
            .trait_alias_map()
            .get(&trait_index_type_alias_id)
            .cloned()
            .expect("missing alias");
        let impl_index_type_alias = ck.sa.alias(impl_index_type_alias_id);

        let impl_item_type_alias_id = impl_
            .trait_alias_map()
            .get(&trait_item_type_alias_id)
            .cloned()
            .expect("missing alias");
        let impl_item_type_alias = ck.sa.alias(impl_item_type_alias_id);

        let call_type = Arc::new(CallType::Expr(
            expr_type.clone(),
            method_id,
            impl_match.bindings.clone(),
        ));
        if is_get {
            array_assignment.index_get = Some(call_type);
        } else {
            array_assignment.index_set = Some(call_type);
        }

        let impl_index_type_alias_ty = impl_index_type_alias.ty();
        let impl_index_type_alias_ty = replace_type(
            ck.sa,
            impl_index_type_alias_ty,
            Some(&impl_match.bindings),
            Some(expr_type.clone()),
        );

        let impl_item_type_alias_ty = impl_item_type_alias.ty();
        let impl_item_type_alias_ty = replace_type(
            ck.sa,
            impl_item_type_alias_ty,
            Some(&impl_match.bindings),
            Some(expr_type),
        );

        (impl_index_type_alias_ty, impl_item_type_alias_ty)
    } else {
        let ty = ck.ty_name(&expr_type);
        let desc = if is_get {
            &INDEX_GET_NOT_IMPLEMENTED
        } else {
            assert_eq!(method_name, "set");
            &INDEX_SET_NOT_IMPLEMENTED
        };
        ck.report(ck.expr_span(expr_id), desc, args![ty]);

        (ty_error(), ty_error())
    }
}

fn check_expr_assign_field(ck: &mut TypeCheck, expr_id: ExprId, sema_expr: &AssignExpr) {
    let lhs_id = sema_expr.lhs;
    let field_sema = ck.expr(lhs_id).as_field();
    let object_type = check_expr(ck, field_sema.lhs, SourceType::Any);

    let Some(ref name) = field_sema.name else {
        ck.body.set_ty(expr_id, ty_error());
        return;
    };

    // Load AST to check if field name is an integer literal (for tuple access)
    let ast_field_expr = ck.syntax_by_id::<ast::AstFieldExpr>(lhs_id);
    let name_token = ast_field_expr.name().unwrap();

    if name_token.syntax_kind() == TokenKind::INT_LITERAL {
        check_expr_assign_unnamed_field(ck, expr_id, sema_expr, lhs_id, object_type);
        return;
    }

    let interned_name = ck.sa.interner.intern(name.as_str());

    if let SourceType::Class(cls_id, class_type_params) = object_type.clone() {
        if let Some((field_index, _)) =
            find_field_in_class(ck.sa, object_type.clone(), interned_name)
        {
            let ident_type = IdentType::Field(object_type.clone(), field_index);
            ck.body.insert_or_replace_ident(lhs_id, ident_type);

            let cls = ck.sa.class(cls_id);
            let field_id = cls.field_id(field_index);
            let field = ck.sa.field(field_id);

            let fty = replace_type(ck.sa, field.ty(), Some(&class_type_params), None);

            if !field.mutable {
                ck.sa
                    .report(ck.file_id, ck.expr_span(expr_id), &LET_REASSIGNED, args!());
            }

            let rhs_type = check_expr(ck, sema_expr.rhs, fty.clone());
            check_assign_type(ck, expr_id, sema_expr.op, fty, rhs_type);
            return;
        }
    }

    if object_type.is_struct() {
        ck.sa
            .report(ck.file_id, ck.expr_span(expr_id), &IMMUTABLE_FIELD, args!());

        // We want to see syntax expressions in the assignment expressions even when we can't
        // find the given field.
        check_expr(ck, sema_expr.rhs, SourceType::Any);

        ck.body.set_ty(expr_id, SourceType::Unit);
        return;
    }

    // We want to see syntax expressions in the assignment expressions even when we can't
    // find the given field.
    check_expr(ck, sema_expr.rhs, SourceType::Any);

    // field not found, report error
    let expr_name = ck.ty_name(&object_type);
    let op_span = ast_field_expr.dot_token().span();
    ck.report(op_span, &UNKNOWN_FIELD, args![name.to_string(), expr_name]);

    ck.body.set_ty(expr_id, SourceType::Unit);
}

fn check_expr_assign_unnamed_field(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    sema_expr: &AssignExpr,
    field_expr_id: ExprId,
    object_type: SourceType,
) {
    let rhs_id = sema_expr.rhs;
    // Load AST for field token (needed for index parsing and span)
    let ast_field_expr = ck.syntax_by_id::<ast::AstFieldExpr>(field_expr_id);
    let field_token = ast_field_expr.name().unwrap();

    let index: usize = field_token.text().parse().unwrap_or(0);

    match object_type.clone() {
        SourceType::Error
        | SourceType::Any
        | SourceType::Unit
        | SourceType::UInt8
        | SourceType::Char
        | SourceType::Int32
        | SourceType::Int64
        | SourceType::Float32
        | SourceType::Float64
        | SourceType::Bool
        | SourceType::Ptr
        | SourceType::This
        | SourceType::TraitObject(..)
        | SourceType::Enum(..)
        | SourceType::TypeParam(..)
        | SourceType::Lambda(..)
        | SourceType::Alias(..)
        | SourceType::Assoc { .. }
        | SourceType::GenericAssoc { .. } => {
            let name = index.to_string();
            let expr_name = ck.ty_name(&object_type);
            ck.report(ck.expr_span(rhs_id), &UNKNOWN_FIELD, args![name, expr_name]);

            check_expr(ck, rhs_id, SourceType::Any);
        }

        SourceType::Struct(struct_id, struct_type_params) => {
            let struct_ = ck.sa.struct_(struct_id);
            if !struct_.field_name_style.is_named() && index < struct_.field_ids().len() {
                let field_id = struct_.field_id(FieldIndex(index));
                let field = ck.sa.field(field_id);
                let ident_type = IdentType::StructField(object_type.clone(), field.index);
                ck.body.insert_or_replace_ident(field_expr_id, ident_type);

                let fty = replace_type(ck.sa, field.ty(), Some(&struct_type_params), None);

                if !struct_field_accessible_from(ck.sa, struct_id, field.index, ck.module_id) {
                    ck.report(field_token.span(), &NOT_ACCESSIBLE, args![]);
                }

                let rhs_type = check_expr(ck, rhs_id, fty.clone());

                if !fty.allows(ck.sa, rhs_type.clone()) && !rhs_type.is_error() {
                    let name = index.to_string();
                    let object_type = ck.ty_name(&object_type);
                    let lhs_type = ck.ty_name(&fty);
                    let rhs_type = ck.ty_name(&rhs_type);

                    ck.report(
                        ck.expr_span(expr_id),
                        &ASSIGN_FIELD,
                        args![name, object_type, lhs_type, rhs_type],
                    );
                }

                ck.report(ck.expr_span(expr_id), &IMMUTABLE_FIELD, args![]);
            } else {
                let name = index.to_string();
                let expr_name = ck.ty_name(&object_type);
                ck.report(field_token.span(), &UNKNOWN_FIELD, args![name, expr_name]);

                check_expr(ck, rhs_id, SourceType::Any);
            }
        }

        SourceType::Tuple(subtypes) => {
            if index < subtypes.len() {
                let ty = subtypes[usize::try_from(index).unwrap()].clone();
                let rhs_type = check_expr(ck, rhs_id, ty.clone());

                if !ty.allows(ck.sa, rhs_type.clone()) && !rhs_type.is_error() {
                    let name = index.to_string();
                    let object_type = ck.ty_name(&object_type);
                    let lhs_type = ck.ty_name(&ty);
                    let rhs_type = ck.ty_name(&rhs_type);

                    ck.report(
                        ck.expr_span(expr_id),
                        &ASSIGN_FIELD,
                        args![name, object_type, lhs_type, rhs_type],
                    );
                }

                ck.report(ck.expr_span(expr_id), &IMMUTABLE_FIELD, args![]);
            } else {
                let name = index.to_string();
                let expr_name = ck.ty_name(&object_type);
                ck.report(field_token.span(), &UNKNOWN_FIELD, args![name, expr_name]);

                check_expr(ck, rhs_id, SourceType::Any);
            }
        }

        SourceType::Class(class_id, class_type_params) => {
            let cls = ck.sa.class(class_id);
            if !cls.field_name_style.is_named() && index < cls.field_ids().len() {
                let field_id = cls.field_id(FieldIndex(index));
                let field = ck.sa.field(field_id);
                let ident_type = IdentType::Field(object_type.clone(), field.index);
                ck.body.insert_or_replace_ident(field_expr_id, ident_type);

                let fty = replace_type(ck.sa, field.ty(), Some(&class_type_params), None);

                if !class_field_accessible_from(ck.sa, class_id, field.index, ck.module_id) {
                    ck.report(field_token.span(), &NOT_ACCESSIBLE, args![]);
                }

                let rhs_type = check_expr(ck, rhs_id, fty.clone());

                if !fty.allows(ck.sa, rhs_type.clone()) && !rhs_type.is_error() {
                    let name = index.to_string();
                    let object_type = ck.ty_name(&object_type);
                    let lhs_type = ck.ty_name(&fty);
                    let rhs_type = ck.ty_name(&rhs_type);

                    ck.report(
                        ck.expr_span(expr_id),
                        &ASSIGN_FIELD,
                        args![name, object_type, lhs_type, rhs_type],
                    );
                }

                ck.body.set_ty(expr_id, fty.clone());
            } else {
                let name = index.to_string();
                let expr_name = ck.ty_name(&object_type);
                ck.report(field_token.span(), &UNKNOWN_FIELD, args![name, expr_name]);

                check_expr(ck, rhs_id, SourceType::Any);
            }
        }
    }
}
