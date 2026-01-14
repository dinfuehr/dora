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
    ASSIGN_FIELD, ASSIGN_TYPE, BIN_OP_TYPE, EXPECTED_MODULE, EXPECTED_SOME_IDENTIFIER,
    IMMUTABLE_FIELD, INDEX_GET_AND_INDEX_SET_DO_NOT_MATCH, INDEX_GET_NOT_IMPLEMENTED,
    INDEX_SET_NOT_IMPLEMENTED, LET_REASSIGNED, LVALUE_EXPECTED, NOT_ACCESSIBLE, UNKNOWN_FIELD,
    UNKNOWN_IDENTIFIER, WRONG_TYPE_FOR_ARGUMENT,
};
use crate::replace_type;
use crate::sema::{
    ArrayAssignment, AssignExpr, CallType, ExprId, FieldIndex, IdentType, TraitDefinitionId,
    find_field_in_class, find_impl, implements_trait,
};
use crate::ty::TraitType;
use crate::typeck::TypeCheck;
use crate::{SourceType, SourceTypeArray, SymbolKind, ty::error as ty_error};

pub(super) fn check_expr_assign(
    ck: &mut TypeCheck,
    expr_id: ExprId,
    e: ast::AstAssignExpr,
    _sema_expr: &AssignExpr,
) -> SourceType {
    let lhs = e.lhs();

    if lhs.is_call_expr() {
        check_expr_assign_call(ck, e.clone());
    } else if lhs.is_method_call_expr() {
        check_expr_assign_method_call(ck, e.clone());
    } else if lhs.is_field_expr() {
        check_expr_assign_field(ck, e.clone());
    } else if lhs.is_path_expr() {
        check_expr_assign_ident(ck, e.clone());
    } else {
        ck.report(e.span(), &LVALUE_EXPECTED, args![]);
    }

    ck.body.set_ty(expr_id, SourceType::Unit);
    SourceType::Unit
}

fn check_expr_assign_ident(ck: &mut TypeCheck, e: ast::AstAssignExpr) {
    let lhs = e.lhs();
    let lhs_ident = lhs.as_path_expr();
    let segments: Vec<_> = lhs_ident.segments().collect();

    // Single segment: simple identifier assignment
    if segments.len() == 1 {
        let sym = segments[0]
            .name()
            .map(|n| ck.symtable.get_string(ck.sa, n.text()))
            .flatten();

        let lhs_type = match sym {
            Some(SymbolKind::Var(var_id)) => {
                if !ck.vars.get_var(var_id).mutable {
                    ck.report(e.span(), &LET_REASSIGNED, args![]);
                }

                // Variable may have to be context-allocated.
                let ident = ck.maybe_allocate_in_context(var_id);
                ck.body.insert_ident(e.lhs().id(), ident);

                ck.vars.get_var(var_id).ty.clone()
            }

            Some(SymbolKind::Global(global_id)) => {
                let global_var = ck.sa.global(global_id);

                if !global_var.mutable {
                    ck.report(e.span(), &LET_REASSIGNED, args![]);
                }

                ck.body
                    .insert_ident(e.lhs().id(), IdentType::Global(global_id));
                global_var.ty()
            }

            None => {
                ck.report(
                    lhs_ident.span(),
                    &UNKNOWN_IDENTIFIER,
                    args![lhs_ident.path_string()],
                );

                return;
            }

            _ => {
                ck.report(lhs_ident.span(), &LVALUE_EXPECTED, args![]);

                return;
            }
        };

        let rhs_type = check_expr(ck, e.rhs(), lhs_type.clone());
        check_assign_type(ck, e, lhs_type, rhs_type);
        return;
    }

    // Multi-segment path: resolve through modules
    let Some(first_name_tok) = segments[0].name() else {
        ck.report(lhs_ident.span(), &EXPECTED_SOME_IDENTIFIER, args![]);
        return;
    };
    let first_name = ck.sa.interner.intern(first_name_tok.text());
    let mut sym = ck.symtable.get(first_name);

    // Resolve intermediate segments
    for segment in &segments[1..segments.len() - 1] {
        match sym {
            Some(SymbolKind::Module(module_id)) => {
                let module = ck.sa.module(module_id);
                let symtable = module.table();
                let Some(name_tok) = segment.name() else {
                    ck.report(lhs_ident.span(), &EXPECTED_SOME_IDENTIFIER, args![]);
                    return;
                };
                let segment_name = ck.sa.interner.intern(name_tok.text());
                sym = symtable.get(segment_name);
            }
            _ => {
                ck.report(lhs_ident.span(), &EXPECTED_MODULE, args![]);
                return;
            }
        }
    }

    // Handle the last segment - must resolve to a global
    let last_segment = &segments[segments.len() - 1];
    let Some(last_name_tok) = last_segment.name() else {
        ck.report(lhs_ident.span(), &EXPECTED_SOME_IDENTIFIER, args![]);
        return;
    };
    let last_name = ck.sa.interner.intern(last_name_tok.text());

    let lhs_type = match sym {
        Some(SymbolKind::Module(module_id)) => {
            let module = ck.sa.module(module_id);
            let symtable = module.table();
            let final_sym = symtable.get(last_name);

            match final_sym {
                Some(SymbolKind::Global(global_id)) => {
                    let global = ck.sa.global(global_id);
                    if !global.mutable {
                        ck.report(e.span(), &LET_REASSIGNED, args![]);
                    }
                    ck.body
                        .insert_ident(e.lhs().id(), IdentType::Global(global_id));
                    global.ty()
                }
                _ => {
                    ck.report(lhs_ident.span(), &LVALUE_EXPECTED, args![]);
                    return;
                }
            }
        }
        _ => {
            ck.report(lhs_ident.span(), &LVALUE_EXPECTED, args![]);
            return;
        }
    };

    let rhs_type = check_expr(ck, e.rhs(), lhs_type.clone());
    check_assign_type(ck, e, lhs_type, rhs_type);
}

fn check_assign_type(
    ck: &mut TypeCheck,
    node: ast::AstAssignExpr,
    lhs_type: SourceType,
    rhs_type: SourceType,
) -> OpTraitInfo {
    ck.body.set_ty(node.id(), SourceType::Unit);

    match node.op() {
        ast::AssignOp::AddAssign => check_expr_assign_trait(
            ck,
            node.clone(),
            node.op(),
            ck.sa.known.traits.add(),
            "add",
            lhs_type,
            rhs_type,
        ),

        ast::AssignOp::SubAssign => check_expr_assign_trait(
            ck,
            node.clone(),
            node.op(),
            ck.sa.known.traits.sub(),
            "sub",
            lhs_type,
            rhs_type,
        ),

        ast::AssignOp::MulAssign => check_expr_assign_trait(
            ck,
            node.clone(),
            node.op(),
            ck.sa.known.traits.mul(),
            "mul",
            lhs_type,
            rhs_type,
        ),

        ast::AssignOp::DivAssign => check_expr_assign_trait(
            ck,
            node.clone(),
            node.op(),
            ck.sa.known.traits.div(),
            "div",
            lhs_type,
            rhs_type,
        ),

        ast::AssignOp::ModAssign => check_expr_assign_trait(
            ck,
            node.clone(),
            node.op(),
            ck.sa.known.traits.mod_(),
            "modulo",
            lhs_type,
            rhs_type,
        ),

        ast::AssignOp::BitOrAssign => check_expr_assign_trait(
            ck,
            node.clone(),
            node.op(),
            ck.sa.known.traits.bit_or(),
            "bitor",
            lhs_type,
            rhs_type,
        ),

        ast::AssignOp::BitAndAssign => check_expr_assign_trait(
            ck,
            node.clone(),
            node.op(),
            ck.sa.known.traits.bit_and(),
            "bitand",
            lhs_type,
            rhs_type,
        ),

        ast::AssignOp::BitXorAssign => check_expr_assign_trait(
            ck,
            node.clone(),
            node.op(),
            ck.sa.known.traits.bit_xor(),
            "bitxor",
            lhs_type,
            rhs_type,
        ),

        ast::AssignOp::ShiftLAssign => check_expr_assign_trait(
            ck,
            node.clone(),
            node.op(),
            ck.sa.known.traits.shl(),
            "shl",
            lhs_type,
            rhs_type,
        ),

        ast::AssignOp::LogicalShiftRAssign => check_expr_assign_trait(
            ck,
            node.clone(),
            node.op(),
            ck.sa.known.traits.shr(),
            "shr",
            lhs_type,
            rhs_type,
        ),

        ast::AssignOp::ArithShiftRAssign => check_expr_assign_trait(
            ck,
            node.clone(),
            node.op(),
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

                ck.report(node.span(), &ASSIGN_TYPE, args![lhs_type, rhs_type]);
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
    node: ast::AstAssignExpr,
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
                .insert_or_replace_call_type(node.id(), Arc::new(call_type));

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
                    node.span(),
                    &BIN_OP_TYPE,
                    args![op.as_str().to_string(), lhs_type, rhs_type],
                );
            }

            let return_type = method.return_type();
            ck.body.set_ty(node.id(), return_type.clone());

            OpTraitInfo {
                rhs_type,
                return_type,
            }
        } else {
            ck.body.set_ty(node.id(), ty_error());
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
            .insert_or_replace_call_type(node.id(), Arc::new(call_type));

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
                node.span(),
                &BIN_OP_TYPE,
                args![op.as_str().to_string(), lhs_type, rhs_type],
            );
        }

        let return_type = method.return_type();
        ck.body.set_ty(node.id(), return_type.clone());

        OpTraitInfo {
            rhs_type,
            return_type,
        }
    } else {
        if !lhs_type.is_error() && !rhs_type.is_error() {
            let lhs_type_name = ck.ty_name(&lhs_type);
            let rhs_type = ck.ty_name(&rhs_type);
            ck.report(
                node.span(),
                &BIN_OP_TYPE,
                args![op.as_str().to_string(), lhs_type_name, rhs_type],
            );
        }
        ck.body.set_ty(node.id(), ty_error());
        OpTraitInfo {
            rhs_type: ty_error(),
            return_type: ty_error(),
        }
    }
}

fn check_expr_assign_call(ck: &mut TypeCheck, e: ast::AstAssignExpr) {
    let call = e.lhs().as_call_expr();
    let object_type = check_expr(ck, call.callee(), SourceType::Any);

    let args = create_call_arguments(ck, &call);

    let value_type = check_expr(ck, e.rhs(), SourceType::Any);
    ck.body.set_ty(e.rhs().id(), value_type.clone());

    let mut array_assignment = ArrayAssignment::new();
    let index_type;
    let item_type;
    let rhs_type;

    if e.op() == ast::AssignOp::Assign {
        (index_type, item_type) =
            check_index_trait_on_ty(ck, &e, &mut array_assignment, object_type.clone(), false);
        rhs_type = item_type.clone();
    } else {
        let (index_get_index, index_get_item) =
            check_index_trait_on_ty(ck, &e, &mut array_assignment, object_type.clone(), true);

        let (index_set_index, index_set_item) =
            check_index_trait_on_ty(ck, &e, &mut array_assignment, object_type.clone(), false);

        if (index_get_index != index_set_index
            && !index_get_index.is_error()
            && !index_set_index.is_error())
            || (index_get_item != index_set_item
                && !index_get_item.is_error()
                && !index_set_item.is_error())
        {
            ck.report(
                call.callee().span(),
                &INDEX_GET_AND_INDEX_SET_DO_NOT_MATCH,
                args![],
            );
        }

        index_type = index_get_index;
        item_type = index_get_item.clone();

        let op_trait_info = check_assign_type(ck, e.clone(), index_get_item, value_type.clone());
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

        ck.report(e.rhs().span(), &WRONG_TYPE_FOR_ARGUMENT, args![exp, got]);
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

    ck.body.set_ty(e.id(), SourceType::Unit);
    array_assignment.item_ty = Some(item_type);
    ck.body.insert_array_assignment(e.id(), array_assignment);
}

fn check_expr_assign_method_call(ck: &mut TypeCheck, e: ast::AstAssignExpr) {
    let call = e.lhs().as_method_call_expr();
    let object_type = check_expr(ck, call.object(), SourceType::Any);

    let name_token = call.name();
    let name = ck.sa.interner.intern(name_token.text());

    // Compute span from object to method name (for error reporting)
    let object_span = call.object().span();
    let name_span = name_token.span();
    let error_span = Span::new(object_span.start(), name_span.end() - object_span.start());

    let field_type = check_expr_field_named(ck, call.clone().into(), error_span, object_type, name);

    let args = create_method_call_arguments(ck, &call);

    let value_type = check_expr(ck, e.rhs(), SourceType::Any);
    ck.body.set_ty(e.rhs().id(), value_type.clone());

    let mut array_assignment = ArrayAssignment::new();
    let index_type;
    let item_type;
    let rhs_type;

    if e.op() == ast::AssignOp::Assign {
        (index_type, item_type) =
            check_index_trait_on_ty(ck, &e, &mut array_assignment, field_type.clone(), false);
        rhs_type = item_type.clone();
    } else {
        let (index_get_index, index_get_item) =
            check_index_trait_on_ty(ck, &e, &mut array_assignment, field_type.clone(), true);

        let (index_set_index, index_set_item) =
            check_index_trait_on_ty(ck, &e, &mut array_assignment, field_type.clone(), false);

        if (index_get_index != index_set_index
            && !index_get_index.is_error()
            && !index_set_index.is_error())
            || (index_get_item != index_set_item
                && !index_get_item.is_error()
                && !index_set_item.is_error())
        {
            ck.report(e.span(), &INDEX_GET_AND_INDEX_SET_DO_NOT_MATCH, args![]);
        }

        index_type = index_get_index;
        item_type = index_get_item.clone();

        let op_trait_info = check_assign_type(ck, e.clone(), index_get_item, value_type.clone());
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

        ck.report(e.rhs().span(), &WRONG_TYPE_FOR_ARGUMENT, args![exp, got]);
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

    ck.body.set_ty(e.id(), SourceType::Unit);
    array_assignment.item_ty = Some(item_type);
    ck.body.insert_array_assignment(e.id(), array_assignment);
}

fn check_index_trait_on_ty(
    ck: &mut TypeCheck,
    e: &ast::AstAssignExpr,
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
        ck.report(e.span(), desc, args![ty]);

        (ty_error(), ty_error())
    }
}

fn check_expr_assign_field(ck: &mut TypeCheck, e: ast::AstAssignExpr) {
    let field_expr = e.lhs().as_field_expr();
    let object_type = check_expr(ck, field_expr.lhs(), SourceType::Any);

    let Some(name_token) = field_expr.name() else {
        ck.body.set_ty(e.id(), ty_error());
        return;
    };

    if name_token.syntax_kind() == TokenKind::INT_LITERAL {
        check_expr_assign_unnamed_field(ck, e, field_expr, object_type);
        return;
    }

    let name = name_token.text().to_string();

    let interned_name = ck.sa.interner.intern(&name);

    if let SourceType::Class(cls_id, class_type_params) = object_type.clone() {
        if let Some((field_index, _)) =
            find_field_in_class(ck.sa, object_type.clone(), interned_name)
        {
            let ident_type = IdentType::Field(object_type.clone(), field_index);
            ck.body.insert_or_replace_ident(e.lhs().id(), ident_type);

            let cls = ck.sa.class(cls_id);
            let field_id = cls.field_id(field_index);
            let field = ck.sa.field(field_id);

            let fty = replace_type(ck.sa, field.ty(), Some(&class_type_params), None);

            if !field.mutable {
                ck.sa.report(ck.file_id, e.span(), &LET_REASSIGNED, args!());
            }

            let rhs_type = check_expr(ck, e.rhs(), fty.clone());
            check_assign_type(ck, e, fty, rhs_type);
            return;
        }
    }

    if object_type.is_struct() {
        ck.sa
            .report(ck.file_id, e.span(), &IMMUTABLE_FIELD, args!());

        // We want to see syntax expressions in the assignment expressions even when we can't
        // find the given field.
        check_expr(ck, e.rhs(), SourceType::Any);

        ck.body.set_ty(e.id(), SourceType::Unit);
        return;
    }

    // We want to see syntax expressions in the assignment expressions even when we can't
    // find the given field.
    check_expr(ck, e.rhs(), SourceType::Any);

    // field not found, report error
    let expr_name = ck.ty_name(&object_type);
    let op_span = field_expr.dot_token().span();
    ck.report(op_span, &UNKNOWN_FIELD, args![name, expr_name]);

    ck.body.set_ty(e.id(), SourceType::Unit);
}

fn check_expr_assign_unnamed_field(
    ck: &mut TypeCheck,
    expr: ast::AstAssignExpr,
    field_expr: ast::AstFieldExpr,
    object_type: SourceType,
) {
    let rhs_expr = expr.rhs();
    let field_token = field_expr.name().unwrap();

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
            ck.report(rhs_expr.span(), &UNKNOWN_FIELD, args![name, expr_name]);

            check_expr(ck, rhs_expr.clone(), SourceType::Any);
        }

        SourceType::Struct(struct_id, struct_type_params) => {
            let struct_ = ck.sa.struct_(struct_id);
            if !struct_.field_name_style.is_named() && index < struct_.field_ids().len() {
                let field_id = struct_.field_id(FieldIndex(index));
                let field = ck.sa.field(field_id);
                let ident_type = IdentType::StructField(object_type.clone(), field.index);
                ck.body.insert_or_replace_ident(field_expr.id(), ident_type);

                let fty = replace_type(ck.sa, field.ty(), Some(&struct_type_params), None);

                if !struct_field_accessible_from(ck.sa, struct_id, field.index, ck.module_id) {
                    ck.report(field_token.span(), &NOT_ACCESSIBLE, args![]);
                }

                let rhs_type = check_expr(ck, rhs_expr.clone(), fty.clone());

                if !fty.allows(ck.sa, rhs_type.clone()) && !rhs_type.is_error() {
                    let name = index.to_string();
                    let object_type = ck.ty_name(&object_type);
                    let lhs_type = ck.ty_name(&fty);
                    let rhs_type = ck.ty_name(&rhs_type);

                    ck.report(
                        expr.span(),
                        &ASSIGN_FIELD,
                        args![name, object_type, lhs_type, rhs_type],
                    );
                }

                ck.report(expr.span(), &IMMUTABLE_FIELD, args![]);
            } else {
                let name = index.to_string();
                let expr_name = ck.ty_name(&object_type);
                ck.report(field_token.span(), &UNKNOWN_FIELD, args![name, expr_name]);

                check_expr(ck, rhs_expr.clone(), SourceType::Any);
            }
        }

        SourceType::Tuple(subtypes) => {
            if index < subtypes.len() {
                let ty = subtypes[usize::try_from(index).unwrap()].clone();
                let rhs_type = check_expr(ck, rhs_expr.clone(), ty.clone());

                if !ty.allows(ck.sa, rhs_type.clone()) && !rhs_type.is_error() {
                    let name = index.to_string();
                    let object_type = ck.ty_name(&object_type);
                    let lhs_type = ck.ty_name(&ty);
                    let rhs_type = ck.ty_name(&rhs_type);

                    ck.report(
                        expr.span(),
                        &ASSIGN_FIELD,
                        args![name, object_type, lhs_type, rhs_type],
                    );
                }

                ck.report(expr.span(), &IMMUTABLE_FIELD, args![]);
            } else {
                let name = index.to_string();
                let expr_name = ck.ty_name(&object_type);
                ck.report(field_token.span(), &UNKNOWN_FIELD, args![name, expr_name]);

                check_expr(ck, rhs_expr.clone(), SourceType::Any);
            }
        }

        SourceType::Class(class_id, class_type_params) => {
            let cls = ck.sa.class(class_id);
            if !cls.field_name_style.is_named() && index < cls.field_ids().len() {
                let field_id = cls.field_id(FieldIndex(index));
                let field = ck.sa.field(field_id);
                let ident_type = IdentType::Field(object_type.clone(), field.index);
                ck.body.insert_or_replace_ident(field_expr.id(), ident_type);

                let fty = replace_type(ck.sa, field.ty(), Some(&class_type_params), None);

                if !class_field_accessible_from(ck.sa, class_id, field.index, ck.module_id) {
                    ck.report(field_token.span(), &NOT_ACCESSIBLE, args![]);
                }

                let rhs_type = check_expr(ck, rhs_expr.clone(), fty.clone());

                if !fty.allows(ck.sa, rhs_type.clone()) && !rhs_type.is_error() {
                    let name = index.to_string();
                    let object_type = ck.ty_name(&object_type);
                    let lhs_type = ck.ty_name(&fty);
                    let rhs_type = ck.ty_name(&rhs_type);

                    ck.report(
                        expr.span(),
                        &ASSIGN_FIELD,
                        args![name, object_type, lhs_type, rhs_type],
                    );
                }

                ck.body.set_ty(expr.id(), fty.clone());
            } else {
                let name = index.to_string();
                let expr_name = ck.ty_name(&object_type);
                ck.report(field_token.span(), &UNKNOWN_FIELD, args![name, expr_name]);

                check_expr(ck, rhs_expr, SourceType::Any);
            }
        }
    }
}
