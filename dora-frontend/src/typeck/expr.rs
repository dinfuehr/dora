use std::sync::Arc;

use dora_parser::ast::{Ast, AstId};
use dora_parser::{Span, ast};

use crate::access::{
    class_field_accessible_from, const_accessible_from, enum_accessible_from,
    global_accessible_from, module_accessible_from, struct_field_accessible_from,
};
use crate::element_collector::Annotations;
use crate::error::msg::ErrorMessage;
use crate::interner::Name;
use crate::sema::{
    AnalysisData, ArrayAssignment, CallType, ConstValue, EnumDefinitionId, FctDefinition,
    FctParent, FieldIndex, IdentType, Intrinsic, LazyLambdaCreationData, LazyLambdaId,
    ModuleDefinitionId, NestedVarId, Param, Params, Sema, SourceFileId, TraitDefinitionId,
    create_tuple, find_field_in_class, find_impl, implements_trait,
};
use crate::ty::TraitType;
use crate::typeck::{
    TypeCheck, check_expr_break_and_continue, check_expr_call, check_expr_for, check_expr_if,
    check_expr_match, check_expr_return, check_expr_while, check_lit_char, check_lit_float,
    check_lit_int, check_lit_str, check_pattern, check_stmt, check_type_params,
    create_call_arguments, is_simple_enum,
};
use crate::{CallSpecializationData, specialize_ty_for_call, specialize_type};
use crate::{SourceType, SourceTypeArray, SymbolKind, replace_type, ty::error as ty_error};

pub(super) fn check_expr(
    ck: &mut TypeCheck,
    id: ast::AstId,
    expected_ty: SourceType,
) -> SourceType {
    let ast_expr = ck.node(id);
    match *ast_expr {
        Ast::LitChar(ref expr) => check_expr_lit_char(ck, id, expr, expected_ty),
        Ast::LitInt(..) => check_expr_lit_int(ck, id, false, expected_ty),
        Ast::LitFloat(ref expr) => check_expr_lit_float(ck, id, expr, false, expected_ty),
        Ast::LitStr(ref expr) => check_expr_lit_str(ck, id, expr, expected_ty),
        Ast::Template(ref expr) => check_expr_template(ck, id, expr, expected_ty),
        Ast::LitBool(ref expr) => check_expr_lit_bool(ck, id, expr, expected_ty),
        Ast::Ident(ref expr) => check_expr_ident(ck, id, expr, expected_ty),
        Ast::Un(ref expr) => check_expr_un(ck, id, expr, expected_ty),
        Ast::Bin(ref expr) => check_expr_bin(ck, id, expr, expected_ty),
        Ast::Call(ref expr) => check_expr_call(ck, id, expr, expected_ty),
        Ast::TypedExpr(ref expr) => check_expr_type_param(ck, id, expr, expected_ty),
        Ast::Path(ref expr) => check_expr_path(ck, id, expr, expected_ty),
        Ast::Dot(ref expr) => check_expr_dot(ck, id, expr, expected_ty),
        Ast::This(ref expr) => check_expr_this(ck, id, expr, expected_ty),
        Ast::Conv(ref expr) => check_expr_conv(ck, id, expr, expected_ty),
        Ast::Is(ref expr) => check_expr_is(ck, id, expr, expected_ty),
        Ast::Lambda(ref expr) => check_expr_lambda(ck, id, expr, expected_ty),
        Ast::Block(ref expr) => check_expr_block(ck, id, expr, expected_ty),
        Ast::If(ref expr) => check_expr_if(ck, id, expr, expected_ty),
        Ast::Tuple(ref expr) => check_expr_tuple(ck, id, expr, expected_ty),
        Ast::Paren(ref expr) => check_expr_paren(ck, id, expr, expected_ty),
        Ast::Match(ref expr) => check_expr_match(ck, id, expr, expected_ty),
        Ast::For(ref expr) => check_expr_for(ck, id, expr, expected_ty),
        Ast::While(ref expr) => check_expr_while(ck, id, expr, expected_ty),
        Ast::Return(ref expr) => check_expr_return(ck, id, expr, expected_ty),
        Ast::Break(..) | Ast::Continue(..) => check_expr_break_and_continue(ck, id, expected_ty),
        Ast::Error { .. } => ty_error(),
        _ => unreachable!(),
    }
}

pub(super) fn check_expr_block(
    ck: &mut TypeCheck,
    node_id: ast::AstId,
    node: &ast::Block,
    _expected_ty: SourceType,
) -> SourceType {
    ck.symtable.push_level();

    for &stmt_id in &node.stmts {
        check_stmt(ck, stmt_id);
    }

    let ty = if let Some(expr) = node.expr {
        check_expr(ck, expr, SourceType::Any)
    } else {
        SourceType::Unit
    };

    ck.analysis.set_ty(node_id, ty.clone());
    ck.symtable.pop_level();

    ty
}

pub(super) fn check_expr_tuple(
    ck: &mut TypeCheck,
    node_id: ast::AstId,
    node: &ast::Tuple,
    _expected_ty: SourceType,
) -> SourceType {
    let mut subtypes = Vec::new();

    if node.values.is_empty() {
        ck.analysis.set_ty(node_id, SourceType::Unit);
        return SourceType::Unit;
    }

    for &value in &node.values {
        let subtype = check_expr(ck, value, SourceType::Any);
        subtypes.push(subtype);
    }

    let ty = create_tuple(ck.sa, subtypes);
    ck.analysis.set_ty(node_id, ty.clone());

    ty
}

pub(super) fn check_expr_paren(
    ck: &mut TypeCheck,
    node_id: ast::AstId,
    node: &ast::Paren,
    _expected_ty: SourceType,
) -> SourceType {
    let ty = check_expr(ck, node.expr, SourceType::Any);
    ck.analysis.set_ty(node_id, ty.clone());

    ty
}

pub(super) fn check_expr_ident(
    ck: &mut TypeCheck,
    expr_ast_id: ast::AstId,
    e: &ast::Ident,
    expected_ty: SourceType,
) -> SourceType {
    let interned_name: Name = ck.sa.interner.intern(&e.name);
    let sym = ck.symtable.get(interned_name);

    match sym {
        Some(SymbolKind::Var(var_id)) => {
            let ty = ck.vars.get_var(var_id).ty.clone();
            ck.analysis.set_ty(expr_ast_id, ty.clone());

            // Variable may have to be context-allocated.
            let ident = ck.maybe_allocate_in_context(var_id);
            ck.analysis.map_idents.insert(expr_ast_id, ident);

            ty
        }

        Some(SymbolKind::Global(globalid)) => {
            let global_var = ck.sa.global(globalid);
            let ty = global_var.ty();
            ck.analysis.set_ty(expr_ast_id, ty.clone());

            ck.analysis
                .map_idents
                .insert(expr_ast_id, IdentType::Global(globalid));

            ty
        }

        Some(SymbolKind::Const(const_id)) => {
            let const_ = ck.sa.const_(const_id);
            ck.analysis.set_ty(expr_ast_id, const_.ty());

            ck.analysis
                .map_idents
                .insert(expr_ast_id, IdentType::Const(const_id));

            const_.ty()
        }

        Some(SymbolKind::EnumVariant(enum_id, variant_idx)) => check_enum_variant_without_args_id(
            ck,
            expr_ast_id,
            e.span,
            expected_ty,
            enum_id,
            SourceTypeArray::empty(),
            variant_idx,
        ),

        None => {
            ck.sa.report(
                ck.file_id,
                e.span,
                ErrorMessage::UnknownIdentifier(e.name.clone()),
            );
            ty_error()
        }

        _ => {
            ck.sa
                .report(ck.file_id, e.span, ErrorMessage::ValueExpected);
            ty_error()
        }
    }
}

pub(super) fn check_expr_assign(ck: &mut TypeCheck, expr_ast_id: ast::AstId, e: &ast::Bin) {
    let lhs = ck.node(e.lhs);

    if lhs.is_call() {
        check_expr_assign_call(ck, expr_ast_id, e);
    } else if lhs.is_dot() {
        check_expr_assign_field(ck, expr_ast_id, e);
    } else if lhs.is_ident() {
        check_expr_assign_ident(ck, expr_ast_id, e);
    } else if lhs.is_path() {
        check_expr_assign_path(ck, expr_ast_id, e);
    } else {
        ck.sa
            .report(ck.file_id, e.span, ErrorMessage::LvalueExpected);
    }

    ck.analysis.set_ty(expr_ast_id, SourceType::Unit);
}

fn check_expr_assign_path(ck: &mut TypeCheck, expr_ast_id: ast::AstId, e: &ast::Bin) {
    let lhs_type = match read_path_expr(ck, e.lhs) {
        Ok(Some(SymbolKind::Global(global_id))) => {
            let global = ck.sa.global(global_id);
            ck.analysis
                .map_idents
                .insert(e.lhs, IdentType::Global(global_id));
            global.ty()
        }

        Ok(_) => {
            let msg = ErrorMessage::LvalueExpected;
            ck.sa.report(ck.file_id, ck.span(e.lhs), msg);
            ty_error()
        }

        Err(()) => ty_error(),
    };

    let rhs_type = check_expr(ck, e.rhs, lhs_type.clone());
    check_assign_type(ck, expr_ast_id, e, lhs_type, rhs_type);
}

fn check_expr_assign_ident(ck: &mut TypeCheck, expr_ast_id: ast::AstId, e: &ast::Bin) {
    let lhs = ck.node(e.lhs);
    let lhs_ident = lhs.as_ident();
    let sym = ck.symtable.get_string(ck.sa, &lhs_ident.name);

    let lhs_type = match sym {
        Some(SymbolKind::Var(var_id)) => {
            if !ck.vars.get_var(var_id).mutable {
                ck.sa
                    .report(ck.file_id, e.span, ErrorMessage::LetReassigned);
            }

            // Variable may have to be context-allocated.
            let ident = ck.maybe_allocate_in_context(var_id);
            ck.analysis.map_idents.insert(e.lhs, ident);

            ck.vars.get_var(var_id).ty.clone()
        }

        Some(SymbolKind::Global(global_id)) => {
            let global_var = ck.sa.global(global_id);

            if !global_var.mutable {
                ck.sa
                    .report(ck.file_id, e.span, ErrorMessage::LetReassigned);
            }

            ck.analysis
                .map_idents
                .insert(e.lhs, IdentType::Global(global_id));
            global_var.ty()
        }

        None => {
            ck.sa.report(
                ck.file_id,
                lhs_ident.span,
                ErrorMessage::UnknownIdentifier(lhs_ident.name.clone()),
            );

            return;
        }

        _ => {
            ck.sa
                .report(ck.file_id, lhs_ident.span, ErrorMessage::LvalueExpected);

            return;
        }
    };

    let rhs_type = check_expr(ck, e.rhs, lhs_type.clone());
    check_assign_type(ck, expr_ast_id, e, lhs_type, rhs_type);
}

fn check_assign_type(
    ck: &mut TypeCheck,
    expr_ast_id: ast::AstId,
    e: &ast::Bin,
    lhs_type: SourceType,
    rhs_type: SourceType,
) -> OpTraitInfo {
    ck.analysis.set_ty(expr_ast_id, SourceType::Unit);

    match e.op {
        ast::BinOp::AddAssign => check_expr_bin_trait(
            ck,
            expr_ast_id,
            e,
            e.op,
            ck.sa.known.traits.add(),
            "add",
            lhs_type,
            rhs_type,
        ),

        ast::BinOp::SubAssign => check_expr_bin_trait(
            ck,
            expr_ast_id,
            e,
            e.op,
            ck.sa.known.traits.sub(),
            "sub",
            lhs_type,
            rhs_type,
        ),

        ast::BinOp::MulAssign => check_expr_bin_trait(
            ck,
            expr_ast_id,
            e,
            e.op,
            ck.sa.known.traits.mul(),
            "mul",
            lhs_type,
            rhs_type,
        ),

        ast::BinOp::DivAssign => check_expr_bin_trait(
            ck,
            expr_ast_id,
            e,
            e.op,
            ck.sa.known.traits.div(),
            "div",
            lhs_type,
            rhs_type,
        ),

        ast::BinOp::ModAssign => check_expr_bin_trait(
            ck,
            expr_ast_id,
            e,
            e.op,
            ck.sa.known.traits.mod_(),
            "modulo",
            lhs_type,
            rhs_type,
        ),

        ast::BinOp::BitOrAssign => check_expr_bin_trait(
            ck,
            expr_ast_id,
            e,
            e.op,
            ck.sa.known.traits.bit_or(),
            "bitor",
            lhs_type,
            rhs_type,
        ),

        ast::BinOp::BitAndAssign => check_expr_bin_trait(
            ck,
            expr_ast_id,
            e,
            e.op,
            ck.sa.known.traits.bit_and(),
            "bitand",
            lhs_type,
            rhs_type,
        ),

        ast::BinOp::BitXorAssign => check_expr_bin_trait(
            ck,
            expr_ast_id,
            e,
            e.op,
            ck.sa.known.traits.bit_xor(),
            "bitxor",
            lhs_type,
            rhs_type,
        ),

        ast::BinOp::ShiftLAssign => check_expr_bin_trait(
            ck,
            expr_ast_id,
            e,
            e.op,
            ck.sa.known.traits.shl(),
            "shl",
            lhs_type,
            rhs_type,
        ),

        ast::BinOp::LogicalShiftRAssign => check_expr_bin_trait(
            ck,
            expr_ast_id,
            e,
            e.op,
            ck.sa.known.traits.shr(),
            "shr",
            lhs_type,
            rhs_type,
        ),

        ast::BinOp::ArithShiftRAssign => check_expr_bin_trait(
            ck,
            expr_ast_id,
            e,
            e.op,
            ck.sa.known.traits.sar(),
            "sar",
            lhs_type,
            rhs_type,
        ),

        ast::BinOp::Assign => {
            if !lhs_type.is_error()
                && !rhs_type.is_error()
                && !lhs_type.allows(ck.sa, rhs_type.clone())
            {
                let lhs_type = ck.ty_name(&lhs_type);
                let rhs_type = ck.ty_name(&rhs_type);

                let msg = ErrorMessage::AssignType(lhs_type, rhs_type);
                ck.sa.report(ck.file_id, e.span, msg);
            }

            OpTraitInfo {
                rhs_type: ty_error(),
                return_type: ty_error(),
            }
        }

        _ => unreachable!(),
    }
}

fn check_expr_assign_call(ck: &mut TypeCheck, expr_ast_id: ast::AstId, e: &ast::Bin) {
    let call = ck.node(e.lhs).as_call();
    let object_type = check_expr(ck, call.callee, SourceType::Any);

    let args = create_call_arguments(ck, call);

    let value_type = check_expr(ck, e.rhs, SourceType::Any);
    ck.analysis.set_ty(e.rhs, value_type.clone());

    let mut array_assignment = ArrayAssignment::new();
    let index_type;
    let item_type;
    let rhs_type;

    if e.op == ast::BinOp::Assign {
        (index_type, item_type) =
            check_index_trait_on_ty(ck, e, &mut array_assignment, object_type.clone(), false);
        rhs_type = item_type.clone();
    } else {
        let (index_get_index, index_get_item) =
            check_index_trait_on_ty(ck, e, &mut array_assignment, object_type.clone(), true);

        let (index_set_index, index_set_item) =
            check_index_trait_on_ty(ck, e, &mut array_assignment, object_type.clone(), false);

        if (index_get_index != index_set_index
            && !index_get_index.is_error()
            && !index_set_index.is_error())
            || (index_get_item != index_set_item
                && !index_get_item.is_error()
                && !index_set_item.is_error())
        {
            ck.sa.report(
                ck.file_id,
                ck.span(call.callee),
                ErrorMessage::IndexGetAndIndexSetDoNotMatch,
            );
        }

        index_type = index_get_index;
        item_type = index_get_item.clone();

        let op_trait_info =
            check_assign_type(ck, expr_ast_id, e, index_get_item, value_type.clone());
        rhs_type = op_trait_info.rhs_type;
    }

    let arg_index_type = args
        .arguments
        .get(0)
        .map(|&arg_id| ck.ty(arg_id))
        .unwrap_or(ty_error());

    if !index_type.allows(ck.sa, arg_index_type.clone()) && !index_type.is_error() {
        let arg_id = args.arguments[0];

        let exp = ck.ty_name(&index_type);
        let got = ck.ty_name(&arg_index_type);

        ck.sa.report(
            ck.file_id,
            ck.span(arg_id),
            ErrorMessage::WrongTypeForArgument(exp, got),
        );
    }

    if !rhs_type.allows(ck.sa, value_type.clone()) && !rhs_type.is_error() && !value_type.is_error()
    {
        let exp = ck.ty_name(&rhs_type);
        let got = ck.ty_name(&value_type);

        ck.sa.report(
            ck.file_id,
            ck.span(e.rhs),
            ErrorMessage::WrongTypeForArgument(exp, got),
        );
    }

    for &arg_id in &args.arguments {
        let arg = ck.node(arg_id).as_argument();
        if let Some(name_id) = arg.name {
            ck.sa.report(
                ck.file_id,
                ck.span(name_id),
                ErrorMessage::UnexpectedNamedArgument,
            );
        }
    }

    if args.arguments.len() > 1 {
        for &arg_id in &args.arguments[1..] {
            ck.sa.report(
                ck.file_id,
                ck.span(arg_id),
                ErrorMessage::SuperfluousArgument,
            );
        }
    }

    ck.analysis.set_ty(expr_ast_id, SourceType::Unit);
    array_assignment.item_ty = Some(item_type);
    ck.analysis
        .map_array_assignments
        .insert(expr_ast_id, array_assignment);
}

fn check_index_trait_on_ty(
    ck: &mut TypeCheck,
    e: &ast::Bin,
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

    let call = ck.node(e.lhs).as_call();

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
        let msg = if is_get {
            ErrorMessage::IndexGetNotImplemented(ty)
        } else {
            assert_eq!(method_name, "set");
            ErrorMessage::IndexSetNotImplemented(ty)
        };
        ck.sa.report(ck.file_id, ck.span(call.callee), msg);

        (ty_error(), ty_error())
    }
}

fn check_expr_assign_field(ck: &mut TypeCheck, expr_ast_id: ast::AstId, e: &ast::Bin) {
    let dot_expr = ck.node(e.lhs).as_dot();
    let object_type = check_expr(ck, dot_expr.lhs, SourceType::Any);

    let rhs = ck.node(dot_expr.rhs);

    if rhs.is_lit_int() {
        check_expr_assign_unnamed_field(ck, expr_ast_id, e, e.lhs, dot_expr, object_type);
        return;
    }

    let name = match rhs.to_ident() {
        Some(ident) => ident.name.clone(),

        None => {
            let msg = ErrorMessage::NameExpected;
            ck.sa.report(ck.file_id, e.span, msg);

            ck.analysis.set_ty(expr_ast_id, ty_error());
            return;
        }
    };

    let interned_name = ck.sa.interner.intern(&name);

    if let SourceType::Class(cls_id, class_type_params) = object_type.clone() {
        if let Some((field_index, _)) =
            find_field_in_class(ck.sa, object_type.clone(), interned_name)
        {
            let ident_type = IdentType::Field(object_type.clone(), field_index);
            ck.analysis.map_idents.insert_or_replace(e.lhs, ident_type);

            let cls = ck.sa.class(cls_id);
            let field_id = cls.field_id(field_index);
            let field = ck.sa.field(field_id);

            let fty = replace_type(ck.sa, field.ty(), Some(&class_type_params), None);

            if !field.mutable {
                ck.sa
                    .report(ck.file_id, e.span, ErrorMessage::LetReassigned);
            }

            let rhs_type = check_expr(ck, e.rhs, fty.clone());
            check_assign_type(ck, expr_ast_id, e, fty, rhs_type);
            return;
        }
    }

    if object_type.is_struct() {
        ck.sa
            .report(ck.file_id, e.span, ErrorMessage::ImmutableField);

        // We want to see syntax expressions in the assignment expressions even when we can't
        // find the given field.
        check_expr(ck, e.rhs, SourceType::Any);

        ck.analysis.set_ty(expr_ast_id, SourceType::Unit);
        return;
    }

    // We want to see syntax expressions in the assignment expressions even when we can't
    // find the given field.
    check_expr(ck, e.rhs, SourceType::Any);

    // field not found, report error
    let expr_name = ck.ty_name(&object_type);
    let msg = ErrorMessage::UnknownField(name, expr_name);
    ck.sa.report(ck.file_id, dot_expr.op_span, msg);

    ck.analysis.set_ty(expr_ast_id, SourceType::Unit);
}

fn check_expr_assign_unnamed_field(
    ck: &mut TypeCheck,
    expr_id: ast::AstId,
    e: &ast::Bin,
    dot_expr_id: ast::AstId,
    dot_expr: &ast::Dot,
    object_type: SourceType,
) {
    let literal = ck
        .node(dot_expr.rhs)
        .to_lit_int()
        .expect("literal expected");

    let (ty, value) = compute_lit_int(ck.sa, ck.file_id, dot_expr.rhs, SourceType::Any);

    if ty.is_float() {
        ck.sa
            .report(ck.file_id, literal.span, ErrorMessage::IndexExpected);
    }

    ck.analysis.set_const_value(dot_expr.rhs, value.clone());

    let index = value.to_i64().unwrap_or(0) as usize;

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
            let msg = ErrorMessage::UnknownField(name, expr_name);
            ck.sa.report(ck.file_id, ck.span(e.rhs), msg);

            check_expr(ck, e.rhs, SourceType::Any);
        }

        SourceType::Struct(struct_id, struct_type_params) => {
            let struct_ = ck.sa.struct_(struct_id);
            if !struct_.field_name_style.is_named() && index < struct_.field_ids().len() {
                let field_id = struct_.field_id(FieldIndex(index));
                let field = ck.sa.field(field_id);
                let ident_type = IdentType::StructField(object_type.clone(), field.index);
                ck.analysis
                    .map_idents
                    .insert_or_replace(dot_expr_id, ident_type);

                let fty = replace_type(ck.sa, field.ty(), Some(&struct_type_params), None);

                if !struct_field_accessible_from(ck.sa, struct_id, field.index, ck.module_id) {
                    let msg = ErrorMessage::NotAccessible;
                    ck.sa.report(ck.file_id, ck.span(dot_expr.rhs), msg);
                }

                let rhs_type = check_expr(ck, e.rhs, fty.clone());

                if !fty.allows(ck.sa, rhs_type.clone()) && !rhs_type.is_error() {
                    let name = index.to_string();
                    let object_type = ck.ty_name(&object_type);
                    let lhs_type = ck.ty_name(&fty);
                    let rhs_type = ck.ty_name(&rhs_type);

                    let msg = ErrorMessage::AssignField(name, object_type, lhs_type, rhs_type);
                    ck.sa.report(ck.file_id, e.span, msg);
                }

                let msg = ErrorMessage::ImmutableField;
                ck.sa.report(ck.file_id, e.span, msg);
            } else {
                let name = index.to_string();
                let expr_name = ck.ty_name(&object_type);
                let msg = ErrorMessage::UnknownField(name, expr_name);
                ck.sa.report(ck.file_id, ck.span(dot_expr.rhs), msg);

                check_expr(ck, e.rhs, SourceType::Any);
            }
        }

        SourceType::Tuple(subtypes) => {
            if index < subtypes.len() {
                let ty = subtypes[usize::try_from(index).unwrap()].clone();
                let rhs_type = check_expr(ck, e.rhs, ty.clone());

                if !ty.allows(ck.sa, rhs_type.clone()) && !rhs_type.is_error() {
                    let name = index.to_string();
                    let object_type = ck.ty_name(&object_type);
                    let lhs_type = ck.ty_name(&ty);
                    let rhs_type = ck.ty_name(&rhs_type);

                    let msg = ErrorMessage::AssignField(name, object_type, lhs_type, rhs_type);
                    ck.sa.report(ck.file_id, e.span, msg);
                }

                let msg = ErrorMessage::ImmutableField;
                ck.sa.report(ck.file_id, e.span, msg);
            } else {
                let name = index.to_string();
                let expr_name = ck.ty_name(&object_type);
                let msg = ErrorMessage::UnknownField(name, expr_name);
                ck.sa.report(ck.file_id, ck.span(dot_expr.rhs), msg);

                check_expr(ck, e.rhs, SourceType::Any);
            }
        }

        SourceType::Class(class_id, class_type_params) => {
            let cls = ck.sa.class(class_id);
            if !cls.field_name_style.is_named() && index < cls.field_ids().len() {
                let field_id = cls.field_id(FieldIndex(index));
                let field = ck.sa.field(field_id);
                let ident_type = IdentType::Field(object_type.clone(), field.index);
                ck.analysis
                    .map_idents
                    .insert_or_replace(dot_expr_id, ident_type);

                let fty = replace_type(ck.sa, field.ty(), Some(&class_type_params), None);

                if !class_field_accessible_from(ck.sa, class_id, field.index, ck.module_id) {
                    let msg = ErrorMessage::NotAccessible;
                    ck.sa.report(ck.file_id, ck.span(dot_expr.rhs), msg);
                }

                let rhs_type = check_expr(ck, e.rhs, fty.clone());

                if !fty.allows(ck.sa, rhs_type.clone()) && !rhs_type.is_error() {
                    let name = index.to_string();
                    let object_type = ck.ty_name(&object_type);
                    let lhs_type = ck.ty_name(&fty);
                    let rhs_type = ck.ty_name(&rhs_type);

                    let msg = ErrorMessage::AssignField(name, object_type, lhs_type, rhs_type);
                    ck.sa.report(ck.file_id, e.span, msg);
                }

                ck.analysis.set_ty(expr_id, fty.clone());
            } else {
                let name = index.to_string();
                let expr_name = ck.ty_name(&object_type);
                let msg = ErrorMessage::UnknownField(name, expr_name);
                ck.sa.report(ck.file_id, ck.span(dot_expr.rhs), msg);

                check_expr(ck, e.rhs, SourceType::Any);
            }
        }
    }
}

pub(super) fn check_expr_dot(
    ck: &mut TypeCheck,
    expr_id: ast::AstId,
    e: &ast::Dot,
    _expected_ty: SourceType,
) -> SourceType {
    let object_type = check_expr(ck, e.lhs, SourceType::Any);

    let rhs = ck.node(e.rhs);

    if rhs.is_lit_int() {
        return check_expr_dot_unnamed_field(ck, expr_id, e, object_type);
    }

    let name = match rhs.to_ident() {
        Some(ident) => ident.name.clone(),

        None => {
            let msg = ErrorMessage::NameExpected;
            ck.sa.report(ck.file_id, e.op_span, msg);

            ck.analysis.set_ty(expr_id, ty_error());
            return ty_error();
        }
    };

    let interned_name = ck.sa.interner.intern(&name);

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
        | SourceType::Tuple(..)
        | SourceType::Alias(..)
        | SourceType::Assoc { .. }
        | SourceType::GenericAssoc { .. } => {}
        SourceType::Class(cls_id, class_type_params) => {
            if let Some((field_index, _)) =
                find_field_in_class(ck.sa, object_type.clone(), interned_name)
            {
                let ident_type = IdentType::Field(object_type.clone(), field_index);
                ck.analysis
                    .map_idents
                    .insert_or_replace(expr_id, ident_type);

                let cls = ck.sa.class(cls_id);
                let field_id = cls.field_id(field_index);
                let field = ck.sa.field(field_id);
                let call_data = CallSpecializationData {
                    object_ty: SourceType::Error,
                    type_params: class_type_params,
                };
                let fty = specialize_ty_for_call(ck.sa, field.ty(), ck.element, &call_data);

                if !class_field_accessible_from(ck.sa, cls_id, field_index, ck.module_id) {
                    let msg = ErrorMessage::NotAccessible;
                    ck.sa.report(ck.file_id, rhs.span(), msg);
                }

                ck.analysis.set_ty(expr_id, fty.clone());
                return fty;
            }
        }
        SourceType::Struct(struct_id, struct_type_params) => {
            let struct_ = ck.sa.struct_(struct_id);
            if let Some(&field_index) = struct_.field_names().get(&interned_name) {
                let ident_type = IdentType::StructField(object_type.clone(), field_index);
                ck.analysis
                    .map_idents
                    .insert_or_replace(expr_id, ident_type);

                let field_id = struct_.field_id(field_index);
                let field = &ck.sa.field(field_id);
                let call_data = CallSpecializationData {
                    object_ty: SourceType::Error,
                    type_params: struct_type_params,
                };
                let fty = specialize_ty_for_call(ck.sa, field.ty(), ck.element, &call_data);

                if !struct_field_accessible_from(ck.sa, struct_id, field_index, ck.module_id) {
                    let msg = ErrorMessage::NotAccessible;
                    ck.sa.report(ck.file_id, rhs.span(), msg);
                }

                ck.analysis.set_ty(expr_id, fty.clone());
                return fty;
            }
        }
    }

    // field not found, report error
    if !object_type.is_error() {
        let expr_name = ck.ty_name(&object_type);
        let msg = ErrorMessage::UnknownField(name, expr_name);
        ck.sa.report(ck.file_id, ck.span(e.rhs), msg);
    }

    ck.analysis.set_ty(expr_id, ty_error());

    ty_error()
}

fn check_expr_dot_unnamed_field(
    ck: &mut TypeCheck,
    expr_id: ast::AstId,
    e: &ast::Dot,
    object_type: SourceType,
) -> SourceType {
    let literal = ck.node(e.rhs).as_lit_int();

    let (ty, value) = compute_lit_int(ck.sa, ck.file_id, e.rhs, SourceType::Any);

    if ty.is_float() {
        ck.sa
            .report(ck.file_id, literal.span, ErrorMessage::IndexExpected);
    }

    ck.analysis.set_const_value(e.rhs, value.clone());

    let index = value.to_i64().unwrap_or(0) as usize;

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
            let msg = ErrorMessage::UnknownField(name, expr_name);
            ck.sa.report(ck.file_id, ck.span(e.rhs), msg);
            SourceType::Error
        }

        SourceType::Class(class_id, class_type_params) => {
            let cls = ck.sa.class(class_id);
            if !cls.field_name_style.is_named() && index < cls.field_ids().len() {
                let field_id = cls.field_id(FieldIndex(index));
                let field = ck.sa.field(field_id);
                let ident_type = IdentType::Field(object_type.clone(), field.index);
                ck.analysis
                    .map_idents
                    .insert_or_replace(expr_id, ident_type);

                let call_data = CallSpecializationData {
                    object_ty: SourceType::Error,
                    type_params: class_type_params,
                };
                let fty = specialize_ty_for_call(ck.sa, field.ty(), ck.element, &call_data);

                if !class_field_accessible_from(ck.sa, class_id, field.index, ck.module_id) {
                    let msg = ErrorMessage::NotAccessible;
                    ck.sa.report(ck.file_id, ck.span(e.rhs), msg);
                }

                ck.analysis.set_ty(expr_id, fty.clone());
                fty
            } else {
                let name = index.to_string();
                let expr_name = ck.ty_name(&object_type);
                let msg = ErrorMessage::UnknownField(name, expr_name);
                ck.sa.report(ck.file_id, ck.span(e.rhs), msg);
                SourceType::Error
            }
        }

        SourceType::Struct(struct_id, struct_type_params) => {
            let struct_ = ck.sa.struct_(struct_id);
            if !struct_.field_name_style.is_named() && index < struct_.field_ids().len() {
                let field_id = struct_.field_id(FieldIndex(index));
                let field = ck.sa.field(field_id);
                let ident_type = IdentType::StructField(object_type.clone(), field.index);
                ck.analysis
                    .map_idents
                    .insert_or_replace(expr_id, ident_type);

                let call_data = CallSpecializationData {
                    object_ty: SourceType::Error,
                    type_params: struct_type_params,
                };
                let fty = specialize_ty_for_call(ck.sa, field.ty(), ck.element, &call_data);

                if !struct_field_accessible_from(ck.sa, struct_id, field.index, ck.module_id) {
                    let msg = ErrorMessage::NotAccessible;
                    ck.sa.report(ck.file_id, ck.span(e.rhs), msg);
                }

                ck.analysis.set_ty(expr_id, fty.clone());
                fty
            } else {
                let name = index.to_string();
                let expr_name = ck.ty_name(&object_type);
                let msg = ErrorMessage::UnknownField(name, expr_name);
                ck.sa.report(ck.file_id, ck.span(e.rhs), msg);
                SourceType::Error
            }
        }

        SourceType::Tuple(subtypes) => {
            if index >= subtypes.len() {
                let msg = ErrorMessage::IllegalTupleIndex(index, ck.ty_name(&object_type));
                ck.sa.report(ck.file_id, e.op_span, msg);

                ck.analysis.set_ty(expr_id, ty_error());
                return ty_error();
            }

            let ty = subtypes[usize::try_from(index).unwrap()].clone();
            ck.analysis.set_ty(expr_id, ty.clone());
            ty
        }
    }
}

pub(super) fn check_expr_this(
    ck: &mut TypeCheck,
    expr_id: ast::AstId,
    e: &ast::This,
    _expected_ty: SourceType,
) -> SourceType {
    if !ck.is_self_available {
        let msg = ErrorMessage::ThisUnavailable;
        ck.sa.report(ck.file_id, e.span, msg);
        ck.analysis.set_ty(expr_id, ty_error());
        return ty_error();
    }

    assert!(ck.is_self_available);
    let var_id = NestedVarId(0);
    let ident = ck.maybe_allocate_in_context(var_id);
    ck.analysis.map_idents.insert(expr_id, ident);

    let var = ck.vars.get_var(var_id);
    ck.analysis.set_ty(expr_id, var.ty.clone());
    var.ty.clone()
}

fn check_expr_conv(
    ck: &mut TypeCheck,
    node_id: ast::AstId,
    e: &ast::Conv,
    _expected_ty: SourceType,
) -> SourceType {
    let object_type = check_expr(ck, e.object, SourceType::Any);
    ck.analysis.set_ty(e.object, object_type.clone());

    let check_type = ck.read_type(e.data_type);
    ck.analysis.set_ty(e.data_type, check_type.clone());

    if check_type.is_trait_object() {
        let implements = implements_trait(
            ck.sa,
            object_type.clone(),
            ck.element,
            TraitType::new_ty(ck.sa, check_type.clone()),
        );

        if !implements {
            let object_type = ck.ty_name(&object_type);
            let check_type = ck.ty_name(&check_type);

            ck.sa.report(
                ck.file_id,
                e.span,
                ErrorMessage::TypeNotImplementingTrait(object_type, check_type),
            );
        }

        ck.analysis.set_ty(node_id, check_type.clone());
        check_type
    } else if !check_type.is_error() {
        let name = ck.ty_name(&check_type);
        ck.sa
            .report(ck.file_id, e.span, ErrorMessage::TraitExpected(name));
        let ty = ty_error();
        ck.analysis.set_ty(node_id, ty.clone());
        ty
    } else {
        ty_error()
    }
}

fn check_expr_is(
    ck: &mut TypeCheck,
    _id: ast::AstId,
    e: &ast::Is,
    _expected_ty: SourceType,
) -> SourceType {
    let value_type = check_expr(ck, e.value, SourceType::Any);
    ck.analysis.set_ty(e.value, value_type.clone());

    check_pattern(ck, e.pattern, value_type.clone());

    SourceType::Bool
}

pub(super) fn check_expr_lit_int(
    ck: &mut TypeCheck,
    expr_id: ast::AstId,
    negate: bool,
    expected_ty: SourceType,
) -> SourceType {
    let (ty, value) = check_lit_int(ck.sa, ck.file_id, expr_id, negate, expected_ty);

    ck.analysis.set_ty(expr_id, ty.clone());
    ck.analysis.set_const_value(expr_id, value);

    ty
}

pub fn compute_lit_int(
    sa: &Sema,
    file_id: SourceFileId,
    expr_id: ast::AstId,
    expected_ty: SourceType,
) -> (SourceType, ConstValue) {
    let e = sa.node(file_id, expr_id);

    if e.is_un_op(ast::UnOp::Neg) {
        let e = e.as_un();
        check_lit_int(sa, file_id, e.opnd, true, expected_ty)
    } else {
        check_lit_int(sa, file_id, expr_id, false, expected_ty)
    }
}

pub fn compute_lit_float(
    sa: &Sema,
    file_id: SourceFileId,
    expr_id: ast::AstId,
) -> (SourceType, f64) {
    let e = sa.node(file_id, expr_id);

    if e.is_un_op(ast::UnOp::Neg) {
        let e = e.as_un();
        let lit = sa
            .node(file_id, e.opnd)
            .to_lit_float()
            .expect("literal expected");
        check_lit_float(sa, file_id, lit, true)
    } else {
        let lit = e.as_lit_float();
        check_lit_float(sa, file_id, lit, false)
    }
}

fn check_expr_lit_float(
    ck: &mut TypeCheck,
    node_id: ast::AstId,
    e: &ast::LitFloat,
    negate: bool,
    _expected_ty: SourceType,
) -> SourceType {
    let (ty, value) = check_lit_float(ck.sa, ck.file_id, e, negate);

    ck.analysis.set_ty(node_id, ty.clone());
    ck.analysis
        .set_const_value(node_id, ConstValue::Float(value));

    ty
}

fn check_expr_lit_bool(
    ck: &mut TypeCheck,
    node_id: ast::AstId,
    _node: &ast::LitBool,
    _expected_ty: SourceType,
) -> SourceType {
    ck.analysis.set_ty(node_id, SourceType::Bool);

    SourceType::Bool
}

pub fn check_expr_lit_char(
    ck: &mut TypeCheck,
    node_id: AstId,
    node: &ast::LitChar,
    _expected_ty: SourceType,
) -> SourceType {
    let value = check_lit_char(ck.sa, ck.file_id, node);

    ck.analysis.set_ty(node_id, SourceType::Char);
    ck.analysis
        .set_const_value(node_id, ConstValue::Char(value));

    SourceType::Char
}

fn check_expr_lit_str(
    ck: &mut TypeCheck,
    node_id: AstId,
    node: &ast::LitStr,
    _expected_ty: SourceType,
) -> SourceType {
    let value = check_lit_str(ck.sa, ck.file_id, node);

    let str_ty = SourceType::Class(ck.sa.known.classes.string(), SourceTypeArray::empty());
    ck.analysis.set_ty(node_id, str_ty.clone());
    ck.analysis
        .set_const_value(node_id, ConstValue::String(value));

    str_ty
}

fn check_expr_template(
    ck: &mut TypeCheck,
    node_id: ast::AstId,
    node: &ast::Template,
    expected_ty: SourceType,
) -> SourceType {
    let stringable_trait_id = ck.sa.known.traits.stringable();
    let stringable_trait_ty = TraitType::from_trait_id(stringable_trait_id);

    for (idx, &part_id) in node.parts.iter().enumerate() {
        if idx % 2 != 0 {
            let part_expr = check_expr(ck, part_id, SourceType::Any);

            if part_expr.is_error() {
                continue;
            }

            if implements_trait(
                ck.sa,
                part_expr.clone(),
                ck.element,
                stringable_trait_ty.clone(),
            ) {
                if !part_expr.is_type_param() {
                    let impl_match = find_impl(
                        ck.sa,
                        ck.element,
                        part_expr.clone(),
                        &ck.type_param_definition,
                        stringable_trait_ty.clone(),
                    )
                    .expect("missing impl");
                    let stringable_impl_id = impl_match.id;

                    let name = ck.sa.interner.intern("toString");
                    let stringable_trait = &ck.sa.trait_(stringable_trait_id);
                    let trait_to_string_id = stringable_trait
                        .get_method(name, false)
                        .expect("missing method");

                    let to_string_id = ck
                        .sa
                        .impl_(stringable_impl_id)
                        .get_method_for_trait_method_id(trait_to_string_id)
                        .expect("missing method");

                    ck.analysis
                        .map_templates
                        .insert(part_id, (to_string_id, impl_match.bindings));
                }
            } else {
                let ty = ck.ty_name(&part_expr);
                ck.sa.report(
                    ck.file_id,
                    ck.span(part_id),
                    ErrorMessage::ExpectedStringable(ty),
                );
            }
        } else {
            let e = ck.node(part_id).as_lit_str();
            check_expr_lit_str(ck, part_id, e, expected_ty.clone());
        }
    }

    let str_ty = SourceType::Class(ck.sa.known.classes.string(), SourceTypeArray::empty());
    ck.analysis.set_ty(node_id, str_ty.clone());

    str_ty
}

pub(super) fn check_expr_un(
    ck: &mut TypeCheck,
    node_id: ast::AstId,
    e: &ast::Un,
    expected_ty: SourceType,
) -> SourceType {
    let opnd = ck.node(e.opnd);

    if e.op == ast::UnOp::Neg && opnd.is_lit_int() {
        let expr_type = check_expr_lit_int(ck, e.opnd, true, expected_ty);
        ck.analysis.set_ty(node_id, expr_type.clone());
        return expr_type;
    }

    let opnd = check_expr(ck, e.opnd, SourceType::Any);

    match e.op {
        ast::UnOp::Neg => {
            check_expr_un_trait(ck, node_id, e, e.op, ck.sa.known.traits.neg(), "neg", opnd)
        }
        ast::UnOp::Not => {
            check_expr_un_trait(ck, node_id, e, e.op, ck.sa.known.traits.not(), "not", opnd)
        }
    }
}

fn check_expr_un_trait(
    ck: &mut TypeCheck,
    node_id: ast::AstId,
    e: &ast::Un,
    op: ast::UnOp,
    trait_id: TraitDefinitionId,
    trait_method_name: &str,
    ty: SourceType,
) -> SourceType {
    let trait_ty = TraitType::from_trait_id(trait_id);
    let trait_method_name = ck.sa.interner.intern(trait_method_name);

    let impl_match = find_impl(
        ck.sa,
        ck.element,
        ty.clone(),
        &ck.type_param_definition,
        trait_ty.clone(),
    );

    if let Some(impl_match) = impl_match {
        let trait_ = ck.sa.trait_(trait_id);
        let trait_method_id = trait_
            .get_method(trait_method_name, false)
            .expect("missing method");
        let method_id = ck
            .sa
            .impl_(impl_match.id)
            .get_method_for_trait_method_id(trait_method_id)
            .expect("method not found");

        let call_type = CallType::Method(ty.clone(), method_id, SourceTypeArray::empty());
        ck.analysis
            .map_calls
            .insert_or_replace(node_id, Arc::new(call_type));

        let method = ck.sa.fct(method_id);

        let return_type = method.return_type();
        ck.analysis.set_ty(node_id, return_type.clone());

        return_type
    } else if ty.is_type_param() && implements_trait(ck.sa, ty.clone(), ck.element, trait_ty) {
        let trait_ = &ck.sa.trait_(trait_id);

        let method_id = trait_
            .get_method(trait_method_name, false)
            .expect("method not found");

        let method = ck.sa.fct(method_id);

        let call_type = CallType::GenericMethod(
            ty.type_param_id().expect("type param expected"),
            trait_id,
            method_id,
            SourceTypeArray::empty(),
            SourceTypeArray::empty(),
        );
        ck.analysis
            .map_calls
            .insert_or_replace(node_id, Arc::new(call_type));

        let return_type = method.return_type();
        let return_type = replace_type(
            ck.sa,
            return_type,
            Some(&SourceTypeArray::empty()),
            Some(ty.clone()),
        );

        ck.analysis.set_ty(node_id, return_type.clone());

        return_type
    } else {
        let ty = ck.ty_name(&ty);
        let msg = ErrorMessage::UnOpType(op.as_str().into(), ty);
        ck.sa.report(ck.file_id, e.span, msg);

        ck.analysis.set_ty(node_id, ty_error());
        ty_error()
    }
}

pub(super) fn check_expr_bin(
    ck: &mut TypeCheck,
    node_id: ast::AstId,
    node: &ast::Bin,
    _expected_ty: SourceType,
) -> SourceType {
    if node.op.is_any_assign() {
        check_expr_assign(ck, node_id, node);
        return SourceType::Unit;
    }

    let lhs = ck.node(node.lhs);

    if node.op == ast::BinOp::And && lhs.is_is() {
        ck.symtable.push_level();
        let is_expr = lhs.as_is();
        let value_ty = check_expr(ck, is_expr.value, SourceType::Any);
        check_pattern(ck, is_expr.pattern, value_ty);
        let cond_ty = check_expr(ck, node.rhs, SourceType::Bool);
        if !cond_ty.is_bool() && !cond_ty.is_error() {
            let cond_ty = cond_ty.name(ck.sa);
            let msg = ErrorMessage::WrongType("Bool".into(), cond_ty);
            ck.sa.report(ck.file_id, node.span, msg);
        }
        ck.symtable.pop_level();
        ck.analysis.set_ty(node_id, SourceType::Bool);
        return SourceType::Bool;
    }

    let lhs_type = check_expr(ck, node.lhs, SourceType::Any);
    let rhs_type = check_expr(ck, node.rhs, SourceType::Any);

    if lhs_type.is_error() || rhs_type.is_error() {
        ck.analysis.set_ty(node_id, ty_error());
        return ty_error();
    }

    match node.op {
        ast::BinOp::Or | ast::BinOp::And => {
            check_expr_bin_bool(ck, node_id, node, node.op, lhs_type, rhs_type)
        }
        ast::BinOp::Cmp(cmp) => check_expr_bin_cmp(ck, node_id, node, cmp, lhs_type, rhs_type),
        ast::BinOp::Add => {
            check_expr_bin_trait(
                ck,
                node_id,
                node,
                node.op,
                ck.sa.known.traits.add(),
                "add",
                lhs_type,
                rhs_type,
            )
            .return_type
        }
        ast::BinOp::Sub => {
            check_expr_bin_trait(
                ck,
                node_id,
                node,
                node.op,
                ck.sa.known.traits.sub(),
                "sub",
                lhs_type,
                rhs_type,
            )
            .return_type
        }
        ast::BinOp::Mul => {
            check_expr_bin_trait(
                ck,
                node_id,
                node,
                node.op,
                ck.sa.known.traits.mul(),
                "mul",
                lhs_type,
                rhs_type,
            )
            .return_type
        }
        ast::BinOp::Div => {
            check_expr_bin_trait(
                ck,
                node_id,
                node,
                node.op,
                ck.sa.known.traits.div(),
                "div",
                lhs_type,
                rhs_type,
            )
            .return_type
        }
        ast::BinOp::Mod => {
            check_expr_bin_trait(
                ck,
                node_id,
                node,
                node.op,
                ck.sa.known.traits.mod_(),
                "modulo",
                lhs_type,
                rhs_type,
            )
            .return_type
        }
        ast::BinOp::BitOr => {
            check_expr_bin_trait(
                ck,
                node_id,
                node,
                node.op,
                ck.sa.known.traits.bit_or(),
                "bitor",
                lhs_type,
                rhs_type,
            )
            .return_type
        }
        ast::BinOp::BitAnd => {
            check_expr_bin_trait(
                ck,
                node_id,
                node,
                node.op,
                ck.sa.known.traits.bit_and(),
                "bitand",
                lhs_type,
                rhs_type,
            )
            .return_type
        }
        ast::BinOp::BitXor => {
            check_expr_bin_trait(
                ck,
                node_id,
                node,
                node.op,
                ck.sa.known.traits.bit_xor(),
                "bitxor",
                lhs_type,
                rhs_type,
            )
            .return_type
        }
        ast::BinOp::ShiftL => {
            check_expr_bin_trait(
                ck,
                node_id,
                node,
                node.op,
                ck.sa.known.traits.shl(),
                "shl",
                lhs_type,
                rhs_type,
            )
            .return_type
        }
        ast::BinOp::ArithShiftR => {
            check_expr_bin_trait(
                ck,
                node_id,
                node,
                node.op,
                ck.sa.known.traits.sar(),
                "sar",
                lhs_type,
                rhs_type,
            )
            .return_type
        }
        ast::BinOp::LogicalShiftR => {
            check_expr_bin_trait(
                ck,
                node_id,
                node,
                node.op,
                ck.sa.known.traits.shr(),
                "shr",
                lhs_type,
                rhs_type,
            )
            .return_type
        }
        ast::BinOp::Assign
        | ast::BinOp::AddAssign
        | ast::BinOp::SubAssign
        | ast::BinOp::MulAssign
        | ast::BinOp::ModAssign
        | ast::BinOp::DivAssign
        | ast::BinOp::BitOrAssign
        | ast::BinOp::BitAndAssign
        | ast::BinOp::BitXorAssign
        | ast::BinOp::ShiftLAssign
        | ast::BinOp::ArithShiftRAssign
        | ast::BinOp::LogicalShiftRAssign => unreachable!(),
    }
}

fn check_expr_bin_bool(
    ck: &mut TypeCheck,
    node_id: AstId,
    node: &ast::Bin,
    op: ast::BinOp,
    lhs_type: SourceType,
    rhs_type: SourceType,
) -> SourceType {
    check_type(ck, node, op, lhs_type, rhs_type, SourceType::Bool);
    ck.analysis.set_ty(node_id, SourceType::Bool);

    SourceType::Bool
}

struct OpTraitInfo {
    rhs_type: SourceType,
    return_type: SourceType,
}

fn check_expr_bin_trait(
    ck: &mut TypeCheck,
    node_id: ast::AstId,
    node: &ast::Bin,
    op: ast::BinOp,
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
            ck.analysis
                .map_calls
                .insert_or_replace(node_id, Arc::new(call_type));

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
                let msg = ErrorMessage::BinOpType(op.as_str().into(), lhs_type, rhs_type);

                ck.sa.report(ck.file_id, node.span, msg);
            }

            let return_type = method.return_type();
            ck.analysis.set_ty(node_id, return_type.clone());

            OpTraitInfo {
                rhs_type,
                return_type,
            }
        } else {
            ck.analysis.set_ty(node_id, ty_error());
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
        ck.analysis
            .map_calls
            .insert_or_replace(node_id, Arc::new(call_type));

        let param = params[0].ty();
        let param = replace_type(
            ck.sa,
            param,
            Some(&SourceTypeArray::empty()),
            Some(lhs_type.clone()),
        );

        if !param.allows(ck.sa, rhs_type.clone()) {
            let lhs_type = ck.ty_name(&lhs_type);
            let rhs_type = ck.ty_name(&rhs_type);
            let msg = ErrorMessage::BinOpType(op.as_str().into(), lhs_type, rhs_type);

            ck.sa.report(ck.file_id, node.span, msg);
        }

        let return_type = method.return_type();
        let return_type = replace_type(
            ck.sa,
            return_type,
            Some(&SourceTypeArray::empty()),
            Some(lhs_type.clone()),
        );

        ck.analysis.set_ty(node_id, return_type.clone());

        OpTraitInfo {
            rhs_type,
            return_type,
        }
    } else {
        if !lhs_type.is_error() && !rhs_type.is_error() {
            let lhs_type = ck.ty_name(&lhs_type);
            let rhs_type = ck.ty_name(&rhs_type);
            let msg = ErrorMessage::BinOpType(op.as_str().into(), lhs_type, rhs_type);

            ck.sa.report(ck.file_id, node.span, msg);
        }

        ck.analysis.set_ty(node_id, ty_error());

        OpTraitInfo {
            rhs_type: ty_error(),
            return_type: ty_error(),
        }
    }
}

fn check_expr_bin_cmp(
    ck: &mut TypeCheck,
    node_id: ast::AstId,
    node: &ast::Bin,
    cmp: ast::CmpOp,
    lhs_type: SourceType,
    rhs_type: SourceType,
) -> SourceType {
    match cmp {
        ast::CmpOp::Is | ast::CmpOp::IsNot => {
            if lhs_type != rhs_type {
                let lhs_type = ck.ty_name(&lhs_type);
                let rhs_type = ck.ty_name(&rhs_type);
                ck.sa.report(
                    ck.file_id,
                    node.span,
                    ErrorMessage::TypesIncompatible(lhs_type, rhs_type),
                );
            } else if !lhs_type.is_class() && !lhs_type.is_lambda() && !lhs_type.is_trait_object() {
                let lhs_type = ck.ty_name(&lhs_type);
                ck.sa.report(
                    ck.file_id,
                    node.span,
                    ErrorMessage::ExpectedIdentityType(lhs_type),
                );
            }

            ck.analysis.set_ty(node_id, SourceType::Bool);
            return SourceType::Bool;
        }

        ast::CmpOp::Eq | ast::CmpOp::Ne => {
            if is_simple_enum(ck.sa, lhs_type.clone()) {
                check_expr_cmp_enum(ck, node_id, node, cmp, lhs_type, rhs_type)
            } else {
                check_expr_bin_trait(
                    ck,
                    node_id,
                    node,
                    node.op,
                    ck.sa.known.traits.equals(),
                    "equals",
                    lhs_type,
                    rhs_type,
                );
            }
        }

        ast::CmpOp::Ge | ast::CmpOp::Gt | ast::CmpOp::Le | ast::CmpOp::Lt => {
            check_expr_bin_trait(
                ck,
                node_id,
                node,
                node.op,
                ck.sa.known.traits.comparable(),
                "cmp",
                lhs_type,
                rhs_type,
            );
        }
    }

    ck.analysis.set_ty(node_id, SourceType::Bool);

    SourceType::Bool
}

fn check_expr_cmp_enum(
    ck: &mut TypeCheck,
    node_id: ast::AstId,
    node: &ast::Bin,
    op: ast::CmpOp,
    lhs_type: SourceType,
    rhs_type: SourceType,
) {
    if lhs_type.allows(ck.sa, rhs_type.clone()) {
        let intrinsic = match op {
            ast::CmpOp::Eq => Intrinsic::EnumEq,
            ast::CmpOp::Ne => Intrinsic::EnumNe,
            _ => unreachable!(),
        };
        let call_type = CallType::Intrinsic(intrinsic);
        ck.analysis
            .map_calls
            .insert_or_replace(node_id, Arc::new(call_type));

        ck.analysis.set_ty(node_id, SourceType::Bool);
    } else {
        let lhs_type = ck.ty_name(&lhs_type);
        let rhs_type = ck.ty_name(&rhs_type);
        let msg = ErrorMessage::BinOpType("equals".into(), lhs_type, rhs_type);

        ck.sa.report(ck.file_id, node.span, msg);

        ck.analysis.set_ty(node_id, ty_error());
    }
}

fn check_expr_lambda(
    ck: &mut TypeCheck,
    lambda_expr_ast_id: ast::AstId,
    lambda_expr: &ast::Lambda,
    _expected_ty: SourceType,
) -> SourceType {
    let node = ck
        .sa
        .file(ck.file_id)
        .node(lambda_expr.fct_id)
        .to_function()
        .expect("fct expected");

    let lambda_return_type = if let Some(ret_type) = node.return_type {
        ck.read_type(ret_type)
    } else {
        SourceType::Unit
    };

    let mut params = Vec::new();

    for &ast_param_id in &node.params {
        let ast_param = ck.node(ast_param_id).as_param();
        let ty = ck.read_type(ast_param.data_type);
        let param = Param::new_ty(ty.clone());
        params.push(param);
    }

    let param_types = params.iter().map(|p| p.ty()).collect::<Vec<_>>();
    let ty = SourceType::Lambda(
        SourceTypeArray::with(param_types),
        Box::new(lambda_return_type.clone()),
    );

    let param = Param::new_ty(SourceType::Ptr);
    let mut lambda_params = vec![param];
    lambda_params.append(&mut params);

    let analysis = {
        let mut analysis = AnalysisData::new();
        analysis.outer_contexts = ck.context_classes.clone();

        {
            let mut typeck = TypeCheck {
                sa: ck.sa,
                type_param_definition: ck.type_param_definition,
                package_id: ck.package_id,
                module_id: ck.module_id,
                file_id: ck.file_id,
                analysis: &mut analysis,
                symtable: &mut ck.symtable,
                in_loop: false,
                is_lambda: true,
                param_types: lambda_params.clone(),
                return_type: Some(lambda_return_type.clone()),
                parent: ck.parent.clone(),
                has_hidden_self_argument: true,
                is_self_available: ck.is_self_available,
                self_ty: ck.self_ty.clone(),
                vars: ck.vars,
                lazy_context_class_creation: ck.lazy_context_class_creation,
                lazy_lambda_creation: ck.lazy_lambda_creation,
                context_classes: ck.context_classes,
                start_context_id: 0,
                needs_context_slot_in_lambda_object: false,
                element: ck.element,
            };

            typeck.check_fct(&node);
        }

        analysis
    };

    let name = ck.sa.generate_lambda_name();
    let name = ck.sa.interner.intern(&name);

    let lambda = FctDefinition::new(
        ck.package_id,
        ck.module_id,
        ck.file_id,
        lambda_expr.fct_id,
        node,
        Annotations::default(),
        name,
        ck.type_param_definition.clone(),
        Params::new(lambda_params, true, false),
        FctParent::Function,
    );
    lambda.parsed_return_type().set_ty(lambda_return_type);
    assert!(lambda.analysis.set(analysis).is_ok());

    let lambda_id = LazyLambdaId::new();

    ck.lazy_lambda_creation.push(LazyLambdaCreationData {
        id: lambda_id.clone(),
        fct_definition: lambda,
    });
    ck.analysis
        .map_lambdas
        .insert(lambda_expr_ast_id, lambda_id);
    ck.analysis.set_ty(lambda_expr_ast_id, ty.clone());

    ty
}

pub(super) fn check_expr_path(
    ck: &mut TypeCheck,
    node_id: ast::AstId,
    e: &ast::Path,
    expected_ty: SourceType,
) -> SourceType {
    let (container_expr, type_params) =
        if let Some(expr_type_params) = ck.node(e.lhs).to_typed_expr() {
            let type_params: Vec<SourceType> = expr_type_params
                .args
                .iter()
                .map(|&p| ck.read_type(p))
                .collect();
            let type_params: SourceTypeArray = SourceTypeArray::with(type_params);

            (expr_type_params.callee, type_params)
        } else {
            (e.lhs, SourceTypeArray::empty())
        };

    let sym = match read_path_expr(ck, container_expr) {
        Ok(sym) => sym,
        Err(()) => {
            ck.analysis.set_ty(node_id, ty_error());
            return ty_error();
        }
    };

    let element_name = if let Some(ident) = ck.node(e.rhs).to_ident() {
        ident.name.clone()
    } else {
        let msg = ErrorMessage::ExpectedSomeIdentifier;
        ck.sa.report(ck.file_id, ck.span(e.rhs), msg);
        return ty_error();
    };

    match sym {
        Some(SymbolKind::Enum(id)) => check_enum_variant_without_args(
            ck,
            node_id,
            e.op_span,
            expected_ty,
            id,
            type_params,
            element_name,
        ),

        Some(SymbolKind::Module(module_id)) => {
            check_expr_path_module(ck, node_id, e, expected_ty, module_id, element_name)
        }

        _ => {
            let msg = ErrorMessage::InvalidLeftSideOfSeparator;
            ck.sa.report(ck.file_id, ck.span(e.lhs), msg);

            ck.analysis.set_ty(node_id, ty_error());
            ty_error()
        }
    }
}

pub(super) fn read_path_expr(
    ck: &mut TypeCheck,
    expr_id: ast::AstId,
) -> Result<Option<SymbolKind>, ()> {
    let expr = ck.node(expr_id);

    if let Some(expr_path) = expr.to_path() {
        let sym = read_path_expr(ck, expr_path.lhs)?;
        let rhs = ck.node(expr_path.rhs);

        let element_name = if let Some(ident) = rhs.to_ident() {
            ident.name.clone()
        } else {
            let msg = ErrorMessage::ExpectedSomeIdentifier;
            ck.sa.report(ck.file_id, rhs.span(), msg);
            return Err(());
        };

        let interned_element_name = ck.sa.interner.intern(&element_name);

        match sym {
            Some(SymbolKind::Module(module_id)) => {
                let module = ck.sa.module(module_id);
                let symtable = module.table();
                let sym = symtable.get(interned_element_name);

                Ok(sym)
            }

            _ => {
                let msg = ErrorMessage::ExpectedModule;
                ck.sa.report(ck.file_id, expr.span(), msg);
                Err(())
            }
        }
    } else if let Some(expr_ident) = expr.to_ident() {
        let sym = ck.symtable.get_string(ck.sa, &expr_ident.name);

        Ok(sym)
    } else {
        let msg = ErrorMessage::ExpectedSomeIdentifier;
        ck.sa.report(ck.file_id, expr.span(), msg);
        Err(())
    }
}

fn check_enum_variant_without_args(
    ck: &mut TypeCheck,
    expr_ast_id: ast::AstId,
    expr_span: Span,
    _expected_ty: SourceType,
    enum_id: EnumDefinitionId,
    type_params: SourceTypeArray,
    name: String,
) -> SourceType {
    let enum_ = ck.sa.enum_(enum_id);

    if !enum_accessible_from(ck.sa, enum_id, ck.module_id) {
        let msg = ErrorMessage::NotAccessible;
        ck.sa.report(ck.file_id, expr_span, msg);
    }

    let type_params_ok = check_type_params(
        ck.sa,
        ck.element,
        ck.type_param_definition,
        enum_,
        &type_params,
        ck.file_id,
        expr_span,
        |ty| specialize_type(ck.sa, ty, &type_params),
    );

    let interned_name = ck.sa.interner.intern(&name);

    if let Some(&value) = enum_.name_to_value().get(&interned_name) {
        let variant_id = enum_.variant_id_at(value as usize);
        let variant = ck.sa.variant(variant_id);

        if !variant.field_ids().is_empty() {
            let msg = ErrorMessage::EnumVariantMissingArguments;
            ck.sa.report(ck.file_id, expr_span, msg);
        }

        ck.analysis.map_idents.insert(
            expr_ast_id,
            IdentType::EnumVariant(enum_id, type_params.clone(), value),
        );
    } else {
        ck.sa.report(
            ck.file_id,
            expr_span,
            ErrorMessage::UnknownEnumVariant(name),
        );
    }

    if type_params_ok {
        let ty = SourceType::Enum(enum_id, type_params);

        ck.analysis.set_ty(expr_ast_id, ty.clone());
        ty
    } else {
        ck.analysis.set_ty(expr_ast_id, ty_error());
        ty_error()
    }
}

pub(super) fn check_expr_type_param(
    ck: &mut TypeCheck,
    expr_id: ast::AstId,
    e: &ast::TypedExpr,
    expected_ty: SourceType,
) -> SourceType {
    let type_params: Vec<SourceType> = e.args.iter().map(|&p| ck.read_type(p)).collect();
    let type_params: SourceTypeArray = SourceTypeArray::with(type_params);

    if let Some(ident) = ck.node(e.callee).to_ident() {
        let sym = ck.symtable.get_string(ck.sa, &ident.name);

        match sym {
            Some(SymbolKind::EnumVariant(enum_id, variant_idx)) => {
                check_enum_variant_without_args_id(
                    ck,
                    expr_id,
                    e.op_span,
                    expected_ty,
                    enum_id,
                    type_params,
                    variant_idx,
                )
            }

            _ => {
                ck.sa
                    .report(ck.file_id, e.op_span, ErrorMessage::NoTypeParamsExpected);

                ck.analysis.set_ty(expr_id, ty_error());
                ty_error()
            }
        }
    } else if let Some(path) = ck.node(e.callee).to_path() {
        let container_name = if let Some(container_expr) = ck.node(path.lhs).to_ident() {
            container_expr.name.clone()
        } else {
            let msg = ErrorMessage::ExpectedSomeIdentifier;
            ck.sa.report(ck.file_id, ck.span(path.lhs), msg);

            ck.analysis.set_ty(expr_id, ty_error());
            return ty_error();
        };

        let method_name = if let Some(ident) = ck.node(path.rhs).to_ident() {
            ident.name.clone()
        } else {
            let msg = ErrorMessage::ExpectedSomeIdentifier;
            ck.sa.report(ck.file_id, ck.span(path.rhs), msg);

            ck.analysis.set_ty(expr_id, ty_error());
            return ty_error();
        };

        let sym = ck.symtable.get_string(ck.sa, &container_name);

        match sym {
            Some(SymbolKind::Enum(enum_id)) => check_enum_variant_without_args(
                ck,
                expr_id,
                e.op_span,
                expected_ty,
                enum_id,
                type_params,
                method_name,
            ),

            _ => {
                let msg = ErrorMessage::NoTypeParamsExpected;
                ck.sa.report(ck.file_id, e.op_span, msg);

                ck.analysis.set_ty(expr_id, ty_error());
                ty_error()
            }
        }
    } else {
        ck.sa
            .report(ck.file_id, e.op_span, ErrorMessage::NoTypeParamsExpected);
        ck.analysis.set_ty(expr_id, ty_error());
        return ty_error();
    }
}

pub(super) fn check_enum_variant_without_args_id(
    ck: &mut TypeCheck,
    expr_ast_id: ast::AstId,
    expr_span: Span,
    expected_ty: SourceType,
    enum_id: EnumDefinitionId,
    type_params: SourceTypeArray,
    variant_idx: u32,
) -> SourceType {
    let enum_ = ck.sa.enum_(enum_id);

    if !enum_accessible_from(ck.sa, enum_id, ck.module_id) {
        let msg = ErrorMessage::NotAccessible;
        ck.sa.report(ck.file_id, expr_span, msg);
    }

    let type_params = if expected_ty.enum_id() == Some(enum_id) && type_params.is_empty() {
        expected_ty.type_params()
    } else {
        type_params
    };

    let type_params_ok = check_type_params(
        ck.sa,
        ck.element,
        ck.type_param_definition,
        enum_,
        &type_params,
        ck.file_id,
        expr_span,
        |ty| specialize_type(ck.sa, ty, &type_params),
    );

    let variant_id = enum_.variant_id_at(variant_idx as usize);
    let variant = ck.sa.variant(variant_id);

    if !variant.field_ids().is_empty() {
        let msg = ErrorMessage::EnumVariantMissingArguments;
        ck.sa.report(ck.file_id, expr_span, msg);
    }

    ck.analysis.map_idents.insert(
        expr_ast_id,
        IdentType::EnumVariant(enum_id, type_params.clone(), variant_idx),
    );

    if type_params_ok {
        let ty = SourceType::Enum(enum_id, type_params);

        ck.analysis.set_ty(expr_ast_id, ty.clone());
        ty
    } else {
        ck.analysis.set_ty(expr_ast_id, ty_error());
        ty_error()
    }
}

fn check_expr_path_module(
    ck: &mut TypeCheck,
    expr_id: ast::AstId,
    e: &ast::Path,
    expected_ty: SourceType,
    module_id: ModuleDefinitionId,
    element_name: String,
) -> SourceType {
    let interned_element_name = ck.sa.interner.intern(&element_name);

    let table = ck.sa.module_table(module_id);
    let sym = table.get(interned_element_name);

    match sym {
        Some(SymbolKind::Global(global_id)) => {
            if !global_accessible_from(ck.sa, global_id, ck.module_id) {
                let msg = ErrorMessage::NotAccessible;
                ck.sa.report(ck.file_id, e.op_span, msg);
            }

            let global_var = ck.sa.global(global_id);
            let ty = global_var.ty();
            ck.analysis.set_ty(expr_id, ty.clone());

            ck.analysis
                .map_idents
                .insert(expr_id, IdentType::Global(global_id));

            ty
        }

        Some(SymbolKind::Const(const_id)) => {
            if !const_accessible_from(ck.sa, const_id, ck.module_id) {
                let msg = ErrorMessage::NotAccessible;
                ck.sa.report(ck.file_id, e.op_span, msg);
            }

            let const_ = ck.sa.const_(const_id);
            ck.analysis.set_ty(expr_id, const_.ty());

            ck.analysis
                .map_idents
                .insert(expr_id, IdentType::Const(const_id));

            const_.ty()
        }

        Some(SymbolKind::EnumVariant(enum_id, variant_idx)) => check_enum_variant_without_args_id(
            ck,
            expr_id,
            e.op_span,
            expected_ty,
            enum_id,
            SourceTypeArray::empty(),
            variant_idx,
        ),

        None => {
            let module = ck.sa.module(module_id).name(ck.sa);
            ck.sa.report(
                ck.file_id,
                e.span,
                ErrorMessage::UnknownIdentifierInModule(module, element_name),
            );
            ty_error()
        }

        _ => {
            ck.sa
                .report(ck.file_id, e.span, ErrorMessage::ValueExpected);
            ty_error()
        }
    }
}

pub(super) fn check_type(
    ck: &mut TypeCheck,
    e: &ast::Bin,
    op: ast::BinOp,
    lhs_type: SourceType,
    rhs_type: SourceType,
    expected_type: SourceType,
) {
    if !expected_type.allows(ck.sa, lhs_type.clone())
        || !expected_type.allows(ck.sa, rhs_type.clone())
    {
        let op = op.as_str().into();
        let lhs_type = ck.ty_name(&lhs_type);
        let rhs_type = ck.ty_name(&rhs_type);
        let msg = ErrorMessage::BinOpType(op, lhs_type, rhs_type);

        ck.sa.report(ck.file_id, e.span, msg);
    }
}

pub(super) fn read_path(ck: &mut TypeCheck, path_id: ast::AstId) -> Result<SymbolKind, ()> {
    let path = ck.node(path_id).as_path_data();
    let names = &path.segments;
    let first_segment = ck.node(names[0]).as_ident();
    let mut sym = ck.symtable.get_string(ck.sa, &first_segment.name);

    for &segment_id in &names[1..] {
        match sym {
            Some(SymbolKind::Module(module_id)) => {
                if !module_accessible_from(ck.sa, module_id, ck.module_id) {
                    let msg = ErrorMessage::NotAccessible;
                    ck.sa.report(ck.file_id, path.span, msg);
                }

                let current_segment = ck.node(segment_id).as_ident();
                let iname = ck.sa.interner.intern(&current_segment.name);
                sym = ck.sa.module_table(module_id).get(iname);
            }

            Some(SymbolKind::Enum(enum_id)) => {
                let enum_ = ck.sa.enum_(enum_id);

                if !enum_accessible_from(ck.sa, enum_id, ck.module_id) {
                    let msg = ErrorMessage::NotAccessible;
                    ck.sa.report(ck.file_id, path.span, msg);
                }

                let current_segment = ck.node(segment_id).as_ident();

                let iname = ck.sa.interner.intern(&current_segment.name);

                if let Some(&variant_idx) = enum_.name_to_value().get(&iname) {
                    sym = Some(SymbolKind::EnumVariant(enum_id, variant_idx));
                } else {
                    let name = current_segment.name.clone();
                    ck.sa.report(
                        ck.file_id.into(),
                        path.span,
                        ErrorMessage::UnknownEnumVariant(name),
                    );
                    return Err(());
                }
            }

            Some(_) => {
                let msg = ErrorMessage::ExpectedModule;
                ck.sa.report(ck.file_id, path.span, msg);
                return Err(());
            }

            None => {
                let current_segment = ck.node(segment_id).as_ident();
                let name = current_segment.name.clone();
                let msg = ErrorMessage::UnknownIdentifier(name);
                ck.sa.report(ck.file_id, path.span, msg);
                return Err(());
            }
        }
    }

    if let Some(sym) = sym {
        Ok(sym)
    } else {
        let name = first_segment.name.clone();
        let msg = ErrorMessage::UnknownIdentifier(name);
        ck.sa.report(ck.file_id, path.span, msg);

        Err(())
    }
}
