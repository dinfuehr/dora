use std::collections::HashMap;

use dora_parser::ast::SyntaxNodeBase;
use dora_parser::{Span, ast};

use crate::access::{
    class_accessible_from, enum_accessible_from, is_default_accessible, struct_accessible_from,
};
use crate::error::msg::ErrorMessage;
use crate::sema::{
    ClassDefinitionId, ConstValue, ElementWithFields, EnumDefinitionId, IdentType,
    StructDefinitionId, VarId,
};
use crate::ty::SourceType;
use crate::typeck::{
    TypeCheck, add_local, check_expr, check_lit_char, check_lit_str, compute_lit_float,
    compute_lit_int, get_subpatterns, read_path,
};
use crate::{Name, SourceTypeArray, SymbolKind, specialize_type, ty};

pub(super) fn check_stmt(ck: &mut TypeCheck, node: ast::AstStmt) {
    match node {
        ast::AstStmt::Let(stmt) => check_stmt_let(ck, stmt),

        ast::AstStmt::ExprStmt(stmt) => {
            check_expr(ck, stmt.expr(), SourceType::Any);
        }

        ast::AstStmt::Error(_) => {}
    }
}

fn check_stmt_let(ck: &mut TypeCheck, s: ast::AstLet) {
    let defined_type = if let Some(data_type) = s.data_type() {
        ck.read_type(ck.file_id, data_type)
    } else {
        SourceType::Any
    };

    let expr_type = s
        .expr()
        .map(|expr| check_expr(ck, expr, defined_type.clone()))
        .unwrap_or(SourceType::Any);

    let defined_type = if s.data_type().is_some() {
        defined_type
    } else {
        expr_type.clone()
    };

    if !defined_type.is_error() && !defined_type.is_defined_type(ck.sa) {
        ck.sa
            .report(ck.file_id, s.span(), ErrorMessage::VarNeedsTypeOrExpression);

        return;
    }

    // update type of variable, necessary when stmt has initializer expression but no type
    let pattern = s.pattern();
    check_pattern(ck, pattern, defined_type.clone());

    if s.expr().is_some() {
        if !expr_type.is_error()
            && !defined_type.is_error()
            && !defined_type.allows(ck.sa, expr_type.clone())
        {
            let defined_type = ck.ty_name(&defined_type);
            let expr_type = ck.ty_name(&expr_type);
            let msg = ErrorMessage::AssignType(defined_type, expr_type);
            ck.sa.report(ck.file_id, s.span(), msg);
        }

    // let variable binding needs to be assigned
    } else {
        ck.sa
            .report(ck.file_id, s.span(), ErrorMessage::LetMissingInitialization);
    }
}

#[derive(Debug, Clone)]
pub struct BindingData {
    pub var_id: VarId,
    pub ty: SourceType,
    pub span: Span,
}

struct Context {
    alt_bindings: HashMap<Name, BindingData>,
    current: HashMap<Name, BindingData>,
}

pub(super) fn check_pattern(
    ck: &mut TypeCheck,
    pattern: ast::AstPattern,
    ty: SourceType,
) -> HashMap<Name, BindingData> {
    let mut ctxt = Context {
        alt_bindings: HashMap::new(),
        current: HashMap::new(),
    };
    check_pattern_inner(ck, &mut ctxt, pattern, ty);

    ctxt.current
}

pub(super) fn check_pattern_opt(
    ck: &mut TypeCheck,
    pattern: Option<ast::AstPattern>,
    ty: SourceType,
) -> HashMap<Name, BindingData> {
    if let Some(pattern) = pattern {
        check_pattern(ck, pattern, ty)
    } else {
        HashMap::new()
    }
}

fn check_pattern_inner(
    ck: &mut TypeCheck,
    ctxt: &mut Context,
    pattern: ast::AstPattern,
    ty: SourceType,
) {
    match pattern.clone() {
        ast::AstPattern::IdentPattern(ident) => {
            let ident_node = ident.name();
            let sym = ck.symtable.get_string(ck.sa, ident_node.text());

            match sym {
                Some(SymbolKind::EnumVariant(enum_id, variant_id)) => {
                    check_pattern_enum(ck, ctxt, pattern, ty, enum_id, variant_id);
                }

                Some(SymbolKind::Class(cls_id)) => {
                    check_pattern_class(ck, ctxt, pattern, ty, cls_id);
                }

                Some(SymbolKind::Struct(struct_id)) => {
                    check_pattern_struct(ck, ctxt, pattern, ty, struct_id);
                }

                _ => {
                    check_pattern_var(ck, ctxt, ident, ty);
                }
            }
        }

        ast::AstPattern::LitPatternBool(..) => {
            check_literal_ty(ck, pattern.span(), SourceType::Bool, ty);
        }

        ast::AstPattern::LitPatternChar(p) => {
            let value = check_lit_char(ck.sa, ck.file_id, p.expr().as_lit_char());
            ck.analysis
                .set_const_value(pattern.id(), ConstValue::Char(value));

            check_literal_ty(ck, pattern.span(), SourceType::Char, ty);
        }

        ast::AstPattern::LitPatternInt(p) => {
            let (value_ty, value) = compute_lit_int(ck.sa, ck.file_id, p.expr(), ty.clone());
            ck.analysis.set_const_value(pattern.id(), value);

            ck.analysis.set_ty(pattern.id(), value_ty.clone());
            check_literal_ty(ck, pattern.span(), value_ty, ty);
        }

        ast::AstPattern::LitPatternStr(p) => {
            let value = check_lit_str(ck.sa, ck.file_id, p.expr().as_lit_str());
            ck.analysis
                .set_const_value(pattern.id(), ConstValue::String(value));

            let str_ty = SourceType::Class(ck.sa.known.classes.string(), SourceTypeArray::empty());
            check_literal_ty(ck, pattern.span(), str_ty, ty);
        }

        ast::AstPattern::LitPatternFloat(p) => {
            let (value_ty, value) = compute_lit_float(ck.sa, ck.file_id, p.expr());
            ck.analysis
                .set_const_value(pattern.id(), ConstValue::Float(value));

            ck.analysis.set_ty(pattern.id(), value_ty.clone());
            check_literal_ty(ck, pattern.span(), value_ty, ty);
        }

        ast::AstPattern::UnderscorePattern(..) | ast::AstPattern::Error(..) => {
            // nothing to do
        }

        ast::AstPattern::Alt(p) => {
            let mut bindings_per_alt: Vec<HashMap<Name, BindingData>> =
                Vec::with_capacity(p.alts().count());
            let mut all_bindings = HashMap::new();

            for alt in p.alts() {
                let mut alt_ctxt = Context {
                    alt_bindings: all_bindings,
                    current: ctxt.current.clone(),
                };

                check_pattern_inner(ck, &mut alt_ctxt, alt, ty.clone());
                all_bindings = alt_ctxt.alt_bindings;

                let mut local_bindings = HashMap::new();

                for (name, data) in &alt_ctxt.current {
                    if !ctxt.current.contains_key(name) {
                        local_bindings.insert(*name, data.clone());
                        all_bindings.entry(*name).or_insert_with(|| data.clone());
                    }
                }

                bindings_per_alt.push(local_bindings);
            }

            for (name, data) in all_bindings {
                let mut defined_in_all_alternatives = true;

                for local_bindings in &bindings_per_alt {
                    if let Some(local_data) = local_bindings.get(&name) {
                        if !data.ty.allows(ck.sa, local_data.ty.clone())
                            && !local_data.ty.is_error()
                        {
                            let ty = local_data.ty.name(ck.sa);
                            let expected_ty = data.ty.name(ck.sa);
                            let msg = ErrorMessage::PatternBindingWrongType(ty, expected_ty);
                            ck.sa.report(ck.file_id, local_data.span, msg);
                        }
                    } else {
                        defined_in_all_alternatives = false;
                    }
                }

                if !defined_in_all_alternatives {
                    let name = ck.sa.interner.str(name).to_string();
                    let msg = ErrorMessage::PatternBindingNotDefinedInAllAlternatives(name);
                    ck.sa.report(ck.file_id, data.span, msg);
                }

                assert!(ctxt.current.insert(name, data).is_none());
            }
        }

        ast::AstPattern::CtorPattern(p) => {
            let sym = read_path(ck, p.path());

            match sym {
                Ok(SymbolKind::EnumVariant(enum_id, variant_id)) => {
                    check_pattern_enum(ck, ctxt, pattern, ty, enum_id, variant_id);
                }

                Ok(SymbolKind::Class(cls_id)) => {
                    check_pattern_class(ck, ctxt, pattern, ty, cls_id);
                }

                Ok(SymbolKind::Struct(struct_id)) => {
                    check_pattern_struct(ck, ctxt, pattern, ty, struct_id);
                }

                Ok(..) => {
                    let msg = ErrorMessage::EnumVariantExpected;
                    ck.sa.report(ck.file_id, p.path().span(), msg);
                }

                Err(..) => {}
            }
        }

        ast::AstPattern::TuplePattern(p) => {
            check_pattern_tuple(ck, ctxt, p, ty);
        }

        ast::AstPattern::Rest(p) => {
            let msg = ErrorMessage::PatternUnexpectedRest;
            ck.sa.report(ck.file_id, p.span(), msg);
        }
    }
}

fn check_literal_ty(ck: &mut TypeCheck, span: Span, ty: SourceType, expected_ty: SourceType) {
    if !expected_ty.allows(ck.sa, ty.clone()) && !ty.is_error() {
        let expected_ty = expected_ty.name(ck.sa);
        let ty_name = ck.ty_name(&ty);
        ck.sa.report(
            ck.file_id,
            span,
            ErrorMessage::WrongType(expected_ty, ty_name),
        );
    }
}

fn check_pattern_enum(
    ck: &mut TypeCheck,
    ctxt: &mut Context,
    pattern: ast::AstPattern,
    ty: SourceType,
    enum_id: EnumDefinitionId,
    variant_index: u32,
) {
    let enum_ = ck.sa.enum_(enum_id);
    let variant_id = enum_.variant_id_at(variant_index as usize);

    let params = get_subpatterns(pattern.clone());
    let given_params = params.as_ref().map(|p| p.items().count()).unwrap_or(0);

    if !enum_accessible_from(ck.sa, enum_id, ck.module_id) {
        let msg = ErrorMessage::NotAccessible;
        ck.sa.report(ck.file_id, pattern.span(), msg);
    }

    if Some(enum_id) == ty.enum_id() {
        let value_type_params = ty.type_params();

        ck.analysis.insert_ident(
            pattern.id(),
            IdentType::EnumVariant(enum_id, value_type_params.clone(), variant_index),
        );

        if params.is_some() && given_params == 0 {
            let msg = ErrorMessage::PatternNoParens;
            ck.sa.report(ck.file_id, pattern.span(), msg);
        }

        let variant = ck.sa.variant(variant_id);

        if variant.field_name_style.is_named() {
            check_subpatterns_named(ck, ctxt, pattern.clone(), variant, &value_type_params);
        } else {
            let expected_types = variant
                .field_ids()
                .iter()
                .map(|&field_id| {
                    let field = ck.sa.field(field_id);
                    specialize_type(ck.sa, field.ty(), &value_type_params)
                })
                .collect::<Vec<_>>();
            check_subpatterns(ck, ctxt, pattern.clone(), &expected_types);
        }
    } else {
        if !ty.is_error() {
            let ty = ty.name(ck.sa);
            let msg = ErrorMessage::PatternTypeMismatch(ty);
            ck.sa.report(ck.file_id, pattern.span(), msg);
        }

        check_subpatterns_error(ck, ctxt, pattern);
    }
}

fn check_pattern_tuple(
    ck: &mut TypeCheck,
    ctxt: &mut Context,
    pattern: ast::AstTuplePattern,
    ty: SourceType,
) {
    let subpatterns = pattern.params();

    if !ty.is_tuple_or_unit() {
        if !ty.is_error() {
            let ty_name = ck.ty_name(&ty);
            ck.sa.report(
                ck.file_id,
                pattern.span(),
                ErrorMessage::PatternTupleExpected(ty_name),
            );
        }

        for subpattern in subpatterns {
            if !subpattern.is_rest() {
                check_pattern_inner(ck, ctxt, subpattern, ty::error());
            }
        }
        return;
    }

    let subtypes = if ty.is_unit() {
        SourceTypeArray::empty()
    } else {
        ty.tuple_subtypes().expect("tuple expected")
    };

    let mut idx = 0;
    let mut rest_seen = false;
    let mut pattern_count: usize = 0;

    let expected_types = subtypes.types();

    for subpattern in subpatterns {
        if subpattern.is_rest() {
            if rest_seen {
                let msg = ErrorMessage::PatternMultipleRest;
                ck.sa.report(ck.file_id, subpattern.span(), msg);
            } else {
                let subpatterns_vec: Vec<_> = pattern.params().collect();
                idx += expected_types
                    .len()
                    .checked_sub(subpatterns_vec.len() - 1)
                    .unwrap_or(0);
                rest_seen = true;
            }
        } else {
            let ty = expected_types.get(idx).cloned().unwrap_or(ty::error());
            let subpattern_id = subpattern.id();
            ck.analysis.insert_field_id(subpattern_id, idx);
            check_pattern_inner(ck, ctxt, subpattern, ty);
            idx += 1;
            pattern_count += 1;
        }
    }

    let subpatterns_vec: Vec<_> = pattern.params().collect();
    if rest_seen {
        if pattern_count > expected_types.len() {
            let msg = ErrorMessage::PatternWrongNumberOfParams(pattern_count, expected_types.len());
            ck.sa.report(ck.file_id, pattern.span(), msg);
        }
    } else {
        if expected_types.len() != pattern_count {
            let msg = ErrorMessage::PatternWrongNumberOfParams(
                subpatterns_vec.len(),
                expected_types.len(),
            );
            ck.sa.report(ck.file_id, pattern.span(), msg);
        }
    }
}

fn check_pattern_class(
    ck: &mut TypeCheck,
    ctxt: &mut Context,
    pattern: ast::AstPattern,
    ty: SourceType,
    cls_id: ClassDefinitionId,
) {
    let cls = ck.sa.class(cls_id);

    if !class_accessible_from(ck.sa, cls_id, ck.module_id) {
        let msg = ErrorMessage::NotAccessible;
        ck.sa.report(ck.file_id, pattern.span(), msg);
    } else if !is_default_accessible(ck.sa, cls.module_id, ck.module_id)
        && !cls.all_fields_are_public(ck.sa)
    {
        let msg = ErrorMessage::ClassConstructorNotAccessible(cls.name(ck.sa));
        ck.sa.report(ck.file_id, pattern.span(), msg);
    }

    if Some(cls_id) == ty.cls_id() {
        let value_type_params = ty.type_params();

        ck.analysis.insert_ident(
            pattern.id(),
            IdentType::Class(cls_id, value_type_params.clone()),
        );

        if cls.field_name_style.is_named() {
            check_subpatterns_named(ck, ctxt, pattern.clone(), cls, &value_type_params);
        } else {
            let expected_types = cls
                .field_ids()
                .iter()
                .map(|&field_id| {
                    let field = ck.sa.field(field_id);
                    specialize_type(ck.sa, field.ty(), &value_type_params)
                })
                .collect::<Vec<_>>();
            check_subpatterns(ck, ctxt, pattern.clone(), &expected_types);
        }
    } else {
        if !ty.is_error() {
            let ty = ty.name(ck.sa);
            let msg = ErrorMessage::PatternTypeMismatch(ty);
            ck.sa.report(ck.file_id, pattern.span(), msg);
        }

        check_subpatterns_error(ck, ctxt, pattern);
    }
}

fn check_pattern_struct(
    ck: &mut TypeCheck,
    ctxt: &mut Context,
    pattern: ast::AstPattern,
    ty: SourceType,
    struct_id: StructDefinitionId,
) {
    let struct_ = ck.sa.struct_(struct_id);

    if !struct_accessible_from(ck.sa, struct_id, ck.module_id) {
        let msg = ErrorMessage::NotAccessible;
        ck.sa.report(ck.file_id, pattern.span(), msg);
    } else if !is_default_accessible(ck.sa, struct_.module_id, ck.module_id)
        && !struct_.all_fields_are_public(ck.sa)
    {
        let msg = ErrorMessage::StructConstructorNotAccessible(struct_.name(ck.sa));
        ck.sa.report(ck.file_id, pattern.span(), msg);
    }

    if Some(struct_id) == ty.struct_id() {
        let value_type_params = ty.type_params();

        ck.analysis.insert_ident(
            pattern.id(),
            IdentType::Struct(struct_id, value_type_params.clone()),
        );

        if struct_.field_name_style.is_named() {
            check_subpatterns_named(ck, ctxt, pattern.clone(), struct_, &value_type_params);
        } else {
            let expected_types = struct_
                .field_ids()
                .iter()
                .map(|&field_id| {
                    let field = ck.sa.field(field_id);
                    specialize_type(ck.sa, field.ty(), &value_type_params)
                })
                .collect::<Vec<_>>();

            check_subpatterns(ck, ctxt, pattern.clone(), &expected_types);
        }
    } else {
        if !ty.is_error() {
            let ty = ty.name(ck.sa);
            let msg = ErrorMessage::PatternTypeMismatch(ty);
            ck.sa.report(ck.file_id, pattern.span(), msg);
        }

        check_subpatterns_error(ck, ctxt, pattern);
    }
}

fn check_subpatterns_named<'a>(
    ck: &mut TypeCheck,
    ctxt: &mut Context,
    pattern: ast::AstPattern,
    element: &dyn ElementWithFields,
    element_type_params: &SourceTypeArray,
) {
    let params = match pattern.clone() {
        ast::AstPattern::CtorPattern(p) => {
            if let Some(ctor_field_list) = p.param_list() {
                Some(ctor_field_list.items().collect::<Vec<_>>())
            } else {
                None
            }
        }
        ast::AstPattern::IdentPattern(..) => None,
        _ => unreachable!(),
    };

    if let Some(params) = params {
        let mut used_names = HashMap::new();
        let mut rest_seen = false;

        let mut add_field = |idx: usize, name: Name, param: &ast::AstCtorField| {
            if used_names.contains_key(&name) {
                let msg = ErrorMessage::DuplicateNamedArgument;
                ck.sa.report(ck.file_id, param.span(), msg);
            } else {
                assert!(used_names.insert(name, idx).is_none());
            }
        };

        for (idx, ctor_field) in params.iter().enumerate() {
            if let Some(ident_node) = ctor_field.ident() {
                let name = ck.sa.interner.intern(ident_node.text());
                add_field(idx, name, ctor_field);
            } else if ctor_field.pattern().is_some_and(|p| p.is_ident_pattern()) {
                let ident = ctor_field.pattern().unwrap().as_ident_pattern();
                let ident = ident.name();
                let name = ck.sa.interner.intern(ident.text());
                add_field(idx, name, ctor_field);
            } else if ctor_field.pattern().is_some_and(|p| p.is_rest()) {
                rest_seen = true;
                if idx + 1 != params.len() {
                    let msg = ErrorMessage::PatternRestShouldBeLast;
                    ck.sa.report(ck.file_id, ctor_field.span(), msg);
                }
            } else {
                let msg = ErrorMessage::ExpectedNamedPattern;
                ck.sa.report(ck.file_id, ctor_field.span(), msg);
            }
        }

        for &field_id in element.field_ids() {
            let field = ck.sa.field(field_id);
            if let Some(name) = field.name {
                if let Some(idx) = used_names.remove(&name) {
                    let ctor_field = params[idx].clone();
                    ck.analysis
                        .insert_field_id(ctor_field.id(), field.index.to_usize());
                    let ty = specialize_type(ck.sa, field.ty(), element_type_params);
                    if let Some(field_pattern) = ctor_field.pattern() {
                        check_pattern_inner(ck, ctxt, field_pattern, ty);
                    }
                } else if !rest_seen {
                    let name = ck.sa.interner.str(name).to_string();
                    let msg = ErrorMessage::MissingNamedArgument(name);
                    ck.sa.report(ck.file_id, pattern.span(), msg);
                }
            }
        }

        for (_name, idx) in used_names {
            ck.sa.report(
                ck.file_id,
                params[idx].span(),
                ErrorMessage::UnexpectedNamedArgument,
            );
        }
    } else {
        let fields = element.field_ids().len();
        assert!(fields > 0);
        let msg = ErrorMessage::PatternWrongNumberOfParams(0, fields);
        ck.sa.report(ck.file_id, pattern.span(), msg);
    }
}

fn check_subpatterns<'a>(
    ck: &mut TypeCheck,
    ctxt: &mut Context,
    pattern: ast::AstPattern,
    expected_types: &'a [SourceType],
) {
    let ctor_fields = get_subpatterns(pattern.clone());

    if let Some(ctor_fields) = ctor_fields {
        let mut idx = 0;
        let mut rest_seen = false;
        let mut pattern_count: usize = 0;

        for ctor_field in ctor_fields.items() {
            let pattern = ctor_field.pattern();

            if pattern.is_none() {
                continue;
            }

            let pattern = pattern.unwrap();

            if pattern.is_rest() {
                if rest_seen {
                    let msg = ErrorMessage::PatternMultipleRest;
                    ck.sa.report(ck.file_id, ctor_field.span(), msg);
                } else {
                    idx += expected_types
                        .len()
                        .checked_sub(ctor_fields.items().count() - 1)
                        .unwrap_or(0);
                    rest_seen = true;
                }
            } else {
                let ty = expected_types.get(idx).cloned().unwrap_or(ty::error());
                ck.analysis.insert_field_id(ctor_field.id(), idx);
                check_pattern_inner(ck, ctxt, pattern, ty);
                idx += 1;
                pattern_count += 1;
            }
        }

        if rest_seen {
            if pattern_count > expected_types.len() {
                let msg =
                    ErrorMessage::PatternWrongNumberOfParams(pattern_count, expected_types.len());
                ck.sa.report(ck.file_id, pattern.span(), msg);
            }
        } else {
            if expected_types.len() != pattern_count {
                let msg = ErrorMessage::PatternWrongNumberOfParams(
                    ctor_fields.items().count(),
                    expected_types.len(),
                );
                ck.sa.report(ck.file_id, pattern.span(), msg);
            }
        }
    } else {
        if expected_types.len() > 0 {
            let msg = ErrorMessage::PatternWrongNumberOfParams(0, expected_types.len());
            ck.sa.report(ck.file_id, pattern.span(), msg);
        }
    }
}

fn check_subpatterns_error(ck: &mut TypeCheck, ctxt: &mut Context, pattern: ast::AstPattern) {
    if let Some(ctor_fields) = get_subpatterns(pattern) {
        for ctor_field in ctor_fields.items() {
            let p = ctor_field.pattern();
            if p.clone().is_some_and(|p| !p.is_rest()) {
                check_pattern_inner(ck, ctxt, p.unwrap(), ty::error());
            }
        }
    }
}

fn check_pattern_var(
    ck: &mut TypeCheck,
    ctxt: &mut Context,
    pattern: ast::AstIdentPattern,
    ty: SourceType,
) {
    let ident = pattern.name();
    let name = ck.sa.interner.intern(ident.text());

    if ctxt.current.contains_key(&name) {
        let msg = ErrorMessage::PatternDuplicateBinding;
        ck.sa.report(ck.file_id, pattern.span(), msg);
    } else {
        let var_id = if let Some(data) = ctxt.alt_bindings.get(&name) {
            data.var_id
        } else {
            let nested_var_id = ck.vars.add_var(name, ty.clone(), pattern.mutable());
            let var_id = ck.vars.local_var_id(nested_var_id);

            add_local(
                ck.sa,
                ck.symtable,
                ck.vars,
                nested_var_id,
                ck.file_id,
                pattern.span(),
            );

            var_id
        };

        assert!(
            ctxt.current
                .insert(
                    name,
                    BindingData {
                        var_id,
                        ty,
                        span: pattern.span()
                    }
                )
                .is_none()
        );

        ck.analysis
            .insert_ident(pattern.id(), IdentType::Var(var_id));

        ck.analysis.insert_var_id(pattern.id(), var_id);
    }
}
