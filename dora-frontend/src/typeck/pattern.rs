use std::collections::HashMap;

use dora_parser::ast::{AstCtorFieldList, SyntaxNodeBase};
use dora_parser::{Span, ast};

use crate::access::{
    class_accessible_from, enum_accessible_from, is_default_accessible, struct_accessible_from,
};
use crate::args;
use crate::error::diagnostics::{
    CLASS_CONSTRUCTOR_NOT_ACCESSIBLE, DUPLICATE_NAMED_ARGUMENT, ENUM_VARIANT_EXPECTED,
    EXPECTED_NAMED_PATTERN, MISSING_NAMED_ARGUMENT, NOT_ACCESSIBLE,
    PATTERN_BINDING_NOT_DEFINED_IN_ALL_ALTERNATIVES, PATTERN_BINDING_WRONG_TYPE,
    PATTERN_DUPLICATE_BINDING, PATTERN_MULTIPLE_REST, PATTERN_NO_PARENS,
    PATTERN_REST_SHOULD_BE_LAST, PATTERN_TUPLE_EXPECTED, PATTERN_TYPE_MISMATCH,
    PATTERN_UNEXPECTED_REST, PATTERN_WRONG_NUMBER_OF_PARAMS, STRUCT_CONSTRUCTOR_NOT_ACCESSIBLE,
    UNEXPECTED_NAMED_ARGUMENT, WRONG_TYPE,
};
use crate::sema::{
    ClassDefinitionId, ConstValue, ElementWithFields, EnumDefinitionId, IdentType, PatternId,
    StructDefinitionId, VarId,
};
use crate::ty::SourceType;
use crate::typeck::expr::read_path;
use crate::typeck::{
    TypeCheck, add_local, check_lit_char, check_lit_str, compute_lit_float, compute_lit_int,
};
use crate::{Name, SourceTypeArray, SymbolKind, specialize_type, ty};

#[derive(Debug, Clone)]
pub(super) struct BindingData {
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

pub(super) fn check_pattern_id(
    ck: &mut TypeCheck,
    pattern_id: PatternId,
    ty: SourceType,
) -> HashMap<Name, BindingData> {
    let pattern = ck.syntax_by_pattern_id::<ast::AstPattern>(pattern_id);
    check_pattern(ck, pattern, ty)
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
            let value = check_lit_char(ck.sa, ck.file_id, p.expr().as_lit_char_expr());
            ck.body
                .set_const_value(pattern.id(), ConstValue::Char(value));

            check_literal_ty(ck, pattern.span(), SourceType::Char, ty);
        }

        ast::AstPattern::LitPatternInt(p) => {
            let (value_ty, value) = compute_lit_int(ck.sa, ck.file_id, p.expr(), ty.clone());
            ck.body.set_const_value(pattern.id(), value);

            ck.body.set_ty(pattern.id(), value_ty.clone());
            check_literal_ty(ck, pattern.span(), value_ty, ty);
        }

        ast::AstPattern::LitPatternStr(p) => {
            let value = check_lit_str(ck.sa, ck.file_id, p.expr().as_lit_str_expr());
            ck.body
                .set_const_value(pattern.id(), ConstValue::String(value));

            let str_ty = SourceType::Class(ck.sa.known.classes.string(), SourceTypeArray::empty());
            check_literal_ty(ck, pattern.span(), str_ty, ty);
        }

        ast::AstPattern::LitPatternFloat(p) => {
            let (value_ty, value) = compute_lit_float(ck.sa, ck.file_id, p.expr());
            ck.body
                .set_const_value(pattern.id(), ConstValue::Float(value));

            ck.body.set_ty(pattern.id(), value_ty.clone());
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
                            ck.report(
                                local_data.span,
                                &PATTERN_BINDING_WRONG_TYPE,
                                args!(ty, expected_ty),
                            );
                        }
                    } else {
                        defined_in_all_alternatives = false;
                    }
                }

                if !defined_in_all_alternatives {
                    let name = ck.sa.interner.str(name).to_string();
                    ck.report(
                        data.span,
                        &PATTERN_BINDING_NOT_DEFINED_IN_ALL_ALTERNATIVES,
                        args!(name),
                    );
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
                    ck.report(p.path().span(), &ENUM_VARIANT_EXPECTED, args!());
                }

                Err(..) => {}
            }
        }

        ast::AstPattern::TuplePattern(p) => {
            check_pattern_tuple(ck, ctxt, p, ty);
        }

        ast::AstPattern::Rest(p) => {
            ck.report(p.span(), &PATTERN_UNEXPECTED_REST, args!());
        }
    }
}

fn check_literal_ty(ck: &mut TypeCheck, span: Span, ty: SourceType, expected_ty: SourceType) {
    if !expected_ty.allows(ck.sa, ty.clone()) && !ty.is_error() {
        let expected_ty = expected_ty.name(ck.sa);
        let ty_name = ck.ty_name(&ty);
        ck.report(span, &WRONG_TYPE, args!(expected_ty, ty_name));
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
        ck.report(pattern.span(), &NOT_ACCESSIBLE, args!());
    }

    if Some(enum_id) == ty.enum_id() {
        let value_type_params = ty.type_params();

        ck.body.insert_ident(
            pattern.id(),
            IdentType::EnumVariant(enum_id, value_type_params.clone(), variant_index),
        );

        if params.is_some() && given_params == 0 {
            ck.report(pattern.span(), &PATTERN_NO_PARENS, args!());
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
            ck.report(pattern.span(), &PATTERN_TYPE_MISMATCH, args!(ty));
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
            ck.report(pattern.span(), &PATTERN_TUPLE_EXPECTED, args!(ty_name));
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
                ck.report(subpattern.span(), &PATTERN_MULTIPLE_REST, args!());
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
            ck.body.insert_field_id(subpattern_id, idx);
            check_pattern_inner(ck, ctxt, subpattern, ty);
            idx += 1;
            pattern_count += 1;
        }
    }

    let subpatterns_vec: Vec<_> = pattern.params().collect();
    if rest_seen {
        if pattern_count > expected_types.len() {
            ck.report(
                pattern.span(),
                &PATTERN_WRONG_NUMBER_OF_PARAMS,
                args!(pattern_count, expected_types.len()),
            );
        }
    } else {
        if expected_types.len() != pattern_count {
            ck.report(
                pattern.span(),
                &PATTERN_WRONG_NUMBER_OF_PARAMS,
                args!(subpatterns_vec.len(), expected_types.len()),
            );
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
        ck.report(pattern.span(), &NOT_ACCESSIBLE, args!());
    } else if !is_default_accessible(ck.sa, cls.module_id, ck.module_id)
        && !cls.all_fields_are_public(ck.sa)
    {
        ck.report(
            pattern.span(),
            &CLASS_CONSTRUCTOR_NOT_ACCESSIBLE,
            args!(cls.name(ck.sa)),
        );
    }

    if Some(cls_id) == ty.cls_id() {
        let value_type_params = ty.type_params();

        ck.body.insert_ident(
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
            ck.report(pattern.span(), &PATTERN_TYPE_MISMATCH, args!(ty));
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
        ck.report(pattern.span(), &NOT_ACCESSIBLE, args!());
    } else if !is_default_accessible(ck.sa, struct_.module_id, ck.module_id)
        && !struct_.all_fields_are_public(ck.sa)
    {
        ck.report(
            pattern.span(),
            &STRUCT_CONSTRUCTOR_NOT_ACCESSIBLE,
            args!(struct_.name(ck.sa)),
        );
    }

    if Some(struct_id) == ty.struct_id() {
        let value_type_params = ty.type_params();

        ck.body.insert_ident(
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
            ck.report(pattern.span(), &PATTERN_TYPE_MISMATCH, args!(ty));
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
                ck.report(param.span(), &DUPLICATE_NAMED_ARGUMENT, args!());
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
                    ck.report(ctor_field.span(), &PATTERN_REST_SHOULD_BE_LAST, args!());
                }
            } else {
                ck.report(ctor_field.span(), &EXPECTED_NAMED_PATTERN, args!());
            }
        }

        for &field_id in element.field_ids() {
            let field = ck.sa.field(field_id);
            if let Some(name) = field.name {
                if let Some(idx) = used_names.remove(&name) {
                    let ctor_field = params[idx].clone();
                    ck.body
                        .insert_field_id(ctor_field.id(), field.index.to_usize());
                    let ty = specialize_type(ck.sa, field.ty(), element_type_params);
                    if let Some(field_pattern) = ctor_field.pattern() {
                        check_pattern_inner(ck, ctxt, field_pattern, ty);
                    }
                } else if !rest_seen {
                    let name = ck.sa.interner.str(name).to_string();
                    ck.report(pattern.span(), &MISSING_NAMED_ARGUMENT, args!(name));
                }
            }
        }

        for (_name, idx) in used_names {
            ck.report(params[idx].span(), &UNEXPECTED_NAMED_ARGUMENT, args!());
        }
    } else {
        let fields = element.field_ids().len();
        assert!(fields > 0);
        ck.report(
            pattern.span(),
            &PATTERN_WRONG_NUMBER_OF_PARAMS,
            args!(0usize, fields),
        );
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
                    ck.report(ctor_field.span(), &PATTERN_MULTIPLE_REST, args!());
                } else {
                    idx += expected_types
                        .len()
                        .checked_sub(ctor_fields.items().count() - 1)
                        .unwrap_or(0);
                    rest_seen = true;
                }
            } else {
                let ty = expected_types.get(idx).cloned().unwrap_or(ty::error());
                ck.body.insert_field_id(ctor_field.id(), idx);
                check_pattern_inner(ck, ctxt, pattern, ty);
                idx += 1;
                pattern_count += 1;
            }
        }

        if rest_seen {
            if pattern_count > expected_types.len() {
                ck.report(
                    pattern.span(),
                    &PATTERN_WRONG_NUMBER_OF_PARAMS,
                    args!(pattern_count, expected_types.len()),
                );
            }
        } else {
            if expected_types.len() != pattern_count {
                ck.report(
                    pattern.span(),
                    &PATTERN_WRONG_NUMBER_OF_PARAMS,
                    args!(ctor_fields.items().count(), expected_types.len()),
                );
            }
        }
    } else {
        if expected_types.len() > 0 {
            ck.report(
                pattern.span(),
                &PATTERN_WRONG_NUMBER_OF_PARAMS,
                args!(0usize, expected_types.len()),
            );
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
        ck.report(pattern.span(), &PATTERN_DUPLICATE_BINDING, args!());
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

        ck.body.insert_ident(pattern.id(), IdentType::Var(var_id));

        ck.body.insert_var_id(pattern.id(), var_id);
    }
}

pub(super) fn get_subpatterns(pattern: ast::AstPattern) -> Option<AstCtorFieldList> {
    match pattern {
        ast::AstPattern::IdentPattern(..) => None,
        ast::AstPattern::CtorPattern(p) => {
            if let Some(ctor_field_list) = p.param_list() {
                Some(ctor_field_list)
            } else {
                None
            }
        }
        _ => unreachable!(),
    }
}
