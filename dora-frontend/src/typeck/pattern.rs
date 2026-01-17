use std::collections::HashMap;

use dora_parser::Span;

use crate::access::{
    class_accessible_from, enum_accessible_from, is_default_accessible, module_accessible_from,
    struct_accessible_from,
};
use crate::args;
use crate::error::diagnostics::{
    CLASS_CONSTRUCTOR_NOT_ACCESSIBLE, DUPLICATE_NAMED_ARGUMENT, ENUM_VARIANT_EXPECTED,
    EXPECTED_MODULE, EXPECTED_NAMED_PATTERN, MISSING_NAMED_ARGUMENT, NOT_ACCESSIBLE,
    PATTERN_BINDING_NOT_DEFINED_IN_ALL_ALTERNATIVES, PATTERN_BINDING_WRONG_TYPE,
    PATTERN_DUPLICATE_BINDING, PATTERN_MULTIPLE_REST, PATTERN_NO_PARENS,
    PATTERN_REST_SHOULD_BE_LAST, PATTERN_TUPLE_EXPECTED, PATTERN_TYPE_MISMATCH,
    PATTERN_UNEXPECTED_REST, PATTERN_WRONG_NUMBER_OF_PARAMS, STRUCT_CONSTRUCTOR_NOT_ACCESSIBLE,
    UNEXPECTED_NAMED_ARGUMENT, UNKNOWN_ENUM_VARIANT, UNKNOWN_IDENTIFIER, WRONG_TYPE,
};
use crate::sema::{
    AltPattern, ClassDefinitionId, ConstValue, CtorPattern, CtorPatternField, ElementWithFields,
    EnumDefinitionId, IdentPattern, IdentType, Pattern, PatternId, StructDefinitionId,
    TuplePattern, VarId,
};
use crate::ty::SourceType;
use crate::typeck::{
    TypeCheck, add_local, check_lit_char_from_text, check_lit_float_from_text,
    check_lit_int_from_text, check_lit_str_from_text,
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
    pattern_id: PatternId,
    ty: SourceType,
) -> HashMap<Name, BindingData> {
    let mut ctxt = Context {
        alt_bindings: HashMap::new(),
        current: HashMap::new(),
    };
    check_pattern_inner(ck, &mut ctxt, pattern_id, ty);

    ctxt.current
}

fn check_pattern_inner(
    ck: &mut TypeCheck,
    ctxt: &mut Context,
    pattern_id: PatternId,
    ty: SourceType,
) {
    let pattern = ck.body.pattern(pattern_id);

    match pattern {
        Pattern::Ident(ident_pattern) => {
            let sym = ck.symtable.get(ident_pattern.name);

            match sym {
                Some(SymbolKind::EnumVariant(enum_id, variant_id)) => {
                    check_pattern_enum(ck, ctxt, pattern_id, ty, enum_id, variant_id);
                }

                Some(SymbolKind::Class(cls_id)) => {
                    check_pattern_class(ck, ctxt, pattern_id, ty, cls_id);
                }

                Some(SymbolKind::Struct(struct_id)) => {
                    check_pattern_struct(ck, ctxt, pattern_id, ty, struct_id);
                }

                _ => {
                    check_pattern_var(ck, ctxt, pattern_id, ident_pattern, ty);
                }
            }
        }

        Pattern::LitBool(..) => {
            check_literal_ty(ck, ck.pattern_span(pattern_id), SourceType::Bool, ty);
        }

        Pattern::LitChar(raw_text) => {
            let span = ck.pattern_span(pattern_id);
            let value = check_lit_char_from_text(ck.sa, ck.file_id, raw_text, span);
            ck.body.set_const_value(pattern_id, ConstValue::Char(value));

            check_literal_ty(ck, span, SourceType::Char, ty);
        }

        Pattern::LitInt(raw_text) => {
            let span = ck.pattern_span(pattern_id);
            let (negate, text) = if let Some(rest) = raw_text.strip_prefix('-') {
                (true, rest)
            } else {
                (false, raw_text.as_str())
            };
            let (value_ty, value) =
                check_lit_int_from_text(ck.sa, ck.file_id, text, span, negate, ty.clone());
            ck.body.set_const_value(pattern_id, value);
            ck.body.set_ty(pattern_id, value_ty.clone());
            check_literal_ty(ck, span, value_ty, ty);
        }

        Pattern::LitStr(raw_text) => {
            let span = ck.pattern_span(pattern_id);
            let value = check_lit_str_from_text(ck.sa, ck.file_id, raw_text, span);
            ck.body
                .set_const_value(pattern_id, ConstValue::String(value));

            let str_ty = SourceType::Class(ck.sa.known.classes.string(), SourceTypeArray::empty());
            check_literal_ty(ck, span, str_ty, ty);
        }

        Pattern::LitFloat(raw_text) => {
            let span = ck.pattern_span(pattern_id);
            let (negate, text) = if let Some(rest) = raw_text.strip_prefix('-') {
                (true, rest)
            } else {
                (false, raw_text.as_str())
            };
            let (value_ty, value) =
                check_lit_float_from_text(ck.sa, ck.file_id, text, span, negate);
            ck.body
                .set_const_value(pattern_id, ConstValue::Float(value));
            ck.body.set_ty(pattern_id, value_ty.clone());
            check_literal_ty(ck, span, value_ty, ty);
        }

        Pattern::Underscore | Pattern::Error => {
            // nothing to do
        }

        Pattern::Alt(alt_pattern) => {
            check_pattern_alt(ck, ctxt, pattern_id, alt_pattern, ty);
        }

        Pattern::Ctor(ctor_pattern) => {
            check_pattern_ctor(ck, ctxt, pattern_id, ctor_pattern, ty);
        }

        Pattern::Tuple(tuple_pattern) => {
            check_pattern_tuple(ck, ctxt, pattern_id, tuple_pattern, ty);
        }

        Pattern::Rest => {
            ck.report(
                ck.pattern_span(pattern_id),
                &PATTERN_UNEXPECTED_REST,
                args!(),
            );
        }
    }
}

fn check_pattern_alt(
    ck: &mut TypeCheck,
    ctxt: &mut Context,
    _pattern_id: PatternId,
    alt_pattern: &AltPattern,
    ty: SourceType,
) {
    let mut bindings_per_alt: Vec<HashMap<Name, BindingData>> =
        Vec::with_capacity(alt_pattern.patterns.len());
    let mut all_bindings = HashMap::new();

    for &alt_id in &alt_pattern.patterns {
        let mut alt_ctxt = Context {
            alt_bindings: all_bindings,
            current: ctxt.current.clone(),
        };

        check_pattern_inner(ck, &mut alt_ctxt, alt_id, ty.clone());
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
                if !data.ty.allows(ck.sa, local_data.ty.clone()) && !local_data.ty.is_error() {
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

fn read_path_from_names(ck: &mut TypeCheck, path: &[Name], span: Span) -> Result<SymbolKind, ()> {
    let mut names_iter = path.iter();
    let first_name = *names_iter.next().unwrap();
    let mut sym = ck.symtable.get(first_name);

    for &name in names_iter {
        match sym {
            Some(SymbolKind::Module(module_id)) => {
                if !module_accessible_from(ck.sa, module_id, ck.module_id) {
                    ck.report(span, &NOT_ACCESSIBLE, args![]);
                }
                sym = ck.sa.module_table(module_id).get(name);
            }

            Some(SymbolKind::Enum(enum_id)) => {
                let enum_ = ck.sa.enum_(enum_id);

                if !enum_accessible_from(ck.sa, enum_id, ck.module_id) {
                    ck.report(span, &NOT_ACCESSIBLE, args![]);
                }

                if let Some(&variant_idx) = enum_.name_to_value().get(&name) {
                    sym = Some(SymbolKind::EnumVariant(enum_id, variant_idx));
                } else {
                    let name = ck.sa.interner.str(name).to_string();
                    ck.report(span, &UNKNOWN_ENUM_VARIANT, args![name]);
                    return Err(());
                }
            }

            Some(_) => {
                ck.report(span, &EXPECTED_MODULE, args![]);
                return Err(());
            }

            None => {
                let name = ck.sa.interner.str(name).to_string();
                ck.report(span, &UNKNOWN_IDENTIFIER, args![name]);
                return Err(());
            }
        }
    }

    if let Some(sym) = sym {
        Ok(sym)
    } else {
        let name = ck.sa.interner.str(first_name).to_string();
        ck.report(span, &UNKNOWN_IDENTIFIER, args![name]);
        Err(())
    }
}

fn check_pattern_ctor(
    ck: &mut TypeCheck,
    ctxt: &mut Context,
    pattern_id: PatternId,
    ctor_pattern: &CtorPattern,
    ty: SourceType,
) {
    let span = ck.pattern_span(pattern_id);
    let sym = read_path_from_names(ck, &ctor_pattern.path, span);

    match sym {
        Ok(SymbolKind::EnumVariant(enum_id, variant_id)) => {
            check_pattern_enum(ck, ctxt, pattern_id, ty, enum_id, variant_id);
        }

        Ok(SymbolKind::Class(cls_id)) => {
            check_pattern_class(ck, ctxt, pattern_id, ty, cls_id);
        }

        Ok(SymbolKind::Struct(struct_id)) => {
            check_pattern_struct(ck, ctxt, pattern_id, ty, struct_id);
        }

        Ok(..) => {
            ck.report(span, &ENUM_VARIANT_EXPECTED, args!());
        }

        Err(..) => {}
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
    pattern_id: PatternId,
    ty: SourceType,
    enum_id: EnumDefinitionId,
    variant_index: u32,
) {
    let enum_ = ck.sa.enum_(enum_id);
    let variant_id = enum_.variant_id_at(variant_index as usize);

    let pattern = ck.body.pattern(pattern_id);
    let (fields, has_parens) = get_ctor_fields_and_parens(pattern);
    let given_params = fields.map(|f| f.len()).unwrap_or(0);

    if !enum_accessible_from(ck.sa, enum_id, ck.module_id) {
        ck.report(ck.pattern_span(pattern_id), &NOT_ACCESSIBLE, args!());
    }

    if Some(enum_id) == ty.enum_id() {
        let value_type_params = ty.type_params();

        ck.body.insert_ident(
            pattern_id,
            IdentType::EnumVariant(enum_id, value_type_params.clone(), variant_index),
        );

        if has_parens && given_params == 0 {
            ck.report(ck.pattern_span(pattern_id), &PATTERN_NO_PARENS, args!());
        }

        let variant = ck.sa.variant(variant_id);

        if variant.field_name_style.is_named() {
            check_subpatterns_named(ck, ctxt, pattern_id, variant, &value_type_params);
        } else {
            let expected_types = variant
                .field_ids()
                .iter()
                .map(|&field_id| {
                    let field = ck.sa.field(field_id);
                    specialize_type(ck.sa, field.ty(), &value_type_params)
                })
                .collect::<Vec<_>>();
            check_subpatterns(ck, ctxt, pattern_id, &expected_types);
        }
    } else {
        if !ty.is_error() {
            let ty = ty.name(ck.sa);
            ck.report(
                ck.pattern_span(pattern_id),
                &PATTERN_TYPE_MISMATCH,
                args!(ty),
            );
        }

        check_subpatterns_error(ck, ctxt, pattern_id);
    }
}

fn check_pattern_tuple(
    ck: &mut TypeCheck,
    ctxt: &mut Context,
    pattern_id: PatternId,
    tuple_pattern: &TuplePattern,
    ty: SourceType,
) {
    if !ty.is_tuple_or_unit() {
        if !ty.is_error() {
            let ty_name = ck.ty_name(&ty);
            ck.report(
                ck.pattern_span(pattern_id),
                &PATTERN_TUPLE_EXPECTED,
                args!(ty_name),
            );
        }

        for &subpattern_id in &tuple_pattern.patterns {
            let subpattern = ck.body.pattern(subpattern_id);
            if !matches!(subpattern, Pattern::Rest) {
                check_pattern_inner(ck, ctxt, subpattern_id, ty::error());
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
    let subpatterns = &tuple_pattern.patterns;

    for &subpattern_id in subpatterns.iter() {
        let subpattern = ck.body.pattern(subpattern_id);
        if matches!(subpattern, Pattern::Rest) {
            if rest_seen {
                ck.report(
                    ck.pattern_span(subpattern_id),
                    &PATTERN_MULTIPLE_REST,
                    args!(),
                );
            } else {
                idx += expected_types
                    .len()
                    .checked_sub(subpatterns.len() - 1)
                    .unwrap_or(0);
                rest_seen = true;
            }
        } else {
            let ty = expected_types.get(idx).cloned().unwrap_or(ty::error());
            ck.body.insert_field_id(subpattern_id, idx);
            check_pattern_inner(ck, ctxt, subpattern_id, ty);
            idx += 1;
            pattern_count += 1;
        }
    }

    if rest_seen {
        if pattern_count > expected_types.len() {
            ck.report(
                ck.pattern_span(pattern_id),
                &PATTERN_WRONG_NUMBER_OF_PARAMS,
                args!(pattern_count, expected_types.len()),
            );
        }
    } else {
        if expected_types.len() != pattern_count {
            ck.report(
                ck.pattern_span(pattern_id),
                &PATTERN_WRONG_NUMBER_OF_PARAMS,
                args!(subpatterns.len(), expected_types.len()),
            );
        }
    }
}

fn check_pattern_class(
    ck: &mut TypeCheck,
    ctxt: &mut Context,
    pattern_id: PatternId,
    ty: SourceType,
    cls_id: ClassDefinitionId,
) {
    let cls = ck.sa.class(cls_id);

    if !class_accessible_from(ck.sa, cls_id, ck.module_id) {
        ck.report(ck.pattern_span(pattern_id), &NOT_ACCESSIBLE, args!());
    } else if !is_default_accessible(ck.sa, cls.module_id, ck.module_id)
        && !cls.all_fields_are_public(ck.sa)
    {
        ck.report(
            ck.pattern_span(pattern_id),
            &CLASS_CONSTRUCTOR_NOT_ACCESSIBLE,
            args!(cls.name(ck.sa)),
        );
    }

    if Some(cls_id) == ty.cls_id() {
        let value_type_params = ty.type_params();

        ck.body.insert_ident(
            pattern_id,
            IdentType::Class(cls_id, value_type_params.clone()),
        );

        if cls.field_name_style.is_named() {
            check_subpatterns_named(ck, ctxt, pattern_id, cls, &value_type_params);
        } else {
            let expected_types = cls
                .field_ids()
                .iter()
                .map(|&field_id| {
                    let field = ck.sa.field(field_id);
                    specialize_type(ck.sa, field.ty(), &value_type_params)
                })
                .collect::<Vec<_>>();
            check_subpatterns(ck, ctxt, pattern_id, &expected_types);
        }
    } else {
        if !ty.is_error() {
            let ty = ty.name(ck.sa);
            ck.report(
                ck.pattern_span(pattern_id),
                &PATTERN_TYPE_MISMATCH,
                args!(ty),
            );
        }

        check_subpatterns_error(ck, ctxt, pattern_id);
    }
}

fn check_pattern_struct(
    ck: &mut TypeCheck,
    ctxt: &mut Context,
    pattern_id: PatternId,
    ty: SourceType,
    struct_id: StructDefinitionId,
) {
    let struct_ = ck.sa.struct_(struct_id);

    if !struct_accessible_from(ck.sa, struct_id, ck.module_id) {
        ck.report(ck.pattern_span(pattern_id), &NOT_ACCESSIBLE, args!());
    } else if !is_default_accessible(ck.sa, struct_.module_id, ck.module_id)
        && !struct_.all_fields_are_public(ck.sa)
    {
        ck.report(
            ck.pattern_span(pattern_id),
            &STRUCT_CONSTRUCTOR_NOT_ACCESSIBLE,
            args!(struct_.name(ck.sa)),
        );
    }

    if Some(struct_id) == ty.struct_id() {
        let value_type_params = ty.type_params();

        ck.body.insert_ident(
            pattern_id,
            IdentType::Struct(struct_id, value_type_params.clone()),
        );

        if struct_.field_name_style.is_named() {
            check_subpatterns_named(ck, ctxt, pattern_id, struct_, &value_type_params);
        } else {
            let expected_types = struct_
                .field_ids()
                .iter()
                .map(|&field_id| {
                    let field = ck.sa.field(field_id);
                    specialize_type(ck.sa, field.ty(), &value_type_params)
                })
                .collect::<Vec<_>>();

            check_subpatterns(ck, ctxt, pattern_id, &expected_types);
        }
    } else {
        if !ty.is_error() {
            let ty = ty.name(ck.sa);
            ck.report(
                ck.pattern_span(pattern_id),
                &PATTERN_TYPE_MISMATCH,
                args!(ty),
            );
        }

        check_subpatterns_error(ck, ctxt, pattern_id);
    }
}

fn check_subpatterns_named<'a>(
    ck: &mut TypeCheck,
    ctxt: &mut Context,
    pattern_id: PatternId,
    element: &dyn ElementWithFields,
    element_type_params: &SourceTypeArray,
) {
    let pattern = ck.body.pattern(pattern_id);
    let sema_fields = match get_ctor_fields(pattern) {
        Some(fields) => fields,
        None => {
            let fields = element.field_ids().len();
            assert!(fields > 0);
            ck.report(
                ck.pattern_span(pattern_id),
                &PATTERN_WRONG_NUMBER_OF_PARAMS,
                args!(0usize, fields),
            );
            return;
        }
    };

    let mut used_names = HashMap::new();
    let mut rest_seen = false;

    for (idx, sema_field) in sema_fields.iter().enumerate() {
        let span = sema_field
            .pattern
            .map(|id| ck.pattern_span(id))
            .unwrap_or_else(|| ck.pattern_span(pattern_id));

        if let Some(name) = sema_field.name {
            if used_names.contains_key(&name) {
                ck.report(span, &DUPLICATE_NAMED_ARGUMENT, args!());
            } else {
                assert!(used_names.insert(name, idx).is_none());
            }
        } else if let Some(subpattern_id) = sema_field.pattern {
            let subpattern = ck.body.pattern(subpattern_id);
            if let Pattern::Ident(ident) = subpattern {
                let name = ident.name;
                if used_names.contains_key(&name) {
                    ck.report(span, &DUPLICATE_NAMED_ARGUMENT, args!());
                } else {
                    assert!(used_names.insert(name, idx).is_none());
                }
            } else if matches!(subpattern, Pattern::Rest) {
                rest_seen = true;
                if idx + 1 != sema_fields.len() {
                    ck.report(span, &PATTERN_REST_SHOULD_BE_LAST, args!());
                }
            } else {
                ck.report(span, &EXPECTED_NAMED_PATTERN, args!());
            }
        } else {
            ck.report(span, &EXPECTED_NAMED_PATTERN, args!());
        }
    }

    for &field_id in element.field_ids() {
        let field = ck.sa.field(field_id);
        if let Some(name) = field.name {
            if let Some(idx) = used_names.remove(&name) {
                let ty = specialize_type(ck.sa, field.ty(), element_type_params);
                if let Some(subpattern_id) = sema_fields[idx].pattern {
                    ck.body
                        .insert_field_id(subpattern_id, field.index.to_usize());
                    check_pattern_inner(ck, ctxt, subpattern_id, ty);
                }
            } else if !rest_seen {
                let name = ck.sa.interner.str(name).to_string();
                ck.report(
                    ck.pattern_span(pattern_id),
                    &MISSING_NAMED_ARGUMENT,
                    args!(name),
                );
            }
        }
    }

    for (_name, idx) in used_names {
        let span = sema_fields[idx]
            .pattern
            .map(|id| ck.pattern_span(id))
            .unwrap_or_else(|| ck.pattern_span(pattern_id));
        ck.report(span, &UNEXPECTED_NAMED_ARGUMENT, args!());
    }
}

fn check_subpatterns<'a>(
    ck: &mut TypeCheck,
    ctxt: &mut Context,
    pattern_id: PatternId,
    expected_types: &'a [SourceType],
) {
    let pattern = ck.body.pattern(pattern_id);
    let sema_fields = match get_ctor_fields(pattern) {
        Some(fields) => fields,
        None => {
            if !expected_types.is_empty() {
                ck.report(
                    ck.pattern_span(pattern_id),
                    &PATTERN_WRONG_NUMBER_OF_PARAMS,
                    args!(0usize, expected_types.len()),
                );
            }
            return;
        }
    };

    let mut idx = 0;
    let mut rest_seen = false;
    let mut pattern_count: usize = 0;

    for sema_field in sema_fields.iter() {
        let subpattern_id = match sema_field.pattern {
            Some(id) => id,
            None => continue,
        };

        let subpattern = ck.body.pattern(subpattern_id);

        if matches!(subpattern, Pattern::Rest) {
            if rest_seen {
                ck.report(
                    ck.pattern_span(subpattern_id),
                    &PATTERN_MULTIPLE_REST,
                    args!(),
                );
            } else {
                idx += expected_types
                    .len()
                    .checked_sub(sema_fields.len() - 1)
                    .unwrap_or(0);
                rest_seen = true;
            }
        } else {
            let ty = expected_types.get(idx).cloned().unwrap_or(ty::error());
            ck.body.insert_field_id(subpattern_id, idx);
            check_pattern_inner(ck, ctxt, subpattern_id, ty);
            idx += 1;
            pattern_count += 1;
        }
    }

    if rest_seen {
        if pattern_count > expected_types.len() {
            ck.report(
                ck.pattern_span(pattern_id),
                &PATTERN_WRONG_NUMBER_OF_PARAMS,
                args!(pattern_count, expected_types.len()),
            );
        }
    } else if expected_types.len() != pattern_count {
        ck.report(
            ck.pattern_span(pattern_id),
            &PATTERN_WRONG_NUMBER_OF_PARAMS,
            args!(sema_fields.len(), expected_types.len()),
        );
    }
}

fn check_subpatterns_error(ck: &mut TypeCheck, ctxt: &mut Context, pattern_id: PatternId) {
    let pattern = ck.body.pattern(pattern_id);
    if let Some(sema_fields) = get_ctor_fields(pattern) {
        for sema_field in sema_fields {
            if let Some(subpattern_id) = sema_field.pattern {
                let subpattern = ck.body.pattern(subpattern_id);
                if !matches!(subpattern, Pattern::Rest) {
                    check_pattern_inner(ck, ctxt, subpattern_id, ty::error());
                }
            }
        }
    }
}

fn check_pattern_var(
    ck: &mut TypeCheck,
    ctxt: &mut Context,
    pattern_id: PatternId,
    ident_pattern: &IdentPattern,
    ty: SourceType,
) {
    let name = ident_pattern.name;

    if ctxt.current.contains_key(&name) {
        ck.report(
            ck.pattern_span(pattern_id),
            &PATTERN_DUPLICATE_BINDING,
            args!(),
        );
    } else {
        let var_id = if let Some(data) = ctxt.alt_bindings.get(&name) {
            data.var_id
        } else {
            let nested_var_id = ck.vars.add_var(name, ty.clone(), ident_pattern.mutable);
            let var_id = ck.vars.local_var_id(nested_var_id);

            add_local(
                ck.sa,
                ck.symtable,
                ck.vars,
                nested_var_id,
                ck.file_id,
                ck.pattern_span(pattern_id),
            );

            var_id
        };

        let span = ck.pattern_span(pattern_id);
        assert!(
            ctxt.current
                .insert(name, BindingData { var_id, ty, span })
                .is_none()
        );

        ck.body.insert_ident(pattern_id, IdentType::Var(var_id));
        ck.body.insert_var_id(pattern_id, var_id);
    }
}

fn get_ctor_fields(pattern: &Pattern) -> Option<&[CtorPatternField]> {
    match pattern {
        Pattern::Ctor(ctor) => Some(&ctor.fields),
        Pattern::Ident(..) => None,
        _ => None,
    }
}

fn get_ctor_fields_and_parens(pattern: &Pattern) -> (Option<&[CtorPatternField]>, bool) {
    match pattern {
        Pattern::Ctor(ctor) => (Some(&ctor.fields), ctor.has_parens),
        Pattern::Ident(..) => (None, false),
        _ => (None, false),
    }
}
