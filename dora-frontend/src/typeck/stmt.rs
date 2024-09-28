use std::collections::{HashMap, HashSet};

use dora_parser::ast;

use crate::access::{
    class_accessible_from, enum_accessible_from, is_default_accessible, struct_accessible_from,
};
use crate::error::msg::ErrorMessage;
use crate::sema::{
    ClassDefinitionId, ConstValue, EnumDefinitionId, IdentType, StructDefinitionId, VarId,
};
use crate::ty::SourceType;
use crate::typeck::{
    add_local, check_expr, check_lit_char, check_lit_str, compute_lit_float, compute_lit_int,
    get_subpatterns, read_path, TypeCheck,
};
use crate::{specialize_type, Name, SourceTypeArray, SymbolKind};

pub(super) fn check_stmt(ck: &mut TypeCheck, s: &ast::StmtData) {
    match *s {
        ast::StmtData::Let(ref stmt) => check_stmt_let(ck, stmt),

        ast::StmtData::Expr(ref stmt) => {
            check_expr(ck, &stmt.expr, SourceType::Any);
        }
    }
}

fn check_stmt_let(ck: &mut TypeCheck, s: &ast::StmtLetType) {
    let defined_type = if let Some(ref data_type) = s.data_type {
        ck.read_type(data_type)
    } else {
        SourceType::Any
    };

    let expr_type = s
        .expr
        .as_ref()
        .map(|expr| check_expr(ck, &expr, defined_type.clone()))
        .unwrap_or(SourceType::Any);

    let defined_type = if s.data_type.is_some() {
        defined_type
    } else {
        expr_type.clone()
    };

    if !defined_type.is_error() && !defined_type.is_defined_type(ck.sa) {
        ck.sa
            .report(ck.file_id, s.span, ErrorMessage::VarNeedsTypeOrExpression);

        return;
    }

    // update type of variable, necessary when stmt has initializer expression but no type
    check_pattern(ck, &s.pattern, defined_type.clone());

    if s.expr.is_some() {
        if !expr_type.is_error()
            && !defined_type.is_error()
            && !defined_type.allows(ck.sa, expr_type.clone())
        {
            let defined_type = ck.ty_name(&defined_type);
            let expr_type = ck.ty_name(&expr_type);
            let msg = ErrorMessage::AssignType(defined_type, expr_type);
            ck.sa.report(ck.file_id, s.span, msg);
        }

    // let variable binding needs to be assigned
    } else {
        ck.sa
            .report(ck.file_id, s.span, ErrorMessage::LetMissingInitialization);
    }
}

#[derive(Debug, Clone)]
struct BindingData {
    var_id: VarId,
    ty: SourceType,
}

#[derive(Clone)]
pub struct Bindings {
    map: HashMap<Name, BindingData>,
}

impl Bindings {
    pub fn new() -> Bindings {
        Bindings {
            map: HashMap::new(),
        }
    }

    fn insert(&mut self, name: Name, var_id: VarId, ty: SourceType) {
        let old = self.map.insert(name, BindingData { var_id, ty });
        assert!(old.is_none());
    }

    fn get(&self, name: Name) -> Option<BindingData> {
        self.map.get(&name).cloned()
    }
}

struct Context {
    alt: Bindings,
    current: HashSet<Name>,
}

pub(super) fn check_pattern(ck: &mut TypeCheck, pattern: &ast::Pattern, ty: SourceType) {
    let mut ctxt = Context {
        alt: Bindings::new(),
        current: HashSet::new(),
    };
    check_pattern_inner(ck, &mut ctxt, pattern, ty);
}

fn check_pattern_inner(
    ck: &mut TypeCheck,
    ctxt: &mut Context,
    pattern: &ast::Pattern,
    ty: SourceType,
) {
    if pattern.alts.len() == 1 {
        check_pattern_alt(ck, ctxt, pattern.alts[0].as_ref(), ty.clone());
    } else {
        let mut bindings = Bindings::new();
        let mut alt_bindings: Vec<HashSet<Name>> = Vec::with_capacity(pattern.alts.len());

        for alt in &pattern.alts {
            let mut alt_ctxt = Context {
                alt: bindings,
                current: ctxt.current.clone(),
            };

            check_pattern_alt(ck, &mut alt_ctxt, alt.as_ref(), ty.clone());
            bindings = alt_ctxt.alt;

            let new_bindings = alt_ctxt
                .current
                .difference(&ctxt.current)
                .map(|n| *n)
                .collect::<HashSet<Name>>();
            alt_bindings.push(new_bindings);
        }

        let mut all = alt_bindings.pop().expect("no element");
        let mut intersect = all.clone();

        for alt in alt_bindings {
            all = all.union(&alt).map(|n| *n).collect::<HashSet<Name>>();
            intersect = intersect
                .intersection(&alt)
                .map(|n| *n)
                .collect::<HashSet<Name>>();
        }

        for &name in all.difference(&intersect) {
            let name = ck.sa.interner.str(name).to_string();
            let msg = ErrorMessage::PatternBindingNotDefinedInAllAlternatives(name);
            ck.sa.report(ck.file_id, pattern.span, msg);
        }

        for (name, data) in bindings.map {
            let old = ctxt.alt.map.insert(name, data.clone());
            assert!(old.is_none());

            assert!(ctxt.current.insert(name));
        }
    }
}

fn check_pattern_alt(
    ck: &mut TypeCheck,
    ctxt: &mut Context,
    pattern: &ast::PatternAlt,
    ty: SourceType,
) {
    match pattern {
        ast::PatternAlt::Ident(ref ident) => {
            let sym = ck.symtable.get_string(ck.sa, &ident.name.name_as_string);

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

        ast::PatternAlt::LitBool(..) => {
            check_literal_ty(ck, pattern, SourceType::Bool, ty);
        }

        ast::PatternAlt::LitChar(ref p) => {
            let e = p.expr.to_lit_char().expect("char expected");
            let value = check_lit_char(ck.sa, ck.file_id, e);
            ck.analysis.set_const_value(p.id, ConstValue::Char(value));
            check_literal_ty(ck, pattern, SourceType::Char, ty);
        }

        ast::PatternAlt::LitInt(ref p) => {
            let (value_ty, value) = compute_lit_int(ck.sa, ck.file_id, &p.expr, ty.clone());
            ck.analysis.set_const_value(p.id, value);
            ck.analysis.set_ty(p.id, value_ty.clone());
            check_literal_ty(ck, pattern, value_ty, ty);
        }

        ast::PatternAlt::LitString(ref p) => {
            let e = p.expr.to_lit_str().expect("string expected");
            let value = check_lit_str(ck.sa, ck.file_id, e);
            ck.analysis.set_const_value(p.id, ConstValue::String(value));
            let str_ty = SourceType::Class(ck.sa.known.classes.string(), SourceTypeArray::empty());
            check_literal_ty(ck, pattern, str_ty, ty);
        }

        ast::PatternAlt::LitFloat(ref p) => {
            let (value_ty, value) = compute_lit_float(ck.sa, ck.file_id, &p.expr);
            ck.analysis.set_const_value(p.id, ConstValue::Float(value));
            ck.analysis.set_ty(p.id, value_ty.clone());
            check_literal_ty(ck, pattern, value_ty, ty);
        }

        ast::PatternAlt::Underscore(..) => {
            // nothing to do
        }

        ast::PatternAlt::ClassOrStructOrEnum(ref p) => {
            let sym = read_path(ck, &p.path);

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
                    ck.sa.report(ck.file_id, p.path.span, msg);
                }

                Err(..) => {}
            }
        }

        ast::PatternAlt::Tuple(..) => {
            check_pattern_tuple(ck, ctxt, pattern, ty);
        }

        ast::PatternAlt::Rest(ref p) => {
            let msg = ErrorMessage::PatternUnexpectedRest;
            ck.sa.report(ck.file_id, p.span, msg);
        }
    }
}

fn check_literal_ty(
    ck: &mut TypeCheck,
    pattern: &ast::PatternAlt,
    ty: SourceType,
    expected_ty: SourceType,
) {
    if !expected_ty.allows(ck.sa, ty.clone()) && !ty.is_error() {
        let expected_ty = expected_ty.name(ck.sa);
        let ty_name = ck.ty_name(&ty);
        ck.sa.report(
            ck.file_id,
            pattern.span(),
            ErrorMessage::WrongType(expected_ty, ty_name),
        );
    }
}

fn check_pattern_enum(
    ck: &mut TypeCheck,
    ctxt: &mut Context,
    pattern: &ast::PatternAlt,
    ty: SourceType,
    enum_id: EnumDefinitionId,
    variant_id: u32,
) {
    let enum_ = ck.sa.enum_(enum_id);
    let variant = &enum_.variants[variant_id as usize];

    let params = get_subpatterns(pattern);
    let given_params = params.as_ref().map(|p| p.len()).unwrap_or(0);

    if !enum_accessible_from(ck.sa, enum_id, ck.module_id) {
        let msg = ErrorMessage::NotAccessible;
        ck.sa.report(ck.file_id, pattern.span(), msg);
    }

    if Some(enum_id) == ty.enum_id() {
        let value_type_params = ty.type_params();

        ck.analysis.map_idents.insert(
            pattern.id(),
            IdentType::EnumVariant(enum_id, value_type_params.clone(), variant_id),
        );

        if params.is_some() && given_params == 0 {
            let msg = ErrorMessage::PatternNoParens;
            ck.sa.report(ck.file_id, pattern.span(), msg);
        }

        let expected_types = variant
            .parsed_types()
            .iter()
            .map(|t| specialize_type(ck.sa, t.ty(), &value_type_params))
            .collect::<Vec<_>>();
        check_subpatterns(ck, ctxt, pattern, &expected_types);
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
    pattern: &ast::PatternAlt,
    ty: SourceType,
) {
    let tuple_pattern = pattern.to_tuple().expect("tuple expected");

    if !ty.is_tuple_or_unit() {
        if !ty.is_error() {
            let ty_name = ck.ty_name(&ty);
            ck.sa.report(
                ck.file_id,
                tuple_pattern.span,
                ErrorMessage::PatternTupleExpected(ty_name),
            );
        }

        check_subpatterns_error(ck, ctxt, pattern);
        return;
    }

    let subtypes = if ty.is_unit() {
        SourceTypeArray::empty()
    } else {
        ty.tuple_subtypes().expect("tuple expected")
    };

    check_subpatterns(ck, ctxt, pattern, subtypes.types());
}

fn check_pattern_class(
    ck: &mut TypeCheck,
    ctxt: &mut Context,
    pattern: &ast::PatternAlt,
    ty: SourceType,
    cls_id: ClassDefinitionId,
) {
    let cls = ck.sa.class(cls_id);

    if !class_accessible_from(ck.sa, cls_id, ck.module_id) {
        let msg = ErrorMessage::NotAccessible;
        ck.sa.report(ck.file_id, pattern.span(), msg);
    } else if !is_default_accessible(ck.sa, cls.module_id, ck.module_id)
        && !cls.all_fields_are_public()
    {
        let msg = ErrorMessage::ClassConstructorNotAccessible(cls.name(ck.sa));
        ck.sa.report(ck.file_id, pattern.span(), msg);
    }

    if Some(cls_id) == ty.cls_id() {
        let value_type_params = ty.type_params();

        ck.analysis.map_idents.insert(
            pattern.id(),
            IdentType::Class(cls_id, value_type_params.clone()),
        );

        let expected_types = cls
            .fields
            .iter()
            .map(|f| specialize_type(ck.sa, f.ty(), &value_type_params))
            .collect::<Vec<_>>();

        check_subpatterns(ck, ctxt, pattern, &expected_types);
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
    pattern: &ast::PatternAlt,
    ty: SourceType,
    struct_id: StructDefinitionId,
) {
    let struct_ = ck.sa.struct_(struct_id);

    if !struct_accessible_from(ck.sa, struct_id, ck.module_id) {
        let msg = ErrorMessage::NotAccessible;
        ck.sa.report(ck.file_id, pattern.span(), msg);
    } else if !is_default_accessible(ck.sa, struct_.module_id, ck.module_id)
        && !struct_.all_fields_are_public()
    {
        let msg = ErrorMessage::StructConstructorNotAccessible(struct_.name(ck.sa));
        ck.sa.report(ck.file_id, pattern.span(), msg);
    }

    if Some(struct_id) == ty.struct_id() {
        let value_type_params = ty.type_params();

        ck.analysis.map_idents.insert(
            pattern.id(),
            IdentType::Struct(struct_id, value_type_params.clone()),
        );

        let expected_types = struct_
            .fields
            .iter()
            .map(|f| specialize_type(ck.sa, f.ty(), &value_type_params))
            .collect::<Vec<_>>();

        check_subpatterns(ck, ctxt, pattern, &expected_types);
    } else {
        if !ty.is_error() {
            let ty = ty.name(ck.sa);
            let msg = ErrorMessage::PatternTypeMismatch(ty);
            ck.sa.report(ck.file_id, pattern.span(), msg);
        }

        check_subpatterns_error(ck, ctxt, pattern);
    }
}

fn check_subpatterns<'a>(
    ck: &mut TypeCheck,
    ctxt: &mut Context,
    pattern: &ast::PatternAlt,
    expected_types: &'a [SourceType],
) {
    let subpatterns = get_subpatterns(pattern);

    if let Some(subpatterns) = subpatterns {
        let rest_count = subpatterns.iter().filter(|p| p.is_rest()).count();

        if rest_count == 0 {
            if subpatterns.len() != expected_types.len() {
                let msg = ErrorMessage::PatternWrongNumberOfParams(
                    subpatterns.len(),
                    expected_types.len(),
                );
                ck.sa.report(ck.file_id, pattern.span(), msg);
            }

            for (idx, subpattern) in subpatterns.iter().enumerate() {
                let ty = expected_types
                    .get(idx)
                    .cloned()
                    .unwrap_or(SourceType::Error);
                check_pattern_inner(ck, ctxt, subpattern.as_ref(), ty);
            }
        } else if rest_count == 1 {
            let pattern_count = subpatterns.len() - 1;

            if pattern_count > expected_types.len() {
                let msg =
                    ErrorMessage::PatternWrongNumberOfParams(pattern_count, expected_types.len());
                ck.sa.report(ck.file_id, pattern.span(), msg);

                check_subpatterns_error(ck, ctxt, pattern);
                return;
            }

            let rest_len = expected_types.len() - pattern_count;
            let mut idx = 0;

            for subpattern in subpatterns {
                if subpattern.is_rest() {
                    idx += rest_len;
                } else {
                    let ty = expected_types
                        .get(idx)
                        .cloned()
                        .unwrap_or(SourceType::Error);
                    check_pattern_inner(ck, ctxt, subpattern.as_ref(), ty);
                    idx += 1;
                }
            }
        } else {
            let msg = ErrorMessage::PatternMultipleRest;
            ck.sa.report(ck.file_id, pattern.span(), msg);

            check_subpatterns_error(ck, ctxt, pattern);
        }
    } else {
        if expected_types.len() > 0 {
            let msg = ErrorMessage::PatternWrongNumberOfParams(0, expected_types.len());
            ck.sa.report(ck.file_id, pattern.span(), msg);
        }
    }
}

fn check_subpatterns_error(ck: &mut TypeCheck, ctxt: &mut Context, pattern: &ast::PatternAlt) {
    if let Some(subpatterns) = get_subpatterns(pattern) {
        for subpattern in subpatterns {
            if !subpattern.is_rest() {
                check_pattern_inner(ck, ctxt, subpattern.as_ref(), SourceType::Error);
            }
        }
    }
}

fn check_pattern_var(
    ck: &mut TypeCheck,
    ctxt: &mut Context,
    pattern: &ast::PatternIdent,
    ty: SourceType,
) {
    let name = ck.sa.interner.intern(&pattern.name.name_as_string);

    if ctxt.current.contains(&name) {
        let msg = ErrorMessage::PatternDuplicateBinding;
        ck.sa.report(ck.file_id, pattern.span, msg);
    } else if let Some(data) = ctxt.alt.get(name) {
        if !data.ty.allows(ck.sa, ty.clone()) && !ty.is_error() {
            let ty = ty.name(ck.sa);
            let expected_ty = data.ty.name(ck.sa);
            let msg = ErrorMessage::PatternBindingWrongType(ty, expected_ty);
            ck.sa.report(ck.file_id, pattern.span, msg);
        }

        assert!(ctxt.current.insert(name));

        ck.analysis
            .map_idents
            .insert(pattern.id, IdentType::Var(data.var_id));
    } else {
        let nested_var_id = ck.vars.add_var(name, ty.clone(), pattern.mutable);
        let var_id = ck.vars.local_var_id(nested_var_id);

        add_local(
            ck.sa,
            ck.symtable,
            ck.vars,
            nested_var_id,
            ck.file_id,
            pattern.span,
        );

        ctxt.alt.insert(name, var_id, ty.clone());
        assert!(ctxt.current.insert(name));

        ck.analysis
            .map_idents
            .insert(pattern.id, IdentType::Var(var_id));
    }
}
