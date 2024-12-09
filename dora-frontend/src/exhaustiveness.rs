use std::collections::HashMap;

use dora_parser::ast;
use dora_parser::ast::visit::{self, Visitor};
use fixedbitset::FixedBitSet;

use crate::sema::{AnalysisData, EnumDefinitionId, IdentType, Sema, SourceFileId};
use crate::ErrorMessage;

pub fn check(sa: &Sema) {
    for (_id, fct) in sa.fcts.iter() {
        if fct.has_body() {
            let mut visitor = Exhaustiveness {
                sa,
                file_id: fct.file_id,
                analysis: fct.analysis(),
                is_new_exhaustiveness: fct.is_new_exhaustiveness,
            };
            visit::walk_fct(&mut visitor, &fct.ast);
        }
    }
}

struct Exhaustiveness<'a> {
    sa: &'a Sema,
    analysis: &'a AnalysisData,
    file_id: SourceFileId,
    is_new_exhaustiveness: bool,
}

impl<'a> Visitor for Exhaustiveness<'a> {
    fn visit_expr(&mut self, e: &ast::ExprData) {
        match *e {
            ast::ExprData::Match(ref expr) => {
                if self.is_new_exhaustiveness {
                    // Improved exhaustiveness check is WIP and not enabled by default.
                    check_match2(self.sa, self.analysis, self.file_id, expr);
                } else {
                    check_match(self.sa, self.analysis, self.file_id, expr);
                }
                visit::walk_expr(self, e);
            }
            ast::ExprData::Lambda(..) => (),
            _ => {
                visit::walk_expr(self, e);
            }
        }
    }
}

fn check_match(
    sa: &Sema,
    analysis: &AnalysisData,
    file_id: SourceFileId,
    node: &ast::ExprMatchType,
) {
    let expr_type = analysis.ty(node.expr.id());
    let enum_id = expr_type.enum_id().expect("enum expected");

    let enum_ = sa.enum_(enum_id);
    let enum_variants = enum_.variants().len();

    let mut used_variants = FixedBitSet::with_capacity(enum_variants);

    for arm in &node.arms {
        if arm.cond.is_some() {
            continue;
        }

        check_pattern(sa, analysis, file_id, &arm.pattern, &mut used_variants);
    }

    used_variants.toggle_range(..);

    if used_variants.count_ones(..) != 0 {
        let msg = ErrorMessage::MatchUncoveredVariant;
        sa.report(file_id, node.expr.span(), msg);
    }
}

fn check_pattern(
    sa: &Sema,
    analysis: &AnalysisData,
    file_id: SourceFileId,
    pattern: &ast::Pattern,
    used_variants: &mut FixedBitSet,
) {
    match pattern {
        ast::Pattern::Underscore(..) => {
            if used_variants.count_zeroes(..) == 0 {
                let msg = ErrorMessage::MatchUnreachablePattern;
                sa.report(file_id, pattern.span(), msg);
            }

            used_variants.insert_range(..);
        }

        ast::Pattern::Rest(..) => unreachable!(),

        ast::Pattern::LitBool(..)
        | ast::Pattern::LitChar(..)
        | ast::Pattern::LitString(..)
        | ast::Pattern::LitInt(..)
        | ast::Pattern::LitFloat(..)
        | ast::Pattern::Tuple(..) => unreachable!(),

        ast::Pattern::Ident(ref pattern_ident) => {
            let ident = analysis
                .map_idents
                .get(pattern_ident.id)
                .expect("missing ident");
            match ident {
                IdentType::EnumVariant(_pattern_enum_id, _type_params, variant_idx) => {
                    if used_variants.contains(*variant_idx as usize) {
                        let msg = ErrorMessage::MatchUnreachablePattern;
                        sa.report(file_id, pattern_ident.span, msg);
                    }

                    used_variants.insert(*variant_idx as usize);
                }

                IdentType::Var(_var_id) => {
                    if used_variants.count_zeroes(..) == 0 {
                        let msg = ErrorMessage::MatchUnreachablePattern;
                        sa.report(file_id, pattern_ident.span, msg);
                    }

                    used_variants.insert_range(..);
                }

                _ => unreachable!(),
            }
        }

        ast::Pattern::Alt(ref p) => {
            for alt in &p.alts {
                check_pattern(sa, analysis, file_id, &alt, used_variants);
            }
        }

        ast::Pattern::ClassOrStructOrEnum(ref p) => {
            let ident = analysis.map_idents.get(p.id).expect("missing ident");

            match ident {
                IdentType::EnumVariant(_pattern_enum_id, _type_params, variant_idx) => {
                    if used_variants.contains(*variant_idx as usize) {
                        let msg = ErrorMessage::MatchUnreachablePattern;
                        sa.report(file_id, p.span, msg);
                    }

                    used_variants.insert(*variant_idx as usize);
                }

                _ => unreachable!(),
            }
        }
    }
}

fn check_match2(
    sa: &Sema,
    analysis: &AnalysisData,
    file_id: SourceFileId,
    node: &ast::ExprMatchType,
) {
    let mut matrix = Vec::new();

    for arm in &node.arms {
        let pattern = vec![convert_pattern(sa, analysis, &arm.pattern)];
        if check_useful(matrix.as_slice(), pattern.as_ref()) {
            matrix.push(pattern);
        } else {
            sa.report(
                file_id,
                arm.pattern.span(),
                ErrorMessage::MatchUnreachablePattern,
            );
        }
    }

    let missing_patterns = check_exhaustive(sa, matrix, 1);

    if !missing_patterns.is_empty() {
        sa.report(
            file_id,
            node.expr.span(),
            ErrorMessage::MatchUncoveredVariant,
        );
    }
}

fn check_exhaustive(sa: &Sema, matrix: Vec<Vec<Pattern>>, n: usize) -> Vec<Vec<Pattern>> {
    if matrix.is_empty() {
        return vec![vec![Pattern::Any; n]];
    }

    if n == 0 {
        assert_eq!(matrix.len(), 1);
        return Vec::new();
    }

    let signature = discover_signature(&matrix);

    match signature {
        Signature::Incomplete => {
            let new_matrix = matrix
                .iter()
                .filter_map(|r| specialize_row_for_any(r))
                .collect::<Vec<_>>();

            let mut result = check_exhaustive(sa, new_matrix, n - 1);

            if result.is_empty() {
                return Vec::new();
            }

            for row in &mut result {
                row.push(Pattern::Any);
            }

            result
        }

        Signature::Complete => {
            let ctor_data = discover_constructors(&matrix);

            if ctor_data.ctors.len() == ctor_data.total(sa) {
                for (&id, &arity) in &ctor_data.ctors {
                    let new_matrix = matrix
                        .iter()
                        .flat_map(|r| specialize_row_for_constructor(r, id, arity))
                        .collect::<Vec<_>>();

                    let uncovered = check_exhaustive(sa, new_matrix, n + arity - 1);

                    if !uncovered.is_empty() {
                        let tail = n - arity;

                        let result = uncovered
                            .into_iter()
                            .map(|mut row| {
                                let ctor_params = row.drain(tail..).collect::<Vec<_>>();
                                row.push(ctor_data.kind.pattern(id, ctor_params));
                                row
                            })
                            .collect::<Vec<_>>();

                        if !result.is_empty() {
                            return result;
                        }
                    }
                }

                Vec::new()
            } else {
                let new_matrix = matrix
                    .iter()
                    .filter_map(|r| specialize_row_for_any(r))
                    .collect::<Vec<_>>();

                let mut result = check_exhaustive(sa, new_matrix, n - 1);

                if result.is_empty() {
                    return Vec::new();
                }

                for row in &mut result {
                    row.push(Pattern::Any);
                }

                result
            }
        }
    }
}

enum Signature {
    Complete,
    Incomplete,
}

fn discover_signature(matrix: &[Vec<Pattern>]) -> Signature {
    let row = matrix.first().expect("missing row");

    match row.last().expect("missing pattern") {
        Pattern::Alt(..) => unimplemented!(),
        Pattern::Literal(value) => match value {
            LiteralValue::Bool(..) => Signature::Complete,
            LiteralValue::Char(..)
            | LiteralValue::Float(..)
            | LiteralValue::Int(..)
            | LiteralValue::String(..) => Signature::Incomplete,
        },
        Pattern::Any => Signature::Incomplete,
        Pattern::EnumVariant(..) => Signature::Complete,
        Pattern::Tuple(..) => Signature::Complete,
    }
}

enum CtorKind {
    Bool,
    Tuple,
    Enum(EnumDefinitionId),
}

impl CtorKind {
    fn pattern(&self, id: usize, params: Vec<Pattern>) -> Pattern {
        match self {
            CtorKind::Bool => {
                assert!(params.is_empty());
                assert!(id == 0 || id == 1);
                Pattern::Literal(LiteralValue::Bool(id == 1))
            }

            CtorKind::Tuple => {
                assert!(id == 0);
                Pattern::Tuple(params)
            }

            CtorKind::Enum(enum_id) => Pattern::EnumVariant(*enum_id, id, params),
        }
    }
}

struct CtorData {
    kind: CtorKind,
    ctors: HashMap<usize, usize>,
}

impl CtorData {
    fn total(&self, sa: &Sema) -> usize {
        match self.kind {
            CtorKind::Bool => 2,
            CtorKind::Enum(enum_id) => sa.enum_(enum_id).variants.len(),
            CtorKind::Tuple => 1,
        }
    }
}

fn discover_constructors(matrix: &[Vec<Pattern>]) -> CtorData {
    let mut ctors = HashMap::new();
    let mut kind = None;

    for row in matrix {
        match row.last().expect("missing pattern") {
            Pattern::Alt(..) => unimplemented!(),
            Pattern::Literal(value) => match value {
                LiteralValue::Bool(value) => {
                    match kind {
                        None => kind = Some(CtorKind::Bool),
                        Some(CtorKind::Bool) => (),
                        Some(_) => unreachable!(),
                    }
                    ctors.insert(*value as usize, 0);
                }
                _ => unimplemented!(),
            },
            Pattern::Any => (),
            Pattern::EnumVariant(enum_id, id, params) => {
                match kind {
                    None => kind = Some(CtorKind::Enum(*enum_id)),
                    Some(CtorKind::Enum(exp_enum_id)) => assert_eq!(exp_enum_id, *enum_id),
                    Some(_) => unreachable!(),
                }
                ctors.insert(*id as usize, params.len());
            }
            Pattern::Tuple(ref params) => {
                match kind {
                    None => kind = Some(CtorKind::Tuple),
                    Some(CtorKind::Tuple) => (),
                    Some(_) => unreachable!(),
                }
                ctors.insert(0, params.len());
            }
        }
    }

    CtorData {
        kind: kind.expect("missing kind"),
        ctors: ctors,
    }
}

fn check_useful(_matrix: &[Vec<Pattern>], _new_pattern: &[Pattern]) -> bool {
    true
}

fn specialize_row_for_any(row: &[Pattern]) -> Option<Vec<Pattern>> {
    let last = row.last().expect("missing pattern");

    match last {
        Pattern::Alt(..) => unimplemented!(),
        Pattern::Literal(..) | Pattern::EnumVariant(..) => None,
        Pattern::Any => {
            let count = row.len();
            Some(row[0..count - 1].to_vec())
        }
        Pattern::Tuple(..) => unimplemented!(),
    }
}

fn specialize_row_for_constructor(
    row: &[Pattern],
    id: usize,
    arity: usize,
) -> Option<Vec<Pattern>> {
    let last = row.last().expect("missing pattern");

    match last {
        Pattern::Alt(..) => unimplemented!(),
        Pattern::Literal(value) => match value {
            LiteralValue::Bool(value) => {
                assert_eq!(arity, 0);
                if id == *value as usize {
                    Some(row[0..row.len() - 1].to_vec())
                } else {
                    None
                }
            }

            _ => unimplemented!(),
        },
        Pattern::EnumVariant(_enum_id, ctor_id, params) => {
            assert_eq!(arity, params.len());
            if id == *ctor_id {
                let mut result = row[0..row.len() - 1].to_vec();
                result.extend_from_slice(params);
                Some(result)
            } else {
                None
            }
        }
        Pattern::Any => unimplemented!(),
        Pattern::Tuple(..) => unimplemented!(),
    }
}

#[allow(unused)]
#[derive(Clone)]
enum LiteralValue {
    Bool(bool),
    Char(char),
    Int(i64),
    Float(f64),
    String(String),
}

#[allow(unused)]
#[derive(Clone)]
enum Pattern {
    Any,
    Literal(LiteralValue),
    Tuple(Vec<Pattern>),
    EnumVariant(EnumDefinitionId, usize, Vec<Pattern>),
    Alt(Vec<Pattern>),
}

fn convert_pattern(sa: &Sema, analysis: &AnalysisData, pattern: &ast::Pattern) -> Pattern {
    match pattern {
        ast::Pattern::Underscore(..) => Pattern::Any,

        ast::Pattern::Rest(..) => unreachable!(),

        ast::Pattern::LitBool(ref lit) => {
            Pattern::Literal(LiteralValue::Bool(lit.expr.is_lit_true()))
        }

        ast::Pattern::LitInt(ref lit) => {
            let value = analysis.const_value(lit.id).to_i64().expect("i64 expected");
            Pattern::Literal(LiteralValue::Int(value))
        }

        ast::Pattern::LitString(ref lit) => {
            let value = analysis
                .const_value(lit.id)
                .to_string()
                .cloned()
                .expect("string expected");
            Pattern::Literal(LiteralValue::String(value))
        }

        ast::Pattern::LitFloat(ref lit) => {
            let value = analysis.const_value(lit.id).to_f64().expect("f64 expected");
            Pattern::Literal(LiteralValue::Float(value))
        }

        ast::Pattern::LitChar(ref lit) => {
            let value = analysis.const_value(lit.id).to_char();
            Pattern::Literal(LiteralValue::Char(value))
        }

        ast::Pattern::Tuple(ref tuple) => {
            let patterns = tuple
                .params
                .iter()
                .map(|p| convert_pattern(sa, analysis, &p))
                .collect();
            Pattern::Tuple(patterns)
        }

        ast::Pattern::Ident(ref pattern_ident) => {
            let ident = analysis
                .map_idents
                .get(pattern_ident.id)
                .expect("missing ident");
            match ident {
                IdentType::EnumVariant(pattern_enum_id, _type_params, variant_idx) => {
                    Pattern::EnumVariant(*pattern_enum_id, *variant_idx as usize, Vec::new())
                }

                IdentType::Var(_var_id) => Pattern::Any,

                _ => unreachable!(),
            }
        }

        ast::Pattern::Alt(ref p) => Pattern::Alt(
            p.alts
                .iter()
                .map(|alt| convert_pattern(sa, analysis, alt.as_ref()))
                .collect(),
        ),

        ast::Pattern::ClassOrStructOrEnum(ref ident) => {
            let subpatterns = ident
                .params
                .as_ref()
                .map(|v| {
                    v.iter()
                        .map(|p| convert_pattern(sa, analysis, p.pattern.as_ref()))
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default();
            let ident = analysis.map_idents.get(ident.id).expect("missing ident");

            match ident {
                IdentType::EnumVariant(pattern_enum_id, _type_params, variant_idx) => {
                    Pattern::EnumVariant(*pattern_enum_id, *variant_idx as usize, subpatterns)
                }

                _ => unreachable!(),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::{err, ok};
    use crate::ErrorMessage;

    #[test]
    fn test_missing_arm() {
        err(
            "
            enum Foo { A, B, C }
            fn f(x: Foo) {
                match x {
                    Foo::A => {}
                    Foo::B => {}
                }
            }
        ",
            (4, 23),
            ErrorMessage::MatchUncoveredVariant,
        );
    }

    #[test]
    fn test_duplicate_arm() {
        err(
            "
            enum Foo { A, B, C }
            fn f(x: Foo) {
                match x {
                    Foo::A => {}
                    Foo::B => {}
                    Foo::B => {}
                    Foo::C => {}
                }
            }
        ",
            (7, 21),
            ErrorMessage::MatchUnreachablePattern,
        );
    }

    #[test]
    fn test_duplicate_arm_after_underscore() {
        err(
            "
            enum Foo { A, B, C }
            fn f(x: Foo) {
                match x {
                    Foo::A => {}
                    _ => {}
                    Foo::C => {}
                }
            }
        ",
            (7, 21),
            ErrorMessage::MatchUnreachablePattern,
        );
    }

    #[test]
    fn test_duplicate_arm_after_var() {
        err(
            "
            enum Foo { A, B, C }
            fn f(x: Foo) {
                match x {
                    Foo::A => {}
                    v => {}
                    Foo::C => {}
                }
            }
        ",
            (7, 21),
            ErrorMessage::MatchUnreachablePattern,
        );
    }

    #[test]
    fn exhaustive_bool() {
        ok("
            @NewExhaustiveness
            fn f(v: Bool) {
                match v {
                    true => {}
                    false => {}
                }
            }
        ");
    }

    #[test]
    fn exhaustive_int() {
        ok("
            @NewExhaustiveness
            fn f(v: Int) {
                match v {
                    1 => {}
                    2 => {}
                    _ => {}
                }
            }
        ");
    }

    #[test]
    fn exhaustive_enum() {
        ok("
            enum Foo { A, B, C, D }
            @NewExhaustiveness
            fn f(v: Foo) {
                match v {
                    Foo::A => {}
                    Foo::B => {}
                    Foo::C => {}
                    Foo::D => {}
                }
            }
        ");

        ok("
            enum Foo { A, B, C, D }
            @NewExhaustiveness
            fn f(v: Foo) {
                match v {
                    Foo::A => {}
                    Foo::C => {}
                    Foo::D => {}
                    _ => {}
                }
            }
        ");
    }

    #[test]
    fn exhaustive_enum_through_underscore() {
        ok("
            enum Foo { A, B, C, D }
            @NewExhaustiveness
            fn f(v: Foo) {
                match v {
                    Foo::A => {}
                    Foo::C => {}
                    Foo::D => {}
                    _ => {}
                }
            }
        ");
    }
}
