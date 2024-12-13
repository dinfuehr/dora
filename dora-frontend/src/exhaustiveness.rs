use std::collections::HashMap;
use std::fmt::{self, Write};

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
                is_check_usefulness: fct.is_check_usefulness,
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
    is_check_usefulness: bool,
}

impl<'a> Visitor for Exhaustiveness<'a> {
    fn visit_expr(&mut self, e: &ast::ExprData) {
        match *e {
            ast::ExprData::Match(ref expr) => {
                if self.is_new_exhaustiveness {
                    // Improved exhaustiveness check is WIP and not enabled by default.
                    check_match2(
                        self.sa,
                        self.analysis,
                        self.file_id,
                        expr,
                        self.is_check_usefulness,
                    );
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
    is_check_usefulness: bool,
) {
    let mut matrix = Vec::new();

    for arm in &node.arms {
        let pattern = vec![convert_pattern(sa, analysis, &arm.pattern)];

        if is_check_usefulness {
            if !check_useful(matrix.clone(), pattern.clone()) {
                sa.report(
                    file_id,
                    arm.pattern.span(),
                    ErrorMessage::MatchUnreachablePattern,
                );
            }
        }

        matrix.push(pattern);
    }

    let missing_patterns = check_exhaustive(sa, matrix, 1);

    if !missing_patterns.is_empty() {
        let mut patterns = Vec::new();

        for mut row in missing_patterns {
            assert_eq!(row.len(), 1);
            let mut pattern_as_string = String::new();
            display_pattern(
                sa,
                row.pop().expect("missing pattern"),
                &mut pattern_as_string,
            )
            .expect("stringify failed for pattern");
            patterns.push(pattern_as_string);
        }

        sa.report(
            file_id,
            node.expr.span(),
            ErrorMessage::MatchUncoveredVariantWithPattern(patterns),
        );
    }
}

fn display_pattern(sa: &Sema, pattern: Pattern, output: &mut String) -> fmt::Result {
    match pattern {
        Pattern::Alt(params) => {
            assert!(params.len() > 1);
            let mut first = true;

            for param in params.into_iter().rev() {
                if !first {
                    output.write_str(" | ")?;
                }
                display_pattern(sa, param, output)?;
                first = false;
            }
            Ok(())
        }

        Pattern::Literal(value) => match value {
            LiteralValue::Bool(value) => write!(output, "{}", value),
            LiteralValue::Char(value) => write!(output, "{}", value),
            LiteralValue::Int(value) => write!(output, "{}", value),
            LiteralValue::Float(value) => write!(output, "{}", value),
            LiteralValue::String(value) => write!(output, "{:?}", value),
        },

        Pattern::EnumVariant(enum_id, variant_id, params) => {
            let enum_ = sa.enum_(enum_id);
            let variant = enum_.variants()[variant_id].name;
            write!(output, "{}::{}", enum_.name(sa), sa.interner.str(variant))?;

            if !params.is_empty() {
                let mut first = true;
                write!(output, "(")?;

                for param in params.into_iter().rev() {
                    if !first {
                        output.write_str(", ")?;
                    }
                    display_pattern(sa, param, output)?;
                    first = false;
                }

                write!(output, ")")?;
            }

            Ok(())
        }

        Pattern::Any => write!(output, "_"),

        Pattern::Tuple(params) => {
            let mut first = true;
            write!(output, "(")?;

            for param in params.into_iter().rev() {
                if !first {
                    output.write_str(", ")?;
                }
                display_pattern(sa, param, output)?;
                first = false;
            }

            write!(output, ")")
        }
    }
}

fn check_exhaustive(sa: &Sema, matrix: Vec<Vec<Pattern>>, n: usize) -> Vec<Vec<Pattern>> {
    for row in &matrix {
        assert_eq!(row.len(), n);
    }

    if matrix.is_empty() {
        return vec![vec![Pattern::Any; n]];
    }

    if n == 0 {
        // With useless patterns in the matrix, the matrix might have
        // more than one row, so this assertion does not hold:
        // assert_eq!(matrix.len(), 1);
        assert!(matrix.len() > 0);
        return Vec::new();
    }

    let signature = discover_signature(&matrix);

    match signature {
        Signature::Incomplete => {
            let new_matrix = matrix
                .iter()
                .flat_map(|r| specialize_row_for_any(r))
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

        Signature::Complete { ctors, kind } => {
            let ctors_total = kind.total(sa);
            assert!(ctors.len() <= ctors_total);

            if ctors.len() == ctors_total {
                for id in 0..ctors_total {
                    let arity = ctors.get(&id).cloned().expect("missing ctor id");
                    let new_matrix = matrix
                        .iter()
                        .flat_map(|r| specialize_row_for_constructor(r, id, arity))
                        .collect::<Vec<_>>();
                    let uncovered = check_exhaustive(sa, new_matrix, n + arity - 1);

                    if !uncovered.is_empty() {
                        let tail = n - 1;

                        let result = uncovered
                            .into_iter()
                            .map(|mut row| {
                                let ctor_params = row.drain(tail..).collect::<Vec<_>>();
                                assert_eq!(ctor_params.len(), arity);
                                row.push(kind.pattern(id, ctor_params));
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
                    .flat_map(|r| specialize_row_for_any(r))
                    .collect::<Vec<_>>();

                let mut result = check_exhaustive(sa, new_matrix, n - 1);

                if result.is_empty() {
                    return Vec::new();
                }

                if result.len() == 1 {
                    let mut result_with_ctor = Vec::new();
                    let row = result.first().expect("missing row");

                    for ctor_id in 0..ctors_total {
                        if ctors.contains_key(&ctor_id) {
                            continue;
                        }

                        let mut result_row = row.clone();

                        // Avoid too many failure patterns.
                        if result_with_ctor.len() == 5 {
                            result_row.push(Pattern::Any);
                            result_with_ctor.push(result_row);
                            break;
                        } else {
                            result_row.push(kind.pattern(ctor_id, Vec::new()));
                            result_with_ctor.push(result_row);
                        }
                    }

                    return result_with_ctor;
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
    Complete {
        ctors: HashMap<usize, usize>,
        kind: CtorKind,
    },
    Incomplete,
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

    fn total(&self, sa: &Sema) -> usize {
        match self {
            CtorKind::Bool => 2,
            CtorKind::Enum(enum_id) => sa.enum_(*enum_id).variants.len(),
            CtorKind::Tuple => 1,
        }
    }
}

fn discover_signature(matrix: &[Vec<Pattern>]) -> Signature {
    let mut ctors = HashMap::new();
    let mut kind = None;

    for row in matrix {
        let pattern = row.last().expect("missing pattern");
        discover_signature_for_pattern(pattern, &mut ctors, &mut kind);
    }

    match kind {
        Some(kind) => {
            assert!(!ctors.is_empty());
            Signature::Complete { ctors, kind }
        }

        None => Signature::Incomplete,
    }
}

fn discover_signature_for_pattern(
    pattern: &Pattern,
    ctors: &mut HashMap<usize, usize>,
    kind: &mut Option<CtorKind>,
) {
    match pattern {
        Pattern::Alt(ref params) => {
            for param in params {
                discover_signature_for_pattern(param, ctors, kind);
            }
        }
        Pattern::Literal(value) => match value {
            LiteralValue::Bool(value) => {
                match kind {
                    None => *kind = Some(CtorKind::Bool),
                    Some(CtorKind::Bool) => (),
                    Some(_) => unreachable!(),
                }
                ctors.insert(*value as usize, 0);
            }
            LiteralValue::Char(..)
            | LiteralValue::Float(..)
            | LiteralValue::Int(..)
            | LiteralValue::String(..) => {}
        },
        Pattern::Any => (),
        Pattern::EnumVariant(enum_id, id, params) => {
            match *kind {
                None => *kind = Some(CtorKind::Enum(*enum_id)),
                Some(CtorKind::Enum(exp_enum_id)) => assert_eq!(exp_enum_id, *enum_id),
                Some(_) => unreachable!(),
            }
            ctors.insert(*id as usize, params.len());
        }
        Pattern::Tuple(ref params) => {
            match *kind {
                None => *kind = Some(CtorKind::Tuple),
                Some(CtorKind::Tuple) => (),
                Some(_) => unreachable!(),
            }
            ctors.insert(0, params.len());
        }
    }
}

fn check_useful(matrix: Vec<Vec<Pattern>>, mut pattern: Vec<Pattern>) -> bool {
    let n = pattern.len();

    for row in &matrix {
        assert_eq!(row.len(), n);
    }

    if matrix.is_empty() {
        return true;
    }

    if n == 0 {
        return false;
    }

    let last = pattern.pop().expect("missing pattern");

    match last {
        Pattern::Alt(params) => {
            for param in params {
                let mut param_pattern = pattern.clone();
                param_pattern.push(param);
                if check_useful(matrix.clone(), param_pattern) {
                    return true;
                }
            }

            false
        }

        Pattern::Literal(literal) => {
            let new_matrix = matrix
                .iter()
                .flat_map(|r| specialize_row_for_literal(r, literal.clone()))
                .collect::<Vec<_>>();

            check_useful(new_matrix, pattern)
        }

        Pattern::Any => {
            let new_matrix = matrix
                .iter()
                .flat_map(|r| specialize_row_for_any(r))
                .collect::<Vec<_>>();

            check_useful(new_matrix, pattern)
        }

        Pattern::EnumVariant(_enum_id, variant_id, params) => {
            let arity = params.len();
            let new_matrix = matrix
                .iter()
                .flat_map(|r| specialize_row_for_constructor(r, variant_id, arity))
                .collect::<Vec<_>>();

            check_useful(new_matrix, pattern)
        }

        Pattern::Tuple(params) => {
            let arity = params.len();

            let new_matrix = matrix
                .iter()
                .flat_map(|r| specialize_row_for_constructor(r, 0, arity))
                .collect::<Vec<_>>();

            check_useful(new_matrix, pattern)
        }
    }
}

fn specialize_row_for_any(row: &[Pattern]) -> Vec<Vec<Pattern>> {
    let mut result_row = row.to_vec();
    let last = result_row.pop().expect("missing pattern");

    match last {
        Pattern::Alt(params) => params
            .into_iter()
            .flat_map(|p| {
                result_row.push(p);
                let rows = specialize_row_for_any(&result_row);
                result_row.pop();
                rows
            })
            .collect::<Vec<_>>(),
        Pattern::Literal(..) | Pattern::EnumVariant(..) => Vec::new(),
        Pattern::Any => vec![result_row],
        // This should never be reached as long as all patterns are useful.
        // This is because tuples conceptually have a single constructor and thus
        // should always reach the complete signature code path.
        Pattern::Tuple(..) => unreachable!(),
    }
}

fn specialize_row_for_literal(row: &[Pattern], literal: LiteralValue) -> Vec<Vec<Pattern>> {
    let mut result_row = row.to_vec();
    let last = result_row.pop().expect("missing pattern");

    match last {
        Pattern::Alt(params) => params
            .into_iter()
            .flat_map(|p| {
                result_row.push(p);
                let rows = specialize_row_for_literal(&result_row, literal.clone());
                result_row.pop();
                rows
            })
            .collect::<Vec<_>>(),
        Pattern::Literal(lit) => {
            if lit == literal {
                vec![result_row]
            } else {
                Vec::new()
            }
        }
        Pattern::EnumVariant(..) => Vec::new(),
        Pattern::Any => vec![result_row],
        // This should never be reached because literals and tuples shouldn't type check.
        Pattern::Tuple(..) => unreachable!(),
    }
}

fn specialize_row_for_constructor(row: &[Pattern], id: usize, arity: usize) -> Vec<Vec<Pattern>> {
    let mut result_row = row.to_vec();
    let last = result_row.pop().expect("missing pattern");

    match last {
        Pattern::Alt(params) => params
            .into_iter()
            .flat_map(|p| {
                result_row.push(p);
                let rows = specialize_row_for_constructor(&result_row, id, arity);
                result_row.pop();
                rows
            })
            .collect::<Vec<_>>(),
        Pattern::Literal(value) => match value {
            LiteralValue::Bool(value) => {
                assert_eq!(arity, 0);
                if id == value as usize {
                    vec![result_row]
                } else {
                    Vec::new()
                }
            }

            // These cases here should be unreachable. This is because
            // literal values of these types never have a complete signature.
            LiteralValue::Char(..)
            | LiteralValue::Float(..)
            | LiteralValue::Int(..)
            | LiteralValue::String(..) => unreachable!(),
        },
        Pattern::EnumVariant(_enum_id, ctor_id, mut params) => {
            if id == ctor_id {
                assert_eq!(arity, params.len());
                result_row.append(&mut params);
                vec![result_row]
            } else {
                Vec::new()
            }
        }
        Pattern::Any => {
            result_row.extend(std::iter::repeat(Pattern::Any).take(arity));
            vec![result_row]
        }

        Pattern::Tuple(mut params) => {
            assert_eq!(id, 0);
            result_row.append(&mut params);
            vec![result_row]
        }
    }
}

#[derive(Clone, PartialEq)]
enum LiteralValue {
    Bool(bool),
    Char(char),
    Int(i64),
    Float(f64),
    String(String),
}

impl fmt::Debug for LiteralValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LiteralValue::Bool(value) => write!(f, "{}", *value),
            LiteralValue::Char(value) => write!(f, "{}", *value),
            LiteralValue::Int(value) => write!(f, "{}", *value),
            LiteralValue::Float(value) => write!(f, "{}", *value),
            LiteralValue::String(value) => write!(f, "{:?}", value),
        }
    }
}

#[derive(Clone)]
enum Pattern {
    Any,
    Literal(LiteralValue),
    Tuple(Vec<Pattern>),
    EnumVariant(EnumDefinitionId, usize, Vec<Pattern>),
    Alt(Vec<Pattern>),
}

impl fmt::Debug for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pattern::Any => write!(f, "_"),
            Pattern::Literal(_lit) => unimplemented!(),
            Pattern::EnumVariant(enum_id, variant_idx, params) => {
                write!(f, "e{}::{}", enum_id.index(), *variant_idx)?;

                if !params.is_empty() {
                    write!(f, "(")?;
                    let mut first = true;
                    for param in params {
                        if !first {
                            write!(f, ", ")?;
                        }
                        write!(f, "{:?}", param)?;
                        first = false;
                    }
                    write!(f, ")")
                } else {
                    Ok(())
                }
            }
            Pattern::Tuple(params) => {
                write!(f, "(")?;
                let mut first = true;
                for param in params {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", param)?;
                    first = false;
                }
                write!(f, ")")
            }
            Pattern::Alt(params) => {
                let mut first = true;
                for param in params {
                    if !first {
                        write!(f, " | ")?;
                    }
                    write!(f, "{:?}", param)?;
                    first = false;
                }
                Ok(())
            }
        }
    }
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
                .rev()
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
                        .rev()
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
    fn usefulness_bool() {
        err(
            "
            @NewExhaustiveness @CheckUsefulness
            fn f(v: Bool) {
                match v {
                    true => {}
                    true => {}
                    false => {}
                }
            }
        ",
            (6, 21),
            ErrorMessage::MatchUnreachablePattern,
        );
    }

    #[test]
    #[ignore]
    fn usefulness_tuple() {
        ok("
            @NewExhaustiveness @CheckUsefulness
            fn f(v: (Int, Int)) {
                match v {
                    _ => {}
                    (1, 1) => {}
                }
            }
        ");
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

        err(
            "
            @NewExhaustiveness
            fn f(v: Bool) {
                match v {
                    true => {}
                }
            }
        ",
            (4, 23),
            ErrorMessage::MatchUncoveredVariantWithPattern(vec!["false".into()]),
        );

        err(
            "
            @NewExhaustiveness
            fn f(v: Bool) {
                match v {
                    false => {}
                }
            }
        ",
            (4, 23),
            ErrorMessage::MatchUncoveredVariantWithPattern(vec!["true".into()]),
        );
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

        err(
            "
            @NewExhaustiveness
            fn f(v: Int) {
                match v {
                    1 => {}
                    2 => {}
                }
            }
        ",
            (4, 23),
            ErrorMessage::MatchUncoveredVariantWithPattern(vec!["_".into()]),
        );
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

        err(
            "
            enum Foo { A, B, C, D }
            @NewExhaustiveness
            fn f(v: Foo) {
                match v {
                    Foo::A => {}
                    Foo::D => {}
                }
            }
        ",
            (5, 23),
            ErrorMessage::MatchUncoveredVariantWithPattern(vec!["Foo::B".into(), "Foo::C".into()]),
        );
    }

    #[test]
    fn exhaustive_enum_with_many_variants() {
        err(
            "
            enum Foo { C1, C2, C3, C4, C5, C6, C7, C8, C9, C10 }
            @NewExhaustiveness
            fn f(v: Foo) {
                match v {
                    Foo::C3 => {}
                    Foo::C5 => {}
                }
            }
        ",
            (5, 23),
            ErrorMessage::MatchUncoveredVariantWithPattern(vec![
                "Foo::C1".into(),
                "Foo::C2".into(),
                "Foo::C4".into(),
                "Foo::C6".into(),
                "Foo::C7".into(),
                "_".into(),
            ]),
        );

        err(
            "
            enum Foo { C1, C2, C3, C4, C5, C6, C7, C8, C9, C10 }
            @NewExhaustiveness
            fn f(v: Foo) {
                match v {
                    Foo::C3 | Foo::C4 | Foo::C5 | Foo::C8 => {}
                }
            }
        ",
            (5, 23),
            ErrorMessage::MatchUncoveredVariantWithPattern(vec![
                "Foo::C1".into(),
                "Foo::C2".into(),
                "Foo::C6".into(),
                "Foo::C7".into(),
                "Foo::C9".into(),
                "_".into(),
            ]),
        );
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

    #[test]
    fn exhaustive_only_underscore() {
        ok("
            enum Foo { A, B, C, D }
            @NewExhaustiveness
            fn f(v: Foo) {
                match v {
                    _ => {}
                }
            }
        ");
    }

    #[test]
    fn exhaustive_only_underscore_tuple() {
        ok("
            enum Foo { A, B, C, D }
            @NewExhaustiveness
            fn f(v: (Foo, Foo)) {
                match v {
                    _ => {}
                }
            }
        ");
    }

    #[test]
    fn exhaustive_underscore_tuple_pattern() {
        ok("
            enum Foo { A, B, C, D }
            @NewExhaustiveness
            fn f(v: (Foo, Foo)) {
                match v {
                    (_, _) => {}
                }
            }
        ");
    }

    #[test]
    fn exhaustive_int_tuple_pattern() {
        ok("
            @NewExhaustiveness
            fn f(v: (Int, Int)) {
                match v {
                    (1, 1) => {}
                    (3, _) => {}
                    _ => {}
                }
            }
        ");
    }

    #[test]
    fn exhaustive_tuple_pattern() {
        ok("
            enum Foo { A, B, C, D }
            @NewExhaustiveness
            fn f(v: (Foo, Foo)) {
                match v {
                    (Foo::A, _) => {}
                    (Foo::B, _) => {}
                    (Foo::C, Foo::A) => {}
                    (Foo::C, Foo::B) => {}
                    (Foo::C, Foo::C) => {}
                    (Foo::C, Foo::D) => {}
                    (Foo::D, _) => {}
                }
            }
        ");

        err(
            "
            enum Foo { A, B, C, D }
            @NewExhaustiveness
            fn f(v: (Foo, Foo)) {
                match v {
                    (Foo::A, _) => {}
                    (Foo::B, _) => {}
                    (Foo::C, Foo::A) => {}
                    (Foo::C, Foo::C) => {}
                    (Foo::C, Foo::D) => {}
                    (Foo::D, _) => {}
                }
            }
        ",
            (5, 23),
            ErrorMessage::MatchUncoveredVariantWithPattern(vec!["(Foo::C, Foo::B)".into()]),
        );

        err(
            "
            enum Foo { A, B, C, D }
            @NewExhaustiveness
            fn f(v: (Foo, Bool)) {
                match v {
                    (Foo::A, _) => {}
                    (Foo::B, _) => {}
                    (Foo::D, true) => {}
                    (Foo::C, _) => {}
                }
            }
        ",
            (5, 23),
            ErrorMessage::MatchUncoveredVariantWithPattern(vec!["(Foo::D, false)".into()]),
        );
    }

    #[test]
    fn exhaustive_enum_with_payload() {
        ok("
            enum Foo { A(Int), C(Bool), D(Bar, Bool) }
            enum Bar { X, Y }
            @NewExhaustiveness
            fn f(v: Foo) {
                match v {
                    Foo::A(1) => {}
                    Foo::A(2) => {}
                    Foo::A(_) => {}
                    Foo::C(_) => {}
                    Foo::D(Bar::X, true) => {}
                    Foo::D(Bar::X, false) => {}
                    Foo::D(Bar::Y, _) => {}
                }
            }
        ");
    }

    #[test]
    fn exhaustive_enum_with_payload_and_alternatives() {
        ok("
            enum Foo { A(Int), C(Bool), D(Bar, Bool) }
            enum Bar { X, Y }
            @NewExhaustiveness
            fn f(v: Foo) {
                match v {
                    Foo::A(1 | 2) | Foo::C(_) => {}
                    Foo::A(_) => {}
                    Foo::D(Bar::X, true | false)
                    | Foo::D(Bar::Y, _) => {}
                }
            }
        ");
    }

    #[test]
    fn exhaustive_enum_with_payload_with_uncovered_int() {
        err(
            "
            enum Foo { A(Int), C(Bool), D(Bar, Bool) }
            enum Bar { X, Y }
            @NewExhaustiveness
            fn f(v: Foo) {
                match v {
                    Foo::A(1) => {}
                    Foo::A(2) => {}
                    Foo::C(_) => {}
                    Foo::D(Bar::X, true) => {}
                    Foo::D(Bar::Y, _) => {}
                }
            }
        ",
            (6, 23),
            ErrorMessage::MatchUncoveredVariantWithPattern(vec!["Foo::A(_)".into()]),
        );

        err(
            "
            enum Foo { A(Int), C(Bool), D(Bar, Bool) }
            enum Bar { X, Y }
            @NewExhaustiveness
            fn f(v: Foo) {
                match v {
                    Foo::A(1) => {}
                    Foo::A(2) => {}
                    Foo::A(_) => {}
                    Foo::C(_) => {}
                    Foo::D(Bar::X, true) => {}
                    Foo::D(Bar::Y, _) => {}
                }
            }
        ",
            (6, 23),
            ErrorMessage::MatchUncoveredVariantWithPattern(vec!["Foo::D(Bar::X, false)".into()]),
        );
    }
}
