use std::collections::{HashMap, HashSet};
use std::fmt::{self, Write};

use dora_parser::ast;
use dora_parser::ast::visit::{self, Visitor};
use dora_parser::Span;

use crate::sema::{
    AnalysisData, ClassDefinitionId, EnumDefinitionId, IdentType, Sema, SourceFileId,
    StructDefinitionId,
};
use crate::ErrorMessage;

pub fn check(sa: &Sema) {
    for (_id, fct) in sa.fcts.iter() {
        if fct.has_body() {
            let mut visitor = Exhaustiveness {
                sa,
                file_id: fct.file_id,
                analysis: fct.analysis(),
            };
            visit::walk_fct(&mut visitor, &fct.ast);
        }
    }
}

struct Exhaustiveness<'a> {
    sa: &'a Sema,
    analysis: &'a AnalysisData,
    file_id: SourceFileId,
}

impl<'a> Visitor for Exhaustiveness<'a> {
    fn visit_expr(&mut self, e: &ast::ExprData) {
        match *e {
            ast::ExprData::Match(ref expr) => {
                check_match(self.sa, self.analysis, self.file_id, expr);
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
    let mut matrix = Vec::new();

    let any_arm_has_guard = node.arms.iter().find(|a| a.cond.is_some()).is_some();
    let patterns_per_row = if any_arm_has_guard { 2 } else { 1 };

    for arm in &node.arms {
        let mut row = Vec::with_capacity(patterns_per_row);
        if any_arm_has_guard {
            if arm.cond.is_some() {
                row.push(Pattern::Guard);
            } else {
                row.push(Pattern::any_no_span());
            }
        }
        row.push(convert_pattern(sa, analysis, &arm.pattern));

        let useless = check_useful_expand(sa, matrix.clone(), row.clone());

        let spans = match useless {
            Useless::Set(spans) => spans,
            Useless::Yes => {
                let mut spans = HashSet::new();
                spans.insert(arm.pattern.span());
                spans
            }
        };

        for span in spans {
            sa.warn(file_id, span, ErrorMessage::UselessPattern);
        }

        matrix.push(row);
    }

    let missing_patterns = check_exhaustive(sa, matrix, patterns_per_row);

    if !missing_patterns.is_empty() {
        let mut patterns = Vec::new();

        for mut row in missing_patterns {
            assert_eq!(row.len(), patterns_per_row);
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
            ErrorMessage::NonExhaustiveMatch(patterns),
        );
    }
}

fn display_pattern(sa: &Sema, pattern: Pattern, output: &mut String) -> fmt::Result {
    match pattern {
        Pattern::Alt { alts, .. } => {
            assert!(alts.len() > 1);
            let mut first = true;

            for param in alts.into_iter().rev() {
                if !first {
                    output.write_str(" | ")?;
                }
                display_pattern(sa, param, output)?;
                first = false;
            }
            Ok(())
        }

        Pattern::Literal { value, .. } => match value {
            LiteralValue::Bool(value) => write!(output, "{}", value),
            LiteralValue::Char(value) => write!(output, "{}", value),
            LiteralValue::Int(value) => write!(output, "{}", value),
            LiteralValue::Float(value) => write!(output, "{}", value),
            LiteralValue::String(value) => write!(output, "{:?}", value),
        },

        Pattern::Any { .. } => write!(output, "_"),

        Pattern::Constructor {
            constructor_id,
            params,
            ..
        } => {
            let mut is_tuple = false;

            match constructor_id {
                ConstructorId::Bool => unreachable!(),

                ConstructorId::Enum(enum_id, variant_id) => {
                    let enum_ = sa.enum_(enum_id);
                    let variant = enum_.variants()[variant_id].name;
                    write!(output, "{}::{}", enum_.name(sa), sa.interner.str(variant))?;
                }

                ConstructorId::Class(class_id) => {
                    let class = sa.class(class_id);
                    write!(output, "{}", class.name(sa))?;
                }

                ConstructorId::Struct(struct_id) => {
                    let struct_ = sa.struct_(struct_id);
                    write!(output, "{}", struct_.name(sa))?;
                }

                ConstructorId::Tuple => is_tuple = true,
            }

            if !params.is_empty() || is_tuple {
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

        Pattern::Guard => unreachable!(),
    }
}

fn check_exhaustive(sa: &Sema, matrix: Vec<Vec<Pattern>>, n: usize) -> Vec<Vec<Pattern>> {
    for row in &matrix {
        assert_eq!(row.len(), n);
    }

    if matrix.is_empty() {
        return vec![vec![Pattern::any_no_span(); n]];
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
                .into_iter()
                .flat_map(|r| specialize_row_for_any(r))
                .collect::<Vec<_>>();

            let mut result = check_exhaustive(sa, new_matrix, n - 1);

            if result.is_empty() {
                return Vec::new();
            }

            for row in &mut result {
                row.push(Pattern::any_no_span());
            }

            result
        }

        Signature::Complete { ctors, first } => {
            let ctors_total = first.total(sa);
            assert!(ctors.len() <= ctors_total);

            if ctors.len() == ctors_total {
                for id in 0..ctors_total {
                    let arity = ctors.get(&id).cloned().expect("missing ctor id");
                    let new_matrix = matrix
                        .iter()
                        .flat_map(|r| specialize_row_for_constructor(r.clone(), id, arity))
                        .collect::<Vec<_>>();
                    let uncovered = check_exhaustive(sa, new_matrix, n + arity - 1);

                    if !uncovered.is_empty() {
                        let tail = n - 1;

                        let result = uncovered
                            .into_iter()
                            .map(|mut row| {
                                let ctor_params = row.drain(tail..).collect::<Vec<_>>();
                                assert_eq!(ctor_params.len(), arity);
                                row.push(first.pattern(id, ctor_params));
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
                    .into_iter()
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
                            result_row.push(Pattern::any_no_span());
                            result_with_ctor.push(result_row);
                            break;
                        } else {
                            result_row.push(first.pattern(ctor_id, Vec::new()));
                            result_with_ctor.push(result_row);
                        }
                    }

                    return result_with_ctor;
                }

                for row in &mut result {
                    row.push(Pattern::any_no_span());
                }

                result
            }
        }
    }
}

enum Signature {
    Complete {
        ctors: HashMap<usize, usize>,
        first: ConstructorId,
    },
    Incomplete,
}

fn discover_signature(matrix: &[Vec<Pattern>]) -> Signature {
    let mut ctors = HashMap::new();
    let mut first = None;

    for row in matrix {
        let pattern = row.last().expect("missing pattern");
        discover_signature_for_pattern(pattern, &mut ctors, &mut first);
    }

    match first {
        Some(first) => {
            assert!(!ctors.is_empty());
            Signature::Complete { ctors, first }
        }

        None => Signature::Incomplete,
    }
}

fn discover_signature_for_pattern(
    pattern: &Pattern,
    ctors: &mut HashMap<usize, usize>,
    kind: &mut Option<ConstructorId>,
) {
    match pattern {
        Pattern::Alt { ref alts, .. } => {
            for param in alts {
                discover_signature_for_pattern(param, ctors, kind);
            }
        }
        Pattern::Literal { value, .. } => match value {
            LiteralValue::Bool(value) => {
                match kind {
                    None => *kind = Some(ConstructorId::Bool),
                    Some(ConstructorId::Bool) => (),
                    Some(_) => unreachable!(),
                }
                ctors.insert(*value as usize, 0);
            }
            LiteralValue::Char(..)
            | LiteralValue::Float(..)
            | LiteralValue::Int(..)
            | LiteralValue::String(..) => {}
        },
        Pattern::Any { .. } => (),
        Pattern::Constructor {
            constructor_id,
            ref params,
            ..
        } => {
            match *kind {
                None => *kind = Some(constructor_id.clone()),
                Some(..) => (),
            }
            ctors.insert(constructor_id.variant_id(), params.len());
        }
        Pattern::Guard => (),
    }
}

fn check_useful_expand(sa: &Sema, matrix: Vec<Vec<Pattern>>, row: Vec<Pattern>) -> Useless {
    let matrix = matrix.into_iter().map(|row| SplitRow::new(row)).collect();
    check_useful_expand_inner(sa, matrix, SplitRow::new(row))
}

#[derive(Clone)]
struct SplitRow {
    p: Vec<Pattern>,
    q: Vec<Pattern>,
    r: Vec<Pattern>,
}

impl SplitRow {
    fn new(p: Vec<Pattern>) -> SplitRow {
        SplitRow {
            p,
            q: Vec::new(),
            r: Vec::new(),
        }
    }

    fn shift_p_into_q(&mut self) {
        let pattern = self.p.pop().expect("missing pattern");
        self.q.push(pattern);
    }

    fn shift_p_into_r(&mut self) {
        let pattern = self.p.pop().expect("missing pattern");
        self.r.push(pattern);
    }
}

impl fmt::Debug for SplitRow {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{p={:?}, q={:?}, r={:?}}}", self.p, self.q, self.r)
    }
}

trait SplitMatrixExt {
    fn extract_matrix_q(&self) -> Vec<Vec<Pattern>>;
    fn extract_matrix_r(&self) -> Vec<Vec<Pattern>>;
    fn from_matrices(p: Vec<Vec<Pattern>>, q: Vec<Vec<Pattern>>, r: Vec<Vec<Pattern>>) -> Self;
}

impl SplitMatrixExt for Vec<SplitRow> {
    fn extract_matrix_r(&self) -> Vec<Vec<Pattern>> {
        self.iter().map(|row| row.r.clone()).collect()
    }

    fn extract_matrix_q(&self) -> Vec<Vec<Pattern>> {
        self.iter().map(|row| row.q.clone()).collect()
    }

    fn from_matrices(
        p: Vec<Vec<Pattern>>,
        q: Vec<Vec<Pattern>>,
        r: Vec<Vec<Pattern>>,
    ) -> Vec<SplitRow> {
        assert_eq!(p.len(), q.len());
        assert_eq!(q.len(), r.len());

        p.into_iter()
            .zip(q.into_iter())
            .into_iter()
            .zip(r.into_iter())
            .map(|((p, q), r)| SplitRow { p, q, r })
            .collect()
    }
}

trait MatrixExt {
    fn concat(&mut self, other: Vec<Vec<Pattern>>);
    fn remove_column(&mut self, idx: usize) -> Vec<Vec<Pattern>>;
}

impl MatrixExt for Vec<Vec<Pattern>> {
    fn concat(&mut self, other: Vec<Vec<Pattern>>) {
        assert_eq!(self.len(), other.len());

        for (row, mut other_row) in self.iter_mut().zip(other.into_iter()) {
            row.append(&mut other_row);
        }
    }

    fn remove_column(&mut self, idx: usize) -> Vec<Vec<Pattern>> {
        let mut result = Vec::with_capacity(self.len());
        for row in self.iter_mut() {
            let el = row.remove(idx);
            result.push(vec![el]);
        }
        result
    }
}

enum Useless {
    // The whole pattern is useless.
    Yes,
    // The set of useless subpatterns.
    Set(HashSet<Span>),
}

impl Useless {
    fn yes() -> Useless {
        Useless::Yes
    }

    fn no() -> Useless {
        Useless::Set(HashSet::new())
    }

    fn union_all(results: Vec<(Useless, Span)>) -> Useless {
        if results.iter().all(|e| matches!(e.0, Useless::Yes)) {
            Useless::Yes
        } else {
            let mut spans = HashSet::new();

            for (useless, span) in results {
                match useless {
                    Useless::Yes => assert!(spans.insert(span)),
                    Useless::Set(set) => {
                        for span in set {
                            assert!(spans.insert(span));
                        }
                    }
                }
            }

            Useless::Set(spans)
        }
    }
}

fn check_useful_expand_inner(sa: &Sema, matrix: Vec<SplitRow>, mut pattern: SplitRow) -> Useless {
    let p_len = pattern.p.len();
    let q_len = pattern.q.len();
    let r_len = pattern.r.len();

    for row in &matrix {
        assert_eq!(row.p.len(), p_len);
        assert_eq!(row.q.len(), q_len);
        assert_eq!(row.r.len(), r_len);
    }

    if let Some(last) = pattern.p.pop() {
        match last {
            Pattern::Literal { value, .. } => {
                let new_matrix = matrix
                    .into_iter()
                    .flat_map(|r| specialize_row_for_literal(r, value.clone()))
                    .collect::<Vec<_>>();

                check_useful_expand_inner(sa, new_matrix, pattern)
            }
            Pattern::Constructor {
                constructor_id,
                mut params,
                ..
            } => {
                let arity = params.len();
                let new_matrix = matrix
                    .into_iter()
                    .flat_map(|r| {
                        specialize_row_for_constructor(r, constructor_id.variant_id(), arity)
                    })
                    .collect::<Vec<_>>();

                pattern.p.append(&mut params);
                check_useful_expand_inner(sa, new_matrix, pattern)
            }
            Pattern::Any { .. } => {
                let new_matrix = matrix
                    .into_iter()
                    .map(|mut row| {
                        row.shift_p_into_q();
                        row
                    })
                    .collect::<Vec<_>>();
                pattern.q.push(last);
                check_useful_expand_inner(sa, new_matrix, pattern)
            }
            Pattern::Alt { .. } => {
                let new_matrix = matrix
                    .into_iter()
                    .map(|mut row| {
                        row.shift_p_into_r();
                        row
                    })
                    .collect::<Vec<_>>();
                pattern.r.push(last);
                check_useful_expand_inner(sa, new_matrix, pattern)
            }
            Pattern::Guard => {
                // Should be last item in row.
                assert!(pattern.p.is_empty());

                let new_matrix = matrix
                    .into_iter()
                    .flat_map(|r| specialize_row_for_any(r))
                    .collect::<Vec<_>>();

                check_useful_expand_inner(sa, new_matrix, pattern)
            }
        }
    } else if pattern.r.is_empty() {
        let matrix = matrix.into_iter().map(|r| r.q).collect::<Vec<_>>();
        let pattern = pattern.q;

        if check_useful(sa, matrix, pattern) {
            Useless::no()
        } else {
            Useless::yes()
        }
    } else {
        let mut r_pattern_useless = Vec::new();

        for (r_idx, r_pattern) in pattern.r.iter().enumerate() {
            let alts = match r_pattern {
                Pattern::Alt { alts, .. } => alts,
                _ => unreachable!(),
            };

            let mut results = Vec::with_capacity(alts.len());
            let mut matrix_r_no_j = matrix.extract_matrix_r();
            let mut new_matrix_p = matrix_r_no_j.remove_column(r_idx);

            let mut new_matrix_q = matrix.extract_matrix_q();
            new_matrix_q.concat(matrix_r_no_j);

            let q_concat_r_no_j = {
                let mut q = pattern.q.clone();
                let mut r = pattern.r.clone();
                r.remove(r_idx);
                q.append(&mut r);
                q
            };
            let mut new_matrix_r = vec![Vec::new(); new_matrix_p.len()];

            for alt in alts {
                let new_matrix: Vec<SplitRow> = SplitMatrixExt::from_matrices(
                    new_matrix_p.clone(),
                    new_matrix_q.clone(),
                    new_matrix_r.clone(),
                );

                let new_pattern = SplitRow {
                    p: vec![alt.clone()],
                    q: q_concat_r_no_j.clone(),
                    r: Vec::new(),
                };

                let alt_result = check_useful_expand_inner(sa, new_matrix, new_pattern);
                results.push((alt_result, alt.span()));

                new_matrix_p.push(vec![alt.clone()]);
                new_matrix_q.push(q_concat_r_no_j.clone());
                new_matrix_r.push(Vec::new());
            }

            let r_pattern_result = Useless::union_all(results);
            r_pattern_useless.push((r_pattern_result, r_pattern.span()));
        }

        Useless::union_all(r_pattern_useless)
    }
}

fn check_useful(sa: &Sema, matrix: Vec<Vec<Pattern>>, mut pattern: Vec<Pattern>) -> bool {
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
        Pattern::Alt { alts, .. } => {
            for param in alts {
                let mut param_pattern = pattern.clone();
                param_pattern.push(param);
                if check_useful(sa, matrix.clone(), param_pattern) {
                    return true;
                }
            }

            false
        }

        Pattern::Literal { value, .. } => {
            let new_matrix = matrix
                .into_iter()
                .flat_map(|r| specialize_row_for_literal(r, value.clone()))
                .collect::<Vec<_>>();

            check_useful(sa, new_matrix, pattern)
        }

        Pattern::Any { .. } => {
            let signature = discover_signature(&matrix);

            match signature {
                Signature::Incomplete => {
                    let new_matrix = matrix
                        .into_iter()
                        .flat_map(|r| specialize_row_for_any(r))
                        .collect::<Vec<_>>();

                    check_useful(sa, new_matrix, pattern)
                }
                Signature::Complete { ctors, first } => {
                    let ctors_total = first.total(sa);
                    assert!(ctors.len() <= ctors_total);

                    if ctors.len() == ctors_total {
                        for id in 0..ctors_total {
                            let arity = ctors.get(&id).cloned().expect("missing ctor");
                            let new_matrix = matrix
                                .iter()
                                .flat_map(|r| specialize_row_for_constructor(r.clone(), id, arity))
                                .collect::<Vec<_>>();

                            let mut new_pattern = pattern.clone();
                            new_pattern
                                .extend(std::iter::repeat(Pattern::any_no_span()).take(arity));

                            if check_useful(sa, new_matrix, new_pattern) {
                                return true;
                            }
                        }

                        false
                    } else {
                        let new_matrix = matrix
                            .into_iter()
                            .flat_map(|r| specialize_row_for_any(r))
                            .collect::<Vec<_>>();

                        check_useful(sa, new_matrix, pattern)
                    }
                }
            }
        }

        Pattern::Constructor {
            constructor_id,
            mut params,
            ..
        } => {
            let arity = params.len();
            let new_matrix = matrix
                .into_iter()
                .flat_map(|r| specialize_row_for_constructor(r, constructor_id.variant_id(), arity))
                .collect::<Vec<_>>();

            pattern.append(&mut params);
            check_useful(sa, new_matrix, pattern)
        }

        Pattern::Guard => {
            // Should be last item in row.
            assert!(pattern.is_empty());

            let new_matrix = matrix
                .into_iter()
                .flat_map(|r| specialize_row_for_any(r))
                .collect::<Vec<_>>();

            check_useful(sa, new_matrix, pattern)
        }
    }
}

fn specialize_row_for_any<T: SpecializeRow + Clone>(mut row: T) -> Vec<T> {
    let last = row.pop();

    match last {
        Pattern::Alt { alts, .. } => alts
            .into_iter()
            .flat_map(|p| {
                let mut p_row = row.clone();
                p_row.push(p);
                let rows = specialize_row_for_any(p_row);
                rows
            })
            .collect::<Vec<_>>(),
        Pattern::Literal { .. } | Pattern::Constructor { .. } => Vec::new(),
        Pattern::Guard => Vec::new(),
        Pattern::Any { .. } => vec![row],
    }
}

fn specialize_row_for_literal<T: SpecializeRow + Clone>(
    mut row: T,
    literal: LiteralValue,
) -> Vec<T> {
    let last = row.pop();

    match last {
        Pattern::Alt { alts, .. } => alts
            .into_iter()
            .flat_map(|p| {
                let mut p_row = row.clone();
                p_row.push(p);
                let rows = specialize_row_for_literal(p_row, literal.clone());
                rows
            })
            .collect::<Vec<_>>(),
        Pattern::Literal { value, .. } => {
            if value == literal {
                vec![row]
            } else {
                Vec::new()
            }
        }
        Pattern::Any { .. } => vec![row],
        // This should never be reached because literals and tuples shouldn't type check.
        Pattern::Constructor { .. } | Pattern::Guard => {
            unreachable!()
        }
    }
}

trait SpecializeRow {
    fn pop(&mut self) -> Pattern;
    fn push(&mut self, p: Pattern);
    fn append(&mut self, patterns: Vec<Pattern>);
    fn append_any(&mut self, arity: usize);
}

impl SpecializeRow for SplitRow {
    fn pop(&mut self) -> Pattern {
        self.p.pop().expect("missing pattern")
    }

    fn push(&mut self, p: Pattern) {
        self.p.push(p);
    }

    fn append(&mut self, mut patterns: Vec<Pattern>) {
        self.p.append(&mut patterns);
    }

    fn append_any(&mut self, arity: usize) {
        self.p
            .extend(std::iter::repeat(Pattern::any_no_span()).take(arity));
    }
}

impl SpecializeRow for Vec<Pattern> {
    fn pop(&mut self) -> Pattern {
        self.pop().expect("missing pattern")
    }

    fn push(&mut self, p: Pattern) {
        self.push(p);
    }

    fn append(&mut self, mut patterns: Vec<Pattern>) {
        self.append(&mut patterns);
    }

    fn append_any(&mut self, arity: usize) {
        self.extend(std::iter::repeat(Pattern::any_no_span()).take(arity));
    }
}

fn specialize_row_for_constructor<T: SpecializeRow + Clone>(
    mut row: T,
    id: usize,
    arity: usize,
) -> Vec<T> {
    let last = row.pop();

    match last {
        Pattern::Alt { alts, .. } => alts
            .into_iter()
            .flat_map(|p| {
                let mut p_row = row.clone();
                p_row.push(p);
                let rows = specialize_row_for_constructor(p_row, id, arity);
                rows
            })
            .collect::<Vec<_>>(),
        Pattern::Literal { value, .. } => match value {
            LiteralValue::Bool(value) => {
                assert_eq!(arity, 0);
                if id == value as usize {
                    vec![row]
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
        Pattern::Constructor {
            constructor_id,
            params,
            ..
        } => {
            if id == constructor_id.variant_id() {
                assert_eq!(arity, params.len());
                row.append(params);
                vec![row]
            } else {
                Vec::new()
            }
        }
        Pattern::Any { .. } => {
            row.append_any(arity);
            vec![row]
        }

        Pattern::Guard => unimplemented!(),
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

#[derive(Clone, Debug)]
#[allow(unused)]
enum ConstructorId {
    Bool,
    Enum(EnumDefinitionId, usize),
    Class(ClassDefinitionId),
    Struct(StructDefinitionId),
    Tuple,
}

impl ConstructorId {
    fn variant_id(&self) -> usize {
        match self {
            ConstructorId::Bool => unreachable!(),
            ConstructorId::Enum(_, variant_id) => *variant_id,
            ConstructorId::Tuple | ConstructorId::Class(..) | ConstructorId::Struct(..) => 0,
        }
    }

    fn pattern(&self, id: usize, params: Vec<Pattern>) -> Pattern {
        match self {
            ConstructorId::Bool => {
                assert!(params.is_empty());
                Pattern::Literal {
                    span: Span::new(1, 1),
                    value: LiteralValue::Bool(id == 1),
                }
            }

            ConstructorId::Class(..) => Pattern::Constructor {
                span: Span::new(1, 1),
                constructor_id: self.clone(),
                params,
            },

            ConstructorId::Struct(..) => Pattern::Constructor {
                span: Span::new(1, 1),
                constructor_id: self.clone(),
                params,
            },

            ConstructorId::Tuple => Pattern::Constructor {
                span: Span::new(1, 1),
                constructor_id: ConstructorId::Tuple,
                params,
            },

            ConstructorId::Enum(enum_id, _) => Pattern::Constructor {
                span: Span::new(1, 1),
                constructor_id: ConstructorId::Enum(*enum_id, id),
                params,
            },
        }
    }

    fn total(&self, sa: &Sema) -> usize {
        match self {
            ConstructorId::Bool => 2,
            ConstructorId::Class(..) | ConstructorId::Struct(..) | ConstructorId::Tuple => 1,
            ConstructorId::Enum(enum_id, _) => sa.enum_(*enum_id).variants.len(),
        }
    }
}

#[derive(Clone)]
enum Pattern {
    Any {
        span: Option<Span>,
    },
    Literal {
        span: Span,
        value: LiteralValue,
    },
    Constructor {
        span: Span,
        constructor_id: ConstructorId,
        params: Vec<Pattern>,
    },
    Alt {
        span: Span,
        alts: Vec<Pattern>,
    },
    Guard,
}

impl Pattern {
    fn any_no_span() -> Pattern {
        Pattern::Any { span: None }
    }

    fn span(&self) -> Span {
        match self {
            Pattern::Any { span } => span.clone().expect("missing span"),
            Pattern::Literal { span, .. } => span.clone(),
            Pattern::Alt { span, .. } => span.clone(),
            Pattern::Constructor { span, .. } => span.clone(),
            Pattern::Guard => unreachable!(),
        }
    }
}

impl fmt::Debug for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pattern::Any { .. } => write!(f, "_"),
            Pattern::Literal { value, .. } => write!(f, "{:?}", value),
            Pattern::Constructor {
                constructor_id,
                params,
                ..
            } => {
                write!(f, "{:?}", constructor_id)?;

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
            Pattern::Alt { alts, .. } => {
                let mut first = true;
                for alt in alts {
                    if !first {
                        write!(f, " | ")?;
                    }
                    write!(f, "{:?}", alt)?;
                    first = false;
                }
                Ok(())
            }
            Pattern::Guard => write!(f, "GUARD"),
        }
    }
}

fn convert_pattern(sa: &Sema, analysis: &AnalysisData, pattern: &ast::Pattern) -> Pattern {
    match pattern {
        ast::Pattern::Underscore(ref p) => Pattern::Any { span: Some(p.span) },

        ast::Pattern::Rest(..) => unreachable!(),

        ast::Pattern::LitBool(ref lit) => Pattern::Literal {
            span: pattern.span(),
            value: LiteralValue::Bool(lit.expr.is_lit_true()),
        },

        ast::Pattern::LitInt(ref lit) => {
            let value = analysis.const_value(lit.id).to_i64().expect("i64 expected");
            Pattern::Literal {
                span: pattern.span(),
                value: LiteralValue::Int(value),
            }
        }

        ast::Pattern::LitString(ref lit) => {
            let value = analysis
                .const_value(lit.id)
                .to_string()
                .cloned()
                .expect("string expected");
            Pattern::Literal {
                span: lit.span,
                value: LiteralValue::String(value),
            }
        }

        ast::Pattern::LitFloat(ref lit) => {
            let value = analysis.const_value(lit.id).to_f64().expect("f64 expected");
            Pattern::Literal {
                span: lit.span,
                value: LiteralValue::Float(value),
            }
        }

        ast::Pattern::LitChar(ref lit) => {
            let value = analysis.const_value(lit.id).to_char();
            Pattern::Literal {
                span: lit.span,
                value: LiteralValue::Char(value),
            }
        }

        ast::Pattern::Tuple(ref tuple) => {
            let patterns = tuple
                .params
                .iter()
                .rev()
                .map(|p| convert_pattern(sa, analysis, &p))
                .collect();
            Pattern::Constructor {
                span: tuple.span,
                constructor_id: ConstructorId::Tuple,
                params: patterns,
            }
        }

        ast::Pattern::Ident(ref pattern_ident) => {
            let ident = analysis
                .map_idents
                .get(pattern_ident.id)
                .expect("missing ident");
            match ident {
                IdentType::EnumVariant(pattern_enum_id, _type_params, variant_id) => {
                    Pattern::Constructor {
                        span: pattern_ident.span,
                        constructor_id: ConstructorId::Enum(*pattern_enum_id, *variant_id as usize),
                        params: Vec::new(),
                    }
                }

                IdentType::Var(_var_id) => Pattern::Any {
                    span: Some(pattern_ident.span),
                },

                _ => unreachable!(),
            }
        }

        ast::Pattern::Alt(ref p) => Pattern::Alt {
            span: p.span,
            alts: p
                .alts
                .iter()
                .map(|alt| convert_pattern(sa, analysis, alt.as_ref()))
                .collect(),
        },

        ast::Pattern::ClassOrStructOrEnum(ref p) => {
            let ident = analysis.map_idents.get(p.id).expect("missing ident");

            match ident {
                IdentType::EnumVariant(pattern_enum_id, _type_params, variant_id) => {
                    let enum_ = sa.enum_(*pattern_enum_id);
                    let variant = &enum_.variants[*variant_id as usize];

                    Pattern::Constructor {
                        span: p.span,
                        constructor_id: ConstructorId::Enum(enum_.id(), variant.id as usize),
                        params: convert_subpatterns(sa, analysis, p, variant.fields.len()),
                    }
                }

                IdentType::Class(cls_id, _type_params) => {
                    let class = sa.class(*cls_id);
                    Pattern::Constructor {
                        span: p.span,
                        constructor_id: ConstructorId::Class(*cls_id),
                        params: convert_subpatterns(sa, analysis, p, class.fields.len()),
                    }
                }

                IdentType::Struct(struct_id, _type_params) => {
                    let struct_ = sa.struct_(*struct_id);
                    Pattern::Constructor {
                        span: p.span,
                        constructor_id: ConstructorId::Struct(*struct_id),
                        params: convert_subpatterns(sa, analysis, p, struct_.fields.len()),
                    }
                }

                _ => unreachable!(),
            }
        }
    }
}

fn convert_subpatterns(
    sa: &Sema,
    analysis: &AnalysisData,
    p: &ast::PatternClassOrStructOrEnum,
    n: usize,
) -> Vec<Pattern> {
    if let Some(ref params) = p.params {
        let mut result = vec![None; n];

        for subpattern in params {
            if subpattern.pattern.is_rest() {
                // Do nothing
            } else {
                let field_id = analysis
                    .map_field_ids
                    .get(subpattern.id)
                    .cloned()
                    .expect("missing field_id");
                let p = convert_pattern(sa, analysis, &subpattern.pattern);
                result[field_id] = Some(p);
            }
        }

        result
            .into_iter()
            .rev()
            .map(|t| t.unwrap_or_else(|| Pattern::any_no_span()))
            .collect()
    } else {
        Vec::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::{err, errors2, ok};
    use crate::{ErrorLevel, ErrorMessage};

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
            ErrorMessage::NonExhaustiveMatch(vec!["Foo::C".into()]),
        );
    }

    #[test]
    fn test_duplicate_arm() {
        errors2(
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
            vec![((7, 21), 6, ErrorLevel::Warn, ErrorMessage::UselessPattern)],
        );
    }

    #[test]
    fn test_duplicate_arm_after_underscore() {
        errors2(
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
            vec![((7, 21), 6, ErrorLevel::Warn, ErrorMessage::UselessPattern)],
        );
    }

    #[test]
    fn test_duplicate_arm_after_var() {
        errors2(
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
            vec![((7, 21), 6, ErrorLevel::Warn, ErrorMessage::UselessPattern)],
        );
    }

    #[test]
    fn usefulness_bool() {
        errors2(
            "
            fn f(v: Bool) {
                match v {
                    true => {}
                    true => {}
                    false => {}
                }
            }
        ",
            vec![((5, 21), 4, ErrorLevel::Warn, ErrorMessage::UselessPattern)],
        );
    }

    #[test]
    fn usefulness_tuple() {
        errors2(
            "
            fn f(v: (Int, Int)) {
                match v {
                    _ => {}
                    (1, 1) => {}
                }
            }
        ",
            vec![((5, 21), 6, ErrorLevel::Warn, ErrorMessage::UselessPattern)],
        );
    }

    #[test]
    fn usefulness_tuple_unreachable_any() {
        errors2(
            "
            fn f(v: (Int, Int)) {
                match v {
                    (_, _) => {}
                    _ => {}
                }
            }
        ",
            vec![((5, 21), 1, ErrorLevel::Warn, ErrorMessage::UselessPattern)],
        );
    }

    #[test]
    fn usefulness_enum() {
        errors2(
            "
            enum Foo { A(Int), C(Bool), D(Int, Bool) }

            fn f(v: Foo) {
                match v {
                    Foo::C(_) => {}
                    Foo::C(_) => {}
                    _ => {}
                }
            }
        ",
            vec![((7, 21), 9, ErrorLevel::Warn, ErrorMessage::UselessPattern)],
        );
    }

    #[test]
    fn usefulness_enum_all_variants() {
        errors2(
            "
            enum Foo { A, B, C }

            fn f(v: Foo) {
                match v {
                    Foo::A => {}
                    Foo::B => {}
                    Foo::C => {}
                    _ => {}
                }
            }
        ",
            vec![((9, 21), 1, ErrorLevel::Warn, ErrorMessage::UselessPattern)],
        );
    }

    #[test]
    fn usefulness_int() {
        errors2(
            "
            fn f(v: Int) {
                match v {
                    1 => {}
                    2 => {}
                    _ => {}
                    _ => {}
                }
            }
        ",
            vec![((7, 21), 1, ErrorLevel::Warn, ErrorMessage::UselessPattern)],
        );
    }

    #[test]
    fn usefulness_int_with_2_alternatives() {
        errors2(
            "
            fn f(v: Int) {
                match v {
                    1 => {}
                    2 => {}
                    _ | 3 => {}
                }
            }
        ",
            vec![((6, 25), 1, ErrorLevel::Warn, ErrorMessage::UselessPattern)],
        );
    }

    #[test]
    fn usefulness_int_with_3_alternatives() {
        errors2(
            "
            fn f(v: Int) {
                match v {
                    1 => {}
                    2 => {}
                    _ | 3 | 4 => {}
                }
            }
        ",
            vec![
                ((6, 25), 1, ErrorLevel::Warn, ErrorMessage::UselessPattern),
                ((6, 29), 1, ErrorLevel::Warn, ErrorMessage::UselessPattern),
            ],
        );
    }

    #[test]
    fn exhaustive_bool() {
        ok("
            fn f(v: Bool) {
                match v {
                    true => {}
                    false => {}
                }
            }
        ");

        err(
            "
            fn f(v: Bool) {
                match v {
                    true => {}
                }
            }
        ",
            (3, 23),
            ErrorMessage::NonExhaustiveMatch(vec!["false".into()]),
        );

        err(
            "
            fn f(v: Bool) {
                match v {
                    false => {}
                }
            }
        ",
            (3, 23),
            ErrorMessage::NonExhaustiveMatch(vec!["true".into()]),
        );
    }

    #[test]
    fn exhaustive_bool_with_guard() {
        ok("
            fn f(v: Bool) {
                match v {
                    true if true => {}
                    true if true => {}
                    true if true => {}
                    true => {}
                    false => {}
                }
            }
        ");

        errors2(
            "
            fn f(v: Bool) {
                match v {
                    true => {}
                    true if true => {}
                    false => {}
                }
            }
        ",
            vec![((5, 21), 4, ErrorLevel::Warn, ErrorMessage::UselessPattern)],
        );
    }

    #[test]
    fn exhaustive_int() {
        ok("
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
            fn f(v: Int) {
                match v {
                    1 => {}
                    2 => {}
                }
            }
        ",
            (3, 23),
            ErrorMessage::NonExhaustiveMatch(vec!["_".into()]),
        );
    }

    #[test]
    fn exhaustive_class() {
        ok("
            class Foo(Int, Bool)
            fn f(v: Foo) {
                match v {
                    Foo(_, true) => {}
                    Foo(_, false) => {}
                }
            }
        ");
    }

    #[test]
    fn non_exhaustive_class() {
        err(
            "
            class Foo(Int, Bool)
            fn f(v: Foo) {
                match v {
                    Foo(_, true) => {}
                }
            }
        ",
            (4, 23),
            ErrorMessage::NonExhaustiveMatch(vec!["Foo(_, false)".into()]),
        );
    }

    #[test]
    fn exhaustive_struct() {
        ok("
            struct Foo(Int, Bool)
            fn f(v: Foo) {
                match v {
                    Foo(_, true) => {}
                    Foo(_, false) => {}
                }
            }
        ");
    }

    #[test]
    fn non_exhaustive_struct() {
        err(
            "
            struct Foo(Int, Bool)
            fn f(v: Foo) {
                match v {
                    Foo(_, false) => {}
                }
            }
        ",
            (4, 23),
            ErrorMessage::NonExhaustiveMatch(vec!["Foo(_, true)".into()]),
        );
    }

    #[test]
    fn exhaustive_class_named_fields() {
        ok("
            class Foo {
                field0: Int,
                field1: Bool
            }

            fn f(v: Foo) {
                match v {
                    Foo(field1 = true, ..) => {}
                    Foo(field1 = false, ..) => {}
                }
            }
        ");
    }

    #[test]
    fn exhaustive_struct_named_fields() {
        ok("
            struct Foo {
                field0: Int,
                field1: Bool
            }

            fn f(v: Foo) {
                match v {
                    Foo(field1 = true, ..) => {}
                    Foo(field1 = false, ..) => {}
                }
            }
        ");
    }

    #[test]
    fn exhaustive_enum() {
        ok("
            enum Foo { A, B, C, D }
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
            fn f(v: Foo) {
                match v {
                    Foo::A => {}
                    Foo::D => {}
                }
            }
        ",
            (4, 23),
            ErrorMessage::NonExhaustiveMatch(vec!["Foo::B".into(), "Foo::C".into()]),
        );
    }

    #[test]
    fn exhaustive_enum_with_many_variants() {
        err(
            "
            enum Foo { C1, C2, C3, C4, C5, C6, C7, C8, C9, C10 }
            fn f(v: Foo) {
                match v {
                    Foo::C3 => {}
                    Foo::C5 => {}
                }
            }
        ",
            (4, 23),
            ErrorMessage::NonExhaustiveMatch(vec![
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
            fn f(v: Foo) {
                match v {
                    Foo::C3 | Foo::C4 | Foo::C5 | Foo::C8 => {}
                }
            }
        ",
            (4, 23),
            ErrorMessage::NonExhaustiveMatch(vec![
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
    fn exhaustive_enum_through_underscore_with_guard() {
        ok("
            enum Foo { A, B, C, D }
            fn f(v: Foo) {
                match v {
                    Foo::A => {}
                    Foo::C if true => {}
                    Foo::D if true => {}
                    _ => {}
                }
            }
        ");
    }

    #[test]
    fn exhaustive_only_underscore() {
        ok("
            enum Foo { A, B, C, D }
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
            (4, 23),
            ErrorMessage::NonExhaustiveMatch(vec!["(Foo::C, Foo::B)".into()]),
        );

        err(
            "
            enum Foo { A, B, C, D }
            fn f(v: (Foo, Bool)) {
                match v {
                    (Foo::A, _) => {}
                    (Foo::B, _) => {}
                    (Foo::D, true) => {}
                    (Foo::C, _) => {}
                }
            }
        ",
            (4, 23),
            ErrorMessage::NonExhaustiveMatch(vec!["(Foo::D, false)".into()]),
        );
    }

    #[test]
    fn exhaustive_enum_with_payload() {
        ok("
            enum Foo { A(Int), C(Bool), D(Bar, Bool) }
            enum Bar { X, Y }
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
    fn exhaustive_enum_with_payload_and_rest() {
        ok("
            enum Foo { A(Int), C(Bool), D(Bar, Bool) }
            enum Bar { X, Y }
            fn f(v: Foo) {
                match v {
                    Foo::A(1) => {}
                    Foo::A(2) => {}
                    Foo::A(_) => {}
                    Foo::D(..) => {}
                    Foo::C(_) => {}
                }
            }
        ");
    }

    #[test]
    fn exhaustive_enum_with_payload_and_alternatives() {
        ok("
            enum Foo { A(Int), C(Bool), D(Bar, Bool) }
            enum Bar { X, Y }
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
            (5, 23),
            ErrorMessage::NonExhaustiveMatch(vec!["Foo::A(_)".into()]),
        );

        err(
            "
            enum Foo { A(Int), C(Bool), D(Bar, Bool) }
            enum Bar { X, Y }
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
            (5, 23),
            ErrorMessage::NonExhaustiveMatch(vec!["Foo::D(Bar::X, false)".into()]),
        );
    }
}
