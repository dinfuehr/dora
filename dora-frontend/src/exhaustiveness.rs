use dora_parser::ast;
use dora_parser::ast::visit::{self, Visitor};
use fixedbitset::FixedBitSet;

use crate::sema::{AnalysisData, IdentType, Sema, SourceFileId};
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
                if self.sa.flags.new_exhaustiveness {
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

        let pattern = arm.pattern.as_ref();
        for pattern in &pattern.alts {
            match pattern.as_ref() {
                ast::PatternAlt::Underscore(..) => {
                    if used_variants.count_zeroes(..) == 0 {
                        let msg = ErrorMessage::MatchUnreachablePattern;
                        sa.report(file_id, arm.span, msg);
                    }

                    used_variants.insert_range(..);
                }

                ast::PatternAlt::Rest(..) => unreachable!(),

                ast::PatternAlt::LitBool(..)
                | ast::PatternAlt::LitChar(..)
                | ast::PatternAlt::LitString(..)
                | ast::PatternAlt::LitInt(..)
                | ast::PatternAlt::LitFloat(..)
                | ast::PatternAlt::Tuple(..) => unreachable!(),

                ast::PatternAlt::Ident(ref pattern_ident) => {
                    let ident = analysis
                        .map_idents
                        .get(pattern_ident.id)
                        .expect("missing ident");
                    match ident {
                        IdentType::EnumVariant(_pattern_enum_id, _type_params, variant_idx) => {
                            if used_variants.contains(*variant_idx as usize) {
                                let msg = ErrorMessage::MatchUnreachablePattern;
                                sa.report(file_id, arm.span, msg);
                            }

                            used_variants.insert(*variant_idx as usize);
                        }

                        IdentType::Var(_var_id) => {
                            if used_variants.count_zeroes(..) == 0 {
                                let msg = ErrorMessage::MatchUnreachablePattern;
                                sa.report(file_id, arm.span, msg);
                            }

                            used_variants.insert_range(..);
                        }

                        _ => unreachable!(),
                    }
                }

                ast::PatternAlt::ClassOrStructOrEnum(ref ident) => {
                    let ident = analysis.map_idents.get(ident.id).expect("missing ident");

                    match ident {
                        IdentType::EnumVariant(_pattern_enum_id, _type_params, variant_idx) => {
                            if used_variants.contains(*variant_idx as usize) {
                                let msg = ErrorMessage::MatchUnreachablePattern;
                                sa.report(file_id, arm.span, msg);
                            }

                            used_variants.insert(*variant_idx as usize);
                        }

                        _ => unreachable!(),
                    }
                }
            }
        }
    }

    used_variants.toggle_range(..);

    if used_variants.count_ones(..) != 0 {
        let msg = ErrorMessage::MatchUncoveredVariant;
        sa.report(file_id, node.expr.span(), msg);
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
        for alt in &arm.pattern.alts {
            let pattern = vec![convert_pattern(sa, analysis, &arm.pattern)];
            if is_useful(matrix.as_slice(), pattern.as_ref()) {
                matrix.push(pattern);
            } else {
                sa.report(file_id, alt.span(), ErrorMessage::MatchUnreachablePattern);
            }
        }
    }

    let missing_patterns = is_exhaustive(matrix, 1);

    if missing_patterns.is_empty() {
        sa.report(
            file_id,
            node.expr.span(),
            ErrorMessage::MatchUncoveredVariant,
        );
    }
}

fn is_exhaustive(_matrix: Vec<Vec<Pattern>>, _n: usize) -> Vec<Vec<Pattern>> {
    Vec::new()
}

fn is_useful(_matrix: &[Vec<Pattern>], _new_pattern: &[Pattern]) -> bool {
    true
}

#[allow(unused)]
enum LiteralValue {
    Bool(bool),
    Int(i64),
    Float(f64),
}

#[allow(unused)]
enum Pattern {
    Any,
    Literal(LiteralValue),
    Tuple(Vec<Pattern>),
    Ctor(usize, Vec<Pattern>),
    Alt(Vec<Pattern>),
}

fn convert_pattern(sa: &Sema, analysis: &AnalysisData, pattern: &ast::Pattern) -> Pattern {
    if pattern.alts.len() == 1 {
        convert_pattern_alt(sa, analysis, &pattern.alts[0])
    } else {
        Pattern::Alt(
            pattern
                .alts
                .iter()
                .map(|alt| convert_pattern_alt(sa, analysis, alt.as_ref()))
                .collect(),
        )
    }
}

fn convert_pattern_alt(sa: &Sema, analysis: &AnalysisData, pattern: &ast::PatternAlt) -> Pattern {
    match pattern {
        ast::PatternAlt::Underscore(..) => Pattern::Any,

        ast::PatternAlt::Rest(..) => unreachable!(),

        ast::PatternAlt::LitBool(..)
        | ast::PatternAlt::LitChar(..)
        | ast::PatternAlt::LitString(..)
        | ast::PatternAlt::LitInt(..)
        | ast::PatternAlt::LitFloat(..)
        | ast::PatternAlt::Tuple(..) => unreachable!(),

        ast::PatternAlt::Ident(ref pattern_ident) => {
            let ident = analysis
                .map_idents
                .get(pattern_ident.id)
                .expect("missing ident");
            match ident {
                IdentType::EnumVariant(_pattern_enum_id, _type_params, variant_idx) => {
                    Pattern::Ctor(*variant_idx as usize, Vec::new())
                }

                IdentType::Var(_var_id) => Pattern::Any,

                _ => unreachable!(),
            }
        }

        ast::PatternAlt::ClassOrStructOrEnum(ref ident) => {
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
                IdentType::EnumVariant(_pattern_enum_id, _type_params, variant_idx) => {
                    Pattern::Ctor(*variant_idx as usize, subpatterns)
                }

                _ => unreachable!(),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::err;
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
}
