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
                check_coverage(self.sa, self.analysis, self.file_id, expr);
                visit::walk_expr(self, e);
            }
            ast::ExprData::Lambda(..) => (),
            _ => {
                visit::walk_expr(self, e);
            }
        }
    }
}

fn check_coverage(
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
