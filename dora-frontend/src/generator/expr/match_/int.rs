use dora_bytecode::{BytecodeType, Register};

use super::int_like_match;
use crate::generator::AstBytecodeGen;
use crate::sema::{ExprId, IdentType, MatchExpr, Pattern, PatternId};
use crate::ty::SourceType;

pub(super) fn gen_int_match(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    expr_ty: &SourceType,
    e: &MatchExpr,
    dest: Register,
    expr_reg: Register,
) -> Option<Register> {
    let ty = match expr_ty {
        SourceType::UInt8 => BytecodeType::UInt8,
        SourceType::Int32 => BytecodeType::Int32,
        SourceType::Int64 => BytecodeType::Int64,
        _ => return None,
    };

    let mut arms = Vec::new();

    for (arm_idx, arm) in e.arms.iter().enumerate() {
        collect_int_pattern(g, arm.pattern, &mut arms, arm_idx)?;
    }

    assert!(!arms.is_empty());

    Some(int_like_match::gen_int_like_match(
        g, expr_id, e, dest, expr_reg, expr_reg, ty, None, arms,
    ))
}

fn collect_int_pattern(
    g: &AstBytecodeGen,
    pattern_id: PatternId,
    arms: &mut Vec<(Option<i64>, usize)>,
    arm_idx: usize,
) -> Option<()> {
    match g.analysis.pattern(pattern_id) {
        Pattern::LitInt(_) => {
            let value = g
                .analysis
                .get_const_value(pattern_id)
                .expect("missing int pattern value")
                .to_i64()
                .expect("integer expected");
            arms.push((Some(value), arm_idx));
            Some(())
        }
        Pattern::Ident(_) => {
            let ident = g.analysis.get_ident(pattern_id).expect("missing ident");
            match ident {
                IdentType::Const(const_id) => {
                    let value =
                        g.sa.const_(const_id)
                            .value()
                            .to_i64()
                            .expect("integer constant expected");
                    arms.push((Some(value), arm_idx));
                    Some(())
                }
                IdentType::Var(_) => None,
                _ => unreachable!("unexpected ident pattern in int match"),
            }
        }
        Pattern::Ctor(ctor) => {
            assert!(!ctor.has_parens);
            assert!(ctor.fields.is_empty());

            let ident = g.analysis.get_ident(pattern_id).expect("missing ident");
            let IdentType::Const(const_id) = ident else {
                unreachable!("unexpected ctor pattern in int match");
            };
            let value =
                g.sa.const_(const_id)
                    .value()
                    .to_i64()
                    .expect("integer constant expected");
            arms.push((Some(value), arm_idx));
            Some(())
        }
        Pattern::Underscore => {
            arms.push((None, arm_idx));
            Some(())
        }
        Pattern::Alt(alt) => {
            assert!(!alt.patterns.is_empty());

            for &alt_pattern_id in &alt.patterns {
                collect_int_pattern(g, alt_pattern_id, arms, arm_idx)?;
            }

            Some(())
        }
        _ => unreachable!("unexpected pattern in int match"),
    }
}
