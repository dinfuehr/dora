use dora_bytecode::{BytecodeType, Register};

use super::int_like_match::gen_int_like_match;
use crate::generator::AstBytecodeGen;
use crate::sema::{EnumDefinitionId, ExprId, IdentType, MatchExpr, Pattern, PatternId};
use crate::ty::SourceType;

pub(super) fn gen_enum_match(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    expr_ty: &SourceType,
    e: &MatchExpr,
    dest: Register,
    expr_reg: Register,
) -> Option<Register> {
    let SourceType::Enum(enum_id, enum_type_params) = expr_ty else {
        return None;
    };

    let enum_ = g.sa.enum_(*enum_id);
    if !enum_.is_simple_enum() {
        return None;
    }

    let mut arms = Vec::new();

    for (arm_idx, arm) in e.arms.iter().enumerate() {
        collect_enum_pattern(g, arm.pattern, *enum_id, &mut arms, arm_idx)?;
    }

    assert!(!arms.is_empty());

    let selector_expr_id = e.expr.expect("missing match expr");
    let selector_reg = g.alloc_temp(BytecodeType::Int32);
    let bc_enum_id = g.emitter.convert_enum_id(g.sa, *enum_id);
    let bc_enum_type_params = g.convert_tya(enum_type_params);
    let enum_idx = g.builder.add_const_enum(bc_enum_id, bc_enum_type_params);
    g.builder.emit_load_enum_variant(
        selector_reg,
        expr_reg,
        enum_idx,
        g.loc_for_expr(selector_expr_id),
    );

    Some(gen_int_like_match(
        g,
        expr_id,
        e,
        dest,
        expr_reg,
        selector_reg,
        BytecodeType::Int32,
        Some(selector_reg),
        arms,
    ))
}

fn collect_enum_pattern(
    g: &AstBytecodeGen,
    pattern_id: PatternId,
    enum_id: EnumDefinitionId,
    arms: &mut Vec<(Option<i64>, usize)>,
    arm_idx: usize,
) -> Option<()> {
    match g.analysis.pattern(pattern_id) {
        Pattern::Underscore => {
            arms.push((None, arm_idx));
            Some(())
        }
        Pattern::Ident(_) => {
            let ident = g.analysis.get_ident(pattern_id)?;
            match ident {
                IdentType::EnumVariant(pattern_enum_id, _, variant_idx)
                    if pattern_enum_id == enum_id =>
                {
                    arms.push((Some(i64::from(variant_idx)), arm_idx));
                    Some(())
                }
                _ => None,
            }
        }
        Pattern::Ctor(ctor) => {
            assert!(ctor.fields.is_empty());

            let ident = g.analysis.get_ident(pattern_id)?;
            match ident {
                IdentType::EnumVariant(pattern_enum_id, _, variant_idx)
                    if pattern_enum_id == enum_id =>
                {
                    arms.push((Some(i64::from(variant_idx)), arm_idx));
                    Some(())
                }
                _ => None,
            }
        }
        Pattern::Alt(alt) => {
            if alt.patterns.is_empty() {
                return None;
            }

            for &alt_pattern_id in &alt.patterns {
                collect_enum_pattern(g, alt_pattern_id, enum_id, arms, arm_idx)?;
            }

            Some(())
        }
        _ => None,
    }
}
