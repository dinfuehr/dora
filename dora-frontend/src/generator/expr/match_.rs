use dora_bytecode::{BytecodeType, Label, Register};
use dora_parser::Span;

use super::{ensure_register, gen_expr};
use crate::generator::pattern::{destruct_pattern, setup_pattern_vars};
use crate::generator::{AstBytecodeGen, DataDest};
use crate::sema::{
    EnumDefinitionId, ExprId, IdentType, MatchArmExpr, MatchExpr, Pattern, PatternId,
};
use crate::ty::{SourceType, SourceTypeArray};

pub(super) fn gen_match(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &MatchExpr,
    dest: DataDest,
) -> Register {
    let result_ty = g.ty(expr_id);
    let expr_id_inner = e.expr.expect("missing match expr");
    let expr_ty = g.ty(expr_id_inner);

    let result_bc_ty = g.emitter.convert_ty_reg(g.sa, result_ty);
    let dest = ensure_register(g, dest, result_bc_ty);

    let fallthrough_lbl = g.builder.create_label();
    let merge_lbl = g.builder.create_label();

    let expr_reg = gen_expr(g, expr_id_inner, DataDest::Alloc);

    if let Some(simple_enum_match) = analyze_simple_enum_match(g, &expr_ty, e) {
        return gen_simple_enum_match(
            g,
            expr_id,
            expr_id_inner,
            e,
            dest,
            expr_reg,
            simple_enum_match,
        );
    }

    let num_arms = e.arms.len();

    let mut arm_labels = Vec::with_capacity(num_arms);

    for _ in 0..num_arms {
        arm_labels.push(g.builder.create_label());
    }

    arm_labels.push(fallthrough_lbl);

    for (idx, arm) in e.arms.iter().enumerate() {
        let arm_lbl = arm_labels[idx];
        g.builder.bind_label(arm_lbl);

        let next_arm_lbl = arm_labels[idx + 1];

        g.push_scope();

        setup_pattern_vars(g, arm.pattern);
        destruct_pattern(
            g,
            arm.pattern,
            expr_reg,
            expr_ty.clone(),
            Some(next_arm_lbl),
        );

        if let Some(cond_id) = arm.cond {
            let cond_reg = gen_expr(g, cond_id, DataDest::Alloc);
            g.builder.emit_jump_if_false(cond_reg, next_arm_lbl);
            g.free_if_temp(cond_reg);
        }

        gen_expr(g, arm.value, DataDest::Reg(dest));

        g.builder.emit_jump(merge_lbl);
        g.pop_scope();
    }

    g.builder.bind_label(fallthrough_lbl);
    let span = g.span_for_expr(expr_id);
    gen_unreachable(g, span);

    g.builder.bind_label(merge_lbl);
    g.free_if_temp(expr_reg);

    dest
}

struct SimpleEnumMatch {
    enum_id: EnumDefinitionId,
    enum_type_params: SourceTypeArray,
    arm_for_variant: Vec<Option<usize>>,
    default_arm: Option<usize>,
}

enum SimpleEnumArmPattern {
    Variants(Vec<u32>),
    Default,
}

fn analyze_simple_enum_match(
    g: &AstBytecodeGen,
    expr_ty: &SourceType,
    e: &MatchExpr,
) -> Option<SimpleEnumMatch> {
    let SourceType::Enum(enum_id, enum_type_params) = expr_ty else {
        return None;
    };

    let enum_ = g.sa.enum_(*enum_id);
    if !enum_.is_simple_enum() || enum_.variant_ids().is_empty() {
        return None;
    }

    let mut arm_for_variant = vec![None; enum_.variant_ids().len()];
    let mut default_arm = None;

    for (arm_idx, arm) in e.arms.iter().enumerate() {
        if arm.cond.is_some() {
            return None;
        }

        if default_arm.is_some() {
            return None;
        }

        match simple_enum_arm_pattern(g, arm, *enum_id)? {
            SimpleEnumArmPattern::Variants(variants) => {
                if variants.is_empty() {
                    return None;
                }

                for variant_idx in variants {
                    let target = &mut arm_for_variant[variant_idx as usize];
                    if target.is_none() {
                        *target = Some(arm_idx);
                    }
                }
            }
            SimpleEnumArmPattern::Default => {
                default_arm = Some(arm_idx);
            }
        }
    }

    Some(SimpleEnumMatch {
        enum_id: *enum_id,
        enum_type_params: enum_type_params.clone(),
        arm_for_variant,
        default_arm,
    })
}

fn simple_enum_arm_pattern(
    g: &AstBytecodeGen,
    arm: &MatchArmExpr,
    enum_id: EnumDefinitionId,
) -> Option<SimpleEnumArmPattern> {
    simple_enum_pattern(g, arm.pattern, enum_id)
}

fn simple_enum_pattern(
    g: &AstBytecodeGen,
    pattern_id: PatternId,
    enum_id: EnumDefinitionId,
) -> Option<SimpleEnumArmPattern> {
    match g.analysis.pattern(pattern_id) {
        Pattern::Underscore => Some(SimpleEnumArmPattern::Default),
        Pattern::Ident(_) => {
            let ident = g.analysis.get_ident(pattern_id)?;
            match ident {
                IdentType::EnumVariant(pattern_enum_id, _, variant_idx)
                    if pattern_enum_id == enum_id =>
                {
                    Some(SimpleEnumArmPattern::Variants(vec![variant_idx]))
                }
                _ => None,
            }
        }
        Pattern::Ctor(ctor) if ctor.fields.is_empty() => {
            let ident = g.analysis.get_ident(pattern_id)?;
            match ident {
                IdentType::EnumVariant(pattern_enum_id, _, variant_idx)
                    if pattern_enum_id == enum_id =>
                {
                    Some(SimpleEnumArmPattern::Variants(vec![variant_idx]))
                }
                _ => None,
            }
        }
        Pattern::Alt(alt) => {
            let mut variants = Vec::new();

            for &alt_pattern_id in &alt.patterns {
                match simple_enum_pattern(g, alt_pattern_id, enum_id)? {
                    SimpleEnumArmPattern::Variants(mut alt_variants) => {
                        variants.append(&mut alt_variants);
                    }
                    SimpleEnumArmPattern::Default => {
                        return Some(SimpleEnumArmPattern::Default);
                    }
                }
            }

            Some(SimpleEnumArmPattern::Variants(variants))
        }
        _ => None,
    }
}

fn gen_simple_enum_match(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    expr_id_inner: ExprId,
    e: &MatchExpr,
    dest: Register,
    expr_reg: Register,
    simple_enum_match: SimpleEnumMatch,
) -> Register {
    let fallthrough_lbl = g.builder.create_label();
    let merge_lbl = g.builder.create_label();

    let mut arm_labels = Vec::with_capacity(e.arms.len());
    for _ in 0..e.arms.len() {
        arm_labels.push(g.builder.create_label());
    }

    let default_lbl = simple_enum_match
        .default_arm
        .map(|arm_idx| arm_labels[arm_idx])
        .unwrap_or(fallthrough_lbl);
    let targets = simple_enum_match
        .arm_for_variant
        .iter()
        .map(|arm_idx| arm_idx.map(|idx| arm_labels[idx]).unwrap_or(default_lbl))
        .collect::<Vec<Label>>();

    let selector_reg = g.alloc_temp(BytecodeType::Int32);
    let bc_enum_id = g.emitter.convert_enum_id(g.sa, simple_enum_match.enum_id);
    let bc_enum_type_params = g.convert_tya(&simple_enum_match.enum_type_params);
    let enum_idx = g.builder.add_const_enum(bc_enum_id, bc_enum_type_params);
    g.builder.emit_load_enum_variant(
        selector_reg,
        expr_reg,
        enum_idx,
        g.loc_for_expr(expr_id_inner),
    );

    let jump_table_idx = g.builder.add_const_jump_table(targets, default_lbl);
    g.builder.emit_switch(selector_reg, jump_table_idx);

    for (idx, arm) in e.arms.iter().enumerate() {
        g.builder.bind_label(arm_labels[idx]);
        g.push_scope();
        gen_expr(g, arm.value, DataDest::Reg(dest));
        g.builder.emit_jump(merge_lbl);
        g.pop_scope();
    }

    g.builder.bind_label(fallthrough_lbl);
    let span = g.span_for_expr(expr_id);
    gen_unreachable(g, span);

    g.builder.bind_label(merge_lbl);
    g.free_temp(selector_reg);
    g.free_if_temp(expr_reg);

    dest
}

fn gen_unreachable(g: &mut AstBytecodeGen, span: Span) {
    let return_type = g.return_type.clone();
    let register_bty = g.emitter.convert_ty_reg(g.sa, return_type.clone());
    let dest = g.alloc_temp(register_bty);
    let fct_type_params = g.convert_tya(&SourceTypeArray::single(return_type));
    let fct_id = g
        .emitter
        .convert_function_id(g.sa, g.sa.known.functions.unreachable());
    let fct_idx = g.builder.add_const_fct_types(fct_id, fct_type_params);
    g.builder
        .emit_invoke_direct(dest, fct_idx, &[], g.loc(span));
    g.builder.emit_ret(dest);
    g.free_temp(dest);
}
