use dora_bytecode::{BytecodeType, Label, Register};
use rustc_hash::FxHashSet;
use std::collections::HashMap;

use super::super::gen_expr;
use super::gen_unreachable;
use crate::generator::int_dispatch::{IntDispatchCase, gen_int_dispatch};
use crate::generator::{AstBytecodeGen, DataDest};
use crate::sema::{ExprId, MatchExpr};

pub(super) fn gen_int_like_match(
    g: &mut AstBytecodeGen,
    expr_id: ExprId,
    e: &MatchExpr,
    dest: Register,
    expr_reg: Register,
    selector_reg: Register,
    selector_ty: BytecodeType,
    selector_temp: Option<Register>,
    arms: Vec<(Option<i64>, usize)>,
) -> Register {
    let selector_expr_id = e.expr.expect("missing match expr");
    let fallthrough_lbl = g.builder.create_label();
    let merge_lbl = g.builder.create_label();

    let mut arm_labels = Vec::with_capacity(e.arms.len());
    for _ in 0..e.arms.len() {
        arm_labels.push(g.builder.create_label());
    }

    let grouped_arms = group_arms_by_value(g, e, &arms, &arm_labels, fallthrough_lbl);

    gen_int_dispatch(
        g,
        selector_ty,
        selector_reg,
        grouped_arms
            .value_groups
            .iter()
            .map(|group| IntDispatchCase {
                value: group.value,
                target_lbl: group.group.target_lbl,
            })
            .collect(),
        grouped_arms.default_group.target_lbl,
        selector_expr_id,
    );

    if let Some(selector_temp) = selector_temp {
        g.free_temp(selector_temp);
    }

    emit_decision_chain_for_group(
        g,
        e,
        &grouped_arms.default_group,
        &arm_labels,
        fallthrough_lbl,
    );
    for group in &grouped_arms.value_groups {
        emit_decision_chain_for_group(g, e, &group.group, &arm_labels, fallthrough_lbl);
    }

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
    g.free_if_temp(expr_reg);

    dest
}

fn group_arms_by_value(
    g: &mut AstBytecodeGen,
    e: &MatchExpr,
    arms: &[(Option<i64>, usize)],
    arm_labels: &[Label],
    fallback_lbl: Label,
) -> GroupedArms {
    let mut arms_by_value = HashMap::<i64, Vec<usize>>::new();
    let mut default_arms = Vec::new();

    for &(value, arm_idx) in arms {
        match value {
            Some(value) => {
                let arms = arms_by_value
                    .entry(value)
                    .or_insert_with(|| default_arms.clone());
                arms.push(arm_idx);
            }
            None => {
                default_arms.push(arm_idx);
                for arms in arms_by_value.values_mut() {
                    arms.push(arm_idx);
                }
            }
        }
    }

    let default_group = if default_arms.is_empty() {
        ArmGroup {
            target_lbl: fallback_lbl,
            candidates: default_arms,
        }
    } else {
        build_arm_group(g, e, default_arms, arm_labels)
    };
    let mut value_groups = arms_by_value
        .into_iter()
        .map(|(value, arms)| {
            let group = build_arm_group(g, e, arms, arm_labels);
            ValueArmGroup { value, group }
        })
        .collect::<Vec<_>>();
    value_groups.sort_by_key(|group| group.value);

    GroupedArms {
        value_groups,
        default_group,
    }
}

fn build_arm_group(
    g: &mut AstBytecodeGen,
    e: &MatchExpr,
    candidates: Vec<usize>,
    arm_labels: &[Label],
) -> ArmGroup {
    let first_arm_idx = *candidates
        .first()
        .expect("missing match arm candidate for int match");
    let target_lbl = if e.arms[first_arm_idx].cond.is_none() {
        arm_labels[first_arm_idx]
    } else {
        g.builder.create_label()
    };

    ArmGroup {
        target_lbl,
        candidates,
    }
}

fn emit_decision_chain_for_group(
    g: &mut AstBytecodeGen,
    e: &MatchExpr,
    group: &ArmGroup,
    arm_labels: &[Label],
    fallback_lbl: Label,
) {
    let needs_decision_chain = group
        .candidates
        .first()
        .is_some_and(|&arm_idx| e.arms[arm_idx].cond.is_some());

    if !needs_decision_chain {
        return;
    }

    g.builder.bind_label(group.target_lbl);
    let mut emitted_guards = FxHashSet::default();

    for &arm_idx in &group.candidates {
        if let Some(cond_id) = e.arms[arm_idx].cond {
            if !emitted_guards.insert(arm_idx) {
                continue;
            }

            let cond_reg = gen_expr(g, cond_id, DataDest::Alloc);
            g.builder.emit_jump_if_true(cond_reg, arm_labels[arm_idx]);
            g.free_if_temp(cond_reg);
        } else {
            g.builder.emit_jump(arm_labels[arm_idx]);
            return;
        }
    }

    g.builder.emit_jump(fallback_lbl);
}

struct GroupedArms {
    value_groups: Vec<ValueArmGroup>,
    default_group: ArmGroup,
}

struct ValueArmGroup {
    value: i64,
    group: ArmGroup,
}

struct ArmGroup {
    target_lbl: Label,
    candidates: Vec<usize>,
}
