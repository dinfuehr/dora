use dora_bytecode::{BytecodeType, Label, Register};

use crate::generator::AstBytecodeGen;
use crate::sema::ExprId;

const MAX_SIMPLE_JUMP_TABLE_ENTRIES: usize = 128;
const MIN_SIMPLE_JUMP_TABLE_TARGETS: usize = 3;

pub(super) fn gen_int_dispatch(
    g: &mut AstBytecodeGen,
    ty: BytecodeType,
    selector_reg: Register,
    mut cases: Vec<IntDispatchCase>,
    default_lbl: Label,
    expr_id: ExprId,
) {
    cases.sort_by_key(|case| case.value);
    debug_assert!(
        cases
            .windows(2)
            .all(|window| window[0].value < window[1].value)
    );

    let dispatch = IntDispatch {
        ty,
        selector_reg,
        cases,
        default_lbl,
    };

    if dispatch.cases.is_empty() {
        g.builder.emit_jump(dispatch.default_lbl);
    } else if dispatch.can_use_jump_table() {
        dispatch.emit_jump_table(g, expr_id);
    } else {
        dispatch.emit_binary_search(g);
    }
}

struct IntDispatch {
    ty: BytecodeType,
    selector_reg: Register,
    cases: Vec<IntDispatchCase>,
    default_lbl: Label,
}

#[derive(Clone, Copy)]
pub(super) struct IntDispatchCase {
    pub(super) value: i64,
    pub(super) target_lbl: Label,
}

impl IntDispatch {
    fn can_use_jump_table(&self) -> bool {
        if self.cases.len() < MIN_SIMPLE_JUMP_TABLE_TARGETS {
            return false;
        }

        let first_value = self.cases.first().expect("missing first value").value;
        let last_value = self.cases.last().expect("missing last value").value;
        let Some(span) = last_value
            .checked_sub(first_value)
            .and_then(|span| span.checked_add(1))
        else {
            return false;
        };

        span <= MAX_SIMPLE_JUMP_TABLE_ENTRIES as i64
    }

    fn emit_jump_table(&self, g: &mut AstBytecodeGen, expr_id: ExprId) {
        let first_value = self.cases.first().expect("missing first value").value;
        let last_value = self.cases.last().expect("missing last value").value;

        let (selector_reg, selector_temp) =
            self.emit_jump_table_selector(g, expr_id, first_value, last_value);

        let mut cases = self.cases.iter().peekable();
        let targets = (first_value..=last_value)
            .map(|value| {
                if cases.peek().is_some_and(|case| case.value == value) {
                    cases.next().expect("missing case").target_lbl
                } else {
                    self.default_lbl
                }
            })
            .collect::<Vec<Label>>();

        let jump_table_idx = g.builder.add_const_jump_table(targets, self.default_lbl);
        g.builder.emit_switch(selector_reg, jump_table_idx);

        if let Some(selector_temp) = selector_temp {
            g.free_temp(selector_temp);
        }
    }

    fn emit_binary_search(&self, g: &mut AstBytecodeGen) {
        let tmp = g.alloc_temp(BytecodeType::Bool);
        self.emit_binary_search_range(g, &self.cases, tmp);
        g.free_temp(tmp);
    }

    fn emit_binary_search_range(
        &self,
        g: &mut AstBytecodeGen,
        cases: &[IntDispatchCase],
        tmp: Register,
    ) {
        if cases.is_empty() {
            g.builder.emit_jump(self.default_lbl);
            return;
        }

        let mid_idx = cases.len() / 2;
        let mid_case = &cases[mid_idx];
        let value_reg = g.alloc_temp(self.ty.clone());
        emit_const_int(g, value_reg, &self.ty, mid_case.value);
        g.builder.emit_test_eq(tmp, self.selector_reg, value_reg);
        g.builder.emit_jump_if_true(tmp, mid_case.target_lbl);

        if cases.len() == 1 {
            g.free_temp(value_reg);
            g.builder.emit_jump(self.default_lbl);
            return;
        }

        let right_lbl = g.builder.create_label();
        g.builder.emit_test_lt(tmp, self.selector_reg, value_reg);
        g.builder.emit_jump_if_false(tmp, right_lbl);
        g.free_temp(value_reg);

        self.emit_binary_search_range(g, &cases[..mid_idx], tmp);

        g.builder.bind_label(right_lbl);
        self.emit_binary_search_range(g, &cases[mid_idx + 1..], tmp);
    }

    fn emit_jump_table_selector(
        &self,
        g: &mut AstBytecodeGen,
        expr_id: ExprId,
        first_value: i64,
        last_value: i64,
    ) -> (Register, Option<Register>) {
        match self.ty {
            BytecodeType::Int32 => {
                if first_value == 0 {
                    (self.selector_reg, None)
                } else {
                    let selector_reg = g.alloc_temp(BytecodeType::Int32);
                    let first_value_reg = g.alloc_temp(BytecodeType::Int32);
                    g.builder
                        .emit_const_int32(first_value_reg, first_value as i32);
                    g.builder
                        .emit_sub(selector_reg, self.selector_reg, first_value_reg);
                    g.free_temp(first_value_reg);
                    (selector_reg, Some(selector_reg))
                }
            }
            BytecodeType::UInt8 => {
                let base_reg = g.alloc_temp(BytecodeType::Int32);
                let fct_id = g.sa.known.functions.uint8_to_int32();
                let fct_idx = g
                    .builder
                    .add_const_fct(g.emitter.convert_function_id(g.sa, fct_id));
                g.builder.emit_invoke_direct(
                    base_reg,
                    fct_idx,
                    &[self.selector_reg],
                    g.loc_for_expr(expr_id),
                );

                if first_value == 0 {
                    (base_reg, Some(base_reg))
                } else {
                    let selector_reg = g.alloc_temp(BytecodeType::Int32);
                    let first_value_reg = g.alloc_temp(BytecodeType::Int32);
                    g.builder
                        .emit_const_int32(first_value_reg, first_value as i32);
                    g.builder.emit_sub(selector_reg, base_reg, first_value_reg);
                    g.free_temp(first_value_reg);
                    g.free_temp(base_reg);
                    (selector_reg, Some(selector_reg))
                }
            }
            BytecodeType::Int64 => {
                emit_int64_jump_table_range_check(
                    g,
                    self.selector_reg,
                    first_value,
                    last_value,
                    self.default_lbl,
                );

                let base_reg = if first_value == 0 {
                    self.selector_reg
                } else {
                    let base_reg = g.alloc_temp(BytecodeType::Int64);
                    let first_value_reg = g.alloc_temp(BytecodeType::Int64);
                    g.builder.emit_const_int64(first_value_reg, first_value);
                    g.builder
                        .emit_sub(base_reg, self.selector_reg, first_value_reg);
                    g.free_temp(first_value_reg);
                    base_reg
                };

                let selector_reg = g.alloc_temp(BytecodeType::Int32);
                let fct_id = g.sa.known.functions.int64_to_int32();
                let fct_idx = g
                    .builder
                    .add_const_fct(g.emitter.convert_function_id(g.sa, fct_id));
                g.builder.emit_invoke_direct(
                    selector_reg,
                    fct_idx,
                    &[base_reg],
                    g.loc_for_expr(expr_id),
                );
                if first_value != 0 {
                    g.free_temp(base_reg);
                }
                (selector_reg, Some(selector_reg))
            }
            _ => unreachable!(),
        }
    }
}

fn emit_int64_jump_table_range_check(
    g: &mut AstBytecodeGen,
    selector_reg: Register,
    first_value: i64,
    last_value: i64,
    default_lbl: Label,
) {
    let tmp = g.alloc_temp(BytecodeType::Bool);

    let first_value_reg = g.alloc_temp(BytecodeType::Int64);
    g.builder.emit_const_int64(first_value_reg, first_value);
    g.builder.emit_test_lt(tmp, selector_reg, first_value_reg);
    g.builder.emit_jump_if_true(tmp, default_lbl);
    g.free_temp(first_value_reg);

    let last_value_reg = g.alloc_temp(BytecodeType::Int64);
    g.builder.emit_const_int64(last_value_reg, last_value);
    g.builder.emit_test_gt(tmp, selector_reg, last_value_reg);
    g.builder.emit_jump_if_true(tmp, default_lbl);
    g.free_temp(last_value_reg);

    g.free_temp(tmp);
}

fn emit_const_int(g: &mut AstBytecodeGen, dest: Register, ty: &BytecodeType, value: i64) {
    match ty {
        BytecodeType::UInt8 => g.builder.emit_const_uint8(dest, value as u8),
        BytecodeType::Int32 => g.builder.emit_const_int32(dest, value as i32),
        BytecodeType::Int64 => g.builder.emit_const_int64(dest, value),
        _ => unreachable!(),
    }
}
