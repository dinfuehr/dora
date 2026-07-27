use std::collections::HashMap;

use dora_bytecode::{
    BytecodeBody, BytecodeType, BytecodeTypeArray, ConstPoolEntry, Location, Register,
};

use crate::expr_block_always_returns;
use crate::program_emitter::Emitter;
use crate::sema::{
    AnalysisData, Element, EnumDefinitionId, FctDefinition, FctDefinitionId, Sema, VarLocation,
    find_impl,
};
use crate::{SourceType, TraitType};

use super::expr::gen_expr;
use super::pattern::{destruct_pattern_or_fail, setup_pattern_vars};
use super::{
    AstBytecodeGen, BytecodeBuilder, DataDest, SELF_VAR_ID, set_var_reg, store_in_context,
};

pub fn generate_fct_id(sa: &Sema, emitter: &mut Emitter, id: FctDefinitionId) -> BytecodeBody {
    let fct = sa.fct(id);
    let analysis = fct.analysis();

    generate_fct(sa, emitter, &fct, analysis)
}

pub fn generate_enum_equals(
    sa: &Sema,
    emitter: &mut Emitter,
    fct: &FctDefinition,
    enum_id: EnumDefinitionId,
) -> BytecodeBody {
    let enum_ = sa.enum_(enum_id);
    assert!(!enum_.is_simple_enum());

    let type_params = enum_.type_param_definition(sa).identity_type_params(sa);
    let enum_ty = SourceType::Enum(enum_id, type_params.clone());
    let bytecode_enum_ty = emitter.convert_ty(sa, enum_ty);
    let bytecode_enum_id = emitter.convert_enum_id(sa, enum_id);
    let bytecode_type_params = emitter.convert_tya(sa, &type_params);
    let enum_const = ConstPoolEntry::Enum(bytecode_enum_id, bytecode_type_params.clone());
    let location = sa.compute_loc(fct.file_id, fct.span);

    let equals_trait_ty = TraitType::from_trait_id(sa.known.traits.equals());
    let equals_method_name = sa.interner.intern("equals");
    let equals_method_id = sa
        .trait_(equals_trait_ty.trait_id)
        .get_method(equals_method_name, false)
        .expect("Equals::equals missing");

    let mut builder = BytecodeBuilder::new();
    builder.push_scope();

    let lhs = builder.alloc_var(bytecode_enum_ty.clone());
    let rhs = builder.alloc_var(bytecode_enum_ty);
    let lhs_variant = builder.alloc_var(BytecodeType::Int32);
    let rhs_variant = builder.alloc_var(BytecodeType::Int32);
    let condition = builder.alloc_var(BytecodeType::Bool);
    let false_label = builder.create_label();
    let variant_labels = enum_
        .variant_ids()
        .iter()
        .map(|_| builder.create_label())
        .collect::<Vec<_>>();
    debug_assert!(
        enum_
            .variant_ids()
            .iter()
            .enumerate()
            .all(|(index, &variant_id)| sa.variant(variant_id).index as usize == index)
    );

    let enum_idx = builder.add_const(enum_const);
    builder.emit_load_enum_variant(lhs_variant, lhs, enum_idx, location);
    builder.emit_load_enum_variant(rhs_variant, rhs, enum_idx, location);
    builder.emit_test_eq(condition, lhs_variant, rhs_variant);
    builder.emit_jump_if_false(condition, false_label);
    let jump_table_idx = builder.add_const_jump_table(variant_labels.clone(), false_label);
    builder.emit_switch(lhs_variant, jump_table_idx);

    for (&variant_id, variant_label) in enum_.variant_ids().iter().zip(variant_labels) {
        let variant = sa.variant(variant_id);
        builder.bind_label(variant_label);

        for (field_index, &field_id) in variant.field_ids().iter().enumerate() {
            let field_ty = sa.field(field_id).ty();
            let bytecode_field_ty = emitter.convert_ty(sa, field_ty.clone());
            let lhs_field = builder.alloc_temp(bytecode_field_ty.clone());
            let rhs_field = builder.alloc_temp(bytecode_field_ty);
            let field_idx = builder.add_const_enum_element(
                bytecode_enum_id,
                bytecode_type_params.clone(),
                variant.index,
                field_index.try_into().expect("enum field index overflow"),
            );

            builder.emit_load_enum_element(lhs_field, lhs, field_idx, location);
            builder.emit_load_enum_element(rhs_field, rhs, field_idx, location);
            emit_equals_call(
                sa,
                emitter,
                &mut builder,
                enum_id,
                &equals_trait_ty,
                equals_method_id,
                field_ty,
                condition,
                lhs_field,
                rhs_field,
                location,
            );
            builder.emit_jump_if_false(condition, false_label);
            builder.free_temp(rhs_field);
            builder.free_temp(lhs_field);
        }

        builder.emit_const_true(condition);
        builder.emit_ret(condition);
    }

    builder.bind_label(false_label);
    builder.emit_const_false(condition);
    builder.emit_ret(condition);
    builder.pop_scope();
    builder.generate()
}

#[allow(clippy::too_many_arguments)]
fn emit_equals_call(
    sa: &Sema,
    emitter: &mut Emitter,
    builder: &mut BytecodeBuilder,
    enum_id: EnumDefinitionId,
    equals_trait_ty: &TraitType,
    equals_method_id: FctDefinitionId,
    field_ty: SourceType,
    condition: Register,
    lhs: Register,
    rhs: Register,
    location: Location,
) {
    let is_generic = field_ty.is_type_param() || field_ty.is_assoc() || field_ty.is_generic_assoc();
    let callee_idx = if is_generic {
        builder.add_const(ConstPoolEntry::Generic {
            object_type: emitter.convert_ty(sa, field_ty),
            trait_ty: emitter.convert_trait_ty(sa, equals_trait_ty),
            fct_id: emitter.convert_function_id(sa, equals_method_id),
            fct_type_params: BytecodeTypeArray::empty(),
        })
    } else {
        let enum_ = sa.enum_(enum_id);
        let impl_match = find_impl(
            sa,
            enum_,
            field_ty,
            enum_.type_param_definition(sa),
            equals_trait_ty.clone(),
        )
        .expect("enum field should implement Equals");
        let method_id = sa
            .impl_(impl_match.id)
            .get_method_for_trait_method_id(equals_method_id)
            .expect("Equals implementation missing equals method");
        builder.add_const(ConstPoolEntry::Fct(
            emitter.convert_function_id(sa, method_id),
            emitter.convert_tya(sa, &impl_match.bindings),
        ))
    };

    if is_generic {
        builder.emit_invoke_generic_direct(condition, callee_idx, &[lhs, rhs], location);
    } else {
        builder.emit_invoke_direct(condition, callee_idx, &[lhs, rhs], location);
    }
}

pub fn generate_fct(
    sa: &Sema,
    emitter: &mut Emitter,
    fct: &FctDefinition,
    src: &AnalysisData,
) -> BytecodeBody {
    let frontend_type_params_len = fct.type_param_definition(sa).type_param_count();
    let ast_bytecode_generator = AstBytecodeGen {
        sa,
        emitter,
        frontend_type_params_len,
        type_params_len: frontend_type_params_len + usize::from(fct.needs_self_type_param(sa)),
        type_param_definition_id: fct.type_param_definition_id,
        is_lambda: fct.is_lambda(),
        return_type: fct.return_type(),
        file_id: fct.file_id,
        span: fct.span,
        analysis: src,

        builder: BytecodeBuilder::new(),
        loops: Vec::new(),
        var_registers: HashMap::new(),
        unit_register: None,
        entered_contexts: Vec::new(),
    };
    generate_fct_impl(ast_bytecode_generator)
}

fn generate_fct_impl(mut g: AstBytecodeGen) -> BytecodeBody {
    g.push_scope();
    create_params(&mut g);
    g.enter_function_context();
    store_params_in_context(&mut g);
    emit_function_body(&mut g);
    g.leave_function_context();
    g.pop_scope();
    g.builder.generate()
}

fn create_params(g: &mut AstBytecodeGen) {
    if g.analysis.has_self() {
        let vars = g.analysis.vars();
        let var_self = vars.get_self();
        // The self type already includes Ref wrapper for mutating methods on value types.
        let var_ty = var_self.ty.clone();

        let bty = if g.is_lambda {
            g.lambda_object_type()
        } else {
            g.emitter.convert_ty(g.sa, var_ty)
        };
        let reg = g.alloc_var(bty);
        set_var_reg(g, SELF_VAR_ID, reg);
    }

    for &param_id in g.analysis.param_pattern_ids() {
        let ty = g.ty(param_id);
        let bty = g.emitter.convert_ty(g.sa, ty.clone());
        g.alloc_var(bty);
    }
}

fn store_params_in_context(g: &mut AstBytecodeGen) {
    let next_register_idx = if g.analysis.has_self() {
        let vars = g.analysis.vars();
        let var_self = vars.get_self();
        let reg = Register(0);

        match var_self.location {
            VarLocation::Context(scope_id, field_id) => {
                store_in_context(g, reg, scope_id, field_id, g.loc(g.span));
            }

            VarLocation::Stack => {
                // Nothing to do.
            }
        }

        1
    } else {
        0
    };

    let param_pattern_ids = g.analysis.param_pattern_ids();

    for (param_idx, &pattern_id) in param_pattern_ids.iter().enumerate() {
        let reg = Register(next_register_idx + param_idx);
        let pattern = g.analysis.pattern(pattern_id);

        if pattern.to_ident().is_some() {
            let var_id = g.analysis.get_var_id(pattern_id).unwrap();
            let vars = g.analysis.vars();
            let var = vars.get_var(var_id);

            match var.location {
                VarLocation::Context(scope_id, field_id) => {
                    store_in_context(g, reg, scope_id, field_id, g.loc(g.span));
                }

                VarLocation::Stack => {
                    set_var_reg(g, var_id, reg);
                }
            }
        } else {
            // Get type from AST param since that's where it's stored
            let ty = g.analysis.ty(pattern_id);
            setup_pattern_vars(g, pattern_id);
            destruct_pattern_or_fail(g, pattern_id, reg, ty);
        }
    }
}

fn emit_function_body(g: &mut AstBytecodeGen) {
    let mut needs_return = true;

    let root_expr_id = g.analysis.root_expr_id();
    let block = g.analysis.expr(root_expr_id).as_block();

    for stmt_id in &block.stmts {
        g.visit_stmt(*stmt_id);
    }

    if let Some(tail_expr_id) = block.expr {
        let reg = gen_expr(g, tail_expr_id, DataDest::Alloc);

        if !expr_block_always_returns(g.analysis, block) {
            g.builder.emit_ret(reg);
        }

        needs_return = false;
        g.free_if_temp(reg);
    }

    if needs_return && g.return_type.is_unit() {
        let dest = g.ensure_unit_register();
        g.builder.emit_ret(dest);
    }
}
