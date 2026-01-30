use dora_bytecode::{BytecodeType, ConstPoolEntry, ConstPoolIdx, Location, Register};

mod as_;
mod assign;
mod bin;
mod block;
mod break_;
mod call;
mod continue_;
mod field;
mod for_;
mod if_;
mod is;
mod lambda;
mod lit;
mod match_;
mod method_call;
mod path;
mod return_;
mod stmt;
mod template;
mod tuple;
mod un;
mod while_;

use self::as_::gen_expr_as;
use self::bin::gen_expr_bin;
use self::block::gen_expr_block;
use self::break_::gen_expr_break;
use self::call::gen_expr_call;
use self::continue_::gen_expr_continue;
use self::field::gen_expr_field;
use self::for_::gen_expr_for;
use self::if_::gen_expr_if;
use self::is::gen_expr_is;
use self::lambda::gen_expr_lambda;
use self::lit::{
    gen_expr_lit_bool, gen_expr_lit_char, gen_expr_lit_float, gen_expr_lit_int, gen_expr_lit_string,
};
use self::match_::gen_match;
use self::method_call::gen_expr_method_call;
use self::path::gen_expr_path;
use self::return_::gen_expr_return;
pub(in crate::generator) use self::stmt::{gen_stmt_expr, gen_stmt_let};
use self::template::gen_expr_template;
use self::tuple::gen_expr_tuple;
use self::un::gen_expr_un;
use self::while_::gen_expr_while;
use super::{AstBytecodeGen, DataDest};
use crate::sema::{CallType, Expr, ExprId, FctDefinition};
use crate::specialize::{replace_type, specialize_type};
use crate::specialize_ty_for_trait_object;
use crate::ty::SourceType;

pub(super) fn gen_expr(g: &mut AstBytecodeGen, expr_id: ExprId, dest: DataDest) -> Register {
    let expr = g.analysis.expr(expr_id);

    match expr {
        Expr::Un(e) => gen_expr_un(g, expr_id, e, dest),
        Expr::Assign(e) => self::assign::gen_expr_assign(g, expr_id, e, dest),
        Expr::Bin(e) => gen_expr_bin(g, expr_id, e, dest),
        Expr::Field(e) => gen_expr_field(g, expr_id, e, dest),
        Expr::Block(e) => gen_expr_block(g, expr_id, e, dest),
        Expr::If(e) => gen_expr_if(g, expr_id, e, dest),
        Expr::Template(e) => gen_expr_template(g, expr_id, e, dest),
        Expr::LitChar(_) => gen_expr_lit_char(g, expr_id, dest),
        Expr::LitInt(_) => gen_expr_lit_int(g, expr_id, dest),
        Expr::LitFloat(_) => gen_expr_lit_float(g, expr_id, dest),
        Expr::LitStr(_) => gen_expr_lit_string(g, expr_id, dest),
        Expr::LitBool(e) => gen_expr_lit_bool(g, *e, dest),
        Expr::Path(_) => gen_expr_path(g, expr_id, dest),
        Expr::QualifiedPath(_) => unreachable!(),
        Expr::Call(e) => gen_expr_call(g, expr_id, e, dest),
        Expr::As(e) => gen_expr_as(g, expr_id, e, dest),
        Expr::Is(e) => gen_expr_is(g, expr_id, e, dest),
        Expr::Tuple(e) => gen_expr_tuple(g, expr_id, e, dest),
        Expr::Paren(inner_expr_id) => gen_expr(g, *inner_expr_id, dest),
        Expr::Match(e) => gen_match(g, expr_id, e, dest),
        Expr::Lambda(e) => gen_expr_lambda(g, expr_id, e, dest),
        Expr::For(e) => gen_expr_for(g, expr_id, e, dest),
        Expr::While(e) => gen_expr_while(g, expr_id, e, dest),
        Expr::Break => gen_expr_break(g, expr_id, dest),
        Expr::Continue => gen_expr_continue(g, expr_id, dest),
        Expr::Return(e) => gen_expr_return(g, expr_id, e, dest),
        Expr::MethodCall(e) => gen_expr_method_call(g, expr_id, e, dest),
        Expr::Error => unreachable!(),
    }
}

pub(super) fn emit_invoke_direct(
    g: &mut AstBytecodeGen,
    return_type: SourceType,
    return_reg: Register,
    callee_id: ConstPoolIdx,
    location: Location,
) {
    if return_type.is_unit() {
        let reg = g.ensure_unit_register();
        g.builder.emit_invoke_direct(reg, callee_id, location);
    } else {
        g.builder
            .emit_invoke_direct(return_reg, callee_id, location);
    }
}

pub(super) fn emit_invoke_generic_direct(
    g: &mut AstBytecodeGen,
    return_type: SourceType,
    return_reg: Register,
    callee_id: ConstPoolIdx,
    location: Location,
) {
    if return_type.is_unit() {
        let dest = g.ensure_unit_register();
        g.builder
            .emit_invoke_generic_direct(dest, callee_id, location);
    } else {
        g.builder
            .emit_invoke_generic_direct(return_reg, callee_id, location);
    }
}

pub(super) fn ensure_register(
    g: &mut AstBytecodeGen,
    dest: DataDest,
    ty: BytecodeType,
) -> Register {
    match dest {
        DataDest::Alloc => g.alloc_temp(ty),
        DataDest::Reg(reg) => reg,
    }
}

pub(super) fn add_const_pool_entry_for_call(
    g: &mut AstBytecodeGen,
    fct: &FctDefinition,
    call_type: &CallType,
) -> ConstPoolIdx {
    match call_type {
        CallType::GenericMethod {
            object_type,
            trait_ty,
            fct_id,
            fct_type_params,
        }
        | CallType::GenericStaticMethod {
            object_type,
            trait_ty,
            fct_id,
            fct_type_params,
        } => {
            let bc_object_type = g.emitter.convert_ty(g.sa, object_type.clone());
            let bc_trait_ty = g.emitter.convert_trait_ty(g.sa, &trait_ty);
            let bc_fct_id = g.emitter.convert_function_id(g.sa, *fct_id);
            let bc_fct_type_params = g.convert_tya(fct_type_params);
            g.builder.add_const(ConstPoolEntry::Generic {
                object_type: bc_object_type,
                trait_ty: bc_trait_ty,
                fct_id: bc_fct_id,
                fct_type_params: bc_fct_type_params,
            })
        }
        CallType::TraitObjectMethod(trait_object_ty, _) => {
            let bc_trait_object_ty = g.emitter.convert_ty(g.sa, trait_object_ty.clone());
            let bc_fct_id = g.emitter.convert_function_id(g.sa, fct.id());
            g.builder.add_const(ConstPoolEntry::TraitObjectMethod(
                bc_trait_object_ty,
                bc_fct_id,
            ))
        }

        CallType::Method(.., type_params)
        | CallType::Expr(.., type_params)
        | CallType::Fct(.., type_params) => {
            assert_eq!(
                fct.type_param_definition.type_param_count(),
                type_params.len()
            );
            let bc_fct_id = g.emitter.convert_function_id(g.sa, fct.id());
            let bc_type_params = g.convert_tya(&type_params);
            g.builder.add_const_fct_types(bc_fct_id, bc_type_params)
        }

        _ => panic!("unexpected call type {:?}", call_type),
    }
}

pub(super) fn specialize_type_for_call(
    g: &AstBytecodeGen,
    call_type: &CallType,
    ty: SourceType,
) -> SourceType {
    match call_type {
        CallType::Fct(_, type_params)
        | CallType::Expr(_, _, type_params)
        | CallType::Method(_, _, type_params) => specialize_type(g.sa, ty, type_params),

        CallType::TraitObjectMethod(trait_ty, _actual_object_ty) => {
            let (trait_id, type_params, assoc_types) = match trait_ty {
                SourceType::TraitObject(trait_id, type_params, assoc_types) => {
                    (*trait_id, type_params, assoc_types)
                }
                _ => unreachable!(),
            };
            specialize_ty_for_trait_object(g.sa, ty, trait_id, type_params, assoc_types)
        }

        CallType::GenericMethod {
            object_type,
            trait_ty,
            fct_type_params,
            ..
        }
        | CallType::GenericStaticMethod {
            object_type,
            trait_ty,
            fct_type_params,
            ..
        } => replace_type(
            g.sa,
            ty,
            Some(&trait_ty.type_params.connect(fct_type_params)),
            Some(object_type.clone()),
        ),

        CallType::Lambda(..)
        | CallType::NewClass(..)
        | CallType::NewStruct(..)
        | CallType::NewEnum(..)
        | CallType::Intrinsic(..) => unreachable!(),
    }
}
