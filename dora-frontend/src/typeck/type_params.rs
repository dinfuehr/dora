use dora_parser::Span;

use crate::args;
use crate::error::diagnostics::{TYPE_NOT_IMPLEMENTING_TRAIT, WRONG_NUMBER_TYPE_PARAMS};
use crate::sema::{Element, Sema, SourceFileId, TypeParamDefinition, implements_trait};
use crate::{SourceType, SourceTypeArray, specialize_trait_type_generic};

pub fn check_type_params<'a, S>(
    sa: &'a Sema,
    caller_element: &'a dyn Element,
    caller_type_param_defs: &'a TypeParamDefinition,
    callee_element: &'a dyn Element,
    params: &'a SourceTypeArray,
    file_id: SourceFileId,
    span: Span,
    specialize: S,
) -> bool
where
    S: Fn(SourceType) -> SourceType,
{
    let callee_type_param_defs = callee_element.type_param_definition();

    if callee_type_param_defs.type_param_count() != params.len() {
        let exp_count;
        let got_count;

        if callee_type_param_defs.container_type_params() > 0 {
            exp_count = callee_type_param_defs.own_type_params_len();
            got_count = params.len() - callee_type_param_defs.container_type_params();
        } else {
            exp_count = callee_type_param_defs.type_param_count();
            got_count = params.len();
        }

        sa.report(
            file_id,
            span,
            &WRONG_NUMBER_TYPE_PARAMS,
            args!(exp_count, got_count),
        );
        return false;
    }

    let mut succeeded = true;

    for bound in callee_type_param_defs.bounds() {
        let tp_ty = bound.ty();
        if let Some(trait_ty) = bound.trait_ty() {
            let tp_ty = specialize(tp_ty);
            let trait_ty = specialize_trait_type_generic(sa, trait_ty, &specialize);

            if !implements_trait(sa, tp_ty.clone(), caller_element, trait_ty.clone()) {
                let name = tp_ty.name_with_type_params(sa, caller_type_param_defs);
                let trait_name = trait_ty.name_with_type_params(sa, caller_type_param_defs);
                sa.report(
                    file_id,
                    span,
                    &TYPE_NOT_IMPLEMENTING_TRAIT,
                    args!(name, trait_name),
                );
                succeeded = false;
            }
        }
    }

    succeeded
}
