use dora_parser::Span;

use crate::error::msg::ErrorMessage;
use crate::sema::{implements_trait, Element, Sema, SourceFileId, TypeParamDefinition};
use crate::specialize::specialize_type;
use crate::{SourceType, SourceTypeArray, TraitType};

pub fn check<'a>(
    sa: &'a Sema,
    caller_type_param_defs: &'a TypeParamDefinition,
    element: &'a dyn Element,
    params: &'a SourceTypeArray,
    file_id: SourceFileId,
    span: Span,
) -> bool {
    let checker = TypeParamCheck {
        sa,
        caller_type_param_defs,
        callee_type_param_defs: element.type_param_definition(),
        file_id,
        span,
    };

    checker.check(params)
}

struct TypeParamCheck<'a> {
    sa: &'a Sema,
    caller_type_param_defs: &'a TypeParamDefinition,
    callee_type_param_defs: &'a TypeParamDefinition,
    file_id: SourceFileId,
    span: Span,
}

impl<'a> TypeParamCheck<'a> {
    fn check(&self, tps: &SourceTypeArray) -> bool {
        if self.callee_type_param_defs.type_param_count() != tps.len() {
            let msg = ErrorMessage::WrongNumberTypeParams(
                self.callee_type_param_defs.type_param_count(),
                tps.len(),
            );
            self.sa.report(self.file_id, self.span, msg);
            return false;
        }

        let mut succeeded = true;

        for bound in self.callee_type_param_defs.bounds() {
            let tp_ty = bound.ty();
            if let Some(trait_ty) = bound.trait_ty() {
                let tp_ty = specialize_type(self.sa, tp_ty, tps);

                if !implements_trait(
                    self.sa,
                    tp_ty.clone(),
                    self.caller_type_param_defs,
                    trait_ty.clone(),
                ) {
                    self.fail_trait_bound(self.file_id, self.span, trait_ty, tp_ty.clone());
                    succeeded = false;
                }
            }
        }

        succeeded
    }

    fn fail_trait_bound(
        &self,
        file_id: SourceFileId,
        span: Span,
        trait_ty: TraitType,
        ty: SourceType,
    ) {
        let name = ty.name_with_type_params(self.sa, self.caller_type_param_defs);
        let trait_name = trait_ty.name_with_type_params(self.sa, self.caller_type_param_defs);
        let msg = ErrorMessage::TypeNotImplementingTrait(name, trait_name);
        self.sa.report(file_id, span, msg);
    }
}
