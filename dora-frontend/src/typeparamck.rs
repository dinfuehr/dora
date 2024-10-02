use dora_parser::Span;

use crate::error::msg::ErrorMessage;
use crate::sema::{
    implements_trait, ClassDefinitionId, EnumDefinitionId, Sema, SourceFileId, StructDefinitionId,
    TypeParamDefinition,
};
use crate::specialize::specialize_type;
use crate::ty::{SourceType, SourceTypeArray};

pub enum ErrorReporting {
    Yes(SourceFileId, Span),
    No,
}

pub fn check_enum(
    sa: &Sema,
    caller_type_param_defs: &TypeParamDefinition,
    enum_id: EnumDefinitionId,
    type_params: &SourceTypeArray,
    error: ErrorReporting,
) -> bool {
    let enum_ = sa.enum_(enum_id);

    let checker = TypeParamCheck {
        sa,
        caller_type_param_defs,
        callee_type_param_defs: enum_.type_param_definition(),
        error,
    };

    checker.check(type_params)
}

pub fn check_struct(
    sa: &Sema,
    caller_type_param_defs: &TypeParamDefinition,
    struct_id: StructDefinitionId,
    type_params: &SourceTypeArray,
    error: ErrorReporting,
) -> bool {
    let struct_ = sa.struct_(struct_id);

    let checker = TypeParamCheck {
        sa,
        caller_type_param_defs,
        callee_type_param_defs: struct_.type_param_definition(),
        error,
    };

    checker.check(type_params)
}

pub fn check_class(
    sa: &Sema,
    caller_type_param_defs: &TypeParamDefinition,
    cls_id: ClassDefinitionId,
    type_params: &SourceTypeArray,
    error: ErrorReporting,
) -> bool {
    let cls = sa.class(cls_id);

    let checker = TypeParamCheck {
        sa,
        caller_type_param_defs,
        callee_type_param_defs: cls.type_param_definition(),
        error,
    };

    checker.check(type_params)
}

pub fn check_params<'a>(
    sa: &'a Sema,
    caller_type_param_defs: &'a TypeParamDefinition,
    callee_type_param_defs: &'a TypeParamDefinition,
    params: &'a SourceTypeArray,
    error: ErrorReporting,
) -> bool {
    let checker = TypeParamCheck {
        sa,
        caller_type_param_defs,
        callee_type_param_defs,
        error,
    };

    checker.check(params)
}

struct TypeParamCheck<'a> {
    sa: &'a Sema,
    caller_type_param_defs: &'a TypeParamDefinition,
    callee_type_param_defs: &'a TypeParamDefinition,
    error: ErrorReporting,
}

impl<'a> TypeParamCheck<'a> {
    fn check(&self, tps: &SourceTypeArray) -> bool {
        if self.callee_type_param_defs.type_param_count() != tps.len() {
            if let ErrorReporting::Yes(file_id, span) = self.error {
                let msg = ErrorMessage::WrongNumberTypeParams(
                    self.callee_type_param_defs.type_param_count(),
                    tps.len(),
                );
                self.sa.report(file_id, span, msg);
            }
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
                    trait_ty.ty(),
                ) {
                    if let ErrorReporting::Yes(file_id, span) = self.error {
                        self.fail_trait_bound(file_id, span, trait_ty.ty(), tp_ty.clone());
                    }
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
        trait_ty: SourceType,
        ty: SourceType,
    ) {
        let name = ty.name_with_type_params(self.sa, self.caller_type_param_defs);
        let trait_name = trait_ty.name_with_type_params(self.sa, self.caller_type_param_defs);
        let msg = ErrorMessage::TypeNotImplementingTrait(name, trait_name);
        self.sa.report(file_id, span, msg);
    }
}
