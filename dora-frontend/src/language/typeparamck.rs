use dora_parser::Span;

use crate::language::error::msg::ErrorMessage;
use crate::language::sem_analysis::{
    implements_trait, ClassDefinitionId, EnumDefinitionId, FctDefinition, SemAnalysis,
    SourceFileId, StructDefinitionId, TypeParamDefinition,
};
use crate::language::specialize::specialize_type;
use crate::language::ty::{SourceType, SourceTypeArray};

pub enum ErrorReporting {
    Yes(SourceFileId, Span),
    No,
}

pub fn check_enum(
    sa: &SemAnalysis,
    fct: &FctDefinition,
    enum_id: EnumDefinitionId,
    type_params: &SourceTypeArray,
    error: ErrorReporting,
) -> bool {
    let enum_ = &sa.enums[enum_id];
    let enum_ = enum_.read();

    let checker = TypeParamCheck {
        sa,
        caller_type_param_defs: &fct.type_params,
        callee_type_param_defs: enum_.type_params(),
        error,
    };

    checker.check(type_params)
}

pub fn check_struct(
    sa: &SemAnalysis,
    fct: &FctDefinition,
    struct_id: StructDefinitionId,
    type_params: &SourceTypeArray,
    error: ErrorReporting,
) -> bool {
    let struct_ = sa.structs.idx(struct_id);
    let struct_ = struct_.read();

    let checker = TypeParamCheck {
        sa,
        caller_type_param_defs: &fct.type_params,
        callee_type_param_defs: struct_.type_params(),
        error,
    };

    checker.check(type_params)
}

pub fn check_class(
    sa: &SemAnalysis,
    fct: &FctDefinition,
    cls_id: ClassDefinitionId,
    type_params: &SourceTypeArray,
    error: ErrorReporting,
) -> bool {
    let cls = sa.classes.idx(cls_id);
    let cls = cls.read();

    let checker = TypeParamCheck {
        sa,
        caller_type_param_defs: &fct.type_params,
        callee_type_param_defs: cls.type_params(),
        error,
    };

    checker.check(type_params)
}

pub fn check_params<'a>(
    sa: &'a SemAnalysis,
    fct: &'a FctDefinition,
    error: ErrorReporting,
    callee_type_param_defs: &'a TypeParamDefinition,
    params: &'a SourceTypeArray,
) -> bool {
    let checker = TypeParamCheck {
        sa,
        caller_type_param_defs: &fct.type_params,
        callee_type_param_defs: callee_type_param_defs,
        error,
    };

    checker.check(params)
}

struct TypeParamCheck<'a> {
    sa: &'a SemAnalysis,
    caller_type_param_defs: &'a TypeParamDefinition,
    callee_type_param_defs: &'a TypeParamDefinition,
    error: ErrorReporting,
}

impl<'a> TypeParamCheck<'a> {
    fn check(&self, tps: &SourceTypeArray) -> bool {
        if self.callee_type_param_defs.len() != tps.len() {
            if let ErrorReporting::Yes(file_id, span) = self.error {
                let msg = ErrorMessage::WrongNumberTypeParams(
                    self.callee_type_param_defs.len(),
                    tps.len(),
                );
                self.sa.diag.lock().report_span(file_id, span, msg);
            }
            return false;
        }

        let mut succeeded = true;

        for bound in self.callee_type_param_defs.bounds() {
            let tp_ty = bound.ty();
            let trait_ty = bound.trait_ty();

            let tp_ty = specialize_type(self.sa, tp_ty, tps);

            if !implements_trait(
                self.sa,
                tp_ty.clone(),
                self.caller_type_param_defs,
                trait_ty.clone(),
            ) {
                if let ErrorReporting::Yes(file_id, span) = self.error {
                    self.fail_trait_bound(file_id, span, trait_ty, tp_ty.clone());
                }
                succeeded = false;
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
        self.sa.diag.lock().report_span(file_id, span, msg);
    }
}
