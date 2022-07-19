use dora_parser::lexer::position::Position;

use crate::language::error::msg::SemError;
use crate::language::sem_analysis::{
    implements_trait, ClassDefinitionId, EnumDefinitionId, FctDefinition, SemAnalysis,
    SourceFileId, StructDefinitionId, TraitDefinitionId, TypeParamsDefinition,
};
use crate::language::ty::{SourceType, SourceTypeArray};

pub enum ErrorReporting {
    Yes(SourceFileId, Position),
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
        callee_type_param_defs: &enum_.type_params,
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
    let xstruct = sa.structs.idx(struct_id);
    let xstruct = xstruct.read();

    let checker = TypeParamCheck {
        sa,
        caller_type_param_defs: &fct.type_params,
        callee_type_param_defs: &xstruct.type_params,
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
        callee_type_param_defs: &cls.type_params,
        error,
    };

    checker.check(type_params)
}

pub fn check_params<'a>(
    sa: &'a SemAnalysis,
    fct: &'a FctDefinition,
    error: ErrorReporting,
    callee_type_param_defs: &'a TypeParamsDefinition,
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
    caller_type_param_defs: &'a TypeParamsDefinition,
    callee_type_param_defs: &'a TypeParamsDefinition,
    error: ErrorReporting,
}

impl<'a> TypeParamCheck<'a> {
    fn check(&self, tps: &SourceTypeArray) -> bool {
        if self.callee_type_param_defs.len() != tps.len() {
            if let ErrorReporting::Yes(file_id, pos) = self.error {
                let msg =
                    SemError::WrongNumberTypeParams(self.callee_type_param_defs.len(), tps.len());
                self.sa.diag.lock().report(file_id, pos, msg);
            }
            return false;
        }

        let mut succeeded = true;

        for (tp_def, ty) in self.callee_type_param_defs.iter().zip(tps.iter()) {
            for &trait_bound in &tp_def.trait_bounds {
                if !implements_trait(
                    self.sa,
                    ty.clone(),
                    self.caller_type_param_defs,
                    trait_bound,
                ) {
                    if let ErrorReporting::Yes(file_id, pos) = self.error {
                        self.fail_trait_bound(file_id, pos, trait_bound, ty.clone());
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
        pos: Position,
        trait_id: TraitDefinitionId,
        ty: SourceType,
    ) {
        let name = ty.name_with_type_params(self.sa, self.caller_type_param_defs);
        let trait_ = self.sa.traits[trait_id].read();
        let trait_name = self.sa.interner.str(trait_.name).to_string();
        let msg = SemError::TypeNotImplementingTrait(name, trait_name);
        self.sa.diag.lock().report(file_id, pos, msg);
    }
}
