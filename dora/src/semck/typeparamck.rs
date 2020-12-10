use dora_parser::lexer::position::Position;

use crate::error::msg::SemError;
use crate::ty::{implements_trait, SourceType, SourceTypeArray};
use crate::vm::{Class, ClassId, EnumId, Fct, FileId, StructId, TraitId, TypeParam, VM};

pub enum ErrorReporting {
    Yes(FileId, Position),
    No,
}

pub fn check_enum(
    vm: &VM,
    fct: &Fct,
    enum_id: EnumId,
    type_params: &SourceTypeArray,
    error: ErrorReporting,
) -> bool {
    let xenum = &vm.enums[enum_id];
    let xenum = xenum.read();

    let checker = TypeParamCheck {
        vm,
        caller_type_param_defs: &fct.type_params,
        callee_type_param_defs: &xenum.type_params,
        error,
    };

    checker.check(type_params)
}

pub fn check_struct(
    vm: &VM,
    fct: &Fct,
    struct_id: StructId,
    type_params: &SourceTypeArray,
    error: ErrorReporting,
) -> bool {
    let xstruct = vm.structs.idx(struct_id);
    let xstruct = xstruct.read();

    let checker = TypeParamCheck {
        vm,
        caller_type_param_defs: &fct.type_params,
        callee_type_param_defs: &xstruct.type_params,
        error,
    };

    checker.check(type_params)
}

pub fn check_class(
    vm: &VM,
    fct: &Fct,
    cls_id: ClassId,
    type_params: &SourceTypeArray,
    error: ErrorReporting,
) -> bool {
    let cls = vm.classes.idx(cls_id);
    let cls = cls.read();

    let checker = TypeParamCheck {
        vm,
        caller_type_param_defs: &fct.type_params,
        callee_type_param_defs: &cls.type_params,
        error,
    };

    checker.check(type_params)
}

pub fn check_super<'a>(vm: &VM, cls: &Class, error: ErrorReporting) -> bool {
    let object_type = cls.parent_class.clone().expect("parent_class missing");

    let super_cls_id = object_type.cls_id().expect("no class");
    let super_cls = vm.classes.idx(super_cls_id);
    let super_cls = super_cls.read();

    let checker = TypeParamCheck {
        vm,
        caller_type_param_defs: &cls.type_params,
        callee_type_param_defs: &super_cls.type_params,
        error,
    };

    let params = object_type.type_params(vm);

    checker.check(&params)
}

pub fn check_params<'a>(
    vm: &'a VM,
    fct: &'a Fct,
    error: ErrorReporting,
    callee_type_param_defs: &'a [TypeParam],
    params: &'a SourceTypeArray,
) -> bool {
    let checker = TypeParamCheck {
        vm,
        caller_type_param_defs: &fct.type_params,
        callee_type_param_defs: callee_type_param_defs,
        error,
    };

    checker.check(params)
}

struct TypeParamCheck<'a> {
    vm: &'a VM,
    caller_type_param_defs: &'a [TypeParam],
    callee_type_param_defs: &'a [TypeParam],
    error: ErrorReporting,
}

impl<'a> TypeParamCheck<'a> {
    fn check(&self, tps: &SourceTypeArray) -> bool {
        if self.callee_type_param_defs.len() != tps.len() {
            if let ErrorReporting::Yes(file_id, pos) = self.error {
                let msg =
                    SemError::WrongNumberTypeParams(self.callee_type_param_defs.len(), tps.len());
                self.vm.diag.lock().report(file_id, pos, msg);
            }
            return false;
        }

        let mut succeeded = true;

        for (tp_def, ty) in self.callee_type_param_defs.iter().zip(tps.iter()) {
            for &trait_bound in &tp_def.trait_bounds {
                if !implements_trait(
                    self.vm,
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

    fn fail_trait_bound(&self, file_id: FileId, pos: Position, trait_id: TraitId, ty: SourceType) {
        let name = ty.name_with_params(self.vm, self.caller_type_param_defs);
        let xtrait = self.vm.traits[trait_id].read();
        let trait_name = self.vm.interner.str(xtrait.name).to_string();
        let msg = SemError::TypeNotImplementingTrait(name, trait_name);
        self.vm.diag.lock().report(file_id, pos, msg);
    }
}
