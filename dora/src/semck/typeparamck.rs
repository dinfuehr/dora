use dora_parser::lexer::position::Position;

use crate::error::msg::SemError;
use crate::ty::{implements_trait, SourceType, SourceTypeArray};
use crate::vm::{Class, Fct, FileId, StructId, TraitId, TypeParam, VM};

pub enum ErrorReporting {
    Yes(FileId, Position),
    No,
}

pub fn check_enum(vm: &VM, fct: &Fct, ty: SourceType, error: ErrorReporting) -> bool {
    let enum_id = ty.enum_id().expect("not an enum");

    let tp_defs = {
        let xenum = &vm.enums[enum_id];
        let xenum = xenum.read();
        xenum.type_params.to_vec()
    };

    let checker = TypeParamCheck {
        vm,
        type_param_defs: &fct.type_params,
        error,
        tp_defs: &tp_defs,
    };

    let params = ty.type_params(vm);

    checker.check(&params)
}

pub fn check_struct(
    vm: &VM,
    fct: &Fct,
    struct_id: StructId,
    type_params: &SourceTypeArray,
    error: ErrorReporting,
) -> bool {
    let tp_defs = {
        let xstruct = vm.structs.idx(struct_id);
        let xstruct = xstruct.read();
        xstruct.type_params.to_vec()
    };

    let checker = TypeParamCheck {
        vm,
        type_param_defs: &fct.type_params,
        error,
        tp_defs: &tp_defs,
    };

    checker.check(type_params)
}

pub fn check_super<'a>(vm: &VM, cls: &Class, error: ErrorReporting) -> bool {
    let object_type = cls.parent_class.clone().expect("parent_class missing");

    let tp_defs = {
        let cls_id = object_type.cls_id(vm).expect("no class");
        let cls = vm.classes.idx(cls_id);
        let cls = cls.read();
        cls.type_params.to_vec()
    };

    let checker = TypeParamCheck {
        vm,
        type_param_defs: &cls.type_params,
        error,
        tp_defs: &tp_defs,
    };

    let params = object_type.type_params(vm);

    checker.check(&params)
}

pub fn check_params<'a>(
    vm: &'a VM,
    fct: &'a Fct,
    error: ErrorReporting,
    tp_defs: &'a [TypeParam],
    params: &'a SourceTypeArray,
) -> bool {
    let checker = TypeParamCheck {
        vm,
        type_param_defs: &fct.type_params,
        error,
        tp_defs,
    };

    checker.check(params)
}

struct TypeParamCheck<'a> {
    vm: &'a VM,
    type_param_defs: &'a [TypeParam],
    error: ErrorReporting,
    tp_defs: &'a [TypeParam],
}

impl<'a> TypeParamCheck<'a> {
    fn check(&self, tps: &SourceTypeArray) -> bool {
        if self.tp_defs.len() != tps.len() {
            if let ErrorReporting::Yes(file_id, pos) = self.error {
                let msg = SemError::WrongNumberTypeParams(self.tp_defs.len(), tps.len());
                self.vm.diag.lock().report(file_id, pos, msg);
            }
            return false;
        }

        let mut succeeded = true;

        for (tp_def, ty) in self.tp_defs.iter().zip(tps.iter()) {
            for &trait_bound in &tp_def.trait_bounds {
                if !implements_trait(self.vm, ty.clone(), self.type_param_defs, trait_bound) {
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
        let name = ty.name_with_params(self.vm, self.type_param_defs);
        let xtrait = self.vm.traits[trait_id].read();
        let trait_name = self.vm.interner.str(xtrait.name).to_string();
        let msg = SemError::TraitBoundNotSatisfied(name, trait_name);
        self.vm.diag.lock().report(file_id, pos, msg);
    }
}
