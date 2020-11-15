use dora_parser::lexer::position::Position;

use std::collections::hash_set::HashSet;

use crate::error::msg::SemError;
use crate::ty::{SourceType, TypeList};
use crate::vm::{Class, ClassId, Fct, FileId, StructId, TraitId, TypeParam, VM};

pub enum ErrorReporting {
    Yes(FileId, Position),
    No,
}

pub fn check_in_fct<'a>(
    vm: &VM,
    fct: &Fct,
    error: ErrorReporting,
    object_type: SourceType,
) -> bool {
    let tp_defs = {
        let cls_id = object_type.cls_id(vm).expect("no class");
        let cls = vm.classes.idx(cls_id);
        let cls = cls.read();
        cls.type_params.to_vec()
    };

    let checker = TypeParamCheck {
        vm,
        use_fct: Some(fct),
        use_cls_id: fct.parent_cls_id(),
        error,
        tp_defs: &tp_defs,
    };

    let params = object_type.type_params(vm);

    checker.check(&params)
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
        use_fct: Some(fct),
        use_cls_id: None,
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
    type_params: &TypeList,
    error: ErrorReporting,
) -> bool {
    let tp_defs = {
        let xstruct = vm.structs.idx(struct_id);
        let xstruct = xstruct.read();
        xstruct.type_params.to_vec()
    };

    let checker = TypeParamCheck {
        vm,
        use_fct: Some(fct),
        use_cls_id: None,
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
        use_fct: None,
        use_cls_id: Some(cls.id),
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
    params: &'a TypeList,
) -> bool {
    let checker = TypeParamCheck {
        vm,
        use_fct: Some(fct),
        use_cls_id: fct.parent_cls_id(),
        error,
        tp_defs,
    };

    checker.check(params)
}

struct TypeParamCheck<'a> {
    vm: &'a VM,
    use_fct: Option<&'a Fct>,
    use_cls_id: Option<ClassId>,
    error: ErrorReporting,
    tp_defs: &'a [TypeParam],
}

impl<'a> TypeParamCheck<'a> {
    fn check(&self, tps: &TypeList) -> bool {
        if self.tp_defs.len() != tps.len() {
            if let ErrorReporting::Yes(file_id, pos) = self.error {
                let msg = SemError::WrongNumberTypeParams(self.tp_defs.len(), tps.len());
                self.vm.diag.lock().report(file_id, pos, msg);
            }
            return false;
        }

        let mut succeeded = true;

        for (tp_def, ty) in self.tp_defs.iter().zip(tps.iter()) {
            if let SourceType::TypeParam(id) = ty {
                let ok = if let Some(use_fct) = self.use_fct {
                    use_fct.type_param_ty(self.vm, ty.clone(), |tp_arg, _| {
                        self.tp_against_definition(tp_def, tp_arg, ty.clone())
                    })
                } else if let Some(use_cls_id) = self.use_cls_id {
                    let cls = self.vm.classes.idx(use_cls_id);
                    let cls = cls.read();
                    self.tp_against_definition(tp_def, cls.type_param(id), ty)
                } else {
                    unreachable!()
                };

                if !ok {
                    succeeded = false;
                }
            } else if !self.type_against_definition(tp_def, ty) {
                succeeded = false;
            }
        }

        succeeded
    }

    fn type_against_definition(&self, tp: &TypeParam, ty: SourceType) -> bool {
        let mut succeeded = true;

        for &trait_bound in &tp.trait_bounds {
            if !ty.implements_trait(self.vm, trait_bound) {
                if let ErrorReporting::Yes(file_id, pos) = self.error {
                    self.fail_trait_bound(file_id, pos, trait_bound, ty.clone());
                }
                succeeded = false;
            }
        }

        succeeded
    }

    fn tp_against_definition(&self, tp: &TypeParam, arg: &TypeParam, arg_ty: SourceType) -> bool {
        let mut succeeded = true;

        if tp.trait_bounds.len() == 0 {
            return succeeded;
        }

        let traits_set = arg.trait_bounds.iter().collect::<HashSet<_>>();

        for &trait_bound in &tp.trait_bounds {
            if !traits_set.contains(&trait_bound) {
                if let ErrorReporting::Yes(file_id, pos) = self.error {
                    self.fail_trait_bound(file_id, pos, trait_bound, arg_ty.clone());
                }
                succeeded = false;
            }
        }
        succeeded
    }

    fn fail_trait_bound(&self, file_id: FileId, pos: Position, trait_id: TraitId, ty: SourceType) {
        let bound = self.vm.traits[trait_id].read();
        let name = if let Some(fct) = self.use_fct {
            ty.name_fct(self.vm, fct)
        } else {
            let cls = self
                .vm
                .classes
                .idx(self.use_cls_id.expect("cls_id missing"));
            let cls = cls.read();
            ty.name_cls(self.vm, &*cls)
        };
        let trait_name = self.vm.interner.str(bound.name).to_string();
        let msg = SemError::TraitBoundNotSatisfied(name, trait_name);
        self.vm.diag.lock().report(file_id, pos, msg);
    }
}
