use dora_parser::lexer::position::Position;

use std::collections::hash_set::HashSet;

use crate::error::msg::SemError;
use crate::ty::{BuiltinType, TypeList};
use crate::vm::{Class, ClassId, Fct, FileId, TraitId, TypeParam, VM};

pub enum ErrorReporting {
    Yes(FileId, Position),
    No,
}

pub fn check_in_fct<'a, 'ast: 'a>(
    vm: &VM<'ast>,
    fct: &Fct<'ast>,
    error: ErrorReporting,
    object_type: BuiltinType,
) -> bool {
    let tp_defs = {
        let cls_id = object_type.cls_id(vm).expect("no class");
        let cls = vm.classes.idx(cls_id);
        let cls = cls.read();
        cls.type_params.to_vec()
    };

    let checker = TypeParamCheck {
        vm,
        fct: Some(fct),
        cls_id: fct.parent_cls_id(),
        error,
        tp_defs: &tp_defs,
    };

    let params = object_type.type_params(vm);

    checker.check(&params)
}

pub fn check_super<'a, 'ast: 'a>(vm: &VM<'ast>, cls: &Class, error: ErrorReporting) -> bool {
    let object_type = cls.parent_class.expect("parent_class missing");

    let tp_defs = {
        let cls_id = object_type.cls_id(vm).expect("no class");
        let cls = vm.classes.idx(cls_id);
        let cls = cls.read();
        cls.type_params.to_vec()
    };

    let checker = TypeParamCheck {
        vm,
        fct: None,
        cls_id: Some(cls.id),
        error,
        tp_defs: &tp_defs,
    };

    let params = object_type.type_params(vm);

    checker.check(&params)
}

pub fn check_params<'a, 'ast: 'a>(
    vm: &'a VM<'ast>,
    fct: &'a Fct<'ast>,
    error: ErrorReporting,
    tp_defs: &'a [TypeParam],
    params: &'a TypeList,
) -> bool {
    let checker = TypeParamCheck {
        vm,
        fct: Some(fct),
        cls_id: fct.parent_cls_id(),
        error,
        tp_defs,
    };

    checker.check(params)
}

struct TypeParamCheck<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    fct: Option<&'a Fct<'ast>>,
    cls_id: Option<ClassId>,
    error: ErrorReporting,
    tp_defs: &'a [TypeParam],
}

impl<'a, 'ast> TypeParamCheck<'a, 'ast> {
    fn check(&self, tps: &TypeList) -> bool {
        if self.tp_defs.len() != tps.len() {
            if let ErrorReporting::Yes(file_id, pos) = self.error {
                let msg = SemError::WrongNumberTypeParams(self.tp_defs.len(), tps.len());
                self.vm.diag.lock().report(file_id, pos, msg);
            }
            return false;
        }

        let mut succeeded = true;

        for (tp, ty) in self.tp_defs.iter().zip(tps.iter()) {
            if ty.is_type_param() {
                let ok = match ty {
                    BuiltinType::ClassTypeParam(cls_id, tpid) => {
                        assert!(cls_id == self.cls_id.expect("missing cls_id"));
                        let cls = self.vm.classes.idx(cls_id);
                        let cls = cls.read();
                        self.tp_against_definition(tp, cls.type_param(tpid), ty)
                    }

                    BuiltinType::FctTypeParam(fct_id, tpid) => {
                        assert!(fct_id == self.fct.expect("missing id").id);
                        let fct = self.vm.fcts.idx(fct_id);
                        let fct = fct.read();
                        self.tp_against_definition(tp, fct.type_param(tpid), ty)
                    }

                    _ => unreachable!(),
                };

                if !ok {
                    succeeded = false;
                }
            } else if !self.type_against_definition(tp, ty) {
                succeeded = false;
            }
        }

        succeeded
    }

    fn type_against_definition(&self, tp: &TypeParam, ty: BuiltinType) -> bool {
        let mut succeeded = true;

        for &trait_bound in &tp.trait_bounds {
            if !ty.implements_trait(self.vm, trait_bound) {
                if let ErrorReporting::Yes(file_id, pos) = self.error {
                    self.fail_trait_bound(file_id, pos, trait_bound, ty);
                }
                succeeded = false;
            }
        }

        succeeded
    }

    fn tp_against_definition(&self, tp: &TypeParam, arg: &TypeParam, arg_ty: BuiltinType) -> bool {
        let mut succeeded = true;

        if tp.trait_bounds.len() == 0 {
            return succeeded;
        }

        let traits_set = arg.trait_bounds.iter().collect::<HashSet<_>>();

        for &trait_bound in &tp.trait_bounds {
            if !traits_set.contains(&trait_bound) {
                if let ErrorReporting::Yes(file_id, pos) = self.error {
                    self.fail_trait_bound(file_id, pos, trait_bound, arg_ty);
                }
                succeeded = false;
            }
        }
        succeeded
    }

    fn fail_trait_bound(&self, file_id: FileId, pos: Position, trait_id: TraitId, ty: BuiltinType) {
        let bound = self.vm.traits[trait_id].read();
        let name = ty.name(self.vm);
        let trait_name = self.vm.interner.str(bound.name).to_string();
        let msg = SemError::TraitBoundNotSatisfied(name, trait_name);
        self.vm.diag.lock().report(file_id, pos, msg);
    }
}
