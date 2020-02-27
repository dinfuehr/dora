use dora_parser::lexer::position::Position;

use std::collections::hash_set::HashSet;

use crate::error::msg::SemError;
use crate::ty::{BuiltinType, TypeList};
use crate::vm::{FileId, TraitId, TypeParam, VM};

pub fn check_type(vm: &VM, file: FileId, pos: Position, object_type: BuiltinType) -> bool {
    let tp_defs = {
        let cls_id = object_type.cls_id(vm).expect("no class");
        let cls = vm.classes.idx(cls_id);
        let cls = cls.read();
        cls.type_params.to_vec()
    };

    let checker = TypeParamCheck {
        vm,
        file,
        pos,
        tp_defs: &tp_defs,
    };

    let params = object_type.type_params(vm);

    checker.check(&params)
}

pub fn check_params(
    vm: &VM,
    file: FileId,
    pos: Position,
    tp_defs: &[TypeParam],
    params: &TypeList,
) -> bool {
    let checker = TypeParamCheck {
        vm,
        file,
        pos,
        tp_defs,
    };

    checker.check(params)
}

struct TypeParamCheck<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    file: FileId,
    pos: Position,
    tp_defs: &'a [TypeParam],
}

impl<'a, 'ast> TypeParamCheck<'a, 'ast> {
    fn check(&self, tps: &TypeList) -> bool {
        if self.tp_defs.len() != tps.len() {
            let msg = SemError::WrongNumberTypeParams(self.tp_defs.len(), tps.len());
            self.vm.diag.lock().report(self.file, self.pos, msg);
            return false;
        }

        let mut succeeded = true;

        for (tp, ty) in self.tp_defs.iter().zip(tps.iter()) {
            if ty.is_type_param() {
                let ok = match ty {
                    BuiltinType::ClassTypeParam(cls_id, tpid) => {
                        let cls = self.vm.classes.idx(cls_id);
                        let cls = cls.read();
                        self.tp_against_definition(tp, &cls.type_params[tpid.idx()], ty)
                    }

                    BuiltinType::FctTypeParam(fct_id, tpid) => {
                        let fct = self.vm.fcts.idx(fct_id);
                        let fct = fct.read();
                        self.tp_against_definition(tp, &fct.type_params[tpid.idx()], ty)
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
                self.fail_trait_bound(trait_bound, ty);
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
                self.fail_trait_bound(trait_bound, arg_ty);
                succeeded = false;
            }
        }
        succeeded
    }

    fn fail_trait_bound(&self, trait_id: TraitId, ty: BuiltinType) {
        let bound = self.vm.traits[trait_id].read();
        let name = ty.name(self.vm);
        let trait_name = self.vm.interner.str(bound.name).to_string();
        let msg = SemError::TraitBoundNotSatisfied(name, trait_name);
        self.vm.diag.lock().report(self.file, self.pos, msg);
    }
}
