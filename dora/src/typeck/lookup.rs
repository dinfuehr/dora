use crate::error::msg::SemError;
use crate::semck::specialize::replace_type_param;
use crate::semck::typeparamck;
use crate::ty::{BuiltinType, TypeList};
use crate::typeck::expr::args_compatible;
use crate::vm::{
    find_methods_in_class, find_methods_in_enum, ClassId, FctId, FctParent, FileId, TraitId,
    TypeParam, VM,
};

use crate::vm::module::find_methods_in_module;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

#[derive(Copy, Clone)]
enum LookupKind {
    Fct,
    Method(BuiltinType),
    Static(ClassId),
    Trait(TraitId),
    Callee(FctId),
    Ctor(ClassId),
}

pub struct MethodLookup<'a, 'ast: 'a> {
    vm: &'a VM<'ast>,
    file: FileId,
    kind: Option<LookupKind>,
    name: Option<Name>,
    args: Option<&'a [BuiltinType]>,
    cls_tps: Option<&'a TypeList>,
    fct_tps: Option<&'a TypeList>,
    ret: Option<BuiltinType>,
    pos: Option<Position>,

    found_fct_id: Option<FctId>,
    found_class_type: Option<BuiltinType>,
    found_ret: Option<BuiltinType>,

    found_multiple_functions: bool,
}

impl<'a, 'ast> MethodLookup<'a, 'ast> {
    pub fn new(vm: &'a VM<'ast>, file: FileId) -> MethodLookup<'a, 'ast> {
        MethodLookup {
            vm,
            file,
            kind: None,
            name: None,
            args: None,
            cls_tps: None,
            fct_tps: None,
            ret: None,
            pos: None,

            found_fct_id: None,
            found_class_type: None,
            found_ret: None,

            found_multiple_functions: false,
        }
    }

    pub fn ctor(mut self, cls_id: ClassId) -> MethodLookup<'a, 'ast> {
        self.kind = Some(LookupKind::Ctor(cls_id));
        self
    }

    pub fn callee(mut self, fct_id: FctId) -> MethodLookup<'a, 'ast> {
        self.kind = Some(LookupKind::Callee(fct_id));
        self
    }

    pub fn method(mut self, obj: BuiltinType) -> MethodLookup<'a, 'ast> {
        self.kind = if let Some(_) = obj.cls_id(self.vm) {
            Some(LookupKind::Method(obj))
        } else if let Some(_) = obj.module_id() {
            Some(LookupKind::Method(obj))
        } else if let BuiltinType::Trait(trait_id) = obj {
            Some(LookupKind::Trait(trait_id))
        } else if obj.is_nil() {
            Some(LookupKind::Method(obj))
        } else if obj.is_enum() {
            Some(LookupKind::Method(obj))
        } else {
            panic!("neither object nor trait object: {:?}", obj);
        };

        self
    }

    pub fn static_method(mut self, cls_id: ClassId) -> MethodLookup<'a, 'ast> {
        self.kind = Some(LookupKind::Static(cls_id));
        self
    }

    pub fn fct(mut self) -> MethodLookup<'a, 'ast> {
        self.kind = Some(LookupKind::Fct);
        self
    }

    pub fn args(mut self, args: &'a [BuiltinType]) -> MethodLookup<'a, 'ast> {
        self.args = Some(args);
        self
    }

    pub fn pos(mut self, pos: Position) -> MethodLookup<'a, 'ast> {
        self.pos = Some(pos);
        self
    }

    pub fn cls_type_params(mut self, cls_tps: &'a TypeList) -> MethodLookup<'a, 'ast> {
        self.cls_tps = Some(cls_tps);
        self
    }

    pub fn fct_type_params(mut self, fct_tps: &'a TypeList) -> MethodLookup<'a, 'ast> {
        self.fct_tps = Some(fct_tps);
        self
    }

    pub fn name(mut self, name: Name) -> MethodLookup<'a, 'ast> {
        self.name = Some(name);
        self
    }

    pub fn return_type(mut self, ret: BuiltinType) -> MethodLookup<'a, 'ast> {
        self.ret = Some(ret);
        self
    }

    pub fn find(&mut self) -> bool {
        let kind = self.kind.expect("kind not set");
        let args = self.args.expect("args not set");

        let fct_id = match kind {
            LookupKind::Fct => {
                assert!(self.cls_tps.is_none());
                let name = self.name.expect("name not set");
                self.find_fct(name)
            }

            LookupKind::Callee(fct_id) => Some(fct_id),

            LookupKind::Method(obj) => {
                let name = self.name.expect("name not set");
                self.find_method(obj, name, false)
            }

            LookupKind::Trait(trait_id) => {
                let name = self.name.expect("name not set");
                self.find_method_in_trait(trait_id, name, false)
            }

            LookupKind::Static(cls_id) => {
                assert!(self.cls_tps.is_none());
                let name = self.name.expect("name not set");
                self.find_method(self.vm.cls(cls_id), name, true)
            }

            LookupKind::Ctor(cls_id) => {
                assert!(self.cls_tps.is_some());
                self.find_ctor(cls_id)
            }
        };

        self.found_fct_id = fct_id;

        let fct_id = if let Some(fct_id) = fct_id {
            fct_id
        } else {
            let name = match kind {
                LookupKind::Ctor(cls_id) => {
                    let cls = self.vm.classes.idx(cls_id);
                    let cls = cls.read();
                    cls.name
                }
                _ => self.name.expect("name not set"),
            };

            let name = self.vm.interner.str(name).to_string();
            let param_names = args
                .iter()
                .map(|a| a.name(self.vm))
                .collect::<Vec<String>>();

            let msg = match kind {
                LookupKind::Fct => SemError::Unimplemented,
                LookupKind::Callee(_) => unreachable!(),
                LookupKind::Method(obj) => {
                    let type_name = obj.name(self.vm);

                    if self.found_multiple_functions {
                        SemError::MultipleCandidatesForMethod(type_name, name, param_names)
                    } else {
                        SemError::UnknownMethod(type_name, name, param_names)
                    }
                }

                LookupKind::Trait(trait_id) => {
                    let xtrait = &self.vm.traits[trait_id];
                    let xtrait = xtrait.read();
                    let type_name = self.vm.interner.str(xtrait.name).to_string();
                    SemError::UnknownMethod(type_name, name, param_names)
                }

                LookupKind::Static(cls_id) => {
                    let type_name = self.vm.cls(cls_id).name(self.vm);
                    SemError::UnknownStaticMethod(type_name, name, param_names)
                }

                LookupKind::Ctor(cls_id) => {
                    let cls = self.vm.classes.idx(cls_id);
                    let cls = cls.read();
                    let name = self.vm.interner.str(cls.name).to_string();
                    SemError::UnknownCtor(name, param_names)
                }
            };

            self.vm
                .diag
                .lock()
                .report(self.file, self.pos.expect("pos not set"), msg);
            return false;
        };

        let fct = self.vm.fcts.idx(fct_id);
        let fct = fct.read();

        let cls_id = match fct.parent {
            FctParent::Class(cls_id) => Some(cls_id),
            FctParent::Impl(impl_id) => {
                let ximpl = self.vm.impls[impl_id].read();
                Some(ximpl.cls_id(self.vm))
            }
            _ => None,
        };

        let cls_tps: TypeList = if let Some(cls_tps) = self.cls_tps {
            cls_tps.clone()
        } else if let LookupKind::Method(obj) = kind {
            obj.type_params(self.vm)
        } else {
            TypeList::empty()
        };

        if cls_id.is_some() && !self.check_cls_tps(cls_id.unwrap(), &cls_tps) {
            return false;
        }

        let fct_tps: TypeList = if let Some(fct_tps) = self.fct_tps {
            if !self.check_fct_tps(fct_tps) {
                return false;
            }

            fct_tps.clone()
        } else {
            TypeList::empty()
        };

        if args.contains(&BuiltinType::Error) {
            return false;
        }

        if !args_compatible(
            self.vm,
            &fct.params_without_self(),
            args,
            cls_id,
            Some(fct_id),
            &cls_tps,
            &fct_tps,
            None,
        ) {
            let fct_name = self.vm.interner.str(fct.name).to_string();
            let fct_params = fct
                .params_without_self()
                .iter()
                .map(|a| a.name(self.vm))
                .collect::<Vec<_>>();
            let call_types = args.iter().map(|a| a.name(self.vm)).collect::<Vec<_>>();
            let msg = SemError::ParamTypesIncompatible(fct_name, fct_params, call_types);
            self.vm
                .diag
                .lock()
                .report(self.file, self.pos.expect("pos not set"), msg);
            return false;
        }

        let cmp_type = match kind {
            LookupKind::Ctor(cls_id) => {
                let list_id = self.vm.lists.lock().insert(cls_tps);
                BuiltinType::Class(cls_id, list_id)
            }

            _ => replace_type_param(self.vm, fct.return_type, &cls_tps, &fct_tps, None),
        };

        if self.ret.is_none() || self.ret.unwrap() == cmp_type {
            self.found_ret = Some(cmp_type);
            true
        } else {
            false
        }
    }

    fn find_fct(&self, _: Name) -> Option<FctId> {
        unimplemented!()
    }

    fn find_ctor(&self, cls_id: ClassId) -> Option<FctId> {
        let cls = self.vm.classes.idx(cls_id);
        let cls = cls.read();

        let type_params = self.cls_tps.as_ref().unwrap();
        let args = self.args.unwrap();

        if let Some(ctor_id) = cls.constructor {
            let ctor = self.vm.fcts.idx(ctor_id);
            let ctor = ctor.read();

            if args_compatible(
                self.vm,
                &ctor.params_without_self(),
                &args,
                Some(cls_id),
                None,
                type_params,
                &TypeList::empty(),
                None,
            ) {
                return Some(ctor_id);
            }
        }

        None
    }

    fn find_method(
        &mut self,
        object_type: BuiltinType,
        name: Name,
        is_static: bool,
    ) -> Option<FctId> {
        if object_type.is_nil() {
            return None;
        }

        let candidates = if object_type.is_module() {
            find_methods_in_module(self.vm, object_type, name)
        } else if object_type.is_enum() {
            find_methods_in_enum(self.vm, object_type, name, is_static)
        } else {
            find_methods_in_class(self.vm, object_type, name, is_static)
        };

        self.found_multiple_functions = candidates.len() > 1;

        if candidates.len() == 1 {
            let candidate = candidates.first().unwrap();
            self.found_class_type = Some(candidate.0);
            Some(candidate.1)
        } else {
            None
        }
    }

    fn find_method_in_trait(
        &mut self,
        trait_id: TraitId,
        name: Name,
        is_static: bool,
    ) -> Option<FctId> {
        let xtrait = &self.vm.traits[trait_id];
        let xtrait = xtrait.read();

        xtrait.find_method(self.vm, name, is_static)
    }

    fn check_cls_tps(&self, cls_id: ClassId, tps: &TypeList) -> bool {
        let cls_tps = {
            let cls = self.vm.classes.idx(cls_id);
            let cls = cls.read();
            cls.type_params.to_vec()
        };

        self.check_tps(&cls_tps, tps)
    }

    fn check_fct_tps(&self, tps: &TypeList) -> bool {
        let fct_tps = {
            let fct_id = self.found_fct_id.expect("found_fct_id not set");

            let fct = self.vm.fcts.idx(fct_id);
            let fct = fct.read();
            fct.type_params.to_vec()
        };

        self.check_tps(&fct_tps, tps)
    }

    fn check_tps(&self, specified_tps: &[TypeParam], tps: &TypeList) -> bool {
        typeparamck::check_params(
            self.vm,
            self.file,
            self.pos.expect("no pos"),
            specified_tps,
            tps,
        )
    }

    pub fn found_fct_id(&self) -> Option<FctId> {
        self.found_fct_id
    }

    pub fn found_class_type(&self) -> Option<BuiltinType> {
        self.found_class_type
    }

    pub fn found_ret(&self) -> Option<BuiltinType> {
        self.found_ret
    }
}
