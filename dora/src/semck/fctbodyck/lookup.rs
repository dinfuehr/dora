use crate::error::msg::SemError;
use crate::semck::fctbodyck::body::args_compatible;
use crate::semck::specialize::replace_type_param;
use crate::semck::typeparamck::{self, ErrorReporting};
use crate::ty::{SourceType, TypeList};
use crate::vm::{
    find_methods_in_class, find_methods_in_enum, ClassId, EnumId, Fct, FctId, FctParent, FileId,
    TraitId, TypeParam, VM,
};

use crate::vm::find_methods_in_module;
use dora_parser::interner::Name;
use dora_parser::lexer::position::Position;

#[derive(Clone)]
enum LookupKind {
    Fct,
    Method(SourceType),
    Static(ClassId),
    Trait(TraitId),
    Callee(FctId),
    Ctor(ClassId),
}

pub struct MethodLookup<'a> {
    vm: &'a VM,
    caller: &'a Fct,
    file: FileId,
    kind: Option<LookupKind>,
    name: Option<Name>,
    args: Option<&'a [SourceType]>,
    container_tps: Option<&'a TypeList>,
    fct_tps: Option<&'a TypeList>,
    ret: Option<SourceType>,
    pos: Option<Position>,
    report_errors: bool,

    found_fct_id: Option<FctId>,
    found_class_type: Option<SourceType>,
    found_ret: Option<SourceType>,

    found_multiple_functions: bool,
}

impl<'a> MethodLookup<'a> {
    pub fn new(vm: &'a VM, caller: &'a Fct) -> MethodLookup<'a> {
        MethodLookup {
            vm,
            caller,
            file: caller.file_id,
            kind: None,
            name: None,
            args: None,
            container_tps: None,
            fct_tps: None,
            ret: None,
            pos: None,
            report_errors: true,

            found_fct_id: None,
            found_class_type: None,
            found_ret: None,

            found_multiple_functions: false,
        }
    }

    pub fn ctor(mut self, cls_id: ClassId) -> MethodLookup<'a> {
        self.kind = Some(LookupKind::Ctor(cls_id));
        self
    }

    pub fn callee(mut self, fct_id: FctId) -> MethodLookup<'a> {
        self.kind = Some(LookupKind::Callee(fct_id));
        self
    }

    pub fn method(mut self, obj: SourceType) -> MethodLookup<'a> {
        self.kind = if let Some(_) = obj.cls_id(self.vm) {
            Some(LookupKind::Method(obj))
        } else if let Some(_) = obj.module_id() {
            Some(LookupKind::Method(obj))
        } else if let SourceType::TraitObject(trait_id) = obj {
            Some(LookupKind::Trait(trait_id))
        } else if obj.is_enum() {
            Some(LookupKind::Method(obj))
        } else {
            panic!("neither object nor trait object: {:?}", obj);
        };

        self
    }

    pub fn no_error_reporting(mut self) -> MethodLookup<'a> {
        self.report_errors = false;
        self
    }

    pub fn static_method(mut self, cls_id: ClassId) -> MethodLookup<'a> {
        self.kind = Some(LookupKind::Static(cls_id));
        self
    }

    pub fn fct(mut self) -> MethodLookup<'a> {
        self.kind = Some(LookupKind::Fct);
        self
    }

    pub fn args(mut self, args: &'a [SourceType]) -> MethodLookup<'a> {
        self.args = Some(args);
        self
    }

    pub fn pos(mut self, pos: Position) -> MethodLookup<'a> {
        self.pos = Some(pos);
        self
    }

    pub fn container_type_params(mut self, cls_tps: &'a TypeList) -> MethodLookup<'a> {
        self.container_tps = Some(cls_tps);
        self
    }

    pub fn fct_type_params(mut self, fct_tps: &'a TypeList) -> MethodLookup<'a> {
        self.fct_tps = Some(fct_tps);
        self
    }

    pub fn name(mut self, name: Name) -> MethodLookup<'a> {
        self.name = Some(name);
        self
    }

    pub fn return_type(mut self, ret: SourceType) -> MethodLookup<'a> {
        self.ret = Some(ret);
        self
    }

    pub fn find(&mut self) -> bool {
        let kind = self.kind.clone().expect("kind not set");
        let args = self.args.expect("args not set");

        let fct_id = match kind {
            LookupKind::Fct => {
                assert!(self.container_tps.is_none());
                let name = self.name.expect("name not set");
                self.find_fct(name)
            }

            LookupKind::Callee(fct_id) => Some(fct_id),

            LookupKind::Method(ref obj) => {
                let name = self.name.expect("name not set");
                self.find_method(obj.clone(), name, false)
            }

            LookupKind::Trait(trait_id) => {
                let name = self.name.expect("name not set");
                self.find_method_in_trait(trait_id, name, false)
            }

            LookupKind::Static(cls_id) => {
                assert!(self.container_tps.is_none());
                let name = self.name.expect("name not set");
                self.find_method(self.vm.cls(cls_id), name, true)
            }

            LookupKind::Ctor(cls_id) => {
                assert!(self.container_tps.is_some());
                self.find_ctor(cls_id)
            }
        };

        self.found_fct_id = fct_id;

        let fct_id = if let Some(fct_id) = fct_id {
            fct_id
        } else if self.report_errors {
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
                .map(|a| a.name_fct(self.vm, self.caller))
                .collect::<Vec<String>>();

            let msg = match kind {
                LookupKind::Fct => SemError::Unimplemented,
                LookupKind::Callee(_) => unreachable!(),
                LookupKind::Method(ref obj) => {
                    let type_name = obj.name_fct(self.vm, self.caller);

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
                    let type_name = self.vm.cls(cls_id).name_fct(self.vm, self.caller);
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
        } else {
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

        let enum_id = match fct.parent {
            FctParent::Extension(extension_id) => {
                let extension = &self.vm.extensions[extension_id];
                let extension = extension.read();
                extension.ty.enum_id()
            }
            _ => None,
        };

        let container_tps: TypeList = if let Some(container_tps) = self.container_tps {
            container_tps.clone()
        } else if let LookupKind::Method(ref obj) = kind {
            obj.type_params(self.vm)
        } else {
            TypeList::empty()
        };

        if cls_id.is_some() && !self.check_cls_tps(cls_id.unwrap(), &container_tps) {
            return false;
        }

        if enum_id.is_some() && !self.check_enum_tps(enum_id.unwrap(), &container_tps) {
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

        if args.contains(&SourceType::Error) {
            return false;
        }

        let type_params = container_tps.append(&fct_tps);
        if !args_compatible(self.vm, &*fct, args, &type_params, None) {
            if !self.report_errors {
                return false;
            }

            let fct_name = self.vm.interner.str(fct.name).to_string();
            let fct_params = fct
                .params_without_self()
                .iter()
                .map(|a| a.name_fct(self.vm, &*fct))
                .collect::<Vec<_>>();
            let call_types = args
                .iter()
                .map(|a| a.name_fct(self.vm, &*fct))
                .collect::<Vec<_>>();
            let msg = SemError::ParamTypesIncompatible(fct_name, fct_params, call_types);
            self.vm
                .diag
                .lock()
                .report(self.file, self.pos.expect("pos not set"), msg);
            return false;
        }

        let cmp_type = match kind {
            LookupKind::Ctor(cls_id) => {
                let list_id = self.vm.lists.lock().insert(container_tps);
                SourceType::Class(cls_id, list_id)
            }
            _ => {
                let type_list = container_tps.append(&fct_tps);
                replace_type_param(self.vm, fct.return_type.clone(), &type_list, None)
            }
        };

        if self.ret.is_none() || self.ret.clone().unwrap() == cmp_type {
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

        cls.constructor
    }

    fn find_method(
        &mut self,
        object_type: SourceType,
        name: Name,
        is_static: bool,
    ) -> Option<FctId> {
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
            self.found_class_type = Some(candidate.0.clone());
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

    fn check_enum_tps(&self, enum_id: EnumId, tps: &TypeList) -> bool {
        let enum_tps = {
            let xenum = &self.vm.enums[enum_id];
            let xenum = xenum.read();
            xenum.type_params.to_vec()
        };

        self.check_tps(&enum_tps, tps)
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
        let error = if self.report_errors {
            ErrorReporting::Yes(self.file, self.pos.expect("no pos"))
        } else {
            ErrorReporting::No
        };

        typeparamck::check_params(self.vm, self.caller, error, specified_tps, tps)
    }

    pub fn found_fct_id(&self) -> Option<FctId> {
        self.found_fct_id
    }

    pub fn found_class_type(&self) -> Option<SourceType> {
        self.found_class_type.clone()
    }

    pub fn found_ret(&self) -> Option<SourceType> {
        self.found_ret.clone()
    }
}
